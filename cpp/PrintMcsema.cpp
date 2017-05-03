#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/InstVisitor.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/ValueMap.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>

using namespace llvm;

namespace {

static const char *getPredicateName(CmpInst::Predicate p) {
#define PREDICATES \
  PREDICATE(FCMP_FALSE) \
  PREDICATE(FCMP_OEQ) \
  PREDICATE(FCMP_OGT) \
  PREDICATE(FCMP_OGE) \
  PREDICATE(FCMP_OLT) \
  PREDICATE(FCMP_OLE) \
  PREDICATE(FCMP_ONE) \
  PREDICATE(FCMP_ORD) \
  PREDICATE(FCMP_UNO) \
  PREDICATE(FCMP_UEQ) \
  PREDICATE(FCMP_UGT) \
  PREDICATE(FCMP_UGE) \
  PREDICATE(FCMP_ULT) \
  PREDICATE(FCMP_ULE) \
  PREDICATE(FCMP_UNE) \
  PREDICATE(FCMP_TRUE) \
  PREDICATE(ICMP_EQ) \
  PREDICATE(ICMP_NE) \
  PREDICATE(ICMP_UGT) \
  PREDICATE(ICMP_UGE) \
  PREDICATE(ICMP_ULT) \
  PREDICATE(ICMP_ULE) \
  PREDICATE(ICMP_SGT) \
  PREDICATE(ICMP_SGE) \
  PREDICATE(ICMP_SLT) \
  PREDICATE(ICMP_SLE)
  switch (p) {
#define PREDICATE(val) \
  case CmpInst::val: \
    return #val;
  PREDICATES
  case CmpInst::BAD_FCMP_PREDICATE:
  case CmpInst::BAD_ICMP_PREDICATE:
    assert("This should not happen");
  }
  assert("This should not happen");
  return "";
}

class MCSemaVisitor : public InstVisitor<MCSemaVisitor> {
  raw_ostream &out;
  const SmallVector<StringRef, 7> &in_regs;
  const SmallVector<StringRef, 7> &out_regs;
  ValueMap<Value *, std::string> variables;
public:
  MCSemaVisitor(raw_ostream &out, const SmallVector<StringRef, 7> &in_regs,
      const SmallVector<StringRef, 7> &out_regs) : out(out), in_regs(in_regs),
  out_regs(out_regs) {
  }

  std::string makeName() {
    return (Twine("var") + Twine(variables.size())).str();
  }

  raw_ostream &getValue(Value *V) {
    if (ConstantInt *C = dyn_cast<ConstantInt>(V)) {
      out << "llvm::ConstantInt::get(" <<
        "llvm::IntegerType::get(b->getContext(), " << C->getBitWidth() << "), "
        << C->getZExtValue() << ", " << C->isNegative() << ")";
    } else {
      auto val = variables.find(V);
      if (val == variables.end()) {
        assert("We don't know what we're doing");
      } else {
        out << val->second;
      }
    }
    return out;
  }

  void visitReturnInst(ReturnInst &I) {
    if (Constant *C = dyn_cast<Constant>(I.getReturnValue())) {
      StructType *retType = dyn_cast<StructType>(C->getType());
      for (unsigned i = 0; i < retType->getNumElements(); i++) {
        handleReturn(i, retType->getElementType(i),
            C->getAggregateElement(i));
      }
    } else if (!isa<InsertValueInst>(I.getReturnValue())) {
      // These shouldn't come up?
      visitInstruction(I);
    }
  }

  void visitInsertValueInst(InsertValueInst &I) {
    assert(I.hasOneUse() && "Should only appear once");
    Value *parent = I.getAggregateOperand();
    assert((isa<InsertValueInst>(parent) || isa<UndefValue>(parent)) &&
        "Can only handle insertvalue chains");
    Value *val = I.getInsertedValueOperand();
    handleReturn(I.getIndices()[0], val->getType(), val);
  }

  void visitBinaryOperator(BinaryOperator &O) {
    std::string var_name = makeName();
    variables.insert(std::make_pair(&O, var_name));
    out << "  llvm::Value *" << var_name << " = BinaryOperator::Create("
      << "Instruction::" << O.getOpcodeName() << ", ";
    getValue(O.getOperand(0)) << ", ";
    getValue(O.getOperand(1)) << ");\n";
  }
  void visitCmpInst(CmpInst &I) {
    std::string var_name = makeName();
    variables.insert(std::make_pair(&I, var_name));
    out << "  llvm::Value *" << var_name << " = CmpInst::Create("
      << "Instruction::" << I.getOpcodeName() << ", "
      << "CmpInst::" << getPredicateName(I.getPredicate()) << ", ";
    getValue(I.getOperand(0)) << ", ";
    getValue(I.getOperand(1)) << ");\n";
  }
  void visitInstruction(Instruction &I) {
    I.dump();
    llvm_unreachable("We don't know how to handle this instruction!");
  }

  void handleReturn(unsigned index, Type *elTy, Value *val) {
    StringRef reg_name = out_regs[index];
    // Is this a flag?
    if (elTy->isIntegerTy(1)) {
      if (Constant *C = dyn_cast<Constant>(val)) {
        if (C->isZeroValue())
          out << "  F_CLEAR";
        else
          out << "  F_READ";
        out << "(block, llvm::X86::" << reg_name << ");\n";
      } else {
        out << "  F_WRITE(block, llvm::X86::" << reg_name << ", ";
        getValue(val) << ");\n";
      }
    } else {
      out << "  R_WRITE<64>(block, inst.getOperand(" << reg_name << "), ";
      getValue(val) << ");\n";
    }
  }

  void addArguments(Function *F) {
    auto arg_value = F->arg_begin();
    auto arg_index = 0;
    for (auto arg : in_regs) {
      Value *argument = &*arg_value++;
      std::string name = (Twine("arg") + Twine(arg_index)).str();
      out << "  llvm::Value *" << name << " = "
        << "R_READ<64>(block, inst.getOperand(" << arg
        << ").getReg());\n";
      variables.insert(std::make_pair(argument, name));
      arg_index++;
    }
  }
};

void translateFunction(Function *F, raw_ostream &out,
    StringRef in_reg, StringRef out_reg) {
  out << "static InstTransResult trans_" << F->getName()
    << "(TranslationContext &ctx, llvm::BasicBlock *&block) {\n";
  out << "  auto &inst = ctx.natI->get_inst();\n";
  SmallVector<StringRef, 7> inRegList, outRegList;
  in_reg.split(inRegList, ' ', -1, false);
  out_reg.split(outRegList, ' ', -1, false);
  MCSemaVisitor visit(out, inRegList, outRegList);
  visit.addArguments(F);
  visit.visit(F);
  // XXX: process Functions
  out << "  return ContinueBlock;\n";
  out << "}\n";
  out.flush();
}
}


extern "C" void write_file(Module *M, const char *outFile) {
  // XXX: The llvm-alt crate doesn't run inlining passes. So do that first.
  {
    PassManagerBuilder pm;
    pm.OptLevel = 3;
    pm.SizeLevel = 2;
    pm.Inliner = createFunctionInliningPass(3, 0);

    legacy::FunctionPassManager FPM(M);
    legacy::PassManager MPM;
    pm.populateFunctionPassManager(FPM);
    pm.populateModulePassManager(MPM);
    // Run function pass manager first... but we've probably already hit
    // the optimizations from Rust + llvm-alt, so ignore it for now? We
    // mostly just want the inliner to work properly.
    MPM.run(*M);
  }

  std::error_code ec;
  raw_fd_ostream out(outFile, ec, sys::fs::F_None);
  {
    auto F = M->getFunction("CLC");
    translateFunction(F, out, "", "CF");
  }
  {
    auto F = M->getFunction("XOR64rr");
    translateFunction(F, out, "1 2", "0 CF PF ZF SF OF");
  }

  out << "\n\n";
  out << "void AUTOGEN_populateDispatchMap(DispatchMap &m) {\n";
  for (auto inst : std::vector<std::string>({"CLC", "XOR64rr"})) {
    out << "  m[llvm::X86::" << inst << "] = trans_" << inst << ";\n";
  }
  out << "}\n";
}