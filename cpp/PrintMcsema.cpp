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

static const char *getCastName(Instruction::CastOps op) {
    switch (op) {
#define HANDLE_CAST_INST(N, OPC, CLASS) \
    case Instruction::OPC: return #OPC;
    #include <llvm/IR/Instruction.def>
#undef HANDLE_CAST_INST
    case Instruction::CastOpsEnd: break;
    }
    assert(false && "This cannot happen");
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
        "llvm::IntegerType::get(block->getContext(), " << C->getBitWidth() << "), "
        << C->getZExtValue() << "U, " << C->isNegative() << ")";
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

  raw_ostream &getType(Type *T) {
      if (IntegerType *IT = dyn_cast<IntegerType>(T)) {
          out << "IntegerType::get(block->getContext(), "
              << IT->getBitWidth() << ")";
      } else {
          assert(false && "We don't know the current type");
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
    std::string opcode_name = O.getOpcodeName();
    opcode_name[0] = std::toupper(opcode_name[0]);
    if (StringRef(opcode_name).endswith("shr"))
      opcode_name[1] = 'S';

    out << "  llvm::Value *" << var_name << " = BinaryOperator::Create("
      << "Instruction::" << opcode_name << ", ";
    getValue(O.getOperand(0)) << ", ";
    getValue(O.getOperand(1)) << ", \"\", block);\n";
  }
  void visitCastInst(CastInst &I) {
    std::string var_name = makeName();
    variables.insert(std::make_pair(&I, var_name));
    out << "  llvm::Value *" << var_name << " = CastInst::Create("
        << "Instruction::" << getCastName(I.getOpcode()) << ", ";
    getValue(I.getOperand(0)) << ", ";
    getType(I.getType()) << ", \"\", block);\n";
  }
  void visitCmpInst(CmpInst &I) {
    std::string var_name = makeName();
    variables.insert(std::make_pair(&I, var_name));
    std::string opcode_name = I.getOpcodeName();
    opcode_name[0] = std::toupper(opcode_name[0]);
    opcode_name[1] = std::toupper(opcode_name[1]);
    out << "  llvm::Value *" << var_name << " = CmpInst::Create("
      << "Instruction::" << opcode_name << ", "
      << "CmpInst::" << getPredicateName(I.getPredicate()) << ", ";
    getValue(I.getOperand(0)) << ", ";
    getValue(I.getOperand(1)) << ", \"\", block);\n";
  }
  void visitExtractValueInst(ExtractValueInst &I) {
    std::string var_name = makeName();
    variables.insert(std::make_pair(&I, var_name));
    out << "  llvm::Value *" << var_name << " = ExtractValueInst::Create(";
    getValue(I.getAggregateOperand()) << ", " << I.getIndices()[0]
      << ", \"\", block);\n";
  }
  void visitIntrinsicInst(IntrinsicInst &I) {
    std::string var_name = makeName();
    variables.insert(std::make_pair(&I, var_name));

    out << "  auto in" << var_name << " = Intrinsic::getDeclaration("
      << "block->getParent()->getParent(), ";
    if (I.getIntrinsicID() == Intrinsic::ctpop) {
      out << "llvm::Intrinsic::ctpop, "
        << "llvm::Type::getInt8Ty(block->getContext()));\n";
    } else if (I.getIntrinsicID() == Intrinsic::uadd_with_overflow) {
      out << "llvm::Intrinsic::uadd_with_overflow, ";
      getType(I.getArgOperand(0)->getType()) << ");\n";
    } else if (I.getIntrinsicID() == Intrinsic::sadd_with_overflow) {
      out << "llvm::Intrinsic::sadd_with_overflow, ";
      getType(I.getArgOperand(0)->getType()) << ");\n";
    } else {
      assert(false && "We found an intrinsic we don't know about");
    }
    out << "  llvm::Value *" << var_name << " = CallInst::Create("
      << "in" << var_name << ", ArrayRef<Value *>({";
    bool comma = false;
    for (auto &op : I.arg_operands()) {
      if (comma)
          out << ", ";
      getValue(op);
      comma = true;
    }
    out << "}), \"\", block);\n";
  }
  void visitSelectInst(SelectInst &I) {
    std::string var_name = makeName();
    variables.insert(std::make_pair(&I, var_name));
    out << "  llvm::Value *" << var_name << " = SelectInst::Create(";
    getValue(I.getCondition()) << ", ";
    getValue(I.getTrueValue()) << ", ";
    getValue(I.getFalseValue()) << ", \"\", block);\n";
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
      out << "  R_WRITE<64>(block, inst.getOperand(" << reg_name << ").getReg(), ";
      getValue(val) << ");\n";
    }
  }

  void addArguments(Function *F) {
    auto arg_value = F->arg_begin();
    auto arg_index = 0;
    for (auto arg : in_regs) {
      Value *argument = &*arg_value++;
      std::string name = (Twine("arg") + Twine(arg_index)).str();
      variables.insert(std::make_pair(argument, name));
      arg_index++;
      out << "  llvm::Value *" << name << " = ";

      int value;
      if (arg.getAsInteger(10, value)) {
        out << "F_READ(block, llvm::X86::" << arg << ");\n";
      } else {
        out << "R_READ<64>(block, inst.getOperand(" << arg
          << ").getReg());\n";
      }
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
  out << "  return ContinueBlock;\n";
  out << "}\n";
  out.flush();
}

struct MCSemaNotes {
    StringRef name;
    StringRef in_string;
    StringRef out_string;
};
}

extern "C" void write_file(Module *M, const char *outFile, MCSemaNotes *notes,
    size_t nNotes) {
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

  // Output the file header
  out << "#include <llvm/IR/BasicBlock.h>\n";
  out << "#include <llvm/IR/Instructions.h>\n";
  out << "#include <llvm/IR/IntrinsicInst.h>\n";
  out << "#include <llvm/IR/Intrinsics.h>\n\n";
  out << "#include \"mcsema/Arch/Arch.h\"\n";
  out << "#include \"mcsema/Arch/Dispatch.h\"\n";
  out << "#include \"mcsema/Arch/Register.h\"\n";
  out << "#include \"mcsema/BC/Util.h\"\n\n";
  out << "using namespace llvm;\n\n";

  // Now, output every instruction output
  ArrayRef<MCSemaNotes> instructions(notes, nNotes);
  for (auto inst : instructions) {
      auto F = M->getFunction(inst.name);
      translateFunction(F, out, inst.in_string, inst.out_string);
  }

  out << "\n\n";
  out << "void AUTOGEN_populateDispatchMap(DispatchMap &m) {\n";
  for (auto inst : instructions) {
    auto name = inst.name;
    out << "  m[llvm::X86::" << name << "] = trans_" << name << ";\n";
  }
  out << "}\n";
}
