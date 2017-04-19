#include <llvm/ADT/Triple.h>
#include <llvm/MC/MCAsmInfo.h>
#include <llvm/MC/MCContext.h>
#include <llvm/MC/MCInstrInfo.h>
#include <llvm/MC/MCObjectFileInfo.h>
#include <llvm/MC/MCParser/MCAsmParser.h>
#include <llvm/MC/MCParser/MCTargetAsmParser.h>
#include <llvm/MC/MCRegisterInfo.h>
#include <llvm/MC/MCStreamer.h>
#include <llvm/MC/MCSubtargetInfo.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>

#include <string>
#include <iostream>

#include "cpp/TargetTriple.h"

using namespace llvm;

bool initialized = false;
static void initialize() {
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllDisassemblers();
  initialized = true;
}

TargetTriple::TargetTriple(const char *name, const char **err) {
  triple = nullptr;
  target = nullptr;
  mii = nullptr;
  mri = nullptr;

  if (!initialized)
    initialize();
  triple = new Triple(Triple::normalize(name));
  std::string err_str;
  target = TargetRegistry::lookupTarget("", *triple, err_str);

  // If we failed, return the error message.
  *err = nullptr;
  if (!target) {
    *err = err_str.c_str();
    return;
  }

  mii = target->createMCInstrInfo();
  mri = target->createMCRegInfo(triple->str());
}

TargetTriple::~TargetTriple() {
  delete triple;
  delete mii;
  delete mri;
}

MCSubtargetInfo *TargetTriple::getSTI(StringRef cpu, StringRef features) const {
  return target->createMCSubtargetInfo(triple->str(), cpu, features);
}

// I hate having to write so much complex code in C++ rather than Rust, but
// there is no easy way to reflect MCStreamer sufficiently in Rust to do what
// is needed.
class AsmOutputStreamer final : public MCStreamer {
public:
  AsmOutputStreamer(MCContext &ctx, InstCallback call, const RustClosure *pThis)
  : MCStreamer(ctx), callback(call), pThis(pThis) {}

  virtual bool EmitSymbolAttribute(MCSymbol *, MCSymbolAttr) override {
    return true;
  }
  virtual void EmitCommonSymbol(MCSymbol *, uint64_t, unsigned) override {}
  virtual void EmitZerofill(MCSection *, MCSymbol *, uint64_t, unsigned) override {}

  virtual void EmitInstruction(const MCInst &inst,
      const MCSubtargetInfo &STI) override {
    callback(inst, pThis);
  }

  virtual void FinishImpl() override {
  }

private:
  InstCallback callback;
  const RustClosure *pThis;
};

void TargetTriple::parseAsmFile(const MCSubtargetInfo *STI, StringRef filename,
    InstCallback callback, const RustClosure *pThis) const {

  // Init the MC things we need.
  std::unique_ptr<MCAsmInfo> mai(target->createMCAsmInfo(*mri, triple->str()));
  MCObjectFileInfo MOFI;
  MCContext ctx(mai.get(), mri, &MOFI);
  MOFI.InitMCObjectFileInfo(*triple, Reloc::Static, CodeModel::Default, ctx);


  // Load the filename.
  auto fileLoc = MemoryBuffer::getFile(filename);
  if (!fileLoc) {
    return;
  }
  std::unique_ptr<MemoryBuffer> buf(fileLoc->release());
  SourceMgr asmSource;
  asmSource.AddNewSourceBuffer(std::move(buf), SMLoc());

  // Run this through the output streamer.
  AsmOutputStreamer aos(ctx, callback, pThis);
  std::unique_ptr<MCAsmParser> parser(createMCAsmParser(asmSource,
    ctx, aos, *mai));
  std::unique_ptr<MCTargetAsmParser> targetParser(
    target->createMCAsmParser(*STI, *parser, *mii,
    MCTargetOptions{}));
  parser->setTargetParser(*targetParser);
  parser->Run(false);

  delete STI;
}
