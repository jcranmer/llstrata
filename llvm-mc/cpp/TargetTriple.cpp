#include <llvm/ADT/Triple.h>
#include <llvm/MC/MCInstrInfo.h>
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
}

TargetTriple::~TargetTriple() {
  delete triple;
  delete mii;
}
