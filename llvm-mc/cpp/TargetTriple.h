namespace llvm {
class MCInst;
class MCInstrInfo;
class MCRegisterInfo;
class MCSubtargetInfo;
class StringRef;
class Target;
class Triple;
}

struct RustClosure;
typedef void (*InstCallback)(const llvm::MCInst &, const RustClosure *);

class TargetTriple {
  const llvm::Target *target;
  llvm::Triple *triple;
  llvm::MCInstrInfo *mii;
  llvm::MCRegisterInfo *mri;

public:
  TargetTriple(const char *triple, const char **err);
  ~TargetTriple();

  static void initializeLLVM();

  llvm::MCInstrInfo *getMII() const { return mii; }
  llvm::MCRegisterInfo *getMRI() const { return mri; }
  llvm::MCSubtargetInfo *getSTI(llvm::StringRef CPU,
      llvm::StringRef features) const;
  void parseAsmFile(const llvm::MCSubtargetInfo *STI, llvm::StringRef filename,
      InstCallback callback, const RustClosure *pThis) const;
};
