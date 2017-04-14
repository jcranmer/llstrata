namespace llvm {
class MCInstrInfo;
class MCRegisterInfo;
class Target;
class Triple;
}

class TargetTriple {
  const llvm::Target *target;
  llvm::Triple *triple;
  llvm::MCInstrInfo *mii;
  llvm::MCRegisterInfo *mri;

public:
  TargetTriple(const char *triple, const char **err);
  ~TargetTriple();

  llvm::MCInstrInfo *getMII() const { return mii; }
  llvm::MCRegisterInfo *getMRI() const { return mri; }
};

