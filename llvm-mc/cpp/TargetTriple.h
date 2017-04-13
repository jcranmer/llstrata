namespace llvm {
class MCInstrInfo;
class Target;
class Triple;
}

class TargetTriple {
  const llvm::Target *target;
  llvm::Triple *triple;
  llvm::MCInstrInfo *mii;

public:
  TargetTriple(const char *triple, const char **err);
  ~TargetTriple();

  llvm::MCInstrInfo *getMII() const { return mii; }
};

