#include "TargetTriple.h"

#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCSymbol.h"

/// <div rustbindgen replaces="llvm::StringRef"></div>
struct PseudoStringRef {
  const char *data;
  size_t length;
};

/// <div rustbindgen replaces="llvm::SmallVectorImpl"></div>
template <typename T>
class SmallVectorImpl {
    T el;
    ~SmallVectorImpl();
};
