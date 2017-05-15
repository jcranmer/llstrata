use super::bindgen::root as cpp;
use self::cpp::llvm as llvm;
use std::mem;

use super::{InstructionDesc, Register, RegisterInfo, TargetTriple};

/// An instruction in assembly.
///
/// This class roughly corresponds to LLVM's [MCInst][1] class.
/// [1]: http://www.llvm.org/docs/doxygen/html/classllvm_1_1MCInst.html
#[derive(Debug)]
pub struct Instruction<'a> {
    /// The opcode of the instruction.
    pub opcode: &'a InstructionDesc<'a>,

    /// The set of operands for this instruction.
    ///
    /// The opcodes should be in one-to-one correspondence with the explicit
    /// operands of the InstructionDesc class. Implicit ones are not found
    /// within this list.
    pub operands: Vec<Operand<'a>>
}

pub fn make_instruction<'a>(inst: &llvm::MCInst, tt: &'a TargetTriple) -> Instruction<'a> {
    unsafe {
        let operands = (0..inst.getNumOperands())
            .map(|i| inst.getOperand(i).as_ref().unwrap())
            .map(|op| Operand::from(op, tt.register_info()))
            .collect();
        let concrete = Instruction {
            opcode: &tt.instructions()[inst.getOpcode() as usize],
            operands: operands
        };
        return concrete;
    }
}

/// A possible operand of an assembly instruction.
#[derive(Debug)]
pub enum Operand<'a> {
    /// An integer immediate value, up to 64-bits. The actual type and sign of
    /// the immediate need not be i64.
    Immediate(i64),

    /// A floating-point immediate value.
    FPImmediate(f64),

    /// A specific register, with a pointer to the architecture register
    /// involved.
    Register(&'a Register),

    /// An operand expression, which includes references to symbol names.
    Expr(OpExpr)
}

impl <'a> Operand<'a> {
    fn from(op: &llvm::MCOperand, mri: &'a RegisterInfo) -> Operand<'a> {
        unsafe {
            if op.isReg() {
                Operand::Register(&mri.get_registers()[op.getReg() as usize - 1])
            } else if op.isImm() {
                Operand::Immediate(op.getImm())
            } else if op.isFPImm() {
                Operand::FPImmediate(op.getFPImm())
            } else if op.isExpr() {
                Operand::Expr(OpExpr::make(op.getExpr()))
            } else {
                panic!("Unknown operand type");
            }
        }
    }

    /// Get the register associated with this operand, if it is a register.
    pub fn get_register(&self) -> Option<&'a Register> {
        match self {
            &Operand::Immediate(_) => None,
            &Operand::FPImmediate(_) => None,
            &Operand::Register(r) => Some(r),
            &Operand::Expr(_) => None
        }
    }
}

#[derive(Debug)]
pub enum OpExpr {
    SymbolRef(String)
}

fn cast<T>(ptr: *const llvm::MCExpr) -> *const T {
    return unsafe { mem::transmute(ptr) };
}

impl OpExpr {
    unsafe fn make(expr: *const llvm::MCExpr) -> OpExpr {
        let expr_ref = expr.as_ref().unwrap();
        match expr_ref.getKind() {
            llvm::MCExpr_ExprKind::SymbolRef => {
                let sym_expr = cast::<llvm::MCSymbolRefExpr>(expr);
                let name = sym_expr.as_ref().unwrap()
                    .getSymbol().as_ref().unwrap()
                    .getName();
                return OpExpr::SymbolRef(String::from(name.as_str().unwrap()));
            },
            _ => {
                expr_ref.dump();
                panic!("Unhandled expression ref kinds");
            },
        }
    }
}
