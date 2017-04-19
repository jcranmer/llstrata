use super::bindgen::root as cpp;
use self::cpp::llvm as llvm;

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
        println!("{:?}", concrete);
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
    Register(&'a Register)
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
            } else {
                panic!("Unknown operand type");
            }
        }
    }
}