use super::bindgen::root as cpp;
use self::cpp::llvm as llvm;

use std::fmt;
use std::mem::transmute;
use super::register::{Register, RegisterInfo, RegisterClass};

/// A descriptor for a single instruction in LLVM's target layer.
///
/// This class roughly corresponds to LLVM's [MCInstrDesc][1] class.
/// Unfortunately, not all of the information that is potentially relevant to
/// understanding instructions is readily exposed. For example, there is no
/// easy mapping between x86 opcode names and LLVM's internal opcode names.
///
/// Counting instructions is difficult, and LLVM's MC layer uses a relatively
/// fine-grained distinction between opcodes. What may be one assembly mnemonic
/// can turn out to refer to a dozen different opcodes.
/// [1]: http://www.llvm.org/docs/doxygen/html/classllvm_1_1MCInstrDesc.html
pub struct InstructionDesc<'a> {
    mc_desc: &'static llvm::MCInstrDesc,

    /// The LLVM name of this opcode type.
    pub name: &'static str,

    operands: Vec<InstructionOperand<'a>>
}

macro_rules! gen_method {
    ($rname: ident, $cxxname: ident) => {
        /// See LLVM documentation for details.
        pub fn $rname(&self) -> bool {
            unsafe {
                self.mc_desc.$cxxname()
            }
        }
    };
}
impl <'a> InstructionDesc<'a> {
    pub fn new(desc: &'static llvm::MCInstrDesc, name: &'static str,
               mri: *const RegisterInfo) -> InstructionDesc<'a> {
        let mut operands = Vec::new();
        unsafe {
            // Load the explicit operands...
            for i in 0..desc.getNumOperands() {
                let op_info = desc.OpInfo.offset(i as isize).as_ref().unwrap();
                let mut op = InstructionOperand::make_op(mri.as_ref().unwrap(),
                    op_info);
                if i < desc.getNumDefs() {
                    op.write = true;
                }
                operands.push(op);
            }

            // ... and the implicit ones
            InstructionOperand::add_implicit(mri.as_ref().unwrap(),
                desc.ImplicitDefs, &mut operands, true);
            InstructionOperand::add_implicit(mri.as_ref().unwrap(),
                desc.ImplicitUses, &mut operands, false);
        }
        InstructionDesc {
            mc_desc: desc,
            name: name,
            operands: operands
        }
    }

    /// Get the operands that make up this method.
    pub fn get_operands(&self) -> &Vec<InstructionOperand> {
        &self.operands
    }

    gen_method!(is_variadic, isVariadic);
    gen_method!(is_psuedo, isPseudo);
    gen_method!(is_return, isReturn);
    gen_method!(is_call, isCall);
    gen_method!(is_barrier, isBarrier);
    gen_method!(is_terminator, isTerminator);
    gen_method!(is_branch, isBranch);
    gen_method!(is_indirect_branch, isIndirectBranch);
    gen_method!(is_conditional_branch, isConditionalBranch);
    gen_method!(is_unconditional_branch, isUnconditionalBranch);
    gen_method!(is_predicable, isPredicable);
    gen_method!(is_compare, isCompare);
    gen_method!(is_move_immediate, isMoveImmediate);
    gen_method!(is_bitcast, isBitcast);
    gen_method!(is_select, isSelect);
    gen_method!(is_not_duplicable, isNotDuplicable);
    gen_method!(has_delay_slot, hasDelaySlot);
    gen_method!(can_fold_as_load, canFoldAsLoad);
    gen_method!(is_convergent, isConvergent);
    gen_method!(may_load, mayLoad);
    gen_method!(may_store, mayStore);
    gen_method!(has_unmodeled_side_effects, hasUnmodeledSideEffects);
    gen_method!(is_commutable, isCommutable);
    gen_method!(is_convertible_to_3_addr, isConvertibleTo3Addr);
    gen_method!(has_extra_src_reg_alloc_req, hasExtraSrcRegAllocReq);
    gen_method!(has_extra_def_reg_alloc_req, hasExtraDefRegAllocReq);
}

impl <'a> fmt::Display for InstructionDesc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl <'a> fmt::Debug for InstructionDesc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "InstructionDesc({})", self.name)
    }
}

/// An operand for an instruction.
///
/// This class sort of corresponds to LLVM's [MCOperandInfo][1], but only
/// extremely loosely. Implicit and explicit arguments both share this data
/// structure.
/// [1]: http://www.llvm.org/docs/doxygen/html/classllvm_1_1MCOperandInfo.html
#[derive(Debug, Eq, PartialEq)]
pub struct InstructionOperand<'a> {
    /// The constraint on the values this operand can hold.
    pub kind: OperandType<'a>,

    /// Whether or not this operand is a write or a read of the relevant
    /// location.
    pub write: bool,

    /// If true, this operand is implicit in the instruction and does not need
    /// to be filled with an operand.
    pub implicit: bool,
}

impl <'a> InstructionOperand<'a> {
    fn make_op(mri: &'a RegisterInfo,
               op_desc: &llvm::MCOperandInfo) -> InstructionOperand<'a> {
        // Load the explicit operands.
        use self::llvm::MCOI::OperandType as MCOperandType;
        let op_type : MCOperandType =
            unsafe { transmute(op_desc.OperandType as u32) };
        let constraints = op_desc.Constraints;
        let is_tied = constraints &
            (1 << llvm::MCOI::OperandConstraint::TIED_TO as u32) != 0;
        let kind = match op_type {
            MCOperandType::OPERAND_UNKNOWN => OperandType::Unknown,
            MCOperandType::OPERAND_IMMEDIATE => OperandType::Immediate,
            MCOperandType::OPERAND_REGISTER => {
                if is_tied {
                    OperandType::TiedRegister(((constraints >> 16) & 0xf) as u8)
                } else {
                    OperandType::Register(
                        &mri.get_register_classes()[op_desc.RegClass as usize])
                }
            },
            MCOperandType::OPERAND_MEMORY => OperandType::Memory,
            MCOperandType::OPERAND_PCREL => OperandType::PCRel,
            MCOperandType::OPERAND_FIRST_TARGET => OperandType::Unknown,
        };

        InstructionOperand {
            kind: kind,
            write: false,
            implicit: false
        }
    }

    fn add_implicit(mri: &'a RegisterInfo, mut regs: *const u16,
                    operands: &mut Vec<InstructionOperand<'a>>, write: bool) {
        unsafe {
            while let Some(&num) = regs.as_ref() {
                if num == 0 { break; }
                let reg_info = &mri.get_registers()[num as usize - 1];
                operands.push(InstructionOperand {
                    kind: OperandType::FixedRegister(reg_info),
                    write: write,
                    implicit: true
                });
                regs = regs.offset(1);
            }
        }
    }
}


/// The constraints on what the valid operands of an InstructionDesc can be.
#[derive(Debug, Eq, PartialEq)]
pub enum OperandType<'a> {
    /// This type is not sufficiently reflected by LLVM's interfaces.
    Unknown,

    /// An immediate operand. Unfortunately, size and type information for
    /// immediate operands are not reflected by LLVM.
    Immediate,

    /// A register of a given register class.
    Register(&'a RegisterClass),

    /// A register constraint that needs to be the same register as another
    /// register in this instruction. The value here is the index into the
    /// register of the other constraint.
    TiedRegister(u8),

    /// A memory operand. The layout of memory operands is very poorly specified
    /// by LLVM (in particularly, the x86 Mod/RM byte uses 5 operands all
    /// classified as memory). Future versions might take the time to bring some
    /// higher-level sanity to this operand type.
    Memory,

    /// This is OPERAND_PCREL in LLVM terms. I don't know what it means.
    PCRel,

    /// This is a register that is fixed to be a particular register,
    /// particularly useful for implicit constraints.
    FixedRegister(&'a Register)
}

#[cfg(test)]
mod tests {
    use ::TargetTriple;
    use super::*;

    #[test]
    fn test_instructions() {
        let target = TargetTriple::get("amd64").expect("Need amd64 support");
        let mri = target.register_info();

        let reg_class = |name: &str| {
            OperandType::Register(mri.get_register_class(name).unwrap())
        };
        let reg = |name: &str| {
            OperandType::FixedRegister(mri.get_register(name).unwrap())
        };
        let insts = target.instructions();
        assert!(insts.len() > 0, "Needs to be a lot of amd64 instructions");

        let andrr = insts.iter().find(|x| x.name == "AND32rr")
            .expect("Need to find a 32-bit and instruction for tests");
        assert!(!andrr.is_call());
        assert!(!andrr.is_psuedo());
        assert!(andrr.is_commutable());

        assert_eq!(andrr.get_operands(), &vec![
                   InstructionOperand {
                       kind: reg_class("GR32"), write: true, implicit: false
                   }, InstructionOperand {
                       kind: OperandType::TiedRegister(0),
                       write: false, implicit: false
                   }, InstructionOperand {
                       kind: reg_class("GR32"), write: false, implicit: false
                   }, InstructionOperand {
                       kind: reg("EFLAGS"), write: true, implicit: true
                   }
        ]);
    }
}
