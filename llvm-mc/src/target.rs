
use super::bindgen::root as cpp;
use std::ffi::CStr;
use std::fmt;
use std::mem::transmute;
use std::os::raw::{c_char, c_uint};
use std::ptr;

use self::cpp::llvm::MCOI::OperandType as MCOperandType;

pub struct TargetTriple {
    target: cpp::TargetTriple,
    mri: super::RegisterInfo
}

impl TargetTriple {
  /// Return hardware information for the given the triple. If the triple string
  /// could not be parsed, return an error message describing the problem.
  pub fn get(name: &str) -> Result<TargetTriple, String> {
    unsafe {
      let mut err : *const c_char = ptr::null();
      let tt = cpp::TargetTriple::new(name.as_ptr() as *const i8, &mut err);
      if !err.is_null() {
        if let Ok(val) = CStr::from_ptr(err).to_str() {
          return Err(String::from(val));
        } else {
          return Err(String::from("Couldn't convert error message"));
        }
      }
      let result = TargetTriple {
          mri: super::RegisterInfo::get(tt.mri.as_ref().unwrap()),
          target: tt,
      };
      return Ok(result);
    }
  }

  fn mii(&self) -> &cpp::llvm::MCInstrInfo {
    unsafe {
      self.target.mii.as_ref().unwrap()
    }
  }

  /// Get all of the instructions for this target.
  ///
  /// Note that some of the instructions might not be representable in all modes
  /// of the compiler (e.g., some x86 instructions are not valid in x86-64
  /// mode).
  pub fn get_instructions<'a>(&'a self) -> Vec<Instruction<'a>> {
    let mut instrs = Vec::new();
    let mii = self.mii();
    unsafe {
      for op in 0..mii.getNumOpcodes() {
        // Ignore pseudo instructions in the MII.
        let desc = mii.get(op).as_ref().expect("Should not have a null desc");
        if desc.isPseudo() {
          continue;
        }
        instrs.push(Instruction {
          tt: self,
          opcode: op
        });
      }
    }
    return instrs;
  }

    pub fn get_register_class(&self, index: u32) -> &super::RegisterClass {
        &self.mri.get_register_classes()[index as usize]
    }

    pub fn get_register_info(&self, index: u32) -> &super::Register {
        &self.mri.get_registers()[index as usize - 1]
    }
}

impl Drop for TargetTriple {
  fn drop(&mut self) {
    unsafe {
      self.target.destruct();
    }
  }
}

pub struct Instruction<'a> {
  tt: &'a TargetTriple,
  opcode: c_uint
}

impl <'a> Instruction<'a> {
  pub fn get_name(&self) -> &str {
    unsafe {
      let name = self.tt.mii().getName(self.opcode);
      if name.is_null() {
        return "";
      }
      return CStr::from_ptr(name).to_str().unwrap();
    }
  }

  pub fn get_operands(&self) -> Vec<OperandInfo> {
    let mut results = Vec::new();
    unsafe {
      let desc = self.tt.mii().get(self.opcode).as_ref()
        .expect("Should not be null");
      let num_defs = desc.NumDefs as isize;
      for i in 0..(desc.NumOperands as isize) {
        let op_info = desc.OpInfo.offset(i).as_ref()
          .expect("op_info should not be null");
        results.push(OperandInfo {
            kind: OperandType::make_operand(op_info),
            write: i < num_defs,
            implicit: false
        });
      }

      let mut implicit_def = desc.ImplicitDefs;
      while let Some(&num) = implicit_def.as_ref() {
          if num == 0 { break; }
          let reg_info = self.tt.get_register_info(num as u32);
          results.push(OperandInfo {
              kind: OperandType::FixedRegister(reg_info),
              write: true,
              implicit: true
          });
          implicit_def = implicit_def.offset(1);
      }

      let mut implicit_use = desc.ImplicitUses;
      while let Some(&num) = implicit_use.as_ref() {
          if num == 0 { break; }
          let reg_info = self.tt.get_register_info(num as u32);
          results.push(OperandInfo {
              kind: OperandType::FixedRegister(reg_info),
              write: false,
              implicit: true
          });
          implicit_use = implicit_use.offset(1);
      }
    }
    return results;
  }
}

impl <'a> fmt::Debug for Instruction<'a> {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
      write!(fmt, "{}", self.get_name())
  }
}

#[derive(Debug)]
pub struct OperandInfo<'a> {
    pub kind: OperandType<'a>,
    pub write: bool,
    pub implicit: bool
}

#[derive(Debug)]
pub enum OperandType<'a> {
  Unknown,
  Register(usize),
  TiedRegister(u8),
  FixedRegister(&'a super::Register),
  Immediate,
  PCRel,
  Mem,
}

impl <'a> OperandType<'a> {
    fn make_operand(opinfo: &cpp::llvm::MCOperandInfo) -> OperandType {
        let op_type : MCOperandType =
            unsafe { transmute(opinfo.OperandType as u32) };
        let constraints = opinfo.Constraints;
        let is_tied = constraints &
            (1 << cpp::llvm::MCOI::OperandConstraint::TIED_TO as u32) != 0;
        return match op_type {
            MCOperandType::OPERAND_UNKNOWN => OperandType::Unknown,
            MCOperandType::OPERAND_IMMEDIATE => OperandType::Immediate,
            MCOperandType::OPERAND_REGISTER => {
                if is_tied {
                    OperandType::TiedRegister(((constraints >> 16) & 0xf) as u8)
                } else {
                    OperandType::Register(opinfo.RegClass as usize)
                }
            },
            MCOperandType::OPERAND_MEMORY => OperandType::Mem,
            MCOperandType::OPERAND_PCREL => OperandType::PCRel,
            MCOperandType::OPERAND_FIRST_TARGET => OperandType::Unknown,
        };
    }
}

