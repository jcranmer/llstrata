
use super::bindgen::root as cpp;
use std::ffi::CStr;
use std::fmt;
use std::mem::transmute;
use std::os::raw::{c_char, c_uint};
use std::ptr;

use self::cpp::llvm::MCOI::OperandType;

pub struct TargetTriple {
  target: cpp::TargetTriple
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
      return Ok(TargetTriple {
        target: tt
      });
    }
  }

  fn mii(&self) -> &cpp::llvm::MCInstrInfo {
    unsafe {
      self.target.mii.as_ref().unwrap()
    }
  }

  fn mri(&self) -> &cpp::llvm::MCRegisterInfo {
    unsafe {
      self.target.mri.as_ref().unwrap()
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

  pub fn get_register_class(&self, index: u32) -> RegisterClass {
    let mri = self.mri();
    unsafe {
      assert!(index < mri.getNumRegClasses(), "Illegal class index");
      let regclass_ptr = mri.getRegClass(index);
      let name = CStr::from_ptr(mri.getRegClassName(regclass_ptr))
        .to_str().expect("Reg class names should be ASCII");
      return RegisterClass {
        name: name,
        index: index
      }
    }
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
      for i in 0..(desc.NumOperands as isize) {
        let op_info = desc.OpInfo.offset(i).as_ref()
          .expect("op_info should not be null");
        let op_type : cpp::llvm::MCOI::OperandType =
          transmute(op_info.OperandType as u32);
        let constraints = op_info.Constraints;
        let is_tied = constraints &
          (1 << cpp::llvm::MCOI::OperandConstraint::TIED_TO as u32) != 0;
        results.push(match op_type {
          OperandType::OPERAND_UNKNOWN => OperandInfo::Unknown,
          OperandType::OPERAND_IMMEDIATE => OperandInfo::Immediate,
          OperandType::OPERAND_REGISTER => {
            if is_tied {
              OperandInfo::TiedRegister(((constraints >> 16) & 0xf) as u8)
            } else {
              OperandInfo::Register(op_info.RegClass as usize)
            }
          },
          OperandType::OPERAND_MEMORY => OperandInfo::Mem,
          OperandType::OPERAND_PCREL => OperandInfo::PCRel,
          OperandType::OPERAND_FIRST_TARGET => OperandInfo::Unknown,
        });
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
pub struct RegisterClass {
  pub name: &'static str,
  index: c_uint
}

#[derive(Debug)]
pub enum OperandInfo {
  Unknown,
  Register(usize),
  TiedRegister(u8),
  Immediate,
  PCRel,
  Mem,
}

