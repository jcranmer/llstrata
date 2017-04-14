
use super::bindgen::root as cpp;
use std::ffi::CStr;
use std::fmt;
use std::mem::transmute;
use std::os::raw::{c_char, c_uint};
use std::ptr;

use self::cpp::llvm::MCOI::OperandType as MCOperandType;

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

    pub fn get_register_info(&self, index: u32) -> RegisterInfo {
        let mri = self.mri();
        unsafe {
            let desc = mri.get(index).as_ref().unwrap();
            return RegisterInfo {
                name: CStr::from_ptr(mri.getName(index)).to_str()
                    .expect("Register names should be ASCII"),
                index: index
            };
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
pub struct RegisterInfo {
    pub name: &'static str,
    index: c_uint,
}

#[derive(Debug)]
pub struct RegisterClass {
  pub name: &'static str,
  index: c_uint
}

#[derive(Debug)]
pub struct OperandInfo {
    pub kind: OperandType,
    pub write: bool,
    pub implicit: bool
}

#[derive(Debug)]
pub enum OperandType {
  Unknown,
  Register(usize),
  TiedRegister(u8),
  FixedRegister(RegisterInfo),
  Immediate,
  PCRel,
  Mem,
}

impl OperandType {
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

