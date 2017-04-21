use super::bindgen::root as cpp;

use std::ffi::{CStr, CString};
use std::mem::transmute;
use std::ptr;
use std::sync::{Once, ONCE_INIT};

use super::instructions::InstructionDesc;
use super::mcinst;
use super::register::RegisterInfo;

static START: Once = ONCE_INIT;

/// A representation of an architecture in LLVM.
///
/// This type corresponds primarily to LLVM's [Target][1] class, although it
/// also incorporates aspects of the triple string, as that is needed for many
/// of the MC interfaces.
///
/// [1]: http://www.llvm.org/docs/doxygen/html/classllvm_1_1Target.html
pub struct TargetTriple<'a> {
    //XXX: set the triple somehow.
    //triple: String,
    target: cpp::TargetTriple,
    mri: RegisterInfo,
    instructions: Vec<InstructionDesc<'a>>,
}

impl <'a> TargetTriple<'a> {
    /// Return a TargetTriple for the given triple string. The triple is used to
    /// search for the target according to LLVM's tables; note that the
    /// architecture itself (e.g., ```amd64```) is sufficient to get an
    /// instance. If no information can be found for that triple, return a
    /// string describing the error message instead.
    pub fn get(name: &str) -> Result<TargetTriple, String> {
        START.call_once(|| {
            unsafe {
                cpp::TargetTriple::initializeLLVM();
            }
        });

        let safe_name = CString::new(name).expect("Need \\0 byte");
        unsafe {
            let mut err = ptr::null();
            let tt = cpp::TargetTriple::new(safe_name.as_ptr(),
              &mut err);
            if !err.is_null() {
                return Err(String::from(match CStr::from_ptr(err).to_str() {
                    Ok(val) => val,
                    _ => "Unknown error message"
                }));
            }
            let mut result = TargetTriple {
                //triple: Default::default(),
                mri: RegisterInfo::get(tt.mri.as_ref().unwrap()),
                instructions: Vec::new(),
                target: tt,
            };
            let mii = result.target.mii.as_ref().unwrap();
            for i in 0..mii.getNumOpcodes() {
                let name = CStr::from_ptr(mii.getName(i))
                    .to_str().expect("Register names should be ASCII");
                result.instructions.push(InstructionDesc::new(
                    mii.get(i).as_ref().unwrap(), name, &result.mri));
            }
            return Ok(result);
        }
    }

    /// Get the register information for this target.
    pub fn register_info(&self) -> &RegisterInfo {
        &self.mri
    }

    /// Get the list of instruction opcodes for this target.
    pub fn instructions(&self) -> &Vec<InstructionDesc> {
        &self.instructions
    }

    /// Parse an assembly file into a list of instructions.
    ///
    /// This is not necessarily a good representation of an assembly file, but
    /// it is sufficient for immediate needs. Actually reflecting a more
    /// general representation of a parser would involve having to reflect
    /// large portions of the LLVM MC backend for sufficient information.
    pub fn parse_instructions(&'a self, cpu: &str, features: &str,
                              file_name: &str) -> Vec<mcinst::Instruction<'a>> {
        let mut insts = Vec::new();
        unsafe {
            let sti = self.target.getSTI(cpp::llvm::StringRef::from(cpu),
                cpp::llvm::StringRef::from(features));
            let mut closure = |inst: *const cpp::llvm::MCInst| {
                let concrete = mcinst::make_instruction(
                    inst.as_ref().unwrap(), self);
                insts.push(concrete);
            };
            self.parse_file(sti, file_name, &Closure(&mut closure));
        }
        return insts;
    }

    fn parse_file(&self, sti: *const cpp::llvm::MCSubtargetInfo, file_name: &str, closure: &Closure) {
        unsafe {
            self.target.parseAsmFile(sti, cpp::llvm::StringRef::from(file_name),
              Some(TargetTriple::callback), transmute(closure));
        }
    }

    extern "C" fn callback(inst: *const cpp::llvm::MCInst, closure: *const cpp::RustClosure) {
        unsafe {
            let rust_fn: *mut Closure = transmute(closure);
            rust_fn.as_mut().expect("How did you become null?").0(inst);
        }
    }
}

struct Closure<'a>(&'a mut FnMut(*const cpp::llvm::MCInst) -> ());


#[cfg(test)]
mod tests {
    use super::TargetTriple;

    #[test]
    fn test_triple_construction() {
        assert!(TargetTriple::get("x86_64-unknown-linux-gnu").is_ok());
        assert!(TargetTriple::get("i am not a valid triple").is_err());
    }
}
