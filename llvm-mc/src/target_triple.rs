use super::bindgen::root as cpp;

use std::ffi::{CStr, CString};
use std::ptr;

use super::register::RegisterInfo;

/// A representation of an architecture in LLVM.
///
/// This type corresponds primarily to LLVM's [Target][1] class, although it
/// also incorporates aspects of the triple string, as that is needed for many
/// of the MC interfaces.
///
/// [1]: http://www.llvm.org/docs/doxygen/html/classllvm_1_1Target.html
pub struct TargetTriple {
    //XXX: set the triple somehow.
    //triple: String,
    //target: cpp::TargetTriple,
    mri: RegisterInfo
}

impl TargetTriple {
    /// Return a TargetTriple for the given triple string. The triple is used to
    /// search for the target according to LLVM's tables; note that the
    /// architecture itself (e.g., ```amd64```) is sufficient to get an
    /// instance. If no information can be found for that triple, return a
    /// string describing the error message instead.
    pub fn get(name: &str) -> Result<TargetTriple, String> {
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
            let result = TargetTriple {
                //triple: Default::default(),
                mri: RegisterInfo::get(tt.mri.as_ref().unwrap()),
                //target: tt,
            };
            return Ok(result);
        }
    }

    pub fn register_info(&self) -> &RegisterInfo {
        &self.mri
    }
}


#[cfg(test)]
mod tests {
    use super::TargetTriple;

    #[test]
    fn test_triple_construction() {
        assert!(TargetTriple::get("x86_64-unknown-linux-gnu").is_ok());
        assert!(TargetTriple::get("i am not a valid triple").is_err());
    }
}
