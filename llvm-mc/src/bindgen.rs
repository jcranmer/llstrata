include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

impl <'a> From<&'a str> for root::llvm::StringRef {
    fn from(s: &'a str) -> Self {
        root::llvm::StringRef {
            data: s.as_ptr() as *const i8,
            length: s.len()
        }
    }
}

use std::{slice, str};

impl root::llvm::StringRef {
    pub fn as_str<'a>(&self) -> Result<&'a str, str::Utf8Error> {
        unsafe {
            str::from_utf8(slice::from_raw_parts(
                    self.data as *const u8, self.length))
        }
    }
}
