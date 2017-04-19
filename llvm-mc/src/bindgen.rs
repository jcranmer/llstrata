include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

impl <'a> From<&'a str> for root::llvm::StringRef {
    fn from(s: &'a str) -> Self {
        root::llvm::StringRef {
            data: s.as_ptr() as *const i8,
            length: s.len()
        }
    }
}
