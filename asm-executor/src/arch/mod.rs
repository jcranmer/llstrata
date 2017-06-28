use std::fmt;
use std::mem;
use std::slice;

pub fn print_bytes<T>(f: &mut fmt::Formatter, t: &T) -> fmt::Result {
    let size = mem::size_of::<T>();
    let bytes = t as *const T as *const u8;
    return print_hex(f, unsafe { slice::from_raw_parts(bytes, size) });
}

pub fn as_mut_bytes<T>(val: &mut T) -> &mut [u8] {
    let size = mem::size_of::<T>();
    let bytes = val as *mut T as *mut u8;
    return unsafe { slice::from_raw_parts_mut(bytes, size) };
}

pub fn as_bytes<T>(val: &T) -> &[u8] {
    let size = mem::size_of::<T>();
    let bytes = val as *const T as *const u8;
    return unsafe { slice::from_raw_parts(bytes, size) };
}

pub fn print_hex(f: &mut fmt::Formatter, t: &[u8]) -> fmt::Result {
    for byte in t.iter().rev() {
        write!(f, " {:02x}", byte)?;
    }
    return Ok(());
}

#[allow(dead_code)]
pub mod amd64;

#[cfg(target_arch="x86_64")]
pub use arch::amd64 as host;
