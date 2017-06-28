#[allow(dead_code)]
pub mod amd64;

#[cfg(target_arch="x86_64")]
pub use arch::amd64 as host;
