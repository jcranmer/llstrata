mod bindgen;
mod register;
mod target_triple;

pub mod target;

pub use register::*;
pub use target_triple::TargetTriple;

#[cfg(test)]
mod tests {
  use super::target::TargetTriple;
    #[test]
    fn test_triple_construction() {
      assert!(TargetTriple::get("x86_64-unknown-linux-gnu").is_ok());
      assert!(TargetTriple::get("i am not a valid triple").is_err());
    }
}
