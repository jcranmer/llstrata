extern crate llvmmc;

use llvmmc::target::TargetTriple;

fn main() {
  let tt = TargetTriple::get("x86_64-unknown-linux-gnu")
    .expect("Better compile LLVM with x86 support!");
  for inst in &tt.get_instructions() {
    println!("{:?}", inst);
  }
}
