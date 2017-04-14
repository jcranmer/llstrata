extern crate llvmmc;

use llvmmc::target::{OperandInfo, TargetTriple};

fn main() {
  let tt = TargetTriple::get("x86_64-unknown-linux-gnu")
    .expect("Better compile LLVM with x86 support!");
  for inst in &tt.get_instructions() {
    // Ignore memory and control-flow instructions
    if inst.get_operands().iter().any(|o| match o {
      &OperandInfo::Mem => true,
      &OperandInfo::PCRel => true,
      _ => false}) {
      continue;
    }
    print!("{}", inst.get_name());
    let mut next_char = " ";
    for op in inst.get_operands() {
      print!("{}", next_char);
      next_char = ", ";
      if let OperandInfo::Register(num) = op {
        print!("{}", tt.get_register_class(num as u32).name);
      } else if let OperandInfo::TiedRegister(index) = op {
        print!("={}", index);
      } else {
        print!("{:?}", op);
      }
    }
    println!("");
  }
}
