extern crate getopts;
extern crate llvmmc;

use getopts::Options;
use llvmmc::target::{OperandType, TargetTriple};
use std::env;

fn print_usage(prog: &str, opts: Options) {
    let brief = format!("Usage: {} [OPTIONS]
    Infer specifications of instruction sets using LLVM and STOKE.", prog);
    print!("{}", opts.usage(&brief));
}

fn main() {
    let args : Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("h", "help", "Show help");
    opts.optflag("v", "verbose", "Show verbose results");
    opts.optopt("t", "triple", "Triple to use for target", "TRIPLE");
    opts.optopt("w", "workdir",
                "Working directory for results and intermediates", "DIR");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string())
    };

    if matches.opt_present("h") {
        print_usage(&program, opts);
        return;
    }

    // Load the triple, default to x86.
    // XXX: Should probably default to $HOST_CPU.
    let triple_str = matches.opt_str("t")
        .unwrap_or(String::from("x86_64-unknown-linux-gnu"));
    let tt_wrapped = TargetTriple::get(&triple_str);
    let tt = match tt_wrapped {
        Err(s) => {
            println!("Target {} is unknown: {}", triple_str, s);
            return;
        },
        Ok(tt) => tt
    };

    for inst in &tt.get_instructions() {
        // Ignore memory and control-flow instructions
        if inst.get_operands().iter().any(|o| match o.kind {
            OperandType::Mem => true,
            OperandType::PCRel => true,
            _ => false}) {
            continue;
        }
        print!("{}", inst.get_name());
        let mut next_char = " ";
        for op in inst.get_operands() {
            print!("{}", next_char);
            next_char = ", ";
            if let OperandType::Register(num) = op.kind {
                print!("{}", tt.get_register_class(num as u32).name);
            } else if let OperandType::TiedRegister(index) = op.kind {
                print!("={}", index);
            } else if let OperandType::FixedRegister(reg) = op.kind {
                print!("{}", reg.name);
            } else {
                print!("{:?}", op.kind);
            }
            print!("[{}]", if op.write { "def" } else { "use" });
            if op.implicit {
                print!(" (implicit)");
            }
        }
        println!("");
    }
}
