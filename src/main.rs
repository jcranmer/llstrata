extern crate getopts;
extern crate llvmmc;
extern crate llvm;
extern crate llvm_sys;
extern crate serde_json;
extern crate tempdir;

mod mcsema;
mod sema;
mod state;
mod stoke;

use getopts::Options;
use llvmmc::TargetTriple;
use std::env;
use std::fs::metadata;
use std::path::Path;
use std::process;

fn print_usage(prog: &str, opts: Options) {
    let brief = format!("Usage: {} [OPTIONS] MODE
    Infer specifications of instruction sets using LLVM and STOKE.

    MODE may be one of:
      init     Initialize a working directory
      status   Show the status of the learning process
      generate Generate MCSema lifiting code for the known instructions
      run      Run the program indefinitely
      step     Try to learn a single instruction", prog);
    print!("{}", opts.usage(&brief));
    process::exit(1);
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

    if matches.free.is_empty() {
        println!("Error: missing mode parameter");
        print_usage(&program, opts);
        return;
    }

    let mode = &matches.free[0];
    let sub_opts = &matches.free[1..];

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

    // Load the work directory for the state option.
    let work_dir = if let Some(dir) = matches.opt_str("w") {
        Path::new(&dir).to_path_buf()
    } else {
        let mut buf = env::home_dir().expect("Cannot find home directory");
        buf.push("dev");
        buf.push("output-strata");
        buf
    };

    if mode == "init" {
        unimplemented!();
    } else {
        if let Err(err) = metadata(&work_dir) {
          println!("Error reading working directory {:?}: {}", work_dir, err);
          process::exit(1);
        }
    }

    let mut state = state::State::load(work_dir, &tt);

    match mode.as_ref() {
        "status" => {
            status(&mut state);
        },
        "run" => unimplemented!(),
        "step" => {
            step(&mut state);
        },
        "generate" => {
            if sub_opts.is_empty() {
                println!("Needs output file as argument");
                return;
            }
            let out = Path::new(&sub_opts[0]);
            generate(&state, &out);
        },
        _ => {
            println!("Unknown command {}", mode);
            print_usage(&program, opts);
            return;
        }
    }
}

fn status(state: &mut state::State) {
    let base = state.get_instructions(state::InstructionState::Base).len();
    let success = state.get_instructions(state::InstructionState::Success).len() - base;
    let partial = state.get_instructions(state::InstructionState::PartialSuccess).len();
    let unsolved = state.get_instructions(state::InstructionState::Unsolved).len();
    let total = (base + success + partial + unsolved) as f64 / 100f64;
    println!("Instruction information");
    println!("═══════════════════════");
    println!("       Base set: {:>5} {:>5.2}%", base, base as f64 / total);
    println!("        Success: {:>5} {:>5.2}%", success, success as f64 / total);
    println!("Partial success: {:>5} {:>5.2}%", partial, partial as f64 / total);
    println!("  Failed search: ????????????");
    println!("      Remaining: {:>5} {:>5.2}%", unsolved, unsolved as f64 / total);
    println!("");
    println!("Elapsed time");
    println!("════════════");
    println!("Wall-clock time: <unknown>");
    println!("       CPU time: <unknown>");
}

fn step(state: &mut state::State) {
    let instrs = state.get_instructions(state::InstructionState::Unsolved);
    let setnae = instrs.iter().find(|&inst| inst.opcode == "setnae_rh")
        .expect("Blah blah");
    stoke::search_instruction(state, &setnae);
}

fn generate(state: &state::State, output: &Path) {
    let base = state.get_instructions(state::InstructionState::Base);
    let translation_state = mcsema::TranslationState::new(state);
    for inst in state.get_instructions(state::InstructionState::Success) {
        if base.iter().any(|i| *i == inst) {
            continue;
        }
        // XXX: skip for now
        if inst.opcode == "movzbq_r64_r8" { continue; }
        if inst.opcode == "movzbl_r32_rh" { continue; }
        if inst.opcode == "decb_r8" { continue; }
        mcsema::translate_instruction(&inst, state, &translation_state, state.get_target_triple());
        if inst.opcode == "setae_r8" {
            break;
        }
    }
    mcsema::write_translations(&translation_state, output);
}
