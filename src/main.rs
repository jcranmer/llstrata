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
use std::collections::HashSet;
use std::env;
use std::fs::{File, metadata};
use std::io::{self, Read, Write};
use std::path::Path;
use std::process;

fn print_usage(prog: &str, opts: Options) {
    let brief = format!("Usage: {} [OPTIONS] MODE
    Infer specifications of instruction sets using LLVM and STOKE.

    MODE may be one of:
      check-base Check the correctness of the base instructions
      init       Initialize a working directory
      status     Show the status of the learning process
      generate   Generate MCSema lifiting code for the known instructions
      run        Run the program indefinitely
      step       Try to learn a single instruction", prog);
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
        "check-base" => {
            check_base(&mut state).unwrap();
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
        mcsema::translate_instruction(&inst, state, &translation_state, state.get_target_triple());
        if inst.opcode == "vmovupd_xmm_xmm" {
            break;
        }
    }
    mcsema::write_translations(&translation_state, output);
}

fn assemble_code(s_file: &Path, o_file: &Path) -> io::Result<bool> {
    let status = process::Command::new("clang")
        .arg("-c").arg(s_file)
        .arg("-o").arg(o_file)
        .status()?;
    return Ok(status.success());
}

fn assemble_llvm(ll_file: &Path, module: &llvm::Module, name: &str,
                 in_regs: &str, out_regs: &str) -> io::Result<()> {
    // Clone the module to get just the function of interest. This is more
    // annoying than it should be, because it's not quite so easy to pass
    // the necessary information to the asm-executor.
    let clone = module.clone();

    // Delete all the other functions in the module. Go into unsafe mode to do
    // this, because the llvm crate doesn't expose this functionality (yuck).
    // While we do this, we're going to clean up the function with some more
    // data for the asm-executor:
    // * set the calling convention to x86_regcallcc (more things get passed in
    //   registers)
    // * add in/out attributes indicating the register mappings.
    // * set the target-features to include AVX support (to make sure we use
    //   the YMM registers instead of XMM).
    unsafe {
        use llvm_sys::core::*;
        use llvm_sys::prelude::*;
        // Arrggrgrabh... we're pinned to a bad revision of llvm_sys, so we
        // need to define these functions ourselves. Hurry up and fix your
        // replacement crate!
        enum LLVMOpaqueAttributeRef {};
        type LLVMAttributeRef = *mut LLVMOpaqueAttributeRef;
        extern "C" {
            fn LLVMCreateStringAttribute(C: LLVMContextRef,
                                         K: *const i8,
                                         KLen: u32,
                                         V: *const i8,
                                         VLen: u32) -> LLVMAttributeRef;
            fn LLVMAddAttributeAtIndex(F: LLVMValueRef,
                                       Idx: u32, A: LLVMAttributeRef);
        }

        // Helper for setting attributes.
        unsafe fn set_attr(func: LLVMValueRef, name: &str, val: &str) {
            let attr = LLVMCreateStringAttribute(
                LLVMGetTypeContext(LLVMTypeOf(func)),
                name.as_ptr() as *const i8,
                name.len() as u32,
                val.as_ptr() as *const i8,
                val.len() as u32);
            LLVMAddAttributeAtIndex(func, !0, attr);
        }

        use std::ops::Deref;
        let module : LLVMModuleRef = clone.deref().into();
        let mut fnptr = LLVMGetFirstFunction(module);
        loop {
            let next = LLVMGetNextFunction(fnptr);
            let clean_fn : &llvm::Function = fnptr.into();
            if clean_fn.get_name() == Some(name) {
                set_attr(fnptr, "in", in_regs);
                set_attr(fnptr, "out", out_regs);
                set_attr(fnptr, "target-features", "+avx");
                LLVMSetFunctionCallConv(fnptr, 92);
            } else if !LLVMGetFirstBasicBlock(fnptr).is_null() {
                LLVMDeleteFunction(fnptr);
            }
            fnptr = next;
            if fnptr.is_null() { break; }
        }
    }

    let mut output = File::create(&ll_file)?;
    write!(output, "{:?}", clone)?;
    output.flush()?;
    return Ok(());
}

fn get_reg_string(mri: &llvmmc::RegisterInfo,
                  registers: Vec<&llvmmc::Register>, flags: Vec<String>)
                  -> (String, String) {
    let regs : (Vec<_>, Vec<_>) = registers.into_iter()
        .map(|llreg| {
            let top = llreg.get_top_register(mri);
            if top == llreg {
                return (top.name, None);
            }
            let offset = top.get_sub_register_slice(llreg, mri)
                .expect("Not in top level?");
            return (top.name, Some(offset));
        }).map(|(reg, off)| (reg.to_lowercase(), off))
    .map(|(mut reg, off)| {
        if reg.starts_with("zmm") {
            unsafe { reg.as_mut_vec()[0] = b'y' }
        }
        (reg, off)
    })
    .map(|(reg, off)| {
        if let Some(off) = off {
            let output = format!("{}:{}", &reg, off.1 / 8);
            (reg, output)
        } else {
            (reg.clone(), reg)
        }
    })
    .chain(flags.into_iter().map(|s| (s.clone(), s)))
    .unzip();
    let reg_str = regs.0.join(" ");
    return (reg_str, regs.1.join(" "));
}

fn verify_equivalence(s_file: &Path, module: &llvm::Module, name: &str,
                      state: &state::State, mri: &llvmmc::RegisterInfo,
                      reg_state: mcsema::RegInfo,
                      flag_state: mcsema::FlagInfo) -> io::Result<bool> {
    // Assemble the .s code.
    let mut o_file = env::temp_dir();
    o_file.push("gold-function.o");
    if !assemble_code(s_file, &o_file)? {
        return Ok(false);
    }

    // Compute the read/write registers for the function.
    let (in_regs, _) = get_reg_string(mri, reg_state.0, flag_state.0);
    let (out_regs, out_valid) = get_reg_string(mri, reg_state.1, flag_state.1);

    // Assemble the .ll file.
    let mut ll_file = env::temp_dir();
    ll_file.push("test-function.ll");
    assemble_llvm(&ll_file, module, name, &in_regs, &out_regs)?;

    // Get the pasth to the asm-executor binary
    let mut asm_dir = env::current_exe()?;
    asm_dir.pop();
    asm_dir.push("asm-executor");

    // Run the code!
    let success = process::Command::new(&asm_dir)
        .arg(&state.get_workdir().join("testcases.tc"))
        .arg(&ll_file)
        .arg(&o_file)
        .arg(&out_valid)
        .status()?;

    return Ok(success.success());
}

fn check_base(state: &mut state::State) -> io::Result<()> {
    let funcs = sema::FunctionInfo::get_functions();
    let mut unseen : HashSet<&'static str> = funcs.keys().map(|k| *k).collect();
    let function_dir = state.get_workdir().join("functions");

    // Add in all the base programs so we get all the LLVM IR.
    let xlation = mcsema::TranslationState::new(state);
    sema::add_base_programs(&xlation.module, &xlation.builder,
                            &xlation, state.get_target_triple());
    let mri = state.get_target_triple().register_info();

    println!("Checking base instructions...");
    let mut base_insts = sema::get_base_instructions();
    for inst in state.get_instructions(state::InstructionState::Base) {
        let s_path = inst.get_inst_file(state);
        let base_inst = base_insts.remove(inst.opcode.as_str());
        if base_inst.is_none() {
            println!("Missing base instruction {}", inst.opcode);
            continue;
        }
        let base_inst = base_inst.unwrap();

        if base_inst.inst_info != inst {
            println!("Error in instruction info metadata for {}", inst.opcode);
            println!("LLStrata: {:?}", base_inst.inst_info);
            println!("Strata: {:?}", inst);
        }

        // Read the .s file from strata, compare it to our own string.
        let mut s_file = File::open(&s_path)?;
        let mut contents = String::new();
        s_file.read_to_string(&mut contents)?;
        let expected = format!(".target:\n  {}  retq\n", base_inst.sample);
        if contents != expected {
            println!("Base instruction {} differs from STRATA:", inst.opcode);
            println!("LLStrata:\n{}", expected);
            println!("STRATA:\n{}", contents);
            continue;
        }

        // Run the test equivalence for the base program
        let llvm_inst = xlation.get_llvm_instruction(&inst);
        let registers = xlation.get_registers(&llvm_inst);
        let flags = xlation.get_flags(&llvm_inst);

        // Since there's no support in asm-executor for immediate parameters,
        // this produces a spurious false positive.
        if inst.opcode == "movq_r64_imm64" {
            continue;
        }

        // LLVM's codegen seems busted in this case.
        if inst.opcode == "vzeroall" {
            continue;
        }

        // This requires fma support, sigh.
        if inst.opcode.contains("132") {
            continue;
        }

        // LLVM function name
        let func = xlation.get_function(&llvm_inst);
        if !verify_equivalence(&s_path, &xlation.module,
                               func.get_name().unwrap(), state,
                               mri, registers, flags)? {
            println!("Base instruction {} failed", inst.opcode);
            panic!("die here");
        }
    }

    if base_insts.len() > 0 {
        println!("Spurious base instructions: {:?}",
                 base_insts.keys().collect::<Vec<_>>());
    }

    println!("Checking base functions...");
    for file in function_dir.read_dir()? {
        let entry = file?;
        let path = entry.path();
        let name = path.file_stem().expect("Should be a .s")
            .to_str().expect("Should be ASCII");
        if let Some(info) = funcs.get(name) {
            unseen.remove(name);
            let mut file = File::open(&path)?;
            let mut contents = String::new();
            file.read_to_string(&mut contents)?;

            // Fix up label references to be unique.
            let mut mangle_assembly = String::from(info.assembly);
            if let Some(lbl_index) = contents.find(".lbl") {
                let more = contents.split_at(lbl_index).1;
                let end_index = more.find("\n").unwrap();
                mangle_assembly = mangle_assembly
                    .replace(".lbl", more.split_at(end_index - 1).0);
            }
            if name.starts_with("move_") && name.find("byte").is_none() {
                if name.find("xmm").is_none() {
                    mangle_assembly = mangle_assembly.replace("  #\n", "");
                }
            }
            if name.starts_with("read_") {
                mangle_assembly = mangle_assembly
                    .replace("#! must-undef { }\n", "");
            }
            let last_retq = contents.rfind("  retq").unwrap();
            let size = contents.rfind(".size").unwrap();
            if last_retq < size {
                let end = mangle_assembly.rfind("  retq").unwrap();
                mangle_assembly.truncate(end);
            }

            if contents.trim() != mangle_assembly.trim() {
                println!("Assembly function {} differs from STRATA:", name);
                println!("LLStrata:\n{}", mangle_assembly);
                println!("STRATA:\n{}", contents);
                continue;
            }

            // XXX: AVX-2 instruction, can't test on huitzilopochtli.
            if contents.contains("vpbroadcastw") {
                continue;
            }

            // Copy the asm file and assemble it.
            let mut temp_s_path = env::temp_dir();
            temp_s_path.push("gold-function.s");

            // STOKE uses a broken assembly format, fix it.
            let fix_shell = process::Command::new("sed")
                // .SYM and SYM are treated as the same symbol name.
                .arg("-e").arg(&format!("s/\\.{0}/{0}/g", name))
                // Sign extend immediate parameters written in hex.
                .arg("-e").arg(r"s/$0x\([8-f][0-9a-f]\{7\}\)/$0xffffffff\1/")
                .arg(&path)
                .output()?;
            let mut s_file = File::create(&temp_s_path)?;
            s_file.write(&fix_shell.stdout)?;
            s_file.flush()?;

            let (registers, flags) = xlation.get_registers_for_pseudo(info);
            if !verify_equivalence(&temp_s_path, &xlation.module, name, state,
                                   mri, registers, flags)? {
                println!("Assembly function {} failed", name);
                panic!("die here");
            }
        } else {
            println!("Unknown assembly function {}", name);
        }
    }
    for func in unseen {
        println!("Spurious function {}", func);
    }
    return Ok(());
}
