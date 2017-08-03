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

fn check_base(state: &mut state::State) -> io::Result<()> {
    let funcs = sema::FunctionInfo::get_functions();
    let mut unseen : HashSet<&'static str> = funcs.keys().map(|k| *k).collect();
    let function_dir = state.get_workdir().join("functions");

    // Get the pasth to the asm-executor binary
    let mut asm_dir = env::current_exe()?;
    asm_dir.pop();
    asm_dir.push("asm-executor");

    let testcase = state.get_workdir().join("testcases.tc");

    let xlation = mcsema::TranslationState::new(state);
    sema::add_base_programs(&xlation.module, &xlation.builder,
                            &xlation, state.get_target_triple());

    for file in function_dir.read_dir()? {
        let entry = file?;
        let path = entry.path();
        let name = path.file_stem().expect("Should be a .s")
            .to_str().expect("Should be ASCII");
        let mri = state.get_target_triple().register_info();
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

            // Compute the read/write registers for this function.
            fn get_reg_string(mri: &llvmmc::RegisterInfo,
                              registers: &[&str]) -> (String, String) {
                let regs : (Vec<_>, Vec<_>) = registers.iter()
                    .map(|reg| {
                        let reg_upper = reg.to_uppercase();
                        let llreg = mri.get_register(&reg_upper);
                        if let Some(llreg) = llreg {
                            let top = llreg.get_top_register(mri);
                            if top == llreg {
                                return (top.name, None);
                            }
                            let offset = top.get_sub_register_slice(llreg, mri)
                                .expect("Not in top level?");
                            return (top.name, Some(offset));
                        }
                        return (reg, None);
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
                    .unzip();
                let reg_str = regs.0.join(" ");
                return (reg_str, regs.1.join(" "));
            }
            let (in_regs, _) = get_reg_string(mri, &info.def_in);
            let (out_regs, out_valid) = get_reg_string(mri, &info.live_out);

            // Dump the function to a file. Since we have a global module, just
            // copy that to a new module and pull at the single function.
            let mut temp_ir_path = env::temp_dir();
            temp_ir_path.push("test-function.ll");
            let mut temp_ir = File::create(&temp_ir_path)?;
            let func = xlation.get_function_for_pseudo(info);
            let module = xlation.module.clone();
            unsafe {
                use llvm_sys::core::*;
                use llvm_sys::prelude::*;
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
                unsafe fn set_attr(func: LLVMValueRef, name: &str, val: &str) {
                    let attr = LLVMCreateStringAttribute(
                        LLVMGetTypeContext(LLVMTypeOf(func)),
                        name.as_ptr() as *const i8,
                        name.len() as u32,
                        val.as_ptr() as *const i8,
                        val.len() as u32);
                    LLVMAddAttributeAtIndex(func, !0, attr);
                }
                let module : &llvm::Module = &module;
                let module : LLVMModuleRef = module.into();
                let mut fnptr = LLVMGetFirstFunction(module);
                loop {
                    let next = LLVMGetNextFunction(fnptr);
                    let clean_fn : &llvm::Function = fnptr.into();
                    if clean_fn.get_name() == func.get_name() {
                        // Add in/out register parameters.
                        set_attr(fnptr, "in", &in_regs);
                        set_attr(fnptr, "out", &out_regs);
                        // We need this to generate correct bindings for ymm registers.
                        set_attr(fnptr, "target-features", "+avx");
                        // Use regcall calling convention
                        LLVMSetFunctionCallConv(fnptr, 92);
                    } else if !LLVMGetFirstBasicBlock(fnptr).is_null() {
                        LLVMDeleteFunction(fnptr);
                    }
                    fnptr = next;
                    if fnptr.is_null() { break; }
                }
            }
            write!(temp_ir, "{:?}", module)?;
            temp_ir.flush()?;

            // Copy the asm file and assemmble it.
            let mut temp_s_path = env::temp_dir();
            temp_s_path.push("gold-function.s");
            let mut temp_o_path = env::temp_dir();
            temp_o_path.push("gold-function.o");
            let fix_shell = process::Command::new("sed")
                .arg("-e").arg(&format!("s/\\.{0}/{0}/g", name))
                .arg("-e").arg(r"s/$0x\([8-f][0-9a-f]\{7\}\)/$0xffffffff\1/")
                .arg(&path)
                .output()?;
            let mut s_file = File::create(&temp_s_path)?;
            s_file.write(&fix_shell.stdout)?;
            s_file.flush()?;

            // Assemble it now...
            let assemble = process::Command::new("clang")
                .arg("-c").arg(&temp_s_path)
                .arg("-o").arg(&temp_o_path)
                .status()?;
            if !assemble.success() {
                println!("Assembly function {} failed", name);
                continue;
            }

            let success = process::Command::new(&asm_dir)
                .arg(&testcase)
                .arg(&temp_ir_path)
                .arg(&temp_o_path)
                .arg(&out_valid)
                .status()?;

            if !success.success() {
                println!("Assembly function {} failed", name);
                println!("out_valid: {:?}", out_valid);
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
