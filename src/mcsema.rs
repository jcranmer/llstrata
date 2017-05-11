use llvm::*;
use llvmmc::*;
use llvm_sys::LLVMModule;
use sema::*;
use state::{InstructionInfo, InstructionState, State};
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[repr(C)]
struct StringRef {
    data: *const u8,
    len: usize
}

fn get_string(string: &str) -> StringRef {
    StringRef {
        data: string.as_ptr(),
        len: string.len()
    }
}

#[repr(C)]
struct MCSemaNotes {
    name: StringRef,
    in_string: StringRef,
    out_string: StringRef
}
extern "C" {
    fn write_file(module: *const LLVMModule, file_name: *const i8,
                  instr_notes: *const MCSemaNotes, instr_count: usize);
}

pub struct TranslationState<'a> {
    module: CSemiBox<'a, Module>,
    builder: CSemiBox<'a, Builder>,
    state: &'a State<'a>
}

// The basic idea behind our translation scheme. We're going to convert each
// operand into a function that computes live_out = opcode(def_in). For example:
// define { i1 } @clc() {
// entry:
//   %xor = call { i64, i1, i1, i1, i1, i1 } @xorq_r64_r64(i64 0, i64 0)
//   %rax = extractvalue %xor, 0
//   %cf = extractvalue %xor, 1
//   %al = trunc i64 %rax to i8
//   %adc8b = call { i8, i1, i1, i1, i1, i1 } @adcb_r8_r8(i8 %al, i8 %al, i1 %cf)
//   %cf2 = extractvalue %adc8b, 1
//   ret %cf2
// }

impl <'a> TranslationState<'a> {
    pub fn new(state: &'a State) -> TranslationState<'a> {
        let ctxt = state.get_llvm_context();
        let xlation = TranslationState {
            module: Module::new("instructions", &ctxt),
            builder: Builder::new(&ctxt),
            state: state
        };
        return xlation;
    }

    fn get_flags(&self, inst: &Instruction) -> (Vec<String>, Vec<String>) {
        let info = self.state.get_info(inst)
            .expect(&format!("Need to implement {:?}", inst));

        fn rewrap(vec: Vec<&str>) -> Vec<String> {
            vec.iter().map(|s| String::from(*s)).collect()
        }
        return (rewrap(info.get_flag_names(false)),
            rewrap(info.get_flag_names(true)));
    }

    pub fn get_function(&'a self, inst: &Instruction) -> &'a Function {
        let ctx = self.module.get_context();
        let mut in_types = Vec::new();
        let mut out_types = Vec::new();

        // Load the known register kinds.
        let mut name_comps = Vec::new();
        name_comps.push(inst.opcode.name);
        let mri = self.state.get_target_triple().register_info();
        let mut real_operands = inst.operands.iter();
        for op in inst.opcode.get_operands().iter() {
            let mut array = if op.write { &mut out_types } else { &mut in_types };
            match op.kind {
                OperandType::Register(ref rc) => {
                    array.push(Type::get::<u64>(&ctx));
                    let real = real_operands.next()
                        .expect("Need matching operand");
                    if rc.name == "GR8" {
                        let reg = real.get_register().unwrap();
                        let top = reg.get_top_register(mri);
                        let (offset, _) = top.get_sub_register_slice(reg, mri)
                            .unwrap();
                        name_comps.push(if offset > 0 { "hi" } else { "lo" });
                    }
                },
                OperandType::FixedRegister(ref reg) => {
                    if is_flags(reg) { continue; }
                    array.push(Type::get::<u64>(&ctx));
                }
                OperandType::TiedRegister(index) => {
                    real_operands.next()
                        .expect("Need matching operand");
                    let other = &inst.opcode.get_operands()[index as usize];
                    assert!(op.write != other.write);
                    match other.kind {
                        OperandType::Register(_) => {
                            array.push(Type::get::<u64>(&ctx));
                        },
                        OperandType::FixedRegister(ref reg) => {
                            if is_flags(reg) { continue; }
                            array.push(Type::get::<u64>(&ctx));
                        },
                        _ => {
                            panic!("Register tied to not-a-register");
                        }
                    }
                }
                _ => {
                    panic!("Shouldn't reach here");
                }
            }
        }

        // Now add in known flag effects.
        let (in_flags, out_flags) = self.get_flags(inst);
        for _ in in_flags {
            in_types.push(Type::get::<bool>(&ctx));
        }
        for _ in out_flags {
            out_types.push(Type::get::<bool>(&ctx));
        }

        // Get the function, if available.
        let func_name = name_comps.join("_");
        if let Some(f) = self.module.get_function(&func_name) {
            return f;
        }

        // If not, add it to the module.
        let struct_ty = StructType::new(&ctx, &out_types, false);
        let fn_ty = FunctionType::new(struct_ty, &in_types);
        let function = self.module.add_function(&func_name, fn_ty);
        return function;
    }

    fn get_registers(&self, inst: &'a Instruction) -> RegInfo<'a> {
        let mut result : RegInfo<'a> = Default::default();
        let mut op_index = 0;
        for op in inst.opcode.get_operands() {
            let mut array = if op.write { &mut result.0 } else { &mut result.1 };

            if op.implicit {
                if let OperandType::FixedRegister(reg) = op.kind {
                    if is_flags(reg) { continue; }
                    array.push(reg);
                } else {
                    panic!("Implicit operands should be fixed registers");
                }
            } else {
                let real_op = &inst.operands[op_index];
                op_index += 1;
                match op.kind {
                    OperandType::Register(_) | OperandType::TiedRegister(_) => {
                        if let &Operand::Register(r) = real_op {
                            array.push(r);
                        } else {
                            panic!("Expected a register here");
                        }
                    },
                    _ => {
                        unimplemented!();
                    }
                }
            }
        }
        return result;
    }

    fn get_register(&'a self, reg: &Register, mri: &RegisterInfo,
                    reg_state: &RegState<'a>) -> &'a Value {
        let top_level = reg.get_top_register(mri);
        let full_value = reg_state.0.get(top_level).unwrap();
        return full_value;
    }

    fn get_flag(&self, flag: &str, reg_state: &RegState<'a>) -> &'a Value {
        reg_state.1.get(flag).expect("Should have entered flag first")
    }

    fn set_register(&self, reg: &'a Register, mri: &'a RegisterInfo,
                    reg_state: &mut RegState<'a>, val: &'a Value) {
        let top_level = reg.get_top_register(mri);
        reg_state.0.insert(top_level, val);
        val.set_name(reg.name);
    }
    
    fn set_flag(&self, flag: &str, reg_state: &mut RegState<'a>, val: &'a Value) {
        reg_state.1.insert(String::from(flag), val);
        val.set_name(flag);
    }
}

type RegInfo<'a> = (Vec<&'a Register>, Vec<&'a Register>);
type RegState<'a> = (HashMap<&'a Register, &'a Value>,
                     HashMap<String, &'a Value>);

fn is_flags(reg: &Register) -> bool {
    return reg.name == "EFLAGS";
}

fn parse_asm_file<'a>(tt: &'a TargetTriple,
                      path: &Path) -> Vec<Instruction<'a>> {
    let mut file = File::open(path).expect("Could not find file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Could not read file");
    if contents.contains("callq") {
        println!("Found call, skipping for now");
        return Vec::new();
    }
    return tt.parse_instructions("", "", &contents);
}

pub fn translate_instruction<'a>(inst: &InstructionInfo,
                             state: &State,
                             xlation: &TranslationState,
                             tt: &'a TargetTriple) {
    let ctxt = state.get_llvm_context();
    let builder = &xlation.builder;

    let mri = tt.register_info();
    println!("Translating {}", inst.opcode);
    let base = parse_asm_file(tt, &inst.get_inst_file(state));
    assert!(base.len() == 2, "We expect the instruction file to have little");
    assert!(base[base.len() - 1].opcode.is_return());

    let representative = &base[0];

    // Print out the header of the function for this instruction.
    let function = xlation.get_function(&base[0]);

    let insts = parse_asm_file(tt, &inst.get_circuit_file(state));
    if insts.is_empty() {
        return; // XXX
    }

    // Set up a register map to keep track of the various registers.
    let top_level_regs = tt.register_info().get_top_level_registers();
    let mut reg_state : RegState = Default::default();
    for register in top_level_regs {
        // XXX: we need to size this better.
        xlation.set_register(register, mri, &mut reg_state, 0u64.compile(&ctxt));
    }

    // For input...
    let (output_registers, input_registers) =
        xlation.get_registers(representative);
    let (input_flags, output_flags) = xlation.get_flags(representative);
    for (i, register) in input_registers.iter().enumerate() {
        xlation.set_register(register, mri, &mut reg_state, &*function[i]);
    }
    for (i, flag) in input_flags.iter().enumerate() {
        xlation.set_flag(flag, &mut reg_state, &*function[i + input_flags.len()]);
    }

    // Build the actual code of the basic block.
    let block = function.append("entry");
    builder.position_at_end(block);

    for step in &insts[0..insts.len() - 1] {
        let inner = xlation.get_function(step);
        let (defined, set) = xlation.get_registers(step);
        let (in_flags, out_flags) = xlation.get_flags(step);
        let args : Vec<&Value> = set.iter()
            .map(|reg| xlation.get_register(reg, mri, &reg_state))
            .chain(in_flags.iter()
                   .map(|flag| xlation.get_flag(flag, &reg_state))
            ).collect();
        let call = builder.build_call(inner, &args);
        for (i, reg) in defined.iter().enumerate() {
            xlation.set_register(reg, mri, &mut reg_state,
                builder.build_extract_value(call, i));
        }
        for (i, flag) in out_flags.iter().enumerate() {
            xlation.set_flag(flag, &mut reg_state,
                builder.build_extract_value(call, i + defined.len()));
        }
    }

    // And the return value.
    let mut ret_val = Value::new_undef(function.get_signature().get_return());
    for (i, reg) in output_registers.iter().enumerate() {
        ret_val = builder.build_insert_value(ret_val,
            xlation.get_register(reg, mri, &reg_state), i);
    }
    for (i, flag) in output_flags.iter().enumerate() {
        ret_val = builder.build_insert_value(ret_val,
            xlation.get_flag(flag, &reg_state), i + output_registers.len());
    }
    builder.build_ret(ret_val);
}

pub fn write_translations(state: &TranslationState, file: &Path) {
    let module = &state.module;
    // Verify the module to make sure we built it correctly.
    module.verify().unwrap();

    // Optimization!
    module.optimize(3, 3);
    add_base_programs(module, &state.builder, &state,
                      &state.state.get_target_triple());
    module.verify().unwrap();
    module.optimize(3, 0);

    // Build the instruction notes for the output.
    // Keep strings alive for the FFI call.
    let mut keep_alive = Vec::new();
    let mut known_insts = Vec::new();
    for inst in state.state.get_instructions(InstructionState::Success) {
        let base = parse_asm_file(state.state.get_target_triple(),
            &inst.get_inst_file(state.state));
        let name = base[0].opcode.name;
        if module.get_function(base[0].opcode.name).is_some() {
            // Strata generates SAL and SHL, but these are two mnemonics for
            // the same opcode.
            if inst.opcode.starts_with("sal") {
                continue;
            }
            let mut in_parts = Vec::new();
            let mut out_parts = Vec::new();
            // XXX: This is broken for fixed register instructions :-(
            let pair_iter = base[0].operands.iter().zip(
                base[0].opcode.get_operands().iter());
            for (i, (real, ty)) in pair_iter.enumerate() {
                let mut parts = if ty.write {
                    &mut out_parts
                } else {
                    &mut in_parts
                };
                if real.get_register().is_some() {
                    parts.push(i.to_string());
                }
            }
            let (mut in_flags, mut out_flags) = state.get_flags(&base[0]);
            in_flags = in_flags.iter().map(|f| f.to_uppercase()).collect();
            out_flags = out_flags.iter().map(|f| f.to_uppercase()).collect();
            in_parts.append(&mut in_flags);
            out_parts.append(&mut out_flags);
            keep_alive.push(in_parts.join(" "));
            let in_str = get_string(keep_alive.last().unwrap());
            keep_alive.push(out_parts.join(" "));
            let out_str = get_string(keep_alive.last().unwrap());
            known_insts.push(MCSemaNotes {
                name: get_string(name),
                in_string: in_str,
                out_string: out_str
            });
        }
    }

    unsafe {
        use std::ffi::CString;

        let filename = CString::new(file.to_str().unwrap()).unwrap();
        write_file(state.module.as_ptr(), filename.as_ptr(),
                   known_insts.as_slice().as_ptr(), known_insts.len());
    }
    //println!("{:?}", module);
}
