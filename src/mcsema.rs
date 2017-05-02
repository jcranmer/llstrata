use llvm::*;
use llvmmc::*;
use llvm_sys::LLVMModule;
use state::{InstructionInfo, State};
use std::collections::HashMap;
use std::path::Path;

extern {
    fn write_file(module: *const LLVMModule, file_name: *const i8);
}

pub struct TranslationState<'a> {
    module: CSemiBox<'a, Module>,
    builder: CSemiBox<'a, Builder>,
    state: &'a State<'a>
}

pub type TranslationCache<'a> = HashMap<InstructionInfo, Vec<Instruction<'a>>>;

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

    fn get_function(&'a self, inst: &Instruction) -> &'a Function {
        let func_name = inst.opcode.name;
        if let Some(f) = self.module.get_function(func_name) {
            return f;
        }

        let ctx = self.module.get_context();
        let mut in_types = Vec::new();
        let mut out_types = Vec::new();

        // Load the known register kinds.
        for op in inst.opcode.get_operands() {
            let mut array = if op.write { &mut out_types } else { &mut in_types };
            match op.kind {
                OperandType::Register(ref rc) => {
                    array.push(Type::get::<u64>(&ctx));
                },
                OperandType::FixedRegister(ref reg) => {
                    if is_flags(reg) { continue; }
                    array.push(Type::get::<u64>(&ctx));
                }
                OperandType::TiedRegister(index) => {
                    let other = &inst.opcode.get_operands()[index as usize];
                    assert!(op.write != other.write);
                    match other.kind {
                        OperandType::Register(ref rc) => {
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
        for flag in in_flags {
            in_types.push(Type::get::<bool>(&ctx));
        }
        for flag in out_flags {
            out_types.push(Type::get::<bool>(&ctx));
        }

        // Get the function.
        let structTy = StructType::new(&ctx, &out_types, false);
        let fnTy = FunctionType::new(structTy, &in_types);
        let function = self.module.add_function(func_name, fnTy);
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
        let value = if let Some((offset, size)) =
                top_level.get_sub_register_slice(reg, mri) {
            self.builder.build_ashr(full_value, 0u64.compile(full_value.get_context()))
        } else {
            full_value
        };
        return value;
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

    // XXX: need a better way to do this.
    fn add_base(&self, mri: &RegisterInfo) {
        {
        let func = self.module.get_function("XOR64rr").unwrap();
        let ctx = self.module.get_context();
        self.builder.position_at_end(func.append("entry"));
        let builder = &self.builder;
        let mut reg_state: RegState = Default::default();
        let lhs = &*func[0];
        let rhs = &*func[1];
        let res = builder.build_or(builder.build_and(lhs, builder.build_not(rhs)),
                                   builder.build_and(builder.build_not(lhs), rhs));
        let cf = false.compile(ctx);
        let pf = false.compile(ctx); // XXX
        let zf = builder.build_cmp(res, 0u64.compile(ctx), Predicate::Equal);
        let sf = builder.build_cmp(res, 0u64.compile(ctx), Predicate::LessThan);
        let of = false.compile(ctx);
        let mut ret = Value::new_undef(func.get_signature().get_return());
        ret = builder.build_insert_value(ret, res, 0);
        ret = builder.build_insert_value(ret, cf, 1);
        ret = builder.build_insert_value(ret, pf, 2);
        ret = builder.build_insert_value(ret, zf, 3);
        ret = builder.build_insert_value(ret, sf, 4);
        ret = builder.build_insert_value(ret, of, 5);
        builder.build_ret(ret);
        }
        {
        let func = self.module.get_function("ADC8rr").unwrap();
        let ctx = self.module.get_context();
        self.builder.position_at_end(func.append("entry"));
        let builder = &self.builder;
        let mut reg_state: RegState = Default::default();
        let lhs = builder.build_trunc(&*func[0], Type::get::<u8>(ctx));
        let rhs = builder.build_trunc(&*func[1], Type::get::<u8>(ctx));
        let in_cf = builder.build_zext(&*func[2], Type::get::<u8>(ctx));
        let full = builder.build_call(self.module.add_function("llvm.uadd.with.overflow.i8",
            FunctionType::new(StructType::new(ctx, &[Type::get::<u8>(ctx), Type::get::<bool>(ctx)], false),
                              &[Type::get::<u8>(ctx), Type::get::<u8>(ctx)])),
                              &[lhs, rhs]);
        let res = builder.build_extract_value(full, 0);
        let cf = builder.build_extract_value(full, 1);
        let pf = false.compile(ctx); // XXX
        let zf = builder.build_cmp(res, 0u8.compile(ctx), Predicate::Equal);
        let sf = builder.build_cmp(res, 0u8.compile(ctx), Predicate::LessThan);
        let of = false.compile(ctx);

        // Retain the top 56 bits, set the bottom 8 bits.
        let full_res = builder.build_or(
            builder.build_and(&*func[0], (!0xffu64).compile(ctx)),
            builder.build_zext(res, Type::get::<u64>(ctx)));
        let mut ret = Value::new_undef(func.get_signature().get_return());
        ret = builder.build_insert_value(ret, full_res, 0);
        ret = builder.build_insert_value(ret, cf, 1);
        ret = builder.build_insert_value(ret, pf, 2);
        ret = builder.build_insert_value(ret, zf, 3);
        ret = builder.build_insert_value(ret, sf, 4);
        ret = builder.build_insert_value(ret, of, 5);
        builder.build_ret(ret);
        }
    }
}

type RegInfo<'a> = (Vec<&'a Register>, Vec<&'a Register>);
type RegState<'a> = (HashMap<&'a Register, &'a Value>,
                     HashMap<String, &'a Value>);

fn is_flags(reg: &Register) -> bool {
    return reg.name == "EFLAGS";
}

pub fn translate_instruction<'a>(inst: &InstructionInfo,
                             state: &State,
                             xlation: &TranslationState,
                             tt: &'a TargetTriple) {
    let ctxt = state.get_llvm_context();
    let builder = &xlation.builder;

    let mri = tt.register_info();
    println!("Translating {}", inst.opcode);
    let base = tt.parse_instructions("", "",
        inst.get_inst_file(state).to_str().unwrap());
    assert!(base.len() == 2, "We expect the instruction file to have little");
    assert!(base[base.len() - 1].opcode.is_return());

    let representative = &base[0];

    // Print out the header of the function for this instruction.
    let function = xlation.get_function(&base[0]);

    let insts = tt.parse_instructions("", "",
        inst.get_circuit_file(state).to_str().unwrap());

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
    assert!(input_registers.is_empty(), "Deal with you later");
    println!("{:?}", reg_state);

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
    state.add_base(state.state.get_target_triple().register_info());
    module.verify().unwrap();
    module.optimize(3, 3);

    unsafe {
        use std::ffi::CString;

        let filename = CString::new(file.to_str().unwrap()).unwrap();
        write_file(state.module.as_ptr(), filename.as_ptr());
    }
}
