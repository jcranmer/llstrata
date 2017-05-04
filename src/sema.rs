use llvm::*;
use llvmmc::TargetTriple;
use mcsema::TranslationState;
use state::InstructionInfo;
use std::collections::HashMap;

struct BaseInfo {
    inst_info: InstructionInfo,
    sample: &'static str,
    build_llvm: Box<Fn(&Function, &Builder)>
}

fn get_constant(value: &Value, c: u64) -> &Value {
    let ctx = value.get_context();
    let ty = value.get_type();
    assert!(ty.is_integer(), "You need the type to be an integer type");
    if ty == bool::get_type(ctx) {
        (c == 0).compile(ctx)
    } else if ty == u8::get_type(ctx) {
        (c as u8).compile(ctx)
    } else if ty == u16::get_type(ctx) {
        (c as u16).compile(ctx)
    } else if ty == u32::get_type(ctx) {
        (c as u32).compile(ctx)
    } else if ty == u64::get_type(ctx) {
        (c as u64).compile(ctx)
    } else {
        panic!("Unknown type to convert");
    }
}

static mut INTRINSIC_BASE : Option<&'static Module> = None;

fn get_intrinsic<T>(name: &str) ->
        &'static Function where T: Compile<'static> {
    unsafe {
        let module = INTRINSIC_BASE.as_ref()
            .expect("Install the module for getting intrinsics first");
        if module.get_function(name).is_none() {
            module.add_function(name, T::get_type(module.get_context()))
        } else {
            module.get_function(name).unwrap()
        }
    }
}

fn pf<'a>(builder: &'a Builder, value: &'a Value) -> &'a Value {
    let ctx = value.get_context();
    let lsb = builder.build_trunc(value, u8::get_type(ctx));
    let ctpop = get_intrinsic::<fn(u8) -> u8>("llvm.ctpop.i8");
    builder.build_xor(true.compile(ctx),
        builder.build_trunc(
            builder.build_call(ctpop, &[lsb]), bool::get_type(ctx)))
}

fn zf<'a>(builder: &'a Builder, value: &'a Value) -> &'a Value {
    builder.build_cmp(value, get_constant(value, 0), Predicate::Equal)
}

fn sf<'a>(builder: &'a Builder, value: &'a Value) -> &'a Value {
    builder.build_cmp(value, get_constant(value, 0), Predicate::LessThan)
}

macro_rules! set_flags {
    ($index:expr, $ret:ident, $builder:ident, $val:ident, $($flag:ident),*) => {{
        let mut index = $index - 1;
        let mut ret = $ret;
        let val = $val;
        let builder = $builder;
        $(
            index += 1;
            ret = builder.build_insert_value(ret, $flag(builder, val), index);
        )*
        ret
    }}
}

fn adcb_r8_r8(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let lhs = builder.build_trunc(&*func[0], Type::get::<u8>(ctx));
    let rhs = builder.build_trunc(&*func[1], Type::get::<u8>(ctx));
    let in_cf = builder.build_zext(&*func[2], Type::get::<u8>(ctx));
    let overflow = 
        get_intrinsic::<fn(u8, u8) -> (u8, bool)>("llvm.uadd.with.overflow.i8");
    let half = builder.build_call(overflow, &[lhs, rhs]);
    let full = builder.build_call(overflow,
        &[builder.build_extract_value(half, 0), in_cf]);
    let res = builder.build_extract_value(full, 0);
    let cf = builder.build_or(
        builder.build_extract_value(full, 1),
        builder.build_extract_value(half, 1));
    let soverflow = 
        get_intrinsic::<fn(u8, u8) -> (u8, bool)>("llvm.sadd.with.overflow.i8");
    let shalf = builder.build_call(soverflow, &[lhs, rhs]);
    let sfull = builder.build_call(soverflow,
        &[builder.build_extract_value(shalf, 0), in_cf]);
    let of = builder.build_or(
        builder.build_extract_value(sfull, 1),
        builder.build_extract_value(shalf, 1));

    // Retain the top 56 bits, set the bottom 8 bits.
    let full_res = builder.build_or(
        builder.build_and(&*func[0], (!0xffu64).compile(ctx)),
        builder.build_zext(res, Type::get::<u64>(ctx)));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, full_res, 0);
    ret = builder.build_insert_value(ret, cf, 1);
    ret = set_flags!(2, ret, builder, res, pf, zf, sf);
    ret = builder.build_insert_value(ret, of, 5);
    builder.build_ret(ret);
}

fn adcw_r16_r16(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let lhs = builder.build_trunc(&*func[0], Type::get::<u16>(ctx));
    let rhs = builder.build_trunc(&*func[1], Type::get::<u16>(ctx));
    let in_cf = builder.build_zext(&*func[2], Type::get::<u16>(ctx));
    let uoverflow = 
        get_intrinsic::<fn(u16, u16) -> (u16, bool)>("llvm.uadd.with.overflow.i16");
    let half = builder.build_call(uoverflow, &[lhs, rhs]);
    let full = builder.build_call(uoverflow,
        &[builder.build_extract_value(half, 0), in_cf]);
    let res = builder.build_extract_value(full, 0);
    let cf = builder.build_or(
        builder.build_extract_value(full, 1),
        builder.build_extract_value(half, 1));
    let soverflow = 
        get_intrinsic::<fn(u16, u16) -> (u16, bool)>("llvm.sadd.with.overflow.i16");
    let shalf = builder.build_call(soverflow, &[lhs, rhs]);
    let sfull = builder.build_call(soverflow,
        &[builder.build_extract_value(shalf, 0), in_cf]);
    let of = builder.build_or(
        builder.build_extract_value(sfull, 1),
        builder.build_extract_value(shalf, 1));

    // Retain the top 48 bits, set the bottom 16 bits.
    let full_res = builder.build_or(
        builder.build_and(&*func[0], (!0xffffu64).compile(ctx)),
        builder.build_zext(res, Type::get::<u64>(ctx)));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, full_res, 0);
    ret = builder.build_insert_value(ret, cf, 1);
    ret = set_flags!(2, ret, builder, res, pf, zf, sf);
    ret = builder.build_insert_value(ret, of, 5);
    builder.build_ret(ret);
}

fn adcl_r32_r32(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let lhs = builder.build_trunc(&*func[0], Type::get::<u32>(ctx));
    let rhs = builder.build_trunc(&*func[1], Type::get::<u32>(ctx));
    let in_cf = builder.build_zext(&*func[2], Type::get::<u32>(ctx));
    let overflow = 
        get_intrinsic::<fn(u32, u32) -> (u32, bool)>("llvm.uadd.with.overflow.i32");
    let half = builder.build_call(overflow, &[lhs, rhs]);
    let full = builder.build_call(overflow,
        &[builder.build_extract_value(half, 0), in_cf]);
    let res = builder.build_extract_value(full, 0);
    let cf = builder.build_or(
        builder.build_extract_value(full, 1),
        builder.build_extract_value(half, 1));
    let soverflow = 
        get_intrinsic::<fn(u32, u32) -> (u32, bool)>("llvm.sadd.with.overflow.i32");
    let shalf = builder.build_call(soverflow, &[lhs, rhs]);
    let sfull = builder.build_call(soverflow,
        &[builder.build_extract_value(shalf, 0), in_cf]);
    let of = builder.build_or(
        builder.build_extract_value(sfull, 1),
        builder.build_extract_value(shalf, 1));

    // For 32-bit mode, the top 32 bits are cleared.
    let full_res = builder.build_zext(res, Type::get::<u64>(ctx));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, full_res, 0);
    ret = builder.build_insert_value(ret, cf, 1);
    ret = set_flags!(2, ret, builder, res, pf, zf, sf);
    ret = builder.build_insert_value(ret, of, 5);
    builder.build_ret(ret);
}

fn adcq_r64_r64(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let lhs = builder.build_trunc(&*func[0], Type::get::<u64>(ctx));
    let rhs = builder.build_trunc(&*func[1], Type::get::<u64>(ctx));
    let in_cf = builder.build_zext(&*func[2], Type::get::<u64>(ctx));
    let overflow = 
        get_intrinsic::<fn(u64, u64) -> (u64, bool)>("llvm.uadd.with.overflow.i64");
    let half = builder.build_call(overflow, &[lhs, rhs]);
    let full = builder.build_call(overflow,
        &[builder.build_extract_value(half, 0), in_cf]);
    let res = builder.build_extract_value(full, 0);
    let cf = builder.build_or(
        builder.build_extract_value(full, 1),
        builder.build_extract_value(half, 1));
    let soverflow = 
        get_intrinsic::<fn(u64, u64) -> (u64, bool)>("llvm.sadd.with.overflow.i64");
    let shalf = builder.build_call(soverflow, &[lhs, rhs]);
    let sfull = builder.build_call(soverflow,
        &[builder.build_extract_value(shalf, 0), in_cf]);
    let of = builder.build_or(
        builder.build_extract_value(sfull, 1),
        builder.build_extract_value(shalf, 1));

    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, res, 0);
    ret = builder.build_insert_value(ret, cf, 1);
    ret = set_flags!(2, ret, builder, res, pf, zf, sf);
    ret = builder.build_insert_value(ret, of, 5);
    builder.build_ret(ret);
}

fn cmoveq_r64_r64(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let cond = builder.build_cmp(&*func[2], false.compile(ctx),
        Predicate::Equal);
    let val = builder.build_select(cond, &*func[1], &*func[0]);
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, val, 0);
    builder.build_ret(ret);
}

fn movswq_r64_r16(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let ecx = builder.build_trunc(&*func[0], Type::get::<u16>(ctx));
    let val = builder.build_sext(ecx, Type::get::<u64>(ctx));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, val, 0);
    builder.build_ret(ret);
}

fn movslq_r64_r32(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let ecx = builder.build_trunc(&*func[0], Type::get::<u32>(ctx));
    let val = builder.build_sext(ecx, Type::get::<u64>(ctx));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, val, 0);
    builder.build_ret(ret);
}

fn orq_r64_r64(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let lhs = &*func[0];
    let rhs = &*func[1];
    let res = builder.build_or(lhs, rhs);
    let cf = false.compile(ctx);
    let of = false.compile(ctx);
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, res, 0);
    ret = builder.build_insert_value(ret, cf, 1);
    ret = set_flags!(2, ret, builder, res, pf, zf, sf);
    ret = builder.build_insert_value(ret, of, 5);
    builder.build_ret(ret);
}

fn xorq_r64_r64(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let lhs = &*func[0];
    let rhs = &*func[1];
    let res = builder.build_xor(lhs, rhs);
    let cf = false.compile(ctx);
    let of = false.compile(ctx);
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, res, 0);
    ret = builder.build_insert_value(ret, cf, 1);
    ret = set_flags!(2, ret, builder, res, pf, zf, sf);
    ret = builder.build_insert_value(ret, of, 5);
    builder.build_ret(ret);
}

macro_rules! strata_str {
    ($($val:expr),*) => { concat!("{", $(" %", $val),*, " }") }
}

fn get_base_instructions() -> HashMap<&'static str, BaseInfo> {
    let mut map = HashMap::new();
    macro_rules! base_instruction {
        ($opcode:ident, $prog:expr,
         in ($( $in_reg:ident ),* ),
         out ($( $out_reg:ident ),* )) => {
            map.insert(stringify!($opcode), BaseInfo {
                inst_info: InstructionInfo {
                    opcode: String::from(stringify!($opcode)),
                    def_in: String::from(strata_str!($(stringify!($in_reg)),*)),
                    live_out: String::from(strata_str!($(stringify!($out_reg)),*))
                },
                sample: concat!($prog, "\n"),
                build_llvm: Box::new($opcode)
            });
        }
    }

    base_instruction!(adcb_r8_r8, "adcb %cl, %bl",
                      in(cl, bl, cf), out(bl, cf, pf, zf, sf, of));
    base_instruction!(adcl_r32_r32, "adcl %ecx, %ebx",
                      in(ecx, ebx, cf), out(rbx, cf, pf, zf, sf, of));
    base_instruction!(adcq_r64_r64, "adcq %rcx, %rbx",
                      in(rcx, rbx, cf), out(rbx, cf, pf, zf, sf, of));
    base_instruction!(adcw_r16_r16, "adcw %cx, %bx",
                      in(cx, bx, cf), out(bx, cf, pf, zf, sf, of));
    base_instruction!(cmoveq_r64_r64, "cmoveq %rcx, %rbx",
                      in(rcx, rbx, zf), out(rbx));
    base_instruction!(movslq_r64_r32, "movslq %ecx, %rbx",
                      in(ecx), out(rbx));
    base_instruction!(movswq_r64_r16, "movswq %cx, %rbx",
                      in(cx), out(rbx));
    base_instruction!(orq_r64_r64, "orq %rcx, %rbx",
                      in(rcx, rbx), out(rcx, cf, pf, zf, sf, of));
    base_instruction!(xorq_r64_r64, "xorq %rcx, %rbx",
                      in(rcx, rbx), out(rcx, cf, pf, zf, sf, of));
    return map;
}


pub fn add_base_programs(module: &Module, builder: &Builder,
                         state: &TranslationState, tt: &TargetTriple) {
    unsafe {
        let ptr_module : *const Module = module;
        INTRINSIC_BASE = ptr_module.as_ref();
    }
    let base_instrs = get_base_instructions();
    for (_, base_info) in base_instrs {
        let inst = tt.parse_instructions("", "", base_info.sample);
        let func = state.get_function(&inst[0]);
        (base_info.build_llvm)(func, builder);
    }
    unsafe {
        INTRINSIC_BASE = None;
    }
}
