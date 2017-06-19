use llvm::*;
use llvmmc::TargetTriple;
use mcsema::TranslationState;
use state::InstructionInfo;
use std::collections::HashMap;
use std::mem;

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

fn movq_r64_r64(func: &Function, builder: &Builder) {
    builder.position_at_end(func.append("entry"));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, &*func[0], 0);
    builder.build_ret(ret);
}

fn movsbq_r64_r8(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let ecx = builder.build_trunc(&*func[0], Type::get::<u8>(ctx));
    let val = builder.build_sext(ecx, Type::get::<u64>(ctx));
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

fn popcntq_r64_r64(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let ctpop = get_intrinsic::<fn(u64) -> u64>("llvm.ctpop.i64");
    let res = builder.build_call(ctpop, &[&*func[0]]);
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, res, 0);
    ret = builder.build_insert_value(ret, false.compile(ctx), 1);
    ret = builder.build_insert_value(ret, false.compile(ctx), 2);
    ret = set_flags!(3, ret, builder, res, zf);
    ret = builder.build_insert_value(ret, false.compile(ctx), 4);
    ret = builder.build_insert_value(ret, false.compile(ctx), 5);
    builder.build_ret(ret);
}

fn salq_r64_cl(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let cl = builder.build_trunc(&*func[0], Type::get::<u8>(ctx));
    let rbx = &*func[1];
    //let in_cf = &*func[2];
    let in_pf = &*func[3];
    let in_zf = &*func[4];
    let in_sf = &*func[5];
    //let in_of = &*func[6];

    // Shift instructions are very complicated.
    // Step 1: Shift mask needs to be masked with 0x3f.
    let shift_width = builder.build_and(
        builder.build_zext(cl, Type::get::<u64>(ctx)),
        get_constant(rbx, 0x3fu64));
    let res = builder.build_shl(rbx, shift_width);

    // We only set flags if the shift width is not 0.
    let set_flags = builder.build_cmp(shift_width, get_constant(shift_width, 0),
        Predicate::Equal);
    // Carry flag is the last bit shifted out.
    // val = bin(abcd)
    // val << 0 = abcd, cf = <unchanged>
    // val << 1 = bcd0, cf = a = abcd >> 3
    // val << 2 = cd00, cf = b = abcd >> 2
    // val << 3 = d000, cf = c = abcd >> 1
    //let cf_if_not0 = builder.build_trunc(builder.build_lshr(
    //    rbx, builder.build_sub(get_constant(rbx, 64), shift_width)),
    //    Type::get::<bool>(ctx));
    //let cf = builder.build_select(set_flags, cf_if_not0, in_cf);
    let pf = builder.build_select(set_flags, pf(builder, res), in_pf);
    let zf = builder.build_select(set_flags, zf(builder, res), in_zf);
    let sf = builder.build_select(set_flags, sf(builder, res), in_sf);

    // Overflow is only set if OF == 1. Actually, it's undefined if
    // OF is not 0 or 1. Its value is whether or not the carry flag is the
    // same as the MSB of the result.
    //let set_of = builder.build_cmp(shift_width, get_constant(shift_width, 1),
    //    Predicate::Equal);
    //let of_val = builder.build_xor(cf, builder.build_trunc(
    //    builder.build_lshr(res, get_constant(shift_width, 63)),
    //    Type::get::<bool>(ctx)));
    //let of = builder.build_select(set_of, of_val, in_of);
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, res, 0);
    ret = builder.build_insert_value(ret, pf, 1);
    ret = builder.build_insert_value(ret, zf, 2);
    ret = builder.build_insert_value(ret, sf, 3);
    // XXX: Strata isn't generating CF or OF flags (because they may be
    // undefined, AIUI).
    builder.build_ret(ret);
}

fn sarq_r64_cl(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let cl = builder.build_trunc(&*func[0], Type::get::<u8>(ctx));
    let rbx = &*func[1];
    let in_cf = &*func[2];
    let in_pf = &*func[3];
    let in_zf = &*func[4];
    let in_sf = &*func[5];
    //let in_of = &*func[6];

    // Shift instructions are very complicated.
    // Step 1: Shift mask needs to be masked with 0x3f.
    let shift_width = builder.build_and(
        builder.build_zext(cl, Type::get::<u64>(ctx)),
        get_constant(rbx, 0x3fu64));
    let res = builder.build_ashr(rbx, shift_width);

    // We only set flags if the shift width is not 0.
    let set_flags = builder.build_cmp(shift_width, get_constant(shift_width, 0),
        Predicate::Equal);
    // Carry flag is the last bit shifted out.
    // val = bin(abcd)
    // val >> 0 = abcd, cf = <unchanged>
    // val >> 1 = 0abc, cf = d = abcd >> 0
    // val >> 2 = 00ab, cf = c = abcd >> 1
    // val >> 3 = 000a, cf = b = abcd >> 2
    let cf_if_not0 = builder.build_trunc(builder.build_lshr(
        rbx, builder.build_sub(shift_width, get_constant(rbx, 1))),
        Type::get::<bool>(ctx));
    let cf = builder.build_select(set_flags, cf_if_not0, in_cf);
    let pf = builder.build_select(set_flags, pf(builder, res), in_pf);
    let zf = builder.build_select(set_flags, zf(builder, res), in_zf);
    let sf = builder.build_select(set_flags, sf(builder, res), in_sf);

    // Since Strata doesn't support OF for SAR, don't think about it for now.
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, res, 0);
    ret = builder.build_insert_value(ret, cf, 1);
    ret = builder.build_insert_value(ret, pf, 2);
    ret = builder.build_insert_value(ret, zf, 3);
    ret = builder.build_insert_value(ret, sf, 4);
    builder.build_ret(ret);
}

fn shrq_r64_cl(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let cl = builder.build_trunc(&*func[0], Type::get::<u8>(ctx));
    let rbx = &*func[1];
    //let in_cf = &*func[2];
    let in_pf = &*func[3];
    let in_zf = &*func[4];
    let in_sf = &*func[5];
    //let in_of = &*func[6];

    // Shift instructions are very complicated.
    // Step 1: Shift mask needs to be masked with 0x3f.
    let shift_width = builder.build_and(
        builder.build_zext(cl, Type::get::<u64>(ctx)),
        get_constant(rbx, 0x3fu64));
    let res = builder.build_lshr(rbx, shift_width);

    // We only set flags if the shift width is not 0.
    let set_flags = builder.build_cmp(shift_width, get_constant(shift_width, 0),
        Predicate::Equal);
    let pf = builder.build_select(set_flags, pf(builder, res), in_pf);
    let zf = builder.build_select(set_flags, zf(builder, res), in_zf);
    let sf = builder.build_select(set_flags, sf(builder, res), in_sf);

    // Since Strata doesn't support CF or OF for SHR, don't think about it yet.
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, res, 0);
    ret = builder.build_insert_value(ret, pf, 1);
    ret = builder.build_insert_value(ret, zf, 2);
    ret = builder.build_insert_value(ret, sf, 3);
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
    ($($val:expr),*) => { concat!("{", $(" %", $val,)* " }") }
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
    // XXX: movq_r64_imm64
    base_instruction!(movq_r64_r64, "movq %rcx, %rbx",
                      in(rcx), out(rbx));
    // XXX: movb_r8_rh
    // XXX: movb_rh_r8
    base_instruction!(movsbq_r64_r8, "movsbq %cl, %rbx",
                      in(cl), out(rbx));
    base_instruction!(movslq_r64_r32, "movslq %ecx, %rbx",
                      in(ecx), out(rbx));
    base_instruction!(movswq_r64_r16, "movswq %cx, %rbx",
                      in(cx), out(rbx));
    base_instruction!(orq_r64_r64, "orq %rcx, %rbx",
                      in(rcx, rbx), out(rcx, cf, pf, zf, sf, of));
    base_instruction!(popcntq_r64_r64, "popcntq %rcx, %rbx",
                      in(rcx), out(rcx, cf, pf, zf, sf, of));
    base_instruction!(salq_r64_cl, "salq %cl, %rbx",
                      in(cl, rbx, cf, pf, zf, sf, of),
                      out(rcx, pf, zf, sf));
    base_instruction!(sarq_r64_cl, "sarq %cl, %rbx",
                      in(cl, rbx, cf, pf, zf, sf, of),
                      out(rcx, cf, pf, zf, sf));
    base_instruction!(shrq_r64_cl, "shrq %cl, %rbx",
                      in(cl, rbx, cf, pf, zf, sf, of),
                      out(rcx, cf, pf, zf, sf));
    base_instruction!(xorq_r64_r64, "xorq %rcx, %rbx",
                      in(rcx, rbx), out(rcx, cf, pf, zf, sf, of));
    return map;
}

pub struct FunctionInfo {
    pub name: &'static str,
    pub assembly: &'static str,
    pub def_in: Vec<&'static str>,
    pub live_out: Vec<&'static str>,
    llvm_func: Box<Fn(&Function, &Builder)>
}

fn read_eflag_to_register<'a>(func: &'a Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let flag = &*func[0];
    let res = builder.build_zext(flag, Type::get::<u64>(ctx));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, res, 0);
    builder.build_ret(ret);
}

fn write_register_to_eflag<'a>(func: &'a Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let flag = &*func[0];
    let res = builder.build_trunc(flag, Type::get::<bool>(ctx));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, res, 0);
    builder.build_ret(ret);
}
fn set_eflag<'a>(func: &'a Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, true.compile(ctx), 0);
    builder.build_ret(ret);
}

fn clear_eflag<'a>(func: &'a Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, false.compile(ctx), 0);
    builder.build_ret(ret);
}

fn set_szp<T>(func: &Function, builder: &Builder) where T: for <'a> Compile<'a> {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let val = builder.build_trunc(&*func[0], Type::get::<T>(ctx));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, zf(builder, val), 0);
    ret = builder.build_insert_value(ret, pf(builder, val), 1);
    ret = builder.build_insert_value(ret, sf(builder, val), 2);
    builder.build_ret(ret);
}

fn move_small_to_large<S>(func: &Function, builder: &Builder)
where S: for <'a> Compile<'a> {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let small_lo = &*func[0];
    let small_hi = &*func[1];
    let shifted = builder.build_shl(
        small_hi, (mem::size_of::<S>() as u64).compile(ctx));
    let val = builder.build_or(shifted, small_lo);
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, val, 0);
    builder.build_ret(ret);
}

fn move_large_to_small<S>(func: &Function, builder: &Builder)
where S: for <'a> Compile<'a> {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let val = &*func[0];
    let small_lo = builder.build_zext(
        builder.build_trunc(val, Type::get::<S>(ctx)),
        Type::get::<u64>(ctx));
    let small_hi = builder.build_zext(
        builder.build_trunc(
            builder.build_lshr(
                val, (mem::size_of::<S>() as u64).compile(ctx)),
            Type::get::<S>(ctx)),
        Type::get::<u64>(ctx));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, small_lo, 0);
    ret = builder.build_insert_value(ret, small_hi, 1);
    builder.build_ret(ret);
}

fn move_byte_small_large(func: &Function, builder: &Builder, byte: u64) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let val = &*func[0];
    let single = builder.build_and(
        builder.build_ashr(val, (byte * 8).compile(ctx)),
        0xffu64.compile(ctx));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, single, 0);
    builder.build_ret(ret);
}

impl FunctionInfo {
    pub fn get_functions() -> HashMap<&'static str, FunctionInfo> {
        let mut functions = HashMap::new();
macro_rules! suffix {
    (u8) => { "b" };
    (u16) => { "w" };
    (u32) => { "l" };
    (u64) => { "q" };
}
macro_rules! reg_size {
    (rax, u8) => { "%al" };
    (rbx, u8) => { "%bl" };
    (rcx, u8) => { "%cl" };
    (rdx, u8) => { "%dl" };
    ($reg:ident, u8) => { concat!("%", stringify!($reg), "b") };
    ($reg:ident, u16) => { concat!("%", stringify!($reg), "w") };
    ($reg:ident, u32) => { concat!("%", stringify!($reg), "d") };
    ($reg:ident, u64) => { concat!("%", stringify!($reg), "") };
}
macro_rules! make_string {
    (opt $prefix: expr, vals()) => { "" };
    (opt $prefix: expr, vals($($val:ident),*)) => {
        make_string!($prefix, vals($($val),*))
    };
    ($prefix: expr, vals($($val:ident),*)) => {
        concat!("#! ", $prefix, " ",
                strata_str!($(stringify!($val)),*),
                "\n");
    }
}
macro_rules! function {
    (fn $strname:expr, $name:path
     [$($in_reg:ident),*] -> ($($out_reg:ident),*)
     : ($($undef_reg:ident),*) {
     doc($doc:expr),
     $code:expr}) => {
        functions.insert($strname, FunctionInfo {
            name: $strname,
            def_in: vec![$(stringify!($in_reg)),*],
            live_out: vec![$(stringify!($out_reg)),*],
            llvm_func: Box::new($name),
            assembly: concat!("  .text
  .globl ", $strname, "
  .type ", $strname, ", @function\n",
  make_string!("maybe-read", vals($($in_reg),*)),
  make_string!("maybe-write", vals($($out_reg),*)),
  make_string!(opt "must-undef", vals($($undef_reg),*)),
".", $strname, ":
  # ----------------------------------------------------------------------------
  # ", $doc, "
  # ----------------------------------------------------------------------------
  #", $code, "
  retq

.size ", $strname, ", .-", $strname, "
  retq\n")
        });
    }
}
macro_rules! read_eflag {
    ($flag:ident, $reg:ident, $setcc:ident) => {
        function!(fn concat!("read_", stringify!($flag), "_into_",
                    stringify!($reg)),
                    read_eflag_to_register[$flag] -> ($reg) : () {
            doc(concat!("read the ", stringify!($flag), " flag into ",
                stringify!($reg))), concat!("
  movq $0x0, %", stringify!($reg), "
  ", stringify!($setcc), " ", reg_size!($reg, u8))
        });
    }
}
macro_rules! read_eflags {
    ($flag:ident, [$($reg:ident),*], $setcc:ident) => {
        $(read_eflag!($flag, $reg, $setcc);)*
    }
}
macro_rules! set_eflag {
    ($flag:ident, $val:expr) => {
        function!(fn concat!("set_", stringify!($flag)),
                    set_eflag[] -> ($flag) : (r14, r15) {
            doc(concat!("set the ", stringify!($flag), " flag")), concat!("
  pushfq
  popq %r15
  movq ", $val, ", %r14 # avoid sign extend when doing the or
  orq %r14, %r15
  pushq %r15
  popfq")
        });
    }
}
macro_rules! clear_eflag {
    ($flag:ident, $val:expr) => {
        function!(fn concat!("clear_", stringify!($flag)),
                    clear_eflag[] -> ($flag) : (r15) {
            doc(concat!("clear the ", stringify!($flag), " flag")), concat!("
  pushfq
  popq %r15
  andq ", $val, ", %r15
  pushq %r15
  popfq")
        });
    }
}
macro_rules! write_eflag {
    ($flag:ident, $reg:ident, $set:expr, $clear:expr) => {
        function!(fn concat!("write_", stringify!($reg), "_to_",
                    stringify!($flag)),
                    write_register_to_eflag[$reg] -> ($flag) : (r14, r15) {
            doc(concat!("set the ", stringify!($flag), " flag")), concat!("
  # read flags
  pushfq
  popq %r15

  # zero out ", stringify!($flag), "
  movq ", $clear, ", %r14 # avoid sign extend when doing the or
  andq %r14, %r15

  # replicate the last bit in ", stringify!($reg), " to all bits
  movb %", stringify!($reg), ", %r14b
  shlq $0x3f, %r14
  sarq $0x3f, %r14

  # test if we need to set the flag
  testq %r14, %r14
  je .lbl0
  movq ", $set, ", %r14 # avoid sign extend when doing the or
  orq %r14, %r15

.lbl0:

  # write new flags
  pushq %r15
  popfq")
        });
    }
}
macro_rules! impl_eflag {
    ($flag:ident, $set:expr, $clear:expr) => {
        set_eflag!($flag, $set);
        clear_eflag!($flag, $clear);
        write_eflag!($flag, cl, $set, $clear);
        write_eflag!($flag, dl, $set, $clear);
    }
}
macro_rules! set_szp {
    ($reg:ident, $postfix:expr, $size:ty) => {
        function!(fn concat!("set_szp_for_", stringify!($reg)),
                    set_szp<$size>[$reg] -> (zf, pf, sf) : (r14, r15) {
            doc("set the zf, sf, pf according to the result in %r8"),
            concat!("
  pushfq
  popq %r15
  # clear zf, sf and pf
  andq $0xffffff3b, %r15
  # set zf if necessary
  test", $postfix, " %", stringify!($reg), ", %", stringify!($reg), "
  jne .lbl0
  orq $0x40, %r15
.lbl0:
  # set sf if necessary
  test", $postfix, " %", stringify!($reg), ", %", stringify!($reg), "
  jns .lbl1
  movq $0x80, %r14 # avoid sign extend when doing the or
  orq %r14, %r15
.lbl1:
  # set zf if necessary
  test", $postfix, " %", stringify!($reg), ", %", stringify!($reg), "
  jnp .lbl2
  orq $0x4, %r15
.lbl2:
  pushq %r15
  popfq")
        });
    }
}
macro_rules! move_small_large {
    ($rs1:ident, $rs2:ident, $rl:ident, $size:ident,
     $lsize:ident, $ss:expr, $ls:expr) => {
        function!(fn concat!("move_0", $ss, "_0", $ls, "_", stringify!($rs1),
                             "_", stringify!($rs2), "_", stringify!($rl)),
                    move_small_to_large<$size>[$rs1, $rs2] -> ($rl) : (r14, r15) {
            doc(concat!("moves ", stringify!($rs1), " and ", stringify!($rs2),
                " to ", stringify!($rl), ".")), concat!("
  pushfq
  mov", suffix!($size), " %", stringify!($rs2), ", ", reg_size!(r15, $size), "
  shlq $", $ss, ", %r15
  mov", suffix!($size), " %", stringify!($rs1), ", ", reg_size!(r14, $size), "
  orq %r14, %r15
  mov", suffix!($lsize), " ", reg_size!(r15, $lsize), ", %", stringify!($rl), "
  popfq")
        });
        function!(fn concat!("move_0", $ls, "_0", $ss, "_", stringify!($rl),
                             "_", stringify!($rs1), "_", stringify!($rs2)),
                    move_large_to_small<$size>[$rl] -> ($rs1, $rs2) : (r15) {
            doc(concat!("moves ", stringify!($rl), " to ", stringify!($rs1),
                " to ", stringify!($rs2), ".")), concat!("
  pushfq
  mov", suffix!($lsize), " %", stringify!($rl), ", %r15
  mov", suffix!($size), " %r15d, %", stringify!($rs1), "
  shrq $", $ss, ", %r15
  mov", suffix!($size), " %r15d, %", stringify!($rs2), "
  popfq")
        });
    }
}
        read_eflags!(cf, [rbx, rcx], setnae);
        read_eflags!(of, [rbx, rcx], seto);
        read_eflags!(pf, [rbx, rcx], setp);
        read_eflags!(sf, [rbx, rcx], sets);
        read_eflags!(zf, [rbx, rcx], setz);
        impl_eflag!(af, "$0x10", "$0xffffffef");
        impl_eflag!(cf, "$0x1", "$0xfffffffe");
        impl_eflag!(of, "$0x800", "$0xfffff7ff");
        impl_eflag!(pf, "$0x4", "$0xfffffffb");
        impl_eflag!(sf, "$0x80", "$0xffffff7f");
        impl_eflag!(zf, "$0x40", "$0xffffffbf");
        set_szp!(bl, "b", u8);
        set_szp!(bx, "w", u16);
        set_szp!(ebx, "l", u32);
        set_szp!(rbx, "q", u64);
        move_small_large!(r10d, r11d, rbx, u32, u64, "32", "64");
        move_small_large!(r8w, r9w, ebx, u16, u32, "16", "32");
        move_small_large!(r8w, r9w, ecx, u16, u32, "16", "32");
        move_small_large!(r8b, r9b, bx, u8, u16, "08", "16");
        move_small_large!(r10b, r11b, bx, u8, u16, "08", "16");
        move_small_large!(r8b, r9b, cx,  u8, u16, "08", "16");
        move_small_large!(r10b, r11b, cx, u8, u16, "08", "16");
        return functions;
    }
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
    for pseudo_inst in state.state.get_pseudo_instructions() {
        // XXX: check assembly correctness
        let func = state.get_function_for_pseudo(pseudo_inst);
        (pseudo_inst.llvm_func)(func, builder);
    }
    module.verify().unwrap();
    unsafe {
        INTRINSIC_BASE = None;
    }
}
