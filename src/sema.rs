use llvm::*;
use llvm_sys::core::*;
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

fn cast_to_xmm<'a>(ctx: &'a Context, builder: &'a Builder,
                   value: &'a Value) -> &'a Value {
    let ymm_ty = Type::get::<[u64; 4]>(ctx);
    return builder.build_shuffle_vector(
        builder.build_bit_cast(value, ymm_ty),
        Value::new_undef(ymm_ty),
        &[0, 1]);
}

fn cast_to_ymm<'a>(ctx: &'a Context, builder: &'a Builder,
                   value: &'a Value) -> &'a Value {
    let xmm_ty = Type::get::<[u64; 2]>(ctx);
    return builder.build_shuffle_vector(
        builder.build_bit_cast(value, xmm_ty),
        Value::new_vector(&[0u64.compile(ctx); 2]),
        &[0, 1, 2, 3]);
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

fn movq_r64_imm64(func: &Function, builder: &Builder) {
    movq_r64_r64(func, builder)
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

fn vzeroall(func: &Function, builder: &Builder) {
    builder.position_at_end(func.append("entry"));
    let ret = unsafe {
        LLVMConstNull(func.get_signature().get_return().into()).into()
    };
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
    // Well, this is the base instruction that Strata uses to refer to MOV64ri.
    // LLVM (and GCC) interpret it as MOV64ri32 instead.
    base_instruction!(movq_r64_imm64, "movq $0x0, %rbx",
                      in(), out(rbx));
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
    base_instruction!(vzeroall, "vzeroall",
                      in(),
                      out(ymm0, ymm1, ymm2, ymm3, ymm4, ymm5, ymm6, ymm7,
                          ymm8, ymm9, ymm10, ymm11, ymm12, ymm13, ymm14, ymm15));
    base_instruction!(xorq_r64_r64, "xorq %rcx, %rbx",
                      in(rcx, rbx), out(rcx, cf, pf, zf, sf, of));
    return map;
}

pub struct FunctionInfo {
    pub name: &'static str,
    pub assembly: &'static str,
    pub def_in: Vec<&'static str>,
    pub live_out: Vec<&'static str>,
    pub llvm_func: Box<Fn(&Function, &Builder)>
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
        small_hi, ((mem::size_of::<S>() * 8) as u64).compile(ctx));
    let small_lo = builder.build_zext(
        builder.build_trunc(small_lo, Type::get::<S>(ctx)),
        Type::get::<u64>(ctx));
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
                val, ((mem::size_of::<S>() * 8) as u64).compile(ctx)),
            Type::get::<S>(ctx)),
        Type::get::<u64>(ctx));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, small_lo, 0);
    ret = builder.build_insert_value(ret, small_hi, 1);
    builder.build_ret(ret);
}

fn write_reg_byte(func: &Function, builder: &Builder, byte: u64) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let val = builder.build_zext(
        builder.build_trunc(
            &*func[0], Type::get::<u8>(ctx)),
            Type::get::<u64>(ctx));
    let cleared_val = builder.build_and(&*func[1],
        builder.build_not(
            builder.build_shl(0xffu64.compile(ctx), (byte * 8).compile(ctx))));
    let result = builder.build_or(cleared_val,
        builder.build_shl(val, (byte * 8).compile(ctx)));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret, result, 0);
    builder.build_ret(ret);
}

fn read_reg_byte(func: &Function, builder: &Builder, byte: u64) {
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

fn move_into_xmm(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let lo = &*func[0];
    let hi = &*func[1];
    let vec_ty = Type::get::<[u64; 2]>(ctx);
    let mut vec = Value::new_undef(vec_ty);
    vec = builder.build_insert_element(vec, lo, 0u32.compile(ctx));
    vec = builder.build_insert_element(vec, hi, 1u32.compile(ctx));
    let mut ret = Value::new_undef(func.get_signature().get_return());
    ret = builder.build_insert_value(ret,
        cast_to_ymm(ctx, builder, vec), 0);
    builder.build_ret(ret);
}

fn move_out_of_xmm(func: &Function, builder: &Builder) {
    let ctx = func.get_context();
    builder.position_at_end(func.append("entry"));
    let xmm = cast_to_xmm(ctx, builder, &*func[0]);
    let mut ret = Value::new_undef(func.get_signature().get_return());
    for i in 0..2 {
        ret = builder.build_insert_value(ret,
            builder.build_extract_element(xmm, i.compile(ctx)), i);
    }
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
    (nil) => { "" };
}
macro_rules! shift_width {
    (u8) => { "$0x8" };
    (u16) => { "$0x10" };
    (u32) => { "$0x20" };
}
macro_rules! reg_size {
    (r8d, $ty:ident) => { reg_size!(r8, $ty); };
    (r9d, $ty:ident) => { reg_size!(r9, $ty); };
    (r12d, $ty:ident) => { reg_size!(r12, $ty); };
    (r13d, $ty:ident) => { reg_size!(r13, $ty); };
    ($reg:ident, $ty:ident) => { concat!("%", sub_reg!($reg, $ty)) };
}
macro_rules! sub_reg {
    (rax, u8) => { "al" };
    (rbx, u8) => { "bl" };
    (rcx, u8) => { "cl" };
    (rdx, u8) => { "dl" };
    (rax, u16) => { "ax" };
    (rbx, u16) => { "bx" };
    (rcx, u16) => { "cx" };
    (rdx, u16) => { "dx" };
    (rax, u32) => { "eax" };
    (rbx, u32) => { "ebx" };
    (rcx, u32) => { "ecx" };
    (rdx, u32) => { "edx" };
    ($reg:ident, u8) => { concat!(stringify!($reg), "b") };
    ($reg:ident, u16) => { concat!(stringify!($reg), "w") };
    ($reg:ident, u32) => { concat!(stringify!($reg), "d") };
    ($reg:ident, u64) => { concat!(stringify!($reg), "") };
    ($reg:ident, nil) => { stringify!($reg) };
}
macro_rules! instr {
    ($opcode:ident, $size:ident, $arg1:ident, $arg2:ident) => {
        concat!("\n  ", stringify!($opcode), suffix!($size), " ",
            reg_size!($arg1, $size), ", ", reg_size!($arg2, $size));
    };
    (e $opcode:ident, $size:ident, $arg1:expr, $arg2:ident) => {
        concat!("\n  ", stringify!($opcode), suffix!($size), " ",
            $arg1, ", ", reg_size!($arg2, $size));
    };
    (e $opcode:ident, $size:ident, $arg1:expr, $arg2:ident # avsext) => {
        concat!("\n  ", stringify!($opcode), suffix!($size), " ",
            $arg1, ", ", reg_size!($arg2, $size), " # avoid sign extend when doing the or");
    };
    ($opcode:ident) => {
        concat!("\n  ", stringify!($opcode));
    };
    ($opcode:ident, $size:ident, $arg:ident) => {
        concat!("\n  ", stringify!($opcode), suffix!($size), " ",
            reg_size!($arg, $size));
    };
    (nosuffix $opcode:ident, $size:ident, $arg:ident) => {
        concat!("\n  ", stringify!($opcode), " ", reg_size!($arg, $size));
    };
    (comment null) => { "\n  #" };
    (comment $comment:expr) => {
        concat!("\n  # ", $comment);
    };
    (jump $opcode:ident $label:ident) => {
        concat!("\n  ", stringify!($opcode), " .", stringify!($label));
    };
    (label $label:ident) => {
        concat!("\n.", stringify!($label), ":");
    };
}
macro_rules! make_string {
    (opt $prefix: expr, vals()) => { "" };
    (opt $prefix: expr, vals($($val:expr),*)) => {
        make_string!($prefix, vals($($val),*))
    };
    ($prefix: expr, vals($($val:expr),*)) => {
        concat!("#! ", $prefix, " ",
                strata_str!($($val),*),
                "\n");
    }
}
macro_rules! function {
    (fn $strname:expr, $name:path
     [$($in_reg:expr),*] -> ($($out_reg:expr),*)
     : ($($undef_reg:expr),*) {
     doc($doc:expr),
     $code:expr}) => {
        function!(fn $strname; ($name)
                  [$($in_reg),*] -> ($($out_reg),*) : ($($undef_reg),*)
                  { doc($doc), $code });
    };
    (fn $strname:expr; ($name:expr)
     [$($in_reg:expr),*] -> ($($out_reg:expr),*)
     : ($($undef_reg:expr),*) {
     doc($doc:expr),
     $code:expr}) => {
        functions.insert($strname, FunctionInfo {
            name: $strname,
            def_in: vec![$($in_reg),*],
            live_out: vec![$($out_reg),*],
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
    ($flag:expr, $reg:ident, $setcc:ident) => {
        function!(fn concat!("read_", $flag, "_into_", stringify!($reg)),
                    read_eflag_to_register[$flag] -> (stringify!($reg)) : () {
            doc(concat!("read the ", $flag, " flag into ",
                stringify!($reg))), concat!(
                    instr!(e mov, u64, "$0x0", $reg),
                    instr!(nosuffix $setcc, u8, $reg))
        });
    }
}
macro_rules! read_eflags {
    ($flag:expr, [$($reg:ident),*], $setcc:ident) => {
        $(read_eflag!($flag, $reg, $setcc);)*
    }
}
macro_rules! set_eflag {
    ($flag:expr, $val:expr) => {
        function!(fn concat!("set_", $flag),
                    set_eflag[] -> ($flag) : ("r14", "r15") {
            doc(concat!("set the ", $flag, " flag")), concat!(
                instr!(pushfq),
                instr!(pop, u64, r15),
                instr!(e mov, u64, $val, r14 # avsext),
                instr!(or, u64, r14, r15),
                instr!(push, u64, r15),
                instr!(popfq))
        });
    }
}
macro_rules! clear_eflag {
    ($flag:expr, $val:expr) => {
        function!(fn concat!("clear_", $flag),
                    clear_eflag[] -> ($flag) : ("r15") {
            doc(concat!("clear the ", $flag, " flag")), concat!(
                instr!(pushfq),
                instr!(pop, u64, r15),
                instr!(e and, u64, $val, r15),
                instr!(push, u64, r15),
                instr!(popfq))
        });
    }
}
macro_rules! write_eflag {
    ($flag:expr, $reg:ident, $set:expr, $clear:expr) => {
        function!(fn concat!("write_", sub_reg!($reg, u8), "_to_", $flag),
                    write_register_to_eflag
                    [sub_reg!($reg, u8)] -> ($flag) : ("r14", "r15") {
            doc(concat!("set the ", $flag, " flag")), concat!(
                instr!(comment "read flags"),
                instr!(pushfq),
                instr!(pop, u64, r15), "\n",
                instr!(comment concat!("zero out ", $flag)),
                instr!(e mov, u64, $clear, r14 # avsext),
                instr!(and, u64, r14, r15), "\n",
                instr!(comment concat!("replicate the last bit in ",
                                       sub_reg!($reg, u8), " to all bits")),
                instr!(mov, u8, $reg, r14),
                instr!(e shl, u64, "$0x3f", r14),
                instr!(e sar, u64, "$0x3f", r14), "\n",
                instr!(comment "test if we need to set the flag"),
                instr!(test, u64, r14, r14),
                instr!(jump je lbl0),
                instr!(e mov, u64, $set, r14 # avsext),
                instr!(or, u64, r14, r15), "\n",
                instr!(label lbl0), "\n",
                instr!(comment "write new flags"),
                instr!(push, u64, r15),
                instr!(popfq))
        });
    }
}
macro_rules! impl_eflag {
    ($flag:expr, $set:expr, $clear:expr) => {
        set_eflag!($flag, $set);
        clear_eflag!($flag, $clear);
        write_eflag!($flag, rcx, $set, $clear);
        write_eflag!($flag, rdx, $set, $clear);
    }
}
macro_rules! set_szp {
    ($reg:ident, $size:ident) => {
        function!(fn concat!("set_szp_for_", sub_reg!($reg, $size)),
                    set_szp<$size>
                    [sub_reg!($reg, $size)] ->
                    ("zf", "pf", "sf") : ("r14", "r15") {
            doc("set the zf, sf, pf according to the result in %r8"),
            concat!(
                instr!(pushfq),
                instr!(pop, u64, r15),
                instr!(comment "clear zf, sf and pf"),
                instr!(e and, u64, "$0xffffff3b", r15),
                instr!(comment "set zf if necessary"),
                instr!(test, $size, $reg, $reg),
                instr!(jump jne lbl0),
                instr!(e or, u64, "$0x40", r15),
                instr!(label lbl0),
                instr!(comment "set sf if necessary"),
                instr!(test, $size, $reg, $reg),
                instr!(jump jns lbl1),
                instr!(e mov, u64, "$0x80", r14 # avsext),
                instr!(or, u64, r14, r15),
                instr!(label lbl1),
                instr!(comment "set zf if necessary"),
                instr!(test, $size, $reg, $reg),
                instr!(jump jnp lbl2),
                instr!(e or, u64, "$0x4", r15),
                instr!(label lbl2),
                instr!(push, u64, r15),
                instr!(popfq))
        });
    }
}
macro_rules! move_small_large {
    ($rs1:ident, $rs2:ident, $rl:ident, $size:ident,
     $lsize:ident, $ss:expr, $ls:expr) => {
        function!(fn concat!("move_0", $ss, "_0", $ls, "_",
                             sub_reg!($rs1, $size), "_",
                             sub_reg!($rs2, $size), "_",
                             sub_reg!($rl, $lsize)),
                    move_small_to_large<$size>
                    [sub_reg!($rs1, $size), sub_reg!($rs2, $size)] ->
                    (sub_reg!($rl, $lsize)) : ("r15") {
            doc(concat!("moves ", sub_reg!($rs1, $size), " and ",
                sub_reg!($rs2, $size), " to ", sub_reg!($rl, $lsize), ".")),
                concat!(
                    instr!(pushfq),
                    instr!(mov, $size, $rs2, r15),
                    instr!(e shl, u64, shift_width!($size), r15),
                    instr!(mov, $size, $rs1, r15),
                    instr!(mov, $lsize, r15, $rl),
                    instr!(popfq))
        });
        function!(fn concat!("move_0", $ls, "_0", $ss, "_",
                             sub_reg!($rl, $lsize), "_",
                             sub_reg!($rs1, $size), "_",
                             sub_reg!($rs2, $size)),
                    move_large_to_small<$size>
                    [sub_reg!($rl, $lsize)] ->
                    (sub_reg!($rs1, $size), sub_reg!($rs2, $size)) : ("r15") {
            doc(concat!("moves ", sub_reg!($rl, $lsize), " to ",
                sub_reg!($rs1, $size), " and ", sub_reg!($rs2, $size), ".")),
                concat!(
                    instr!(pushfq),
                    instr!(mov, $lsize, $rl, r15),
                    instr!(mov, $size, r15, $rs1),
                    instr!(e shr, u64, shift_width!($size), r15),
                    instr!(mov, $size, r15, $rs2),
                    instr!(popfq))
        });
    }
}
macro_rules! move_pair {
    ([$($rs1:ident, $rs2:ident),*], $rl:tt, $size:ident, $lsize:ident,
      $ss:expr, $ls:expr) => {
        $(move_pair!($rs1, $rs2, $rl, $size, $lsize, $ss, $ls);)*
    };
    ($rs1:ident, $rs2:ident, [$($rl:ident),*], $size:ident, $lsize:ident,
      $ss:expr, $ls:expr) => {
        $(move_small_large!($rs1, $rs2, $rl, $size, $lsize, $ss, $ls);)*
    }
}
macro_rules! move_bytes {
    ($reg:ident <- $byte_reg:ident, $index:expr, $index8:expr) => {{
        fn close_function<'a>(func: &'a Function, builder: &Builder) {
            write_reg_byte(func, builder, $index);
        }
        function!(fn concat!("move_", sub_reg!($byte_reg, u8), "_to_byte_",
                             stringify!($index), "_of_", stringify!($reg)),
                    close_function
                    [sub_reg!($byte_reg, u8), stringify!($reg)] ->
                    (stringify!($reg)) : ("r14", "r15") {
            doc(concat!("move ", sub_reg!($byte_reg, u8), " to the byte ",
                stringify!($index), " of ", stringify!($reg))), concat!(
                    instr!(pushfq),
                    instr!(xor, u64, r15, r15),
                    instr!(mov, u8, $byte_reg, r15),
                    instr!(e shl, u64, concat!("$", $index8), r15),
                    instr!(e mov, u64, "$0xff", r14),
                    instr!(e shl, u64, concat!("$", $index8), r14),
                    instr!(not, u64, r14),
                    instr!(and, u64, r14, $reg),
                    instr!(or, u64, r15, $reg),
                    instr!(popfq))
        });
    }};
    ($reg:ident -> $byte_reg:ident, $index:expr, $index8:expr) => {{
        fn close_function<'a>(func: &'a Function, builder: &Builder) {
            read_reg_byte(func, builder, $index);
        }
        function!(fn concat!("move_byte_", stringify!($index), "_of_",
                             stringify!($reg), "_to_", sub_reg!($byte_reg, u8)),
                    close_function
                    [stringify!($reg)] -> (sub_reg!($byte_reg, u8)) : ("r15") {
            doc(concat!("move the byte ", stringify!($index), " of ",
                stringify!($reg), " to ", sub_reg!($byte_reg, u8))), concat!(
                    instr!(pushfq),
                    instr!(mov, u64, $reg, r15),
                    instr!(e shr, u64, concat!("$", $index8), r15),
                    instr!(mov, u8, r15, $byte_reg),
                    instr!(popfq))
        });
    }};
    (gp $reg:ident, [$($index:tt=$x8:expr),*]) => {
        $(move_bytes!(rbx <- $reg, $index, $x8);)*
        $(move_bytes!(rbx -> $reg, $index, $x8);)*
    }
}
macro_rules! move_xmm {
    ($gp0:ident $gp1:ident -> $xmm:ident) => {
        function!(fn concat!("move_064_128_", stringify!($gp0), "_",
                             stringify!($gp1), "_", stringify!($xmm)),
                    move_into_xmm
                    [stringify!($gp0), stringify!($gp1)] ->
                    (stringify!($xmm)) : ("ymm15") {
            doc(concat!("moves ", stringify!($gp0), " to the lower 64 bits of ",
                stringify!($xmm), ", and ", stringify!($gp1), " to the higher\n",
                "  # 64 bits.")), concat!(
                    instr!(comment "move lower bits"),
                    instr!(mov, u64, $gp0, $xmm),
                    instr!(comment null),
                    instr!(comment "move the higher bits"),
                    instr!(mov, u64, $gp1, xmm15),
                    instr!(punpcklqd, u64, xmm15, $xmm)
                )
        });
    };
    ($gp0:ident $gp1:ident <- $xmm:ident) => {
        function!(fn concat!("move_128_064_", stringify!($xmm), "_",
                             stringify!($gp0), "_", stringify!($gp1)),
                    move_out_of_xmm
                    [stringify!($xmm)] -> (stringify!($gp0), stringify!($gp1))
                    : ("r15", "ymm14", "ymm15") {
            doc(concat!("moves the lower 64 bits of ", stringify!($xmm),
                " to ", stringify!($gp0), ", and the higher 64 bits\n  #",
                " to ", stringify!($gp1), ".")), concat!(
                    instr!(comment "move lower bits"),
                    instr!(mov, u64, $xmm, $gp0),
                    instr!(comment null),
                    instr!(comment "move the shuffling constant to xmm15"),
                    instr!(e mov, u64, "$0x0706050403020100", r15),
                    instr!(mov, u64, r15, xmm14),
                    instr!(e mov, u64, "$0x0f0e0d0c0b0a0908", r15),
                    instr!(mov, u64, r15, xmm15),
                    instr!(punpcklqd, u64, xmm14, xmm15),
                    instr!(comment null),
                    instr!(comment concat!("swap low and high 64 bytes of ", stringify!($xmm))),
                    instr!(pshufb, nil, xmm15, $xmm),
                    instr!(comment null),
                    instr!(comment "move higher bits"),
                    instr!(mov, u64, $xmm, $gp1),
                    instr!(comment null),
                    instr!(comment concat!("swap low and high 64 bytes of ", stringify!($xmm))),
                    instr!(pshufb, nil, xmm15, $xmm)
                )
        });
    }
}
        read_eflags!("cf", [rbx, rcx], setnae);
        read_eflags!("of", [rbx, rcx], seto);
        read_eflags!("pf", [rbx, rcx], setp);
        read_eflags!("sf", [rbx, rcx], sets);
        read_eflags!("zf", [rbx, rcx], setz);
        impl_eflag!("af", "$0x10", "$0xffffffef");
        impl_eflag!("cf", "$0x1", "$0xfffffffe");
        impl_eflag!("of", "$0x800", "$0xfffff7ff");
        impl_eflag!("pf", "$0x4", "$0xfffffffb");
        impl_eflag!("sf", "$0x80", "$0xffffff7f");
        impl_eflag!("zf", "$0x40", "$0xffffffbf");
        set_szp!(rbx, u8);
        set_szp!(rbx, u16);
        set_szp!(rbx, u32);
        set_szp!(rbx, u64);
        move_pair!([r8, r9, r10, r11, r12, r13], [rbx, rcx, rdx],
                   u8, u16, "08", "16");
        move_pair!([r8, r9, r10, r11, r12, r13], [rbx, rcx, rdx],
                   u16, u32, "16", "32");
        move_pair!([r8, r9, r10, r11, r12, r13], [rbx, rcx, rdx],
                   u32, u64, "32", "64");
        move_bytes!(gp r8, [2="0x10",3="0x18",4="0x20",5="0x28",6="0x30",7="0x38"]);
        move_bytes!(gp r9, [2="0x10",3="0x18",4="0x20",5="0x28",6="0x30",7="0x38"]);
        move_xmm!(r8 r9 -> xmm1);
        move_xmm!(r8 r9 <- xmm2);
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
