extern crate libc;

mod arch;

use arch::host::RegState;

extern "C" {
    fn call_asm_func(regs: *mut RegState, asm: extern "C" fn (i32, i32) -> i32);
}

extern "C" fn do_nothing(x: i32, y: i32) -> i32 {
    return x / y;
}

fn main() {
    let mut regs : RegState = Default::default();
    let reg_state = [0u8; 1048576];
    regs.gp_regs[7] = 100;
    regs.gp_regs[6] = 1;
    regs.gp_regs[4] = (&reg_state as *const u8 as usize as u64) + 0x10000;
    arch::host::init_exceptions();
    unsafe {
        call_asm_func(&mut regs, do_nothing);
    }
    println!("Registers: {}", regs);
}
