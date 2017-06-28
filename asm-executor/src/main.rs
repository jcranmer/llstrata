extern crate libc;

mod arch;

use arch::host::RegState;
use std::mem;
use std::ptr;

extern "C" {
    fn call_asm_func(regs: *mut RegState, asm: extern "C" fn (i32, i32) -> i32);
    fn asm_signal_return();
    static mut asm_register_struct : RegState;
}

fn signal_handler(_: libc::c_int, _: *const libc::siginfo_t,
                  context: *mut libc::ucontext_t) {
    unsafe {
        let mut regs = &mut context.as_mut().unwrap().uc_mcontext.gregs;
        // Store the trap value in the register struct. (This is REG_TRAPNO in
        // the thread context--it's so nice for Linux to actually pass this
        // along for us).
        asm_register_struct.trap = regs[20] as u64 | 0x100;

        // Set RIP to the function just after the call. We might want to do a
        // call to unwind to properly do this, but there's no guarantee that our
        // functions have unwind tables set up properly in the first place.
        regs[16] = asm_signal_return as usize as i64;
        libc::setcontext(context);
    }
}

fn empty_sigset() -> libc::sigset_t {
    let mut sigset: libc::sigset_t = unsafe { mem::uninitialized() };
    unsafe { libc::sigemptyset(&mut sigset); }

    return sigset;
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
    unsafe {
        let mut handler : libc::sigaction = mem::zeroed();
        handler.sa_sigaction = signal_handler as usize;
        handler.sa_mask = empty_sigset();
        handler.sa_flags = libc::SA_SIGINFO;
        libc::sigaction(libc::SIGSEGV, &handler, ptr::null_mut());
        libc::sigaction(libc::SIGFPE, &handler, ptr::null_mut());
        libc::sigaction(libc::SIGILL, &handler, ptr::null_mut());
        libc::sigaction(libc::SIGBUS, &handler, ptr::null_mut());
        libc::sigaction(libc::SIGTRAP, &handler, ptr::null_mut());
        call_asm_func(&mut regs, do_nothing);
    }
    println!("Registers: {}", regs);
}
