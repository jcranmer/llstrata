use std::fmt;
#[cfg(not(feature="not-build"))]
use std::io;
use std::mem;
#[cfg(feature="not-build")]
use std::ptr;

#[cfg(feature="not-build")]
use libc;

use arch::{as_mut_bytes, print_bytes};

macro_rules! register_list {
    ($reg:ident gp) => {
        $reg!(gp rax: u64, 0);
        $reg!(gp rbx: u64, 1);
        $reg!(gp rcx: u64, 2);
        $reg!(gp rdx: u64, 3);
        $reg!(gp rsp: u64, 4);
        $reg!(gp rbp: u64, 5);
        $reg!(gp rsi: u64, 6);
        $reg!(gp rdi: u64, 7);
        $reg!(gp r8: u64, 8);
        $reg!(gp r9: u64, 9);
        $reg!(gp r10: u64, 10);
        $reg!(gp r11: u64, 11);
        $reg!(gp r12: u64, 12);
        $reg!(gp r13: u64, 13);
        $reg!(gp r14: u64, 14);
        $reg!(gp r15: u64, 15);
    };

    ($reg:ident sse) => {
        $reg!(sse ymm0, 0);
        $reg!(sse ymm1, 1);
        $reg!(sse ymm2, 2);
        $reg!(sse ymm3, 3);
        $reg!(sse ymm4, 4);
        $reg!(sse ymm5, 5);
        $reg!(sse ymm6, 6);
        $reg!(sse ymm7, 7);
        $reg!(sse ymm8, 8);
        $reg!(sse ymm9, 9);
        $reg!(sse ymm10, 10);
        $reg!(sse ymm11, 11);
        $reg!(sse ymm12, 12);
        $reg!(sse ymm13, 13);
        $reg!(sse ymm14, 14);
        $reg!(sse ymm15, 15);
    };

    ($reg:ident flags) => {
        $reg!(flag "cf"      ,  0);
        $reg!(flag 1         ,  1);
        $reg!(flag "pf"      ,  2);
        $reg!(flag 0         ,  3);
        $reg!(flag 0         ,  4);
        $reg!(flag "af"      ,  5);
        $reg!(flag "zf"      ,  6);
        $reg!(flag "sf"      ,  7);
        $reg!(flag "tf"      ,  8);
        $reg!(flag "if"      ,  9);
        $reg!(flag "df"      , 10);
        $reg!(flag "of"      , 11);
        $reg!(flag "iopl[0]" , 12);
        $reg!(flag "iopl[1]" , 13);
        $reg!(flag "nt"      , 14);
        $reg!(flag  0        , 15);
        $reg!(flag "rf"      , 16);
        $reg!(flag "vm"      , 17);
        $reg!(flag "ac"      , 18);
        $reg!(flag "vif"     , 19);
        $reg!(flag "vip"     , 20);
        $reg!(flag "id"      , 21);
    };
}

#[repr(C)]
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RegState {
    pub gp_regs: [u64; 16],
    pub sse_regs: [[u8; 32]; 16],
    pub rflags: u64,
    pub trap: u64
}

pub const REGISTER_BANKS : u32 = 2;

impl RegState {
    pub fn get_stack_register(&mut self) -> &mut u64 {
        macro_rules! get_stack {
            (gp rsp: $t:ty, $i:expr) => { return &mut self.gp_regs[$i]; };
            (gp $name:ident: $t:ty, $i: expr) => { }
        };
        register_list!(get_stack gp);
    }

    pub fn get_bank_size(bank_num: u32) -> usize {
        match bank_num {
            0 =>  8, // GP registers
            1 => 32, // SSE registers
            _ => panic!("Invalid bank count")
        }
    }

    pub fn get_bank_registers(bank_num: u32) -> &'static [&'static str] {
        static mut GP_REGS : [&'static str; 16] = [""; 16];
        static mut SSE_REGS : [&'static str; 16] = [""; 16];
        macro_rules! reg_name {
            (gp $name:ident: $t:ty, $i: expr) => {
                GP_REGS[$i] = stringify!($name);
            };
            (sse $name:ident, $i: expr) => {
                SSE_REGS[$i] = stringify!($name);
            };
        }
        unsafe {
            if GP_REGS[0].is_empty() {
                register_list!(reg_name gp);
                register_list!(reg_name sse);
            }
            match bank_num {
                0 => &GP_REGS,
                1 => &SSE_REGS,
                _ => panic!("Invalid bank count")
            }
        }
    }

    pub fn get_register(&mut self, name: &str) -> Option<&mut [u8]> {
        fn map<T>(val: &mut T) -> Option<&mut [u8]> {
            return Some(as_mut_bytes(val));
        }
        macro_rules! get_reg {
            (gp $name:ident: $t:ty, $i: expr) => {
                if name == stringify!($name) {
                    return map(&mut self.gp_regs[$i]);
                }
            };
            (sse $name:ident, $i: expr) => {
                if name == stringify!($name) {
                    return map(&mut self.sse_regs[$i]);
                }
            }
        }
        register_list!(get_reg gp);
        register_list!(get_reg sse);
        return None;
    }

    pub fn get_flag(&mut self, name: &str) -> Option<(&mut u64, u8)> {
        macro_rules! get_flag {
            (flag 0, $shift:expr) => { };
            (flag 1, $shift:expr) => { };
            (flag $n:expr, $shift:expr) => {
                if name == $n {
                    return Some((&mut self.rflags, $shift));
                }
            };
        }
        register_list!(get_flag flags);
        return None;
    }
}

impl Default for RegState {
    fn default() -> Self {
        return Self {
            gp_regs: Default::default(),
            sse_regs: Default::default(),
            trap: Default::default(),
            rflags: 0x00000002,
        }
    }
}

fn map_trap(code: u64) -> &'static str {
    match code {
        0 => "normal exit",
        0x100 => "#DE Divide Error Exception",
        0x101 => "#DB Debug Exception",
        0x102 => "NMI Interrupt",
        0x103 => "#BP Breakpoint Exception",
        0x104 => "#OF Overflow Exception",
        0x105 => "#BR BOUND Range Exceeded Exception",
        0x106 => "#UD Invalid opcode Exception",
        0x107 => "#NM Device Not Available Exception",
        0x108 => "#DF Double Fault Exception",
        0x109 => "Coprocessor Segment Overrun",
        0x10a => "#TS Invalid TSS Exception",
        0x10b => "#NP Segment Not Present",
        0x10c => "#SS Stack Fault Exception",
        0x10d => "#GP General Protection Exception",
        0x10e => "#PF Page Fault Exception",
        0x110 => "#MF x87 FPU Floating-Point Error",
        0x111 => "#AC Alignment Check Exception",
        0x112 => "#MC Machine-Check Exception",
        0x113 => "#XM SIMD Floating-Point Exception",
        0x114 => "#VE Virtualization Exception",
        0x120...0x1ff => "user defined interrupt",
        0x100...0x11f => "unknown x86 exception",
        _ => "unknown code"
    }
}

impl fmt::Display for RegState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Trap code comes first
        macro_rules! print {
            (gp $name:ident: $t:ty, $i:expr) => {
                write!(f, "%{:<10}", stringify!($name))?;
                print_bytes(f, &self.gp_regs[$i])?;
                writeln!(f, "")?;
            };
            (sse $name:ident, $i:expr) => {
                write!(f, "%{:<10}", stringify!($name))?;
                print_bytes(f, &self.sse_regs[$i])?;
                writeln!(f, "")?;
            };
            (flag $name:expr, $i:expr) => {
                writeln!(f, "%{:<10} {}", $name,
                         if self.rflags & (1 << $i) != 0 { 1 } else { 0 })?;
            };
        }
        writeln!(f, "SIGNAL {:x} [{}]\n", self.trap, map_trap(self.trap))?;
        register_list!(print gp);
        writeln!(f, "")?;
        register_list!(print sse);
        writeln!(f, "")?;
        register_list!(print flags);
        return Ok(());
    }
}

#[cfg(not(feature="not-build"))]
pub fn write_asm(out: &mut io::Write) -> io::Result<()> {
    let gp_size = 8 * 16;
    let sse_size = 16 * 32;
    let rflags_offset = gp_size + sse_size;
    let size = gp_size + sse_size + 8 + 8;
    assert!(size == mem::size_of::<RegState>(),
        "We don't have the right size of the register state struct");
    writeln!(out, ".file \"register-state.asm\"")?;
    writeln!(out, ".local __xsave_blob")?;
    writeln!(out, ".comm __xsave_blob, 4096, 256")?;
    writeln!(out, ".globl asm_register_struct")?;
    writeln!(out, ".comm asm_register_struct, {0}, 256",
             mem::size_of::<RegState>())?;
    writeln!(out, ".local __arg1")?;
    writeln!(out, ".comm __arg1, 64, 64")?;
    writeln!(out, ".local __arg2")?;
    writeln!(out, ".comm __arg2, 64, 64")?;
    writeln!(out, ".text")?;
    writeln!(out, ".globl call_asm_func")?;
    writeln!(out, ".type call_asm_func, @function")?;
    writeln!(out, "call_asm_func:")?;
    writeln!(out, ".cfi_startproc")?;

    // Save state we can't xchg
    writeln!(out, "pushq %rax")?;
    writeln!(out, "pushq %rdx")?;
    writeln!(out, "mov $0xffffffff, %eax")?;
    writeln!(out, "mov $0xffffffff, %edx")?;
    writeln!(out, "xsave __xsave_blob(%rip)")?;
    writeln!(out, "popq %rdx")?;
    writeln!(out, "popq %rax")?;

    // Save the input parameters.
    writeln!(out, "movq %rdi, __arg1(%rip)")?;
    writeln!(out, "movq %rsi, __arg2(%rip)")?;

    // Copy the input register state to the blob.
    writeln!(out, "lea asm_register_struct(%rip), %rdi")?; // Destination
    writeln!(out, "mov __arg1(%rip), %rsi")?; // Source
    writeln!(out, "movq ${}, %rdx", size)?;
    writeln!(out, "call memcpy@PLT")?;

    macro_rules! load_cpu {
        (gp $name:ident: $t:ty, $i:expr) => {
            writeln!(out, "xchg 0x{:x}+asm_register_struct(%rip), %{}",
                8 * $i, stringify!($name))?;
        };
        (sse $name:ident, $i:expr) => {
            writeln!(out, "vmovapd {0}+asm_register_struct(%rip), %{1}",
                gp_size + 32 * $i, stringify!($name))?;
        };
    }
    macro_rules! load_struct {
        (gp $name:ident: $t:ty, $i:expr) => {
            writeln!(out, "xchg 0x{:x}+asm_register_struct(%rip), %{}",
                8 * $i, stringify!($name))?;
        };
        (sse $name:ident, $i:expr) => {
            writeln!(out, "vmovapd %{1}, {0}+asm_register_struct(%rip)",
                gp_size + 32 * $i, stringify!($name))?;
        };
    }

    // Move register state before calling the function.
    register_list!(load_cpu gp);
    register_list!(load_cpu sse);
    writeln!(out, "pushq %rax")?;
    writeln!(out, "mov {}+asm_register_struct(%rip), %rax", rflags_offset)?;
    writeln!(out, "pushq %rax")?;
    writeln!(out, "popfq")?;
    writeln!(out, "popq %rax")?;

    // Call the function!
    writeln!(out, "call *__arg2(%rip)")?;
    writeln!(out, ".globl asm_signal_return")?;
    writeln!(out, "asm_signal_return:")?;

    // Move register state after calling the function.
    register_list!(load_struct gp);
    register_list!(load_struct sse);
    writeln!(out, "pushq %rax")?;
    writeln!(out, "pushfq")?;
    writeln!(out, "popq %rax")?;
    writeln!(out, "mov %rax, {}+asm_register_struct(%rip)", rflags_offset)?;
    writeln!(out, "popq %rax")?;

    // Restore state we can't xchg
    writeln!(out, "pushq %rax")?;
    writeln!(out, "pushq %rdx")?;
    writeln!(out, "mov $0xffffffff, %eax")?;
    writeln!(out, "mov $0xffffffff, %edx")?;
    writeln!(out, "xrstor __xsave_blob(%rip)")?;
    writeln!(out, "popq %rdx")?;
    writeln!(out, "popq %rax")?;

    // Copy register blob into the input parameter.
    writeln!(out, "mov __arg1(%rip), %rdi")?; // Destination
    writeln!(out, "lea asm_register_struct(%rip), %rsi")?; // Source
    writeln!(out, "movq ${}, %rdx", size)?;
    writeln!(out, "call memcpy@PLT")?;

    // Return
    writeln!(out, "ret")?;
    writeln!(out, ".cfi_endproc")?;
    writeln!(out, ".size call_asm_func, .-call_asm_func")?;
    return Ok(())
}

#[cfg(all(feature="not-build",target_arch="x86_64"))]
pub fn init_exceptions() {
    extern "C" {
        fn asm_signal_return();
        static mut asm_register_struct : RegState;
    }

    fn signal_handler(_: libc::c_int, _: *const libc::siginfo_t,
                      context: *mut libc::ucontext_t) {
        unsafe {
            let mut regs = &mut context.as_mut().unwrap().uc_mcontext.gregs;
            // Store the trap value in the register struct. (This is REG_TRAPNO
            // in the thread context--it's so nice for Linux to actually pass
            // this along for us).
            asm_register_struct.trap = regs[20] as u64 | 0x100;

            // Set RIP to the function just after the call. We might want to do
            // a call to unwind to properly do this, but there's no guarantee
            // that our functions have unwind tables set up properly in the
            // first place.
            regs[16] = asm_signal_return as usize as i64;
            libc::setcontext(context);
        }
    }

    fn empty_sigset() -> libc::sigset_t {
        let mut sigset: libc::sigset_t = unsafe { mem::uninitialized() };
        unsafe { libc::sigemptyset(&mut sigset); }
        return sigset;
    }
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
    }
}
