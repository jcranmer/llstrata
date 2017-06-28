use std::fmt;
use std::io;
use std::mem;

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
#[derive(Default, Debug, Eq, PartialEq, Clone)]
pub struct RegState {
    pub gp_regs: [u64; 16],
    pub sse_regs: [[u8; 32]; 16],
    pub rflags: u64,
    pub trap: u64
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

fn print_bytes<T>(f: &mut fmt::Formatter, t: &T) -> fmt::Result {
    let size = mem::size_of::<T>() as isize;
    let bytes = t as *const T as *const u8;
    for i in (0..size).rev() {
        write!(f, " {:02x}", unsafe { *bytes.offset(i) })?;
    }
    return Ok(());
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
            writeln!(out, "vmovapd %{1}, {0}+asm_register_struct(%rip)",
                gp_size + 32 * $i, stringify!($name))?;
        };
    }
    macro_rules! load_struct {
        (gp $name:ident: $t:ty, $i:expr) => {
            writeln!(out, "xchg 0x{:x}+asm_register_struct(%rip), %{}",
                8 * $i, stringify!($name))?;
        };
        (sse $name:ident, $i:expr) => {
            writeln!(out, "vmovapd {0}+asm_register_struct(%rip), %{1}",
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