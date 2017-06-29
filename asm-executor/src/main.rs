#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate lazy_static;
extern crate libc;
extern crate regex;

mod arch;
mod state;

use std::mem;

extern "C" {
    fn call_asm_func(regs: *mut state::RegState, asm: usize);
}

extern "C" fn do_nothing(x: i32, y: i32) -> i32 {
    return x / y;
}

fn execute_assembly(state: &mut state::State, func: usize) {
    use state::Permissions::*;
    use libc::*;

    // Code for mapping banks
    let page_mask = unsafe { sysconf(_SC_PAGESIZE) } as usize - 1;
    let page_align = |base, len| {
        let page_offset = base & page_mask;
        return (base - page_offset, len + page_offset);
    };
    let map_bank = |bank: &state::Memory, extra_flags| {
        let (page_addr, mmap_len) = page_align(bank.base as usize,
                                               bank.contents.len());
        let address = unsafe {
            mmap(page_addr as *mut c_void, mmap_len,
                 match bank.flags {
                     Read => PROT_READ,
                     ReadWrite => PROT_READ | PROT_WRITE,
                     ReadExec => PROT_READ | PROT_EXEC,
                     ReadWriteExec => PROT_READ | PROT_WRITE | PROT_EXEC
                 },
                 MAP_PRIVATE | MAP_FIXED | MAP_ANONYMOUS | extra_flags,
                 -1, 0)
        } as usize;
        if address != page_addr {
            panic!("Couldn't allocate memory at page {:x}", page_addr);
        }
        return (address, mmap_len);
    };

    // Add MAP_GROWSDOWN to the stack pointer. This means that the kernel will
    // add extra pages as necessary to run any extra stack code.
    let stack_map = map_bank(&state.stack, MAP_GROWSDOWN);
    let mut other_maps : Vec<_> = state.other_banks.iter()
        .map(|bank| map_bank(bank, 0))
        .collect();
    other_maps.push(stack_map);

    // Set up the code to catch architecture traps such as seg faults. Do this
    // last so we don't stop over our own code.
    arch::host::init_exceptions();
    unsafe {
        call_asm_func(&mut state.registers, mem::transmute(func));
    }

    // Now that we're done, unmap the banks we had to map.
    for (addr, len) in other_maps {
        unsafe { munmap(addr as *mut c_void, len); }
    }
}

fn main() {
    let mut bytes = Vec::new();
    bytes.resize(32, 0);
    let mem = state::Memory {
        base: (0x7 << 32),
        contents: bytes,
        flags: state::Permissions::ReadWrite
    };
    let mut state = state::State {
        registers: state::RegState::new(mem.base),
        stack: mem,
        other_banks: Vec::new()
    };
    state.registers.set_register("rsi", 100u64).unwrap();
    state.registers.set_register("rdi", 10u64).unwrap();
    execute_assembly(&mut state, 0);
    println!("State: {}", state);

    let text = "SIGNAL 0 [normal exit]

%rax     33 c4 11 e1 59 c9 70 46
%rcx     60 16 47 18 ee 07 8f 05
%rdx     dc e2 5d 02 63 93 3b c6
%rbx     bf 4a c1 50 8d 69 a9 9d
%rsp     00 00 00 07 00 00 00 00
%rbp     63 2d 56 59 b6 93 9a 9e
%rsi     1d 8d f9 cc f8 b2 a3 a0
%rdi     a3 f0 2a a9 88 4c 99 bf
%r8      43 7d 9c a4 06 20 9e 95
%r9      1b 00 f5 8a 9c 96 9b 40
%r10     a6 d6 5e ab 40 3a 85 72
%r11     88 79 58 c9 be 85 91 e4
%r12     4f f1 c3 5b 61 df 84 db
%r13     9b 47 9b 63 d9 94 84 11
%r14     33 95 77 7e b8 4a 43 3b
%r15     ad 2f 5a 9e db fb 8e 26

%ymm0    cb 6a eb b2 06 d8 76 82 71 2e ca da 06 7b 0f 9a 96 1f c6 d6 c9 20 83 3f 8f a2 b4 69 19 f4 68 b8
%ymm1    ad 25 d5 3f 4e 6f bb 4e 89 e1 45 ab 57 d6 2a ed a4 7b 5f b3 d9 45 2b b3 e8 cd e4 21 db 54 de cc
%ymm2    c5 ce 06 97 d2 b7 0b ae 90 f7 e3 52 dc e0 42 35 f8 94 43 4e f6 68 49 00 83 7c 48 cf 65 98 1f 58
%ymm3    f4 8e a1 e6 49 b4 af ab ad da d7 b4 18 c5 84 8d 8e 28 eb 1c 19 74 ec 03 18 bb 53 46 17 72 46 2b
%ymm4    d4 7a 0b 84 3d e6 54 fa 7b ce 93 43 09 3a 9f 17 f9 ec 40 6f 4a fd da f8 09 87 d5 9a 9c 47 86 49
%ymm5    7d aa 47 4a 12 ec a2 9a 64 12 03 fd df 0b 74 21 b7 69 ad ad ee 0d 52 d9 47 e9 7d 71 38 fb ae f5
%ymm6    b4 b3 b3 62 01 5d 70 aa 80 fe 20 a6 91 32 b0 a8 cb e6 dc f4 30 ac 97 a9 87 5b 63 ae c0 6b ca a2
%ymm7    50 8a 28 c5 33 e1 fc 7b 61 24 32 9e 0b e3 f9 b9 09 f2 e7 1e 54 0f c5 9c 43 53 1d 85 cc ff de 69
%ymm8    5b 2e 7e 00 a1 4d d7 6f 53 66 03 7c c4 9c 0a b2 4b 7c b0 18 77 90 df 30 6d 3d 12 aa 78 34 4e 24
%ymm9    22 8e 5d ed de dd a1 27 22 94 5d cf 8b e8 86 9a bd 37 30 03 11 c5 92 99 ea e6 cb 83 ef 0f e0 6e
%ymm10   88 fa bf 3b 57 25 a2 0e 53 b4 d3 40 df 25 bf 4c 45 22 f5 78 92 f8 dd 50 ee db 0b 99 81 a1 c8 0e
%ymm11   1a 4d 39 c9 7d 41 fc 46 eb d2 c8 95 49 e6 e0 b6 4f 88 8b 8b 74 42 a9 c2 f3 cd 8c cb a4 9f 82 fa
%ymm12   ba 46 87 14 27 bd c1 21 b2 9a 99 81 3e 5c 65 d9 8d 97 8f d0 d3 3d cd 61 02 e7 ca bd a6 51 55 60
%ymm13   4b bc bb 8c 2d 47 f5 d2 94 c3 28 bb 78 ca fd 91 af 31 9e 5d 63 ca 2b b8 27 02 00 21 34 d7 07 c6
%ymm14   f2 53 30 e2 e7 e8 71 42 ed 67 1b e9 a3 52 b2 fa 6d 55 d2 80 23 48 a1 c0 ab a1 66 82 21 42 38 7e
%ymm15   91 18 97 74 26 45 f6 40 c2 c2 65 84 94 e0 f6 64 69 f9 2e 20 37 fd 17 1b b8 17 fd c1 39 27 31 59

%cf      0
%1       1
%pf      1
%0       0
%af      0
%0       0
%zf      1
%sf      1
%tf      0
%if      1
%df      0
%of      1
%iopl[0] 0
%iopl[1] 0
%nt      0
%0       0
%rf      0
%vm      0
%ac      0
%vif     0
%vip     0
%id      0

[ 00000007 00000000 - 00000006 ffffffe0 ]
[ 4 valid rows shown ]

00000006 fffffff8   v v v v v v v v   91 6d 5c 61 51 15 1e 66
00000006 fffffff0   v v v v v v v v   b1 51 c0 c5 e4 4e 91 f4
00000006 ffffffe8   v v v v v v v v   76 c3 6f a5 4f 08 0e d0
00000006 ffffffe0   v v v v v v v v   85 db 1e 3f 3a 31 d7 f6

[ 00000001 00000000 - 00000001 00000000 ]
[ 0 valid rows shown ]

[ 00000000 00000000 - 00000000 00000000 ]
[ 0 valid rows shown ]

0 more segment(s)";
    let mut result = state::State::parse_text(&mut text.as_bytes());
    if let Err(ref e) = result {
        println!("error: {}", e);
        for e in e.iter().skip(1) {
            println!("caused by: {}", e);
        }

        if let Some(backtrace) = e.backtrace() {
            println!("backtrace: {:?}", backtrace);
        }
    } else if let Ok(ref mut s) = result {
        println!("{}", s);
    }

}
