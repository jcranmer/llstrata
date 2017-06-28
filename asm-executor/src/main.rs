extern crate libc;

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
    println!("Registers: {}", state.registers);
}
