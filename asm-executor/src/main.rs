#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate lazy_static;
extern crate libc;
extern crate llvm_sys;
extern crate regex;

mod arch;
mod state;

use std::env;
use std::fs::File;
use std::io;
use std::mem;
use state::{Result,ResultExt,State};

extern "C" {
    fn call_asm_func(regs: *mut state::RegState, asm: usize);
}

extern "C" fn do_nothing(x: i32, y: i32) -> i32 {
    return x / y;
}

fn page_align(base: usize, len: usize) -> (usize, usize) {
    lazy_static! {
        static ref PAGE_MASK: usize =
            unsafe { libc::sysconf(libc::_SC_PAGE_SIZE) } as usize - 1;
    };
    let page_offset = base & *PAGE_MASK;
    return (base - page_offset, len + page_offset);
}

fn execute_assembly(state: &mut State, func: usize) {
    use state::Permissions::*;
    use libc::*;

    // Code for mapping banks
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
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    if args.len() < 3 {
        println!("Usage: {} <testcases> <program a> <program b>", program);
        return;
    }

    if let Err(ref e) = run_main(&args[1], &args[2], None) {
        println!("error: {}", e);
        for e in e.iter().skip(1) {
            println!("caused by: {}", e);
        }

        if let Some(backtrace) = e.backtrace() {
            println!("backtrace: {:?}", backtrace);
        }
    }
}

fn link_file(path: &str) -> state::Result<usize> {
    use std::ffi::{CString, CStr};
    use llvm_sys::core::*;
    use llvm_sys::object::*;

    // Load the file.
    let c_name = CString::new(path)?;
    let buffer = unsafe {
        let mut buffer = mem::uninitialized();
        let mut error = mem::uninitialized();
        let did_fail = LLVMCreateMemoryBufferWithContentsOfFile(
            c_name.as_ptr(), &mut buffer, &mut error);
        if did_fail == 0 {
            buffer
        } else {
            bail!(String::from(CStr::from_ptr(error).to_string_lossy()));
        }
    };
    let object_file = unsafe { LLVMCreateObjectFile(buffer) };

    // Find the first symbol with an actual name.
    let symbol_ref = unsafe { LLVMGetSymbols(object_file) };
    while unsafe { LLVMIsSymbolIteratorAtEnd(object_file, symbol_ref) } == 0 {
        let name = unsafe { CStr::from_ptr(LLVMGetSymbolName(symbol_ref)) };
        if !name.to_bytes().is_empty() {
            break;
        }
        unsafe { LLVMMoveToNextSymbol(symbol_ref) };
    }
    let addr = unsafe { LLVMGetSymbolAddress(symbol_ref) } as usize;

    // Get the contents of the section.
    let section_ref = unsafe { LLVMGetSections(object_file) };
    unsafe { LLVMMoveToContainingSection(section_ref, symbol_ref) };
    let section = unsafe { LLVMGetSectionContents(section_ref) } as usize;
    let section_size = unsafe { LLVMGetSectionSize(section_ref) };

    // Set the section contents to be executable. This may also make other
    // random heap memory also be executable, but considering we're already
    // running random x86 code, that's probably the least of our worries.
    unsafe {
        let (base, pages) = page_align(section, section_size as usize);
        libc::mprotect(base as *mut libc::c_void, pages as usize,
                       libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC);
    }

    unsafe { LLVMDisposeSectionIterator(section_ref) };
    unsafe { LLVMDisposeSymbolIterator(symbol_ref) };

    // XXX: leaks symbol section.
    return Ok(addr + section);
}

fn run_main(testcase: &str, test: &str,
            gold: Option<&str>) -> state::Result<()> {
    let tc_file = File::open(testcase)
        .chain_err(|| format!("Could not open file {}", testcase))?;
    let mut tests = State::parse_testcases(&mut io::BufReader::new(tc_file))
        .chain_err(|| "Could not parse testcases")?;

    let prog_addr = link_file(test)
        .chain_err(|| "Error loading test program")?;

    let gold_addr = gold.map_or(Ok(None), |g| link_file(g).map(|v| Some(v)))
        .chain_err(|| "Error loading gold program")?;

    for (i, test) in tests.iter_mut().enumerate() {
        let mut state = test.clone();
        execute_assembly(&mut state, prog_addr);
        if let Some(addr) = gold_addr {
            let mut gold = test;
            execute_assembly(&mut gold, addr);
            if state != *gold {
                println!("Testcase {} differs", i);
                println!("Expected:\n{}", gold);
                println!("Found:\n{}", state);
            }
        } else {
            println!("Testcase {}:\n{}", i, state);
        }
    }
    return Ok(());
}
