use ::arch;
use ::state::{Memory, RegState, Result, State};

use libc;
use llvm_sys::core::*;
use llvm_sys::object::*;
use llvm_sys::prelude::*;
use std::ffi::{CString, CStr};
use std::mem;

pub trait ExecFragment {
    fn execute(&self, state: &mut State);
}

extern "C" {
    fn call_asm_func(regs: *mut RegState, asm: usize);
}

fn page_align(base: usize, len: usize) -> (usize, usize) {
    lazy_static! {
        static ref PAGE_MASK: usize =
            unsafe { libc::sysconf(libc::_SC_PAGE_SIZE) } as usize - 1;
    };
    let page_offset = base & *PAGE_MASK;
    return (base - page_offset, len + page_offset);
}

struct AsmFragment {
    object_file: LLVMObjectFileRef,
    start: usize
}

impl Drop for AsmFragment {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeObjectFile(self.object_file);
        }
    }
}

impl ExecFragment for AsmFragment {
    fn execute(&self, state: &mut State) {
        execute_assembly(state, self.start);
    }
}

fn execute_assembly(state: &mut State, func: usize) {
    use state::Permissions::*;
    use libc::*;

    // Code for mapping banks
    let map_bank = |bank: &Memory, extra_flags| {
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

pub fn link_file(path: &str) -> Result<Box<ExecFragment>> {
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

    let asm_frag = link_object(path, buffer)?;
    return Ok(asm_frag);
}

fn link_object(path: &str, buffer: LLVMMemoryBufferRef) ->
        Result<Box<AsmFragment>> {
    let object_file = unsafe { LLVMCreateObjectFile(buffer) };

    if object_file.is_null() {
        bail!(format!("{} is not an IR file", path));
    }

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

    return Ok(Box::new(AsmFragment {
        object_file: object_file,
        start: addr + section
    }));
}

