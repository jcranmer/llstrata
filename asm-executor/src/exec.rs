use ::arch;
use ::state::{Memory, RegState, Result, State};

use libc;
use llvm_sys;
use llvm_sys::core::*;
use llvm_sys::execution_engine::*;
use llvm_sys::ir_reader::*;
use llvm_sys::object::*;
use llvm_sys::prelude::*;
use std::ffi::{CString, CStr};
use std::mem;
use std::slice;
use std::str;

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


type RegMap = Vec<(&'static str, &'static str)>;

struct IRFragment {
    exec_engine: LLVMExecutionEngineRef,
    in_regs: RegMap,
    out_regs: RegMap,
    main: &'static CStr,
}

impl Drop for IRFragment {
    fn drop(&mut self) {
        unsafe { LLVMDisposeExecutionEngine(self.exec_engine) }
    }
}

impl ExecFragment for IRFragment {
    fn execute(&self, state: &mut State) {
        use ::state::Permissions::*;
        let addr = unsafe {
            LLVMGetFunctionAddress(self.exec_engine, self.main.as_ptr())
        };

        // Copy the state to the registers.
        let mut real_state = state.clone();
        real_state.other_banks.push(real_state.stack);
        real_state.stack = Memory::allocate(0x10000000, 0x100, ReadWrite);
        real_state.registers.set_stack_pointer(real_state.stack.base + 0x20);

        fn copy_registers(from: &RegState, to: &mut RegState,
                          list: &RegMap) -> Result<()> {
            for &(from_reg, to_reg) in list {
                if to_reg == "" {
                    panic!("Need to implement stack support!");
                } else {
                    to.set_register_bytes(to_reg,
                                          from.get_register_bytes(from_reg)?)?;
                }
            }
            return Ok(());
        }

        copy_registers(&state.registers, &mut real_state.registers,
                       &self.in_regs)
            .unwrap();
        execute_assembly(&mut real_state, addr as usize);
        copy_registers(&real_state.registers, &mut state.registers,
                       &self.out_regs)
            .unwrap();
    }
}

fn link_ir(path: &str,
           buffer: LLVMMemoryBufferRef) -> Result<Box<IRFragment>> {
    unsafe {
        use llvm_sys::target::*;
        LLVMLinkInMCJIT();
        LLVM_InitializeNativeTarget();
        LLVM_InitializeNativeAsmPrinter();
    }
    // Create a copy of the buffer
    let shadow = unsafe {
        let c_name = CString::new(path)?;
        LLVMCreateMemoryBufferWithMemoryRange(
            LLVMGetBufferStart(buffer),
            LLVMGetBufferSize(buffer),
            c_name.as_ptr(),
            0)
    };

    let context = unsafe { LLVMGetGlobalContext() };

    let module = unsafe {
        let mut out_module = mem::uninitialized();
        let mut error = mem::uninitialized();
        let failed = LLVMParseIRInContext(context, shadow, &mut out_module,
                                          &mut error);
        if failed != 0 {
            bail!(CString::from_raw(error).into_string()?);
        }
        out_module
    };

    let function = unsafe { LLVMGetFirstFunction(module) };
    fn get_attr(function: LLVMValueRef, name: &str) -> &str {
        let len = name.len();
        let chars = name.as_bytes().as_ptr() as *const i8;
        let attr_ref = unsafe {
            LLVMGetStringAttributeAtIndex(function,
                                          llvm_sys::LLVMAttributeFunctionIndex,
                                          chars, len as u32)
        };
        let mut val_length = 0;
        let val_chars = unsafe {
            LLVMGetStringAttributeValue(attr_ref, &mut val_length)
        } as *const u8;
        let bytes = unsafe {
            slice::from_raw_parts(val_chars, val_length as usize)
        };
        return unsafe { str::from_utf8_unchecked(bytes) };
    }
    let in_regs = get_attr(function, "in");
    let out_regs = get_attr(function, "out");

    // Build the in and output register map.
    let abi_in = vec![("rdi", 8), ("rsi", 8), ("rdx", 8), ("rcx", 8),
        ("r8", 8), ("r9", 8)];
    let abi_out = vec![("rax", 8), ("rdx", 8), ("rcx", 8),
        ("ymm0", 32), ("ymm1", 32), ("ymm2", 32), ("ymm3", 32)];

    fn map_registers(from_regs: &'static str,
                     reg_parms: &[(&'static str, u8)]) -> RegMap {
        let reg_list = from_regs.split(" ");
        let mut parm_iter = reg_parms.iter();
        return reg_list.map(|reg| {
            while let Some(&(parm_name, parm_size)) = parm_iter.next() {
                if parm_size != 8 {
                    continue; // XXX: check against size
                }
                return (reg, parm_name);
            }
            return (reg, "");
        }).collect();
    }
    let in_map = map_registers(in_regs, &abi_in);
    let mut out_map = map_registers(out_regs, &abi_out);
    for pair in out_map.iter_mut() {
        *pair = (pair.1, pair.0);
    }

    let ee = unsafe {
        let mut out_ee = mem::uninitialized();
        let mut error = mem::uninitialized();
        let failed = LLVMCreateJITCompilerForModule(&mut out_ee, module, 0,
                                                    &mut error);
        if failed != 0 {
            bail!(CString::from_raw(error).into_string()?);
        }
        out_ee
    };

    return Ok(Box::new(IRFragment {
        exec_engine: ee,
        main: unsafe { CStr::from_ptr(LLVMGetValueName(function)) },
        in_regs: in_map, out_regs: out_map
    }));
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

    if let Ok(ir_exec) = link_ir(path, buffer) {
        return Ok(ir_exec);
    }

    let asm_frag = link_object(path, buffer)?;
    return Ok(asm_frag);
}
