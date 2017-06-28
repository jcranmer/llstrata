use ::arch;

use std::fmt;
use std::mem;

#[repr(C)]
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RegState {
    _state: arch::host::RegState
}

impl fmt::Display for RegState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self._state, f)
    }
}

impl RegState {
    /// Allocate a new register state with the stack pointer initially pointing
    /// at the given address.
    pub fn new(sp: u64) -> RegState {
        let mut state : arch::host::RegState = Default::default();
        *state.get_stack_register() = sp;
        return RegState { _state: state };
    }

    /// Set a particular register to have a given value.
    pub fn set_register<T>(&mut self, reg: &str, val: T) -> Result<(), String> {
        self._state.get_register(reg)
            .ok_or(format!("{} is not a register", reg))
            .and_then(|ptr| {
                if ptr.len() != mem::size_of::<T>() {
                    return Err(String::from("Invalid size"));
                }
                ptr.copy_from_slice(arch::as_bytes(&val));
                return Ok(());
            })
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Permissions {
    Read, ReadWrite, ReadExec, ReadWriteExec
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Memory {
    pub base: u64,
    pub contents: Vec<u8>,
    pub flags: Permissions
}

impl fmt::Display for Memory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn print_offset(f: &mut fmt::Formatter, ptr: u64) -> fmt::Result {
            write!(f, "{:08x} ", ptr >> 32)?;
            write!(f, "{:08x}", (ptr & 0xffffffff))
        }

        let end = self.base + self.contents.len() as u64;
        write!(f, "[ ")?;
        print_offset(f, end)?;
        write!(f, " - ")?;
        print_offset(f, self.base)?;
        writeln!(f, " ]")?;

        // Rows are measured in steps of 8 bytes.
        assert!(self.base & 0x8 == 0 && end & 0x8 == 0,
                "The code below requires 8-byte aligned memory!");
        let rows = self.contents.chunks(8);
        writeln!(f, "[ {} valid rows shown ]\n", rows.len())?;

        for (row, bytes) in rows.enumerate().rev() {
            print_offset(f, self.base + (row << 3) as u64)?;
            write!(f,"   v v v v v v v v  ")?;
            arch::print_hex(f, bytes)?;
            writeln!(f, "")?;
        }
        return Ok(());
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct State {
    pub registers: RegState,
    pub stack: Memory,
    pub other_banks: Vec<Memory>
}
