use ::arch;

use regex::Regex;
use std::sync::Mutex;
use std::fmt;
use std::io;
use std::iter;
use std::ops::Range;
use std::str::FromStr;

mod errors {
    error_chain!{
        foreign_links {
            Null(::std::ffi::NulError);
            Fmt(::std::num::ParseIntError);
            Io(::std::io::Error);
            Ffi(::std::ffi::IntoStringError);
        }
    }
}

pub use self::errors::{Error, Result, ResultExt};

#[repr(C)]
#[derive(Debug, Eq, Clone)]
pub struct RegState {
    _state: arch::host::RegState
}

impl fmt::Display for RegState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self._state, f)
    }
}

lazy_static! {
    static ref REGISTER_CHECK: Mutex<Vec<(String, Range<usize>)>> = {
        Mutex::new(
        (0..arch::host::REGISTER_BANKS)
            .map(|bank| (bank, arch::host::RegState::get_bank_size(bank)))
            .flat_map(|(bank, bank_size)| {
                arch::host::RegState::get_bank_registers(bank).iter()
                    .zip(iter::repeat(bank_size))
                    .map(|(reg, bank_size)| (String::from(*reg), 0..bank_size))
            })
            .collect()
        )
    };
}

pub fn only_compare_registers(reg_list: &str) {
    let regs : Vec<_> = reg_list.split(" ")
        .map(|reg| {
            if let Some(idx) = reg.find(":") {
                let (reg, idx) = reg.split_at(idx);
                (reg, usize::from_str(&idx[1..]).expect("Can't parse register"))
            } else {
                let size = REGISTER_CHECK.lock().unwrap().iter()
                    .find(|&&(ref r, ref size)| r == reg)
                    .expect("Unknown register")
                    .1.end;
                (reg, size)
            }
        })
        .map(|(reg, size)| {
            (String::from(reg), 0..size)
        })
        .collect();
    *REGISTER_CHECK.lock().unwrap() = regs;
}

impl RegState {
    fn mut_state(&self) -> &mut arch::host::RegState {
        unsafe {
            (&self._state as *const arch::host::RegState
                as *mut arch::host::RegState).as_mut().unwrap()
        }
    }

    /// Allocate a new register state with the stack pointer initially pointing
    /// at the given address.
    pub fn new(sp: u64) -> RegState {
        let mut state : arch::host::RegState = Default::default();
        *state.get_stack_register() = sp;
        return RegState { _state: state };
    }

    /// Set a particular register to have a given value.
    pub fn set_register<T>(&mut self, reg: &str, val: T) -> Result<()> {
        self.set_register_bytes(reg, arch::as_bytes(&val))
    }

    pub fn set_stack_pointer(&mut self, val: u64) {
        *self._state.get_stack_register() = val;
    }

    pub fn set_register_bytes(&mut self, reg: &str, val: &[u8]) -> Result<()> {
        self._state.get_register(reg)
            .ok_or(format!("{} is not a register", reg).into())
            .and_then(|ptr| {
                if ptr.len() != val.len() {
                    return Err("Invalid size".into());
                }
                ptr.copy_from_slice(val);
                return Ok(());
            })
    }

    pub fn get_register_bytes(&self, reg: &str) -> Result<&[u8]> {
        self.mut_state().get_register(reg)
            .ok_or(format!("{} is not a register", reg).into())
            .map(|s| &*s)
    }

    pub fn set_flag(&mut self, flag: &str, val: bool) -> Result<()> {
        self._state.get_flag(flag)
            .ok_or(format!("{} is not a flag", flag).into())
            .and_then(|(flags, index)| {
                let mask = 1 << index;
                *flags = if val { *flags | mask } else { *flags & !mask };
                return Ok(());
            })
    }

    pub fn get_flag(&mut self, flag: &str) -> Result<bool> {
        self.mut_state().get_flag(flag)
            .ok_or(format!("{} is not a flag", flag).into())
            .and_then(|(flags, index)| {
                let mask = 1 << index;
                return Ok(*flags & mask == mask);
            })
    }

    pub fn parse_text(file: &mut io::BufRead) -> Result<RegState> {
        let mut state = RegState { _state: Default::default() };
        let mut line = String::new();
        let mut next_line = || -> Result<String> {
            line.clear();
            if file.read_line(&mut line)? > 0 {
                return Ok(line.clone());
            } else {
                return Err("Unexpected EOF".into());
            }
        };

        // First line should be signal.
        if let Some(captures) = SIGNAL_RE.captures(next_line()?.trim()) {
            let trap_no = u64::from_str(captures.get(1).unwrap().as_str())?;
            let signal_name = captures.get(2).unwrap().as_str();
            state._state.trap = trap_no;
            // XXX: check signal name correctness.
        } else {
            return Err("Didn't find signal condition".into());
        }

        if !next_line()?.trim().is_empty() { return Err("Missing line break".into()); }

        // Parse register banks.
        for _ in 0..arch::host::REGISTER_BANKS {
            loop {
                let line = next_line()?;
                let line = line.trim();
                if line.is_empty() { break; }

                if let Some(captures) = REG_RE.captures(line) {
                    let reg = captures.get(1).unwrap().as_str();
                    let value_str = captures.get(2).unwrap().as_str();
                    let mut bytes : Vec<u8> = BYTES_RE.find_iter(value_str)
                        .map(|s| u8::from_str_radix(s.as_str(), 16).unwrap())
                        .collect();
                    bytes.reverse();
                    state.set_register_bytes(reg, &bytes)?;
                }
            }
        }

        // Parse flag banks (only one for the moment, hardcoded)
        for _ in 0..1 {
            loop {
                let line = next_line()?;
                let line = line.trim();
                if line.is_empty() { break; }

                if let Some(captures) = FLAG_RE.captures(line) {
                    let flag = captures.get(1).unwrap().as_str();
                    let value_str = captures.get(2).unwrap().as_str();
                    let value = value_str == "1";
                    if flag == "0" || flag == "1" {
                        if value_str != flag {
                            return Err("Expected hardcoded flag value".into());
                        }
                    } else {
                        state.set_flag(flag, value)?;
                    }
                } else {
                    return Err("Expected flag line".into());
                }
            }
        }

        return Ok(state);
    }

    pub fn find_differences(&self, other: &Self) -> Vec<String> {
        let mut diffs = Vec::new();
        let mut us = self.mut_state();
        let mut them = other.mut_state();
        if us.trap != them.trap {
            diffs.push("trap".into());
        }

        let registers = REGISTER_CHECK.lock().unwrap();

        for &(ref register, ref range) in (*registers).iter() {
            if us.get_register(&register).unwrap()[range.clone()] !=
                them.get_register(&register).unwrap()[range.clone()] {
                diffs.push(register.clone());
            }
        }

        // XXX: Add flag support.
        //if us.rflags != them.rflags {
        //    diffs.push("flags".into());
        //}

        return diffs;
    }
}

impl PartialEq for RegState {
    fn eq(&self, other: &Self) -> bool {
        return self.find_differences(other).is_empty();
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

impl Memory {
    pub fn allocate(base: u64, size: usize,
                    perms: Permissions) -> Memory {
        let mut contents = Vec::with_capacity(size);
        contents.resize(size, 0);
        return Memory { base, contents, flags: perms };
    }

    pub fn parse_text(file: &mut io::BufRead,
                      perms: Permissions) -> Result<Memory> {
        let mut line = String::new();
        let mut next_line = || -> Result<String> {
            line.clear();
            if file.read_line(&mut line)? > 0 {
                return Ok(line.clone());
            } else {
                return Err("Unexpected EOF".into());
            }
        };

        fn parse_addr(addr: &str) -> Result<u64> {
            let mut halves = addr.split(' ');
            let hi = u64::from_str_radix(halves.next().unwrap(), 16)?;
            let lo = u64::from_str_radix(halves.next().unwrap(), 16)?;
            return Ok(hi << 32 | lo);
        }

        // Find the range, and initialize the backing memory.
        let line = next_line()?;
        let captures = RANGE_RE.captures(line.trim())
            .ok_or("Didn't find range")?;
        let start = parse_addr(captures.get(2).unwrap().as_str())?;
        let end = parse_addr(captures.get(1).unwrap().as_str())?;
        if start > end {
            bail!("Start must be less than end");
        }

        let mut bytes : Vec<u8> = Vec::with_capacity((end - start) as usize);

        // Read the contents. The memory is printed out in effectively reverse
        // order (we start from the end and work back to the beginning as we
        // read, both top-to-bottom and left-to-right), so the easiest way to
        // actually read the contents is to read it in textual order and then
        // punch the reverse button at the end.
        let num_rows = NUM_ROWS_RE.captures(next_line()?.trim())
            .ok_or("Expected number of rows")?
            .get(1).map(|s| u64::from_str(s.as_str()))
            .unwrap()?;

        if !next_line()?.trim().is_empty() { return Err("Missing line break".into()); }
        for index in 0..num_rows {
            let line = next_line()?;
            let captures = ROW_RE.captures(line.trim())
                .ok_or("Expected row information")?;
            let row_start = parse_addr(captures.get(1).unwrap().as_str())?;
            let value_str = captures.get(2).unwrap().as_str();

            let expected_offset = end - (index + 1) * 8;
            if row_start != expected_offset {
                bail!(format!("Expected offset {:x}", expected_offset));
            }

            let mut row_bytes : Vec<u8> = BYTES_RE.find_iter(value_str)
                .map(|s| u8::from_str_radix(s.as_str(), 16).unwrap())
                .collect();
            bytes.append(&mut row_bytes);
        }
        bytes.reverse();

        // Clean up a following newline to make sure we're at the end.
        if num_rows > 0 && !next_line()?.trim().is_empty() {
            return Err("Missing line break".into());
        }

        return Ok(Memory {
            flags: perms,
            base: start,
            contents: bytes
        });
    }
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

impl State {
    pub fn parse_text(file: &mut io::BufRead) -> Result<State> {
        fn expect_line(file: &mut io::BufRead, expected: &str) -> Result<()> {
            let mut line = String::new();
            if file.read_line(&mut line)? > 0 {
                if line.trim() != expected {
                    return Err(format!("Expected {}", expected).into());
                } else {
                    return Ok(());
                }
            } else {
                return Err("Unexpected EOF".into());
            }
        }

        let registers = RegState::parse_text(file)
            .chain_err(|| "unable to parse registers")?;
        let stack = Memory::parse_text(file, Permissions::ReadWrite)
            .chain_err(|| "unable to parse stack")?;
        Memory::parse_text(file, Permissions::ReadWrite)
            .chain_err(|| "unable to parse heap")?;
        Memory::parse_text(file, Permissions::ReadWrite)
            .chain_err(|| "unable to parse data")?;

        expect_line(file, "0 more segment(s)")?;

        return Ok(State {
            registers: registers,
            stack: stack,
            other_banks: Vec::new()
        });
    }

    pub fn parse_testcases(file: &mut io::BufRead) -> Result<Vec<State>> {
        fn next_line(file: &mut io::BufRead) -> Result<Option<String>> {
            let mut line = String::new();
            line.clear();
            if file.read_line(&mut line)? > 0 {
                line.pop(); // Pop the newline.
                return Ok(Some(line));
            } else {
                return Ok(None);
            }
        }
        let mut testcases = Vec::new();
        while let Some(line) = next_line(file)? {
            // Skip any empty lines we find.
            if line.is_empty() { continue; }
            let number = TESTCASE_RE.captures(&line)
                .ok_or("Excepted testcase line")?
                .get(1).map(|s| usize::from_str(s.as_str())).unwrap().unwrap();
            if number != testcases.len() {
                bail!(format!("Expected testcase {}", testcases.len()));
            }
            next_line(file)?; // Skip empty line.
            testcases.push(Self::parse_text(file)
                .chain_err(|| format!("Parsing testcase {}", number))?);
        }
        return Ok(testcases);
    }

    pub fn find_differences(&self, other: &Self) -> Vec<String> {
        let mut diffs = self.registers.find_differences(&other.registers);

        if self.stack != other.stack {
            diffs.push("stack".into());
        }

        return diffs;
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.registers)?;
        writeln!(f, "{}", self.stack)?;
        // We don't use the heap or data segments...
        writeln!(f, "[ 00000001 00000000 - 00000001 00000000 ]")?;
        writeln!(f, "[ 0 valid rows shown ]")?;
        writeln!(f, "")?;
        writeln!(f, "[ 00000000 00000000 - 00000000 00000000 ]")?;
        writeln!(f, "[ 0 valid rows shown ]")?;
        writeln!(f, "")?;
        writeln!(f, "{} more segment(s)", self.other_banks.len())?;
        // XXX: actually output said segments
        return Ok(());
    }
}

macro_rules! addr {
    () => { "([0-9a-f]{8} [0-9a-f]{8})" }
}
lazy_static! {
    static ref TESTCASE_RE: Regex =
        Regex::new(r"^Testcase (\d+):$").unwrap();
    static ref SIGNAL_RE: Regex =
        Regex::new(r"^SIGNAL (\d+) \[([^\]]+)\]$").unwrap();
    static ref REG_RE: Regex =
        Regex::new(r"^%(\w+)((?:\s*[0-9a-f]{2})+)$").unwrap();
    static ref BYTES_RE: Regex = Regex::new(r"[0-9a-f]{2}").unwrap();
    static ref FLAG_RE: Regex =
        Regex::new(r"^%([a-z0-9\[\]]+)\s+([01])$").unwrap();
    static ref RANGE_RE: Regex =
        Regex::new(concat!(r"^\[ ", addr!(), " - ", addr!(), r" \]$"))
        .unwrap();
    static ref NUM_ROWS_RE: Regex =
        Regex::new(r"^\[ (\d+) valid rows shown \]$").unwrap();
    static ref ROW_RE: Regex =
        Regex::new(concat!(r"^", addr!(), r"(?:\s*v){8}",
                           r"((?:\s+[0-9a-f]{2}){8})"))
        .unwrap();
}
