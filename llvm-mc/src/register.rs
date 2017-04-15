use super::bindgen::root as cpp;
use self::cpp::llvm as llvm;

use std::ffi::CStr;
use std::fmt;
use std::os::raw::{c_int, c_uint};

/// A description of the various registers and subregisters present in a target
/// architecture. This class mostly corresponds to LLVM's [MCRegisterInfo][1].
///
/// [1]: http://www.llvm.org/docs/doxygen/html/classllvm_1_1MCRegisterInfo.html
pub struct RegisterInfo {
    reg_classes: Vec<RegisterClass>,
    registers: Vec<Register>
}

impl RegisterInfo {
    /// Initialize from the underlying MCRegisterInfo.
    pub fn get(mri: &llvm::MCRegisterInfo) -> RegisterInfo {
        RegisterInfo {
            reg_classes: RegisterInfo::init_reg_classes(&mri),
            registers: RegisterInfo::init_registers(&mri)
        }
    }

    fn init_reg_classes(mri: &llvm::MCRegisterInfo) -> Vec<RegisterClass> {
        let mut reg_classes = Vec::new();
        unsafe {
            let num = mri.getNumRegClasses();
            for i in 0..num {
                let class = mri.getRegClass(i);
                let name = CStr::from_ptr(mri.getRegClassName(class))
                    .to_str().expect("RegClass names should be ASCII");
                reg_classes.push(RegisterClass::get(name,
                    class.as_ref().expect("Should not be null")));
            }
        }
        return reg_classes;
    }

    fn init_registers(mri: &llvm::MCRegisterInfo) -> Vec<Register> {
        let mut registers = Vec::new();
        unsafe {
            let num = mri.getNumRegs();
            for i in 1..num {
                let name = CStr::from_ptr(mri.getName(i))
                    .to_str().expect("Register names should be ASCII");
                registers.push(Register::get(name, i, mri));
            }
        }
        return registers;
    }

    /// Return the list of register classes in this target.
    pub fn get_register_classes(&self) -> &Vec<RegisterClass> {
        &self.reg_classes
    }

    /// Get a register class with a specific name.
    pub fn get_register_class(&self, name: &str) -> Option<&RegisterClass> {
        self.reg_classes.iter().find(|c| c.name == name)
    }

    /// Return the list of registers in this target.
    pub fn get_registers(&self) -> &Vec<Register> {
        &self.registers
    }

    /// Get a specific register by name.
    pub fn get_register(&self, name: &str) -> Option<&Register> {
        self.registers.iter().find(|c| c.name == name)
    }

    /// Get the list of top-level registers on thsi target.
    pub fn get_top_level_registers(&self) -> Vec<&Register> {
        self.registers.iter().filter(|&r| r.super_regs.is_empty()).collect()
    }
}

/// A class of registers, for example, the set of general-purpose 32-bit
/// registers, or the set of x87 floating point stack registers.
#[derive(Debug)]
pub struct RegisterClass {
    /// The name of the register class, e.g., GP32.
    pub name: &'static str,
    /// The size of this register class in bytes.
    pub size: usize,
    /// The minimum alignment of this register class.
    pub align: usize,

    registers: Vec<usize>
}

impl RegisterClass {
    fn get(name: &'static str, class: &llvm::MCRegisterClass) -> Self {
        unsafe {
            RegisterClass {
                name: name,
                size: class.getSize() as usize,
                align: class.getAlignment() as usize,
                registers: (0..class.getNumRegs())
                    .map(|i| (class.getRegister(i) - 1) as usize).collect()
            }
        }
    }

    /// Get the list of registers in this register class.
    pub fn get_registers<'a>(&self,
                         reg_info: &'a RegisterInfo) -> Vec<&'a Register> {
        self.registers.iter().map(|&reg| &reg_info.registers[reg]).collect()
    }
}

impl fmt::Display for RegisterClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Eq)]
pub struct Register {
    /// The name of the register, e.g., RAX.
    pub name: &'static str,

    /// The number of the register as used in DWARF sections (if it exists).
    pub dwarf_num: Option<u32>,

    /// The number of the register as used in .eh_frame tables (if it exists).
    pub dwarf_eh_num: Option<u32>,

    sub_regs: Vec<usize>,
    super_regs: Vec<usize>
}

fn map_dwarf_num(num: c_int) -> Option<u32> {
    if num >= 0 {
        Some(num as u32)
    } else {
        None
    }
}

impl Register {
    fn get(name: &'static str, num: c_uint,
           mri: &llvm::MCRegisterInfo) -> Register {
        unsafe {
            // LLVM does not make it easy to get the list of subregisters or the
            // list of super registers. So, we'll instead do this the hard way.
            let sub_regs = (1..mri.getNumRegs())
                .filter(|&i| mri.isSubRegister(num, i))
                .map(|i| (i - 1) as usize)
                .collect();
            let super_regs = (1..mri.getNumRegs())
                .filter(|&i| mri.isSubRegister(i, num))
                .map(|i| (i - 1) as usize)
                .collect();

            Register {
                name: name,
                dwarf_num: map_dwarf_num(mri.getDwarfRegNum(num, false)),
                dwarf_eh_num: map_dwarf_num(mri.getDwarfRegNum(num, true)),
                sub_regs: sub_regs,
                super_regs: super_regs
            }
        }
    }

    /// Get all registers that are contained within this register.
    pub fn get_sub_registers<'a>(&self, reg_info: &'a RegisterInfo) ->
          Vec<&'a Register> {
        self.sub_regs.iter().map(|&r| &reg_info.registers[r]).collect()
    }

    /// Get all registers that this register is a part of.
    pub fn get_super_registers<'a>(&self, reg_info: &'a RegisterInfo) ->
          Vec<&'a Register> {
        self.super_regs.iter().map(|&r| &reg_info.registers[r]).collect()
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Debug for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Register({})", self.name)
    }
}

impl PartialEq for Register {
    fn eq(&self, rhs: &Register) -> bool {
        self.name == rhs.name
    }
}

#[cfg(test)]
mod tests {
    use ::{Register, RegisterInfo};
    use ::TargetTriple;

    fn reg_list<'a>(mri: &'a RegisterInfo,
                    names: Vec<&str>) -> Vec<&'a Register> {
        names.iter().map(|name| mri.get_register(name)
                                   .expect("Register name doesn't exist"))
             .collect()
    }

    #[test]
    fn test_register_classes() {
        let target = TargetTriple::get("amd64").expect("Need amd64 support");
        let mri = target.register_info();
        let classes = mri.get_register_classes();
        let mut names : Vec<&str> = classes.iter().map(|c| c.name).collect();
        names.sort();
        assert_eq!(names, vec!["BNDR", "CCR", "CONTROL_REG", "DEBUG_REG",
                   "FPCCR", "FR128", "FR32", "FR32X", "FR64", "FR64X", "GR16",
                   "GR16_ABCD", "GR16_NOREX", "GR32", "GR32_ABCD",
                   "GR32_ABCD_and_GR32_NOAX", "GR32_AD",
                   "GR32_AD_and_GR32_NOAX", "GR32_NOAX",
                   "GR32_NOAX_and_GR32_NOREX", "GR32_NOAX_and_GR32_NOREX_NOSP",
                   "GR32_NOAX_and_GR32_NOSP", "GR32_NOAX_and_GR32_TC",
                   "GR32_NOREX", "GR32_NOREX_NOSP", "GR32_NOSP", "GR32_TC",
                   "GR64", "GR64_ABCD", "GR64_NOREX", "GR64_NOREX_NOSP",
                   "GR64_NOREX_NOSP_and_GR64_TC",
                   "GR64_NOREX_NOSP_and_LOW32_ADDR_ACCESS_RBP",
                   "GR64_NOREX_and_GR64_TC", "GR64_NOREX_and_GR64_TCW64",
                   "GR64_NOSP", "GR64_NOSP_and_GR64_TC",
                   "GR64_NOSP_and_GR64_TCW64", "GR64_TC", "GR64_TCW64",
                   "GR64_TCW64_and_GR64_TC_and_GR64_with_sub_32bit_in_GR32_NOAX",
                   "GR64_TCW64_and_GR64_with_sub_32bit_in_GR32_NOAX",
                   "GR64_TC_and_GR64_NOSP_and_GR64_TCW64",
                   "GR64_TC_and_GR64_TCW64",
                   "GR64_TC_and_GR64_with_sub_32bit_in_GR32_NOAX",
                   "GR64_TC_and_GR64_with_sub_32bit_in_GR32_NOAX_and_GR32_NOREX",
                   "GR64_and_LOW32_ADDR_ACCESS",
                   "GR64_and_LOW32_ADDR_ACCESS_RBP",
                   "GR64_with_sub_16bit_in_GR16_NOREX",
                   "GR64_with_sub_32bit_in_GR32_ABCD_and_GR32_NOAX",
                   "GR64_with_sub_32bit_in_GR32_AD",
                   "GR64_with_sub_32bit_in_GR32_AD_and_GR32_NOAX",
                   "GR64_with_sub_32bit_in_GR32_NOAX",
                   "GR64_with_sub_32bit_in_GR32_NOAX_and_GR32_NOREX",
                   "GR64_with_sub_32bit_in_GR32_NOAX_and_GR32_NOREX_NOSP",
                   "GR64_with_sub_32bit_in_GR32_NOAX_and_GR32_NOSP",
                   "GR64_with_sub_32bit_in_GR32_NOAX_and_GR32_TC",
                   "GR64_with_sub_32bit_in_GR32_TC",
                   "GR64_with_sub_8bit", "GR8", "GR8_ABCD_H", "GR8_ABCD_L",
                   "GR8_NOREX", "LOW32_ADDR_ACCESS", "LOW32_ADDR_ACCESS_RBP", 
                   "LOW32_ADDR_ACCESS_RBP_with_sub_16bit_in_GR16_NOREX",
                   "LOW32_ADDR_ACCESS_RBP_with_sub_32bit",
                   "LOW32_ADDR_ACCESS_RBP_with_sub_8bit",
                   "LOW32_ADDR_ACCESS_RBP_with_sub_8bit_with_sub_32bit",
                   "LOW32_ADDR_ACCESS_with_sub_32bit", "RFP32", "RFP64",
                   "RFP80", "RST", "SEGMENT_REG", "VK1", "VK16", "VK16WM",
                   "VK1WM", "VK2", "VK2WM", "VK32", "VK32WM", "VK4", "VK4WM",
                   "VK64", "VK64WM", "VK8", "VK8WM", "VR128", "VR128H",
                   "VR128L", "VR128X", "VR256", "VR256H", "VR256L", "VR256X",
                   "VR512", "VR512_with_sub_xmm_in_FR128",
                   "VR512_with_sub_xmm_in_VR128H",
                   "VR512_with_sub_xmm_in_VR128L", "VR64"]);

        assert!(mri.get_register_class("GR32").is_some(), "has GR32");
        assert!(mri.get_register_class("I do not exist").is_none());

        let gr32 = mri.get_register_class("GR32").unwrap();
        assert_eq!(gr32.name, "GR32");
        assert_eq!(gr32.size, 4);
        assert_eq!(gr32.align, 4);
        assert_eq!(gr32.get_registers(mri), reg_list(mri, vec![
            "EAX", "ECX", "EDX", "ESI", "EDI", "EBX", "EBP", "ESP",
            "R8D", "R9D", "R10D", "R11D", "R14D", "R15D", "R12D", "R13D"]));

        let x87 = mri.get_register_class("RFP80").unwrap();
        assert_eq!(x87.name, "RFP80");
        assert_eq!(x87.size, 10);
        assert_eq!(x87.align, 4);
        assert_eq!(format!("{}", x87), "RFP80");
    }

    #[test]
    fn test_registers() {
        let target = TargetTriple::get("amd64").expect("Need amd64 support");
        let mri = target.register_info();
        let regs = mri.get_registers();
        let mut names : Vec<&str> = regs.iter().map(|c| c.name).collect();
        names.sort();
        assert_eq!(names, vec!["AH", "AL", "AX", "BH", "BL", "BND0", "BND1",
                   "BND2", "BND3", "BP", "BPL", "BX", "CH", "CL", "CR0", "CR1",
                   "CR10", "CR11", "CR12", "CR13", "CR14", "CR15", "CR2", "CR3",
                   "CR4", "CR5", "CR6", "CR7", "CR8", "CR9", "CS", "CX", "DH",
                   "DI", "DIL", "DL", "DR0", "DR1", "DR10", "DR11", "DR12",
                   "DR13", "DR14", "DR15", "DR2", "DR3", "DR4", "DR5", "DR6",
                   "DR7", "DR8", "DR9", "DS", "DX", "EAX", "EBP", "EBX", "ECX",
                   "EDI", "EDX", "EFLAGS", "EIP", "EIZ", "ES", "ESI", "ESP",
                   "FP0", "FP1", "FP2", "FP3", "FP4", "FP5", "FP6", "FP7",
                   "FPSW", "FS", "GS", "IP", "K0", "K1", "K2", "K3", "K4", "K5",
                   "K6", "K7", "MM0", "MM1", "MM2", "MM3", "MM4", "MM5", "MM6",
                   "MM7", "R10", "R10B", "R10D", "R10W", "R11", "R11B", "R11D",
                   "R11W", "R12", "R12B", "R12D", "R12W", "R13", "R13B", "R13D",
                   "R13W", "R14", "R14B", "R14D", "R14W", "R15", "R15B", "R15D",
                   "R15W", "R8", "R8B", "R8D", "R8W", "R9", "R9B", "R9D", "R9W",
                   "RAX", "RBP", "RBX", "RCX", "RDI", "RDX", "RIP", "RIZ",
                   "RSI", "RSP", "SI", "SIL", "SP", "SPL", "SS", "ST0", "ST1",
                   "ST2", "ST3", "ST4", "ST5", "ST6", "ST7", "XMM0", "XMM1",
                   "XMM10", "XMM11", "XMM12", "XMM13", "XMM14", "XMM15",
                   "XMM16", "XMM17", "XMM18", "XMM19", "XMM2", "XMM20",
                   "XMM21", "XMM22", "XMM23", "XMM24", "XMM25", "XMM26",
                   "XMM27", "XMM28", "XMM29", "XMM3", "XMM30", "XMM31", "XMM4",
                   "XMM5", "XMM6", "XMM7", "XMM8", "XMM9", "YMM0", "YMM1",
                   "YMM10", "YMM11", "YMM12", "YMM13", "YMM14", "YMM15",
                   "YMM16", "YMM17", "YMM18", "YMM19", "YMM2", "YMM20",
                   "YMM21", "YMM22", "YMM23", "YMM24", "YMM25", "YMM26",
                   "YMM27", "YMM28", "YMM29", "YMM3", "YMM30", "YMM31", "YMM4",
                   "YMM5", "YMM6", "YMM7", "YMM8", "YMM9", "ZMM0", "ZMM1",
                   "ZMM10", "ZMM11", "ZMM12", "ZMM13", "ZMM14", "ZMM15",
                   "ZMM16", "ZMM17", "ZMM18", "ZMM19", "ZMM2", "ZMM20", "ZMM21",
                   "ZMM22", "ZMM23", "ZMM24", "ZMM25", "ZMM26", "ZMM27",
                   "ZMM28", "ZMM29", "ZMM3", "ZMM30", "ZMM31", "ZMM4", "ZMM5",
                   "ZMM6", "ZMM7", "ZMM8", "ZMM9"]);

        assert!(mri.get_register("R15W").is_some(), "has R15W");
        assert!(mri.get_register("I do not exist").is_none());

        let reg = mri.get_register("EAX").expect("amd64 needs %eax");
        assert_eq!(reg.name, "EAX");
        assert!(reg.dwarf_num.is_none());
        assert_eq!(mri.get_register("RAX").unwrap().dwarf_num.unwrap(), 0);

        // Test sub/super register support.
        assert_eq!(reg.get_sub_registers(mri), reg_list(mri, vec![
            "AH", "AL", "AX"]));
        assert_eq!(reg.get_super_registers(mri), reg_list(mri, vec!["RAX"]));

        // List of all top-level registers.
        assert_eq!(mri.get_top_level_registers(), reg_list(mri, vec![
            "CS", "DS", "EFLAGS", "EIZ", "ES", "FPSW", "FS", "GS", "RAX", "RBP",
            "RBX", "RCX", "RDI", "RDX", "RIP", "RIZ", "RSI", "RSP", "SS",
            "BND0", "BND1", "BND2", "BND3", "CR0", "CR1", "CR2", "CR3", "CR4",
            "CR5", "CR6", "CR7", "CR8", "CR9", "CR10", "CR11", "CR12", "CR13",
            "CR14", "CR15", "DR0", "DR1", "DR2", "DR3", "DR4", "DR5", "DR6",
            "DR7", "DR8", "DR9", "DR10", "DR11", "DR12", "DR13", "DR14", "DR15",
            "FP0", "FP1", "FP2", "FP3", "FP4", "FP5", "FP6", "FP7", "K0", "K1",
            "K2", "K3", "K4", "K5", "K6", "K7", "MM0", "MM1", "MM2", "MM3",
            "MM4", "MM5", "MM6", "MM7", "R8", "R9", "R10", "R11", "R12", "R13",
            "R14", "R15", "ST0", "ST1", "ST2", "ST3", "ST4", "ST5", "ST6",
            "ST7", "ZMM0", "ZMM1", "ZMM2", "ZMM3", "ZMM4", "ZMM5", "ZMM6",
            "ZMM7", "ZMM8", "ZMM9", "ZMM10", "ZMM11", "ZMM12", "ZMM13", "ZMM14",
            "ZMM15", "ZMM16", "ZMM17", "ZMM18", "ZMM19", "ZMM20", "ZMM21",
            "ZMM22", "ZMM23", "ZMM24", "ZMM25", "ZMM26", "ZMM27", "ZMM28",
            "ZMM29", "ZMM30", "ZMM31"]));
    }
}
