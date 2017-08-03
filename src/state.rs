use llvm::{CBox, Context};
use llvmmc::{Instruction, TargetTriple};
use serde_json;
use serde_json::Value;
use sema::FunctionInfo;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};
use std::path::PathBuf;

pub struct State<'a> {
    llvm_ctx: CBox<Context>,
    path: PathBuf,
    tt: &'a TargetTriple<'a>,
    cache: RefCell<HashMap<String, InstructionInfo>>,
    pseudo_functions: HashMap<&'static str, FunctionInfo>
}

impl <'a> State<'a> {
    pub fn load(path: PathBuf, tt: &'a TargetTriple) -> State<'a> {
        State {
            llvm_ctx: Context::new(),
            path: path,
            tt: tt,
            cache: Default::default(),
            pseudo_functions: FunctionInfo::get_functions()
        }
    }

    pub fn get_llvm_context(&self) -> &Context {
        return &self.llvm_ctx;
    }

    pub fn get_workdir(&self) -> PathBuf {
        self.path.clone()
    }

    pub fn get_target_triple(&self) -> &TargetTriple {
        self.tt
    }

    pub fn get_instructions(&self,
                            state: InstructionState) -> Vec<InstructionInfo> {
        let mut file = self.path.clone();
        file.push("information");
        file.push(match state {
            InstructionState::Base => "initial_base.instrs",
            InstructionState::Success => "success.instrs",
            InstructionState::PartialSuccess => "partial_success.instrs",
            InstructionState::Unsolved => "remaining_goal.instrs"
        });

        let contents = BufReader::new(File::open(file)
            .expect("State directory is missing filename"))
            .lines();
        let mut instrs = Vec::new();
        for opcode in contents {
            let name = opcode.unwrap();
            // Some instructions are broken according to LLVM's assembler.
            // Don't bother with these (they're just NOPs anyhow).
            match &name as &str {
                "nopl_r32" | "nopw_r16" => continue,
                "vcvtdq2pd_ymm_ymm" => continue,
                _ => {}
            };
            instrs.push(InstructionInfo::load(&name, &self));
        }
        return instrs;
    }

    pub fn get_info(&self, mcinst: &Instruction) -> Option<InstructionInfo> {
        if self.cache.borrow().is_empty() {
            let instrs = self.get_instructions(InstructionState::Success);
            for inst in instrs {
                let path = inst.get_inst_file(self);
                let mut file = File::open(&path).expect("Could not find file");
                let mut contents = String::new();
                file.read_to_string(&mut contents)
                    .expect("Could not read file");
                let base = self.tt.parse_instructions("", "", &contents);
                if base.is_empty() {
                    println!("We need to add info for {}", inst.opcode);
                    continue;
                }
                assert!(base.len() == 2, "We expect the instruction file to have little");
                assert!(base[base.len() - 1].opcode.is_return());

                self.cache.borrow_mut()
                    .insert(String::from(base[0].opcode.name), inst);
            }
        }

        return self.cache.borrow().get(mcinst.opcode.name)
            .map(|ii| ii.clone());
    }

    pub fn get_pseudo_instruction(&self, name: &str) -> Option<&FunctionInfo> {
        return self.pseudo_functions.get(name);
    }

    pub fn get_pseudo_instructions<'b>(&'b self)
        -> Box<Iterator<Item=&FunctionInfo> + 'b> {
        Box::new(self.pseudo_functions.values())
    }
}

pub enum InstructionState {
    Base,
    Success,
    PartialSuccess,
    Unsolved
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct InstructionInfo {
    pub opcode: String,
    pub def_in: String,
    pub live_out: String,
}

impl InstructionInfo {
    fn load(opcode: &str, state: &State) -> InstructionInfo {
        let mut json_file = state.path.clone();
        json_file.push("instructions");
        json_file.push(opcode);
        json_file.push(format!("{}.meta.json", opcode));

        let mut ii = InstructionInfo {
            opcode: String::from(opcode),
            def_in: Default::default(),
            live_out: Default::default(),
        };
        if let Ok(file) = File::open(json_file) {
            let json : Value = serde_json::from_reader(file)
                .expect("Cannot parse JSON file");
            ii.def_in = String::from(json.get("def_in").unwrap().as_str().unwrap());
            ii.live_out = String::from(json.get("live_out").unwrap().as_str().unwrap());
        }

        return ii;
    }

    pub fn get_inst_file(&self, state: &State) -> PathBuf {
        let mut file = state.path.clone();
        file.push("instructions");
        file.push(&self.opcode);
        file.push(format!("{}.s", &self.opcode));
        return file;
    }

    pub fn get_circuit_file(&self, state: &State) -> PathBuf {
        let mut file = state.path.clone();
        file.push("circuits");
        file.push(format!("{}.s", &self.opcode));
        return file;
    }

    pub fn get_reg_names(&self, write: bool) -> Vec<&str> {
        let name_str = if write { &self.live_out } else { &self.def_in };
        return name_str[1..name_str.len() - 1].split_whitespace()
            .map(|n| &n[1..])
            .collect();
    }

    pub fn get_flag_names(&self, write: bool) -> Vec<&str> {
        let names = ["cf", "af", "pf", "zf", "sf", "of"];
        return self.get_reg_names(write).iter()
            .filter(|n| names.contains(n))
            .map(|n| *n)
            .collect();
    }
}
