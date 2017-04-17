use serde_json;
use serde_json::Value;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

pub struct State {
    path: PathBuf
}

impl State {
    pub fn load(path: PathBuf) -> State {
        State {
            path: path
        }
    }

    pub fn get_workdir(&self) -> PathBuf {
        self.path.clone()
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
            instrs.push(InstructionInfo::load(&opcode.unwrap(), &self));
        }
        return instrs;
    }
}

pub enum InstructionState {
    Base,
    Success,
    PartialSuccess,
    Unsolved
}

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
}
