
use std::env;
use std::fs;
use std::io::prelude::*;
use std::process::{Command, Stdio};
use ::state::{InstructionInfo, InstructionState, State};
use tempdir::TempDir;

// bin/stoke search 
//   --config $STRATA/resources/conf-files/search.conf
//   --config $TMPDIR/base.conf
//   --target $WORKDIR/instructions/$INST/$INST.s
//   --def_in "{ %xmm1 %xmm2 }"
//   --live_out "{ %xmm1 }"
//   --functions $WORKDIR/functions
//   --testcases $TMPDIR/testcases.tc
//   --machine_output search.json
//   --call_weight 316
//   --timeout_iterations 200000
//   --cost correctness

macro_rules! path {
    ($base:expr, $( $comps:expr ),*) => {{
        let mut path = $base.clone();
        $(
            path.push($comps);
        )*
        path
    }}
}

pub fn search_instruction(state: &State, inst: &InstructionInfo) {
    // Get the path of the stoke binary.
    let mut stoke_path = env::current_exe()
        .expect("Can't get path of executable");
    stoke_path.pop();
    stoke_path.pop();
    stoke_path.pop();
    stoke_path.push("stoke");

    // Install some temporary files to a working directory for the search.
    let search_dir = TempDir::new("initial-search")
        .expect("Could not create temporary directory");
    let search_path = search_dir.path().to_path_buf();
    fs::copy(path!(state.get_workdir(), "testcases.tc"),
             path!(search_path, "testcases.tc"))
        .expect("Could not copy testcases");

    // Get the whitelist of allowed operands
    let success = state.get_instructions(InstructionState::Success);
    {
        let mut whitelist_file = fs::File::create(path!(search_path, "base.conf"))
            .expect("Need to create file");
        write!(whitelist_file, "--opc_whitelist \"{{");
        for instr in success {
            write!(whitelist_file, " {}", instr.opcode);
        }
        writeln!(whitelist_file, " }}\"");
    }

    let stoke_status = Command::new(path!(stoke_path, "bin", "stoke"))
        .arg("search")
        .arg("--config").arg("/home/jcranmer/source/contl/strata/resources/conf-files/search.conf")
        .arg("--config").arg(path!(search_path, "base.conf"))
        .arg("--target").arg(inst.get_inst_file(state))
        .arg("--def_in").arg(&inst.def_in)
        .arg("--live_out").arg(&inst.live_out)
        .arg("--functions").arg(path!(state.get_workdir(), "functions"))
        .arg("--testcases").arg(path!(search_path, "testcases.tc"))
        .arg("--machine_output").arg(path!(search_path, "search.json"))
        .arg("--call_weight").arg("316")
        .arg("--timeout_iterations").arg("200000")
        .arg("--cost").arg("correctness")
        .current_dir(search_path)
        .stdout(Stdio::null())
        .status().expect("Failed to execute stoke search");
    println!("Exit code: {:?}", stoke_status);
}
