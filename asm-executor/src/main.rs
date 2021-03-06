#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate lazy_static;
extern crate libc;
extern crate llvm_sys;
extern crate regex;

#[allow(dead_code)]
mod arch;
mod exec;
mod state;

use std::env;
use std::fs::File;
use std::io;
use std::process;
use state::{only_compare_registers,ResultExt,State};
use exec::link_file;

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    if args.len() < 3 {
        println!("Usage: {} <testcases> <program a> [<program b>]", program);
        return;
    }

    if let Some(reg_str) = args.get(4) {
        only_compare_registers(&reg_str);
    }

    let res = run_main(&args[1], &args[2], args.get(3).map(|s| &**s));
    if let Err(ref e) = res {
        println!("error: {}", e);
        for e in e.iter().skip(1) {
            println!("caused by: {}", e);
        }

        if let Some(backtrace) = e.backtrace() {
            println!("backtrace: {:?}", backtrace);
        }
        process::exit(2);
    } else if res.unwrap() {
        process::exit(1);
    }
}

fn run_main(testcase: &str, test: &str,
            gold: Option<&str>) -> state::Result<bool> {
    let tc_file = File::open(testcase)
        .chain_err(|| format!("Could not open file {}", testcase))?;
    let mut tests = State::parse_testcases(&mut io::BufReader::new(tc_file))
        .chain_err(|| "Could not parse testcases")?;

    let prog = link_file(test)
        .chain_err(|| "Error loading test program")?;

    let gold = gold.map_or(Ok(None), |g| link_file(g).map(|v| Some(v)))
        .chain_err(|| "Error loading gold program")?;

    let mut differed = false;

    for (i, test) in tests.iter_mut().enumerate() {
        let mut state = test.clone();
        prog.execute(&mut state);
        if let Some(ref gold_prog) = gold {
            let mut gold = test;
            gold_prog.execute(&mut gold);
            if state != *gold {
                println!("Testcase {} differs", i);
                println!("Expected:\n{}", gold);
                println!("Found:\n{}", state);
                println!("Differences: {}",
                         gold.find_differences(&state).join(", "));
                differed = true;
            }
        } else {
            println!("Testcase {}:\n{}", i, state);
        }
    }
    return Ok(differed);
}
