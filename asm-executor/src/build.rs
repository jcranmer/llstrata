extern crate gcc;

mod arch;

use std::env;
use std::fs::File;
use std::io;

fn main() {
    build_asm().unwrap();

    gcc::Config::new()
        .file("glue.s")
        .compile("libasm-glue.a");
}

fn build_asm() -> io::Result<()> {
    let target = env::var("TARGET").unwrap();
    let arch = target.split("-").next().unwrap();
    let asm_fun = match arch {
        "x86_64" => arch::amd64::write_asm,
        target => panic!("Unsupported target {}", target)
    };

    let mut asm_file = File::create("glue.s")?;
    asm_fun(&mut asm_file)?;

    return Ok(());
}
