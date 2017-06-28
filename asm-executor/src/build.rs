extern crate gcc;

mod arch;

use std::env;
use std::fs::File;
use std::io;
use std::path::Path;

fn main() {
    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("glue.s");
    build_asm(&path).unwrap();

    gcc::Config::new()
        .file(&path)
        .compile("libasm-glue.a");
    println!("cargo:rustc-cfg=feature=\"not-build\"");
}

fn build_asm(path: &Path) -> io::Result<()> {
    let target = env::var("TARGET").unwrap();
    let arch = target.split("-").next().unwrap();
    let asm_fun = match arch {
        "x86_64" => arch::amd64::write_asm,
        target => panic!("Unsupported target {}", target)
    };

    let mut asm_file = File::create(path)?;
    asm_fun(&mut asm_file)?;

    return Ok(());
}
