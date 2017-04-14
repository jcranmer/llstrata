extern crate bindgen;
extern crate gcc;

use std::env;
use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::OsStringExt;
use std::path::{Path, PathBuf};
use std::process;

fn llvm_config<S: AsRef<OsStr>>(args: &[S]) -> Vec<u8> {
  let output = process::Command::new("llvm-config")
    .args(args)
    .output()
    .expect("llvm-config could not be run");
  if !output.status.success() {
    panic!("llvm-config failed:\n\n{}",
           String::from_utf8_lossy(&output.stderr));
  }
  output.stdout
}

fn llvm_config_path<S: AsRef<OsStr>>(args: &[S]) -> PathBuf {
  let mut vec = llvm_config(args);
  while vec.last() == Some(&b'\n') {
    vec.pop();
  }
  Path::new(&OsString::from_vec(vec)).to_path_buf()
}

fn llvm_config_str<S: AsRef<OsStr>>(args: &[S]) -> String {
  let mut vec = llvm_config(args);
  while vec.last() == Some(&b'\n') {
    vec.pop();
  }
  String::from_utf8(vec).expect("llvm-config returned invalid UTF-8")
}

fn with_components<S: AsRef<OsStr>>(args: &[S]) -> Vec<OsString> {
  let targets = llvm_config_str(&["--targets-built"]);
  let mut components = vec!["core"];
  for target in targets.split(" ") {
    components.push(target);
  }

  args.iter()
    .map(|s| s.as_ref())
    .chain(components.iter().map(|s| s.as_ref()))
    .map(|s| s.to_owned())
    .collect()
}

fn build_cpp_files() {
  let version = llvm_config_str(&["--version"]);
  if !version.starts_with("3.9.") && !version.starts_with("4.0.") && version != "5.0.0svn" {
    panic!("unsupported LLVM version: {}", version);
  }
  let libkind = llvm_config_str(&with_components(&["--shared-mode"]));
  let includedir = llvm_config_path(&["--includedir"]);
  let libdir = llvm_config_str(&["--libdir"]);
  let libs = llvm_config_str(&with_components(&["--libs"]));
  let system_libs = llvm_config_str(&with_components(&["--system-libs"]));
  let libkind = match libkind.as_ref() {
    "shared" => "dylib",
    "static" => "static",
    x => panic!("llvm-config --shared-mode returned invalid value {}", x),
  };
  println!("cargo:rustc-link-search=native={}", libdir);
  for lib in libs.split_whitespace() {
    let lib = lib.trim_left_matches("-l");
    println!("cargo:rustc-link-lib={}={}", libkind, lib);
  }
  if libkind == "static" {
    for lib in system_libs.split_whitespace() {
      let lib = lib.trim_left_matches("-l");
      println!("cargo:rustc-link-lib=dylib={}", lib);
    }
  }

  println!("cargo:rerun-if-changed={}", "cpp/TargetTriple.h");
  println!("cargo:rerun-if-changed={}", "cpp/TargetTriple.cpp");
  gcc::Config::new()
    .cpp(true)
    .include(&includedir)
    .include(env::var("CARGO_MANIFEST_DIR").unwrap())
    .flag("-std=c++14")
    .flag("-Wall")
    .flag("-fkeep-inline-functions")
    .flag("-fno-rtti")
    .file("cpp/TargetTriple.cpp")
    .compile("libllvm-glue.a");
}

fn main() {
  build_cpp_files();
  let out_dir = env::var("OUT_DIR").unwrap();
  let bindings = bindgen::builder()
    .no_unstable_rust()
    .enable_cxx_namespaces()
    .header("cpp/bindgen.h")
    .clang_arg("-x").clang_arg("c++").clang_arg("-std=c++14")
    .generate_inline_functions(true)
    .whitelisted_type("TargetTriple")
    .whitelisted_type("llvm::MCOI::.*")
    .whitelisted_type("llvm::MC(Instr|Register)Info")
    .opaque_type("std::string")
    .opaque_type("llvm::DenseMap")
    .opaque_type("llvm::DenseMapPair")
    .hide_type("llvm::DenseMap_BaseT")
    .raw_line("#[allow(dead_code)]")
    .generate().expect("Unable to generate bindings");
  bindings.write_to_file(Path::new(&out_dir).join("bindings.rs"))
    .expect("Couldn't write bindings");
}
