use std::env;
use std::fs;
use std::path::PathBuf;

use bbnf::{CompileOptions, ModuleInput, StageResult, compile_module};

fn main() {
    let mut arguments = env::args_os().skip(1);
    let input_path = PathBuf::from(arguments.next().expect("usage: bbnf-gen INPUT OUTPUT"));
    let output_path = PathBuf::from(arguments.next().expect("usage: bbnf-gen INPUT OUTPUT"));
    assert!(arguments.next().is_none(), "usage: bbnf-gen INPUT OUTPUT");

    let bytes = fs::read(&input_path).expect("read BBNF module");
    let logical_path = input_path.to_string_lossy();
    let compilation = match compile_module(
        ModuleInput::new(&logical_path, &bytes),
        CompileOptions::default(),
    ) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            for diagnostic in diagnostics.iter() {
                eprintln!("{diagnostic}");
            }
            std::process::exit(1);
        }
    };
    fs::write(output_path, compilation.generated.source()).expect("write generated Rust");
}
