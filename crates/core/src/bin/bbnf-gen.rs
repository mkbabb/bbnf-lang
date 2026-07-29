use std::env;
use std::fs;
use std::path::PathBuf;

use bbnf::{CompileOptions, GrammarInstanceInput, StageResult, compile_instance};

fn main() {
    let mut arguments = env::args_os().skip(1);
    let manifest_path = PathBuf::from(
        arguments
            .next()
            .expect("usage: bbnf-gen MANIFEST SOURCE OUTPUT"),
    );
    let source_path = PathBuf::from(
        arguments
            .next()
            .expect("usage: bbnf-gen MANIFEST SOURCE OUTPUT"),
    );
    let output_path = PathBuf::from(
        arguments
            .next()
            .expect("usage: bbnf-gen MANIFEST SOURCE OUTPUT"),
    );
    assert!(
        arguments.next().is_none(),
        "usage: bbnf-gen MANIFEST SOURCE OUTPUT"
    );

    let manifest_bytes = fs::read(&manifest_path).expect("read grammar-instance manifest");
    let source_bytes = fs::read(&source_path).expect("read BBNF source");
    let compilation = match compile_instance(
        GrammarInstanceInput::new(&manifest_bytes, &source_bytes),
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
    fs::write(output_path, compilation.generated().source()).expect("write generated Rust");
}
