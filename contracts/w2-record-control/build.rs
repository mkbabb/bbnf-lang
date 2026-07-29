use std::env;
use std::fs;
use std::path::PathBuf;

use bbnf::{CompileOptions, CompiledModule, ModuleInput, StageResult, compile_module};

const GRAMMAR_PATH: &str = "grammar/record.bbnf";
const GRAMMAR: &[u8] = include_bytes!("grammar/record.bbnf");

fn compile() -> CompiledModule {
    match compile_module(
        ModuleInput::new(GRAMMAR_PATH, GRAMMAR),
        CompileOptions::default(),
    ) {
        StageResult::Accepted { value, diagnostics } => {
            assert!(diagnostics.is_empty());
            value
        }
        StageResult::Rejected { diagnostics } => panic!(
            "record contract grammar was rejected:\n{}",
            diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n")
        ),
    }
}

fn main() {
    println!("cargo:rerun-if-changed={GRAMMAR_PATH}");
    let first = compile();
    let second = compile();
    assert_eq!(first.replay_json().unwrap(), second.replay_json().unwrap());
    assert_eq!(first.generated().source(), second.generated().source());
    assert_eq!(first.generated().manifest(), second.generated().manifest());

    let output = PathBuf::from(env::var_os("OUT_DIR").expect("Cargo supplies OUT_DIR"));
    fs::write(output.join("generated.rs"), first.generated().source())
        .expect("write generated Rust");
    fs::write(
        output.join("generated.manifest.json"),
        first.generated().manifest(),
    )
    .expect("write generated manifest");
}
