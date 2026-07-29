use std::env;
use std::fs;
use std::path::PathBuf;

use bbnf::{CompileOptions, CompiledModule, ModuleInput, StageResult, compile_module};

const GRAMMAR_PATH: &str = "grammar/recursive.bbnf";
const GRAMMAR: &[u8] = include_bytes!("grammar/recursive.bbnf");

fn compile() -> CompiledModule {
    match compile_module(
        ModuleInput::new(GRAMMAR_PATH, GRAMMAR),
        CompileOptions::default(),
    ) {
        StageResult::Accepted { value, diagnostics } => {
            assert!(
                diagnostics.is_empty(),
                "accepted recursive contract grammar carried diagnostics"
            );
            value
        }
        StageResult::Rejected { diagnostics } => {
            let rendered = diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n");
            panic!("recursive contract grammar was rejected:\n{rendered}");
        }
    }
}

fn main() {
    println!("cargo:rerun-if-changed={GRAMMAR_PATH}");

    let first = compile();
    let second = compile();
    assert_eq!(
        first.generated().source(),
        second.generated().source(),
        "two compiler runs emitted different recursive Rust"
    );
    assert_eq!(
        first.generated().manifest(),
        second.generated().manifest(),
        "two compiler runs emitted different recursive manifests"
    );

    let output = PathBuf::from(env::var_os("OUT_DIR").expect("Cargo supplies OUT_DIR"));
    fs::write(output.join("generated.rs"), first.generated().source())
        .expect("write generated Rust");
    fs::write(
        output.join("generated.manifest.json"),
        first.generated().manifest(),
    )
        .expect("write generated manifest");
}
