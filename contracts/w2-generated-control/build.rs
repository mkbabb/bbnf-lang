use std::env;
use std::fs;
use std::path::PathBuf;

use bbnf::{CompileOptions, ModuleInput, StageResult, compile_module};

const GRAMMAR_PATH: &str = "grammar/control.bbnf";
const GRAMMAR: &[u8] = include_bytes!("grammar/control.bbnf");

fn compile() -> bbnf::GeneratedModule {
    match compile_module(
        ModuleInput::new(GRAMMAR_PATH, GRAMMAR),
        CompileOptions::default(),
    ) {
        StageResult::Accepted { value, diagnostics } => {
            assert!(
                diagnostics.is_empty(),
                "accepted contract grammar carried diagnostics"
            );
            value.generated
        }
        StageResult::Rejected { diagnostics } => {
            let rendered = diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n");
            panic!("contract grammar was rejected:\n{rendered}");
        }
    }
}

fn main() {
    println!("cargo:rerun-if-changed={GRAMMAR_PATH}");

    let first = compile();
    let second = compile();
    assert_eq!(
        first.source(),
        second.source(),
        "two compiler runs emitted different Rust"
    );
    assert_eq!(
        first.manifest(),
        second.manifest(),
        "two compiler runs emitted different manifests"
    );

    let output = PathBuf::from(env::var_os("OUT_DIR").expect("Cargo supplies OUT_DIR"));
    fs::write(output.join("generated.rs"), first.source()).expect("write generated Rust");
    fs::write(output.join("generated.manifest.json"), first.manifest())
        .expect("write generated manifest");
}
