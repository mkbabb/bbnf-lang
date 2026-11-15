//! sk-v27 LW3.GEN-RECURSE (RULINGS 2026-11-14k GEN-RECURSE; 2026-11-15a GENREC-Q (a), QX-1 clause 8):
//! root's generator terminates on a grammar whose rules reach themselves through rules it inlines.
//!
//! The driver classifies an Alt branch with `backend/driver/alt.rs::branch_pushes_children`, which
//! follows an inlined or transparent `Ref` into its target's body. With no visited set, a value rule
//! that reaches itself through inlined rules (`value -> call -> args -> value`) recursed until the
//! stack overflowed, at generation time, aborting the process. `grammar/css/l4/keyframes.bbnf`, a
//! MANIFEST grammar, hit it. The first test is the minimal reproduction; the second is the MANIFEST
//! grammar. RED was the process aborting ("has overflowed its stack"); GREEN is generation finishing.
use bbnf::ParserAttributes;
use bbnf::generate::generate_all;
use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
    compile_paths_request,
};
use std::path::PathBuf;

const CYCLE: &str = r#"
number = /[0-9]+/ ;
name = /[a-z]+/ ;
args = value , ("," >> value) * ;
call = /[a-z]+\(/ , args , ")" ;
wrap = "w" , "(" , (number | name) , ")" ;
value = call | wrap | number | name ;
cell = /=?/ , value ;
"#;

fn request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Rust {
            requested_prettify: false,
        },
    }
}

fn generate(output: CompileOutput, parser: &str) -> String {
    let prepared = match output {
        CompileOutput::Rust(prepared) => *prepared,
        _ => panic!("expected Rust output"),
    };
    let attrs = ParserAttributes {
        builder_path: Some("crate::runtime::bbnf::BbnfStructBuilder".to_string()),
        document_path: Some("crate::runtime::bbnf::BbnfDocument".to_string()),
        ..ParserAttributes::default()
    };
    generate_all(&prepared, &attrs, &quote::format_ident!("{}", parser)).to_string()
}

#[test]
fn a_value_cycle_through_inlined_rules_generates() {
    let output = compile_grammar_request(CYCLE, &request()).expect("grammar compiles");
    let source = generate(output, "GenRecurseParser");
    assert!(source.contains("GenRecurseParser"));
}

#[test]
fn css_l4_keyframes_compiles_and_generates_without_overflow() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/css/l4/keyframes.bbnf");
    let output = compile_paths_request(&[path], &request()).expect("keyframes.bbnf compiles");
    let source = generate(output, "KeyframesParser");
    assert!(source.contains("KeyframesParser"));
}
