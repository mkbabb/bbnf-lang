//! sk-v27 GEN-RECURSE (RULINGS 2026-11-14k): a minimal RED reproduction, kept in evidence, never in the
//! merged suite (owner LW4.COMP-1 unless a MANIFEST grammar hits it). Copied into `crates/core/tests/` by
//! `repro.sh`, run, and removed. Root's generator classifies an Alt branch with
//! `backend/driver/alt.rs::branch_pushes_children`, which follows an inlined or transparent `Ref` into its
//! target's body with no visited set; a value rule that reaches itself through inlined rules (here
//! `value -> call -> args -> value`) recurses until the stack overflows, at generation time, before any
//! emitted code exists. RED = the process aborts ("has overflowed its stack"); GREEN would be the
//! grammar generating (or refusing with a named diagnostic).
use bbnf::ParserAttributes;
use bbnf::generate::generate_all;
use bbnf::pipeline::{CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request};

const CYCLE: &str = r#"
number = /[0-9]+/ ;
name = /[a-z]+/ ;
args = value , ("," >> value) * ;
call = /[a-z]+\(/ , args , ")" ;
wrap = "w" , "(" , (number | name) , ")" ;
value = call | wrap | number | name ;
cell = /=?/ , value ;
"#;

#[test]
fn a_value_cycle_through_inlined_rules_generates() {
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Rust { requested_prettify: false },
    };
    let prepared = match compile_grammar_request(CYCLE, &request).expect("grammar compiles") {
        CompileOutput::Rust(prepared) => *prepared,
        _ => panic!("expected Rust output"),
    };
    let attrs = ParserAttributes {
        builder_path: Some("crate::runtime::bbnf::BbnfStructBuilder".to_string()),
        document_path: Some("crate::runtime::bbnf::BbnfDocument".to_string()),
        ..ParserAttributes::default()
    };
    let out = generate_all(&prepared, &attrs, &quote::format_ident!("GenRecurseParser")).to_string();
    assert!(out.contains("GenRecurseParser"));
}
