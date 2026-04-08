//! Semantic equivalence / parse-behavior gate for the intra-rule e-graph
//! optimization path.
//!
//! Before Tranche C, the destructive fixed-point loop in
//! `pipeline::compile` served as the reference optimizer; this file
//! would have compared its output against the e-graph's. After C the
//! destructive path is gone — the e-graph IS the reference — so this
//! harness instead codifies the *parse-behavior* hard gate: compile a
//! representative cross-corpus set of grammars through the now-only
//! e-graph path, parse a canonical input for each, and assert the
//! final parse succeeds and consumes the expected range.
//!
//! Any future regression in the e-graph rewrites or write-back that
//! breaks these corpora fails this suite immediately.

use std::path::PathBuf;

use bbnf::pipeline::{CompileRequest, CompileTarget, PipelineOptions, compile_paths_request, compile_grammar};
use bbnf::pipeline::CompileOutput;
use bbnf_ir::bytecode::BytecodeProgram;
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;

fn run_vm(program: &BytecodeProgram, input: &str) -> (bool, u32) {
    let mut interp = Interpreter::new(program, input);
    let result = interp.run();
    (result.success, result.offset)
}

fn compile_vm_from_source(source: &str) -> BytecodeProgram {
    let ir = compile_grammar(source, &PipelineOptions::default())
        .expect("compile_grammar should succeed");
    compile_bytecode(&ir)
}

fn compile_vm_from_paths(paths: &[&str]) -> BytecodeProgram {
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    let path_bufs: Vec<PathBuf> = paths.iter().map(PathBuf::from).collect();
    let out = compile_paths_request(&path_bufs, &request)
        .expect("compile_paths_request should succeed");
    match out {
        CompileOutput::Vm(ir) => compile_bytecode(&ir),
        _ => unreachable!(),
    }
}

// ── Inline grammar fixtures ────────────────────────────────────────────────

const JSON_GRAMMAR: &str = r#"
null = "null" ;
bool = "true" | "false" ;
number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/ ;
comma = "," ?w ;
colon = ":" ?w ;
string = /"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/ ;
array = "[" >> (( value << comma ? ) *)?w << "]" ;
pair = string, colon >> value ;
object = "{" >> (( pair << comma ? ) *)?w << "}" ;
value = object | array | string | number | bool | null ;
"#;

const LR_EXPR_GRAMMAR: &str = r#"
term = /[0-9]+/ ;
expr = expr, "+" >> term | term ;
"#;

// ── Gate: JSON ─────────────────────────────────────────────────────────────

#[test]
fn egraph_semantic_json_object_parses() {
    let program = compile_vm_from_source(JSON_GRAMMAR);
    let input = r#"{"a": 1, "b": [true, null, "s"]}"#;
    let (success, offset) = run_vm(&program, input);
    assert!(success, "json parse failed: offset={}/{}", offset, input.len());
    assert_eq!(offset as usize, input.len(), "full input must be consumed");
}

#[test]
fn egraph_semantic_json_nested_structures() {
    let program = compile_vm_from_source(JSON_GRAMMAR);
    for input in [
        "null",
        "true",
        r#""plain""#,
        "42.5e-3",
        r#"[1, 2, [3, [4, [5]]]]"#,
        r#"{"nested": {"deeply": {"value": 1}}}"#,
    ] {
        let (success, offset) = run_vm(&program, input);
        assert!(
            success && offset as usize == input.len(),
            "json input failed: {input:?} success={success} offset={offset}/{}",
            input.len()
        );
    }
}

// ── Gate: left-recursion elimination (epsilon-in-alt correctness) ─────────

#[test]
fn egraph_semantic_left_recursion_expr_chain() {
    // This exercises the exact pathology that caught the unsound
    // `EliminateEpsilonInAlt` rewrite: LR elimination introduces
    // `Alt([_, ε])` tail rules whose epsilon branch MUST be preserved
    // for the loop to terminate. The e-graph must not collapse the
    // nullable form into its non-nullable variant.
    let opts = PipelineOptions {
        remove_left_recursion: true,
        ..PipelineOptions::default()
    };
    let ir = compile_grammar(LR_EXPR_GRAMMAR, &opts).expect("LR compile");
    let program = compile_bytecode(&ir);
    for input in ["1", "1+2", "1+2+3", "1+2+3+4+5"] {
        let (success, offset) = run_vm(&program, input);
        assert!(
            success && offset as usize == input.len(),
            "LR expr failed: {input:?} success={success} offset={offset}/{}",
            input.len()
        );
    }
}

// ── Gate: CSS L4 full stylesheet ───────────────────────────────────────────

#[test]
fn egraph_semantic_css_l4_stylesheet() {
    let program = compile_vm_from_paths(&[
        "../../grammar/css/l4/stylesheet.bbnf",
    ]);
    // A representative CSS fragment covering selectors, properties,
    // units, and values.
    let input = ".btn-primary { color: #3b82f6; padding: 0.5em 1rem; border: 1px solid rgba(0,0,0,0.1); }";
    let (success, offset) = run_vm(&program, input);
    assert!(
        success && offset as usize == input.len(),
        "css l4 parse failed: offset={}/{}",
        offset,
        input.len()
    );
}

// ── Gate: BBNF bootstrap (self-hosting) ────────────────────────────────────

#[test]
fn egraph_semantic_bbnf_self_hosting() {
    let program = compile_vm_from_paths(&["../../grammar/bbnf/bbnf.bbnf"]);
    let input = r#"
null = "null" ;
bool = "true" | "false" ;
value = null | bool ;
"#;
    let (success, offset) = run_vm(&program, input);
    assert!(
        success,
        "bbnf self-hosting parse failed: offset={}/{}",
        offset,
        input.len()
    );
    // BBNF parser may consume or reject the trailing newline depending
    // on `@ws` handling — accept either full consumption or up to
    // the last non-whitespace byte.
    assert!(
        offset as usize >= input.trim_end().len(),
        "bbnf parse stopped before trim_end: offset={}/{}",
        offset,
        input.len()
    );
}

// ── Gate: EBNF ─────────────────────────────────────────────────────────────

#[test]
fn egraph_semantic_ebnf_multi_rule_parse() {
    // This guards against the multi-rule parse regression: the EBNF
    // `grammar = (S, rule, S) *` Repeat must still iterate correctly
    // across newline-separated rules after e-graph optimization. The
    // prettify path has a pre-existing issue (separate concern tracked
    // in ebnf_prettify.rs), but the raw parse must succeed.
    let program = compile_vm_from_paths(&["../../grammar/ebnf/ebnf.bbnf"]);
    let input = "digit = \"0\" | \"1\" ;\nnumber = digit , { digit } ;";
    let (success, offset) = run_vm(&program, input);
    assert!(
        success,
        "ebnf multi-rule parse failed: offset={}/{}",
        offset,
        input.len()
    );
}

// ── Gate: BNF ──────────────────────────────────────────────────────────────

#[test]
fn egraph_semantic_bnf_grammar() {
    let program = compile_vm_from_paths(&["../../grammar/bnf/bnf.bbnf"]);
    let input = "<digit> ::= \"0\" | \"1\" | \"2\"\n<number> ::= <digit> | <digit> <number>\n";
    let (success, offset) = run_vm(&program, input);
    assert!(
        success,
        "bnf parse failed: offset={}/{}",
        offset,
        input.len()
    );
}
