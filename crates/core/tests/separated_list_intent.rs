//! LW2 ruling S1-B grammar-intent falsifiers.
//!
//! `( x << sep ? ) *` reads literally, so every in-repo grammar that
//! used it for a strict separated list is rewritten as `x (sep x)*`
//! (audit: `separated_list_audit.rs`). Each rewritten grammar gets an
//! input on which the two readings disagree, read on every backend
//! that compiles the grammar: the bytecode VM (which has always read
//! the IR literally) and, for a workspace grammar, its generated Rust
//! parser. The strict reading is the grammar's intent, so the input is
//! rejected; the positive rows guard the rewrite from over-rejecting.

use std::path::{Path, PathBuf};

use bbnf::grammar::generated::google_sheets::GoogleSheetsParser;
use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

/// Full-input acceptance on the VM, from the grammar's entry rule.
fn vm_accepts(grammar: &str, input: &str) -> bool {
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    let ir = match compile_paths_request(&[root().join(grammar)], &request)
        .unwrap_or_else(|e| panic!("{grammar}: {e:?}"))
    {
        CompileOutput::Vm(ir) => *ir,
        other => panic!("{grammar}: expected VM output, got {other:?}"),
    };
    let program = compile_bytecode(&ir);
    let r = Interpreter::new(&program, input).run();
    r.success && r.offset as usize == input.len()
}

/// `(grammar, input, accepted under the grammar's strict intent)`.
fn check(cases: &[(&str, &str, bool)], rust: Option<fn(&str) -> bool>) {
    let mut wrong = Vec::new();
    for &(grammar, input, want) in cases {
        let vm = vm_accepts(grammar, input);
        if vm != want {
            wrong.push(format!(
                "vm {grammar} {input:?}: accepted={vm}, intent says {want}"
            ));
        }
        if let Some(rust) = rust {
            let got = rust(input);
            if got != want {
                wrong.push(format!(
                    "rust {grammar} {input:?}: accepted={got}, intent says {want}"
                ));
            }
        }
    }
    assert!(
        wrong.is_empty(),
        "separated-list intent divergences:\n{}",
        wrong.join("\n")
    );
}

const SHEETS: &str = "grammar/google-sheets/google-sheets.bbnf";
const SHEETS_GORGEOUS: &str = "crates/gorgeous/grammar/google-sheets/google-sheets.bbnf";

/// Sheets function arguments and lambda parameters are comma-separated
/// (`func_args`, `lambda_params`); an argument may be empty.
fn sheets_cases(grammar: &'static str) -> Vec<(&'static str, &'static str, bool)> {
    vec![
        (grammar, "=SUM(1 2)", false),
        (grammar, "=LAMBDA(x y, x)", false),
        (grammar, "=SUM(1,2)", true),
        (grammar, "=SUM(1, 2, 3)", true),
        (grammar, "=IF(A1,,1)", true),
        (grammar, "=LAMBDA(x, y, x)", true),
    ]
}

#[test]
fn google_sheets_args_are_comma_separated() {
    check(
        &sheets_cases(SHEETS),
        Some(|i| GoogleSheetsParser::parse(i).is_ok()),
    );
}

#[test]
fn gorgeous_google_sheets_args_are_comma_separated() {
    check(&sheets_cases(SHEETS_GORGEOUS), None);
}

#[test]
fn gorgeous_json_lists_require_commas() {
    const G: &str = "crates/gorgeous/grammar/json/json.bbnf";
    check(
        &[
            (G, "[1 2]", false),
            (G, "[1,]", false),
            (G, "{\"a\":1 \"b\":2}", false),
            (G, "{\"a\":1,}", false),
            (G, "[1,2]", true),
            (G, "[]", true),
            (G, "{\"a\":[1, {}]}", true),
        ],
        None,
    );
}

#[test]
fn keyframe_declarations_are_semicolon_separated() {
    const G: &str = "grammar/css/l4/keyframes.bbnf";
    check(
        &[
            (G, "@keyframes x { from { opacity: 0 color: red } }", false),
            (G, "@keyframes x { from { opacity: 0; color: red } }", true),
            (G, "@keyframes x { from { opacity: 0; color: red; } }", true),
            (G, "@keyframes x { 50% { opacity: 1 } }", true),
        ],
        None,
    );
}
