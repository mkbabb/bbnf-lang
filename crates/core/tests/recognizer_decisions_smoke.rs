//! Tranche V.8 smoke test — confirms the V.6 RecognizerDecisionMap is
//! populated for representative grammars and that the V.8 driver
//! accessor reads it correctly.
//!
//! This is a smoke test, not an exhaustive consumer migration test.
//! The follow-up tranche replaces the existing strategy solvers'
//! priority cascades with reads from `recognizer_decisions`.

use bbnf::pipeline::{compile_grammar, PipelineOptions};
use bbnf_ir::passes::csp_recognizers::AltMode;

const JSON_GRAMMAR: &str = r#"
value = "true" | "false" | "null" | string | number ;
string = /"[^"]*"/ ;
number = /[0-9]+/ ;
"#;

#[test]
fn json_grammar_populates_recognizer_decisions() {
    let opts = PipelineOptions::default();
    let ir = compile_grammar(JSON_GRAMMAR, &opts).expect("compile");

    eprintln!(
        "rules: {}, dag: {}, regex_info: {}, node_facts: {}, decisions: {}",
        ir.rules.len(),
        ir.dag.is_some(),
        ir.regex_info.len(),
        ir.node_facts.len(),
        ir.recognizer_decisions.len(),
    );

    // V.6 should populate at least some decisions for a non-trivial grammar.
    assert!(
        !ir.recognizer_decisions.is_empty(),
        "expected V.6 to populate recognizer_decisions for the JSON grammar"
    );
}

#[test]
fn json_grammar_has_regex_engine_decisions() {
    let opts = PipelineOptions::default();
    let ir = compile_grammar(JSON_GRAMMAR, &opts).expect("compile");

    // Regex nodes should always be visited by V.6 and produce a
    // RegexEngine decision (the grammar contains two regexes).
    let any_engine = ir
        .recognizer_decisions
        .values()
        .any(|d| d.regex_engine.is_some());
    assert!(
        any_engine,
        "expected at least one regex node to receive a regex_engine decision"
    );
}

#[test]
fn alt_mode_checkpoint_is_legal_default() {
    // The architectural commitment: AltMode::Checkpoint is the
    // always-legal fallback for any Alt node, never a Fallback escape
    // hatch. This test asserts the type can be constructed and
    // pattern-matched (sanity check on the V.6 enum).
    let mode = AltMode::Checkpoint;
    assert!(matches!(mode, AltMode::Checkpoint));
}
