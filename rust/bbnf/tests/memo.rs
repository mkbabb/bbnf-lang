//! Memoization strategy audit for real grammars.
//!
//! Verifies that the memo pass assigns expected strategies after the full
//! pipeline. JSON grammar should have NO memoized rules — all alternations
//! are total-dispatch, so memoization would only add cache overhead.

use bbnf::pipeline::{PipelineOptions, compile_grammar};
use bbnf_ir::MemoStrategy;

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

#[test]
fn json_grammar_memo_assignments() {
    let ir = compile_grammar(JSON_GRAMMAR, &PipelineOptions::default()).unwrap();

    // Collect memo assignments for inspection.
    let memo_assignments: Vec<(&str, MemoStrategy)> = ir
        .rules
        .iter()
        .map(|r| (ir.get_string(r.name), r.meta.memo.clone()))
        .collect();

    // After the full pipeline (which includes inlining and SCC detection),
    // some rules participate in cycles (value→object→pair→value). The memo
    // pass correctly assigns Full to SCC entry points. Verify:
    // - Only cyclic rules get Full memo.
    // - No rules get Selective (no redundant caching).
    for (name, memo) in &memo_assignments {
        assert_ne!(
            *memo,
            MemoStrategy::Selective,
            "rule `{}` should not have Selective memo — JSON grammar \
             has total-dispatch alternations, so selective caching adds \
             overhead without benefit",
            name,
        );
    }

    // Verify that Full memo is only on cyclic rules.
    for rule in &ir.rules {
        let name = ir.get_string(rule.name);
        if rule.meta.memo == MemoStrategy::Full {
            assert!(
                rule.meta.is_cyclic,
                "rule `{}` has Full memo but is not cyclic",
                name,
            );
        }
    }
}
