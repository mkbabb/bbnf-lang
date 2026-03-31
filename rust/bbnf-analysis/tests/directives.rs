//! Tests for directive coverage in the LSP analysis engine:
//! @inline, @debug, @ws — semantic tokens, hover, completion, diagnostics.

use bbnf_analysis::analysis::LineIndex;
use bbnf_analysis::state::diagnostics::analyze;

// ── @inline ──────────────────────────────────────────────────────────────────

// ── @debug ───────────────────────────────────────────────────────────────────

#[test]
fn debug_directive_no_spurious_diagnostic() {
    let grammar = "@debug value ;\nvalue = /[0-9]+/ ;";
    let info = analyze(grammar, &LineIndex::new(grammar));
    let debug_warnings: Vec<_> = info
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("@debug") || d.message.contains("Undefined"))
        .collect();
    assert!(
        debug_warnings.is_empty(),
        "should not warn about valid @debug: {:?}",
        debug_warnings
    );
}

#[test]
fn debug_wildcard_no_warning() {
    let grammar = "@debug * ;\nvalue = /[0-9]+/ ;";
    let info = analyze(grammar, &LineIndex::new(grammar));
    let warnings: Vec<_> = info
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("@debug"))
        .collect();
    assert!(
        warnings.is_empty(),
        "should not warn about @debug *: {:?}",
        warnings
    );
}

#[test]
fn debug_undefined_target_warns() {
    let grammar = "@debug nonexistent ;\nentry = \"x\" ;";
    let info = analyze(grammar, &LineIndex::new(grammar));
    let warnings: Vec<_> = info
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("@debug") && d.message.contains("undefined"))
        .collect();
    assert!(
        !warnings.is_empty(),
        "should warn about @debug targeting undefined rule"
    );
}

#[test]
fn debug_directive_has_semantic_tokens() {
    let grammar = "@debug value ;\nvalue = /[0-9]+/ ;";
    let info = analyze(grammar, &LineIndex::new(grammar));
    let keyword_tokens: Vec<_> = info
        .semantic_tokens
        .iter()
        .filter(|t| t.token_type == 5) // KEYWORD
        .filter(|t| {
            let text = &grammar[t.span.0..t.span.1];
            text == "@debug"
        })
        .collect();
    assert!(
        !keyword_tokens.is_empty(),
        "should have semantic token for @debug keyword"
    );
}

// ── @ws ──────────────────────────────────────────────────────────────────────

#[test]
fn ws_directive_no_spurious_diagnostic() {
    let grammar = "@ws /\\s+/ ;\nvalue = /[0-9]+/ ;";
    let info = analyze(grammar, &LineIndex::new(grammar));
    let ws_warnings: Vec<_> = info
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("@ws"))
        .collect();
    assert!(
        ws_warnings.is_empty(),
        "should not warn about valid @ws: {:?}",
        ws_warnings
    );
}

#[test]
fn ws_directive_extracted() {
    let grammar = "@ws /(?s)(?:\\s|\\/\\*.*?\\*\\/)*/ ;\nvalue = /[0-9]+/ ;";
    let info = analyze(grammar, &LineIndex::new(grammar));
    assert!(
        info.ws_pattern.is_some(),
        "ws_pattern should be populated from @ws directive"
    );
}

#[test]
fn ws_directive_has_semantic_token() {
    let grammar = "@ws /\\s+/ ;\nvalue = /[0-9]+/ ;";
    let info = analyze(grammar, &LineIndex::new(grammar));
    let keyword_tokens: Vec<_> = info
        .semantic_tokens
        .iter()
        .filter(|t| t.token_type == 5) // KEYWORD
        .filter(|t| {
            let text = &grammar[t.span.0..t.span.1];
            text == "@ws"
        })
        .collect();
    assert!(
        !keyword_tokens.is_empty(),
        "should have semantic token for @ws keyword"
    );
}

// ── Document info population ─────────────────────────────────────────────────

#[test]
fn document_info_has_all_directive_fields() {
    let grammar = "@debug value ;\n@ws /\\s+/ ;\nhelper = \"x\" ;\nvalue = helper ;";
    let info = analyze(grammar, &LineIndex::new(grammar));
    assert!(!info.debugs.is_empty(), "debugs should be populated");
    assert!(info.ws_pattern.is_some(), "ws_pattern should be populated");
}

// ── IR-backed metadata ───────────────────────────────────────────────────────

#[test]
fn ir_meta_populated_for_json_grammar() {
    let grammar = r#"
null = "null" ;
bool = "true" | "false" ;
number = /[0-9]+/ ;
string = "\"" , /[^"]*/ , "\"" ;
array = "[" , value , ("," , value) * , "]" ;
value = string | number | bool | null | array ;
"#;
    let info = analyze(grammar, &LineIndex::new(grammar));
    assert!(
        !info.ir_meta.is_empty(),
        "ir_meta should be populated for a valid grammar"
    );
    // The entry rule (value) should have metadata.
    let value_meta = info.ir_meta.get("value");
    assert!(value_meta.is_some(), "value rule should have IR metadata");
    let vm = value_meta.unwrap();
    assert!(vm.inferred_type.is_some(), "value should have an inferred type");
}

#[test]
fn ir_meta_has_follow_sets() {
    let grammar = "a = \"x\" , b ;\nb = \"y\" ;";
    let info = analyze(grammar, &LineIndex::new(grammar));
    // After the full pipeline, at least one rule should have a FOLLOW set.
    let has_follow = info.ir_meta.values().any(|m| m.follow_set_label.is_some());
    assert!(has_follow, "at least one rule should have a FOLLOW set label");
}

#[test]
fn ir_meta_graceful_on_empty_grammar() {
    let grammar = "";
    let info = analyze(grammar, &LineIndex::new(grammar));
    assert!(info.ir_meta.is_empty(), "empty grammar → empty ir_meta");
}

#[test]
fn ir_meta_has_memo_and_span_info() {
    let grammar = "value = \"x\" | \"y\" | \"z\" ;";
    let info = analyze(grammar, &LineIndex::new(grammar));
    let meta = info.ir_meta.get("value");
    assert!(meta.is_some(), "value should have IR metadata");
    let m = meta.unwrap();
    // Memo strategy should be reported (even if "None").
    assert!(!m.memo_strategy.is_empty(), "memo_strategy should be non-empty");
    // Span eligibility should be set for a simple literal alternation.
    assert!(m.span_eligible, "literal alternation should be span-eligible");
}

// ── Import semantic tokens ───────────────────────────────────────────────────

#[test]
fn import_directive_has_semantic_tokens() {
    // Note: import target file doesn't need to exist for parsing — just testing token emission.
    let grammar = "@import { foo, bar } from \"other.bbnf\" ;\nentry = foo ;";
    let info = analyze(grammar, &LineIndex::new(grammar));
    let keyword_tokens: Vec<_> = info
        .semantic_tokens
        .iter()
        .filter(|t| t.token_type == 5) // KEYWORD
        .filter(|t| {
            let text = &grammar[t.span.0..t.span.1];
            text == "@import"
        })
        .collect();
    assert!(
        !keyword_tokens.is_empty(),
        "should have semantic token for @import keyword"
    );
    // Imported names should have RULE_REFERENCE tokens.
    let ref_tokens: Vec<_> = info
        .semantic_tokens
        .iter()
        .filter(|t| t.token_type == 1) // RULE_REFERENCE
        .filter(|t| {
            let text = &grammar[t.span.0..t.span.1];
            text == "foo" || text == "bar"
        })
        .collect();
    assert!(
        ref_tokens.len() >= 2,
        "should have RULE_REFERENCE tokens for imported names, got {}",
        ref_tokens.len()
    );
}

#[test]
fn import_item_spans_correct() {
    let grammar = "@import { number, string } from \"values.bbnf\" ;\nentry = number ;";
    let info = analyze(grammar, &LineIndex::new(grammar));
    assert!(!info.imports.is_empty());
    let imp = &info.imports[0];
    let items = imp.items.as_ref().unwrap();
    assert_eq!(items.len(), 2);
    assert_eq!(items[0].name, "number");
    assert_eq!(items[1].name, "string");
    // Verify spans point to actual text.
    assert_eq!(&grammar[items[0].span.0..items[0].span.1], "number");
    assert_eq!(&grammar[items[1].span.0..items[1].span.1], "string");
}
