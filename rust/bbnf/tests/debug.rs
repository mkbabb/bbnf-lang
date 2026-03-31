//! Tests for @debug directive parsing, pipeline threading, and compiled trace codegen.

use bbnf::grammar::BBNFGrammar;
use bbnf::pipeline::{compile_grammar, PipelineOptions};

// ── @debug directive parsing ─────────────────────────────────────────────────

#[test]
fn parse_debug_single_rule() {
    let source = "@debug value ;\nvalue = /[0-9]+/ ;";
    let parser = BBNFGrammar::grammar_with_imports();
    let result = parser.parse(source).unwrap();
    assert_eq!(result.debug_rules.len(), 1);
    assert_eq!(result.debug_rules[0].as_ref(), "value");
}

#[test]
fn parse_debug_wildcard() {
    let source = "@debug * ;\nvalue = /[0-9]+/ ;";
    let parser = BBNFGrammar::grammar_with_imports();
    let result = parser.parse(source).unwrap();
    assert_eq!(result.debug_rules.len(), 1);
    assert_eq!(result.debug_rules[0].as_ref(), "*");
}

#[test]
fn parse_debug_multiple_rules() {
    let source = "@debug value ;\n@debug pair ;\nvalue = /[0-9]+/ ;\npair = value , value ;";
    let parser = BBNFGrammar::grammar_with_imports();
    let result = parser.parse(source).unwrap();
    assert_eq!(result.debug_rules.len(), 2);
    assert_eq!(result.debug_rules[0].as_ref(), "value");
    assert_eq!(result.debug_rules[1].as_ref(), "pair");
}

#[test]
fn parse_debug_with_other_directives() {
    let source = "@debug value ;\nhelper = \"x\" ;\nvalue = helper ;";
    let parser = BBNFGrammar::grammar_with_imports();
    let result = parser.parse(source).unwrap();
    assert_eq!(result.debug_rules.len(), 1);
}

// ── Pipeline: @debug → RuleMeta.debug ────────────────────────────────────────

#[test]
fn pipeline_debug_single_rule_meta() {
    let source = "@debug value ;\nvalue = /[0-9]+/ ;\nentry = value ;";
    let ir = compile_grammar(source, &PipelineOptions::default()).unwrap();
    let value_rule = ir.find_rule("value");
    assert!(value_rule.is_some(), "value rule should exist");
    // After inlining/pruning, the rule may be optimized away. Check if it survives.
    if let Some(rule) = value_rule {
        assert!(rule.meta.debug, "value should have debug=true");
    }
}

#[test]
fn pipeline_debug_wildcard_sets_all() {
    let source = "@debug * ;\nnull = \"null\" ;\nbool = \"true\" | \"false\" ;\nvalue = null | bool ;";
    let ir = compile_grammar(source, &PipelineOptions::default()).unwrap();
    assert!(ir.debug_all, "debug_all should be true");
}

#[test]
fn pipeline_no_debug_default() {
    let source = "value = /[0-9]+/ ;";
    let ir = compile_grammar(source, &PipelineOptions::default()).unwrap();
    assert!(!ir.debug_all);
    for rule in &ir.rules {
        assert!(!rule.meta.debug, "no rules should have debug without @debug");
    }
}

// ── Source map: IrRule.source_span populated ─────────────────────────────────

#[test]
fn pipeline_source_span_populated() {
    let source = "value = /[0-9]+/ ;\nentry = value ;";
    let ir = compile_grammar(source, &PipelineOptions::default()).unwrap();
    // At least one surviving rule should have a source_span.
    let has_span = ir.rules.iter().any(|r| r.source_span.is_some());
    assert!(has_span, "at least one rule should have source_span");
}

#[test]
fn pipeline_source_span_byte_offsets_valid() {
    let source = "value = /[0-9]+/ ;";
    let ir = compile_grammar(source, &PipelineOptions::default()).unwrap();
    for rule in &ir.rules {
        if let Some(ref span) = rule.source_span {
            assert!(span.start < span.end, "span should be non-empty");
            assert!(
                (span.end as usize) <= source.len(),
                "span end should not exceed source length"
            );
        }
    }
}
