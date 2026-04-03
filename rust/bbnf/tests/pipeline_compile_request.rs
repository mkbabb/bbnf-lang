use std::borrow::Cow;

use bbnf::backend::PreparedGrammar;
use bbnf::generate::generate_all;
use bbnf::lower::DirectiveSet;
use bbnf::pipeline::{
    CompileError, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
    compile_ast_request, compile_grammar_request, compile_paths_request,
};
use bbnf::{Expression, ParserAttributes, Token};
use indexmap::IndexMap;
use tempfile::tempdir;

fn aot_request(requested_prettify: bool) -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Rust { requested_prettify },
    }
}

fn render_tokens(prepared: &PreparedGrammar, attrs: &ParserAttributes, ident: &str) -> String {
    let ident = quote::format_ident!("{ident}");
    generate_all(prepared, attrs, &ident).to_string()
}

#[test]
fn source_aot_enables_prettify_for_pretty_directive() {
    let grammar = r#"
        value = "x" ;
        @pretty value group ;
    "#;

    let prepared = match compile_grammar_request(grammar, &aot_request(false)).unwrap() {
        CompileOutput::Rust(prepared) => prepared,
        _ => panic!("expected Rust output"),
    };

    assert!(prepared.prep.effective_prettify);
    let tokens = render_tokens(&prepared, &ParserAttributes::default(), "PrettyOnlyParser");
    assert!(
        tokens.contains("value_prettify"),
        "missing prettify method: {tokens}"
    );
}

#[test]
fn source_aot_respects_explicit_prettify_without_directives() {
    let grammar = r#"
        value = "x" ;
    "#;

    let prepared = match compile_grammar_request(grammar, &aot_request(true)).unwrap() {
        CompileOutput::Rust(prepared) => prepared,
        _ => panic!("expected Rust output"),
    };

    assert!(prepared.prep.effective_prettify);
    let tokens = render_tokens(
        &prepared,
        &ParserAttributes::default(),
        "ExplicitPrettyParser",
    );
    assert!(
        tokens.contains("value_prettify"),
        "missing prettify method: {tokens}"
    );
}

#[test]
fn source_aot_skips_prettify_without_flag_or_directive() {
    let grammar = r#"
        value = "x" ;
    "#;

    let prepared = match compile_grammar_request(grammar, &aot_request(false)).unwrap() {
        CompileOutput::Rust(prepared) => prepared,
        _ => panic!("expected Rust output"),
    };

    assert!(!prepared.prep.effective_prettify);
    let tokens = render_tokens(&prepared, &ParserAttributes::default(), "PlainParser");
    assert!(
        !tokens.contains("value_prettify"),
        "unexpected prettify method: {tokens}"
    );
}

#[test]
fn compile_request_rejects_unknown_pretty_hint() {
    let grammar = r#"
        value = "x" ;
        @pretty value mystery ;
    "#;

    let err = compile_grammar_request(grammar, &aot_request(false)).unwrap_err();
    assert!(matches!(
        err,
        CompileError::UnknownPrettyHint { ref rule, ref hint }
            if rule == "value" && hint == "mystery"
    ));
}

#[test]
fn compile_request_preserves_split_pretty_hint_for_codegen_error() {
    let grammar = r#"
        value = "x" ;
        @pretty value split(",") ;
    "#;

    let prepared = match compile_grammar_request(grammar, &aot_request(false)).unwrap() {
        CompileOutput::Rust(prepared) => prepared,
        _ => panic!("expected Rust output"),
    };

    assert!(prepared.prep.effective_prettify);
    let tokens = render_tokens(&prepared, &ParserAttributes::default(), "SplitPrettyParser");
    assert!(
        tokens.contains("split(\\\",\\\")"),
        "missing explicit split compile error: {tokens}"
    );
}

#[test]
fn compile_request_rejects_unknown_nonterminal() {
    let grammar = r#"
        value = missing ;
    "#;

    let err = compile_grammar_request(grammar, &aot_request(false)).unwrap_err();
    assert!(matches!(
        err,
        CompileError::UnknownNonterminal { ref rule, ref name }
            if rule == "value" && name == "missing"
    ));
}

#[test]
fn compile_request_rejects_standalone_mapping_fn_bodies() {
    let lhs = Expression::Nonterminal(Token::new_without_span(Cow::Borrowed("value")));
    let rhs = Expression::MappingFn(Token::new_without_span(Cow::Borrowed("f64")));
    let mut ast = IndexMap::new();
    ast.insert(lhs, rhs);

    let directives = DirectiveSet::empty();
    let err = compile_ast_request(ast, &directives, &aot_request(false)).unwrap_err();
    assert!(matches!(
        err,
        CompileError::InvalidMappingFn { ref rule } if rule == "value"
    ));
}

#[test]
fn compile_request_rejects_nested_production_rules() {
    let lhs = Expression::Nonterminal(Token::new_without_span(Cow::Borrowed("value")));
    let nested_lhs = Expression::Nonterminal(Token::new_without_span(Cow::Borrowed("other")));
    let nested_rhs = Expression::Literal(Token::new_without_span(Cow::Borrowed("x")));
    let rhs = Expression::ProductionRule(Box::new(nested_lhs), Box::new(nested_rhs));
    let mut ast = IndexMap::new();
    ast.insert(lhs, rhs);

    let directives = DirectiveSet::empty();
    let err = compile_ast_request(ast, &directives, &aot_request(false)).unwrap_err();
    assert!(matches!(
        err,
        CompileError::InvalidProductionRule { ref rule } if rule == "value"
    ));
}

#[test]
fn compile_paths_preserves_pretty_directives_across_multiple_explicit_paths() {
    let dir = tempdir().unwrap();
    let first = dir.path().join("first.bbnf");
    let second = dir.path().join("second.bbnf");

    std::fs::write(
        &first,
        r#"
        foo = "a" ;
        @pretty foo group ;
    "#,
    )
    .unwrap();
    std::fs::write(
        &second,
        r#"
        bar = foo ;
    "#,
    )
    .unwrap();

    let paths = vec![first.clone(), second.clone()];
    let prepared = match compile_paths_request(&paths, &aot_request(false)).unwrap() {
        CompileOutput::Rust(prepared) => prepared,
        _ => panic!("expected Rust output"),
    };

    assert!(prepared.prep.effective_prettify);
    let attrs = ParserAttributes {
        paths,
        ..ParserAttributes::default()
    };
    let tokens = render_tokens(&prepared, &attrs, "MultiPathParser");
    assert!(
        tokens.contains("foo_prettify"),
        "missing prettify method: {tokens}"
    );
}

#[test]
fn compile_paths_preserves_pretty_directives_through_import_resolution() {
    let dir = tempdir().unwrap();
    let child = dir.path().join("child.bbnf");
    let entry = dir.path().join("entry.bbnf");

    std::fs::write(
        &child,
        r#"
        child = "x" ;
        @pretty child group ;
    "#,
    )
    .unwrap();
    std::fs::write(
        &entry,
        r#"
        @import "child.bbnf" ;
        root = child ;
    "#,
    )
    .unwrap();

    let paths = vec![entry.clone()];
    let prepared = match compile_paths_request(&paths, &aot_request(false)).unwrap() {
        CompileOutput::Rust(prepared) => prepared,
        _ => panic!("expected Rust output"),
    };

    assert!(prepared.prep.effective_prettify);
    let attrs = ParserAttributes {
        paths,
        ..ParserAttributes::default()
    };
    let tokens = render_tokens(&prepared, &attrs, "ImportPrettyParser");
    assert!(
        tokens.contains("child_prettify"),
        "missing prettify method: {tokens}"
    );
}

// ── TypeScript backend tests ────────────────────────────────────────────────

fn ts_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Ts,
    }
}

#[test]
fn ts_backend_produces_valid_structure() {
    let grammar = r#"
        value = "true" | "false" | "null" ;
    "#;

    let output = match compile_grammar_request(grammar, &ts_request()).unwrap() {
        CompileOutput::Ts(source) => source,
        _ => panic!("expected TS output"),
    };

    // Structural checks: has runtime types, parser functions, public API.
    assert!(output.contains("interface ParserState"), "missing ParserState: {output}");
    assert!(output.contains("interface Span"), "missing Span: {output}");
    assert!(output.contains("function __value"), "missing __value function: {output}");
    assert!(output.contains("export function parse"), "missing public parse: {output}");
    assert!(output.contains("type valueValue"), "missing discriminated union: {output}");
}

#[test]
fn ts_backend_emits_discriminated_union() {
    let grammar = r#"
        item = "x" | "y" ;
        list = item, { item } ;
    "#;

    let output = match compile_grammar_request(grammar, &ts_request()).unwrap() {
        CompileOutput::Ts(source) => source,
        _ => panic!("expected TS output"),
    };

    // Non-transparent rules should appear as union variants.
    assert!(output.contains("tag: \"item\""), "missing item variant: {output}");
    assert!(output.contains("tag: \"list\""), "missing list variant: {output}");
}

// ── WASM backend tests ──────────────────────────────────────────────────

fn wasm_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Wasm,
    }
}

#[test]
fn wasm_backend_produces_valid_wat() {
    let grammar = r#"
        value = "true" | "false" | "null" ;
    "#;

    let output = match compile_grammar_request(grammar, &wasm_request()).unwrap() {
        CompileOutput::Wasm(bytes) => String::from_utf8(bytes).unwrap(),
        _ => panic!("expected WASM output"),
    };

    // Structural checks: valid WAT module with functions and exports.
    assert!(output.contains("(module $"), "missing module declaration: {output}");
    assert!(output.contains("(func $__value"), "missing __value function: {output}");
    assert!(output.contains("(export \"parse\""), "missing parse export: {output}");
    assert!(output.contains("(memory"), "missing memory declaration: {output}");
}

#[test]
fn wasm_backend_emits_literal_matching() {
    let grammar = r#"
        item = "hello" ;
    "#;

    let output = match compile_grammar_request(grammar, &wasm_request()).unwrap() {
        CompileOutput::Wasm(bytes) => String::from_utf8(bytes).unwrap(),
        _ => panic!("expected WASM output"),
    };

    // Should contain byte-level comparisons for "hello" (h=104, e=101, l=108, l=108, o=111).
    assert!(output.contains("i32.const 104"), "missing 'h' byte check: {output}");
    assert!(output.contains("i32.load8_u"), "missing byte load: {output}");
}

#[test]
fn ts_backend_handles_alternation_dispatch() {
    let grammar = r#"
        digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
    "#;

    let output = match compile_grammar_request(grammar, &ts_request()).unwrap() {
        CompileOutput::Ts(source) => source,
        _ => panic!("expected TS output"),
    };

    // Should contain either a switch dispatch or checkpoint chain.
    let has_switch = output.contains("switch (s.input.charCodeAt");
    let has_checkpoint = output.contains("const __cp");
    assert!(
        has_switch || has_checkpoint,
        "missing dispatch or checkpoint in alternation: {output}"
    );
}
