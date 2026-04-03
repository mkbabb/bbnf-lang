use std::borrow::Cow;

use bbnf::backend::PreparedAotGrammar;
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
        target: CompileTarget::Aot { requested_prettify },
    }
}

fn render_tokens(prepared: &PreparedAotGrammar, attrs: &ParserAttributes, ident: &str) -> String {
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
        CompileOutput::Aot(prepared) => prepared,
        CompileOutput::Vm(_) => panic!("expected AOT output"),
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
        CompileOutput::Aot(prepared) => prepared,
        CompileOutput::Vm(_) => panic!("expected AOT output"),
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
        CompileOutput::Aot(prepared) => prepared,
        CompileOutput::Vm(_) => panic!("expected AOT output"),
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
        CompileOutput::Aot(prepared) => prepared,
        CompileOutput::Vm(_) => panic!("expected AOT output"),
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
        CompileOutput::Aot(prepared) => prepared,
        CompileOutput::Vm(_) => panic!("expected AOT output"),
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
        CompileOutput::Aot(prepared) => prepared,
        CompileOutput::Vm(_) => panic!("expected AOT output"),
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
