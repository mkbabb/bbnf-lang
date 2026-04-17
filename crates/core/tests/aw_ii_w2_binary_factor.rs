//! Tranche AW-II.W2 reproducer: exercises the `binary_factor` lowering path
//! under DTA tape shapes against the four `binary_factor could not resolve
//! operator — empty gap` panics surfaced by gorgeous subgrammar dev-deps
//! (`json.rs`, `ebnf.rs`, `bnf.rs`, `google_sheets.rs`).
//!
//! Drives synthetic grammars through `bbnf::grammar::parse` →
//! `bbnf::lower::lower_to_ir` and asserts the resulting IR contains
//! `Skip` / `Next` / `Minus` nodes with the correct structural shape.
//! No panic on any operator chain is the hard gate.
//!
//! The panics cover four grammar shapes:
//!  * `"[" >> ( value << comma ? ) * ?w << "]"` (gorgeous/json.rs)
//!  * `character - "'"` (gorgeous/ebnf.rs)
//!  * `"<" >> identifier << ">"` (gorgeous/bnf.rs)
//!  * `arg << comma ?` (gorgeous/google_sheets.rs)

use bbnf::graph::{calculate_ast_deps, tarjan_scc};
use bbnf::lower::{DirectiveSet, lower_to_ir};

use bbnf_ir::{GrammarIR, IrNode};

fn lower_grammar(source: &str) -> GrammarIR {
    let ast = bbnf::grammar::parse(source)
        .expect("failed to parse grammar")
        .rules;

    let deps = calculate_ast_deps(&ast);
    let scc_result = tarjan_scc(&deps);

    let directives = DirectiveSet::empty();

    let mut ir = lower_to_ir(&ast, &scc_result, &directives, &[]);

    bbnf_ir::passes::compute_first_sets(&mut ir);
    bbnf_ir::passes::compute_scc(&mut ir);
    bbnf_ir::passes::compute_aliases(&mut ir);
    bbnf_ir::passes::compute_transparent(&mut ir);
    bbnf_ir::passes::refine_span_eligibility(&mut ir);

    ir
}

fn body_of<'a>(ir: &'a GrammarIR, name: &str) -> &'a IrNode {
    let rule = ir
        .find_rule(name)
        .unwrap_or_else(|| panic!("rule {name:?} not found in IR"));
    &rule.body
}

/// The simple, known-working baseline: direct `a = x << y` path. Anchors
/// the invariant that the fix does not regress previously-working shapes.
#[test]
fn lower_binary_skip_direct() {
    let ir = lower_grammar(r#"a = "x" << "y" ;"#);
    assert!(matches!(body_of(&ir, "a"), IrNode::Skip(_, _)));
}

#[test]
fn lower_binary_next_direct() {
    let ir = lower_grammar(r#"b = "p" >> "q" ;"#);
    assert!(matches!(body_of(&ir, "b"), IrNode::Next(_, _)));
}

#[test]
fn lower_binary_minus_direct() {
    let ir = lower_grammar(r#"c = "m" - "n" ;"#);
    assert!(matches!(body_of(&ir, "c"), IrNode::Minus(_, _)));
}

/// Three-operand left-associative chain — validates chain iteration.
#[test]
fn lower_binary_three_operand_chain() {
    let ir = lower_grammar(r#"d = "u" << "v" >> "w" ;"#);
    // `u << v >> w` parses as `(u << v) >> w` (left associative).
    let body = body_of(&ir, "d");
    match body {
        IrNode::Next(lhs, _rhs) => match lhs.as_ref() {
            IrNode::Skip(_, _) => {}
            other => panic!("expected Skip on lhs of Next chain, got {:?}", other),
        },
        other => panic!("expected Next(Skip(...), ...), got {:?}", other),
    }
}

/// Reproducer for gorgeous/ebnf.rs — `character - "'"` shape. `-` operator
/// between an identifier and a string literal; this was the shortest of
/// the four known-panicking grammars.
#[test]
fn lower_binary_minus_ident_literal() {
    let ir = lower_grammar(
        r#"
        character = /[a-z]/ ;
        char_non_quote = character - "'" ;
        "#,
    );
    assert!(matches!(body_of(&ir, "char_non_quote"), IrNode::Minus(_, _)));
}

/// Reproducer for gorgeous/google_sheets.rs — `arg << comma ?` shape.
/// The `?` suffix on `comma` puts the RHS inside an Optional wrapper,
/// which historically shifted tape shapes.
#[test]
fn lower_binary_skip_with_optional_rhs() {
    let ir = lower_grammar(
        r#"
        arg = /[a-z]+/ ;
        comma = "," ;
        call_inner = arg << comma ? ;
        "#,
    );
    assert!(matches!(body_of(&ir, "call_inner"), IrNode::Skip(_, _)));
}

/// Reproducer for gorgeous/bnf.rs — `"<" >> identifier << ">"` shape.
/// Mixed `>>` then `<<` with literal-identifier-literal operands.
#[test]
fn lower_binary_bracket_delimited_ident() {
    let ir = lower_grammar(
        r#"
        identifier = /[a-z]+/ ;
        bracketed = "<" >> identifier << ">" ;
        "#,
    );
    // `"<" >> identifier << ">"` parses left-associatively as
    // `("<" >> identifier) << ">"` -> `Skip(Next(...), ...)`.
    let body = body_of(&ir, "bracketed");
    match body {
        IrNode::Skip(lhs, _rhs) => match lhs.as_ref() {
            IrNode::Next(_, _) => {}
            other => panic!("expected Next on lhs of Skip chain, got {:?}", other),
        },
        other => panic!("expected Skip(Next(...), ...), got {:?}", other),
    }
}

/// Reproducer for gorgeous/json.rs — the most complex panicking chain:
/// `"[" >> ( value << comma ? ) * ?w << "]"`. Combines nested
/// binary_factor inside a `Many` (`*`) wrapper with outer `>>` and `<<`
/// around bracket delimiters.
#[test]
fn lower_binary_nested_json_array_shape() {
    let ir = lower_grammar(
        r#"
        value = /[a-z]+/ ;
        comma = "," ?w ;
        array = "[" >> (( value << comma ? ) *)?w << "]" ;
        "#,
    );
    // Outer shape: `"[" >> INNER << "]"` → `Skip(Next("[", INNER), "]")`.
    // Inner shape: `(( value << comma ? ) *)?w` — a Repeat over a
    // `Skip(value, Optional(comma))` binary_factor. The hard gate is
    // that lowering completes without panicking.
    let body = body_of(&ir, "array");
    match body {
        IrNode::Skip(_lhs, _rhs) => {}
        other => panic!("expected Skip(...) for array rule, got {:?}", other),
    }
}
