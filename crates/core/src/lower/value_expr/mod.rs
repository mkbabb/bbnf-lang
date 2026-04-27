//! Value expression lowering: `->` map syntax.
//!
//! Lowers the value expression sub-language (arithmetic, boolean, function
//! calls, closures, literals) from `BbnfBootstrapNodeView` nodes into
//! `MapExpr`.
//!
//! Tranche AE.1 — shape-agnostic walking. Under structural-mode tape
//! emission, the value-expression sub-grammar collapses far more
//! aggressively than the outer expression layers: most leaf rules
//! (`int_lit` / `float_lit` / `bool_lit` / `string_lit` / `value_ident` /
//! `value_path` / `value_input` / `value_fn_call` / `value_closure` /
//! `mul_op` / `add_op` / `cmp_op`) are inlined into their callers and
//! never produce a tape compound of their own. The rule_kinds that
//! actually surface on the tape are:
//!
//!   - `value_expr` (alt of closure / value_or chain)
//!   - `value_and`, `value_cmp`, `value_add`, `value_mul`
//!   - `value_unary`, `value_atom`
//!
//! Operator tokens (`||`, `&&`, `==` ... `*`, `/`, `%`, unary `-`/`!`,
//! closure `|...|` markers, function-call parentheses, etc.) consume
//! source bytes but push **nothing**. The lowering recovers them by
//! inspecting the source slice between adjacent operand spans (for
//! binary chains) or the leading bytes of a compound's span text (for
//! atoms / closures / unary prefixes).
//!
//! Every rule_kind reachable from the value-expression grammar gets
//! an explicit handler in `dispatch_value_expr`. Unknown rule_kinds
//! panic with a descriptive message; silent `MapExpr::Input`
//! fallbacks are forbidden — they would corrupt every downstream
//! `->` mapping invisibly.

use bbnf_ir::MapExpr;

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use super::LowerCtx;

mod atom;
mod literals;
mod precedence;
mod simple_kinds;
mod unwrap;

pub(crate) use literals::{parse_float_literal, parse_int_literal, split_numeric_suffix};
pub(crate) use unwrap::{deep_unwrap_value, extract_value_func_name, is_type_name, unwrap_value_ident_str};

// ─── Public entry: lower a value expression ─────────────────────────────────

/// Lower a value expression view to a `MapExpr`. Single source of
/// truth for value-expression dispatch — every recursive descent
/// (operator-layer fold, atom unwrap, function-call args, closure
/// body) routes back through here.
pub(crate) fn lower_value_expr<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    dispatch_value_expr(node, ctx)
}

/// Central rule_kind dispatcher. The bbnf value-expression grammar
/// is a closed schema; every reachable rule_kind has an explicit
/// handler. Unknown rule_kinds panic with a descriptive message
/// pointing at the offending node — silent fallbacks are forbidden.
pub(super) fn dispatch_value_expr<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    match node.rule_kind() {
        // Top-level value_expr wrapper (= value_closure | value_or).
        // Peel by re-dispatching on the inner head.
        //
        // Under DTA, the value_expr rule body is wrapped in a Seq
        // compound — `node.child(0)` picks the anonymous wrapper, not
        // the semantic head. Descend through anonymous wrappers to the
        // first value-layer compound (`value_closure` or `value_or`);
        // if the optimizer inlined even those, descend to any
        // downstream value-precedence-chain head.
        BbnfBootstrapRuleKind::value_expr => {
            let inner = simple_kinds::value_expr_head(node).unwrap_or(node);
            if inner.cursor().offset() == node.cursor().offset() {
                // Defensive — avoid recursion if descent returned the
                // same node. Fall through to the atom path.
                return atom::lower_value_atom(node, ctx);
            }
            dispatch_value_expr(inner, ctx)
        }

        // Top of the value sub-grammar — alt of closure / value_or chain.
        BbnfBootstrapRuleKind::value_or => {
            simple_kinds::lower_value_expr_or_closure(node, ctx)
        }

        // Precedence-chain layers. Each is `(operand, (op operand)*)`
        // under structural mode the chain layer's compound has shape
        // `[first_operand, Repeat([rest_operands])]` with operator
        // bytes consumed-but-not-pushed between adjacent operand
        // spans.
        BbnfBootstrapRuleKind::value_and => {
            precedence::fold_value_chain(node, &precedence::LAYER_AND, ctx)
        }
        BbnfBootstrapRuleKind::value_cmp => {
            precedence::fold_value_chain(node, &precedence::LAYER_CMP, ctx)
        }
        BbnfBootstrapRuleKind::value_add => {
            precedence::fold_value_chain(node, &precedence::LAYER_ADD, ctx)
        }
        BbnfBootstrapRuleKind::value_mul => {
            precedence::fold_value_chain(node, &precedence::LAYER_MUL, ctx)
        }

        // Unary prefix layer. Body is `( "!" | "-" )? value_atom`;
        // the prefix consumes bytes without pushing, so we
        // disambiguate via leading-byte inspection of the unary
        // span text.
        BbnfBootstrapRuleKind::value_unary => atom::lower_value_unary(node, ctx),

        // Atom layer. Inlined alt over int/float/bool/string lits,
        // input chains, paths, function calls, and parenthesised
        // sub-expressions. Discriminated by leading-byte inspection
        // of the atom span text.
        BbnfBootstrapRuleKind::value_atom => atom::lower_value_atom(node, ctx),

        // Leaf rule_kinds — these only surface when the optimizer
        // happens to preserve the wrapper compound rather than
        // inlining.
        //
        // Under DTA `int_lit` is ALSO the walker's sentinel rule_kind
        // for compounds emitted without a `DtaState::Ref` (the
        // optimizer fully inlined the value_unary + value_atom
        // layers, so the value-expression body surfaces as a
        // sentinel-tagged compound carrying the atom's span).
        // Distinguish real int literals from the sentinel by the
        // leading byte: real numeric starts with a digit or `.`,
        // anything else is the inlined atom and should route through
        // `lower_value_atom` for proper classification by span text.
        BbnfBootstrapRuleKind::int_lit => {
            let text = node.span_text();
            let first = text.trim_start().as_bytes().first().copied();
            match first {
                Some(b'0'..=b'9') | Some(b'.') => parse_int_literal(text),
                _ => atom::lower_value_atom(node, ctx),
            }
        }
        BbnfBootstrapRuleKind::float_lit => parse_float_literal(node.span_text()),
        BbnfBootstrapRuleKind::bool_lit => MapExpr::BoolLit(node.span_text() == "true"),
        BbnfBootstrapRuleKind::string_lit => atom::lower_string_lit(node, ctx),
        BbnfBootstrapRuleKind::value_ident => atom::lower_bare_ident(node.span_text(), ctx),
        BbnfBootstrapRuleKind::identifier => atom::lower_bare_ident(node.span_text(), ctx),
        BbnfBootstrapRuleKind::value_input => simple_kinds::lower_value_input(node, ctx),
        BbnfBootstrapRuleKind::value_path => simple_kinds::lower_value_path(node, ctx),
        BbnfBootstrapRuleKind::value_fn_call => simple_kinds::lower_value_fn_call(node, ctx),
        BbnfBootstrapRuleKind::value_closure => simple_kinds::lower_value_closure(node, ctx),

        other => panic!(
            "lower/value_expr.rs: dispatch_value_expr called on \
             unhandled rule_kind {:?} (span = {:?}, text = {:?}). \
             Add an explicit handler for this rule_kind.",
            other,
            node.span(),
            node.span_text(),
        ),
    }
}
