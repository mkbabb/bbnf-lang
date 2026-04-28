//! Value expression lowering: `->` map syntax.
//!
//! Lowers the value expression sub-language (arithmetic, boolean, function
//! calls, closures, literals) from `BbnfView` nodes into `MapExpr`.
//!
//! Tranche AZ-II.cutover.D2 — struct-direct projection. Under the
//! struct-direct BBNF runtime (`crate::runtime::bbnf::BbnfDocument`),
//! every value-expression compound rule emits a typed
//! [`crate::runtime::bbnf::BbnfCompoundKind`] arm and every leaf rule
//! projects directly to its [`crate::runtime::bbnf::BbnfValue`] variant
//! (`Int` / `Float` / `Bool` / `Span`). Compound rules that surface:
//!
//!   - `value_expr` (alt of closure / value_or chain)
//!   - `value_and`, `value_cmp`, `value_add`, `value_mul`
//!   - `value_unary`, `value_atom`
//!   - `value_path`, `value_input`, `value_fn_call`, `value_closure`
//!
//! Leaf rules (`int_lit` / `float_lit` / `bool_lit` / `string_lit` /
//! `value_ident` / `identifier`) project to their typed payload
//! directly — `BbnfValue::Int(i)` / `BbnfValue::Float(f)` /
//! `BbnfValue::Bool(b)` / `BbnfValue::Span(s)` — never carrying their
//! own compound. The dispatch matches on these payload arms first,
//! then on compound-kind arms for structural rules.
//!
//! Operator tokens (`||`, `&&`, `==` ... `*`, `/`, `%`, unary `-`/`!`,
//! closure `|...|` markers, function-call parentheses, etc.) consume
//! source bytes but project no payload. The lowering recovers them
//! by inspecting the source slice between adjacent operand spans (for
//! binary chains) or the leading bytes of a compound's span text (for
//! atoms / closures / unary prefixes).
//!
//! Every kind reachable from the value-expression grammar gets an
//! explicit handler in `dispatch_value_expr`. Unknown kinds panic
//! with a descriptive message; silent `MapExpr::Input` fallbacks are
//! forbidden — they would corrupt every downstream `->` mapping
//! invisibly.

use bbnf_ir::MapExpr;

use crate::runtime::bbnf::{BbnfCompoundKind, BbnfValue, BbnfView};

use super::LowerCtx;

mod atom;
mod literals;
mod precedence;
mod simple_kinds;
mod unwrap;
mod view_walk;

pub(crate) use literals::split_numeric_suffix;
pub(crate) use unwrap::{deep_unwrap_value, extract_value_func_name, is_type_name, unwrap_value_ident_str};

// ─── Public entry: lower a value expression ─────────────────────────────────

/// Lower a value expression view to a `MapExpr`. Single source of
/// truth for value-expression dispatch — every recursive descent
/// (operator-layer fold, atom unwrap, function-call args, closure
/// body) routes back through here.
pub(crate) fn lower_value_expr<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    dispatch_value_expr(node, ctx)
}

/// Central kind dispatcher. The bbnf value-expression grammar is a
/// closed schema; every reachable kind has an explicit handler.
/// Unknown kinds panic with a descriptive message pointing at the
/// offending node — silent fallbacks are forbidden.
pub(super) fn dispatch_value_expr<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    // Leaf-payload dispatch comes first — typed projection delivers
    // the parsed payload directly without a per-leaf compound.
    match node.focus() {
        BbnfValue::Int(v) => return MapExpr::IntLit(v),
        BbnfValue::Float(v) => return MapExpr::FloatLit(v),
        BbnfValue::Bool(v) => return MapExpr::BoolLit(v),
        BbnfValue::Span(s) => return atom::lower_bare_ident_or_string(s, ctx),
        BbnfValue::Tag(_) | BbnfValue::Unit => {
            panic!(
                "lower/value_expr/mod.rs: dispatch_value_expr called on \
                 Tag / Unit leaf — value-expression rules never project \
                 to these payload arms"
            );
        }
        BbnfValue::Compound(_) => {}
    }

    // Compound-kind dispatch.
    match node.compound_kind() {
        // Top-level value_expr wrapper (= value_closure | value_or).
        // Peel by re-dispatching on the inner head.
        Some(BbnfCompoundKind::ValueExpr) => {
            let inner = simple_kinds::value_expr_head(node).unwrap_or(node);
            // Defensive — avoid recursion if descent returned the
            // same node. Fall through to the atom path.
            if atom::same_focus(inner, node) {
                return atom::lower_value_atom(node, ctx);
            }
            dispatch_value_expr(inner, ctx)
        }

        // Top of the value sub-grammar — alt of closure / value_or chain.
        Some(BbnfCompoundKind::ValueOr) => {
            simple_kinds::lower_value_expr_or_closure(node, ctx)
        }

        // Precedence-chain layers. Each is `(operand, (op operand)*)`
        // under structural mode the chain layer's compound has shape
        // `[first_operand, (rest_operands)...]` with operator bytes
        // consumed-but-not-pushed between adjacent operand spans.
        Some(BbnfCompoundKind::ValueAnd) => {
            precedence::fold_value_chain(node, &precedence::LAYER_AND, ctx)
        }
        Some(BbnfCompoundKind::ValueCmp) => {
            precedence::fold_value_chain(node, &precedence::LAYER_CMP, ctx)
        }
        Some(BbnfCompoundKind::ValueAdd) => {
            precedence::fold_value_chain(node, &precedence::LAYER_ADD, ctx)
        }
        Some(BbnfCompoundKind::ValueMul) => {
            precedence::fold_value_chain(node, &precedence::LAYER_MUL, ctx)
        }

        // Unary prefix layer. Body is `( "!" | "-" )? value_atom`;
        // the prefix consumes bytes without pushing, so we
        // disambiguate via leading-byte inspection of the unary
        // span text.
        Some(BbnfCompoundKind::ValueUnary) => atom::lower_value_unary(node, ctx),

        // Atom layer. Inlined alt over int/float/bool/string lits,
        // input chains, paths, function calls, and parenthesised
        // sub-expressions. Discriminated by leading-byte inspection
        // of the atom span text.
        Some(BbnfCompoundKind::ValueAtom) => atom::lower_value_atom(node, ctx),

        // Standalone leaf-shaped compound rules — these surface when
        // the optimizer preserves the wrapper rather than inlining.
        Some(BbnfCompoundKind::ValueInput) => simple_kinds::lower_value_input(node, ctx),
        Some(BbnfCompoundKind::ValuePath) => simple_kinds::lower_value_path(node, ctx),
        Some(BbnfCompoundKind::ValueFnCall) => simple_kinds::lower_value_fn_call(node, ctx),
        Some(BbnfCompoundKind::ValueClosure) => simple_kinds::lower_value_closure(node, ctx),

        // Other compound kinds reachable from the value-expression
        // grammar would only appear if an outer wrapper rule (e.g.
        // a `binary_factor` that wraps a `value_expr`) leaks into
        // the value-expression dispatch. The defensive route is to
        // descend into the first child and re-dispatch.
        Some(BbnfCompoundKind::Other) => {
            // The optimizer fully inlined the value layer wrapper
            // and what reached us is an anonymous compound carrying
            // the atom's span. Route through the atom path.
            atom::lower_value_atom(node, ctx)
        }
        Some(other) => panic!(
            "lower/value_expr/mod.rs: dispatch_value_expr called on \
             unhandled compound kind {:?} (focus = {:?}, \
             span_text = {:?}). Add an explicit handler for this kind.",
            other,
            node.focus(),
            node.span_text(),
        ),
        None => {
            // Cannot reach: leaf payload handled above. If we ever
            // do, the panic surfaces a typed-AST invariant break.
            panic!(
                "lower/value_expr/mod.rs: dispatch_value_expr saw a leaf \
                 payload {:?} after the leaf-dispatch fall-through — \
                 the typed-AST invariant is broken",
                node.focus()
            );
        }
    }
}
