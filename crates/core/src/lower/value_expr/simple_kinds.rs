//! Wrappers for value rules the optimizer sometimes preserves
//! (`value_input` / `value_path` / `value_fn_call` / `value_closure`),
//! the value_expr-head descent helpers, and the closure-aware value
//! environment lookup.

use std::collections::HashMap;

use bbnf_ir::MapExpr;

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfValue, BbnfView};

use super::super::LowerCtx;
use super::atom::{lower_fn_call_atom, lower_input_chain, lower_path_atom, same_focus};
use super::dispatch_value_expr;
use super::precedence::{LAYER_OR, fold_value_chain};
use super::view_walk::find_descendant_by_compound_kind;

// ─── value-expression descent helpers ────────────────────────────────────────

/// Outermost-first ordering of value-layer compound kinds used to
/// find the inner head of a `value_expr` compound.
///
/// Under struct-direct projection the wrapper structure preserves
/// rule identity on each compound's [`BbnfCompoundKind`] arm. A
/// descendant search against this ordered list returns the outermost
/// semantic rule in document order. `value_closure` and `value_or`
/// alternate at the `value_expr` body layer; the remaining
/// precedence-chain kinds + atom handle optimizer-collapsed shapes
/// where the outer wrapper was inlined away.
pub(super) const VALUE_HEAD_KINDS: &[BbnfCompoundKind] = &[
    BbnfCompoundKind::ValueClosure,
    BbnfCompoundKind::ValueOr,
    BbnfCompoundKind::ValueAnd,
    BbnfCompoundKind::ValueCmp,
    BbnfCompoundKind::ValueAdd,
    BbnfCompoundKind::ValueMul,
    BbnfCompoundKind::ValueUnary,
    BbnfCompoundKind::ValueAtom,
];

/// Find the semantic head of a `value_expr` compound.
///
/// `find_descendant_by_compound_kind` is called against each
/// value-layer compound kind in outermost-first priority order.
/// First hit wins; returns `None` only if none of the value-layer
/// compound kinds surface as descendants (pathological input —
/// every `value_expr` body must resolve to at least a `value_atom`).
pub(super) fn value_expr_head<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
) -> Option<BbnfView<'a, 'p>> {
    for &kind in VALUE_HEAD_KINDS {
        if let Some(v) = find_descendant_by_compound_kind(node, kind) {
            // Skip `node` itself — only return distinct views.
            if !same_focus(v, node) {
                return Some(v);
            }
        }
    }
    None
}

// ─── value_expr: closure / or-chain ──────────────────────────────────────────

/// Distinguish closure (`|p1, ...| body`) from a value_or chain by
/// inspecting the leading byte of the compound's span text. Under
/// structural mode the closure markers consume bytes without
/// pushing, so the closure case is identified by the leading `|`.
pub(super) fn lower_value_expr_or_closure<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    let text = node.span_text().unwrap_or("");
    if text.as_bytes().first() == Some(&b'|') {
        lower_value_closure(node, ctx)
    } else {
        // value_or chain — body is `value_and , ( "||" ?w , value_and ) *`.
        fold_value_chain(node, &LAYER_OR, ctx)
    }
}

// ─── Standalone leaf accessors (when the optimizer preserves the wrapper) ──

/// Lower a `value_input` rule compound (when preserved). Defers to
/// the same source-slice walk used for inlined input chains —
/// behavioural parity with `lower_input_chain`.
pub(super) fn lower_value_input<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    let span = node.span_text().unwrap_or("").trim_start();
    lower_input_chain(node, span, ctx)
}

/// Lower a `value_path` rule compound (when preserved). Same
/// source-slice walk as the inlined path-atom case.
pub(super) fn lower_value_path<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    let span = node.span_text().unwrap_or("").trim_start();
    lower_path_atom(node, span, ctx)
}

/// Lower a `value_fn_call` rule compound (when preserved).
pub(super) fn lower_value_fn_call<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    let span = node.span_text().unwrap_or("").trim_start();
    lower_fn_call_atom(node, span, ctx)
}

// ─── Closures ────────────────────────────────────────────────────────────────

/// Lower a value closure compound. The grammar is
/// `value_closure = "|", value_ident, ( "," ?w , value_ident ) * , "|", value_expr`.
///
/// Under structural-mode emission the `|` markers and parameter
/// idents consume bytes without pushing, but the inner `value_expr`
/// body DOES push a rule compound. Param recovery walks the source
/// slice between the leading `|` and the matching closing `|`,
/// stripping `,` separators and whitespace.
pub(super) fn lower_value_closure<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    let text: &'p str = node.span_text().unwrap_or_else(|| {
        panic!(
            "lower_value_closure: value_closure compound has no recoverable \
             source span — typed-projection invariants imply at least the \
             closing `|` and body expression carry source position",
        )
    });
    debug_assert!(
        text.as_bytes().first() == Some(&b'|'),
        "lower_value_closure: closure span doesn't start with `|`: {:?}",
        text,
    );

    // Find the closing `|` matching the opening one. Closure
    // params are bare identifiers separated by `,`; no nested `|`
    // appears within the param list.
    let after_open: &'p str = &text[1..];
    let close_rel = after_open
        .find('|')
        .expect("lower_value_closure: missing closing `|` for closure params");
    let params_text: &'p str = &after_open[..close_rel];

    let params: Vec<&'p str> = params_text
        .split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .collect();

    // The body is the trailing `value_expr` rule compound. Look it
    // up by compound-kind descent.
    let body = find_descendant_by_compound_kind(node, BbnfCompoundKind::ValueExpr)
        .or_else(|| {
            // Fallback when the body's `value_expr` wrapper was
            // inlined away — the body resolves to one of the
            // chain-layer compound kinds directly.
            for &kind in VALUE_HEAD_KINDS {
                if let Some(v) = find_descendant_by_compound_kind(node, kind) {
                    if !same_focus(v, node) {
                        return Some(v);
                    }
                }
            }
            None
        })
        .or_else(|| {
            // Last-resort fallback: pick the first compound child
            // (defensive against shape-routed empty-wrap collapses).
            RuntimeView::children(&node)
                .find(|c| matches!(c.focus(), BbnfValue::Compound(_)))
        })
        .expect("lower_value_closure: missing body value_expr child");

    // Bind each param into the value-environment frame. The first
    // param maps to `MapExpr::Input` (the convention — the closure
    // is applied at parse time to the matched span); the remaining
    // params map to `InputProp { prop: <param_name> }` so users can
    // pull additional payloads off composite inputs.
    let mut frame: HashMap<&'p str, MapExpr> = HashMap::new();
    for (i, name) in params.iter().copied().enumerate() {
        let value = if i == 0 {
            MapExpr::Input
        } else {
            let sid = ctx.strings.intern(name);
            MapExpr::InputProp { prop: sid }
        };
        frame.insert(name, value);
    }

    ctx.value_env.push(frame);
    let result = dispatch_value_expr(body, ctx);
    ctx.value_env.pop();
    result
}

// ─── Value env lookup ────────────────────────────────────────────────────────

/// Look up a name in the value-environment stack (top frame first,
/// mirroring lexical scope). Returns a clone of the bound `MapExpr`
/// if found.
pub(super) fn lookup_value_env(name: &str, env: &[HashMap<&str, MapExpr>]) -> Option<MapExpr> {
    for frame in env.iter().rev() {
        if let Some(bound) = frame.get(name) {
            return Some(bound.clone());
        }
    }
    None
}
