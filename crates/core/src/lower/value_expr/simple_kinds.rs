//! Wrappers for value rules the optimizer sometimes preserves
//! (`value_input` / `value_path` / `value_fn_call` / `value_closure`),
//! the value_expr-head descent helpers, and the closure-aware value
//! environment lookup.

use std::collections::HashMap;

use bbnf_ir::MapExpr;

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use crate::lower::tape_walk::find_descendant_by_kind;

use super::super::LowerCtx;
use super::atom::{lower_fn_call_atom, lower_input_chain, lower_path_atom};
use super::dispatch_value_expr;
use super::precedence::{LAYER_OR, fold_value_chain};

// ─── DTA descent helpers ─────────────────────────────────────────────────────

/// Outermost-first ordering of value-layer rule kinds used to find
/// the inner head of a `value_expr` compound under DTA.
///
/// The DTA walker wraps rule bodies in Seq compounds; `node.child(0)`
/// picks the anonymous wrapper, not the semantic head. A descendant
/// search against this ordered list returns the outermost semantic
/// rule in document order. `value_closure` and `value_or` alternate
/// at the `value_expr` body layer; the remaining precedence-chain
/// kinds + atom handle optimizer-collapsed shapes where the outer
/// wrapper was inlined away.
pub(super) const VALUE_HEAD_KINDS: &[BbnfBootstrapRuleKind] = &[
    BbnfBootstrapRuleKind::value_closure,
    BbnfBootstrapRuleKind::value_or,
    BbnfBootstrapRuleKind::value_and,
    BbnfBootstrapRuleKind::value_cmp,
    BbnfBootstrapRuleKind::value_add,
    BbnfBootstrapRuleKind::value_mul,
    BbnfBootstrapRuleKind::value_unary,
    BbnfBootstrapRuleKind::value_atom,
];

/// Find the semantic head of a `value_expr` compound under DTA.
///
/// `find_descendant_by_kind` is called against each value-layer rule
/// kind in outermost-first priority order. First hit wins; returns
/// `None` only if none of the value-layer rule kinds surface as
/// descendants (pathological input — every `value_expr` body must
/// resolve to at least a `value_atom`).
pub(super) fn value_expr_head<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    for &kind in VALUE_HEAD_KINDS {
        if let Some(v) = find_descendant_by_kind(node, kind) {
            // Skip `node` itself — if `node.rule_kind() == value_expr`
            // and `find_descendant_by_kind` matches the root (it won't
            // here because value_expr is not in VALUE_HEAD_KINDS), we
            // still want to descend. Defensive: only return distinct
            // views.
            if v.cursor().offset() != node.cursor().offset() {
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
pub(super) fn lower_value_expr_or_closure<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let text = node.span_text();
    if text.as_bytes().first() == Some(&b'|') {
        lower_value_closure(node, ctx)
    } else {
        // value_or chain — body is `value_and , ( "||" ?w , value_and ) *`.
        fold_value_chain(node, &LAYER_OR, ctx)
    }
}

// ─── Standalone leaf accessors (when the tape DOES preserve the rule) ───────

/// Lower a `value_input` rule compound (when preserved). Defers to
/// the same source-slice walk used for inlined input chains —
/// behavioural parity with `lower_input_chain`.
pub(super) fn lower_value_input<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    lower_input_chain(node, node.span_text().trim_start(), ctx)
}

/// Lower a `value_path` rule compound (when preserved). Same
/// source-slice walk as the inlined path-atom case.
pub(super) fn lower_value_path<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    lower_path_atom(node, node.span_text().trim_start(), ctx)
}

/// Lower a `value_fn_call` rule compound (when preserved).
pub(super) fn lower_value_fn_call<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    lower_fn_call_atom(node, node.span_text().trim_start(), ctx)
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
pub(super) fn lower_value_closure<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let text: &'a str = node.span_text();
    debug_assert!(
        text.as_bytes().first() == Some(&b'|'),
        "lower_value_closure: closure span doesn't start with `|`: {:?}",
        text,
    );

    // Find the closing `|` matching the opening one. Closure
    // params are bare identifiers separated by `,`; no nested `|`
    // appears within the param list.
    let after_open: &'a str = &text[1..];
    let close_rel = after_open
        .find('|')
        .expect("lower_value_closure: missing closing `|` for closure params");
    let params_text: &'a str = &after_open[..close_rel];

    // `text` is `&'a str` (the parser input lifetime), so every
    // sub-slice carries the same lifetime. No unsafe needed.
    let params: Vec<&'a str> = params_text
        .split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .collect();

    // The body is the trailing `value_expr` rule compound — it's
    // the only Rule child this compound contains (the param
    // identifiers don't push).
    //
    // Under DTA the body `value_expr` may sit inside an anonymous
    // Seq wrapper emitted by the walker; a direct `TapeKind::Rule`
    // first-match could pick that wrapper (also `TapeKind::Rule`
    // under the walker's `frame_to_tape_kind(Seq) == Rule` policy)
    // rather than the real value_expr. Descend to the value_expr
    // descendant; fall back to the first TapeKind::Rule child for
    // non-DTA shapes.
    use crate::runtime::tape::TapeKind;
    let body = find_descendant_by_kind(node, BbnfBootstrapRuleKind::value_expr)
        .or_else(|| node.children().find(|c| c.kind() == TapeKind::Rule))
        .expect("lower_value_closure: missing body value_expr child");

    // Bind each param into the value-environment frame. The first
    // param maps to `MapExpr::Input` (the convention — the closure
    // is applied at parse time to the matched span); the remaining
    // params map to `InputProp { prop: <param_name> }` so users can
    // pull additional payloads off composite inputs.
    let mut frame: HashMap<&'a str, MapExpr> = HashMap::new();
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
