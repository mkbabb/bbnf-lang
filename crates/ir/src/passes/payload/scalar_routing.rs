//! AW.0.8 scalar-range routing for inline vs. wide payloads.
//!
//! Background: `PayloadData::InlineScalar(u32::MAX)` is
//! indistinguishable from payload-absent (`u32::MAX == TapeOffset::
//! NONE`). The `white = 0xFFFFFFFFu32` named-color value collides with
//! the sentinel — the runtime cannot tell whether a record carries
//! white or no payload at all.
//!
//! Fix: when a rule's projected `u32` payload can reach `u32::MAX`, the
//! codegen must route through `PayloadData::WideScalar` (8-byte
//! representation, not sentinel-colliding). Rules whose value domain
//! cannot touch the sentinel keep `InlineScalar`.
//!
//! This module exposes [`scalar_range_includes_sentinel`], a single
//! predicate that inspects a rule's body for `MapExpr` terminals
//! producing `u32` values and determines whether the computed range
//! includes `u32::MAX`. The emitter calls it at codegen time to decide
//! between `InlineScalar` and `WideScalar` for the `u32` case.

use std::collections::HashSet;

use crate::{FnDescriptor, GrammarIR, IrNode, IrRule, MapExpr, RuleId};

/// Return true iff the rule's body can emit a `u32` payload whose
/// value could equal `u32::MAX` — meaning the codegen must route
/// through `PayloadData::WideScalar` to avoid the sentinel collision.
///
/// The walk inspects `Map { ..., FnDescriptor::Expr { expr } }` nodes
/// reachable from the body. A `MapExpr::IntLit(value)` whose `value`
/// matches `u32::MAX` is the primary concern; any other expression
/// whose evaluation cannot be statically bounded below `u32::MAX`
/// (e.g. a free function call returning `u32`, a hex-convert on
/// arbitrary input, an arithmetic over free variables) is also
/// considered sentinel-reaching.
///
/// Conservative by design: when the range cannot be proven bounded,
/// the rule promotes to `WideScalar`. False positives cost an extra
/// 4 bytes per record; false negatives would corrupt the payload.
pub fn scalar_range_includes_sentinel(rule: &IrRule, ir: &GrammarIR) -> bool {
    let mut visited = HashSet::new();
    body_can_reach_u32_max(&rule.body, ir, &mut visited)
}

fn body_can_reach_u32_max(node: &IrNode, ir: &GrammarIR, visited: &mut HashSet<RuleId>) -> bool {
    match node {
        IrNode::Map { fn_id, .. } => {
            let fn_desc = &ir.fns[*fn_id as usize];
            match fn_desc {
                FnDescriptor::Expr { expr, .. } => map_expr_can_reach_u32_max(expr),
                // HexConvert takes hex input; any hex digit sequence can
                // encode u32::MAX (`ffffffff`). Conservative: promote.
                FnDescriptor::HexConvert { .. } => true,
                FnDescriptor::NumberConvert { .. } => false, // produces F64
                FnDescriptor::EnumWrap { .. }
                | FnDescriptor::BoxWrap
                | FnDescriptor::SpanCapture => false,
            }
        }
        IrNode::Alt(branches, _) => branches
            .iter()
            .any(|b| body_can_reach_u32_max(&b.node, ir, visited)),
        IrNode::Seq(children) => children
            .iter()
            .any(|c| body_can_reach_u32_max(c, ir, visited)),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner) => body_can_reach_u32_max(inner, ir, visited),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            body_can_reach_u32_max(a, ir, visited) || body_can_reach_u32_max(b, ir, visited)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            body_can_reach_u32_max(token, ir, visited)
                || arms
                    .iter()
                    .any(|a| body_can_reach_u32_max(&a.continuation, ir, visited))
                || body_can_reach_u32_max(fallback, ir, visited)
        }
        IrNode::Ref(rid) => {
            // Cycle guard: a recursive grammar would otherwise loop
            // forever. On cycle detection return false — the walk
            // must terminate; if a referenced rule elsewhere in the
            // tree actually hits the sentinel, that path will find
            // it without the cycle.
            if !visited.insert(*rid) {
                return false;
            }
            let result = if let Some(called) = ir.rules.iter().find(|r| r.id == *rid) {
                body_can_reach_u32_max(&called.body, ir, visited)
            } else {
                // Missing rule is conservative-promote.
                true
            };
            visited.remove(rid);
            result
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => false,
    }
}

fn map_expr_can_reach_u32_max(expr: &MapExpr) -> bool {
    match expr {
        MapExpr::IntLit(v) => {
            // i64 → u32 reinterpretation: `as u32` keeps the low 32 bits.
            // u32::MAX is 0xFFFFFFFFu32 == -1i32 == 4294967295i64.
            let as_u32 = (*v as u64) as u32;
            as_u32 == u32::MAX
        }
        MapExpr::FloatLit(_) | MapExpr::BoolLit(_) | MapExpr::StringLit(_) => false,
        MapExpr::Input | MapExpr::InputProp { .. } => {
            // Parse-result input can be any value; conservative.
            true
        }
        MapExpr::FnCall { args, .. } => {
            // Free function calls over arbitrary input can return
            // u32::MAX. Conservative: promote unless every arg is
            // bounded below the sentinel (which we cannot prove for
            // free functions).
            if args.iter().all(|a| a.is_constant()) {
                // Constant-propagated: safe only if none of the
                // constants hit the sentinel.
                args.iter().any(map_expr_can_reach_u32_max)
            } else {
                true
            }
        }
        MapExpr::BinOp { lhs, rhs, .. } => {
            map_expr_can_reach_u32_max(lhs) || map_expr_can_reach_u32_max(rhs)
        }
        MapExpr::UnaryOp { inner, .. } => map_expr_can_reach_u32_max(inner),
    }
}
