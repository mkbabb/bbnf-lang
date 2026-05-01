//! Delimiter-scan detection (Tranche X.8a — upstream replacement).
//!
//! Identifies `Wrap(open, Repeat(Alt(...)), close)` patterns where the
//! Alt branches can be distinguished by a forward scan for a pivot
//! byte. Detection runs during `mine_recognizers` (Tranche Z.0
//! unified walk), populating `ir.delim_scan_configs` keyed by the
//! wrap root's stable `NodeId`.
//!
//! This is the upstream half of the Tranche X.8 activation that moves
//! structural pattern detection out of `backend/patterns/` and into
//! the IR. The backend reads the resulting sidecar map via
//! `GrammarIR::delim_scan_configs` — it does not recompute.
//!
//! Previously lived at `backend/patterns/delim_scan.rs` in the
//! `bbnf-core` crate. Moved intact in Tranche X.8a; the detection
//! logic is identical, only the home crate changed. Tranche Z.0
//! additionally collapses the per-miner tree walk into the shared
//! orchestrator `walk_unified`.

use std::collections::HashSet;

use crate::dag::NodeId;
use crate::passes::inspect::{
    single_byte_literal, unwrap_map_ow, unwrap_to_alt, unwrap_to_repeat, unwrap_wrap,
};
use crate::{DelimScanConfig, GrammarIR, IrNode, RuleId};

use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

pub struct DelimScanMiner;

impl RecognizerMiner for DelimScanMiner {
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        let Some((open, middle, close)) = unwrap_wrap(node) else {
            return;
        };
        if let Some(config) = try_detect(open, middle, close, ctx.ir) {
            outputs.delim_scan_configs.insert(node_id, config);
        }
    }
}

/// Try to detect a delimiter-scannable `Wrap(open, Repeat(Alt), close)`
/// pattern from a raw `open / middle / close` triple. Returns `Some`
/// when every structural check passes; `None` when any invariant is
/// violated (non-byte literal delimiter, same open/close, missing
/// pivot, pivot-close collision, etc.).
pub fn try_detect(
    open: &IrNode,
    middle: &IrNode,
    close: &IrNode,
    ir: &GrammarIR,
) -> Option<DelimScanConfig> {
    let open_byte = single_byte_literal(open, ir)?;
    let close_byte = single_byte_literal(close, ir)?;
    if open_byte == close_byte {
        return None;
    }

    let mut visited = HashSet::new();
    let repeat_inner = unwrap_to_repeat(middle, ir, &mut visited)?;
    let mut visited = HashSet::new();
    let branches = unwrap_to_alt(repeat_inner, ir, &mut visited)?;
    if branches.len() < 2 {
        return None;
    }

    let mut pivot_byte: Option<u8> = None;
    let mut trail_byte: Option<u8> = None;
    let mut block_rule: Option<(RuleId, String)> = None;
    let mut pivot_rule: Option<(RuleId, String)> = None;

    for branch in branches {
        let inner = unwrap_map_ow(&branch.node);
        let mut visited = HashSet::new();
        if let Some((piv, trail)) = find_pivot_in_seq(inner, ir, &mut visited) {
            if pivot_byte.is_some() && pivot_byte != Some(piv) {
                return None;
            }
            pivot_byte = Some(piv);
            if trail.is_some() {
                trail_byte = trail;
            }
            if let IrNode::Ref(id) = inner {
                let name = ir.get_string(ir.rules[*id as usize].name).to_string();
                pivot_rule = Some((*id, name));
            }
        } else if let Some(rule_id) = find_block_ref(inner) {
            let name = ir.get_string(ir.rules[rule_id as usize].name).to_string();
            block_rule = Some((rule_id, name));
        }
    }

    let pivot_byte = pivot_byte?;
    if pivot_byte == open_byte || pivot_byte == close_byte {
        return None;
    }
    if let Some(tb) = trail_byte {
        if tb == open_byte || tb == close_byte || tb == pivot_byte {
            return None;
        }
    }

    Some(DelimScanConfig {
        open_byte,
        close_byte,
        pivot_byte,
        trail_byte,
        block_rule,
        pivot_rule,
    })
}

// ─── Helpers ───────────────────────────────────────────────────────────────

fn trailing_delimiter_byte(node: &IrNode, ir: &GrammarIR) -> Option<u8> {
    if let IrNode::Literal(sid) = node {
        let bytes = ir.get_string(*sid).as_bytes();
        if bytes.len() >= 2 {
            let last = *bytes.last()?;
            if last == b':' || last == b';' {
                return Some(last);
            }
        }
    }
    None
}

fn find_pivot_in_seq(
    node: &IrNode,
    ir: &GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> Option<(u8, Option<u8>)> {
    match node {
        IrNode::Seq(children) => find_pivot_in_children(children, ir),
        IrNode::Ref(rule_id) => {
            if !visited.insert(*rule_id) {
                return None;
            }
            let result = find_pivot_in_seq(
                unwrap_map_ow(&ir.rules[*rule_id as usize].body),
                ir,
                visited,
            );
            visited.remove(rule_id);
            result
        }
        IrNode::Alt(branches, _) => {
            let mut common_pivot: Option<u8> = None;
            let mut common_trail: Option<u8> = None;
            for branch in branches {
                let (piv, trail) = find_pivot_in_seq(unwrap_map_ow(&branch.node), ir, visited)?;
                if let Some(cp) = common_pivot {
                    if cp != piv {
                        return None;
                    }
                } else {
                    common_pivot = Some(piv);
                }
                if trail.is_some() {
                    common_trail = trail;
                }
            }
            common_pivot.map(|p| (p, common_trail))
        }
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            find_pivot_in_seq(inner, ir, visited)
        }
        _ => None,
    }
}

fn find_pivot_in_children(children: &[IrNode], ir: &GrammarIR) -> Option<(u8, Option<u8>)> {
    if children.len() < 2 {
        return None;
    }
    let mut pivot: Option<u8> = None;
    let mut trail: Option<u8> = None;
    if let Some(byte) = trailing_delimiter_byte(&children[0], ir) {
        pivot = Some(byte);
    }
    for (i, child) in children.iter().enumerate() {
        if i == 0 {
            continue;
        }
        let inner = unwrap_map_ow(child);
        if let Some(byte) = single_byte_literal(inner, ir) {
            if pivot.is_none() {
                pivot = Some(byte);
            }
            continue;
        }
        if let IrNode::Repeat {
            inner: rep_inner,
            lo: 0,
            hi: 1,
        } = inner
        {
            if let Some(byte) = single_byte_literal(unwrap_map_ow(rep_inner), ir) {
                trail = Some(byte);
            }
        }
    }
    pivot.map(|p| (p, trail))
}

fn find_block_ref(node: &IrNode) -> Option<RuleId> {
    match node {
        IrNode::Ref(rule_id) => Some(*rule_id),
        IrNode::Seq(children) => children
            .iter()
            .find_map(|c| find_block_ref(unwrap_map_ow(c))),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => find_block_ref(inner),
        _ => None,
    }
}
