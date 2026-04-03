//! Pattern detection for delimiter-driven flat scanning.
//!
//! Identifies `Wrap(open, Repeat(Alt(...)), close)` patterns where the Alt
//! branches can be distinguished by a forward `memchr` scan for a pivot byte.

use bbnf_ir::{GrammarIR, IrNode, RuleId};

use super::DelimScanConfig;
use crate::backend::rust::unescape_literal;

// ── Public Entry Point ──────────────────────────────────────────────────────

/// Try to detect a delimiter-scannable Wrap(open, Repeat(Alt(...)), close) pattern.
///
/// Returns `None` if the pattern doesn't match or the extracted bytes collide.
pub(in crate::backend::rust) fn try_detect(
    open: &IrNode,
    middle: &IrNode,
    close: &IrNode,
    ir: &GrammarIR,
) -> Option<DelimScanConfig> {
    // 1. Open and close must be single-byte Literals.
    let open_byte = single_byte_literal(open, ir)?;
    let close_byte = single_byte_literal(close, ir)?;
    if open_byte == close_byte {
        return None;
    }

    // 2. Unwrap middle through OW/Map/Ref to find the Repeat.
    let (repeat_inner, content_rule) = unwrap_to_repeat_with_rule(middle, ir)?;

    // 3. Unwrap Repeat inner through OW/Map/Ref to find the Alt.
    let branches = unwrap_to_alt(repeat_inner, ir)?;
    if branches.len() < 2 {
        return None;
    }

    // 4. The Alt must NOT already have a dispatch table (overlapping FIRST sets).
    // We detect this by checking that no dispatch is present — dispatch is stored
    // in the Alt node itself, but we only have the branches here. The caller
    // (emit_span_wrap / emit_mono_wrap) should only call us when the middle is
    // a general-case (no sep_by_ws_until hit), which implies no dispatch.

    // 5. Classify branches: find a pivot branch and a block/fallback branch.
    let mut pivot_byte: Option<u8> = None;
    let mut trail_byte: Option<u8> = None;
    let mut block_fn: Option<RuleId> = None;

    let mut pivot_fn: Option<RuleId> = None;

    for branch in branches {
        let inner = unwrap_map_ow(&branch.node);
        if let Some((piv, trail)) = find_pivot_in_seq(inner, ir) {
            if pivot_byte.is_some() && pivot_byte != Some(piv) {
                return None; // Multiple different pivots — too complex.
            }
            pivot_byte = Some(piv);
            if trail.is_some() {
                trail_byte = trail;
            }
            // Track the pivot branch's rule for monolithic fallback.
            if let IrNode::Ref(id) = inner {
                pivot_fn = Some(*id);
            }
        } else if let Some(rule_id) = find_block_ref(inner, open_byte, ir) {
            block_fn = Some(rule_id);
        }
    }

    let pivot_byte = pivot_byte?; // Must have at least one pivot branch.

    // 6. Verify all bytes are distinct.
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
        block_fn,
        pivot_fn,
        content_rule,
    })
}

// ── Detection Helpers ───────────────────────────────────────────────────────

/// Extract a single byte from a Literal node.
pub(super) fn single_byte_literal(node: &IrNode, ir: &GrammarIR) -> Option<u8> {
    if let IrNode::Literal(sid) = node {
        let raw = ir.get_string(*sid);
        let unescaped = unescape_literal(raw);
        let bytes = unescaped.as_bytes();
        if bytes.len() == 1 {
            return Some(bytes[0]);
        }
    }
    None
}

/// Detect a trailing delimiter byte in a multi-char Literal.
/// Handles the case where `merge_literals` fused a property name with `:`,
/// e.g. `"display:"` → trailing byte is `:`.
/// Only returns known delimiter bytes (`:`, `;`) to avoid false positives.
fn trailing_delimiter_byte(node: &IrNode, ir: &GrammarIR) -> Option<u8> {
    if let IrNode::Literal(sid) = node {
        let raw = ir.get_string(*sid);
        let unescaped = unescape_literal(raw);
        let bytes = unescaped.as_bytes();
        if bytes.len() >= 2 {
            let last = *bytes.last()?;
            if last == b':' || last == b';' {
                return Some(last);
            }
        }
    }
    None
}

/// Unwrap through OW/Map/Ref/Next/Skip layers to find a Repeat node.
/// Returns (Repeat inner node, Option<RuleId of the Ref that was followed>).
fn unwrap_to_repeat_with_rule<'a>(
    node: &'a IrNode,
    ir: &'a GrammarIR,
) -> Option<(&'a IrNode, Option<RuleId>)> {
    match node {
        IrNode::Repeat { inner, lo: 0, .. } => Some((inner, None)),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            unwrap_to_repeat_with_rule(inner, ir)
        }
        IrNode::Ref(rule_id) => {
            let rule = &ir.rules[*rule_id as usize];
            let result = unwrap_to_repeat_with_rule(&rule.body, ir)?;
            // Capture the outermost Ref that led to the Repeat.
            Some((result.0, Some(result.1.unwrap_or(*rule_id))))
        }
        IrNode::Next(_, b) => unwrap_to_repeat_with_rule(b, ir),
        IrNode::Skip(a, _) => unwrap_to_repeat_with_rule(a, ir),
        _ => None,
    }
}

/// Unwrap through OW/Map/Ref/Next/Skip layers to find an Alt node.
/// Returns branches only if no dispatch table.
fn unwrap_to_alt<'a>(node: &'a IrNode, ir: &'a GrammarIR) -> Option<&'a [bbnf_ir::AltBranch]> {
    match node {
        IrNode::Alt(branches, dispatch) if dispatch.is_none() => Some(branches),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => unwrap_to_alt(inner, ir),
        IrNode::Ref(rule_id) => {
            let rule = &ir.rules[*rule_id as usize];
            unwrap_to_alt(&rule.body, ir)
        }
        // Next(a, b) keeps right → the Alt is in b
        IrNode::Next(_, b) => unwrap_to_alt(b, ir),
        // Skip(a, b) keeps left → the Alt is in a
        IrNode::Skip(a, _) => unwrap_to_alt(a, ir),
        _ => None,
    }
}

/// Unwrap Map and OptionalWhitespace wrappers.
pub(super) fn unwrap_map_ow(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => unwrap_map_ow(inner),
        other => other,
    }
}

/// Find a pivot byte in a branch: the first single-byte Literal at position > 0
/// in a Seq, or the common pivot across all branches of an Alt.
/// Also detects a trailing optional Literal (e.g., `";" ?`).
/// Follows Ref nodes to find the Seq/Alt inside referenced rules.
fn find_pivot_in_seq(node: &IrNode, ir: &GrammarIR) -> Option<(u8, Option<u8>)> {
    match node {
        IrNode::Seq(children) => find_pivot_in_children(children, ir),
        IrNode::Ref(rule_id) => {
            let rule = &ir.rules[*rule_id as usize];
            find_pivot_in_seq(unwrap_map_ow(&rule.body), ir)
        }
        IrNode::Alt(branches, _) => {
            // All branches must share the same pivot byte.
            let mut common_pivot: Option<u8> = None;
            let mut common_trail: Option<u8> = None;
            for branch in branches {
                let (piv, trail) = find_pivot_in_seq(unwrap_map_ow(&branch.node), ir)?;
                if let Some(cp) = common_pivot {
                    if cp != piv {
                        return None; // Different pivots — can't use delim_scan.
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
            find_pivot_in_seq(inner, ir)
        }
        _ => None,
    }
}

/// Find a pivot byte within a Seq's children.
fn find_pivot_in_children(children: &[IrNode], ir: &GrammarIR) -> Option<(u8, Option<u8>)> {
    if children.len() < 2 {
        return None;
    }

    let mut pivot: Option<u8> = None;
    let mut trail: Option<u8> = None;

    // Check if the first child is a literal ending with a delimiter byte
    // (from merge_literals fusing e.g. "display" + ":" → "display:").
    if let Some(byte) = trailing_delimiter_byte(&children[0], ir) {
        pivot = Some(byte);
    }

    for (i, child) in children.iter().enumerate() {
        if i == 0 {
            continue; // Skip leading element (the regex/ident before the pivot).
        }
        let inner = unwrap_map_ow(child);
        // Check for single-byte Literal.
        if let Some(byte) = single_byte_literal(inner, ir) {
            if pivot.is_none() {
                pivot = Some(byte);
            }
            continue;
        }
        // Check for optional trailing literal: Repeat { Literal(x), 0, 1 }.
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

/// Check if a node is (or contains) a Ref to a cyclic rule — indicating a
/// fallback/block branch that handles content not matching the pivot delimiter.
/// In a delimiter scanner, this branch is invoked when `open_byte` is encountered
/// (nested block) or when no pivot is found (selector/other content).
fn find_block_ref(node: &IrNode, _open_byte: u8, _ir: &GrammarIR) -> Option<RuleId> {
    match node {
        IrNode::Ref(rule_id) => Some(*rule_id),
        IrNode::Seq(children) => {
            for child in children {
                if let Some(id) = find_block_ref(unwrap_map_ow(child), _open_byte, _ir) {
                    return Some(id);
                }
            }
            None
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            find_block_ref(inner, _open_byte, _ir)
        }
        _ => None,
    }
}
