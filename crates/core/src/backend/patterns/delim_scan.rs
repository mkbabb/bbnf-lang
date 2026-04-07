//! Shared delimiter-scan pattern detection.
//!
//! Identifies `Wrap(open, Repeat(Alt(...)), close)` patterns where the Alt
//! branches can be distinguished by a forward scan for a pivot byte.
//! Detection is target-agnostic; each backend emits its own scanning code.

use bbnf_ir::{GrammarIR, IrNode, RuleId};

use crate::backend::unescape_literal;
use crate::backend::types::DelimScanConfig;

// ─── Public Entry Point ────────────────────────────────────────────────────

/// Try to detect a delimiter-scannable Wrap(open, Repeat(Alt), close) pattern.
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

    let repeat_inner = unwrap_to_repeat(middle, ir)?;
    let branches = unwrap_to_alt(repeat_inner, ir)?;
    if branches.len() < 2 {
        return None;
    }

    let mut pivot_byte: Option<u8> = None;
    let mut trail_byte: Option<u8> = None;
    let mut block_rule: Option<(RuleId, String)> = None;
    let mut pivot_rule: Option<(RuleId, String)> = None;

    for branch in branches {
        let inner = unwrap_map_ow(&branch.node);
        if let Some((piv, trail)) = find_pivot_in_seq(inner, ir) {
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

fn single_byte_literal(node: &IrNode, ir: &GrammarIR) -> Option<u8> {
    if let IrNode::Literal(sid) = node {
        let unescaped = unescape_literal(ir.get_string(*sid));
        let bytes = unescaped.as_bytes();
        if bytes.len() == 1 {
            return Some(bytes[0]);
        }
    }
    None
}

fn trailing_delimiter_byte(node: &IrNode, ir: &GrammarIR) -> Option<u8> {
    if let IrNode::Literal(sid) = node {
        let unescaped = unescape_literal(ir.get_string(*sid));
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

fn unwrap_to_repeat<'a>(node: &'a IrNode, ir: &'a GrammarIR) -> Option<&'a IrNode> {
    match node {
        IrNode::Repeat { inner, lo: 0, .. } => Some(inner),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            unwrap_to_repeat(inner, ir)
        }
        IrNode::Ref(rule_id) => unwrap_to_repeat(&ir.rules[*rule_id as usize].body, ir),
        IrNode::Next(_, b) => unwrap_to_repeat(b, ir),
        IrNode::Skip(a, _) => unwrap_to_repeat(a, ir),
        _ => None,
    }
}

fn unwrap_to_alt<'a>(node: &'a IrNode, ir: &'a GrammarIR) -> Option<&'a [bbnf_ir::AltBranch]> {
    match node {
        IrNode::Alt(branches, dispatch) if dispatch.is_none() => Some(branches),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => unwrap_to_alt(inner, ir),
        IrNode::Ref(rule_id) => unwrap_to_alt(&ir.rules[*rule_id as usize].body, ir),
        IrNode::Next(_, b) => unwrap_to_alt(b, ir),
        IrNode::Skip(a, _) => unwrap_to_alt(a, ir),
        _ => None,
    }
}

fn unwrap_map_ow(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => unwrap_map_ow(inner),
        other => other,
    }
}

fn find_pivot_in_seq(node: &IrNode, ir: &GrammarIR) -> Option<(u8, Option<u8>)> {
    match node {
        IrNode::Seq(children) => find_pivot_in_children(children, ir),
        IrNode::Ref(rule_id) => {
            find_pivot_in_seq(unwrap_map_ow(&ir.rules[*rule_id as usize].body), ir)
        }
        IrNode::Alt(branches, _) => {
            let mut common_pivot: Option<u8> = None;
            let mut common_trail: Option<u8> = None;
            for branch in branches {
                let (piv, trail) = find_pivot_in_seq(unwrap_map_ow(&branch.node), ir)?;
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
            find_pivot_in_seq(inner, ir)
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
        IrNode::Seq(children) => children.iter().find_map(|c| find_block_ref(unwrap_map_ow(c))),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => find_block_ref(inner),
        _ => None,
    }
}
