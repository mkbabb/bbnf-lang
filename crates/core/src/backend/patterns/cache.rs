//! Pattern detection cache builders.
//!
//! Pre-solve delim-scan and key-dispatch configurations for every
//! eligible node in a grammar once, keyed by the node's stable
//! `NodeId`. The driver looks up pre-solved results at compile time
//! instead of re-walking the IR tree per-node. This is the Tranche F
//! architectural win: detection moves from the backend's per-Alt
//! walks into a one-shot prepare pass, turning an O(N × depth)
//! re-walk per compile into an O(N) cache build + O(1) lookups.

use std::collections::HashMap;

use bbnf_ir::dag::NodeId;
use bbnf_ir::{GrammarIR, IrNode};

use super::delim_scan::{self};
use super::key_dispatch::{self, DetectedBranch, KeyDispatchConfig};
use crate::backend::types::DelimScanConfig;

/// Pre-solved key-dispatch detection for an Alt node.
pub type KeyDispatchMatch = (KeyDispatchConfig, Vec<DetectedBranch>, Option<usize>);

/// Build the delim-scan configuration cache. Walks the IR looking
/// for Wrap patterns (`Skip(Next(open, middle), close)` or
/// `Next(open, Skip(middle, close))`) and runs `delim_scan::try_detect`
/// on each candidate. Results are keyed by the Wrap root's stable
/// `NodeId` (the outer `Skip` / `Next` node).
pub fn solve_delim_scan_configs(ir: &GrammarIR) -> HashMap<NodeId, DelimScanConfig> {
    let mut out = HashMap::new();
    let Some(dag) = ir.dag.as_ref() else {
        return out;
    };
    for rule in &ir.rules {
        walk_delim_scan(&rule.body, ir, dag, &mut out);
    }
    out
}

fn walk_delim_scan(
    node: &IrNode,
    ir: &GrammarIR,
    dag: &bbnf_ir::dag::GrammarDag,
    out: &mut HashMap<NodeId, DelimScanConfig>,
) {
    // Recognize wrap patterns.
    if let Some((open, middle, close)) = unwrap_wrap(node) {
        if let Some(config) = delim_scan::try_detect(open, middle, close, ir) {
            if let Some(nid) = dag.node_for(node) {
                out.insert(nid, config);
            }
        }
    }

    // Recurse into children.
    match node {
        IrNode::Seq(children) => {
            for c in children {
                walk_delim_scan(c, ir, dag, out);
            }
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                walk_delim_scan(&b.node, ir, dag, out);
            }
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            walk_delim_scan(a, ir, dag, out);
            walk_delim_scan(b, ir, dag, out);
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            walk_delim_scan(inner, ir, dag, out);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            walk_delim_scan(token, ir, dag, out);
            for arm in arms {
                walk_delim_scan(&arm.continuation, ir, dag, out);
            }
            walk_delim_scan(fallback, ir, dag, out);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

/// Build the key-dispatch configuration cache. Walks the IR and
/// runs `key_dispatch::try_detect` on every Alt node. Results are
/// keyed by the Alt's stable `NodeId`.
pub fn solve_key_dispatch_configs(ir: &GrammarIR) -> HashMap<NodeId, KeyDispatchMatch> {
    let mut out = HashMap::new();
    let Some(dag) = ir.dag.as_ref() else {
        return out;
    };
    for rule in &ir.rules {
        walk_key_dispatch(&rule.body, ir, dag, &mut out);
    }
    out
}

fn walk_key_dispatch(
    node: &IrNode,
    ir: &GrammarIR,
    dag: &bbnf_ir::dag::GrammarDag,
    out: &mut HashMap<NodeId, KeyDispatchMatch>,
) {
    if let IrNode::Alt(branches, _) = node {
        if let Some(result) = key_dispatch::try_detect(branches, ir) {
            if let Some(nid) = dag.node_for(node) {
                out.insert(nid, result);
            }
        }
    }

    match node {
        IrNode::Seq(children) => {
            for c in children {
                walk_key_dispatch(c, ir, dag, out);
            }
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                walk_key_dispatch(&b.node, ir, dag, out);
            }
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            walk_key_dispatch(a, ir, dag, out);
            walk_key_dispatch(b, ir, dag, out);
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            walk_key_dispatch(inner, ir, dag, out);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            walk_key_dispatch(token, ir, dag, out);
            for arm in arms {
                walk_key_dispatch(&arm.continuation, ir, dag, out);
            }
            walk_key_dispatch(fallback, ir, dag, out);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

/// Extract `(open, middle, close)` from a node that matches the
/// Wrap pattern. Mirrors the dispatcher branching in
/// `driver/node.rs`: `Skip(Next(open, middle), close)` or
/// `Next(open, Skip(middle, close))`.
fn unwrap_wrap(node: &IrNode) -> Option<(&IrNode, &IrNode, &IrNode)> {
    match node {
        IrNode::Skip(left, right) => {
            if let IrNode::Next(open, middle) = left.as_ref() {
                return Some((open, middle, right));
            }
            None
        }
        IrNode::Next(left, right) => {
            if let IrNode::Skip(middle, close) = right.as_ref() {
                return Some((left, middle, close));
            }
            None
        }
        _ => None,
    }
}
