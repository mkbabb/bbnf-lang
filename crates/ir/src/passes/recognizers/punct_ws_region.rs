//! Structural punctuation+whitespace region recognizer (Tranche X.11b,
//! AQ.4 deoverfit).
//!
//! Detects single-byte literals wrapped in optional whitespace that
//! also act as branch separators in some `AltDispatch` table elsewhere
//! in the grammar — i.e., bytes the grammar itself treats as cluster
//! separators. The canonical shapes are:
//!
//! - `OptionalWhitespace(Literal(p))` where `p` is a single byte that
//!   appears in the grammar's `AltDispatch` byte vocabulary — `p` with
//!   ws around it.
//! - `Skip(OptionalWhitespace(Literal(p)), _)` / `Next(_,
//!   OptionalWhitespace(Literal(p)))` — `ws p ws` embedded inside a
//!   larger sequence.
//!
//! The detected shape lands as `RecognizerShape::PunctWsRegion {
//! puncts }`. The backend `kernels::punct_ws_region` module emits a
//! single SIMD-friendly scanner that consumes the surrounding
//! whitespace + punctuation byte in one pass.
//!
//! ## Byte-set derivation
//!
//! The set of "structural" bytes is derived per-grammar by walking
//! every `IrNode::Alt` and collecting the bytes for which an
//! `AltDispatch.table` entry is `< 255` (i.e., bytes that route to a
//! concrete branch). This set is the grammar's own self-described
//! cluster-separator alphabet — no hardcoded `b",:{}[]"`.

use std::collections::HashSet;
use std::sync::OnceLock;

use smallvec::SmallVec;

use crate::dag::NodeId;
use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};
use crate::{GrammarIR, IrNode};

use super::signature::compute_shape_hash;
use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

/// Recognizer for ws-padded single-byte structural punctuation.
///
/// Caches the per-grammar `AltDispatch` byte vocabulary across the
/// recognizer-mining walk. The cache is keyed only by `*const
/// GrammarIR` so re-instantiating the miner produces a fresh cache;
/// the substrate constructs miners per `mine_recognizers` call so
/// this is sound for the lifetime of one walk.
#[derive(Default)]
pub struct PunctWsRegionMiner {
    dispatch_bytes: OnceLock<HashSet<u8>>,
}

impl PunctWsRegionMiner {
    pub fn new() -> Self {
        Self::default()
    }

    fn punct_set(&self, ir: &GrammarIR) -> &HashSet<u8> {
        self.dispatch_bytes
            .get_or_init(|| collect_dispatch_byte_vocabulary(ir))
    }
}

impl RecognizerMiner for PunctWsRegionMiner {
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        let punct_set = self.punct_set(ctx.ir);
        let Some(puncts) = try_match_punct_ws_region(node, ctx.ir, punct_set) else {
            return;
        };
        let shape = RecognizerShape::PunctWsRegion {
            puncts: SmallVec::from_slice(&puncts),
        };
        let signature = compute_shape_hash(
            &shape,
            OutputShape::SpanOnly,
            false,
            OnePassGrade::OnePass,
            ctx.ir,
        );
        outputs.recognizers.push((
            node_id,
            Recognizer {
                role: RecognizerRole::Standalone,
                shape,
                signature,
            },
        ));
    }
}

/// Walk every rule body and collect the bytes that appear as entries
/// in any `AltDispatch.table`. These are the bytes the grammar's own
/// dispatch pass treats as branch keys — the natural definition of
/// the grammar's "structural" byte alphabet.
fn collect_dispatch_byte_vocabulary(ir: &GrammarIR) -> HashSet<u8> {
    let mut set = HashSet::new();
    for rule in &ir.rules {
        collect_dispatch_bytes(&rule.body, &mut set);
    }
    set
}

fn collect_dispatch_bytes(node: &IrNode, set: &mut HashSet<u8>) {
    if let IrNode::Alt(_, Some(dispatch)) = node {
        for (byte_idx, &branch_idx) in dispatch.table.iter().enumerate() {
            if branch_idx != 255 && byte_idx < 128 {
                set.insert(byte_idx as u8);
            }
        }
    }
    node.for_each_child(&mut |child| collect_dispatch_bytes(child, set));
}

/// Try to extract the cluster of punctuation bytes from a whitespace-
/// padded structural punctuation site.
///
/// Recognized forms:
/// 1. `OptionalWhitespace(Literal(p))` → `[p]`
/// 2. `Skip(OptionalWhitespace(Literal(p)), _)` → `[p]`
/// 3. `Next(_, OptionalWhitespace(Literal(p)))` → `[p]`
///
/// The literal byte `p` must be a member of `punct_set` — the set of
/// bytes the grammar's `AltDispatch` tables treat as branch keys.
fn try_match_punct_ws_region(
    node: &IrNode,
    ir: &GrammarIR,
    punct_set: &HashSet<u8>,
) -> Option<Vec<u8>> {
    let lit = match node {
        IrNode::OptionalWhitespace(inner) => extract_single_byte_literal(inner, ir)?,
        IrNode::Skip(lhs, _) => match lhs.as_ref() {
            IrNode::OptionalWhitespace(inner) => extract_single_byte_literal(inner, ir)?,
            _ => return None,
        },
        IrNode::Next(_, rhs) => match rhs.as_ref() {
            IrNode::OptionalWhitespace(inner) => extract_single_byte_literal(inner, ir)?,
            _ => return None,
        },
        _ => return None,
    };
    if !punct_set.contains(&lit) {
        return None;
    }
    Some(vec![lit])
}

/// Extract a single-byte literal from a node, handling the bare
/// `Literal(sid)` shape plus the one-element `Seq` / trivial wrappers
/// the recognizer mining will see.
fn extract_single_byte_literal(node: &IrNode, ir: &GrammarIR) -> Option<u8> {
    let sid = match node {
        IrNode::Literal(sid) => *sid,
        IrNode::Seq(children) if children.len() == 1 => {
            if let IrNode::Literal(sid) = &children[0] {
                *sid
            } else {
                return None;
            }
        }
        _ => return None,
    };
    let bytes = ir.get_string(sid).as_bytes();
    if bytes.len() != 1 {
        return None;
    }
    Some(bytes[0])
}
