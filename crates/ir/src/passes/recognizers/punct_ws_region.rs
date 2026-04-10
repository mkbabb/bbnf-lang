//! JSON / dictionary-shape punctuation+whitespace region recognizer
//! (Tranche X.11b).
//!
//! Detects structural punctuation bytes (`, : { } [ ]`) wrapped in
//! optional whitespace. The canonical shapes are:
//!
//! - `OptionalWhitespace(Literal(p))` where `p` is a single structural
//!   byte — `p` with ws around it.
//! - `Skip(OptionalWhitespace(Literal(p)), _)` / `Next(_,
//!   OptionalWhitespace(Literal(p)))` — `ws p ws` embedded inside a
//!   larger sequence.
//!
//! The detected shape lands as `RecognizerShape::PunctWsRegion {
//! puncts }`. The backend `kernels::punct_ws_region` module emits a
//! single SIMD-friendly scanner that consumes the surrounding
//! whitespace + punctuation byte in one pass.

use smallvec::SmallVec;

use crate::dag::NodeId;
use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};
use crate::{GrammarIR, IrNode};

use super::signature::compute_shape_hash;
use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

/// Canonical JSON / dictionary structural punctuation bytes.
const STRUCTURAL_PUNCTS: &[u8] = b",:{}[]";

pub struct PunctWsRegionMiner;

impl RecognizerMiner for PunctWsRegionMiner {
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        let Some(puncts) = try_match_punct_ws_region(node, ctx.ir) else {
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

/// Try to extract the cluster of punctuation bytes from a whitespace-
/// padded structural punctuation site.
///
/// Recognized forms:
/// 1. `OptionalWhitespace(Literal(p))` → `[p]`
/// 2. `Skip(OptionalWhitespace(Literal(p)), _)` → `[p]`
/// 3. `Next(_, OptionalWhitespace(Literal(p)))` → `[p]`
fn try_match_punct_ws_region(node: &IrNode, ir: &GrammarIR) -> Option<Vec<u8>> {
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
    if !STRUCTURAL_PUNCTS.contains(&lit) {
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
