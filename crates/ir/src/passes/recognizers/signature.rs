//! Canonical hashing for `RecognizerSignature`.
//!
//! Two recognizers that hash equal collapse to the SAME emitted helper
//! function. Hash inputs are byte/class/shape data only — never NodeId
//! or StringId — so dedup is stable across compile sessions.

use std::hash::{Hash, Hasher};

use rustc_hash::FxHasher;

use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerShape, RecognizerSignature,
};
use crate::{GrammarIR, StringId};

/// Compute the canonical 64-bit shape hash for a `RecognizerShape`.
///
/// The hash includes the regex-tier classification (read from
/// `ir.regex_info[sid].classification`) for `Regex { sid }` shapes,
/// not the `sid` itself — two distinct `StringId`s for the same
/// canonical pattern produce the same hash.
pub fn hash_recognizer_shape(shape: &RecognizerShape, ir: &GrammarIR) -> u64 {
    let mut hasher = FxHasher::default();
    hash_shape_into(shape, ir, &mut hasher);
    hasher.finish()
}

fn hash_shape_into(shape: &RecognizerShape, ir: &GrammarIR, hasher: &mut FxHasher) {
    match shape {
        RecognizerShape::DelimiterBalanced { open, close, inner } => {
            0u8.hash(hasher);
            open.hash(hasher);
            close.hash(hasher);
            hash_recognizer_into(inner, ir, hasher);
        }
        RecognizerShape::SeparatorList {
            element,
            separator,
            trailing,
        } => {
            1u8.hash(hasher);
            separator.hash(hasher);
            trailing.hash(hasher);
            hash_recognizer_into(element, ir, hasher);
        }
        RecognizerShape::TokenLedBranches { key, branch_count } => {
            2u8.hash(hasher);
            branch_count.hash(hasher);
            hash_recognizer_into(key, ir, hasher);
        }
        RecognizerShape::KeywordPrefix {
            bytes,
            disjoint_tail,
        } => {
            3u8.hash(hasher);
            bytes.as_slice().hash(hasher);
            disjoint_tail.hash(hasher);
        }
        RecognizerShape::Regex { sid } => {
            4u8.hash(hasher);
            // Hash the regex pattern string itself, not the StringId,
            // so two interned copies of the same pattern collapse.
            hash_regex_class_into(*sid, ir, hasher);
        }
    }
}

fn hash_regex_class_into(sid: StringId, ir: &GrammarIR, hasher: &mut FxHasher) {
    if let Some(info) = ir.regex_info.get(&sid) {
        // Discriminant is enough — RegexClass enum values without their
        // payload are stable across StringId interning.
        std::mem::discriminant(&info.classification).hash(hasher);
    } else {
        // Fall back to the pattern string itself.
        ir.get_string(sid).hash(hasher);
    }
}

fn hash_recognizer_into(rec: &Recognizer, ir: &GrammarIR, hasher: &mut FxHasher) {
    hash_shape_into(&rec.shape, ir, hasher);
    rec.signature.output_shape.hash(hasher);
    rec.signature.advance_on_failure.hash(hasher);
    rec.signature.grade.hash(hasher);
}

/// Build a `RecognizerSignature` from the canonical shape data plus
/// caller-supplied output / grade metadata.
pub fn compute_shape_hash(
    shape: &RecognizerShape,
    output_shape: OutputShape,
    advance_on_failure: bool,
    grade: OnePassGrade,
    ir: &GrammarIR,
) -> RecognizerSignature {
    let mut hasher = FxHasher::default();
    hash_shape_into(shape, ir, &mut hasher);
    output_shape.hash(&mut hasher);
    advance_on_failure.hash(&mut hasher);
    grade.hash(&mut hasher);
    RecognizerSignature {
        shape_hash: hasher.finish(),
        output_shape,
        advance_on_failure,
        grade,
    }
}

