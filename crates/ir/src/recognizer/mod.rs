//! Unified recognizer abstraction.
//!
//! Regexes, literals, token rules, dispatch groups, and delim-scan wrappers
//! all share "recognizer-like" properties — they match a prefix of the input
//! and either succeed (consuming some bytes) or fail. The `RecognizerInfo`
//! trait captures this shared vocabulary so strategy solvers and cost
//! models can reason uniformly regardless of the underlying machinery.
//!
//! # Implementors
//!
//! - `RegexInfo` (from `bbnf_regex`) via the `RegexRecognizer` wrapper
//! - `Literal` nodes via `LiteralRecognizer`
//! - Token rules via `TokenRecognizer`
//! - Pre-computed `AltDispatch` groups via `DispatchGroupRecognizer`
//! - Delim-scan wrappers via `DelimScanRecognizer`
//!
//! Tranche V completes the migration: every consumer (strategy solvers,
//! cost models, scanner planner) reads `RecognizerInfo` directly instead
//! of querying `RegexInfo` / `RegexClass` / `IrNode` shapes.

mod facts;
mod plans;

pub use facts::{RecognizerInfo, RecognizerKind, Width};
pub use plans::{ExecutionPlan, ExecutionPlanKind};

use bbnf_regex::{CharSet128, EngineSet, RegexClass, RegexInfo};

use crate::AltDispatch;

/// Wrap a `RegexInfo` as a `RecognizerInfo`.
pub struct RegexRecognizer<'a> {
    pub info: &'a RegexInfo,
}

impl<'a> RecognizerInfo for RegexRecognizer<'a> {
    fn kind(&self) -> RecognizerKind {
        RecognizerKind::Regex
    }

    fn first_bytes(&self) -> Option<CharSet128> {
        Some(self.info.first_chars.clone())
    }

    fn nullable(&self) -> bool {
        self.info.nullable
    }

    fn must_consume(&self) -> bool {
        self.info.must_consume
    }

    fn width(&self) -> Width {
        Width {
            min: self.info.min_match_len,
            max: self.info.max_match_len,
        }
    }

    fn literal_prefix(&self) -> Option<&[u8]> {
        self.info.literal_prefix.as_deref()
    }

    fn literal_suffix(&self) -> Option<&[u8]> {
        self.info.literal_suffix.as_deref()
    }

    fn accel_candidate(&self) -> Option<u8> {
        self.info.accel_candidate
    }

    fn scanable(&self) -> bool {
        self.info.one_pass_eligible
    }

    fn regex_class(&self) -> Option<&RegexClass> {
        Some(&self.info.classification)
    }

    fn feasible_engines(&self) -> EngineSet {
        self.info.feasible_engines
    }
}

/// Wrap a literal string as a `RecognizerInfo`.
pub struct LiteralRecognizer<'a> {
    pub bytes: &'a [u8],
}

impl<'a> RecognizerInfo for LiteralRecognizer<'a> {
    fn kind(&self) -> RecognizerKind {
        RecognizerKind::Literal
    }

    fn first_bytes(&self) -> Option<CharSet128> {
        let mut cs = CharSet128::new();
        if let Some(&b) = self.bytes.first() {
            if b < 128 {
                cs.add(b);
            }
        }
        Some(cs)
    }

    fn nullable(&self) -> bool {
        self.bytes.is_empty()
    }

    fn must_consume(&self) -> bool {
        !self.bytes.is_empty()
    }

    fn width(&self) -> Width {
        Width {
            min: self.bytes.len(),
            max: Some(self.bytes.len()),
        }
    }

    fn literal_prefix(&self) -> Option<&[u8]> {
        if self.bytes.is_empty() {
            None
        } else {
            Some(self.bytes)
        }
    }

    fn literal_suffix(&self) -> Option<&[u8]> {
        self.literal_prefix()
    }

    fn accel_candidate(&self) -> Option<u8> {
        self.bytes.first().copied()
    }

    fn scanable(&self) -> bool {
        true
    }

    fn feasible_engines(&self) -> EngineSet {
        // A literal is always memchr1-eligible (its first byte) and is
        // a family kernel (the literal-memcmp helper).
        EngineSet::MEMCHR1 | EngineSet::FAMILY_HELPER
    }
}

/// Wrap a `@token`-marked rule body as a `RecognizerInfo`.
///
/// Tranche V: tokens are architectural recognizers — their identity is
/// the rule, not the body shape. The `inner` recognizer (typically a
/// `LiteralRecognizer` or `RegexRecognizer`) carries the actual byte
/// matching semantics; this wrapper just lifts the kind to
/// `RecognizerKind::Token` so role-aware consumers can distinguish.
pub struct TokenRecognizer<'a> {
    pub rule_id: crate::RuleId,
    pub inner: &'a dyn RecognizerInfo,
}

impl<'a> RecognizerInfo for TokenRecognizer<'a> {
    fn kind(&self) -> RecognizerKind {
        RecognizerKind::Token
    }
    fn first_bytes(&self) -> Option<CharSet128> {
        self.inner.first_bytes()
    }
    fn nullable(&self) -> bool {
        self.inner.nullable()
    }
    fn must_consume(&self) -> bool {
        self.inner.must_consume()
    }
    fn width(&self) -> Width {
        self.inner.width()
    }
    fn literal_prefix(&self) -> Option<&[u8]> {
        self.inner.literal_prefix()
    }
    fn literal_suffix(&self) -> Option<&[u8]> {
        self.inner.literal_suffix()
    }
    fn accel_candidate(&self) -> Option<u8> {
        self.inner.accel_candidate()
    }
    fn scanable(&self) -> bool {
        self.inner.scanable()
    }
    fn regex_class(&self) -> Option<&RegexClass> {
        self.inner.regex_class()
    }
    fn feasible_engines(&self) -> EngineSet {
        self.inner.feasible_engines()
    }
}

/// Wrap a pre-computed `AltDispatch` table as a `RecognizerInfo`.
///
/// Tranche V: a dispatch group recognizer's first-byte set is the union
/// of its arms' first bytes (already computed during dispatch table
/// generation). It is always single-byte-deterministic, never nullable,
/// and consumes exactly one byte to discriminate.
pub struct DispatchGroupRecognizer<'a> {
    pub dispatch: &'a AltDispatch,
}

impl<'a> RecognizerInfo for DispatchGroupRecognizer<'a> {
    fn kind(&self) -> RecognizerKind {
        RecognizerKind::DispatchGroup
    }

    fn first_bytes(&self) -> Option<CharSet128> {
        let mut cs = CharSet128::new();
        // table[byte] = branch index, or 255 for no match.
        for (byte_idx, &slot) in self.dispatch.table.iter().enumerate() {
            if slot != 255 && byte_idx < 128 {
                cs.add(byte_idx as u8);
            }
        }
        Some(cs)
    }

    fn nullable(&self) -> bool {
        false
    }

    fn must_consume(&self) -> bool {
        true
    }

    fn width(&self) -> Width {
        Width { min: 1, max: None }
    }

    fn accel_candidate(&self) -> Option<u8> {
        // No single accel byte: dispatch is by first-byte switch.
        None
    }

    fn scanable(&self) -> bool {
        true
    }

    fn feasible_engines(&self) -> EngineSet {
        // Dispatch groups always reach via the family-helper kernel
        // (which is the byte-switch dispatch helper itself).
        EngineSet::FAMILY_HELPER | EngineSet::ONE_PASS
    }
}

/// Wrap a delim-scan-eligible Wrap node as a `RecognizerInfo`.
///
/// Tranche V: delim-scan wrappers are characterized by an open byte, a
/// close byte, and the inner body's recognizer. They are always
/// scanable (the wrap kernel performs a forward scan to the matching
/// close) and consume at least 2 bytes.
pub struct DelimScanRecognizer<'a> {
    pub open: u8,
    pub close: u8,
    pub inner: Option<&'a dyn RecognizerInfo>,
}

impl<'a> RecognizerInfo for DelimScanRecognizer<'a> {
    fn kind(&self) -> RecognizerKind {
        RecognizerKind::DelimScan
    }

    fn first_bytes(&self) -> Option<CharSet128> {
        let mut cs = CharSet128::new();
        if self.open < 128 {
            cs.add(self.open);
        }
        Some(cs)
    }

    fn nullable(&self) -> bool {
        false
    }

    fn must_consume(&self) -> bool {
        true
    }

    fn width(&self) -> Width {
        Width { min: 2, max: None }
    }

    fn literal_prefix(&self) -> Option<&[u8]> {
        // We don't carry the byte slice as a static reference; consumers
        // read `self.open` directly when emitting the kernel.
        None
    }

    fn accel_candidate(&self) -> Option<u8> {
        Some(self.close)
    }

    fn scanable(&self) -> bool {
        true
    }

    fn feasible_engines(&self) -> EngineSet {
        EngineSet::FAMILY_HELPER | EngineSet::MEMCHR1
    }
}
