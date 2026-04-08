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
//! - `RegexInfo` (from `bbnf_regex`) via the `Recognizer` wrapper impl below
//! - `Literal` nodes via `LiteralRecognizer`
//! - Future: token rules, dispatch groups, delim-scan wrappers
//!
//! # Phase 8 scope
//!
//! The trait + `LiteralRecognizer` + `RegexRecognizer` wrappers are in place.
//! Consumers (strategy solvers, cost models) migrate to reading
//! `RecognizerInfo` incrementally in follow-up work.

mod facts;
mod plans;

pub use facts::{RecognizerInfo, RecognizerKind, Width};
pub use plans::{ExecutionPlan, ExecutionPlanKind};

use bbnf_regex::{CharSet128, RegexInfo};

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
}
