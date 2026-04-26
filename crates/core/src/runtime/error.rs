//! `ParseErr` — the public parse-error type returned by generated
//! `Grammar::parse(input)` functions.
//!
//! Tranche AC.1. Carries either a syntactic failure (input byte
//! offset + optional rule label) or a tape-construction failure
//! (propagated from the tape builder's sticky error state).
//!
//! The error is intentionally minimal — a richer diagnostic surface
//! layered on top can walk the rule stack and the source map, but
//! the baseline public type stays small so it's cheap to return
//! and cheap to pattern-match.

use tape::TapeBuildError;

/// Error returned by the generated `parse` entry point.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErr {
    /// The parser rejected the input. `offset` is the byte at which
    /// the furthest failing rule stopped; `rule` is the optional
    /// label of that rule (codegen-assigned `u32` identifying the
    /// rule by its interned name, not the public string — keeps
    /// the error `Copy`-friendly).
    Syntax {
        /// Byte offset in the input where parsing halted.
        offset: u32,
        /// Optional rule-label id for diagnostics. `None` when
        /// the failure site is an anonymous inline sub-expression.
        rule: Option<u32>,
    },

    /// The tape builder's sticky error fired during construction.
    /// Only possible when the generated parser calls
    /// [`tape::FusedBuilder::set_error`] from a recovery path
    /// — otherwise the builder never surfaces an error.
    Tape(TapeBuildError),
}

impl std::fmt::Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErr::Syntax { offset, rule } => match rule {
                Some(r) => write!(f, "parse error at byte {} (rule #{})", offset, r),
                None => write!(f, "parse error at byte {}", offset),
            },
            ParseErr::Tape(e) => write!(f, "tape build error: {:?}", e),
        }
    }
}

impl std::error::Error for ParseErr {}

impl From<TapeBuildError> for ParseErr {
    fn from(e: TapeBuildError) -> Self {
        ParseErr::Tape(e)
    }
}
