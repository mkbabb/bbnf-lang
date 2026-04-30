//! Parse and dispatcher error surfaces for generated grammars.
//!
//! `DtaError` is the internal rejection type returned by generated
//! dispatchers. `ParseErr` is the public parse-entry type. Both carry
//! only parse-position facts; DTA state/rule provenance is IR/compiler
//! data and does not belong in the runtime surface.

/// Internal error returned by generated dispatcher functions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DtaError {
    /// The dispatcher could not match at `offset`.
    Syntax {
        /// Byte offset where the match attempt failed.
        offset: u32,
    },
    /// Parse needed another byte but reached EOF.
    UnexpectedEnd {
        /// Byte offset where the dispatcher terminated.
        offset: u32,
    },
    /// Generated dispatch requested a state outside the valid range.
    InvalidState {
        /// Out-of-range generated state identifier.
        state: u32,
    },
}

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
}

impl std::fmt::Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErr::Syntax { offset, rule } => match rule {
                Some(r) => write!(f, "parse error at byte {} (rule #{})", offset, r),
                None => write!(f, "parse error at byte {}", offset),
            },
        }
    }
}

impl std::error::Error for ParseErr {}
