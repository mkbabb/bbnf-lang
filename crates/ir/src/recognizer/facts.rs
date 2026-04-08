//! Shared recognizer fact types.

use bbnf_regex::CharSet128;

/// What kind of recognizer an implementor represents.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RecognizerKind {
    /// A literal byte string.
    Literal,
    /// A regex pattern (wrapping `bbnf_regex::RegexInfo`).
    Regex,
    /// A token rule (grammar rule with `@token` directive).
    Token,
    /// A dispatch group (pre-computed AltDispatch over multiple branches).
    DispatchGroup,
    /// A delim-scan wrapper.
    DelimScan,
}

/// Width bounds: `[min, max]` bytes consumed on a successful match.
#[derive(Clone, Copy, Debug, Default)]
pub struct Width {
    pub min: usize,
    /// `None` indicates unbounded (e.g., `.*`).
    pub max: Option<usize>,
}

/// The shared recognizer interface. Regexes, literals, token rules, and
/// dispatch groups all implement this so strategy solvers can reason
/// uniformly about their first-set, width, acceleration candidate, etc.
pub trait RecognizerInfo {
    /// What kind of recognizer is this?
    fn kind(&self) -> RecognizerKind;

    /// Possible first bytes. `None` means unknown / unbounded.
    fn first_bytes(&self) -> Option<CharSet128>;

    /// Whether the recognizer accepts the empty string.
    fn nullable(&self) -> bool;

    /// Whether the recognizer always advances input on success.
    fn must_consume(&self) -> bool;

    /// Width bounds in bytes.
    fn width(&self) -> Width;

    /// Fixed byte prefix, if any (enables memcmp fast path).
    fn literal_prefix(&self) -> Option<&[u8]> {
        None
    }

    /// Fixed byte suffix, if any.
    fn literal_suffix(&self) -> Option<&[u8]> {
        None
    }

    /// A single discriminating byte suitable for `memchr` acceleration.
    fn accel_candidate(&self) -> Option<u8> {
        None
    }

    /// Whether this recognizer can be scanned in a single forward pass
    /// (no backtracking).
    fn scanable(&self) -> bool {
        false
    }
}
