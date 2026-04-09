//! Shared recognizer fact types.

use bbnf_regex::{CharSet128, EngineSet, RegexClass};

/// What kind of recognizer an implementor represents.
///
/// These are *architectural roles*, not family classifications. The
/// regex-domain family taxonomy lives in `bbnf_regex::RegexClass` and is
/// surfaced via `RecognizerInfo::regex_class()`.
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

    /// Tranche V: regex-domain family classification, if applicable.
    ///
    /// Literals, tokens, and dispatch groups return `None`; only the
    /// `RegexRecognizer` wrapper returns `Some(class)`. This is the
    /// single touch point that lets every consumer reach family
    /// information through the trait without breaking the leaf-crate
    /// boundary — `RegexClass` lives in `bbnf-regex` and is referenced
    /// here, not duplicated.
    fn regex_class(&self) -> Option<&RegexClass> {
        None
    }

    /// Tranche V: bitset of engines that can drive this recognizer.
    ///
    /// Monotone derivation from the underlying facts. Used as the legal
    /// domain for the per-pattern `RegexEngine` variable in the
    /// recognizer-tier CSP. Defaults to a generic family-helper bit so
    /// that recognizers without a feasibility analysis still have a
    /// nonempty domain.
    fn feasible_engines(&self) -> EngineSet {
        EngineSet::FAMILY_HELPER
    }
}
