//! Wrap strategy selection.
//!
//! Wrap nodes (`open >> body << close`) have three execution shapes:
//! generic descent, delim-scan acceleration (flat scanner with a known
//! close byte), and delimited sep-by (when the body is a sep-by loop
//! inside the wrap).

/// Resolved strategy for a Wrap node.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WrapStrategy {
    /// Generic: recurse into the body between open/close markers.
    Generic,
    /// Delim-scan: a flat scanner driven by the close byte (used when
    /// the body's FIRST set is disjoint from `close`).
    DelimScan,
    /// Delimited sep-by: the body is a sep-by loop with a terminator byte.
    DelimitedSepBy,
}
