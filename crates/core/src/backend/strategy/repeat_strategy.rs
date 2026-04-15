//! Repeat strategy selection.
//!
//! Repeat nodes have several execution shapes — optional (0..1), generic
//! many-loop, sep-by (when wrapped in a Skip with an optional separator),
//! and delimited sep-by (sep-by inside a Wrap with a known terminator).

/// Resolved strategy for a Repeat node.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RepeatStrategy {
    /// `?` — zero or one occurrence.
    Optional,
    /// `*` or `+` — generic many-loop.
    Many,
    /// Sep-by: `element (sep element)*` detected via a Skip wrapper.
    SepBy,
    /// Sep-by inside a Wrap (`{ items }`) with a known terminator byte.
    DelimitedSepBy,
}

/// Classify a Repeat by its `lo/hi` bounds.
///
/// Sep-by and DelimitedSepBy are detected at the enclosing Skip/Wrap
/// level, not here — this classification only covers the intrinsic
/// Repeat shape.
pub fn classify_repeat(_lo: u32, hi: u32) -> RepeatStrategy {
    if hi <= 1 {
        RepeatStrategy::Optional
    } else {
        RepeatStrategy::Many
    }
}
