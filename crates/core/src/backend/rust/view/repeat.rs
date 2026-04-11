//! View emission for `MustTape` Repeat rules.
//!
//! A Repeat rule's view exposes `.iter()` — an iterator over child
//! cursors — and the convenience `.len()`. Each yielded child is a
//! cursor the caller walks with the same universal accessor set.
//!
//! Full implementation lands in AC.2.
