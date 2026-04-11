//! Top-level `<Grammar>View<'tape>` discriminator enum emission.
//!
//! The grammar view is a tagged union whose variants are the
//! per-rule views emitted by `leaves.rs`, `seq.rs`, `alt.rs`, and
//! `repeat.rs`. This module also emits the `impl
//! ::bbnf::runtime::Root for <Grammar>` binding that ties the
//! grammar marker struct's `type View<'tape>` GAT to this enum —
//! the surface `Parsed<Grammar>::view(&self)` lends from.
//!
//! Full implementation lands in AC.2.
