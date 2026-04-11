//! View emission for `TapeSpanOnly` (single-leaf) rules.
//!
//! A `TapeSpanOnly` rule's view exposes `.span()` and `.is_recovered()`
//! only — there are no children to walk, no variant discriminator,
//! and no compound header. The emitted struct wraps a
//! `TapeCursor<'tape>` pointing at the single leaf record the rule
//! pushed.
//!
//! Full implementation lands in AC.2.

// Kept deliberately sparse — populated by the atomic AC.2 emitter
// rewrite. The module exists so `view/mod.rs` can declare it without
// needing to touch the file-by-file table again when the generator
// lands.
