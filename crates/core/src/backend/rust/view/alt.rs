//! View emission for `MustTape` Alt rules.
//!
//! An Alt rule's view exposes `.variant_idx()` (which branch was
//! chosen) and `.chosen()` (a cursor at the single child record
//! the chosen branch pushed). Heterogeneous Alts with sub-variant
//! coercion reuse the same cursor-level surface — the
//! `variant_idx` encodes the coerced discriminator.
//!
//! Full implementation lands in AC.2.
