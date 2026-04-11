//! View emission for `MustTape` Seq rules.
//!
//! A Seq rule's view wraps a compound `TapeCursor<'tape>` whose
//! children are the individually-pushed sub-records. Accessors
//! are positional (`.child(0)`, `.child(1)`, …) during the AC.2
//! universal-cursor baseline; rule-specific typed accessors
//! (e.g. `PairView::key() -> StringView`) are a post-AC tranche
//! over the stable view substrate.
//!
//! Full implementation lands in AC.2.
