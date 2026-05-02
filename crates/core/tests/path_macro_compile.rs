//! AZ-IV.W2.4 — positive compile fixtures for the `path!` proc-macro.
//!
//! Confirms `path!(GrammarMarker, ...)` resolves at compile time
//! against the macro crate's per-grammar fixture
//! [`bbnf_path::registry`] and emits a [`bbnf::path::TypedPath`]
//! literal with the expected segment shape.
//!
//! Coverage:
//!
//! 1. `path!(Json, "statuses", 0, "text")` resolves to
//!    `TypedPath<Json, ()>` with three owned segments. This is the
//!    canonical hard-gate path from `docs/tranches/AZ-IV/waves/W2.md`
//!    §AZ-IV.W2.4.
//! 2. A compound string literal (`"items.0"`) lowers through the W2.3
//!    path lexer into separate `Field` + `Index` segments.
//! 3. The `*` token form lands a wildcard segment.
//! 4. The CssL4 marker resolves through its fixture entry rule.
//! 5. The Bbnf marker resolves through its fixture entry rule.

use bbnf::path::{Json, OwnedPathSegment, TypedPath};
use bbnf_path::path;

#[test]
fn json_statuses_zero_text_compiles_to_typed_path() {
    let p: TypedPath<Json, ()> = path!(Json, "statuses", 0, "text");
    assert_eq!(p.len(), 3);
    let owned = p.owned_segments();
    assert!(matches!(&owned[0], OwnedPathSegment::Field(s) if s == "statuses"));
    assert!(matches!(&owned[1], OwnedPathSegment::Index(0)));
    assert!(matches!(&owned[2], OwnedPathSegment::Field(s) if s == "text"));
}

#[test]
fn compound_string_literal_lowers_through_path_lexer() {
    // The path-lexer turns `"statuses.0"` into Field, Index segments;
    // the macro re-folds into the `OwnedSegment` sequence.
    let p: TypedPath<Json, ()> = path!(Json, "statuses.0.text");
    assert_eq!(p.len(), 3);
    let owned = p.owned_segments();
    assert!(matches!(&owned[0], OwnedPathSegment::Field(s) if s == "statuses"));
    assert!(matches!(&owned[1], OwnedPathSegment::Index(0)));
    assert!(matches!(&owned[2], OwnedPathSegment::Field(s) if s == "text"));
}

#[test]
fn bracket_index_in_string_literal_lowers_correctly() {
    let p: TypedPath<Json, ()> = path!(Json, "statuses[0].text");
    assert_eq!(p.len(), 3);
    let owned = p.owned_segments();
    assert!(matches!(&owned[1], OwnedPathSegment::Index(0)));
}

#[test]
fn wildcard_token_lands_wildcard_segment() {
    let p: TypedPath<Json, ()> = path!(Json, "statuses", *, "text");
    assert_eq!(p.len(), 3);
    assert!(matches!(p.owned_segments()[1], OwnedPathSegment::Wildcard));
}

#[test]
fn css_l4_marker_resolves_through_fixture() {
    use bbnf::path::CssL4;
    let p: TypedPath<CssL4, ()> = path!(CssL4, "rules", 0, "declarations", 0, "value");
    assert_eq!(p.len(), 5);
    assert!(matches!(&p.owned_segments()[0], OwnedPathSegment::Field(s) if s == "rules"));
    assert!(matches!(&p.owned_segments()[4], OwnedPathSegment::Field(s) if s == "value"));
}

#[test]
fn bbnf_marker_resolves_through_fixture() {
    use bbnf::path::Bbnf;
    let p: TypedPath<Bbnf, ()> = path!(Bbnf, "rules", 0, "name");
    assert_eq!(p.len(), 3);
    assert!(matches!(&p.owned_segments()[2], OwnedPathSegment::Field(s) if s == "name"));
}

#[test]
fn fully_qualified_marker_path_compiles() {
    // The grammar argument may be a Rust path of any depth; the macro
    // reads the trailing identifier as the fixture key and passes the
    // full path through to the emitted `TypedPath::<...>`.
    let p: TypedPath<bbnf::path::Json, ()> = path!(bbnf::path::Json, "statuses", 0, "text");
    assert_eq!(p.len(), 3);
}
