//! AZ-IV.W3.7 — JSON `parse_with` lazy-error-elision contract +
//! happy-path coverage.
//!
//! Three rows per grammar per W3.7's scope:
//!
//! 1. Happy-path parity against eager. `parse_with(input, &path)` must
//!    equal `parse(input)?.get(legacy_path)` for every fixture where
//!    the eager parse succeeds.
//! 2. Negative-fixture lazy-error-elision. With bytes after the path's
//!    reach mangled into a parse error, the lazy lane returns
//!    `Some(leaf)` (the path resolved before the malformation was
//!    visited); the eager lane returns a parse error. By construction:
//!    bytes the cursor causes the dispatcher to skip never reach a
//!    parse error path.
//! 3. Out-of-bounds path. A path that walks past the document
//!    structure resolves to `None` in both lanes.

use bbnf::grammar::generated::json::JsonParser;
use bbnf::path::ir::{OwnedPathSegment, TypedPath};
use bbnf::path::markers::Json;
use bbnf::runtime::json::parse_with;
use bbnf::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};

#[test]
fn happy_path_parity_against_eager() {
    // `path = ["title"]` → string leaf "hi". Both lanes must agree.
    let src = r#"{"title":"hi","count":42}"#;
    let path: TypedPath<Json, &str> =
        TypedPath::from_owned(vec![OwnedPathSegment::Field("title".to_owned())]);

    let lazy = parse_with::<&str>(src, &path);
    let doc = JsonParser::parse(src).expect("eager JSON parse");
    let legacy = [LegacySegment::Field("title")];
    let eager = doc.get::<&str>(LegacyPath::new(&legacy));

    assert_eq!(lazy, Some("hi"));
    assert_eq!(lazy, eager, "lazy + eager same Option<&str> semantics");
}

#[test]
fn lazy_error_elision_after_path_reach() {
    // Path resolves at the FIRST field; bytes AFTER the path's reach
    // contain a deliberate parse error (`@@@`). Lazy returns the
    // resolved leaf; eager fails.
    let malformed = r#"{"title":"hi", @@@ malformed past path's reach @@@"#;
    let path: TypedPath<Json, &str> =
        TypedPath::from_owned(vec![OwnedPathSegment::Field("title".to_owned())]);

    let lazy = parse_with::<&str>(malformed, &path);
    let eager_doc = JsonParser::parse(malformed);

    assert!(
        eager_doc.is_err(),
        "eager parse must fail on malformed input"
    );
    assert_eq!(
        lazy,
        Some("hi"),
        "lazy must elide parse errors past the path's reach"
    );
}

#[test]
fn out_of_bounds_path_returns_none() {
    // Both lanes return `None` when the path walks past the
    // document's structure.
    let src = r#"{"title":"hi"}"#;
    let path: TypedPath<Json, &str> =
        TypedPath::from_owned(vec![OwnedPathSegment::Field("absent".to_owned())]);

    let lazy = parse_with::<&str>(src, &path);
    let doc = JsonParser::parse(src).expect("eager JSON parse");
    let legacy = [LegacySegment::Field("absent")];
    let eager = doc.get::<&str>(LegacyPath::new(&legacy));

    assert!(lazy.is_none());
    assert_eq!(lazy, eager);
}
