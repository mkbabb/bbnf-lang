//! AZ-IV.W3.7 — CSS L4 `parse_with` lazy-error-elision contract +
//! happy-path coverage. See
//! [`parse_with_json`](super::parse_with_json) for the row layout.

use bbnf::grammar::generated::css_l4::CssL4Parser;
use bbnf::path::ir::{OwnedPathSegment, TypedPath};
use bbnf::path::markers::CssL4;
use bbnf::runtime::css_l4::parse_with;
use bbnf::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};

#[test]
fn happy_path_parity_against_eager() {
    // [Index(0), Index(0)] → first rule's first decl property.
    let src = "a { color: red; }";
    let path: TypedPath<CssL4, &str> =
        TypedPath::from_owned(vec![OwnedPathSegment::Index(0), OwnedPathSegment::Index(0)]);

    let lazy = parse_with::<&str>(src, &path);
    let doc = CssL4Parser::parse(src).expect("eager CSS parse");
    let legacy = [LegacySegment::Index(0), LegacySegment::Index(0)];
    let eager = doc.get::<&str>(LegacyPath::new(&legacy));

    assert_eq!(lazy, eager, "lazy + eager same Option<&str> semantics");
}

#[test]
fn lazy_error_elision_after_path_reach() {
    // First rule resolves cleanly; trailing bytes contain a
    // deliberate parse error (`@@@`) past the path's reach. Lazy
    // returns the resolved leaf; eager fails because of the trailing
    // garbage.
    let malformed = "a { color: red; } @@@ malformed past path's reach @@@";
    let path: TypedPath<CssL4, &str> =
        TypedPath::from_owned(vec![OwnedPathSegment::Index(0), OwnedPathSegment::Index(0)]);

    let lazy = parse_with::<&str>(malformed, &path);
    let eager_doc = CssL4Parser::parse(malformed);

    assert!(
        eager_doc.is_err(),
        "eager parse must fail on trailing garbage"
    );
    assert!(
        lazy.is_some(),
        "lazy must elide parse errors past the path's reach"
    );
}

#[test]
fn out_of_bounds_path_returns_none() {
    let src = "a { color: red; }";
    let path: TypedPath<CssL4, &str> = TypedPath::from_owned(vec![
        OwnedPathSegment::Index(99),
        OwnedPathSegment::Index(0),
    ]);

    let lazy = parse_with::<&str>(src, &path);
    let doc = CssL4Parser::parse(src).expect("eager CSS parse");
    let legacy = [LegacySegment::Index(99), LegacySegment::Index(0)];
    let eager = doc.get::<&str>(LegacyPath::new(&legacy));

    assert!(lazy.is_none());
    assert_eq!(lazy, eager);
}
