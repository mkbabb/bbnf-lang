//! AZ-IV.W3.7 — BBNF `parse_with` lazy-error-elision contract +
//! happy-path coverage. See
//! [`parse_with_json`](super::parse_with_json) for the row layout.

use bbnf::grammar::generated::bbnf::BbnfBootstrap;
use bbnf::path::ir::{OwnedPathSegment, TypedPath};
use bbnf::path::markers::Bbnf;
use bbnf::runtime::bbnf::parse_with;
use bbnf::runtime::bbnf::value::BbnfValue;

#[test]
fn happy_path_resolves_root_value() {
    // Empty path → root document. Lazy resolves to `BbnfValue` at
    // identity; eager parses + projects through the document.
    let src = "a = b ;\n";
    let path: TypedPath<Bbnf, BbnfValue<'_>> = TypedPath::from_owned(Vec::new());

    let lazy = parse_with::<BbnfValue<'_>>(src, &path);
    let eager = BbnfBootstrap::parse(src);

    assert!(lazy.is_some(), "BBNF root path resolves through lazy lane");
    assert!(eager.is_ok(), "eager parse succeeds on well-formed input");
}

#[test]
fn lazy_error_elision_after_path_reach() {
    // Single-rule grammar parses cleanly; trailing `@@@` past the
    // path's reach is a parse error. Lazy returns the root identity;
    // eager surfaces the parse error.
    let malformed = "a = b ;\n@@@ not bbnf @@@";
    let path: TypedPath<Bbnf, BbnfValue<'_>> = TypedPath::from_owned(Vec::new());

    let lazy = parse_with::<BbnfValue<'_>>(malformed, &path);
    let eager = BbnfBootstrap::parse(malformed);

    assert!(
        eager.is_err(),
        "eager parse must fail on trailing garbage past the rule"
    );
    assert!(
        lazy.is_some(),
        "lazy must elide parse errors past the path's reach"
    );
}

#[test]
fn invalid_input_returns_none() {
    let path: TypedPath<Bbnf, BbnfValue<'_>> = TypedPath::from_owned(Vec::new());
    let out = parse_with::<BbnfValue<'_>>("@@@ not bbnf @@@", &path);
    assert!(out.is_none());
}
