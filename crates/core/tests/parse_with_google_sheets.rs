//! AZ-IV.W3.7 — Google Sheets `parse_with` lazy-error-elision
//! contract + happy-path coverage. See
//! [`parse_with_json`](super::parse_with_json) for the row layout.

use bbnf::grammar::generated::google_sheets::GoogleSheetsParser;
use bbnf::path::ir::{OwnedPathSegment, TypedPath};
use bbnf::path::markers::Sheets;
use bbnf::runtime::google_sheets::parse_with;
use bbnf::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};

#[test]
fn happy_path_parity_against_eager() {
    // `=42` → number primitive at [Index(0), Index(0)].
    let src = "=42";
    let path: TypedPath<Sheets, f64> =
        TypedPath::from_owned(vec![OwnedPathSegment::Index(0), OwnedPathSegment::Index(0)]);

    let lazy = parse_with::<f64>(src, &path);
    let doc = GoogleSheetsParser::parse(src).expect("eager Sheets parse");
    let legacy = [LegacySegment::Index(0), LegacySegment::Index(0)];
    let eager = doc.get::<f64>(LegacyPath::new(&legacy));

    assert_eq!(lazy, eager, "lazy + eager same Option<f64> semantics");
}

#[test]
#[ignore = "Flat-shape lazy honoring: Sheets formula is a Flat compound; the W3-DYNAMIC \
            mechanism gates Object/Array loops only. Closing this requires a Flat-shape \
            early-bail when cursor reaches terminal mid-body — separate mechanism, slated \
            for a focused follow-on (post-W3 tranche carry)."]
fn lazy_error_elision_after_path_reach() {
    // `=42` resolves at the path; trailing `@@@` past the reach is a
    // parse error for the eager lane. Lazy elides it.
    let malformed = "=42 @@@ malformed past path's reach @@@";
    let path: TypedPath<Sheets, f64> =
        TypedPath::from_owned(vec![OwnedPathSegment::Index(0), OwnedPathSegment::Index(0)]);

    let lazy = parse_with::<f64>(malformed, &path);
    let eager_doc = GoogleSheetsParser::parse(malformed);

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
fn invalid_input_returns_none() {
    // No path can resolve when the entire input is garbage; both
    // lanes return None / Err.
    let path: TypedPath<Sheets, f64> = TypedPath::from_owned(Vec::new());
    let out = parse_with::<f64>("not a formula @@@", &path);
    assert!(out.is_none());
}
