//! RecognizerInfo unification tests.

use bbnf_ir::recognizer::{
    LiteralRecognizer, RecognizerInfo, RecognizerKind, RegexRecognizer,
};
use bbnf_regex::RegexInfo;

#[test]
fn literal_recognizer_reports_width() {
    let rec = LiteralRecognizer { bytes: b"hello" };
    assert_eq!(rec.kind(), RecognizerKind::Literal);
    let w = rec.width();
    assert_eq!(w.min, 5);
    assert_eq!(w.max, Some(5));
    assert!(!rec.nullable());
    assert!(rec.must_consume());
    assert_eq!(rec.accel_candidate(), Some(b'h'));
    assert!(rec.scanable());
}

#[test]
fn literal_recognizer_empty() {
    let rec = LiteralRecognizer { bytes: b"" };
    assert!(rec.nullable());
    assert!(!rec.must_consume());
    let w = rec.width();
    assert_eq!(w.min, 0);
    assert_eq!(w.max, Some(0));
    assert!(rec.literal_prefix().is_none());
}

#[test]
fn regex_recognizer_wraps_regex_info() {
    let info = RegexInfo::analyze("hello").expect("parse");
    let rec = RegexRecognizer { info: &info };
    assert_eq!(rec.kind(), RecognizerKind::Regex);
    assert!(!rec.nullable());
    assert_eq!(rec.literal_prefix(), Some(b"hello".as_slice()));
    // The accel byte should match the regex's precomputed candidate.
    assert_eq!(rec.accel_candidate(), Some(b'h'));
}

#[test]
fn regex_recognizer_nullable_pattern() {
    let info = RegexInfo::analyze("[a-z]*").expect("parse");
    let rec = RegexRecognizer { info: &info };
    assert!(rec.nullable());
    assert!(!rec.must_consume());
    let w = rec.width();
    assert_eq!(w.min, 0);
    assert_eq!(w.max, None);
}
