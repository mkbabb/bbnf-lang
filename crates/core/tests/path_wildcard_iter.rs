//! AZ-IV.W2.5 — fixtures for the wildcard lazy-iter surface.
//!
//! Covers four shapes:
//!
//! 1. A 100-element source iterates lazily through `WildcardIter`
//!    without allocating an intermediate `Vec<_>`.
//! 2. `.with_anchors()` pairs every yielded item with the path prefix.
//! 3. `.collect::<Vec<_>>()` is a call-site choice; the wildcard
//!    machinery itself never materialises a list.
//! 4. The depth cap fires when the configured limit is exceeded; the
//!    overflow diagnostic carries the offending prefix.

use bbnf::path::error::PathErrorReason;
use bbnf::path::{
    DEFAULT_WILDCARD_DEPTH_CAP, Path, PathSegment, WildcardConfig, WildcardIter, ends_with_wildcard,
};

#[test]
fn wildcard_iter_yields_every_element_lazily() {
    // 100-element source — lazy iteration without an intermediate Vec.
    let source = 0u32..100;
    let segments: [PathSegment<'_>; 2] = [PathSegment::Field("items"), PathSegment::Wildcard];
    let prefix = Path::new(&segments);
    let iter = WildcardIter::new(source, prefix, WildcardConfig::new(), 0);

    let mut count = 0u32;
    let mut sum = 0u64;
    for v in iter {
        sum += u64::from(v);
        count += 1;
    }
    assert_eq!(count, 100);
    assert_eq!(sum, (0u64..100).sum::<u64>());
}

#[test]
fn wildcard_iter_size_hint_passes_through() {
    let source = 0u32..50;
    let segments: [PathSegment<'_>; 1] = [PathSegment::Wildcard];
    let prefix = Path::new(&segments);
    let iter = WildcardIter::new(source, prefix, WildcardConfig::new(), 0);
    assert_eq!(iter.size_hint(), (50, Some(50)));
}

#[test]
fn with_anchors_pairs_every_item_with_prefix() {
    let source = vec!["alpha", "beta", "gamma"].into_iter();
    let segments: [PathSegment<'_>; 2] = [PathSegment::Field("users"), PathSegment::Wildcard];
    let prefix = Path::new(&segments);
    let iter = WildcardIter::new(source, prefix, WildcardConfig::new(), 0);

    let anchored: Vec<(Path<'_>, &str)> = iter.with_anchors().collect();
    assert_eq!(anchored.len(), 3);
    for (path, _value) in &anchored {
        assert_eq!(path.len(), 2);
        assert!(matches!(path.as_slice()[0], PathSegment::Field("users")));
        assert!(matches!(path.as_slice()[1], PathSegment::Wildcard));
    }
    let values: Vec<&str> = anchored.iter().map(|(_, v)| *v).collect();
    assert_eq!(values, vec!["alpha", "beta", "gamma"]);
}

#[test]
fn collect_is_call_site_choice() {
    // The wildcard iterator is just an Iterator; .collect() is the
    // standard one. No structural recursion inside the wildcard
    // surface — the consumer materialises whatever it wants.
    let source = 0u32..10;
    let segments: [PathSegment<'_>; 1] = [PathSegment::Wildcard];
    let prefix = Path::new(&segments);
    let iter = WildcardIter::new(source, prefix, WildcardConfig::new(), 0);

    let collected: Vec<u32> = iter.collect();
    assert_eq!(collected, (0u32..10).collect::<Vec<_>>());
}

#[test]
fn depth_cap_default_fires_on_overflow() {
    // depth = cap means the next .next() short-circuits to None, and
    // overflow_error() carries the prefix-anchored diagnostic.
    let source = 0u32..5;
    let segments: [PathSegment<'_>; 3] = [
        PathSegment::Field("a"),
        PathSegment::Wildcard,
        PathSegment::Wildcard,
    ];
    let prefix = Path::new(&segments);
    let mut iter = WildcardIter::new(
        source,
        prefix,
        WildcardConfig::new(),
        DEFAULT_WILDCARD_DEPTH_CAP,
    );

    assert!(iter.would_overflow());
    assert_eq!(iter.next(), None);
    assert_eq!(iter.size_hint(), (0, Some(0)));

    let err = iter.overflow_error();
    assert_eq!(err.reason, PathErrorReason::WildcardOverflow);
    assert_eq!(err.struct_name, "Wildcard");
}

#[test]
fn depth_cap_zero_disables_expansion() {
    let source = 0u32..5;
    let segments: [PathSegment<'_>; 1] = [PathSegment::Wildcard];
    let prefix = Path::new(&segments);
    let mut iter = WildcardIter::new(source, prefix, WildcardConfig::new().with_depth_cap(0), 0);

    // depth 0 already meets the cap of 0 — overflow fires immediately.
    assert!(iter.would_overflow());
    assert_eq!(iter.next(), None);
}

#[test]
fn ends_with_wildcard_helper_routes_correctly() {
    let segs_yes: [PathSegment<'_>; 2] = [PathSegment::Field("items"), PathSegment::Wildcard];
    let segs_no: [PathSegment<'_>; 2] = [PathSegment::Field("items"), PathSegment::Index(0)];
    assert!(ends_with_wildcard(Path::new(&segs_yes)));
    assert!(!ends_with_wildcard(Path::new(&segs_no)));
}
