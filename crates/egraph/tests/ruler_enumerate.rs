//! Contract tests for [`egraph::ruler::enumerate`].
//!
//! Verifies the CVC enumerator emits the expected combinatorial set of
//! patterns under the boolean DSL fixture, with deterministic seeded
//! ordering and faithful depth/size cap enforcement.

mod common;

use common::{BoolAlphabet, Tag};
use egraph::ruler::{EnumerateConfig, Pattern, enumerate};

use crate::common::Bool;

#[test]
fn depth_one_emits_only_leaves() {
    let cfg = EnumerateConfig {
        max_depth: 1,
        max_size: 16,
        seed: 0,
    };
    let alpha = BoolAlphabet;
    let patterns: Vec<Pattern<Bool>> = enumerate::<_, Bool>(&cfg, &alpha).collect();

    assert_eq!(patterns.len(), 2, "two arity-0 variants expected");
    assert!(patterns.iter().all(|p| p.depth() == 1));
    assert!(patterns.iter().any(|p| p.tag == Tag::True));
    assert!(patterns.iter().any(|p| p.tag == Tag::False));
}

#[test]
fn depth_two_emits_unary_and_binary_combinations() {
    let cfg = EnumerateConfig {
        max_depth: 2,
        max_size: 16,
        seed: 0,
    };
    let alpha = BoolAlphabet;
    let patterns: Vec<Pattern<Bool>> = enumerate::<_, Bool>(&cfg, &alpha).collect();

    // Depth 1: True, False (2)
    // Depth 2: Not(T), Not(F)            (2)
    //          And(T,T), And(T,F),
    //          And(F,T), And(F,F)        (4)
    // Total: 8.
    assert_eq!(patterns.len(), 8);
    let depth_2 = patterns.iter().filter(|p| p.depth() == 2).count();
    assert_eq!(depth_2, 6);
}

#[test]
fn size_cap_clips_oversize_candidates() {
    let cfg = EnumerateConfig {
        max_depth: 3,
        max_size: 3,
        seed: 0,
    };
    let alpha = BoolAlphabet;
    let patterns: Vec<Pattern<Bool>> = enumerate::<_, Bool>(&cfg, &alpha).collect();

    assert!(patterns.iter().all(|p| p.size() <= 3));
    assert!(patterns.iter().any(|p| p.depth() == 2));
}

#[test]
fn enumeration_is_deterministic_under_same_seed() {
    let cfg = EnumerateConfig {
        max_depth: 2,
        max_size: 16,
        seed: 7,
    };
    let alpha = BoolAlphabet;
    let a: Vec<_> = enumerate::<_, Bool>(&cfg, &alpha)
        .map(|p| format!("{:?}", p))
        .collect();
    let b: Vec<_> = enumerate::<_, Bool>(&cfg, &alpha)
        .map(|p| format!("{:?}", p))
        .collect();
    assert_eq!(a, b);
}

#[test]
fn enumeration_count_grows_with_depth() {
    let alpha = BoolAlphabet;
    let n1 = enumerate::<_, Bool>(
        &EnumerateConfig {
            max_depth: 1,
            max_size: 64,
            seed: 0,
        },
        &alpha,
    )
    .count();
    let n2 = enumerate::<_, Bool>(
        &EnumerateConfig {
            max_depth: 2,
            max_size: 64,
            seed: 0,
        },
        &alpha,
    )
    .count();
    let n3 = enumerate::<_, Bool>(
        &EnumerateConfig {
            max_depth: 3,
            max_size: 64,
            seed: 0,
        },
        &alpha,
    )
    .count();
    assert!(n1 < n2);
    assert!(n2 < n3);
}
