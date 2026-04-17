//! AW-III.W5.a — `kernel_shape` selector pass tests.
//!
//! Verifies that `select_kernel_strategy` mechanically maps mined
//! `StructuralAlphabet` cardinality into the correct
//! `(KernelShape, has_digraphs, has_quote_parity)` triple per the
//! architectural-lever boundaries declared in
//! `docs/tranches/AW/AW-III.md` §W5.

use std::collections::BTreeSet;

use bbnf_ir::passes::recognizers::kernel_shape::{
    KernelShape, KernelStrategy, select_kernel_strategy,
};
use bbnf_ir::passes::sets::StructuralAlphabet;

// ── Fixture builders ─────────────────────────────────────────────────────

/// Build an alphabet with `n` synthetic singleton bytes (`b'a' .. b'a'+n`).
fn alphabet_with_n_singletons(n: usize) -> StructuralAlphabet {
    let mut alphabet = StructuralAlphabet::default();
    for i in 0..n {
        alphabet.single_bytes.insert(b'a' + i as u8);
    }
    alphabet
}

fn alphabet_with_digraphs() -> StructuralAlphabet {
    let mut alphabet = alphabet_with_n_singletons(4);
    alphabet.single_bytes.insert(b'/');
    alphabet.digraphs.push((b'/', b'*'));
    alphabet.digraphs.push((b'*', b'/'));
    alphabet
}

fn alphabet_with_quote_classes() -> StructuralAlphabet {
    let mut alphabet = alphabet_with_n_singletons(4);
    let mut q = BTreeSet::new();
    q.insert(b'"');
    alphabet.quote_classes = q;
    alphabet
}

// ── Singleton-shape selection tests ──────────────────────────────────────

#[test]
fn nibble_lut_chosen_for_small_singleton_set() {
    for n in 1..=8 {
        let alphabet = alphabet_with_n_singletons(n);
        let strategy = select_kernel_strategy(&alphabet);
        assert_eq!(
            strategy.singleton_kernel,
            KernelShape::NibbleLut,
            "n={n}: small singleton sets should pick NibbleLut"
        );
    }
}

#[test]
fn wide_lut_chosen_for_medium_singleton_set() {
    for n in 9..=16 {
        let alphabet = alphabet_with_n_singletons(n);
        let strategy = select_kernel_strategy(&alphabet);
        assert_eq!(
            strategy.singleton_kernel,
            KernelShape::WideLut,
            "n={n}: medium singleton sets should pick WideLut"
        );
    }
}

#[test]
fn multipass_cmpeq_chosen_for_large_singleton_set() {
    for n in 17..=32 {
        let alphabet = alphabet_with_n_singletons(n);
        let strategy = select_kernel_strategy(&alphabet);
        assert_eq!(
            strategy.singleton_kernel,
            KernelShape::MultipassCmpEq,
            "n={n}: large singleton sets should pick MultipassCmpEq"
        );
    }
}

#[test]
fn boundary_eight_is_nibble_lut() {
    let alphabet = alphabet_with_n_singletons(8);
    let strategy = select_kernel_strategy(&alphabet);
    assert_eq!(strategy.singleton_kernel, KernelShape::NibbleLut);
}

#[test]
fn boundary_nine_is_wide_lut() {
    let alphabet = alphabet_with_n_singletons(9);
    let strategy = select_kernel_strategy(&alphabet);
    assert_eq!(strategy.singleton_kernel, KernelShape::WideLut);
}

#[test]
fn boundary_sixteen_is_wide_lut() {
    let alphabet = alphabet_with_n_singletons(16);
    let strategy = select_kernel_strategy(&alphabet);
    assert_eq!(strategy.singleton_kernel, KernelShape::WideLut);
}

#[test]
fn boundary_seventeen_is_multipass_cmpeq() {
    let alphabet = alphabet_with_n_singletons(17);
    let strategy = select_kernel_strategy(&alphabet);
    assert_eq!(strategy.singleton_kernel, KernelShape::MultipassCmpEq);
}

// ── Lever flag tests ─────────────────────────────────────────────────────

#[test]
fn has_digraphs_reflects_alphabet_digraphs() {
    let no_digraphs = alphabet_with_n_singletons(4);
    let with_digraphs = alphabet_with_digraphs();

    let strategy_no = select_kernel_strategy(&no_digraphs);
    let strategy_with = select_kernel_strategy(&with_digraphs);

    assert!(!strategy_no.has_digraphs);
    assert!(strategy_with.has_digraphs);
}

#[test]
fn has_quote_parity_reflects_alphabet_quote_classes() {
    let no_quotes = alphabet_with_n_singletons(4);
    let with_quotes = alphabet_with_quote_classes();

    let strategy_no = select_kernel_strategy(&no_quotes);
    let strategy_with = select_kernel_strategy(&with_quotes);

    assert!(!strategy_no.has_quote_parity);
    assert!(strategy_with.has_quote_parity);
}

#[test]
fn full_strategy_combines_all_levers() {
    let mut alphabet = alphabet_with_digraphs();
    let mut q = BTreeSet::new();
    q.insert(b'"');
    alphabet.quote_classes = q;
    let strategy = select_kernel_strategy(&alphabet);

    assert_eq!(strategy.singleton_kernel, KernelShape::NibbleLut);
    assert!(strategy.has_digraphs);
    assert!(strategy.has_quote_parity);
}

#[test]
fn empty_strategy_constant_is_safe_default() {
    let strategy = KernelStrategy::EMPTY;
    assert_eq!(strategy.singleton_kernel, KernelShape::NibbleLut);
    assert!(!strategy.has_digraphs);
    assert!(!strategy.has_quote_parity);
}

#[test]
fn for_singleton_count_matches_thresholds() {
    assert_eq!(KernelShape::for_singleton_count(0), KernelShape::NibbleLut);
    assert_eq!(KernelShape::for_singleton_count(1), KernelShape::NibbleLut);
    assert_eq!(
        KernelShape::for_singleton_count(KernelShape::NIBBLE_LUT_MAX),
        KernelShape::NibbleLut
    );
    assert_eq!(
        KernelShape::for_singleton_count(KernelShape::NIBBLE_LUT_MAX + 1),
        KernelShape::WideLut
    );
    assert_eq!(
        KernelShape::for_singleton_count(KernelShape::WIDE_LUT_MAX),
        KernelShape::WideLut
    );
    assert_eq!(
        KernelShape::for_singleton_count(KernelShape::WIDE_LUT_MAX + 1),
        KernelShape::MultipassCmpEq
    );
    assert_eq!(
        KernelShape::for_singleton_count(256),
        KernelShape::MultipassCmpEq
    );
}
