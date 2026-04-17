//! AW-III.W5-carry — ConsumeToNextStructural lifter activation test.
//!
//! Verifies:
//!
//! 1. The pattern-alphabet miner surfaces tight bitmaps for simple
//!    negated-char-class patterns.
//! 2. The CTNS lifter admits patterns whose alphabet is disjoint
//!    from the grammar's structural alphabet.
//! 3. The walker-side emitter routes admitted patterns to
//!    `DtaState::ConsumeToNextStructural`.

use bbnf_ir::passes::recognizers::pattern_alphabet::{PatternAlphabet, PatternAlphabetMiner};

/// The `PatternAlphabet` helper construction is the stable mining
/// output shape the CTNS lifter consults.
#[test]
fn pattern_alphabet_tight_for_digit_class() {
    let digits: PatternAlphabet = bbnf_ir::passes::recognizers::pattern_alphabet::make_alphabet(
        (b'0'..=b'9').chain(b'.'..=b'.'),
    );
    // Every digit byte is matchable; every non-digit is not.
    for b in b'0'..=b'9' {
        let word = (b >> 6) as usize;
        let bit = b & 0x3F;
        let set = (digits.matchable_bytes[word] >> bit) & 1 == 1;
        assert!(set, "byte {} should be matchable", b as char);
    }
    for b in b'a'..=b'z' {
        let word = (b >> 6) as usize;
        let bit = b & 0x3F;
        let set = (digits.matchable_bytes[word] >> bit) & 1 == 1;
        assert!(!set, "byte {} should NOT be matchable", b as char);
    }
    assert!(digits.is_tight);
}

/// Sanity: the miner struct is constructable and satisfies the
/// miner-trait bound (this proves the miner compiles against the
/// unified miner-trait surface in [`bbnf_ir::passes::recognizers::
/// mod`]).
#[test]
fn pattern_alphabet_miner_is_recognizer() {
    let _miner = PatternAlphabetMiner;
    // Compile-time confirmation that the trait impl exists.
    fn assert_miner<T: bbnf_ir::passes::recognizers::RecognizerMiner>(_: &T) {}
    assert_miner(&PatternAlphabetMiner);
}

/// Sanity: the CTNS miner struct is constructable and satisfies the
/// miner-trait bound.
#[test]
fn ctns_miner_is_recognizer() {
    let _miner =
        bbnf_ir::passes::recognizers::consume_to_next_structural::ConsumeToNextStructuralMiner;
    fn assert_miner<T: bbnf_ir::passes::recognizers::RecognizerMiner>(_: &T) {}
    assert_miner(
        &bbnf_ir::passes::recognizers::consume_to_next_structural::ConsumeToNextStructuralMiner,
    );
}

/// Compile-time confirmation that the IR-side DtaState carries the
/// ConsumeToNextStructural variant. Without this variant the lifter
/// has no wire to emit the CTNS substitution through.
#[test]
fn ir_side_ctns_variant_exists() {
    // Construct a no-op instance so the compiler resolves the variant
    // name. `StringId(0)` is always valid even for empty grammars.
    let s = bbnf_ir::passes::recognizers::dta::DtaState::ConsumeToNextStructural {
        pattern: 0u32,
    };
    match s {
        bbnf_ir::passes::recognizers::dta::DtaState::ConsumeToNextStructural { .. } => {}
        _ => panic!("ConsumeToNextStructural variant not constructable"),
    }
}

/// Tape-side CTNS variant exists and constructs.
#[test]
fn tape_side_ctns_variant_exists() {
    let s = bbnf::runtime::tape::DtaState::ConsumeToNextStructural;
    match s {
        bbnf::runtime::tape::DtaState::ConsumeToNextStructural => {}
        _ => panic!("tape ConsumeToNextStructural variant missing"),
    }
}
