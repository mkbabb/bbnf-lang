use bbnf_simd::{scan_dispatch, scan_scalar, StructuralAlphabet};

const TEST_ALPHABET: StructuralAlphabet = StructuralAlphabet::from_bytes(b"{}[],:\"");

#[test]
fn available_corpora_match_generic_scalar() {
    for fixture in test_fixtures::load_available_bench_fixtures().unwrap() {
        let scalar = scan_scalar(&fixture.bytes, &TEST_ALPHABET);
        let dispatched = scan_dispatch(&fixture.bytes, &TEST_ALPHABET);
        assert_eq!(
            dispatched.positions(),
            scalar.positions(),
            "{}",
            fixture.name
        );
    }
}
