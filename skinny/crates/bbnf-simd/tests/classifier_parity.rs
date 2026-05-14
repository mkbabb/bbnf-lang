use bbnf_simd::{scan_dispatch, scan_scalar, StructuralAlphabet};

const TEST_ALPHABET: StructuralAlphabet = StructuralAlphabet::from_bytes(b"{}[],:\"");

#[test]
fn all_one_byte_inputs_match_scalar() {
    for byte in 0u8..=255 {
        let input = [byte];
        let scalar = scan_scalar(&input, &TEST_ALPHABET);
        let dispatched = scan_dispatch(&input, &TEST_ALPHABET);
        assert_eq!(dispatched.positions(), scalar.positions(), "byte {byte}");
    }
}

#[test]
fn generic_structural_dispatch_matches_scalar() {
    let input = br#"{"x":"{\"a\":[1,2]}","y":true}"#;
    let scalar = scan_scalar(input, &TEST_ALPHABET);
    let dispatched = scan_dispatch(input, &TEST_ALPHABET);
    assert_eq!(dispatched.positions(), scalar.positions());
}
