use bbnf_simd::{scan_json_structurals, scan_scalar, JSON_STRUCTURAL};

#[test]
fn all_one_byte_inputs_match_scalar() {
    for byte in 0u8..=255 {
        let input = [byte];
        let scalar = scan_scalar(&input, &JSON_STRUCTURAL);
        let simd = scan_json_structurals(&input);
        assert_eq!(simd.positions(), scalar.positions(), "byte {byte}");
    }
}

#[test]
fn escaped_string_structurals_match_scalar() {
    let input = br#"{"x":"{\"a\":[1,2]}","y":true}"#;
    let scalar = scan_scalar(input, &JSON_STRUCTURAL);
    let simd = scan_json_structurals(input);
    assert_eq!(simd.positions(), scalar.positions());
}

#[test]
fn parse_index_reports_string_prefilter_columns() {
    let index = bbnf_simd::scan_json_parse_index(br#"{"x":"a\nb"}"#);
    let (offsets, escapes, controls) = index.into_parts();
    assert!(!offsets.is_empty());
    assert_eq!(escapes, vec![7]);
    assert!(controls.is_empty());
}
