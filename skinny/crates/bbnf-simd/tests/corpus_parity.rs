use bbnf_simd::{scan_json_structurals, scan_scalar, JSON_STRUCTURAL};

#[test]
fn available_json_corpora_match_scalar() {
    for fixture in test_fixtures::load_available_bench_fixtures().unwrap() {
        let scalar = scan_scalar(&fixture.bytes, &JSON_STRUCTURAL);
        let simd = scan_json_structurals(&fixture.bytes);
        assert_eq!(simd.positions(), scalar.positions(), "{}", fixture.name);
    }
}
