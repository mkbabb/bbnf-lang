//! Correctness gate: every kernel must produce identical
//! `StructuralIndex` on the canonical corpus. Scalar path is the
//! reference; per-arch paths must match byte-for-byte.

use simd_scan::{StructuralIndex, alphabet::StructuralAlphabet, scalar, scan_structural};

const JSON_ALPHABET: StructuralAlphabet = StructuralAlphabet::from_parts(
    &[b'{', b'}', b'[', b']', b':', b',', b'"', b'\\'],
    &[],
    &[b'"'],
);

const JSON_NO_QUOTES: StructuralAlphabet =
    StructuralAlphabet::from_parts(&[b'{', b'}', b'[', b']', b':', b','], &[], &[]);

const CSS_ALPHABET: StructuralAlphabet = StructuralAlphabet::from_parts(
    &[b'{', b'}', b'(', b')', b';', b':', b','],
    &[(b'/', b'*'), (b'*', b'/')],
    &[b'"', b'\''],
);

fn read_corpus(name: &str) -> Vec<u8> {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    let path = root.join(name);
    std::fs::read(&path).unwrap_or_else(|e| panic!("failed to read {}: {}", path.display(), e))
}

fn assert_indices_equal(a: &StructuralIndex, b: &StructuralIndex, ctx: &str) {
    assert_eq!(
        a.positions.len(),
        b.positions.len(),
        "{ctx}: position count mismatch (scalar={}, simd={})",
        a.positions.len(),
        b.positions.len()
    );
    for (i, (&p, &k)) in a.positions.iter().zip(a.kinds.iter()).enumerate() {
        assert_eq!(
            (p, k),
            (b.positions[i], b.kinds[i]),
            "{ctx}: divergence at index {i}",
        );
    }
}

#[test]
fn empty_alphabet_yields_empty_index() {
    let input = b"abcdefghijklmnopqrstuvwxyz";
    let alphabet = StructuralAlphabet::EMPTY;
    let scalar_idx = scalar::scan(input, &alphabet);
    let simd_idx = scan_structural(input, &alphabet);
    assert!(scalar_idx.is_empty());
    assert!(simd_idx.is_empty());
}

#[test]
fn empty_input_yields_empty_index() {
    let scalar_idx = scalar::scan(&[], &JSON_ALPHABET);
    let simd_idx = scan_structural(&[], &JSON_ALPHABET);
    assert!(scalar_idx.is_empty());
    assert!(simd_idx.is_empty());
}

#[test]
fn short_input_below_stripe() {
    let input = b"{\"a\":1}";
    let scalar_idx = scalar::scan(input, &JSON_ALPHABET);
    let simd_idx = scan_structural(input, &JSON_ALPHABET);
    assert_indices_equal(&scalar_idx, &simd_idx, "short_input");
}

#[test]
fn no_quotes_simple() {
    let input = b"{a,b,c:[1,2,3]}";
    let scalar_idx = scalar::scan(input, &JSON_NO_QUOTES);
    let simd_idx = scan_structural(input, &JSON_NO_QUOTES);
    assert_indices_equal(&scalar_idx, &simd_idx, "no_quotes_simple");
    // Spot-check positions: { , , , : [ , , ] }
    let expected: Vec<u32> = vec![0, 2, 4, 6, 7, 9, 11, 13, 14];
    assert_eq!(simd_idx.positions, expected);
}

#[test]
fn json_twitter_canonical() {
    let input = read_corpus("data/json/twitter.json");
    let scalar_idx = scalar::scan(&input, &JSON_ALPHABET);
    let simd_idx = scan_structural(&input, &JSON_ALPHABET);
    assert_indices_equal(&scalar_idx, &simd_idx, "twitter.json");
    assert!(scalar_idx.len() > 0, "twitter must have structural bytes");
}

#[test]
fn json_citm_canonical() {
    let input = read_corpus("data/json/citm_catalog.json");
    let scalar_idx = scalar::scan(&input, &JSON_ALPHABET);
    let simd_idx = scan_structural(&input, &JSON_ALPHABET);
    assert_indices_equal(&scalar_idx, &simd_idx, "citm_catalog.json");
}

#[test]
fn json_canada_canonical() {
    let input = read_corpus("data/json/canada.json");
    let scalar_idx = scalar::scan(&input, &JSON_ALPHABET);
    let simd_idx = scan_structural(&input, &JSON_ALPHABET);
    assert_indices_equal(&scalar_idx, &simd_idx, "canada.json");
}

#[test]
fn css_bootstrap_canonical() {
    let input = read_corpus("data/css/bootstrap.css");
    let scalar_idx = scalar::scan(&input, &CSS_ALPHABET);
    let simd_idx = scan_structural(&input, &CSS_ALPHABET);
    assert_indices_equal(&scalar_idx, &simd_idx, "bootstrap.css");
}

#[test]
fn css_tailwind_canonical() {
    let input = read_corpus("data/css/tailwind.css");
    let scalar_idx = scalar::scan(&input, &CSS_ALPHABET);
    let simd_idx = scan_structural(&input, &CSS_ALPHABET);
    assert_indices_equal(&scalar_idx, &simd_idx, "tailwind.css");
}

#[test]
fn wide_lut_alphabet_15_singletons() {
    let alphabet: StructuralAlphabet = StructuralAlphabet::from_parts(
        &[
            b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm', b'n',
            b'o',
        ],
        &[],
        &[],
    );
    let input = b"the quick brown fox jumps over the lazy dog";
    let scalar_idx = scalar::scan(input, &alphabet);
    let simd_idx = scan_structural(input, &alphabet);
    assert_indices_equal(&scalar_idx, &simd_idx, "wide_lut");
}

#[test]
fn multi_cmp_alphabet_20_singletons() {
    let alphabet: StructuralAlphabet = StructuralAlphabet::from_parts(
        &[
            b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm', b'n',
            b'o', b'p', b'q', b'r', b's', b't',
        ],
        &[],
        &[],
    );
    let input = b"the quick brown fox jumps over the lazy dog";
    let scalar_idx = scalar::scan(input, &alphabet);
    let simd_idx = scan_structural(input, &alphabet);
    assert_indices_equal(&scalar_idx, &simd_idx, "multi_cmp");
}

#[test]
fn stripe_boundary_64_byte_input() {
    // Exactly one stripe; the kernel takes the stripe path and skips
    // the tail.
    let input = b"{abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()_+abcdefghijklmn,";
    assert_eq!(input.len(), 64);
    let scalar_idx = scalar::scan(input, &JSON_NO_QUOTES);
    let simd_idx = scan_structural(input, &JSON_NO_QUOTES);
    assert_indices_equal(&scalar_idx, &simd_idx, "stripe_boundary_64");
}

#[test]
fn stripe_plus_one_input() {
    // 65 bytes — one stripe + one byte tail. Exercises the
    // tail-handling branch.
    let mut input = vec![b'a'; 65];
    input[0] = b'{';
    input[63] = b',';
    input[64] = b'}';
    let scalar_idx = scalar::scan(&input, &JSON_NO_QUOTES);
    let simd_idx = scan_structural(&input, &JSON_NO_QUOTES);
    assert_indices_equal(&scalar_idx, &simd_idx, "stripe_plus_one");
}
