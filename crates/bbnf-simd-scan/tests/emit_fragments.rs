//! Tests for the `bbnf-simd-scan::emit` body-fragment exporters.
//!
//! Per AW-V.W1.2, each kernel fragment must:
//! 1. Exist as a `SOURCE` (or per-arch `SOURCE_*`) `&'static str`.
//! 2. Parse as valid Rust via [`syn::parse_str`].
//! 3. Return a [`proc_macro2::TokenStream`] from its `fragment()` (or
//!    per-arch `fragment_*()`) fn.
//!
//! This harness exercises every fragment exporter. Failures halt the
//! build — the per-shape emitter that consumes these fragments cannot
//! produce valid Rust if a fragment is malformed.

use bbnf_simd_scan::emit;

// ── 1. nibble_lut_scan ──────────────────────────────────────────────

#[test]
fn nibble_lut_scan_neon_fragment_parses() {
    let ts = emit::nibble_lut_scan::fragment_neon();
    assert!(
        !ts.is_empty(),
        "nibble_lut_scan::fragment_neon() produced an empty TokenStream"
    );
    let _: syn::Block = syn::parse2(ts)
        .expect("nibble_lut_scan NEON fragment must re-parse as syn::Block");
}

#[test]
fn nibble_lut_scan_avx2_fragment_parses() {
    let ts = emit::nibble_lut_scan::fragment_avx2();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("nibble_lut_scan AVX2 fragment must re-parse as syn::Block");
}

// ── 2. multi_cmp_scan ───────────────────────────────────────────────

#[test]
fn multi_cmp_scan_neon_fragment_parses() {
    let ts = emit::multi_cmp_scan::fragment_neon();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("multi_cmp_scan NEON fragment must re-parse as syn::Block");
}

#[test]
fn multi_cmp_scan_avx2_fragment_parses() {
    let ts = emit::multi_cmp_scan::fragment_avx2();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("multi_cmp_scan AVX2 fragment must re-parse as syn::Block");
}

// ── 3. clmul_parity ─────────────────────────────────────────────────

#[test]
fn clmul_parity_aarch64_fragment_parses() {
    let ts = emit::clmul_parity::fragment_aarch64();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("clmul_parity aarch64 fragment must re-parse as syn::Block");
}

#[test]
fn clmul_parity_x86_64_fragment_parses() {
    let ts = emit::clmul_parity::fragment_x86_64();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("clmul_parity x86_64 fragment must re-parse as syn::Block");
}

// ── 4. shift_xor_parity ─────────────────────────────────────────────

#[test]
fn shift_xor_parity_fragment_parses() {
    let ts = emit::shift_xor_parity::fragment();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("shift_xor_parity fragment must re-parse as syn::Block");
}

// ── 5. tzcnt_compact ────────────────────────────────────────────────

#[test]
fn tzcnt_compact_fragment_parses() {
    let ts = emit::tzcnt_compact::fragment();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("tzcnt_compact fragment must re-parse as syn::Block");
}

// ── 6. nospace64_scan ───────────────────────────────────────────────

#[test]
fn nospace64_scan_neon_fragment_parses() {
    let ts = emit::nospace64_scan::fragment_neon();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("nospace64_scan NEON fragment must re-parse as syn::Block");
}

#[test]
fn nospace64_scan_chunk_ns_mask16_neon_fragment_parses() {
    let ts = emit::nospace64_scan::fragment_chunk_ns_mask16_neon();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts).expect(
        "nospace64_scan chunk_ns_mask16 NEON helper must re-parse as syn::Block",
    );
}

#[test]
fn nospace64_scan_avx2_fragment_parses() {
    let ts = emit::nospace64_scan::fragment_avx2();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("nospace64_scan AVX2 fragment must re-parse as syn::Block");
}

#[test]
fn nospace64_scan_scalar_fragment_parses() {
    let ts = emit::nospace64_scan::fragment_scalar();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("nospace64_scan scalar fragment must re-parse as syn::Block");
}

// ── 7. first_quote_or_backslash ─────────────────────────────────────

#[test]
fn first_quote_or_backslash_neon_fragment_parses() {
    let ts = emit::first_quote_or_backslash::fragment_neon();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("first_quote_or_backslash NEON fragment must re-parse as syn::Block");
}

#[test]
fn first_quote_or_backslash_avx2_fragment_parses() {
    let ts = emit::first_quote_or_backslash::fragment_avx2();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("first_quote_or_backslash AVX2 fragment must re-parse as syn::Block");
}

#[test]
fn first_quote_or_backslash_scalar_fragment_parses() {
    let ts = emit::first_quote_or_backslash::fragment_scalar();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("first_quote_or_backslash scalar fragment must re-parse as syn::Block");
}

// ── 8. quoted_string_simd_body ──────────────────────────────────────

#[test]
fn quoted_string_simd_body_fragment_parses() {
    let ts = emit::quoted_string_simd_body::fragment();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("quoted_string_simd_body fragment must re-parse as syn::Block");
}

// ── 9. eisel_lemire_body ────────────────────────────────────────────

#[test]
fn eisel_lemire_body_fragment_parses() {
    let ts = emit::eisel_lemire_body::fragment();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("eisel_lemire_body fragment must re-parse as syn::Block");
}

#[test]
fn eisel_lemire_try_fast_path_fragment_parses() {
    let ts = emit::eisel_lemire_body::fragment_try_fast_path();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("eisel_lemire try_fast_path helper must re-parse as syn::Block");
}

#[test]
fn eisel_lemire_compute_float_slow_fragment_parses() {
    let ts = emit::eisel_lemire_body::fragment_compute_float_slow();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("eisel_lemire compute_float_slow helper must re-parse as syn::Block");
}

#[test]
fn eisel_lemire_compute_product_approx_fragment_parses() {
    let ts = emit::eisel_lemire_body::fragment_compute_product_approx();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("eisel_lemire compute_product_approx helper must re-parse as syn::Block");
}

#[test]
fn eisel_lemire_power_of_q_fragment_parses() {
    let ts = emit::eisel_lemire_body::fragment_power_of_q();
    assert!(!ts.is_empty());
    let _: syn::Block = syn::parse2(ts)
        .expect("eisel_lemire power_of_q helper must re-parse as syn::Block");
}
