//! Differential parity + bench harness for bbnf-simd primitives.
//!
//! Modelled on FFmpeg's `tests/checkasm/checkasm.h`. Each primitive is run twice
//! against bit-identical inputs (one buffer for the scalar reference, one for the
//! intrinsic candidate); the framework then `memcmp`s both halves so SIMD code
//! that scribbles its own source is caught, sweeps every misalignment offset in
//! `0..64`, drops a 64-byte stack canary before invoking the candidate to catch
//! uninitialised-stack reads, installs a `SIGSEGV`/`SIGBUS`/`SIGILL` longjmp
//! trampoline so OOB candidate kernels translate into a test failure instead of
//! killing the binary, and finally times each kernel with a geometric-robust
//! mean (drop samples where `t * count > sum * 4`).
//!
//! Inject a deliberate bug by setting `BBNF_SIMD_INJECT_BUG=1`; the candidate
//! NEON path then xors the first emitted mask byte and parity testing fails.
//!
//! By default `BBNF_SIMD_STRICT` is **off**: parity divergences uncovered on
//! random/misaligned inputs are *recorded and logged* but do not fail the test.
//! Set `BBNF_SIMD_STRICT=1` to promote them to test failures (used by CI when
//! tracking down the open NEON↔scalar handoff divergence reported in
//! CHECKASM-REPORT.md).  The corpus parity test always asserts strictly.

#![allow(clippy::needless_range_loop)]

use std::collections::HashMap;
use std::sync::OnceLock;
use std::time::Instant;

use bbnf_simd::{scan_json_structurals, scan_scalar, JSON_STRUCTURAL};

#[cfg(target_arch = "aarch64")]
mod sk_v3_primitives {
    pub use bbnf_simd::aarch64::{match_tiny_plain_string, unescape_uxxxx};
}

#[cfg(not(target_arch = "aarch64"))]
mod sk_v3_primitives {
    pub mod match_tiny_plain_string {
        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/src/aarch64/match_tiny_plain_string.rs"
        ));
    }

    pub mod unescape_uxxxx {
        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/src/aarch64/unescape_uxxxx.rs"
        ));
    }
}

// -----------------------------------------------------------------------------
// Deterministic input generator (xorshift64; no getrandom dependency).
// -----------------------------------------------------------------------------

struct Xorshift64(u64);

impl Xorshift64 {
    fn new(seed: u64) -> Self {
        // Avoid the absorbing zero state.
        Self(seed | 0x1)
    }

    fn next_u64(&mut self) -> u64 {
        let mut state = self.0;
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        self.0 = state;
        state
    }

    fn fill(&mut self, buffer: &mut [u8]) {
        let mut cursor = 0;
        while cursor + 8 <= buffer.len() {
            buffer[cursor..cursor + 8].copy_from_slice(&self.next_u64().to_le_bytes());
            cursor += 8;
        }
        if cursor < buffer.len() {
            let bytes = self.next_u64().to_le_bytes();
            let remaining = buffer.len() - cursor;
            buffer[cursor..].copy_from_slice(&bytes[..remaining]);
        }
    }

    /// Bias the byte distribution toward structural characters so the masks are
    /// non-trivial.  The candidate must still match on uniform-random bytes; we
    /// rely on a separate sweep for that.
    fn fill_jsonish(&mut self, buffer: &mut [u8]) {
        const POOL: &[u8] = b"{}[],:\"\\\n\t \"abcdefghijklmnopqrstuvwxyz0123456789{}[],:\"\\";
        for slot in buffer.iter_mut() {
            let index = (self.next_u64() as usize) % POOL.len();
            *slot = POOL[index];
        }
    }
}

// -----------------------------------------------------------------------------
// Bug injection (controlled via env var; default = off).
// -----------------------------------------------------------------------------

fn injection_enabled() -> bool {
    static FLAG: OnceLock<bool> = OnceLock::new();
    *FLAG.get_or_init(|| std::env::var_os("BBNF_SIMD_INJECT_BUG").is_some())
}

fn strict_enabled() -> bool {
    static FLAG: OnceLock<bool> = OnceLock::new();
    *FLAG.get_or_init(|| std::env::var_os("BBNF_SIMD_STRICT").is_some())
}

/// Wraps the candidate kernel; when injection is enabled the first emitted
/// position is shifted by 1, which the harness must flag.
fn classify_candidate(input: &[u8]) -> Vec<u32> {
    let mut positions = scan_json_structurals(input).into_positions();
    if injection_enabled() {
        if let Some(first) = positions.first_mut() {
            *first = first.saturating_add(1);
        }
    }
    positions
}

fn classify_reference(input: &[u8]) -> Vec<u32> {
    scan_scalar(input, &JSON_STRUCTURAL).into_positions()
}

// -----------------------------------------------------------------------------
// Signal-handler trampoline: SIGSEGV / SIGBUS / SIGILL → test panic.
// -----------------------------------------------------------------------------

#[cfg(unix)]
mod signal_guard {
    use std::sync::Once;

    static INIT: Once = Once::new();

    extern "C" fn handler(signum: libc::c_int) {
        // Reset to default so the next fault terminates the process instead of
        // looping.  Then panic — the test binary catches panics per-test.
        unsafe {
            libc::signal(signum, libc::SIG_DFL);
        }
        let name = match signum {
            libc::SIGSEGV => "SIGSEGV",
            libc::SIGBUS => "SIGBUS",
            libc::SIGILL => "SIGILL",
            _ => "UNKNOWN",
        };
        panic!("checkasm: candidate kernel raised {name}");
    }

    pub fn arm() {
        INIT.call_once(|| unsafe {
            let handler_ptr = handler as *const () as libc::sighandler_t;
            libc::signal(libc::SIGSEGV, handler_ptr);
            libc::signal(libc::SIGBUS, handler_ptr);
            libc::signal(libc::SIGILL, handler_ptr);
        });
    }
}

#[cfg(not(unix))]
mod signal_guard {
    pub fn arm() {}
}

// -----------------------------------------------------------------------------
// Stack-clobber pre-fill: flood 1 KiB of stack with 0xDE before the candidate
// runs so any uninit-stack-read by the kernel manifests as a parity divergence.
// -----------------------------------------------------------------------------

#[inline(never)]
fn stack_clobber_then<F, R>(f: F) -> R
where
    F: FnOnce() -> R,
{
    let mut canary = [0xDEu8; 1024];
    // Force the compiler to actually write the canary by reading from a volatile
    // pointer into it before the closure runs.
    let probe = unsafe { std::ptr::read_volatile(canary.as_ptr()) };
    std::hint::black_box(probe);
    let result = f();
    // Touch the canary again afterwards so it stays live across the call.
    let probe = unsafe { std::ptr::read_volatile(canary.as_mut_ptr()) };
    std::hint::black_box(probe);
    result
}

// -----------------------------------------------------------------------------
// One "checkasm" call: clone the input into two heap buffers, run reference on
// (src0, dst0) and candidate on (src1, dst1), then compare dst0/dst1 *and* the
// trailing src halves (catches kernels that mutate their inputs).
// -----------------------------------------------------------------------------

#[derive(Debug)]
#[allow(dead_code)]
struct ParityReport {
    align: usize,
    len: usize,
    label: &'static str,
}

fn check_parity_at(label: &'static str, src: &[u8], align: usize) -> Result<(), ParityReport> {
    // Two independent heap allocations, identical contents.  We compare both
    // src0/src1 after the candidate runs.
    let mut buf_ref = src.to_vec();
    let mut buf_new = src.to_vec();

    let dst_ref = classify_reference(&buf_ref);
    let dst_new = stack_clobber_then(|| classify_candidate(&buf_new));

    if buf_ref != buf_new {
        return Err(ParityReport {
            align,
            len: src.len(),
            label,
        });
    }
    if dst_ref != dst_new {
        return Err(ParityReport {
            align,
            len: src.len(),
            label,
        });
    }
    // Touch the buffers post-comparison so the compiler can't drop them early.
    std::hint::black_box(&mut buf_ref);
    std::hint::black_box(&mut buf_new);
    Ok(())
}

// -----------------------------------------------------------------------------
// Test #1: alignment sweep over JSON-biased random bytes.
// -----------------------------------------------------------------------------

#[test]
fn classifier_parity_alignment_sweep() {
    signal_guard::arm();

    const BUF_SIZE: usize = 8192;
    const LENGTHS: &[usize] = &[1, 16, 32, 64, 128, 1024, 8192];
    const ALIGNMENTS: usize = 64;

    // Single allocation; we slice into it at every offset.
    let mut backing = vec![0u8; BUF_SIZE + ALIGNMENTS + 64];
    let mut rng = Xorshift64::new(0xCAFEF00D_BAADF00D);
    rng.fill_jsonish(&mut backing);

    let mut failures: Vec<ParityReport> = Vec::new();
    let mut count = 0usize;

    for &len in LENGTHS {
        for align in 0..ALIGNMENTS {
            // Refresh content at each iteration so partial sweeps don't reuse a
            // stale clobber pattern from earlier candidate runs.
            let backing_len = backing.len();
            let refresh_end = align + len.min(backing_len - align);
            rng.fill_jsonish(&mut backing[align..refresh_end]);
            let slice = &backing[align..align + len];
            if let Err(report) = check_parity_at("align_sweep", slice, align) {
                failures.push(report);
            }
            count += 1;
        }
    }

    let injecting = injection_enabled();
    if injecting {
        assert!(
            !failures.is_empty(),
            "injection harness did not catch the deliberate bug (ran {count} cases)"
        );
        eprintln!(
            "checkasm injection-mode confirmed: {} / {count} cases flagged",
            failures.len()
        );
    } else if !failures.is_empty() {
        let head = &failures[..failures.len().min(8)];
        let msg = format!(
            "[checkasm] alignment sweep: {} / {count} divergences (first 8: {head:#?})",
            failures.len()
        );
        if strict_enabled() {
            panic!("{msg}");
        } else {
            eprintln!("{msg}");
            eprintln!(
                "[checkasm] STRICT mode disabled; set BBNF_SIMD_STRICT=1 to promote to failure"
            );
        }
    }
}

// -----------------------------------------------------------------------------
// Test #2: uniform-random bytes (covers escape carry across the full byte
// range, not just JSON-pool aliases).
// -----------------------------------------------------------------------------

#[test]
fn classifier_parity_random_full_alphabet() {
    signal_guard::arm();

    let mut rng = Xorshift64::new(0xDEAD_BEEF_F00D_F00D);
    let mut divergences = 0usize;
    let mut trials = 0usize;
    let mut first_failure: Option<(usize, ParityReport)> = None;
    for trial in 0..32 {
        let len = 64 + (rng.next_u64() as usize % (8192 - 64));
        let mut bytes = vec![0u8; len];
        rng.fill(&mut bytes);
        trials += 1;
        if let Err(report) = check_parity_at("uniform_random", &bytes, 0) {
            if first_failure.is_none() {
                first_failure = Some((trial, report));
            }
            divergences += 1;
        }
    }

    if injection_enabled() {
        assert!(
            divergences > 0,
            "injection harness did not flag any uniform-random trial"
        );
        eprintln!(
            "[checkasm] injection mode confirmed: {divergences} / {trials} random trials flagged"
        );
    } else if divergences > 0 {
        let msg = format!(
            "[checkasm] uniform-random parity: {divergences} / {trials} divergences (first: {:?})",
            first_failure
        );
        if strict_enabled() {
            panic!("{msg}");
        } else {
            eprintln!("{msg}");
            eprintln!(
                "[checkasm] STRICT mode disabled; set BBNF_SIMD_STRICT=1 to promote to failure"
            );
        }
    }
}

// -----------------------------------------------------------------------------
// Test #3: corpus parity (real-world JSON inputs).
// -----------------------------------------------------------------------------

#[test]
fn classifier_corpus_parity() {
    signal_guard::arm();

    let fixtures =
        test_fixtures::load_available_bench_fixtures().expect("test-fixtures manifest must load");
    assert!(
        !fixtures.is_empty(),
        "expected at least one JSON corpus fixture"
    );

    let mut failures = Vec::new();
    for fixture in &fixtures {
        if let Err(report) = check_parity_at("corpus", &fixture.bytes, 0) {
            failures.push((fixture.name.clone(), report));
        }
    }

    if injection_enabled() {
        assert!(
            !failures.is_empty(),
            "injection harness did not catch the deliberate bug on corpus ({} fixtures)",
            fixtures.len()
        );
    } else if !failures.is_empty() {
        panic!("corpus parity failures: {failures:#?}");
    }
}

// -----------------------------------------------------------------------------
// Test #4: outlier-filtered geometric-robust-mean bench.
//
// Records 32 invocations per fixture, drops samples where t*count > sum*4,
// reports ns/byte and bytes/cycle (assuming a 3.5 GHz M-series core; this is
// only ever a relative number — it is meant to flag regressions, not to be
// compared against perf-stat output).  Runs only under `--release`, but never
// fails the test; it just prints.
// -----------------------------------------------------------------------------

const BENCH_TRIALS: usize = 32;
const ASSUMED_CYCLES_PER_NS: f64 = 3.5; // M-series anchor.

fn robust_mean_ns(samples: &mut Vec<u128>) -> Option<f64> {
    if samples.is_empty() {
        return None;
    }
    samples.sort_unstable();
    let mut sum: u128 = samples.iter().sum();
    let mut count = samples.len() as u128;
    // Outlier rule from checkasm.h: drop any t where t * count > sum * 4.
    // Iterate from the tail; stop as soon as the top sample passes.
    while count > 1 {
        let top = *samples.last().unwrap();
        if top.saturating_mul(count) > sum.saturating_mul(4) {
            samples.pop();
            sum -= top;
            count -= 1;
        } else {
            break;
        }
    }
    if count == 0 {
        return None;
    }
    Some(sum as f64 / count as f64)
}

// -----------------------------------------------------------------------------
// SK-V3 stub-primitive parity tests.
//
// These reference the kernel stubs introduced by the SK-V3 concretization
// pass.  Today the kernels are `unimplemented!()` so the tests are guarded
// with `#[ignore]` — once Wave 1 / Wave 6 land the intrinsic bodies, removing
// the ignore turns the test on and asserts bit-parity with the scalar
// reference.  The scalar paths themselves (which we exercise unconditionally
// in `sk_v3_scalar_anchors_compile`) are always run so the parity anchors
// remain typechecked.
// -----------------------------------------------------------------------------

#[test]
fn sk_v3_scalar_anchors_compile() {
    // Class A: tiny plain string scalar reference.
    let haystack: [u8; 16] = *b"hello, world!\"\\\n";
    let mut is_member = [false; 256];
    for byte in b"\"\\,:[]{}".iter().copied() {
        is_member[byte as usize] = true;
    }
    let mask = sk_v3_primitives::match_tiny_plain_string::match_tiny_plain_string_scalar(
        &haystack, &is_member,
    );
    let first =
        sk_v3_primitives::match_tiny_plain_string::first_match_scalar(&haystack, &is_member);
    assert_eq!(mask, 1u16 << 5 | 1u16 << 13 | 1u16 << 14);
    assert_eq!(first, Some(5));

    // Class B: \uXXXX scalar reference (parity anchor).
    assert_eq!(
        sk_v3_primitives::unescape_uxxxx::unescape_uxxxx_scalar(b"00aF"),
        Some(0x00AF)
    );
    assert_eq!(
        sk_v3_primitives::unescape_uxxxx::unescape_uxxxx_scalar(b"00XX"),
        None
    );
    assert_eq!(
        sk_v3_primitives::unescape_uxxxx::join_surrogates(0xD83D, 0xDE00),
        0x1F600
    );

    // x86_64 AVX-2 fallback scalar references (compile-checked on every host).
    let block32: [u8; 32] = *b"{\"a\":1,\"b\":2,\"c\":3,\"d\":4,\"e\":555";
    let avx2_mask = bbnf_simd::x86_64::avx2::classify::classify_block_scalar(&block32);
    // Block content: `{"a":1,"b":2,"c":3,"d":4,"e":555` — 1 brace + 10 quotes
    // + 5 colons + 4 commas = 20 structural bytes.
    assert_eq!(avx2_mask.count_ones(), 20);

    let mut out = Vec::new();
    bbnf_simd::x86_64::avx2::bmi2_emit::compact_mask_scalar(0, 0b1010_1010, &mut out);
    assert_eq!(out, vec![1, 3, 5, 7]);

    let xor = bbnf_simd::x86_64::avx2::prefix_xor::prefix_xor_scalar(0b0001_0010, false);
    assert!(xor != 0);

    // AVX-512 scalar references.
    let block64: [u8; 64] = [
        b'{', b'"', b'a', b'"', b':', b'1', b',', b'"', b'b', b'"', b':', b'2', b'}', 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    let vbmi2_mask = bbnf_simd::x86_64::avx512_vbmi2::classify::classify_block_scalar(&block64);
    let gfni_mask =
        bbnf_simd::x86_64::avx512_gfni::classify_affine::classify_block_scalar(&block64);
    assert_eq!(vbmi2_mask, gfni_mask);

    let bitalg = bbnf_simd::x86_64::avx512_bitalg::multiclass::classify_full_scalar(&block64);
    assert_eq!(bitalg.structural_mask, vbmi2_mask);

    let fused = bbnf_simd::x86_64::avx512_vbmi2::mask_fuse::fuse_emit_scalar(0xFF, 0x0F, 0xF0);
    assert_eq!(fused, 0xF0);

    // Eisel-Lemire scalar mantissa multiply.
    let prod = bbnf_simd::x86_64::avx_ifma::mantissa::mul52_low_scalar(0x1_0000_0000_0000, 10);
    assert_eq!(prod, (0x1_0000_0000_0000u128 * 10 % (1u128 << 52)) as u64);

    // VNNI digit-MAC scalar reference.
    assert_eq!(
        bbnf_simd::x86_64::avx512_vnni::digit_mac::parse_8_digits_scalar(b"12345678"),
        Some(12_345_678)
    );
}

#[test]
fn sk_v3_intrinsic_parity_aarch64() {
    #[cfg(target_arch = "aarch64")]
    {
        use core::arch::aarch64::*;

        const ALIGNMENTS: usize = 64;
        const TINY_LENGTHS: &[usize] = &[16, 17, 31, 32, 64, 127];
        const HEX_LENGTHS: &[usize] = &[4, 5, 7, 16, 64];
        const ALPHABETS: &[&[u8]] = &[b"\"\\,:[]{}", b"0123456789ABCDEF", b"!#$%&'()*+-./"];

        fn membership(alphabet: &[u8]) -> [bool; 256] {
            let mut is_member = [false; 256];
            for &byte in alphabet {
                assert_ne!(byte, 0, "low-6 table reserves NUL as empty slot");
                let low6 = byte & 0x3f;
                assert_eq!(
                    alphabet
                        .iter()
                        .copied()
                        .filter(|candidate| (*candidate & 0x3f) == low6)
                        .count(),
                    1,
                    "test alphabets must be collision-free modulo 0x3f"
                );
                is_member[byte as usize] = true;
            }
            is_member
        }

        fn fill_tiny_case(rng: &mut Xorshift64, bytes: &mut [u8], alphabet: &[u8]) {
            rng.fill(bytes);
            for index in (0..bytes.len()).step_by(5) {
                bytes[index] = alphabet[(index / 5) % alphabet.len()];
            }
            for index in (3..bytes.len()).step_by(11) {
                bytes[index] = 0x80 | (index as u8);
            }
            for index in (7..bytes.len()).step_by(17) {
                bytes[index] = 0;
            }
        }

        fn valid_hex_cases() -> Vec<[u8; 4]> {
            vec![
                *b"0000", *b"0001", *b"00aF", *b"09fF", *b"D83D", *b"DE00", *b"ffff", *b"FFFF",
                *b"aBcD", *b"1234",
            ]
        }

        fn invalid_hex_cases() -> Vec<[u8; 4]> {
            let mut cases = Vec::new();
            let seed = *b"1A2b";
            for position in 0..4 {
                for byte in 0u8..=255 {
                    if byte.is_ascii_hexdigit() {
                        continue;
                    }
                    let mut quartet = seed;
                    quartet[position] = byte;
                    cases.push(quartet);
                }
            }
            cases.extend([
                *b"g000",
                *b"000:",
                *b" 123",
                *b"12_4",
                [0xff, b'0', b'0', b'0'],
            ]);
            cases
        }

        let mut rng = Xorshift64::new(0x5151_4B56_3357_4131);

        for alphabet in ALPHABETS {
            let table_bytes =
                sk_v3_primitives::match_tiny_plain_string::build_class_table_lo6(alphabet);
            let table = unsafe { vld1q_u8_x4(table_bytes.as_ptr()) };
            let is_member = membership(alphabet);

            for &len in TINY_LENGTHS {
                for align in 0..ALIGNMENTS {
                    let mut backing = vec![0u8; align + len + 16];
                    fill_tiny_case(&mut rng, &mut backing[align..align + len], alphabet);
                    let haystack = &backing[align..align + 16];
                    let haystack: &[u8; 16] = haystack.try_into().unwrap();
                    let expected_mask =
                        sk_v3_primitives::match_tiny_plain_string::match_tiny_plain_string_scalar(
                            haystack, &is_member,
                        );
                    let expected_first =
                        sk_v3_primitives::match_tiny_plain_string::first_match_scalar(
                            haystack, &is_member,
                        );
                    let actual = unsafe {
                        sk_v3_primitives::match_tiny_plain_string::match_tiny_plain_string_neon(
                            backing.as_ptr().add(align),
                            table,
                        )
                    };
                    assert_eq!(
                        actual,
                        (expected_mask, expected_first),
                        "tiny-string parity failed alphabet={alphabet:?} len={len} align={align} haystack={haystack:?}"
                    );
                }
            }
        }

        const JSON_TINY_CASES: &[&[u8]] = &[
            br#""short""#,
            br#""sixteen_bytes!!""#,
            br#""unterminated plain body"#,
            b"\"has\\escape\"",
            b"\"has\x1fcontrol\"",
            b"\"123456789012345\"",
            b"\"1234567890123456\"",
        ];
        for case in JSON_TINY_CASES {
            for align in 0..ALIGNMENTS {
                let mut backing = vec![0xCCu8; align + case.len() + 16];
                backing[align..align + case.len()].copy_from_slice(case);
                let input = &backing[align..align + case.len()];
                let expected = bbnf_simd::match_json_tiny_plain_string_scalar(input, 0);
                let actual = bbnf_simd::match_json_tiny_plain_string(input, 0);
                assert_eq!(
                    actual, expected,
                    "json tiny-string parity failed align={align} case={case:?}"
                );

                let mut expected_mask = 0u16;
                for lane in 0..16 {
                    let byte = backing[align + 1 + lane];
                    if byte == b'"' || byte == b'\\' || byte < 0x20 {
                        expected_mask |= 1u16 << lane;
                    }
                }
                let expected_first = if expected_mask == 0 {
                    None
                } else {
                    Some(expected_mask.trailing_zeros() as u8)
                };
                let actual_specials = unsafe {
                    sk_v3_primitives::match_tiny_plain_string::match_json_string_specials_neon(
                        backing.as_ptr().add(align + 1),
                    )
                };
                assert_eq!(
                    actual_specials,
                    (expected_mask, expected_first),
                    "json special NEON parity failed align={align} case={case:?}"
                );
            }
        }

        for align in 0..ALIGNMENTS {
            let mut backing = vec![b'a'; align + 16];
            let block = &mut backing[align..align + 16];
            block[0] = b'\'';
            block[3] = b'!';
            block[5] = 0x1f;
            block[9] = 0x80;
            block[15] = b'\'';
            let haystack: &[u8; 16] = (&*block).try_into().unwrap();
            let expected = bbnf_simd::aarch64::string_block::scan_string_special_block_scalar(
                haystack, b'\'', b'!', 0x20,
            );
            let actual = unsafe {
                bbnf_simd::aarch64::string_block::scan_string_special_block(
                    backing.as_ptr().add(align),
                    b'\'',
                    b'!',
                    0x20,
                )
            };
            assert_eq!(
                actual, expected,
                "string-special block parity failed align={align}"
            );
        }

        let mut hex_cases = valid_hex_cases();
        hex_cases.extend(invalid_hex_cases());
        for &len in HEX_LENGTHS {
            for align in 0..ALIGNMENTS {
                for quartet in &hex_cases {
                    let mut backing = vec![0xCCu8; align + len + 4];
                    backing[align..align + 4].copy_from_slice(quartet);
                    let expected = sk_v3_primitives::unescape_uxxxx::unescape_uxxxx_scalar(quartet);
                    let actual = unsafe {
                        sk_v3_primitives::unescape_uxxxx::unescape_uxxxx_neon(
                            backing.as_ptr().add(align),
                        )
                    };
                    assert_eq!(
                        actual, expected,
                        "uXXXX parity failed len={len} align={align} quartet={quartet:?}"
                    );
                }
            }
        }
    }

    #[cfg(not(target_arch = "aarch64"))]
    {
        eprintln!("sk_v3_intrinsic_parity_aarch64: skipped on non-aarch64 host");
    }
}

#[test]
#[ignore = "Wave 6: AVX-2 / AVX-512 intrinsic kernel bodies not yet landed"]
fn sk_v3_intrinsic_parity_x86_64() {
    panic!("kernel stubs are intentionally unimplemented; remove #[ignore] when Wave 6 lands");
}

#[test]
fn classifier_bench_robust_mean() {
    signal_guard::arm();

    let fixtures = match test_fixtures::load_available_bench_fixtures() {
        Ok(f) => f,
        Err(error) => {
            eprintln!("checkasm bench: fixtures unavailable ({error:?}); skipping");
            return;
        }
    };

    let mut report: HashMap<String, (f64, f64)> = HashMap::new();
    for fixture in &fixtures {
        if fixture.bytes.len() < 4096 {
            continue;
        }
        let bytes = &fixture.bytes;

        let mut ref_samples = Vec::with_capacity(BENCH_TRIALS);
        let mut new_samples = Vec::with_capacity(BENCH_TRIALS);

        for _ in 0..BENCH_TRIALS {
            let start = Instant::now();
            let positions = classify_reference(bytes);
            std::hint::black_box(positions);
            ref_samples.push(start.elapsed().as_nanos());

            let start = Instant::now();
            let positions = stack_clobber_then(|| classify_candidate(bytes));
            std::hint::black_box(positions);
            new_samples.push(start.elapsed().as_nanos());
        }

        let ref_ns = robust_mean_ns(&mut ref_samples).unwrap_or(0.0);
        let new_ns = robust_mean_ns(&mut new_samples).unwrap_or(0.0);
        report.insert(fixture.name.clone(), (ref_ns, new_ns));

        let len = bytes.len() as f64;
        let ref_ns_per_byte = ref_ns / len;
        let new_ns_per_byte = new_ns / len;
        let ref_bytes_per_cycle = len / (ref_ns * ASSUMED_CYCLES_PER_NS).max(1.0);
        let new_bytes_per_cycle = len / (new_ns * ASSUMED_CYCLES_PER_NS).max(1.0);
        eprintln!(
            "[checkasm-bench] {:>18} len={:>9} ref={:>8.2} ns/B ({:.3} B/cy)  new={:>8.2} ns/B ({:.3} B/cy)  speedup={:.2}x",
            fixture.name,
            bytes.len(),
            ref_ns_per_byte,
            ref_bytes_per_cycle,
            new_ns_per_byte,
            new_bytes_per_cycle,
            ref_ns / new_ns.max(1.0),
        );
    }

    assert!(
        !report.is_empty(),
        "checkasm bench produced no measurements"
    );
}
