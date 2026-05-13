//! Differential parity harness for the `byte_class_from_eq_set_64` primitive.
//!
//! Mirrors the structure of `tests/checkasm_parity.rs`: deterministic xorshift
//! input generation, alignment sweeps over a 128-byte backing buffer, signal
//! trampoline for SIGSEGV/SIGBUS/SIGILL, adversarial seeds that exposed prior
//! NEON↔scalar handoff bugs, and corpus parity against `twitter.json`.
//!
//! The primitive under test is the auto-dispatched
//! `bbnf_simd::prim::byte_class_from_eq_set_64(src: &[u8; 64], set: &[u8]) -> u64`
//! which returns a 64-bit mask whose bit `i` is set iff `src[i] ∈ set`.  The
//! contract requires `set.len() ≤ 8`; this is asserted in the wrapper layer
//! and re-validated here as a documentation hint.
//!
//! The scalar reference at
//! `bbnf_simd::scalar::byte_class_from_eq_set_64::byte_class_from_eq_set_64_scalar`
//! is the executable specification; every active backend must agree with it
//! bit-for-bit.
//!
//! Helper duplication note: the `Xorshift64`, `signal_guard`, and
//! `stack_clobber_then` constructs in `tests/checkasm_parity.rs` are
//! module-private (each integration test compiles as its own crate).  We
//! redefine the minimal subset here rather than promote the originals to
//! `pub(crate)` — Cargo would still hide them across the integration-test
//! crate boundary.  The shapes match the originals verbatim so cross-file
//! audits remain mechanical.

#![allow(clippy::needless_range_loop)]

use bbnf_simd::prim::byte_class_from_eq_set_64;
use bbnf_simd::scalar::byte_class_from_eq_set_64::byte_class_from_eq_set_64_scalar;

// -----------------------------------------------------------------------------
// Deterministic input generator (xorshift64; matches checkasm_parity.rs).
// -----------------------------------------------------------------------------

struct Xorshift64(u64);

impl Xorshift64 {
    fn new(seed: u64) -> Self {
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

    /// Draw `count` distinct bytes (membership-set construction).  `count` must
    /// be ≤ 8 per the primitive contract; we additionally cap at the
    /// alphabet's natural ceiling of 256.
    fn draw_distinct_set(&mut self, count: usize) -> Vec<u8> {
        assert!(count <= 8, "primitive contract: set.len() ≤ 8");
        let mut seen = [false; 256];
        let mut set = Vec::with_capacity(count);
        while set.len() < count {
            let candidate = (self.next_u64() & 0xFF) as u8;
            if !seen[candidate as usize] {
                seen[candidate as usize] = true;
                set.push(candidate);
            }
        }
        set
    }
}

// -----------------------------------------------------------------------------
// Signal-handler trampoline.  Mirrors checkasm_parity.rs verbatim.
// -----------------------------------------------------------------------------

#[cfg(unix)]
mod signal_guard {
    use std::sync::Once;

    static INIT: Once = Once::new();

    extern "C" fn handler(signum: libc::c_int) {
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
// Stack-clobber pre-fill — flood 1 KiB of stack with 0xDE before the candidate
// runs so any uninit-stack-read by the kernel manifests as parity divergence.
// -----------------------------------------------------------------------------

#[inline(never)]
fn stack_clobber_then<F, R>(f: F) -> R
where
    F: FnOnce() -> R,
{
    let mut canary = [0xDEu8; 1024];
    let probe = unsafe { std::ptr::read_volatile(canary.as_ptr()) };
    std::hint::black_box(probe);
    let result = f();
    let probe = unsafe { std::ptr::read_volatile(canary.as_mut_ptr()) };
    std::hint::black_box(probe);
    result
}

// -----------------------------------------------------------------------------
// Single parity check: run candidate (dispatch) and reference (scalar) over a
// 64-byte window, then assert bit-equality.  Returns the divergent mask pair
// for caller-side reporting.
// -----------------------------------------------------------------------------

#[derive(Debug)]
#[allow(dead_code)]
struct ParityFailure {
    label: &'static str,
    align: usize,
    set: Vec<u8>,
    src: [u8; 64],
    candidate_mask: u64,
    reference_mask: u64,
    xor: u64,
    first_diff_lane: u32,
}

fn check_parity(
    label: &'static str,
    align: usize,
    src: &[u8; 64],
    set: &[u8],
) -> Result<u64, ParityFailure> {
    let reference_mask = byte_class_from_eq_set_64_scalar(src, set);
    let candidate_mask = stack_clobber_then(|| byte_class_from_eq_set_64(src, set));
    if candidate_mask == reference_mask {
        Ok(candidate_mask)
    } else {
        let xor = candidate_mask ^ reference_mask;
        let first_diff_lane = xor.trailing_zeros();
        Err(ParityFailure {
            label,
            align,
            set: set.to_vec(),
            src: *src,
            candidate_mask,
            reference_mask,
            xor,
            first_diff_lane,
        })
    }
}

// -----------------------------------------------------------------------------
// Test #1: alignment sweep.  For each offset in 0..64, slice a 64-byte window
// out of a 128-byte adversarially-filled buffer and assert parity.  This
// catches misalignment-sensitive loads (e.g. an `vmovdqa64` that should be
// `vmovdqu64`).
// -----------------------------------------------------------------------------

#[test]
fn byte_class_alignment_sweep() {
    signal_guard::arm();

    let mut rng = Xorshift64::new(0xCAFE_F00D_BAAD_F00D);
    let mut backing = [0u8; 128];
    rng.fill(&mut backing);

    let set: &[u8] = b"{}[],:\"\\";
    let mut failures: Vec<ParityFailure> = Vec::new();

    for align in 0..64 {
        // Refresh the window per iteration so a leaky kernel that scribbles
        // its inputs cannot poison later cases.
        let mut window = [0u8; 64];
        window.copy_from_slice(&backing[align..align + 64]);
        if let Err(failure) = check_parity("alignment_sweep", align, &window, set) {
            failures.push(failure);
        }
    }

    assert!(
        failures.is_empty(),
        "alignment sweep produced {} divergence(s); first: {:#?}",
        failures.len(),
        failures.first()
    );
}

// -----------------------------------------------------------------------------
// Test #2: set-size sweep.  For each set_size in 1..=8, draw a fresh distinct
// random set and run the primitive against a handful of random source
// windows.  Exercises every legal set-cardinality the contract admits.
// -----------------------------------------------------------------------------

#[test]
fn byte_class_set_size_sweep() {
    signal_guard::arm();

    let mut rng = Xorshift64::new(0xDEAD_BEEF_1234_5678);
    let mut failures: Vec<ParityFailure> = Vec::new();

    for set_size in 1..=8usize {
        let set = rng.draw_distinct_set(set_size);
        for trial in 0..16usize {
            let mut src = [0u8; 64];
            rng.fill(&mut src);
            // Sprinkle a few set members in deterministically so we hit the
            // "some hits" regime, not just "almost-empty mask".
            for index in (trial % 5..64).step_by(7) {
                src[index] = set[index % set.len()];
            }
            if let Err(failure) = check_parity("set_size_sweep", trial, &src, &set) {
                failures.push(failure);
            }
        }
    }

    assert!(
        failures.is_empty(),
        "set-size sweep produced {} divergence(s); first: {:#?}",
        failures.len(),
        failures.first()
    );
}

// -----------------------------------------------------------------------------
// Test #3: adversarial seeds.  Reproduces the seed shape that caught the
// escape_mask_64 bug in the existing harness; we exercise both seeds over
// every legal set cardinality and every alignment.
// -----------------------------------------------------------------------------

#[test]
fn byte_class_adversarial_seeds() {
    signal_guard::arm();

    const SEEDS: &[u64] = &[0xCAFEF00D_BAADF00D, 0xDEADBEEF_12345678];
    let mut failures: Vec<ParityFailure> = Vec::new();

    for &seed in SEEDS {
        let mut rng = Xorshift64::new(seed);
        let mut backing = [0u8; 128];
        rng.fill(&mut backing);

        for set_size in 1..=8usize {
            let set = rng.draw_distinct_set(set_size);
            for align in 0..64 {
                let mut window = [0u8; 64];
                window.copy_from_slice(&backing[align..align + 64]);
                if let Err(failure) = check_parity("adversarial", align, &window, &set) {
                    failures.push(failure);
                }
            }
        }
    }

    assert!(
        failures.is_empty(),
        "adversarial seeds produced {} divergence(s); first: {:#?}",
        failures.len(),
        failures.first()
    );
}

// -----------------------------------------------------------------------------
// Test #4: corpus parity.  Slide a 64-byte window across the first 2 MiB of
// twitter.json (or its entirety, whichever is smaller) and assert parity for
// the JSON-structural set `b"{}[],:"`.  This is the closest analogue to the
// production hot path the primitive is intended to serve.
// -----------------------------------------------------------------------------

#[test]
fn byte_class_corpus_parity_twitter() {
    signal_guard::arm();

    let fixtures = match test_fixtures::load_available_bench_fixtures() {
        Ok(f) => f,
        Err(error) => {
            eprintln!(
                "byte_class corpus parity: fixtures unavailable ({error:?}); skipping"
            );
            return;
        }
    };

    let twitter = match fixtures.iter().find(|f| f.name == "twitter") {
        Some(fixture) => fixture,
        None => {
            eprintln!("byte_class corpus parity: twitter fixture not in manifest; skipping");
            return;
        }
    };

    const STRUCTURAL_SET: &[u8] = b"{}[],:";
    const WINDOW_LEN: usize = 64;
    const SCAN_CAP: usize = 2 * 1024 * 1024;

    let bytes = &twitter.bytes;
    let scan_end = bytes.len().min(SCAN_CAP);
    if scan_end < WINDOW_LEN {
        eprintln!(
            "byte_class corpus parity: fixture {} smaller than window; skipping",
            twitter.name
        );
        return;
    }

    // Parity-hash discipline (mirrors checkasm_parity.rs): accumulate every
    // candidate mask and reference mask into independent rolling u128 mixers;
    // a single divergence yields divergent digests.  Mixer is splitmix64-style
    // (constants from Stafford / Vigna) so neighbouring masks do not cancel.
    fn mix64(state: u64, input: u64) -> u64 {
        let mut z = state.wrapping_add(input).wrapping_add(0x9E37_79B9_7F4A_7C15);
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }

    let mut candidate_state: u64 = 0;
    let mut reference_state: u64 = 0;
    let mut first_divergence: Option<(usize, u64, u64)> = None;
    let mut window = [0u8; WINDOW_LEN];

    for offset in 0..=(scan_end - WINDOW_LEN) {
        window.copy_from_slice(&bytes[offset..offset + WINDOW_LEN]);
        let reference_mask = byte_class_from_eq_set_64_scalar(&window, STRUCTURAL_SET);
        let candidate_mask =
            stack_clobber_then(|| byte_class_from_eq_set_64(&window, STRUCTURAL_SET));
        candidate_state = mix64(candidate_state, candidate_mask);
        reference_state = mix64(reference_state, reference_mask);
        if first_divergence.is_none() && candidate_mask != reference_mask {
            first_divergence = Some((offset, candidate_mask, reference_mask));
        }
    }

    assert_eq!(
        candidate_state, reference_state,
        "twitter corpus parity diverged; first byte offset / candidate / reference = {:?}",
        first_divergence
    );
}

// -----------------------------------------------------------------------------
// Test #5a: empty-set edge case.  set = &[] must always return mask = 0
// regardless of the source bytes.
// -----------------------------------------------------------------------------

#[test]
fn byte_class_empty_set_returns_zero_mask() {
    signal_guard::arm();

    let mut rng = Xorshift64::new(0x0000_5151_5151_5151);
    let empty: &[u8] = &[];
    let mut failures: Vec<ParityFailure> = Vec::new();

    for trial in 0..32 {
        let mut src = [0u8; 64];
        rng.fill(&mut src);
        let result = check_parity("empty_set", trial, &src, empty);
        match result {
            Ok(mask) => assert_eq!(
                mask, 0,
                "empty set must yield mask=0; got {mask:#018x} on trial {trial}"
            ),
            Err(failure) => failures.push(failure),
        }
    }

    assert!(
        failures.is_empty(),
        "empty-set sweep produced {} divergence(s); first: {:#?}",
        failures.len(),
        failures.first()
    );
}

// -----------------------------------------------------------------------------
// Test #5b: all-zero and all-0xFF inputs.  Each constant fill picks out
// either all 64 lanes or none, depending on the set; we sweep a curated set
// of single-byte memberships against both fills.
// -----------------------------------------------------------------------------

#[test]
fn byte_class_constant_fill_inputs() {
    signal_guard::arm();

    let mut failures: Vec<ParityFailure> = Vec::new();

    for &fill in &[0x00u8, 0xFFu8] {
        let src = [fill; 64];
        // Single-byte sets covering both extremes plus a few interior bytes.
        for &probe in &[0x00u8, 0x01, 0x20, 0x7F, 0x80, 0xFE, 0xFF] {
            let set = [probe];
            let expected = if probe == fill { u64::MAX } else { 0 };
            let result = check_parity("constant_fill", probe as usize, &src, &set);
            match result {
                Ok(mask) => assert_eq!(
                    mask, expected,
                    "constant fill={fill:#04x} probe={probe:#04x}: expected {expected:#018x}, got {mask:#018x}"
                ),
                Err(failure) => failures.push(failure),
            }
        }
    }

    assert!(
        failures.is_empty(),
        "constant-fill sweep produced {} divergence(s); first: {:#?}",
        failures.len(),
        failures.first()
    );
}

// -----------------------------------------------------------------------------
// Test #5c: duplicate-byte set.  A set that names the same byte twice must
// behave identically to the deduplicated set.  Verifies that the primitive
// does not double-count or otherwise misinterpret duplicated entries.
// -----------------------------------------------------------------------------

#[test]
fn byte_class_duplicate_set_entry() {
    signal_guard::arm();

    let mut rng = Xorshift64::new(0x4242_BEEF_CAFE_0001);
    let mut failures: Vec<ParityFailure> = Vec::new();

    for trial in 0..16usize {
        let mut src = [0u8; 64];
        rng.fill(&mut src);

        let probe = (rng.next_u64() & 0xFF) as u8;
        // Sprinkle the probe in so the resulting mask is non-trivial.
        for index in (trial % 3..64).step_by(5) {
            src[index] = probe;
        }

        // Dedup-equivalent reference: {probe} alone.
        let canonical = [probe];
        let canonical_mask = byte_class_from_eq_set_64_scalar(&src, &canonical);

        // Duplicate sets of cardinality 2, 4, 8.
        for dup_size in &[2usize, 4, 8] {
            let dup_set = vec![probe; *dup_size];
            let result = check_parity("duplicate_set", *dup_size, &src, &dup_set);
            match result {
                Ok(mask) => assert_eq!(
                    mask, canonical_mask,
                    "duplicate set (×{dup_size}) on probe={probe:#04x} diverged from canonical mask"
                ),
                Err(failure) => failures.push(failure),
            }
        }
    }

    assert!(
        failures.is_empty(),
        "duplicate-set sweep produced {} divergence(s); first: {:#?}",
        failures.len(),
        failures.first()
    );
}

// -----------------------------------------------------------------------------
// Test #6: tail-padding contract.  The primitive operates on exactly 64
// bytes — caller-supplied as a `&[u8; 64]` reference, which by Rust's type
// system already guarantees the read window is exactly that wide.  This test
// pins the contract: we allocate a tightly-sized 64-byte buffer with no
// trailing slack, take a `&[u8; 64]` reference, and run the primitive.  Any
// implementation that reads byte 65 would fault under MIRI / ASan and any
// implementation that returns lane bits outside [0..64) is caught by the
// scalar comparison.  No padding bytes are introduced anywhere — if the
// implementation requires them it must allocate internally, not steal them
// from the caller.
// -----------------------------------------------------------------------------

#[test]
fn byte_class_tail_padding_contract() {
    signal_guard::arm();

    let mut rng = Xorshift64::new(0xFEED_FACE_DEAD_BEEF);
    let set: &[u8] = b"{}[],:";
    let mut failures: Vec<ParityFailure> = Vec::new();

    for trial in 0..32usize {
        // Tightly-sized Vec; no trailing slack.  We deliberately do NOT use
        // a 128-byte backing buffer — the point is that the primitive must
        // not read past the caller's 64 bytes.
        let mut owned = vec![0u8; 64];
        rng.fill(&mut owned);
        let src: &[u8; 64] = owned.as_slice().try_into().expect("exactly 64 bytes");
        if let Err(failure) = check_parity("tail_padding", trial, src, set) {
            failures.push(failure);
        }
    }

    assert!(
        failures.is_empty(),
        "tail-padding sweep produced {} divergence(s); first: {:#?}",
        failures.len(),
        failures.first()
    );
}
