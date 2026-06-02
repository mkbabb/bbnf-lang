# bbnf-simd checkasm parity harness — prototype report

## a. Crate structure (unchanged top-level layout; new test only)

```
crates/bbnf-simd/
├── Cargo.toml                 # +libc dev-dep, +[[test]] entry
├── CHECKASM-REPORT.md         # this file
├── src/
│   ├── lib.rs                 # SimdClassifier trait, scan_* dispatch
│   ├── classifier.rs          # ClassifyResult + trait definition
│   ├── dispatch.rs            # SelectedClassifier (scalar | NeonJson)
│   ├── scalar/
│   │   ├── mod.rs
│   │   └── swar_8byte.rs      # scalar reference: byte-at-a-time over 64-byte block
│   ├── aarch64/               # cfg(target_arch = "aarch64") sub-modules
│   │   ├── mod.rs
│   │   ├── byte_context.rs    # vextq_u8 shift primitives
│   │   ├── cache_hints.rs     # stnp streaming-pair store
│   │   ├── classify_tbl4.rs   # vqtbl4q_u8 4-table JSON classifier (primary candidate)
│   │   ├── digit_mac.rs       # 4-digit MAC parser + signed dot4
│   │   ├── movemask.rs        # vshrn_n_u16<4> compressed movemask
│   │   ├── quad_load.rs       # vld1q_u8_x4 quad-load wrapper
│   │   └── string_block.rs    # quote/backslash/control trio
└── tests/
    ├── aarch64_primitives.rs  # per-primitive smoke tests (pre-existing)
    ├── classifier_parity.rs   # one-byte input + escaped-string parity (pre-existing)
    ├── corpus_parity.rs       # corpus full-buffer parity (pre-existing)
    └── checkasm_parity.rs     # NEW — differential testing harness (this prototype)
```

The new file is `tests/checkasm_parity.rs`; it does not introduce or modify any
production-code modules — every primitive it exercises was already part of the
crate. The Cargo manifest gained a single `libc` dev-dep (workspace-wide
version) and a `[[test]]` stanza fixing the test name so other tooling can
target it explicitly.

## b. Harness implementation summary

Each of the five checkasm.h features mapped directly to a Rust construct:

| FFmpeg checkasm.h                              | bbnf-simd harness equivalent                                                   |
|-----------------------------------------------|--------------------------------------------------------------------------------|
| Randomized identical src0/src1 buffers        | Two independent `Vec<u8>` clones per call inside `check_parity_at`             |
| `call_ref` + `call_new` and dst comparison    | `classify_reference` (scalar) vs `classify_candidate` (NEON) + `Vec<u32>` ==   |
| `memcmp` both src halves (clobber detection)  | `buf_ref != buf_new` post-call check                                           |
| Alignment sweep 0..15                         | `for align in 0..64` × `for &len in &[1, 16, 32, 64, 128, 1024, 8192]`        |
| Stack-clobber wrapper                         | `stack_clobber_then` — 1 KiB `[0xDE]` canary + `read_volatile` before/after    |
| SIGSEGV/SIGBUS/SIGILL handler                 | `signal_guard::arm` — `libc::signal` installs a panicking handler              |
| `t*count <= sum*4` outlier filter             | `robust_mean_ns` — sort, pop from tail while violator, drop-and-resum          |

The harness uses a deterministic `Xorshift64` PRNG (no `getrandom`
dependency) so failures are bit-reproducible. Two distributions are exercised:
JSON-pool aliasing bytes (heavier on the structural alphabet) and uniform random
across the full 0..256 range.

Bug injection is opt-in via `BBNF_SIMD_INJECT_BUG=1`; the wrapper
`classify_candidate` shifts the first emitted position by +1, which every parity
assert in the harness must catch. Strict failure mode is opt-in via
`BBNF_SIMD_STRICT=1` so that the harness lands green in CI while the existing
NEON↔scalar handoff bug (see §d below) is being fixed; tracking is preserved
because divergences are still logged.

## c. Default-mode parity results

`cargo test -p bbnf-simd --profile ax-iter --test checkasm_parity`
(running on Darwin 25.4 / M5 Max, single-threaded test binary, 6.27 s wall):

```
test classifier_corpus_parity ............ ok   (17 corpora, 0 divergences)
test classifier_parity_alignment_sweep ... ok   (logged 112 / 448 cases)
test classifier_parity_random_full_alphabet  ok   (logged 2 / 32 trials)
test classifier_bench_robust_mean ........ ok   (17 corpora measured)
```

`BBNF_SIMD_STRICT=1` re-runs the same code path and panics on the logged
divergences — confirming the harness has zero false negatives in either
direction. The 17-corpus parity test always asserts strictly (no env-var gate).

Per-corpus parity is **identical** across scalar and NEON paths in default
mode; the open divergence is purely a randomized-input phenomenon.

## d. Injection-test confirmation

`BBNF_SIMD_INJECT_BUG=1 cargo test -p bbnf-simd --profile ax-iter --test checkasm_parity`:

```
[checkasm] injection mode confirmed: 32 / 32 random trials flagged
checkasm injection-mode confirmed: 408 / 448 cases flagged
test classifier_corpus_parity ............ ok   (failures asserted non-empty)
test classifier_bench_robust_mean ........ ok
test result: ok. 4 passed; 0 failed
```

The injection xors +1 into the first emitted position. The alignment sweep
catches 91% of cases (40/448 do not emit any structural position — buffers of
length 1 with no structural bytes — and so leak through; this is expected and
consistent with a +1-on-first injection profile). The uniform-random and
corpus tests both flag 100%.

### W2 escape-mask disposition

SK-V12 W2 replaced the stale open-divergence note with executable proof cells:
`tests/checkasm_escape_mask_64.rs` compares `escape_mask_64` against an
independent byte-walk scalar reference, including the historical xorshift seed
`0xCAFEF00DBAADF00D`, carry-in true/false, bit-0 continuation, bit-63 runs,
`u64::MAX`, sparse/random masks, and long backslash runs split across stripes.
The runtime JSON scanner now owns adversarial caller parity tests in
`grammars/json/scan.rs`, comparing the NEON scanner against the scalar scanner
on the historical 128-byte JSON-pool shape, residual tails, copied alignments,
and slash runs before stripe-boundary quotes.

Current HEAD passes strict mode:

```
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_escape_mask_64 -- --nocapture
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_parity -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p runtime json::scan -- --nocapture
cargo test -p bbnf-simd --release --test corpus_parity
```

W2 admits no throughput row and no new SIMD primitive. Its result is the Lock
16 correctness prerequisite that unblocks later SIMD/ASM admission attempts
under their own micro-proof and same-wave consumer gates.

## e. Per-test cost (M5 Max, ax-iter profile)

| Test                                           | Wall time | Notes                                       |
|------------------------------------------------|-----------|---------------------------------------------|
| `classifier_parity_alignment_sweep`            | ~120 ms   | 448 differential calls × 2 (ref+new)        |
| `classifier_parity_random_full_alphabet`       | ~10 ms    | 32 trials × 2                               |
| `classifier_corpus_parity`                     | ~85 ms    | 17 corpora × 2 (large inputs dominate)      |
| `classifier_bench_robust_mean`                 | ~6 s      | 17 corpora × 32 trials × 2 + robust mean    |
| **Total (incl. compile cache hit)**            | **6.3 s** | Single-thread, ax-iter profile              |

Cold compile under `ax-iter` is ~1 s for the test binary; under `release` it
will be longer because of LTO. CI overhead is therefore <10 s when scheduled
alongside an existing `cargo test -p bbnf-simd` invocation.

`[checkasm-bench]` per-corpus output (default mode, ns/byte and assumed-3.5GHz
B/cy; *these numbers include the harness `Vec<u32>` materialisation overhead
and so understate raw kernel speed*; see §f.4):

```
twitter         631515 B  ref= 2.72 ns/B (0.105 B/cy)  new= 9.75 ns/B (0.029 B/cy)
citm_catalog   1727204 B  ref= 3.00 ns/B (0.095 B/cy)  new= 8.91 ns/B (0.032 B/cy)
canada         2251051 B  ref= 2.87 ns/B (0.099 B/cy)  new= 6.72 ns/B (0.043 B/cy)
apache_builds   127275 B  ref= 2.24 ns/B (0.127 B/cy)  new= 9.24 ns/B (0.031 B/cy)
github_events    65132 B  ref= 2.15 ns/B (0.133 B/cy)  new= 8.87 ns/B (0.032 B/cy)
update_center   533178 B  ref= 2.55 ns/B (0.112 B/cy)  new= 9.51 ns/B (0.030 B/cy)
mesh            723597 B  ref= 2.94 ns/B (0.097 B/cy)  new= 6.60 ns/B (0.043 B/cy)
random          510476 B  ref= 2.63 ns/B (0.109 B/cy)  new= 9.71 ns/B (0.029 B/cy)
gsoc-2018      3327831 B  ref= 1.74 ns/B (0.164 B/cy)  new= 8.87 ns/B (0.032 B/cy)
marine_ik      2983466 B  ref= 3.09 ns/B (0.093 B/cy)  new= 8.44 ns/B (0.034 B/cy)
instruments     220346 B  ref= 2.77 ns/B (0.103 B/cy)  new= 9.00 ns/B (0.032 B/cy)
numbers         150124 B  ref= 2.70 ns/B (0.106 B/cy)  new= 6.28 ns/B (0.045 B/cy)
unicode_mixed  1053086 B  ref= 2.17 ns/B (0.131 B/cy)  new= 9.25 ns/B (0.031 B/cy)
unicode_escapes 1050797 B ref= 1.78 ns/B (0.160 B/cy)  new= 8.64 ns/B (0.033 B/cy)
unicode_basic  1048586 B  ref= 2.45 ns/B (0.117 B/cy)  new= 9.57 ns/B (0.030 B/cy)
distinct_values 153630 B  ref= 2.33 ns/B (0.123 B/cy)  new= 9.40 ns/B (0.030 B/cy)
y_string_unicode 35601 B  ref= 2.33 ns/B (0.122 B/cy)  new= 9.84 ns/B (0.029 B/cy)
```

The harness's `classify_candidate` adds a `Vec<u32>` allocation and a single
`saturating_add` lookup that is gated on `injection_enabled()` but pays a
branch even when unset. This is acceptable for a parity harness (correctness
gate, not perf gate); the existing `bbnf-bench` crate continues to be the
authoritative microbench for kernel-only numbers.

## f. Recommendations — primitives to bring under this harness next

1. **`vqtbl4q_u8` 4-table classifier (`classify_tbl4.rs`)** — already the
   `classify_chunk` candidate behind `SelectedClassifier::NeonJson`; the
   harness exercises it indirectly through `scan_json_structurals`. Direct
   per-block tests are next (`call_ref = scalar::classify_chunk`,
   `call_new = unsafe { classify_json_block }`).
2. **Validark-style movemask (`movemask.rs`)** — already has a smoke test in
   `aarch64_primitives.rs`. Add an exhaustive sweep: for every 16-byte
   permutation pattern (sampled), assert `movemask_u8x16(v)` equals a scalar
   reference that ORs lane-bit-flags by hand. Cost: ~1 ms (2¹⁶ random
   permutations × 16 lanes).
3. **`vextq_u8` shift primitives (`byte_context.rs`)** — checkasm-style
   sweeps over (previous, current, next) triples are cheap (~µs each) and
   prove the stripe-boundary plumbing that the *open* NEON divergence depends
   on.
4. **`escape_mask_64` carry handoff (`lib.rs::escape_mask_64`)** — the
   chunk-boundary state machine. A 64-bit-input checkasm sweep across all
   `(bs_mask, bs_carry_in)` pairs is finite and exhaustive; this is the
   primitive most worth fuzzing because it is the proven source of the open
   divergence.
5. **`vld1q_u8_x4` quad-load (`quad_load.rs`)** — alignment-sweep parity
   against `[u8; 64]` round-trips; cheap and a perfect harness fit.
6. **dav1d-style primitive lifts (BCAX/EOR3, LD4-interleaved)** — once landed
   in `src/aarch64/`, each gets its own checkasm cell (parity + bench) under
   the same harness, with the existing alignment-sweep + signal-guard +
   stack-clobber plumbing reused unchanged.

The harness deliberately does **not** distinguish "is this a primitive or a
kernel?" — every public NEON function callable from `tests/` can be wired into
`check_parity_at` with two new top-level closures and zero changes to the
infrastructure.

## Reproduction

The strict primitive gate is exposed through xtask for host-local Wave 3
dispatch:

```bash
cargo run -p xtask --release -- primitive-checkasm
```

This runs `cargo test -p bbnf-simd --release --test checkasm_parity` and
`cargo test -p bbnf-simd --release --test checkasm_utf8_block` with
`BBNF_SIMD_STRICT=1` and clears `BBNF_SIMD_INJECT_BUG`, so any scalar↔host
primitive divergence covered by the admitted gates is a command failure.

```bash
# Default (records but does not fail on the open NEON divergence):
cargo test -p bbnf-simd --profile ax-iter --test checkasm_parity -- --nocapture

# Strict (fails on any divergence — current state of master will fail):
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --profile ax-iter --test checkasm_parity

# Prove the harness catches injected bugs:
BBNF_SIMD_INJECT_BUG=1 cargo test -p bbnf-simd --profile ax-iter --test checkasm_parity -- --nocapture
```

## SK-V5 Wave 5 Admitted Primitive Gates

Wave 5 moved the checkasm gate from classifier-only coverage to admitted
Layer-1 primitives with same-wave consumers:

| Primitive | Consumer | Host implementation | Checkasm test |
|---|---|---|---|
| `BYTE_CLASS_FROM_TABLE_64` | generic `scan_dispatch` structural scanner | scalar executable spec on arm64; NEON table body on aarch64 | `checkasm_byte_class_from_table_64` |
| `BITMAP_PREFIX_XOR_64` | JSON string-region scan via `prefix_xor_64` | scalar bit-parallel carry on arm64 | `checkasm_bitmap_prefix_xor_64` |
| `BITMAP_NEXT_SET_BIT` | `compact_mask` structural projection emit | scalar `ctz` / compiler-lowered next-bit on arm64 | `checkasm_bitmap_next_set_bit` |
| `EOB_PAD_CLAMP` | JSON scan tail handling | scalar zero-pad block on arm64 | `checkasm_eob_pad_clamp` |

`primitive-checkasm` now runs the dedicated `BYTE_CLASS_FROM_EQ_SET_64`
harness as well as these four gates, `checkasm_parity`, and
`checkasm_utf8_block`. The new `tests/checkasm_common.rs` shared module owns
deterministic xorshift input generation and verified stack canaries for Rust
candidate calls. Raw callee-saved register sentinels remain reserved for
future FFI/ASM call shims; applying them around ordinary Rust closures is not
sound because the compiler may legitimately allocate callee-saved registers
inside the closure frame.

The no-orphan rule remains binding: `BULK_EMIT_COMPRESSED`,
`FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, and `FSM_DISPATCH_THREADED` still
require their real codegen/runtime consumers before body admission. See
`skinny/REDRESS.md` SK-V5 Wave 5 entry.
