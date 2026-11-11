# LW1.PERF-1: every dispatch tier is reachable on a default build

**Branch** `sk-v27-rust-perf`, base `b4e635d3e` (`origin/sk-v27`). **Crate** `crates/simd-scan`.
**Logs** `~/.dev-logs/bbnf-lockstep/rust-perf-lw1/` (names given per row).
**Toolchain** pinned `nightly-2026-04-11` (rustc 1.96.0-nightly). **Host** Apple M5 Max, macOS 26.4.
x86_64 execution runs under Rosetta 2, which advertises SSE4.2 at most (no AVX, no AVX2). AVX2 is executed under Miri (`x86_64-unknown-linux-gnu`).

## Falsifiers (born RED, each RED x2 for its stated reason)

| id | test | stated reason | RED evidence |
|---|---|---|---|
| R1 | `tests/dispatch_tiers.rs::every_isa_tier_of_the_target_is_compiled`, default `x86_64-apple-darwin` build | `#![cfg(all(target_arch = "x86_64", target_feature = "avx2"))]` (and the same for `avx512vbmi2`) removes both x86 kernels from every build without `RUSTFLAGS`; `compiled = [Scalar]` | `perf1-red-x86-run{1,2}.log`, rc 101 x2: "avx2 kernel is not compiled into a default x86_64 build (cfg(target_feature) gate); compiled = [Scalar]" |
| R2 | `dispatch_tiers.rs::every_supported_tier_matches_the_scalar_oracle`, native aarch64 | the NEON wide-LUT and multi-cmp epilogues ignore backslash escapes, digraphs and per-class quote state, so they diverge from `scalar::scan` | `precure-native-aarch64-run{1,2}.log`, rc 101 x2: "tier neon diverges from scalar (wide alphabet)" |
| R2x | the same oracle test, AVX2 forced (`RUSTFLAGS=+avx2,+pclmulqdq,+bmi2`) under Miri | the AVX2 tail toggles quote parity without escape parity; its stripe path folds all quote classes into one parity | `precure-avx2-miri.log`, rc 101: "tier avx2 diverges from scalar" |
| R3 | `tests/guard_page.rs::no_tier_reads_past_the_end_of_its_input` (input ends at a `PROT_NONE` page) | digraph second-byte loads read `ptr + W + 1` while the loop bound is only `i + W <= len`: a one-byte overread past the slice (UB) | `precure-native-aarch64-run{1,2}.log`: guard_page aborted with "signal: 10, SIGBUS: access to undefined memory" x2 |

The same digraph overread is in the AVX2 and AVX-512 stripe loops. Neither could fault on this host before the cure, because neither was compiled in.

## Cure

- **`src/dispatch.rs`** (new). `Tier {Scalar, Neon, Avx2, Avx512, Simd128}`:
  - `is_compiled` is const and depends on `target_arch` only.
  - `is_supported` uses runtime detection: `is_x86_feature_detected!` for `avx2`, and for `avx512f`, `avx512bw` and `avx512vbmi2`.
  - `selected_tier` is detected once and held in a `OnceLock`.
  - `scan_with(tier, …) -> Option<StructuralIndex>` runs the tier against the oracle. `scan_structural` runs `selected_tier`.
- **`src/avx2.rs`** and **`src/avx512.rs`** are gated `cfg(target_arch = "x86_64")` only. Their bodies carry `#[target_feature(enable = "avx2")]` and `#[target_feature(enable = "avx512f,avx512bw,avx512vbmi2")]`. Their entry points are `unsafe fn`, called only after detection.
- **`src/stripe.rs`** (new, arch-neutral) is the shared semantics of every SIMD tier:
  - `QuoteState::body` holds escape parity, the single-class prefix-XOR and the multi-class serial walk for any region of 1 to 64 bytes.
  - `epilogue` is the byte-at-a-time tail, carrying the vector loop's state.
  - `region_fits` adds one lookahead byte when the alphabet has digraphs. This cures R3.
  - A kernel now produces only raw masks, so the semantics are written once, not re-implemented per ISA. This cures R2 and R2x.
- **NEON** keeps its classifiers and movemask unchanged. Its three drivers share `finish_region`, `digraph_region` and `raw_quotes_and_bs`, built on `stripe`. The diff is 530 lines in and out.
- **AVX2** fix: the nibble classifier used a signed `_mm256_cmpgt_epi8(matched, 0)`, which drops singleton 7 (LUT bit `0x80`). It now uses `cmpeq` against zero, inverted. The `bbnf` 8-singleton alphabet was added to the oracle test to hold it.
- **`Cargo.toml`:** the inert `avx512` cargo feature is deleted (it gated nothing; there is no compat shim). `libc` is added as a unix dev-dependency for the guard-page test.

## GREEN x2

| run | where | result | log |
|---|---|---|---|
| native aarch64, all simd-scan tests (incl. fuzz, correctness, dispatch, guard page) | host | rc 0; 41 passed / 0 failed | `cure-native-try2.log` |
| default x86_64 build, dispatch + guard page + fuzz + correctness | Rosetta | rc 0 x2; 25 passed x2. R1 GREEN: Avx2 and Avx512 compiled. Selected tier = scalar, because Rosetta has no AVX2. | `cure-x86-rosetta-run{1,2}.log` |
| AVX2 tier vs scalar oracle (4 alphabets x 141 lengths) | Miri, x86_64-linux, `+avx2` (detection only; the kernel bodies are the same `#[target_feature]` code a default build dispatches to) | rc 0 x2; "tiers held to the oracle: [avx2, scalar]"; selected tier avx2; Miri reports no UB | `cure-avx2-miri-run{1,2}.log` |
| clippy, aarch64 + x86_64 `--all-targets`, wasm32 `--lib` | host | 0 errors; only warnings that were already there (doc-list indentation, byte-str style) | — |

## What is not proved here (carried, with owners)

- **AVX-512 has not been executed anywhere.** Miri stops at `llvm.x86.avx512.mask.compress.b.512` (unsupported; `cure-avx512-miri-run1.log`). Every op before the compress ran. Under X-4 the tier needs a CI arm on AVX-512 hardware or under Intel SDE, or it is deleted. Owner: LW2.PERF-2 / LW2.TOPO-3.
- **AVX2 on real silicon:** this has been checked under Miri only. The `ubuntu-latest` CI arm is owed in LW2.TOPO-3.
- **NEON under Miri:** Miri lacks `llvm.aarch64.neon.pmull64` (`cure-neon-miri-run1.log`). The guard-page test and the oracle stand in for it.
- **wasm32:** `src/wasm.rs` was not ported to `stripe` and is untested; there is no wasm runtime on the host. LW2.PERF-1 owns it.
- **CLMUL prefix-XOR:** `parity::prefix_xor_impl` still selects PCLMULQDQ or PMULL by `cfg(target_feature)`. On a default x86_64 or aarch64-linux build it is the 6-op shift-XOR, which is correct but costs a few cycles per stripe. Owner: LW2.PERF-1 (the `OnceLock` table).
- **`compaction::compact_stripe_pext`** is never called, and despite its name it is a tzcnt loop. It is dead code for LW7 deletion.
- **Corpora:** `tests/correctness.rs` and the bench read `data/{css,json}`, which `.gitignore` excludes, so they are not hermetic. For this run they were linked in from the main checkout. Owner: LW1.COMP-4 / LW1.CON-1.

## Bench

See the `LW1-PERF-1` rows in [`BENCH-LW1.md`](BENCH-LW1.md).
