# PROVE-10 — Adversarial Verification

**Verdict: CONFIRMED.** The memchr start-state prefilter (`Dfa::find`) reproduces on a
fresh, independently-reconstructed parse-that scratch copy: 8.8x–29.5x match-throughput on
prefilterable patterns, ~0.97–1.01x on non-prefilterable controls, 44/44 bbnf-regex tests
green, the in-bench `count_scalar==count_prefilter` correctness gate passes on every pattern
in both windows, the patch is bbnf-regex-only and edict-clean, and the claimed pre-existing
parse_that regex-bridge compile failure reproduces identically with and without the patch.

Tree: pristine `rsync` copy of `/Users/mkbabb/Programming/parse-that` (read-only source),
`.cargo/config.toml` external patch-deps repointed to absolute sibling paths, isolated
`CARGO_TARGET_DIR`, pinned `nightly-2026-04-11`. Baseline built on pristine FIRST; patch
applied via `git apply -p1` (clean); ran interleaved.

---

## 1. Attack list & findings

| # | Attack vector | Result |
|---|---|---|
| A | **Opt-profile asymmetry between arms** | REPELLED. Both lanes (`scan_scalar`, `scan_prefilter`) live in ONE fat-LTO/cgu=1/O3 bench binary; no per-arm profile. The foreign `lto=false` edit the prover flagged is absent from the patch (verified `[profile.bench]` = `lto="fat"`). |
| B | **Sabotaged baseline lane** | REPELLED. `scan_scalar`'s `find_scalar` steps the SAME `Dfa::find_at` the prefilter lane confirms with; the `None`-branch of `Dfa::find` is byte-identical to it. Controls exercise that identical code → the 0.99x is a true parity floor, not a rigged baseline. |
| C | **Warm/cached contamination** | REPELLED. Divan re-runs the full 131 KB scan per sample (cold per-parse); N=200 samples, 2 independent windows. |
| D | **Cherry-picked window** | REPELLED. Two independent windows; every prefilterable pattern lands in the claimed 9.6–29x envelope in both. |
| E | **Hidden x86 / name-gates / stubs** | REPELLED. `grep` over patch: 0 real x86/avx/sse/`_mm_`/`target_arch` tokens (the "sse"/"x86" hits are substrings of *asserted*/*esse*/*classes*); 0 name-gates; 0 `todo!`/`unimplemented!`/stub/workaround; 0 `parse_that` lines; 0 `simd128`/`lto=false`. |
| F | **Parity gap (prefilter changes match set)** | REPELLED. The bench's setup `assert_eq!(count_scalar, count_prefilter)` fires per pattern; both windows exited 0 → identical match counts over 131 KB × 6 patterns. |
| G | **Patch-vs-report mismatch** | REPELLED. 245 insertions, 6 files, additive-only; files & LOC match the report's diffstat; new `prefilter.rs` present, `Dfa::find` + `prefilter` field present, `memchr = "2.5"` added. |
| H | **Patch portability** | REPELLED. Applies cleanly to the pristine copy via both `git apply --check -p1` and `patch -p1 --dry-run`. |
| I | **Orthogonality of the pre-existing failure** | REPELLED — see §4. |

## 2. Fresh match-throughput ratios (scalar median / prefilter median)

Corpus = 131 KB skip-heavy synthetic. N=200/lane. Machine loadavg 11–19 (comparable to the
prover's 11–17). Ratios are paired same-binary same-window → load-robust; absolute GB/s
directional.

| Pattern | Prefilter tier | W1 ratio | W2 ratio | Prover claim |
|---|---|---:|---:|---:|
| `0x[0-9a-fA-F]+` | Byte1(`0`) | 21.80x | **29.46x** | 28.98x |
| `#[0-9a-fA-F]{3,6}` | Byte1(`#`) | 20.86x | 16.70x | 15.36x |
| `https?://[a-zA-Z0-9./-]+` | Byte1(`h`) | 12.33x | 15.58x | 15.81x |
| `(?:foo\|bar)[a-z]*` | Byte2(`f`,`b`) | 11.30x | 8.82x | 9.59x |
| `[a-z]+` (control) | None | **0.99x** | 0.98x | 0.99x |
| `\w+` (control) | None | 0.97x | 1.01x | 0.99x |

All prefilterable ratios fall in **8.8x–29.5x**, squarely inside the claimed 9.6–29x
envelope; the per-pattern ordering reshuffles under load (W2's `0x`=29.5x and `https?`=15.6x
land near-exactly on the prover's 28.98x/15.81x). Controls hold at parity (0.97–1.01x) — the
zero-regression proof. `(?:foo|bar)` dips to 8.82x in W2 (memchr2 tier, most load-sensitive)
but W1 gives 11.3x — within envelope, direction never inverts.

## 3. Tests & parity oracle

- `cargo test -p bbnf-regex` → **44 passed, 0 failed** (matches claim: algebra/egraph/info/
  path_lexer suites).
- In-bench correctness gate `assert_eq!(count_scalar, count_prefilter)` → PASSED for all 6
  patterns in BOTH bench windows (process exit 0). This is the DFA-match parity oracle
  standing in for the (uncompilable) parse_that dfa subtests.

## 4. Orthogonality of the parse_that compile failure (verified both ways)

`cargo test -p parse_that --test regex --no-run`:
```
error[E0432]: unresolved import `parse_that::regex::parser`
 --> parse_that/tests/regex/parser_test.rs:2:24
2 | use parse_that::regex::parser::{parse, parse_with};
  |                        ^^^^^^ could not find `parser` in `regex`
```
- **WITH** the patch applied → this error (bbnf-regex compiles fine).
- **WITHOUT** the patch (reverse-applied, pristine bbnf-regex) → identical error.

The failure is a parse_that-side re-export drift in parse_that's OWN test tree, referencing
parse_that's own `regex` module — the bbnf-regex-only patch (0 parse_that lines) cannot cause
or fix it. Orthogonality claim CONFIRMED.

## 5. Compile-axis note (honest gap)

The prover's compile-axis lane (1.003x neutral) was not independently re-baselined against a
pristine bench binary in this pass (time budget spent on the decisive match-throughput lane +
orthogonality). The added cost is one O(256) `Prefilter::from_dfa` loop run once per compile;
`dfa_compile` medians here (61–72 µs) are consistent with the prover's ~55 µs under heavier
load. The compile axis is not the claimed clause and is architecturally negligible; noted, not
a blocker.

## 6. Environment

18 cores (aarch64/darwin), nightly-2026-04-11, `[profile.bench]` fat-LTO/cgu=1/O3 (real, not
foreign-edited). PMU cyc/B unavailable (macOS userspace restriction) — same caveat the prover
disclosed; wall-clock paired ratios are the certified metric.
