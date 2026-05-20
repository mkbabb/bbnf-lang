# SK-V12 P1-A: Parse-Only Diagnostic Profile

Pass: S-P1 Profile. Cycle: V12.
Date: 2026-05-20.
Scope: parse-only diagnostic profile for generated Track 1 and independent
Track 2 across all 17 JSON corpora.
Output: this file.
Baseline: SK-V12-open (`50bd1648`).
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: release profile with debug symbols; `RUSTFLAGS="-C target-cpu=native"`;
target directory `/tmp/skv12-profile-target-50bd1648`.
Profile tool: `samply 0.13.1`, `xcrun xctrace` Time Profiler / CPU
Counters, and `xctrace_probe` PMU rows.
Corpus coverage: parse-only Track 1/Track 2 17/17.

Shared capture provenance:

- Run locator: `sk-v12-open:50bd1648:/tmp/skv12-p1`.
- Capture root: `/tmp/skv12-p1`.
- Binaries:
  `/tmp/skv12-profile-target-50bd1648/release/xctrace_probe` and
  `/tmp/skv12-profile-target-50bd1648/release/profile_direct`.
- Toolchain: `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM 22.1.2.
- Behavior-source check: `git diff --name-only 3ce75df4..HEAD -- skinny/crates
  skinny/Cargo.toml skinny/Cargo.lock` returns only
  `skinny/crates/bbnf-bench/src/bin/gate.rs` and
  `skinny/crates/bbnf-bench/src/report.rs`; parse behavior source is
  code-identical to the SK-V11 open profile. `git diff --name-only
  50bd1648..HEAD -- skinny/crates skinny/Cargo.toml skinny/Cargo.lock` is empty.

## §1 - Method

Build command:

```bash
cd /Users/mkbabb/Programming/bbnf-lang
CARGO_TARGET_DIR=/tmp/skv12-profile-target-50bd1648 \
RUSTFLAGS="-C target-cpu=native" \
  cargo build --manifest-path skinny/Cargo.toml --release \
  -p bbnf-bench --bin xctrace_probe --bin profile_direct
```

Inspection commands:

```bash
awk -F '\t' 'NR>1 {c[$1 FS $4 FS $5]++} END {for (k in c) print c[k], k}' \
  /tmp/skv12-p1/pmu/capture_status.tsv

find /tmp/skv12-p1/samply/parse -maxdepth 1 -name '*.json.gz' | wc -l
find /tmp/skv12-p1/samply/parse -maxdepth 1 -name '*.json.syms.json' | wc -l
find /tmp/skv12-p1/parse-xctrace/time-profiler -maxdepth 1 -name '*.trace' | wc -l
find /tmp/skv12-p1/parse-xctrace/cpu-counters -maxdepth 1 -name '*.trace' | wc -l
find /tmp/skv12-p1/parse-xctrace -path '*exports*' -type f | wc -l
sed -n '1,80p' /tmp/skv12-p1/pmu/parse_pmu_rows.tsv
```

Retained capture command shape:

```bash
xcrun xctrace record \
  --template "Time Profiler" \
  --time-limit 1000ms \
  --output /tmp/skv12-p1/parse-xctrace/time-profiler/<corpus>__<track>.trace \
  --launch -- \
  /tmp/skv12-profile-target-50bd1648/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <track1-or-track2> <iters>

xcrun xctrace record \
  --template "CPU Counters" \
  --time-limit 1000ms \
  --output /tmp/skv12-p1/parse-xctrace/cpu-counters/<corpus>__<track>.trace \
  --launch -- \
  /tmp/skv12-profile-target-50bd1648/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <track1-or-track2> <iters>

samply record --save-only --unstable-presymbolicate \
  -o /tmp/skv12-p1/samply/parse/<corpus>__<track>.json.gz \
  -- \
  /tmp/skv12-profile-target-50bd1648/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <track1-or-track2> <iters>
```

Status facts from `/tmp/skv12-p1/pmu/capture_status.tsv`:

| Capture family | Parse artifacts | Return codes |
|---|---:|---|
| `pmu-parse` | 34/34 | all `rc=0` |
| `samply-parse` | 34/34 | all `rc=0` |
| `xctrace-time-profiler-parse` | 34/34 | all `rc=54` |
| `xctrace-cpu-counters-parse` | 34/34 | all `rc=54` |

The `rc=54` xctrace rows are retained 1000 ms time-limit traces, not missing
captures; the logs say "Reached specified time limit" and "Output file saved
as". The retained samply JSON metadata reports `symbolicated=false`, while the
matching `.json.syms.json` sidecars carry symbol maps. No retained
`/tmp/skv12-p1/parse-xctrace/exports/*.symbols.json` or summary export exists
in the SK-V12 capture root, so this file does not claim fresh top-leaf
percentages. The primitive interpretation below is source-level carryover from
SK-V11 P1-A/P1 hardening, justified only by the unchanged parse behavior
source. Fresh SK-V12 quantitative evidence is the PMU table.

Parse-only is diagnostic only. `skinny/RESULTS.md` still records `parse_only`
as 16 `S / NO-GO` rows plus `canada` as `L / NO-GO`; none of these rows can
count toward SK-V12 SOTA admission or close.

## §2 - Findings

PMU source: `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`.

Primitive source map:

| Primitive label | Source-level anchor |
|---|---|
| `string_tiny_scan` | `skinny/crates/runtime/src/grammars/json/generated.rs:171`; `skinny/crates/bbnf-bench/src/track2/json.rs:314` |
| `whitespace_skip` | `skinny/crates/parse-that-regex/src/lib.rs:113` |
| `simd_movemask` | `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4` |
| `dispatch_walk` | `skinny/crates/runtime/src/grammars/json/generated.rs:47`; `skinny/crates/bbnf-bench/src/track2/json.rs:53` |
| `number_scan` / `number_digit_scan` | `skinny/crates/parse-that-regex/src/number/mod.rs:38`; `skinny/crates/parse-that-regex/src/number/mod.rs:106` |
| `container_next` / `key_colon` | `skinny/crates/runtime/src/grammars/json/generated.rs:348`; `skinny/crates/bbnf-bench/src/track2/json.rs:271`; `skinny/crates/bbnf-bench/src/track2/json.rs:97` |
| `string_full_scan` / `string_escape` | `skinny/crates/parse-that-regex/src/lib.rs:162`; `skinny/crates/parse-that-regex/src/lib.rs:284` |
| `unicode_escape_hex` | `skinny/crates/parse-that-regex/src/lib.rs:945`; `skinny/crates/parse-that-regex/src/lib.rs:959` |
| `memcpy` / `trailing_zeros` | Rust core-library leaves surfaced in SK-V11/SK-V12 symbol maps |

The table covers every corpus x Track 1/Track 2. Primitive labels are
diagnostic source interpretation, not fresh SK-V12 self-time percentages.

| Corpus | Track 1 PMU | Track 1 diagnostic primitives | Track 2 PMU | Track 2 diagnostic primitives |
|---|---:|---|---:|---|
| `twitter` | 2.21 c/B; 16334 Mbps | `string_tiny_scan`, `whitespace_skip`, `dispatch_walk` | 2.85 c/B; 12864 Mbps | `string_tiny_scan`, `whitespace_skip`, `simd_movemask` |
| `citm_catalog` | 1.12 c/B; 31987 Mbps | `whitespace_skip`, `string_tiny_scan`, `memcpy` | 1.65 c/B; 22138 Mbps | `whitespace_skip`, `string_tiny_scan`, `container_next` |
| `canada` | 1.93 c/B; 18309 Mbps | `number_digit_scan`, `memcpy`, `dispatch_walk` | 2.08 c/B; 16934 Mbps | `number_digit_scan`, `dispatch_walk`, `number_scan` |
| `apache_builds` | 2.74 c/B; 13366 Mbps | `string_tiny_scan`, `whitespace_skip`, `simd_movemask` | 2.84 c/B; 12867 Mbps | `string_tiny_scan`, `whitespace_skip`, `simd_movemask` |
| `github_events` | 2.28 c/B; 16029 Mbps | `string_tiny_scan`, `simd_movemask`, `whitespace_skip` | 2.66 c/B; 13762 Mbps | `string_tiny_scan`, `simd_movemask`, `whitespace_skip` |
| `update_center` | 2.89 c/B; 12516 Mbps | `string_tiny_scan`, `simd_movemask`, `dispatch_walk` | 3.73 c/B; 9784 Mbps | `string_tiny_scan`, `simd_movemask`, `dispatch_walk` |
| `mesh` | 2.65 c/B; 13334 Mbps | `number_digit_scan`, `dispatch_walk`, `whitespace_skip` | 2.80 c/B; 12552 Mbps | `dispatch_walk`, `number_digit_scan`, `whitespace_skip` |
| `random` | 3.52 c/B; 10281 Mbps | `string_tiny_scan`, `whitespace_skip`, `simd_movemask` | 4.41 c/B; 8245 Mbps | `string_tiny_scan`, `whitespace_skip`, `key_colon` |
| `gsoc-2018` | 1.48 c/B; 24009 Mbps | `simd_movemask`, `string_tiny_scan`, `trailing_zeros` | 1.57 c/B; 22634 Mbps | `simd_movemask`, `string_tiny_scan`, `trailing_zeros` |
| `marine_ik` | 2.56 c/B; 13674 Mbps | `dispatch_walk`, `number_digit_scan`, `memcpy` | 2.80 c/B; 12573 Mbps | `dispatch_walk`, `number_digit_scan`, `number_scan` |
| `instruments` | 2.03 c/B; 17458 Mbps | `string_tiny_scan`, `whitespace_skip`, `dispatch_walk` | 2.93 c/B; 12318 Mbps | `whitespace_skip`, `string_tiny_scan`, `simd_movemask` |
| `numbers` | 1.74 c/B; 19951 Mbps | `number_digit_scan`, `dispatch_walk`, `container_next` | 1.81 c/B; 19267 Mbps | `number_digit_scan`, `dispatch_walk`, `number_scan` |
| `unicode_mixed` | 4.30 c/B; 8412 Mbps | `dispatch_walk`, `string_escape`, `string_full_scan` | 3.89 c/B; 9259 Mbps | `dispatch_walk`, `string_escape`, `simd_movemask` |
| `unicode_escapes` | 2.82 c/B; 12660 Mbps | `unicode_escape_hex`, `string_full_scan`, `dispatch_walk` | 2.73 c/B; 13129 Mbps | `unicode_escape_hex`, `dispatch_walk`, `string_full_scan` |
| `unicode_basic` | 2.86 c/B; 12297 Mbps | `string_tiny_scan`, `trailing_zeros`, `dispatch_walk` | 3.23 c/B; 10914 Mbps | `string_tiny_scan`, `trailing_zeros`, `dispatch_walk` |
| `distinct_values` | 3.58 c/B; 9957 Mbps | `string_tiny_scan`, `whitespace_skip`, `dispatch_walk` | 5.68 c/B; 6355 Mbps | `string_tiny_scan`, `trailing_zeros`, `whitespace_skip` |
| `y_string_unicode` | 5.62 c/B; 6282 Mbps | `unicode_escape_hex`, `string_tiny_scan` | 5.90 c/B; 6072 Mbps | `unicode_escape_hex`, `string_tiny_scan` |

## §3 - Delta vs SK-V11

This is a diagnostic profile delta, not an admission delta. P1-F owns row-level
Mbps deltas against the prior SK close.

The behavior parser source is unchanged from the SK-V11 open profile except for
bench gate/report files, so the source-level primitive shape carries forward:

- String/object-heavy rows stay anchored on `string_tiny_scan`,
  `whitespace_skip`, `simd_movemask`, and dispatch/key-colon leaves.
- Numeric rows stay anchored on `number_digit_scan`, `number_scan`,
  `dispatch_walk`, and occasional `memcpy`.
- Unicode rows stay anchored on `unicode_escape_hex`, `string_escape`, and
  `string_full_scan`.

The SK-V12 seed result surface is unchanged from SK-V11 close:
`parse_only` remains diagnostic at 16 `S / NO-GO` plus `canada` as
`L / NO-GO`; direct residual rows remain pre-blocked by REDRESS 119/120; and
SK-V12's first material target remains a generated non-JSON baseline, not a
parse-only or JSON-direct retry.

## §4 - Anomalies + Masking Signals

- No parse-only row admits. The high PMU rows (`citm_catalog`, `gsoc-2018`,
  `numbers`, and `canada`) are parser-health signals only.
- Worst fresh c/B rows are `y_string_unicode` Track 2 at 5.90 c/B,
  `y_string_unicode` Track 1 at 5.62 c/B, `distinct_values` Track 2 at
  5.68 c/B, `random` Track 2 at 4.41 c/B, and `unicode_mixed` Track 1 at
  4.30 c/B. These are S-P2 clues, not SK-V12 admission evidence.
- `unicode_mixed` and `unicode_escapes` show Track 2 faster than Track 1 in the
  PMU wrapper; that is a diagnostic wrapper/workload signal and does not move
  `skinny/RESULTS.md`.
- The absent retained xctrace export set is a method caveat. The trace bundles
  exist and can be opened/exported, but this artifact does not turn ad-hoc
  stdout exports into citable per-row self-time percentages.
- REDRESS 102 and REDRESS 119/120 keep parse-only movement, W3 substrate
  routes, sidecar/cursor variants, decoded-byte/materialization routes, and
  JSON direct residual retries pre-blocked unless a later pass supplies fresh
  material product-plane evidence.

## §5 - Sources

- `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`
- `/tmp/skv12-p1/pmu/capture_status.tsv`
- `/tmp/skv12-p1/samply/parse/*.json.gz`
- `/tmp/skv12-p1/samply/parse/*.json.syms.json`
- `/tmp/skv12-p1/parse-xctrace/time-profiler/*.trace`
- `/tmp/skv12-p1/parse-xctrace/cpu-counters/*.trace`
- `/tmp/skv12-p1/logs/xctrace-time-parse-*.log.out`
- `/tmp/skv12-p1/logs/xctrace-cpu-parse-*.log.out`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v11/research/p1/hardening/V4/CH4.md`
- `restart/skinny/tranches/sk-v11/research/p1/hardening/V4/CH5.md`
