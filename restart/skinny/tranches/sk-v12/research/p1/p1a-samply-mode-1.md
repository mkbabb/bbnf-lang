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

Exact parse replay rows are enumerated in
`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`. The block
below is the readable command shape only; the TSV carries the concrete corpus,
track, iteration count, cwd, output path, expected return-code policy, and full
command for every row.

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
as". The retained samply lane is artifact-only because it uses `--save-only`;
the retained samply JSON metadata reports `symbolicated=false`, while the
matching `.json.syms.json` sidecars carry symbol maps. The V1 hardening fold
exported all 34 parse Time Profiler bundles to
`/tmp/skv12-p1/parse-xctrace/exports` and parsed them into
`/tmp/skv12-p1/time_profile_hot_leaf_{summary,details}.tsv`. Parse export
coverage is 34,129 sample rows, 34,080 selected target rows, and 99.86%
selected target time after filtering dyld/startup frames.

Parse-only is diagnostic only. `skinny/RESULTS.md` still records `parse_only`
as 16 `S / NO-GO` rows plus `canada` as `L / NO-GO`; none of these rows can
count toward SK-V12 SOTA admission or close.

## §2 - Findings

PMU source: `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`.

Primitive source map:

| Primitive label | Source-level anchor |
|---|---|
| `bounded_plain_string_scan` | `skinny/crates/runtime/src/grammars/json/generated.rs:171`; `skinny/crates/bbnf-bench/src/track2/json.rs:314` |
| `ascii_whitespace_skip` | `skinny/crates/parse-that-regex/src/lib.rs:113` |
| `simd_movemask` | `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`; core `trailing_zeros` support |
| `container_dispatch` | `skinny/crates/runtime/src/grammars/json/generated.rs:47`; `skinny/crates/runtime/src/grammars/json/generated.rs:292`; `skinny/crates/bbnf-bench/src/track2/json.rs:53` |
| `number_digit_span` | `skinny/crates/parse-that-regex/src/number/mod.rs:38`; `skinny/crates/parse-that-regex/src/number/mod.rs:106` |
| `string_escape_decode` | `skinny/crates/parse-that-regex/src/lib.rs:162`; `skinny/crates/parse-that-regex/src/lib.rs:284`; generated/Track 2 `parse_string` callers |
| `unicode_escape_hex_decode` | `skinny/crates/parse-that-regex/src/lib.rs:945`; `skinny/crates/parse-that-regex/src/lib.rs:959` |
| `memory_copy` | core copy leaves when the caller stack resolves to parse copy/support work |
| `runtime_support` | core/runtime support leaves that do not resolve to a narrower behavior primitive in the caller stack |

The table covers every corpus x Track 1/Track 2. Top leaf symbol, `%`
self-time, and file:line are in
`/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`; the row table below names
the leading fresh xctrace self-time families.

| Corpus | Track 1 PMU | Track 1 top self-time families | Track 2 PMU | Track 2 top self-time families |
|---|---:|---|---:|---|
| `twitter` | 2.21 c/B; 16334 Mbps | `bounded_plain_string_scan` 51.6%, `container_dispatch` 23.0%, `simd_movemask` 11.8% | 2.85 c/B; 12864 Mbps | `container_dispatch` 32.8%, `bounded_plain_string_scan` 25.4%, `ascii_whitespace_skip` 19.3% |
| `citm_catalog` | 1.12 c/B; 31987 Mbps | `container_dispatch` 46.0%, `ascii_whitespace_skip` 24.4%, `bounded_plain_string_scan` 20.5% | 1.65 c/B; 22138 Mbps | `container_dispatch` 46.7%, `ascii_whitespace_skip` 24.6%, `bounded_plain_string_scan` 19.0% |
| `canada` | 1.93 c/B; 18309 Mbps | `container_dispatch` 51.5%, `number_digit_span` 39.5%, `ascii_whitespace_skip` 8.9% | 2.08 c/B; 16934 Mbps | `container_dispatch` 54.2%, `number_digit_span` 38.5%, `ascii_whitespace_skip` 7.2% |
| `apache_builds` | 2.74 c/B; 13366 Mbps | `bounded_plain_string_scan` 53.2%, `container_dispatch` 20.1%, `ascii_whitespace_skip` 11.7% | 2.84 c/B; 12867 Mbps | `bounded_plain_string_scan` 42.4%, `container_dispatch` 22.0%, `simd_movemask` 14.1% |
| `github_events` | 2.28 c/B; 16029 Mbps | `bounded_plain_string_scan` 40.1%, `container_dispatch` 27.4%, `simd_movemask` 13.4% | 2.66 c/B; 13762 Mbps | `container_dispatch` 32.8%, `bounded_plain_string_scan` 27.1%, `simd_movemask` 16.1% |
| `update_center` | 2.89 c/B; 12516 Mbps | `bounded_plain_string_scan` 55.0%, `container_dispatch` 21.7%, `simd_movemask` 13.4% | 3.73 c/B; 9784 Mbps | `bounded_plain_string_scan` 47.1%, `container_dispatch` 19.7%, `simd_movemask` 17.5% |
| `mesh` | 2.65 c/B; 13334 Mbps | `container_dispatch` 55.7%, `number_digit_span` 32.5%, `ascii_whitespace_skip` 11.7% | 2.80 c/B; 12552 Mbps | `container_dispatch` 56.9%, `number_digit_span` 32.7%, `ascii_whitespace_skip` 10.3% |
| `random` | 3.52 c/B; 10281 Mbps | `bounded_plain_string_scan` 50.0%, `container_dispatch` 27.3%, `ascii_whitespace_skip` 13.1% | 4.41 c/B; 8245 Mbps | `bounded_plain_string_scan` 41.5%, `container_dispatch` 33.9%, `ascii_whitespace_skip` 11.6% |
| `gsoc-2018` | 1.48 c/B; 24009 Mbps | `simd_movemask` 39.6%, `container_dispatch` 24.3%, `bounded_plain_string_scan` 23.1% | 1.57 c/B; 22634 Mbps | `simd_movemask` 39.0%, `bounded_plain_string_scan` 19.8%, `runtime_support` 17.5% |
| `marine_ik` | 2.56 c/B; 13674 Mbps | `container_dispatch` 60.0%, `number_digit_span` 26.8%, `ascii_whitespace_skip` 10.8% | 2.80 c/B; 12573 Mbps | `container_dispatch` 58.6%, `number_digit_span` 31.3%, `ascii_whitespace_skip` 8.0% |
| `instruments` | 2.03 c/B; 17458 Mbps | `bounded_plain_string_scan` 36.6%, `container_dispatch` 33.0%, `ascii_whitespace_skip` 19.3% | 2.93 c/B; 12318 Mbps | `bounded_plain_string_scan` 38.9%, `container_dispatch` 28.2%, `ascii_whitespace_skip` 17.8% |
| `numbers` | 1.74 c/B; 19951 Mbps | `number_digit_span` 52.0%, `container_dispatch` 43.9%, `ascii_whitespace_skip` 3.9% | 1.81 c/B; 19267 Mbps | `number_digit_span` 49.3%, `container_dispatch` 47.2%, `ascii_whitespace_skip` 3.3% |
| `unicode_mixed` | 4.30 c/B; 8412 Mbps | `string_escape_decode` 36.2%, `container_dispatch` 32.2%, `simd_movemask` 22.8% | 3.89 c/B; 9259 Mbps | `container_dispatch` 35.1%, `string_escape_decode` 31.9%, `simd_movemask` 22.5% |
| `unicode_escapes` | 2.82 c/B; 12660 Mbps | `unicode_escape_hex_decode` 38.1%, `container_dispatch` 32.8%, `string_escape_decode` 23.7% | 2.73 c/B; 13129 Mbps | `unicode_escape_hex_decode` 38.0%, `container_dispatch` 31.1%, `string_escape_decode` 22.5% |
| `unicode_basic` | 2.86 c/B; 12297 Mbps | `bounded_plain_string_scan` 30.5%, `container_dispatch` 27.6%, `simd_movemask` 21.7% | 3.23 c/B; 10914 Mbps | `bounded_plain_string_scan` 32.1%, `container_dispatch` 30.7%, `simd_movemask` 21.1% |
| `distinct_values` | 3.58 c/B; 9957 Mbps | `bounded_plain_string_scan` 65.7%, `container_dispatch` 17.3%, `ascii_whitespace_skip` 8.1% | 5.68 c/B; 6355 Mbps | `bounded_plain_string_scan` 65.1%, `container_dispatch` 15.4%, `simd_movemask` 10.5% |
| `y_string_unicode` | 5.62 c/B; 6282 Mbps | `unicode_escape_hex_decode` 45.2%, `container_dispatch` 31.1%, `bounded_plain_string_scan` 7.0% | 5.90 c/B; 6072 Mbps | `unicode_escape_hex_decode` 46.4%, `container_dispatch` 29.1%, `string_escape_decode` 8.4% |

## §3 - Delta vs SK-V11

This is a diagnostic profile delta, not an admission delta. P1-F owns row-level
Mbps deltas against the prior SK close.

The behavior parser source is unchanged from the SK-V11 open profile except for
bench gate/report files, and the fresh SK-V12 Time Profiler export confirms the
same grammar-neutral family shape:

- Bounded string rows stay anchored on `bounded_plain_string_scan`,
  `container_dispatch`, `ascii_whitespace_skip`, and `simd_movemask`.
- Numeric rows stay anchored on `number_digit_span` and `container_dispatch`.
- Unicode rows stay anchored on `unicode_escape_hex_decode`,
  `string_escape_decode`, and `container_dispatch`.

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
- The self-time percentages come from exported xctrace Time Profiler XML, not
  from samply JSON. The companion details TSV carries the exact top leaf
  symbol and file:line for each parse row and track.
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
- `/tmp/skv12-p1/parse-xctrace/exports/*.time-profile.xml`
- `/tmp/skv12-p1/parse-xctrace/cpu-counters/*.trace`
- `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv`
- `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
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
