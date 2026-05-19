# SK-V10 P1-D: PMU And Cycles-Per-Byte

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-19.
Scope: fresh PMU counters and cycles-per-byte for the diagnostic parse lane;
record product-plane PMU instrumentation absence honestly.
Output: this file.
Baseline: SK-V10 Alpha inherits W1-rendered `SK-V9-open`, run
`sk-v9-open:criterion-fnv64-a1e8a51ae806d386`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile, debug symbols.
Profile tool: `xcrun xctrace record --template "CPU Counters"` plus
`proc_pid_rusage(RUSAGE_INFO_V5)` emitted by
`skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`.
Corpus coverage: parse-only Track 1/Track 2 17/17; direct and typed PMU
0/23 because no existing direct/typed PMU probe emits `PROBE_RESULT` counters.

## Section 1 - Method

Commands:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin profile_direct --bin xctrace_probe

xcrun xctrace record \
  --template "CPU Counters" \
  --no-prompt \
  --output /tmp/skv10-p1/parse-xctrace/cpu-counters/<corpus>__<track>.trace \
  --launch -- \
  /Users/mkbabb/Programming/bbnf-lang/skinny/target/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <track1-or-track2> <iters>
```

The wrapper appended each probe's stdout `PROBE_RESULT` line to
`/tmp/skv10-p1/parse-xctrace/pmu_rows.tsv`. `xctrace_probe` reads
`ri_cycles` and `ri_instructions` before and after the parse loop; cycles/B and
CPI below are not inferred from wall clock.

The `update_center` fixture is hyphenated on disk and appears in this table as
`update-center`. RESULTS/HANDOFF row identity remains `update_center`.

## Section 2 - Findings

| Corpus | Track | Mbps | cycles/B | CPI | Trace |
|---|---|---:|---:|---:|---|
| `twitter` | Track 1 | 16136 | 2.208 | 0.196 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/twitter__track1.trace` |
| `twitter` | Track 2 | 12317 | 2.842 | 0.262 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/twitter__track2.trace` |
| `citm_catalog` | Track 1 | 31260 | 1.110 | 0.145 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/citm_catalog__track1.trace` |
| `citm_catalog` | Track 2 | 20972 | 1.650 | 0.208 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/citm_catalog__track2.trace` |
| `canada` | Track 1 | 17653 | 1.953 | 0.118 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/canada__track1.trace` |
| `canada` | Track 2 | 16808 | 2.052 | 0.125 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/canada__track2.trace` |
| `apache_builds` | Track 1 | 12343 | 2.783 | 0.219 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/apache_builds__track1.trace` |
| `apache_builds` | Track 2 | 12192 | 2.819 | 0.239 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/apache_builds__track2.trace` |
| `github_events` | Track 1 | 15183 | 2.253 | 0.206 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/github_events__track1.trace` |
| `github_events` | Track 2 | 12947 | 2.652 | 0.253 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/github_events__track2.trace` |
| `update_center` | Track 1 | 11927 | 2.879 | 0.200 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/update-center__track1.trace` |
| `update_center` | Track 2 | 9222 | 3.718 | 0.269 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/update-center__track2.trace` |
| `mesh` | Track 1 | 12907 | 2.660 | 0.134 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/mesh__track1.trace` |
| `mesh` | Track 2 | 12553 | 2.735 | 0.137 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/mesh__track2.trace` |
| `random` | Track 1 | 9786 | 3.504 | 0.185 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/random__track1.trace` |
| `random` | Track 2 | 7773 | 4.408 | 0.239 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/random__track2.trace` |
| `gsoc-2018` | Track 1 | 22760 | 1.508 | 0.223 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/gsoc-2018__track1.trace` |
| `gsoc-2018` | Track 2 | 21890 | 1.569 | 0.240 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/gsoc-2018__track2.trace` |
| `marine_ik` | Track 1 | 12886 | 2.621 | 0.143 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/marine_ik__track1.trace` |
| `marine_ik` | Track 2 | 12107 | 2.817 | 0.154 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/marine_ik__track2.trace` |
| `instruments` | Track 1 | 16940 | 2.021 | 0.159 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/instruments__track1.trace` |
| `instruments` | Track 2 | 11702 | 2.936 | 0.231 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/instruments__track2.trace` |
| `numbers` | Track 1 | 18756 | 1.811 | 0.143 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/numbers__track1.trace` |
| `numbers` | Track 2 | 17309 | 1.965 | 0.154 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/numbers__track2.trace` |
| `unicode_mixed` | Track 1 | 7847 | 4.378 | 0.368 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/unicode_mixed__track1.trace` |
| `unicode_mixed` | Track 2 | 8623 | 3.982 | 0.345 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/unicode_mixed__track2.trace` |
| `unicode_escapes` | Track 1 | 11579 | 2.962 | 0.228 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/unicode_escapes__track1.trace` |
| `unicode_escapes` | Track 2 | 11747 | 2.888 | 0.223 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/unicode_escapes__track2.trace` |
| `unicode_basic` | Track 1 | 11937 | 2.878 | 0.195 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/unicode_basic__track1.trace` |
| `unicode_basic` | Track 2 | 10762 | 3.185 | 0.216 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/unicode_basic__track2.trace` |
| `distinct_values` | Track 1 | 9509 | 3.604 | 0.193 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/distinct_values__track1.trace` |
| `distinct_values` | Track 2 | 6070 | 5.655 | 0.299 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/distinct_values__track2.trace` |
| `y_string_unicode` | Track 1 | 6126 | 5.613 | 0.236 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/y_string_unicode__track1.trace` |
| `y_string_unicode` | Track 2 | 5839 | 5.888 | 0.250 | `/tmp/skv10-p1/parse-xctrace/cpu-counters/y_string_unicode__track2.trace` |

The table again shows the wide-issue host signature: CPI is below 0.37 on every
row, and most hot loops are throughput-bound scalar or short SIMD loops rather
than branch-mispredict-bound code. This reinforces REDRESS 98's W3 retirement:
cycles are not waiting for a retained substrate cursor.

## Section 3 - Delta vs SK-V9

The fresh values are same-shape with SK-V9 S-P1 V3/V6:

- Low c/B rows remain `citm_catalog`, `gsoc-2018`, `canada`, and `numbers`.
- High c/B rows remain `y_string_unicode`, `distinct_values` Track 2,
  `unicode_mixed`, and `random`.
- `unicode_mixed` remains a special row where Track 2 beats Track 1 on
  cycles/B; this is a call-site/string/escape issue, not a proof of substrate
  value.

No row admission uses this PMU table. It is diagnostic profile evidence only.

## Section 4 - Anomalies + Masking Signals

- Direct/typed c/B is absent in V1 because the only existing PMU emitter,
  `xctrace_probe`, is hard-wired to parse-only Track 1 and Track 2. A
  direct/typed PMU probe would be a profiling-tool redress, not behavior
  evidence, and must be committed separately if CHALLENGE requires it.
- `xcrun xctrace` CPU Counters trace bundles are retained even though the
  public export path for per-event PMC tables is not consumed here. The
  authoritative numeric rows come from `proc_pid_rusage`, as in SK-V9 S-P1.
- Masking probes are not product rows and remain fenced as diagnostic
  non-producers.

## Section 5 - Sources

- `/tmp/skv10-p1/parse-xctrace/pmu_rows.tsv`
- `/tmp/skv10-p1/parse-xctrace/cpu-counters/*.trace`
- `/tmp/skv10-p1/parse-xctrace/capture.log`
- `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-A-xctrace-cpu-counters.md`
