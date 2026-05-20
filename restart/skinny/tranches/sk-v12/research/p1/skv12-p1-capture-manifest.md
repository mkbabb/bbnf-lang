# SK-V12 P1 Capture Manifest

Pass: S-P1 Profile. Cycle: V12.
Date: 2026-05-20.
Scope: replayable manifest for the SK-V12-open profile capture and the V1
hardening self-time export fold.
Output: this file.

## Run Identity

- Source baseline: `50bd1648` (`docs(sk-v12-g-alpha): present converged alpha
  contract`).
- Capture root: `/tmp/skv12-p1`.
- Build root: `/tmp/skv12-profile-target-50bd1648`.
- Result authority remains `skinny/RESULTS.md`; this manifest records profile
  evidence only and moves no rows.

Host and tools:

| Tool | Version |
|---|---|
| macOS | `26.4.1` build `25E253` |
| Darwin | `25.4.0 arm64` |
| Rust | `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM `22.1.2` |
| Cargo | `cargo 1.96.0-nightly (eb94155a9 2026-04-09)` |
| samply | `samply 0.13.1` |
| xctrace | `xctrace version 26.0 (17A5241e)` |

Build command:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv12-profile-target-50bd1648 \
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin xctrace_probe --bin profile_direct
```

## Exact Replay Surface

The repo-tracked replay ledger is
`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`. It is the
authoritative command surface for independent replay; command blocks below are
readable recipes only.

Replay rows:

| Lane | Rows |
|---|---:|
| `pmu` | 82 |
| `samply` | 82 |
| `xctrace-cpu-counters` | 82 |
| `xctrace-time-profiler-primary` | 82 |
| `xctrace-time-profiler-export` | 34 |
| `xctrace-time-profiler-export-primary` | 48 |
| `xctrace-time-profiler-product-v2` | 48 |
| `xctrace-time-profiler-product-v2-export` | 48 |

Every row records lane, family, plane, corpus, mode, launch alias, iteration
count, cwd, binary path, expected return-code policy, output artifact, status
artifact, full command, and notes. The samply rows are retained artifact-only
evidence because they use `--save-only`; self-time percentages are sourced from
exported xctrace Time Profiler XML.

## Primary Capture

Primary PMU/samply/xctrace status is
`/tmp/skv12-p1/pmu/capture_status.tsv`.

| Family | Rows | Status |
|---|---:|---|
| `pmu-parse` | 34 | PASS `rc=0` |
| `pmu-direct` | 34 | PASS `rc=0` |
| `pmu-typed` | 14 | PASS `rc=0` |
| `samply-parse` | 34 | PASS `rc=0` |
| `samply-direct` | 34 | PASS `rc=0` |
| `samply-typed` | 14 | PASS `rc=0` |
| `xctrace-time-profiler-parse` | 34 | PASS `rc=54` |
| `xctrace-time-profiler-direct` | 34 | PASS `rc=54` |
| `xctrace-time-profiler-typed` | 14 | PASS `rc=54` |
| `xctrace-cpu-counters-parse` | 34 | PASS `rc=54` |
| `xctrace-cpu-counters-direct` | 34 | PASS, 32 `rc=54`, 2 `rc=0` |
| `xctrace-cpu-counters-typed` | 14 | PASS `rc=54` |

`rc=54` is accepted when the xctrace log records "Reached specified time
limit" or "Target app exited" followed by "Output file saved as". The final
product PMU run is from `skinny/`; the initial product run from the repository
root failed fixture lookup and is preserved separately at
`/tmp/skv12-p1/pmu/capture_status.initial-product-cwd-fail.tsv`.

Product fixture alias rule: the PMU TSV row key is `update_center`, but
`profile_direct` direct modes require the launch argument `update-center`.
Typed modes locate the same fixture through the typed locator. The V1 fold
replayed the direct `update_center` Time Profiler rows with the alias and
records the correction in `/tmp/skv12-p1/product_time_profile_v2_alias_fixes.tsv`.

## Self-Time Export

Parse Time Profiler export command shape:

```bash
for trace in /tmp/skv12-p1/parse-xctrace/time-profiler/*.trace; do
  base=$(basename "$trace" .trace)
  xctrace export \
    --input "$trace" \
    --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]' \
    > "/tmp/skv12-p1/parse-xctrace/exports/${base}.time-profile.xml"
done
```

The original product Time Profiler exports under
`/tmp/skv12-p1/direct-xctrace/exports/` are retained but are shallow for many
rows because the target process exited before the sampler collected a useful
hot-loop table. The V1 fold therefore recaptured product Time Profiler rows
with a 2s time limit and 20,000 product iterations. Exact per-row commands and
the `update_center` launch alias are enumerated in `skv12-p1-replay.tsv`:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
xctrace record \
  --template "Time Profiler" \
  --output /tmp/skv12-p1/direct-xctrace/time-profiler-v2/<corpus>__<mode>.trace \
  --no-prompt \
  --time-limit 2000ms \
  --launch -- \
  /tmp/skv12-profile-target-50bd1648/release/profile_direct \
  20000 <corpus-or-update-center-alias> <mode>

xctrace export \
  --input /tmp/skv12-p1/direct-xctrace/time-profiler-v2/<corpus>__<mode>.trace \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]' \
  > /tmp/skv12-p1/direct-xctrace/exports-v2/<corpus>__<mode>.time-profile.xml
```

Export status:

| Export lane | Rows | Status | Bytes |
|---|---:|---|---:|
| original Time Profiler export | 82 | 82 PASS | 9,327,356 |
| product Time Profiler v2 | 48 | 48 PASS | 23,383,417 |

The original all-row export status is
`/tmp/skv12-p1/time_profile_export_status.tsv`. The product v2 status is
`/tmp/skv12-p1/product_time_profile_v2_status.tsv`.

## Derived Self-Time Tables

The V1 fold parsed xctrace Time Profiler XML into target-binary leaf self-time
tables. Percentages are over selected target-binary running samples after
filtering dyld/startup frames; the summary keeps total sample coverage too.

| Plane | Rows | Sample rows | Selected rows | Selected target time |
|---|---:|---:|---:|---:|
| parse | 34 | 34,129 | 34,080 | 99.86% |
| direct | 34 | 64,593 | 64,541 | 99.92% |
| typed | 14 | 25,713 | 25,692 | 99.92% |

Derived artifacts:

- `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv`
- `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`
- `/tmp/skv12-p1/time_profile_parse_table.md`
- `/tmp/skv12-p1/time_profile_direct_table.md`
- `/tmp/skv12-p1/time_profile_typed_table.md`

The V2 hardening fold normalized xctrace line-zero frames to concrete current
source anchors in those derived TSVs. The summary table has 82/82 rows with no
`top_leaf_source` ending in `:0`; the detail table has 410/410 rows with no
`source` ending in `:0` and no `UNRESOLVED_LINE_ZERO` markers.

Top-family distribution by row:

| Plane | Leading families |
|---|---|
| parse | `bounded_plain_string_scan` 14 rows; `container_dispatch` 11; `unicode_escape_hex_decode` 4; `number_digit_span` 2; `simd_movemask` 2; `string_escape_decode` 1 |
| direct | `output_digest_hash` 18 rows; `container_dispatch` 10; `string_escape_decode` 4; `bounded_plain_string_scan` 1; `number_digit_span` 1 |
| typed | `serde_json_oracle_read_parse` 7 rows; `typed_direct_projection` 5; `number_digit_span` 2 |

## PMU Aggregates

Weighted PMU aggregates from the primary TSVs:

| Plane | Rows | Aggregate c/B | Aggregate CPI |
|---|---:|---:|---:|
| parse | 34 | 2.920217 | 0.204887 |
| direct | 34 | 4.290305 | 0.183717 |
| typed guards | 14 | 3.123172 | 0.185056 |

The TSVs expose cycles, instructions, c/B, CPI, user ns, system ns, and
checksums. Branch-miss, L1, and LLC columns are not present and are not
inferred.

## Mode III Boundary

The SK-V12-open `/tmp/skv12-p1` primary capture contains parse, direct, and
typed lanes. It does not contain fresh samply call stacks for
`host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`, or a
fresh structural-scan-only xctrace lane. P1-C carries the W0 raw Criterion Mode
III throughput and structural-scan matrix as diagnostic nonproducer evidence.
No S-P2 or S-P3 wave may use Mode III symbols as fresh SK-V12 hot-leaf
authority unless a later capture supplies those call stacks explicitly.
