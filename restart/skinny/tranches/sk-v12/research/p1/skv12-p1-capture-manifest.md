# SK-V12 P1 Capture Manifest

Pass: S-P1 Profile. Cycle: V12 user-pin rerun.
Date: 2026-05-20.
Scope: replayable manifest for the pin-aware SK-V12-open profile capture.
Output: this file.

## Pin Run Identity

- Capture source commit: `cf7848b2` (`docs(sk-v12-alpha-hardening): converge
  pin-aware G-Alpha V4`).
- Initial committed S-P1 fold: `b1043383` (`docs(sk-v12-p1-profile): fold
  pin-aware profile capture`).
- Capture root: `/tmp/skv12-pin-p1`.
- Build root: `/tmp/skv12-pin-profile-target-cf7848b2`.
- Binaries:
  `/tmp/skv12-pin-profile-target-cf7848b2/release/xctrace_probe` and
  `/tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct`.
- Completion stamps:
  - PMU: `/tmp/skv12-pin-p1/pmu/done.txt` =
    `done 2026-05-20T18:05:34Z`.
  - samply: `/tmp/skv12-pin-p1/samply/done.txt` =
    `done 2026-05-20T18:15:35Z`.
  - xctrace: `/tmp/skv12-pin-p1/xctrace/done.txt` =
    `done 2026-05-20T18:40:17Z`.
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
CARGO_TARGET_DIR=/tmp/skv12-pin-profile-target-cf7848b2 \
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin xctrace_probe --bin profile_direct
```

The pre-pin manifest/replay surface at `/tmp/skv12-p1`,
`/tmp/skv12-profile-target-50bd1648`, and `skv12-p1-replay.tsv` is historical
only. It is not pin-era replay authority.

## Replay Surface

Pin-era replay authority is split by profiler because each tool records a
different artifact contract:

| Lane | Rows | Pin replay authority |
|---|---:|---|
| PMU parse/direct/typed | 82 | `/tmp/skv12-pin-p1/pmu/pmu-commands.sh` plus `/tmp/skv12-pin-p1/pmu/capture_status.tsv` |
| samply parse/direct/typed | 82 | `/tmp/skv12-pin-p1/samply/samply-commands.sh` plus `/tmp/skv12-pin-p1/samply/capture_status.tsv` |
| xctrace primary Time Profiler | 82 | `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`; command shape below |
| xctrace CPU Counters | 82 | `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`; command shape below |
| xctrace product-v2 Time Profiler | 48 | `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`; command shape below |
| Time Profiler XML exports | 82 | `/tmp/skv12-pin-p1/time_profile_export_status.tsv` records `SKIP` for already-existing nonzero XML files |
| Derived hot-leaf tables | 82 summary / 410 detail rows | `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` and `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv` |

The tracked replay ledger
`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv` contains
458 pin-era command rows: 82 PMU, 82 samply, 212 xctrace capture, and 82
xctrace export rows.

Representative xctrace command shapes:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
xctrace record \
  --template "Time Profiler" \
  --output /tmp/skv12-pin-p1/parse-xctrace/time-profiler/<corpus>__<mode>.trace \
  --no-prompt \
  --time-limit 2000ms \
  --launch -- \
  /tmp/skv12-pin-profile-target-cf7848b2/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <mode> <iters>

xctrace record \
  --template "CPU Counters" \
  --output /tmp/skv12-pin-p1/parse-xctrace/cpu-counters/<corpus>__<mode>.trace \
  --no-prompt \
  --time-limit 2000ms \
  --launch -- \
  /tmp/skv12-pin-profile-target-cf7848b2/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <mode> <iters>

xctrace record \
  --template "Time Profiler" \
  --output /tmp/skv12-pin-p1/direct-xctrace/time-profiler-v2/<corpus>__<mode>.trace \
  --no-prompt \
  --time-limit 2000ms \
  --launch -- \
  /tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct \
  20000 <corpus-or-update-center-alias> <mode>
```

The exact per-row corpus, mode, artifact, stdout, stderr, return code, and
accepted status are in `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`.
`rc=54` is accepted only when the xctrace stderr records an accepted stop
condition and "Output file saved as".

## Coverage

| Lane | Rows | Status |
|---|---:|---|
| PMU parse/direct/typed | 82 | PASS |
| samply parse/direct/typed | 82 | PASS |
| xctrace primary Time Profiler | 82 | PASS |
| xctrace CPU Counters | 82 | PASS |
| xctrace product-v2 Time Profiler | 48 | PASS |
| Time Profiler XML exports | 82 | present and nonzero; status TSV `SKIP` because exports already existed |
| hot-leaf summary/details | 82 / 410 data rows | PASS, no unresolved source anchors |

Validation:

```bash
awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++}
  END{print total, bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
# 212 0

awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++}
  END{print total, bad+0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
# 82 0

awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++}
  END{print total, bad+0}' /tmp/skv12-pin-p1/samply/capture_status.tsv
# 82 0

awk -F '\t' 'NR>1{total++; if($4!="SKIP") bad++}
  END{print total, bad+0}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
# 82 0

awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none") bad++}
  END{print n, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
# 82 0

awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none") bad++}
  END{print n, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
# 410 0
```

Mode III remains absent; the pin root captures parse, direct, and typed JSON
lanes only.

CSS L4 remains unprofiled in the pin root because `skinny/` does not yet have
a generated CSS L4 Track 1 runtime, lightningcss same-plane comparator row, or
strict equality oracle row.

## Derived Self-Time Tables

The pin fold parsed xctrace Time Profiler XML into target-binary leaf
self-time tables. Percentages are over selected target-binary running samples
after filtering dyld/startup frames; the summary keeps total sample coverage.

Derived artifacts:

- `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv`
- `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv`
- `/tmp/skv12-pin-p1/time_profile_parse_table.md`
- `/tmp/skv12-pin-p1/time_profile_direct_table.md`
- `/tmp/skv12-pin-p1/time_profile_typed_table.md`

Top-family distribution by row, split by mode:

| Plane/mode | Leading families |
|---|---|
| `parse/track1` | `bounded_plain_string_scan` 7; `container_dispatch` 7; `number_digit_span` 1; `simd_movemask` 1; `unicode_escape_hex_decode` 1 |
| `parse/track2` | `container_dispatch` 11; `bounded_plain_string_scan` 5; `unicode_escape_hex_decode` 1 |
| `direct/track1` | `output_digest_hash` 17 |
| `direct/track2` | `runtime_support` 14; `string_escape_decode` 2; `allocation_support` 1 |
| `typed/real_typed_track1` | `typed_direct_projection` 6; `string_full_scan` 1 |
| `typed/real_typed_track2` | `serde_json_oracle_read_parse` 7 |

Track 2/oracle-only families are guard/comparator context. They are not
generated Track 1 optimization antecedents.

## PMU Aggregates

Weighted PMU aggregates from the pin TSVs:

| Plane | Rows | Aggregate Mbps | Aggregate c/B | Aggregate CPI |
|---|---:|---:|---:|---:|
| parse | 34 | 8669.019 | 2.971206 | 0.208405 |
| direct | 34 | 5773.975 | 4.411311 | 0.188854 |
| typed guards | 14 | 8959.011 | 3.137378 | 0.185866 |

The TSVs expose cycles, instructions, c/B, CPI, user ns, system ns, and
checksums. Branch-miss, L1, and LLC columns are not present and are not
inferred.
