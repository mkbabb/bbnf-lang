# SK-V16 P1-D: PMU And Cycles-Per-Byte

Pass: S-P1 Profile. Cycle: V16.
Date: 2026-05-28.
Scope: cold `profile_direct` cycle/instruction ledger plus PMU availability probe.
Output: this file.
Baseline: SK-V16-open (`5ed43f8e1`).
Host triple: `aarch64-apple-darwin`.
Build flags: release profile with debuginfo; `warmup_iters=0`.
Profile tool: `proc_pid_rusage(RUSAGE_INFO_V5)` in `profile_direct`; `xcrun xctrace` availability probe.
Corpus coverage: 17/17 for P1-A/B/C modes.

## Section 1 - Method

```sh
cd /Users/mkbabb/Programming/bbnf-skv16-p1/skinny
./target/release/profile_direct 500 <corpus> parse_only_track1 0
./target/release/profile_direct 500 <corpus> direct_strict_track1 0
./target/release/profile_direct 500 <corpus> real_typed_track1 0
./target/release/profile_direct 100 <corpus> host_call_eager_decode 0
./target/release/profile_direct 100 <corpus> alternate_scalar_plan 0
./target/release/profile_direct 100 <corpus> cold_first_parse 0
./target/release/profile_direct 100 <corpus> structural_scan_only 0

xcrun xctrace record --template "CPU Counters" \
  --output /tmp/skv16-p1/xctrace/twitter-parse.trace \
  --no-prompt --launch -- ./target/release/profile_direct 1000 twitter parse_only_track1 0
xcrun xctrace export --input /tmp/skv16-p1/xctrace/twitter-parse.trace \
  --toc > /tmp/skv16-p1/xctrace/twitter-parse-toc.xml
xcrun xctrace export --input /tmp/skv16-p1/xctrace/twitter-parse.trace \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="cpu-state"]' \
  --output /tmp/skv16-p1/xctrace/twitter-parse-cpu-state.xml
```

## Section 2 - Findings

`profile_direct` emits real process cycles and instructions from
`proc_pid_rusage(RUSAGE_INFO_V5)`. Branch misses and cache misses are not
available from that API. `xctrace` successfully recorded `CPU Counters`, but
the export TOC exposed `cpu-state`, `time-profile`, and related scheduling
schemas, not branch/L1/LLC counter columns. This artifact therefore treats
branch/cache counters as `unavailable_from_current_export` and does not invent
values.

Worst c/B across the baseline JSON modes:

| Corpus | Mode | Mbps | c/B |
|---|---|---:|---:|
| unicode_escapes | direct_strict_track1 | 2746.357 | 11.611600 |
| unicode_escapes | real_typed_track1 | 2818.360 | 11.597315 |
| canada | real_typed_track1 | 4930.202 | 6.964183 |
| canada | direct_strict_track1 | 4979.517 | 6.900380 |
| unicode_mixed | real_typed_track1 | 5795.830 | 5.816435 |
| unicode_mixed | direct_strict_track1 | 6047.748 | 5.675189 |

Worst c/B across masking modes:

| Corpus | Mode | Mbps | c/B |
|---|---|---:|---:|
| unicode_mixed | cold_first_parse | 1480.548 | 19.093185 |
| y_string_unicode | cold_first_parse | 1531.562 | 18.625012 |
| y_string_unicode | host_call_eager_decode | 1585.552 | 17.960892 |
| unicode_mixed | host_call_eager_decode | 1608.118 | 17.532932 |
| unicode_escapes | cold_first_parse | 1843.354 | 15.656950 |
| unicode_escapes | host_call_eager_decode | 1843.567 | 15.568485 |

## Section 3 - Delta Vs SK-V15

No admission delta. The c/B ledger is an empirical floor for S-P2 and S-P3.
JSON remains 51/51 admitted; CSS remains 0/24 admitted.

## Section 4 - Anomalies And Masking Signals

`cold_first_parse` is the most expensive diagnostic mode on average; eager
decode is nearly as expensive on Unicode-heavy corpora. Structural scan is
cheaper but heavily polluted by the local FNV checksum used to keep the
profile loop live, so S-P2 must not treat it as a direct structural-scan
kernel source without a checksum-free consumer.

`xctrace` PMU export is not rich enough here for branch/cache counters. Any
future P1-D revision that claims those counters must cite exported columns,
not only the `CPU Counters` template name.

## Section 5 - Sources

- `/tmp/skv16-p1/probe-results.tsv`
- `/tmp/skv16-p1/probe-summary.tsv`
- `/tmp/skv16-p1-mode3/probe-results.tsv`
- `/tmp/skv16-p1-mode3/probe-summary.tsv`
- `/tmp/skv16-p1/xctrace/twitter-parse.trace`
- `/tmp/skv16-p1/xctrace/twitter-parse-toc.xml`
- `/tmp/skv16-p1/xctrace/twitter-parse-cpu-state.xml`
