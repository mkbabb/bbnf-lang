# SK-V13 P1-D: PMU + Cycles-Per-Byte

Pass: S-P1 Profile. Cycle: V13.
Date: 2026-05-21.
Scope: PMU counters and derived cycles-per-byte for captured JSON parse, direct, and typed product planes.
Output: this file.
Baseline: SK-V13-open (`f8be692068e9e464b6ed24027ab26edfd05303fd`).
Host triple: aarch64-apple-darwin.
Build flags: release profile, `debug=true`, `RUSTFLAGS=-C target-cpu=native`, native target CPU.
Profile tool: `/tmp/skv13-profile-target-0a7b41c5/release/{xctrace_probe,profile_direct}` plus Xcode Instruments `CPU Counters` and `Time Profiler` captures.
Corpus coverage: 17/17 JSON corpora for parse and direct; 7/7 captured `real_typed_struct` rows from `skinny/RESULTS.md`.

## §1 - Method (commands run; verbatim, reproducible)

Identity source:

```sh
cat /tmp/skv13-p1/artifacts/identity.txt
```

Run identity:

```text
root=/tmp/skv13-p1
bin=/tmp/skv13-profile-target-0a7b41c5/release
commit=f8be692068e9e464b6ed24027ab26edfd05303fd
date=2026-05-21T06:01:45Z
```

PMU capture driver:

```sh
bash /tmp/skv13-p1/pmu/run-pmu.sh
```

The PMU driver ran these command shapes from `/Users/mkbabb/Programming/bbnf-lang/skinny`:

```sh
/tmp/skv13-profile-target-0a7b41c5/release/xctrace_probe <json-path> track1 40
/tmp/skv13-profile-target-0a7b41c5/release/xctrace_probe <json-path> track2 40
/tmp/skv13-profile-target-0a7b41c5/release/profile_direct 2000 '<corpus-or-path>' track1
/tmp/skv13-profile-target-0a7b41c5/release/profile_direct 2000 '<corpus-or-path>' track2
/tmp/skv13-profile-target-0a7b41c5/release/profile_direct 2000 '<corpus-or-path>' sonic
/tmp/skv13-profile-target-0a7b41c5/release/profile_direct 2000 '<corpus-or-path>' serde
/tmp/skv13-profile-target-0a7b41c5/release/profile_direct 1000 <corpus> real_typed_track1
/tmp/skv13-profile-target-0a7b41c5/release/profile_direct 1000 <corpus> real_typed_track2
/tmp/skv13-profile-target-0a7b41c5/release/profile_direct 1000 <corpus> real_typed_sonic
/tmp/skv13-profile-target-0a7b41c5/release/profile_direct 1000 <corpus> real_typed_serde
```

Exact per-row commands and logs are in `/tmp/skv13-p1/pmu/capture_status.tsv`. Derived PMU rows are in `/tmp/skv13-p1/pmu/pmu_rows.tsv` with schema:

```text
lane, corpus, mode, rc, mbps, cycles, instructions, cycles_per_byte, cpi, checksum, log
```

Xcode Instruments coverage source:

```sh
cat /tmp/skv13-p1/xctrace/capture_status.tsv
```

The xctrace status file records both `CPU Counters` and `Time Profiler` traces for each logical row captured there, with trace bundles under `/tmp/skv13-p1/xctrace/traces/` and logs under `/tmp/skv13-p1/xctrace/logs/`.

## §2 - Findings (per-corpus PMU tables)

### §2.1 Coverage Summary

| Surface | Rows | Bad rc | Coverage |
|---|---:|---:|---|
| PMU `parse` | 34 | 0 | 17 corpora x Track 1/Track 2 |
| PMU `direct` | 68 | 0 | 17 corpora x Track 1/Track 2/sonic/serde |
| PMU `typed` | 28 | 0 | 7 captured typed rows x Track 1/Track 2/sonic/serde |
| xctrace `CPU Counters` | 82 | 0 | parse/direct Track 1/2 plus captured typed Track 1/2 |
| xctrace `Time Profiler` | 82 | 0 | same logical rows as CPU Counters |

The PMU row file exposes cycles, instructions, cycles-per-byte, CPI, checksum, and log path. It does not expose branch misses, L1 misses, or LLC misses. Those requested fields therefore cannot be reported from this capture and remain explicit missing telemetry, not zeroes.

### §2.2 Parse Plane: `parse_only`

`T1 vs sonic` uses the `sonic-rs strict Mbps` column from `skinny/RESULTS.md` because the PMU parse capture did not include same-run sonic parse rows.

| Corpus | T1 Mbps | T1 c/B | T2 Mbps | T2 c/B | T1 vs sonic | T2-T1 c/B | Outcome |
|---|---:|---:|---:|---:|---:|---:|---|
| twitter | 15093.5 | 2.256 | 11771.6 | 2.872 | -19.4% | 0.616 | S |
| citm_catalog | 30057.3 | 1.135 | 20168.7 | 1.689 | +45.6% | 0.554 | S |
| canada | 17413.7 | 1.941 | 16529.5 | 2.059 | +304.8% | 0.118 | S |
| apache_builds | 11999.7 | 2.822 | 12095.0 | 2.849 | +34.5% | 0.028 | S |
| github_events | 13476.3 | 2.407 | 12641.9 | 2.710 | +9.9% | 0.303 | S |
| update_center | 11102.3 | 3.058 | 8893.6 | 3.817 | -19.8% | 0.759 | S |
| mesh | 13020.3 | 2.632 | 11489.2 | 2.977 | +45.0% | 0.345 | S |
| random | 9847.2 | 3.482 | 7725.4 | 4.409 | +38.4% | 0.927 | S |
| gsoc-2018 | 18942.6 | 1.599 | 17695.1 | 1.828 | +11.9% | 0.229 | S |
| marine_ik | 13000.5 | 2.635 | 12598.7 | 2.720 | +77.3% | 0.085 | S |
| instruments | 17118.9 | 2.014 | 11729.7 | 2.939 | +12.6% | 0.925 | S |
| numbers | 18568.8 | 1.868 | 18336.0 | 1.891 | +81.5% | 0.024 | S |
| unicode_mixed | 7301.9 | 4.711 | 7326.7 | 4.463 | +5.2% | -0.248 | S |
| unicode_escapes | 10518.3 | 3.264 | 11229.3 | 3.054 | -28.0% | -0.210 | S |
| unicode_basic | 11702.0 | 2.920 | 10765.4 | 3.180 | -8.3% | 0.260 | S |
| distinct_values | 9361.4 | 3.664 | 5913.8 | 5.833 | -45.2% | 2.168 | S |
| y_string_unicode | 6080.6 | 5.674 | 5301.7 | 6.253 | -56.1% | 0.579 | S |

Worst parse c/B rows by Track 1 are `y_string_unicode` 5.674 c/B, `unicode_mixed` 4.711 c/B, `distinct_values` 3.664 c/B, `random` 3.482 c/B, and `unicode_escapes` 3.264 c/B. The Track 2 substrate is usually costlier than generated Track 1; the exceptions are `unicode_mixed` and `unicode_escapes`, where Track 2 reports lower c/B in this capture.

### §2.3 Direct Product Plane: `direct_to_struct`

`T1 c/B edge vs sonic` is positive when generated Track 1 spends fewer cycles per byte than the same-run sonic direct comparator.

| Corpus | T1 Mbps | T1 c/B | T2 c/B | sonic Mbps | sonic c/B | serde c/B | T1 vs sonic | T1 c/B edge | Outcome |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---|
| twitter | 11433.6 | 2.980 | 3.243 | 10284.6 | 3.326 | 4.745 | +11.2% | +10.4% | N-direct |
| citm_catalog | 21099.3 | 1.615 | 1.698 | 14852.6 | 2.292 | 3.419 | +42.1% | +29.5% | A |
| canada | 10509.0 | 3.252 | 3.340 | 12173.5 | 2.803 | 4.659 | -13.7% | -16.0% | N-direct |
| apache_builds | 10895.3 | 3.086 | 3.365 | 9297.8 | 3.641 | 4.500 | +17.2% | +15.3% | A |
| github_events | 12009.6 | 2.839 | 3.085 | 11539.2 | 2.943 | 3.717 | +4.1% | +3.5% | N-direct |
| update_center | 8196.7 | 4.142 | 4.645 | 8033.5 | 4.254 | 5.893 | +2.0% | +2.6% | N-direct |
| mesh | 8778.5 | 3.890 | 4.046 | 9632.8 | 3.549 | 4.718 | -8.9% | -9.6% | N-direct |
| random | 7607.4 | 4.436 | 4.900 | 5712.3 | 5.648 | 7.586 | +33.2% | +21.4% | N-direct |
| gsoc-2018 | 9982.2 | 2.920 | 2.569 | 19948.1 | 1.710 | 2.085 | -50.0% | -70.7% | N-direct |
| marine_ik | 9238.4 | 3.684 | 3.668 | 7609.6 | 4.474 | 5.404 | +21.4% | +17.7% | A |
| instruments | 11960.7 | 2.870 | 3.112 | 7960.7 | 4.304 | 5.501 | +50.2% | +33.3% | N-direct |
| numbers | 12369.3 | 2.775 | 2.816 | 12791.7 | 2.685 | 4.264 | -3.3% | -3.3% | A |
| unicode_mixed | 4558.8 | 7.537 | 7.707 | 8890.2 | 3.859 | 7.105 | -48.7% | -95.3% | N-direct |
| unicode_escapes | 5018.3 | 6.834 | 7.052 | 13491.2 | 2.547 | 5.692 | -62.8% | -168.4% | N-direct |
| unicode_basic | 9038.9 | 3.789 | 4.229 | 6624.9 | 5.166 | 7.657 | +36.4% | +26.6% | A |
| distinct_values | 6255.7 | 5.513 | 6.175 | 8033.8 | 4.287 | 6.074 | -22.1% | -28.6% | N-direct |
| y_string_unicode | 3232.5 | 10.621 | 11.728 | 8634.8 | 3.963 | 4.307 | -62.6% | -168.0% | N-direct |

Direct rows with same-run PMU deficits against sonic are `unicode_escapes`, `y_string_unicode`, `gsoc-2018`, `unicode_mixed`, `distinct_values`, `canada`, `mesh`, and `numbers`. The unicode-heavy direct plane is the clearest cycles-per-byte outlier: `y_string_unicode` spends 10.621 c/B in Track 1 versus sonic 3.963 c/B, while `unicode_escapes` spends 6.834 c/B versus sonic 2.547 c/B.

### §2.4 Typed Product Plane: `real_typed_struct`

The PMU capture includes the seven typed rows present in the current `skinny/RESULTS.md` table.

| Corpus | T1 Mbps | T1 c/B | T2 c/B | sonic Mbps | sonic c/B | serde c/B | T1 vs sonic | T1 c/B edge | Outcome |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---|
| twitter | 18492.4 | 1.860 | 2.127 | 15303.7 | 2.252 | 2.127 | +20.8% | +17.4% | A |
| citm_catalog | 35379.3 | 0.972 | 1.792 | 21888.6 | 1.567 | 1.794 | +61.6% | +38.0% | A |
| apache_builds | 8549.8 | 4.025 | 5.957 | 6695.3 | 5.141 | 6.118 | +27.7% | +21.7% | A |
| github_events | 12484.9 | 2.735 | 3.032 | 11643.2 | 2.959 | 2.999 | +7.2% | +7.6% | A |
| update_center | 12131.8 | 2.835 | 3.532 | 11398.2 | 3.024 | 3.661 | +6.4% | +6.3% | A |
| mesh | 9082.6 | 3.794 | 4.829 | 8504.5 | 4.049 | 4.740 | +6.8% | +6.3% | A |
| marine_ik | 12037.2 | 2.855 | 3.539 | 8833.3 | 3.748 | 3.439 | +36.3% | +23.8% | A |

All captured typed rows beat same-run sonic on Mbps and c/B. The weakest PMU typed margins are `update_center` (+6.4% Mbps), `mesh` (+6.8%), and `github_events` (+7.2%); these are still positive in this capture.

### §2.5 Counter Field Availability

Raw cycles, instructions, CPI, checksums, and log paths remain in `/tmp/skv13-p1/pmu/pmu_rows.tsv` for every row summarized above. The derived c/B values in this document are copied from that TSV, not recomputed from Criterion. Branch-miss, L1-miss, and LLC-miss fields are absent from the TSV schema; xctrace trace bundles may contain CPU-counter detail, but no exported tabular branch/cache miss columns were provided to this agent.

## §3 - Delta vs SK-V12 and JSON-vs-SOTA Margins

No SK-V12 PMU counter ledger was included in the required inputs, so a prior-tranche c/B delta cannot be computed honestly. This artifact therefore reports current SK-V13-open PMU c/B and same-run JSON-vs-SOTA margins where captured:

| Plane | Comparator basis | Current PMU/SOTA result |
|---|---|---|
| `parse_only` | Track 1 PMU Mbps vs `skinny/RESULTS.md` sonic-rs strict Mbps | 11/17 parse rows above sonic Mbps; 6/17 below (`y_string_unicode`, `distinct_values`, `unicode_escapes`, `update_center`, `twitter`, `unicode_basic`) |
| `direct_to_struct` | same-run PMU Track 1 vs PMU sonic direct | 9/17 direct rows above sonic Mbps; 8/17 below (`unicode_escapes`, `y_string_unicode`, `gsoc-2018`, `unicode_mixed`, `distinct_values`, `canada`, `mesh`, `numbers`) |
| `real_typed_struct` | same-run PMU Track 1 vs PMU sonic typed | 7/7 captured typed rows above sonic Mbps and below sonic c/B |

The current `skinny/RESULTS.md` gate still classifies every parse row as `S / NO-GO` despite several Track 1 parse Mbps wins, because SK-V13 requires all JSON rows and planes above strict sonic-rs or architecturally blocked, and direct/unicode/product-plane deficits remain.

## §4 - Anomalies + masking signals (flagged for S-P2)

1. PMU branch/cache miss telemetry is missing from the exported row schema. The prompt asks for branch misses, L1 misses, and LLC misses; `/tmp/skv13-p1/pmu/pmu_rows.tsv` exposes only cycles, instructions, cycles-per-byte, and CPI. Treat branch/L1/LLC as missing fields, not zero-miss results.
2. Parse comparator PMU is asymmetric. The parse lane captured Track 1 and Track 2 only, so parse JSON-vs-SOTA margins use `skinny/RESULTS.md` sonic-rs strict Mbps rather than same-run PMU sonic cycles-per-byte.
3. `real_typed_struct` PMU coverage is intentionally narrower than the 17-corpus parse/direct set because the current results table has seven typed rows. Missing typed PMU rows for the other ten corpora are not failures of the 130-row capture; they are absent product-plane rows in the current bench surface.
4. The direct unicode rows carry the strongest current c/B anomalies: `unicode_escapes` and `y_string_unicode` are roughly 2.68x sonic c/B in Track 1. `unicode_mixed` is 1.95x sonic c/B. These align with the S-P1 requirement to avoid float-heavy overfit and keep string/unicode rows load-bearing.
5. The direct `gsoc-2018` row is a string-heavy anomaly: Track 1 reports 2.920 c/B while sonic reports 1.710 c/B, a -50.0% Mbps margin. This is not explained by numeric parsing.
6. `unicode_mixed` and `unicode_escapes` parse lanes show Track 2 lower c/B than Track 1 in this capture. That inversion is narrow but worth preserving for P1-E/P1-C cross-check because it may separate generated parser overhead from shared tape substrate cost.
7. Masking-probe rows named in `skinny/RESULTS.md` (`host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`, structural-scan-only) are not present as independent PMU rows in `/tmp/skv13-p1/pmu/pmu_rows.tsv`. This P1-D artifact therefore cannot attribute those masking signals with PMU counters beyond the existing `skinny/RESULTS.md` signal text. P1-C/P1-E should be used for symbol attribution of masking probes.
8. No REDRESS route is reopened here. The observations above are measurement facts only; they do not propose a new dispatch table, SIMD path, PMULL/CSSC route, or alternate union substrate.

## §5 - Sources (artefact paths + run id)

| Source | Use |
|---|---|
| `/tmp/skv13-p1/artifacts/identity.txt` | run root, binary root, baseline commit, capture date |
| `/tmp/skv13-p1/pmu/run-pmu.sh` | PMU capture command generator |
| `/tmp/skv13-p1/pmu/capture_status.tsv` | 130 PMU row statuses and exact commands, 0 bad return codes |
| `/tmp/skv13-p1/pmu/pmu_rows.tsv` | cycles, instructions, c/B, CPI, checksum, log path for 130 rows |
| `/tmp/skv13-p1/pmu/logs/*.log` | per-row PMU/probe logs |
| `/tmp/skv13-p1/xctrace/capture_status.tsv` | 164 xctrace statuses: 82 CPU Counters and 82 Time Profiler rows, 0 bad return codes |
| `/tmp/skv13-p1/xctrace/traces/*.trace` | Instruments trace bundles |
| `/tmp/skv13-p1/xctrace/logs/*.log` | per-row xctrace logs |
| `skinny/RESULTS.md` | gate outcome, sonic strict parse Mbps, current product-plane row set |
| `skinny/REDRESS.md` | rejected-route and masking-probe context |
| `restart/skinny/tranches/sk-v13/HANDOFF.md` | SK-V13 scope, pre-G-Omega source-edit block, JSON all-planes obligation |
| `restart/skinny/tranches/sk-v13/scoping/*.md` | SK-V13 CSS/JSON/profile-truth/union/SIMD scoping constraints |
