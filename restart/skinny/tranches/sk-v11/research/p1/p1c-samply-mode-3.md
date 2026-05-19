# SK-V11 P1-C: samply mode III masking probes and structural scan

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-19.
Scope: Mode III diagnostic masking probes (`host_call_eager_decode`,
`alternate_scalar_plan`, `cold_first_parse`) plus structural-scan-only and
lazy-tape W0 evidence.
Output: this file.
Baseline: SK-V11-open (`3ce75df4`).
Host triple: `aarch64-apple-darwin`; host facts record Apple M5 Max.
Build flags: `profile=bench; rustflags=-C target-cpu=native; target_cpu=native`.
Profile tool: Criterion.rs W0 diagnostic artefacts under
`/tmp/skv11-open-criterion-3ce75df`; no new samply call-stack capture is
claimed in this artefact.
Corpus coverage: 17/17 JSON corpora.

## Section 1 - Method

W0 capture authority:

```sh
CARGO_TARGET_DIR=/tmp/skv11-open-target-3ce75df CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory
```

W0 gate authority:

```sh
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

Extraction used the W0 Criterion JSON files directly:

```sh
mbps = benchmark.throughput.Bytes * 8 * 1000 / estimates.slope.point_estimate
```

If `slope.point_estimate` is absent, the gate code falls back to
`mean.point_estimate` (`skinny/crates/bbnf-bench/src/bin/gate.rs:1785`).
The source definitions are:

| Surface | Source locus |
|---|---|
| probe group and `host_call_eager_decode` | `skinny/crates/bbnf-bench/benches/json_parity.rs:381` |
| `alternate_scalar_plan` | `skinny/crates/bbnf-bench/benches/json_parity.rs:407` |
| `cold_first_parse` | `skinny/crates/bbnf-bench/benches/json_parity.rs:422` |
| eager string walk | `skinny/crates/bbnf-bench/benches/json_parity.rs:440` |
| probe signal thresholds | `skinny/crates/bbnf-bench/src/bin/gate.rs:1822` |
| structural-scan benchmark | `skinny/crates/bbnf-bench/benches/simd_scan.rs:9` |
| scalar/SIMD scan wrappers | `skinny/crates/bbnf-bench/src/scan.rs:1` |
| lazy-tape materialization summary | `skinny/crates/bbnf-bench/src/materialization.rs:82` |
| materialization note emission | `skinny/crates/bbnf-bench/src/bin/gate.rs:1810` |

Parse-only remains diagnostic. PMU and Time Profiler product data is being
captured separately under `/tmp/skv11-p1`; this P1-C file does not report c/B,
PMU counters, or product-plane admissions.

## Section 2 - Findings

This table is diagnostic. `T1 parse Mbps` is the W0 parse-only Track 1 slope
for the same corpus and is used only as the probe denominator. None of the
probe or structural-scan rows is a behavior admission or a SOTA close target.

| Corpus | T1 parse Mbps | eager Mbps | eager/T1 | eager signal | alt scalar Mbps | alt/T1 | cold Mbps | cold/T1 | cold signal | struct simd Mbps | struct scalar Mbps |
|---|---:|---:|---:|---|---:|---:|---:|---:|---|---:|---:|
| `twitter` | 10474 | 4292 | 2.44 | MASKING >1.15x T1 | 6623 | 1.58 | 11737 | 0.89 | PASS <=2.00x T1 | 8252 | 2984 |
| `citm_catalog` | 26791 | 7024 | 3.81 | MASKING >1.08x T1 | 7820 | 3.43 | 27508 | 0.97 | PASS <=2.00x T1 | 8796 | 4561 |
| `canada` | 15544 | 3895 | 3.99 | MASKING >1.02x T1 | 4422 | 3.52 | 16292 | 0.95 | PASS <=2.00x T1 | 14249 | 3715 |
| `apache_builds` | 12733 | 4672 | 2.73 | MASKING >1.10x T1 | 6522 | 1.95 | 12294 | 1.04 | PASS <=2.00x T1 | 7520 | 4469 |
| `github_events` | 14805 | 5470 | 2.71 | MASKING >1.10x T1 | 7871 | 1.88 | 14496 | 1.02 | PASS <=2.00x T1 | 7597 | 4307 |
| `update_center` | 11493 | 3278 | 3.51 | MASKING >1.10x T1 | 4576 | 2.51 | 11485 | 1.00 | PASS <=2.00x T1 | 6965 | 4342 |
| `mesh` | 13325 | 4836 | 2.76 | MASKING >1.10x T1 | 4666 | 2.86 | 12636 | 1.05 | PASS <=2.00x T1 | 20114 | 2624 |
| `random` | 7747 | 1754 | 4.42 | MASKING >1.10x T1 | 2299 | 3.37 | 3522 | 2.20 | reported cold-sensitive | 3787 | 3020 |
| `gsoc-2018` | 4887 | 1602 | 3.05 | MASKING >1.10x T1 | 2442 | 2.00 | 6862 | 0.71 | PASS <=2.00x T1 | 9165 | 3155 |
| `marine_ik` | 10675 | 2280 | 4.68 | MASKING >1.10x T1 | 3897 | 2.74 | 12297 | 0.87 | PASS <=2.00x T1 | 8003 | 3749 |
| `instruments` | 16574 | 5155 | 3.21 | MASKING >1.10x T1 | 4745 | 3.49 | 15812 | 1.05 | PASS <=2.00x T1 | 8093 | 4539 |
| `numbers` | 17941 | 2144 | 8.37 | MASKING >1.10x T1 | 1345 | 13.34 | 4205 | 4.27 | reported cold-sensitive | 18514 | 4472 |
| `unicode_mixed` | 1883 | 1020 | 1.85 | MASKING >1.10x T1 | 1721 | 1.09 | 1979 | 0.95 | PASS <=2.00x T1 | 9768 | 7865 |
| `unicode_escapes` | 3733 | 576 | 6.48 | MASKING >1.10x T1 | 1374 | 2.72 | 2600 | 1.44 | PASS <=2.00x T1 | 19826 | 7059 |
| `unicode_basic` | 3217 | 1242 | 2.59 | MASKING >1.10x T1 | 997 | 3.23 | 1891 | 1.70 | PASS <=2.00x T1 | 7693 | 5109 |
| `distinct_values` | 2335 | 1487 | 1.57 | MASKING >1.10x T1 | 1881 | 1.24 | 2921 | 0.80 | PASS <=2.00x T1 | 8219 | 5246 |
| `y_string_unicode` | 1965 | 581 | 3.38 | MASKING >1.10x T1 | 2612 | 0.75 | 2053 | 0.96 | PASS <=2.00x T1 | 9916 | 5564 |

Structural-scan-only is a nonproducer. W0 records `canada` structural scan at
14249 Mbps against a 40000 Mbps floor (`skinny/RESULTS.md:103`), and no
structural-scan row in the table above reaches that floor. This is a diagnostic
scan-path signal, not a direct-row close.

Lazy-tape materialization from W0:

| Corpus | offsets | logical/input | allocated/input | sparse flags | string quotes | numbers | literals | payload bytes |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| `twitter` | 29573 | 0.19x | 0.21x | 1560 | 18099 | 2109 | 4737 | 0 |
| `citm_catalog` | 85035 | 0.20x | 0.30x | 5 | 26604 | 14392 | 1263 | 0 |
| `canada` | 223236 | 0.40x | 0.47x | 0 | 12 | 111126 | 0 | 0 |
| `apache_builds` | 7068 | 0.22x | 0.26x | 5 | 5289 | 2 | 3 | 0 |
| `github_events` | 2526 | 0.16x | 0.25x | 25 | 1891 | 149 | 88 | 0 |
| `update_center` | 35281 | 0.27x | 0.49x | 1045 | 27229 | 0 | 386 | 0 |
| `mesh` | 80250 | 0.44x | 0.72x | 0 | 11 | 73013 | 0 | 0 |
| `random` | 49011 | 0.38x | 0.51x | 0 | 33005 | 5002 | 1000 | 0 |
| `gsoc-2018` | 41714 | 0.05x | 0.08x | 8545 | 34128 | 0 | 0 | 0 |
| `marine_ik` | 359563 | 0.48x | 0.70x | 0 | 38268 | 245175 | 6 | 0 |
| `instruments` | 14793 | 0.27x | 0.30x | 0 | 6889 | 4935 | 557 | 0 |
| `numbers` | 10003 | 0.27x | 0.44x | 0 | 0 | 10001 | 0 | 0 |
| `unicode_mixed` | 41870 | 0.17x | 0.26x | 9795 | 25121 | 8371 | 0 | 0 |
| `unicode_escapes` | 11274 | 0.05x | 0.07x | 9385 | 5636 | 1877 | 1 | 0 |
| `unicode_basic` | 92146 | 0.35x | 0.50x | 0 | 57590 | 11518 | 0 | 0 |
| `distinct_values` | 11118 | 0.29x | 0.43x | 0 | 9796 | 440 | 0 | 0 |
| `y_string_unicode` | 2202 | 0.50x | 0.75x | 9000 | 2200 | 0 | 0 | 0 |

The lazy-tape rows show zero payload bytes for all 17 corpora and matching
Track 1/Track 2 materialization summaries in W0. They are useful substrate
shape evidence, but not behavior admissions.

## Section 3 - Delta vs SK-V10

P1-C does not own product delta extraction; P1-F owns row delta and P1-D owns
c/B and PMU. W0 has no prior-SK machine-readable Mode III probe comparator in
this artefact, so no SK-V10-to-SK-V11 probe delta is admitted here.

The product-plane context from W0 remains:

| Family | SK-V11-open state | P1-C treatment |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | diagnostic only; not SOTA target |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | primary closure surface for later waves; P1-C only exposes masking signals |
| `real_typed_struct` | 7 `A / GO` | guard/product surface; PMU and Time Profiler data handled separately |

The 13 direct residual rows are unchanged as W0 context:

| Row | Track 1 | Track 2 | sonic direct | floor | Track 1 gap | Track 2 gap | W0 note |
|---|---:|---:|---:|---:|---:|---:|---|
| `twitter` | 11613 | 10816 | 15113 | 13740 | 2127 | 2924 | residual |
| `canada` | 10316 | 9819 | 11700 | 10637 | 321 | 818 | residual |
| `github_events` | 11918 | 10596 | 14743 | 13403 | 1485 | 2807 | residual |
| `update_center` | 8187 | 7474 | 11064 | 10059 | 1872 | 2585 | residual |
| `mesh` | 8561 | 8652 | 9542 | 8675 | 114 | 23 | near-floor residual |
| `random` | 7693 | 6949 | 8665 | 7878 | 185 | 929 | near-floor residual |
| `gsoc-2018` | 2665 | 2578 | 4110 | 3737 | 1072 | 1159 | residual |
| `instruments` | 11569 | 10736 | 9865 | 8969 | -2600 | -1767 | W0-clamped non-admission |
| `numbers` | 4479 | 2366 | 2667 | 2425 | -2054 | 59 | W0-clamped; Track 2 still short |
| `unicode_mixed` | 3753 | 2427 | 2846 | 2588 | -1165 | 161 | W0-clamped; Track 2 still short |
| `unicode_escapes` | 1345 | 1341 | 3785 | 3441 | 2096 | 2100 | residual |
| `distinct_values` | 1750 | 1625 | 2923 | 2658 | 908 | 1033 | residual |
| `y_string_unicode` | 1983 | 1029 | 4344 | 3950 | 1967 | 2921 | residual |

`instruments`, `numbers`, and `unicode_mixed` stay `N-direct / NO-GO` because
W0 captures are planning evidence, not behavior-wave admissions.

## Section 4 - Anomalies and masking signals

1. `host_call_eager_decode` fires `MASKING` on all 17 corpora. The largest
   ratios are `numbers` at 8.37x T1, `unicode_escapes` at 6.48x T1, and
   `marine_ik` at 4.68x T1. This confirms that eager decode/materialization is
   a cost probe and must not be read as an admissible production plan.
2. `cold_first_parse` is `reported cold-sensitive` for `random` at 2.20x T1
   and `numbers` at 4.27x T1. The remaining corpora are within the W0 <=2.00x
   diagnostic threshold.
3. `alternate_scalar_plan` is reported-only. It is faster than T1 on
   `y_string_unicode` in this W0 probe table (0.75x T1) and near parity on
   `unicode_mixed` (1.09x T1), but no scalar-plan behavior claim is admitted.
4. Structural scan is below the 40000 Mbps aarch64 floor in W0. The strongest
   rows are `mesh` at 20114 Mbps, `unicode_escapes` at 19826 Mbps, and
   `numbers` at 18514 Mbps; `canada` is explicitly recorded at 14249 Mbps.
5. Lazy-tape allocation pressure is most visible on `y_string_unicode` (0.75x
   input allocated), `mesh` (0.72x), `marine_ik` (0.70x), `random` (0.51x),
   and `unicode_basic` (0.50x). The escape-heavy rows carry sparse flags
   (`unicode_mixed` 9795, `unicode_escapes` 9385, `y_string_unicode` 9000,
   `gsoc-2018` 8545), which is diagnostic substrate evidence only.

No behavior route is proposed here. The W3 union/event/class-column/
streaming-cursor/class-lane/sidecar substrate family remains pre-blocked, and
Mode III diagnostics do not reopen it.

## Section 5 - Sources

- W0 baseline doc: `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md`.
- W0 result authority: `skinny/RESULTS.md`.
- Criterion root: `/tmp/skv11-open-criterion-3ce75df`.
- Target root: `/tmp/skv11-open-target-3ce75df`.
- Run id: `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- Probe estimates: `/tmp/skv11-open-criterion-3ce75df/json_probes_<corpus>/{host_call_eager_decode,alternate_scalar_plan,cold_first_parse}/new/estimates.json`.
- Probe benchmark metadata: `/tmp/skv11-open-criterion-3ce75df/json_probes_<corpus>/{host_call_eager_decode,alternate_scalar_plan,cold_first_parse}/new/benchmark.json`.
- Additional W0 masking probe present but not used as a throughput row:
  `/tmp/skv11-open-criterion-3ce75df/json_probes_<corpus>/host_call_dispatch_overhead/new/{estimates.json,benchmark.json}`. Gate code treats its probe bytes as zero.
- Structural scan estimates:
  `/tmp/skv11-open-criterion-3ce75df/simd_structural_scan/<corpus>_{simd,scalar}/new/estimates.json`.
- Structural scan benchmark metadata:
  `/tmp/skv11-open-criterion-3ce75df/simd_structural_scan/<corpus>_{simd,scalar}/new/benchmark.json`.
- Structural scan report index:
  `/tmp/skv11-open-criterion-3ce75df/simd_structural_scan/report/index.html`.
- Top-level Criterion report index:
  `/tmp/skv11-open-criterion-3ce75df/report/index.html`.
