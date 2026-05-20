# SK-V12 P1-C: Masking probes and structural scan diagnostics

Pass: S-P1 Profile. Cycle: V12.
Date: 2026-05-20.
Scope: Mode III diagnostic extraction for `host_call_eager_decode`,
`alternate_scalar_plan`, `cold_first_parse`, and structural-scan-only evidence.
Output: this file.
Baseline: SK-V12-open (`50bd1648`), seeded from SK-V11 close / REDRESS 120
with unchanged `skinny/RESULTS.md` row surface.
Host triple: `aarch64-apple-darwin`; host facts record Apple M5 Max.
Build flags: W0 Criterion `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`;
fresh `/tmp/skv12-p1` capture uses release binaries built for native aarch64.
Profile tool: W0 Criterion diagnostic artifacts under
`/tmp/skv11-open-criterion-3ce75df` plus fresh `/tmp/skv12-p1`
PMU/samply/xctrace baseline capture. No separate fresh Mode III samply
call-stack capture is claimed here.
Corpus coverage: 17/17 JSON corpora for W0 masking and structural-scan facts;
17/17 for fresh parse PMU baseline rows; 0/17 for fresh Mode III samply
call-stack probe rows, absent by capture shape.

Shared provenance:

- SK-V12 source/documentation SHA: `50bd1648`.
- W0 Criterion run id: `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- W0 Criterion root: `/tmp/skv11-open-criterion-3ce75df`.
- Fresh capture root: `/tmp/skv12-p1`.
- Fresh capture completion markers: `/tmp/skv12-p1/pmu/done.txt`
  (`2026-05-20T06:34:59Z`) and `/tmp/skv12-p1/samply/done.txt`
  (`2026-05-20T06:41:16Z`).
- Toolchain observed for this extraction: `rustc 1.96.0-nightly
  (02c7f9bec 2026-04-10)`, LLVM 22.1.2, `aarch64-apple-darwin`.

## Section 1 - Method

This is source-read and artifact extraction only. I did not run a fresh
Criterion Mode III suite, did not run a fresh structural-scan Criterion suite,
did not edit behavior source, did not edit `skinny/RESULTS.md`, and did not
stage or commit.

W0 gate authority is carried from REDRESS 119 and 120:

```sh
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --advisory
```

Fresh baseline capture presence was checked with:

```sh
find /tmp/skv12-p1/samply -maxdepth 2 -type f -name '*.json.gz' | wc -l
find /tmp/skv12-p1/samply -maxdepth 2 -type f -name '*.json.gz' | sed 's#/tmp/skv12-p1/samply/##; s#/.*##' | sort | uniq -c
wc -l /tmp/skv12-p1/pmu/parse_pmu_rows.tsv /tmp/skv12-p1/pmu/product_pmu_rows.tsv /tmp/skv12-p1/pmu/capture_status.tsv
```

Those checks showed 82 samply captures: 34 `parse`, 34 `direct`, and 14
`typed`. There is no `/tmp/skv12-p1/samply/probes`, no
`json_probes_*` capture under `/tmp/skv12-p1`, and no structural-scan capture
under `/tmp/skv12-p1`.

The V1 hardening fold records this as an explicit boundary in
`skv12-p1-capture-manifest.md`: Mode III throughput and structural-scan values
are W0 raw Criterion diagnostic nonproducer evidence only. S-P2/S-P3 may not
use Mode III call-stack symbols as fresh SK-V12 hot-leaf authority unless a
later capture supplies those call stacks explicitly.

The W0 Mode III table was recomputed from raw Criterion JSON with:

```sh
for corpus in twitter citm_catalog canada apache_builds github_events update_center mesh random gsoc-2018 marine_ik instruments numbers unicode_mixed unicode_escapes unicode_basic distinct_values y_string_unicode; do
  bytes=$(jq -r '.throughput.Bytes' "/tmp/skv11-open-criterion-3ce75df/json_probes_${corpus}/host_call_eager_decode/new/benchmark.json")
  t1ns=$(jq -r '.slope.point_estimate // .mean.point_estimate' "/tmp/skv11-open-criterion-3ce75df/json_${corpus}/track1_generated/new/estimates.json")
  eager=$(jq -r '.slope.point_estimate // .mean.point_estimate' "/tmp/skv11-open-criterion-3ce75df/json_probes_${corpus}/host_call_eager_decode/new/estimates.json")
  alt=$(jq -r '.slope.point_estimate // .mean.point_estimate' "/tmp/skv11-open-criterion-3ce75df/json_probes_${corpus}/alternate_scalar_plan/new/estimates.json")
  cold=$(jq -r '.slope.point_estimate // .mean.point_estimate' "/tmp/skv11-open-criterion-3ce75df/json_probes_${corpus}/cold_first_parse/new/estimates.json")
  simdns=$(jq -r '.slope.point_estimate // .mean.point_estimate' "/tmp/skv11-open-criterion-3ce75df/simd_structural_scan/${corpus}_simd/new/estimates.json")
  scalarns=$(jq -r '.slope.point_estimate // .mean.point_estimate' "/tmp/skv11-open-criterion-3ce75df/simd_structural_scan/${corpus}_scalar/new/estimates.json")
  awk -v c="$corpus" -v b="$bytes" -v t="$t1ns" -v e="$eager" -v a="$alt" -v cold="$cold" -v s="$simdns" -v sc="$scalarns" 'BEGIN{printf "%s\t%.0f\t%.0f\t%.2f\t%.0f\t%.2f\t%.0f\t%.2f\t%.0f\t%.0f\n", c, b*8*1000/t, b*8*1000/e, e/t, b*8*1000/a, a/t, b*8*1000/cold, cold/t, b*8*1000/s, b*8*1000/sc }'
done
```

The fresh parse baseline column was extracted with:

```sh
awk 'BEGIN{FS="\t"} NR==1{next} $2=="track1"{printf "| `%s` | %.0f | %.3f | %.0f |\n", $1,$8,$11,$10}' /tmp/skv12-p1/pmu/parse_pmu_rows.tsv
```

Source surfaces read:

| Surface | Source locus |
|---|---|
| Probe bodies | `skinny/crates/bbnf-bench/benches/json_parity.rs:381-455` |
| Configured probe matrix | `skinny/crates/bbnf-bench/src/probes.rs:1-57` |
| Probe report rows and signals | `skinny/crates/bbnf-bench/src/bin/gate.rs:1859-1948` |
| Structural-scan bench and metadata | `skinny/crates/bbnf-bench/benches/simd_scan.rs:9-95` |
| Structural scan wrappers and hash | `skinny/crates/bbnf-bench/src/scan.rs:1-20` |
| Diagnostic nonproducer validation | `skinny/crates/bbnf-bench/src/report.rs:396-398` |
| Probe markdown rendering | `skinny/crates/bbnf-bench/src/report.rs:535-551`, `:711-724` |
| Strict admission freshness guard | `skinny/crates/bbnf-bench/src/gate.rs:150-182` |
| SIMD corpus parity | `skinny/crates/bbnf-simd/tests/corpus_parity.rs:1-17` |
| AArch64 structural terminator parity | `skinny/crates/bbnf-simd/tests/checkasm_structural_terminator_64.rs:1-62` |
| Byte-class / emit primitive parity | `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs:1-49`, `skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs:1-60` |

## Section 2 - Findings

`nsx` below is `probe ns / W0 Track 1 parse ns`; values above 1.00 are slower
than the W0 Track 1 parse denominator. The fresh parse column is a `/tmp/skv12-p1`
PMU baseline recapture only and is not used to re-denominate W0 Criterion
probe ratios.

| Corpus | W0 T1 parse Mbps | Fresh parse T1 Mbps/cB | eager Mbps/nsx/signal | alt scalar Mbps/nsx/signal | cold Mbps/nsx/signal | structural simd/scalar Mbps |
|---|---:|---:|---|---|---|---:|
| `twitter` | 10474 | 16334 / 2.214 | 4292 / 2.44 / MASKING >1.15x T1 | 6623 / 1.58 / reported | 11737 / 0.89 / PASS <=2.00x T1 | 8252 / 2984 |
| `citm_catalog` | 26791 | 31987 / 1.123 | 7024 / 3.81 / MASKING >1.08x T1 | 7820 / 3.43 / reported | 27508 / 0.97 / PASS <=2.00x T1 | 8796 / 4561 |
| `canada` | 15544 | 18309 / 1.933 | 3895 / 3.99 / MASKING >1.02x T1 | 4422 / 3.52 / reported | 16292 / 0.95 / PASS <=2.00x T1 | 14249 / 3715 |
| `apache_builds` | 12733 | 13366 / 2.737 | 4672 / 2.73 / MASKING >1.10x T1 | 6522 / 1.95 / reported | 12294 / 1.04 / PASS <=2.00x T1 | 7520 / 4469 |
| `github_events` | 14805 | 16029 / 2.281 | 5470 / 2.71 / MASKING >1.10x T1 | 7871 / 1.88 / reported | 14496 / 1.02 / PASS <=2.00x T1 | 7597 / 4307 |
| `update_center` | 11493 | 12516 / 2.893 | 3278 / 3.51 / MASKING >1.10x T1 | 4576 / 2.51 / reported | 11485 / 1.00 / PASS <=2.00x T1 | 6965 / 4342 |
| `mesh` | 13325 | 13334 / 2.653 | 4836 / 2.76 / MASKING >1.10x T1 | 4666 / 2.86 / reported | 12636 / 1.05 / PASS <=2.00x T1 | 20114 / 2624 |
| `random` | 7747 | 10281 / 3.519 | 1754 / 4.42 / MASKING >1.10x T1 | 2299 / 3.37 / reported | 3522 / 2.20 / reported cold-sensitive | 3787 / 3020 |
| `gsoc-2018` | 4887 | 24009 / 1.481 | 1602 / 3.05 / MASKING >1.10x T1 | 2442 / 2.00 / reported | 6862 / 0.71 / PASS <=2.00x T1 | 9165 / 3155 |
| `marine_ik` | 10675 | 13674 / 2.556 | 2280 / 4.68 / MASKING >1.10x T1 | 3897 / 2.74 / reported | 12297 / 0.87 / PASS <=2.00x T1 | 8003 / 3749 |
| `instruments` | 16574 | 17458 / 2.028 | 5155 / 3.21 / MASKING >1.10x T1 | 4745 / 3.49 / reported | 15812 / 1.05 / PASS <=2.00x T1 | 8093 / 4539 |
| `numbers` | 17941 | 19951 / 1.742 | 2144 / 8.37 / MASKING >1.10x T1 | 1345 / 13.34 / reported | 4205 / 4.27 / reported cold-sensitive | 18514 / 4472 |
| `unicode_mixed` | 1883 | 8412 / 4.297 | 1020 / 1.85 / MASKING >1.10x T1 | 1721 / 1.09 / reported | 1979 / 0.95 / PASS <=2.00x T1 | 9768 / 7865 |
| `unicode_escapes` | 3733 | 12660 / 2.819 | 576 / 6.48 / MASKING >1.10x T1 | 1374 / 2.72 / reported | 2600 / 1.44 / PASS <=2.00x T1 | 19826 / 7059 |
| `unicode_basic` | 3217 | 12297 / 2.865 | 1242 / 2.59 / MASKING >1.10x T1 | 997 / 3.23 / reported | 1891 / 1.70 / PASS <=2.00x T1 | 7693 / 5109 |
| `distinct_values` | 2335 | 9957 / 3.585 | 1487 / 1.57 / MASKING >1.10x T1 | 1881 / 1.24 / reported | 2921 / 0.80 / PASS <=2.00x T1 | 8219 / 5246 |
| `y_string_unicode` | 1965 | 6282 / 5.622 | 581 / 3.38 / MASKING >1.10x T1 | 2612 / 0.75 / reported | 2053 / 0.96 / PASS <=2.00x T1 | 9916 / 5564 |

Immediate diagnostic readings:

- `host_call_eager_decode` is `MASKING` on all 17 corpora. The largest ns
  ratios are `numbers` 8.37x, `unicode_escapes` 6.48x, `marine_ik` 4.68x,
  `random` 4.42x, and `canada` 3.99x.
- `cold_first_parse` is cold-sensitive only on `random` at 2.20x and
  `numbers` at 4.27x. The other 15 rows remain inside the W0 2.00x diagnostic
  threshold.
- `alternate_scalar_plan` is report-only by gate code. It is faster than the
  W0 Track 1 denominator on `y_string_unicode` (0.75x) and near parity on
  `unicode_mixed` (1.09x), but it is not a product route or admission claim.
- Structural scan is below the current 40000 Mbps aarch64 floor for every raw
  W0 row. The strongest rows are `mesh` 20114 Mbps, `unicode_escapes` 19826
  Mbps, and `numbers` 18514 Mbps; `skinny/RESULTS.md` explicitly records
  `canada` at 14249 Mbps against that floor.

Absent or unmeasurable cells are kept absent:

| Cell | Coverage | Status |
|---|---:|---|
| Fresh Mode III samply call stack for `host_call_eager_decode` | 0/17 | Absent from `/tmp/skv12-p1`; do not infer symbols from parse/direct captures. |
| Fresh Mode III samply call stack for `alternate_scalar_plan` | 0/17 | Absent from `/tmp/skv12-p1`; W0 Criterion throughput only. |
| Fresh Mode III samply call stack for `cold_first_parse` | 0/17 | Absent from `/tmp/skv12-p1`; W0 Criterion throughput only. |
| Fresh structural-scan Criterion under `/tmp/skv12-p1` | 0/17 | Absent; structural values are W0 raw Criterion extraction. |
| `host_call_dispatch_overhead` Mbps | 0/17 | Gate sets probe bytes to zero, so Mbps is intentionally unmeasurable; only ns thresholding exists. |
| `alternate_dispatch_table_plan` | 0/17 | Gate emits an invalid disabled row: duplicate-probe route regressed. |
| `alternate_pext_mask_plan` on this host | 0/17 | `json_parity.rs` compiles it only for x86/x86_64; current host is aarch64. |
| Product admission from probes or structural scan | 0/17 | Pre-blocked: report metadata marks `structural_scan+masking_probes+pmu+cycles:nonproducer`. |

The raw artifact counts match that absence model: W0 has 68 probe
`estimates.json` files (17 corpora times four measured probes) and 34
structural scan `estimates.json` files (17 corpora times SIMD/scalar). Fresh
`/tmp/skv12-p1` has parse/direct/typed captures only.

## Section 3 - Delta vs SK-V11

There is no SK-V12 row movement in this P1-C artifact. SK-V12 opens from
REDRESS 120, which states that SK-V11 closed as a measured fixpoint with no
behavior source, generated runtime, benchmark body, gate semantic, or
`skinny/RESULTS.md` change. The product surface remains:

| Family | SK-V12-open state | P1-C treatment |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | diagnostic only; never SOTA admission |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | guard plus pre-blocked residual fixpoint |
| `real_typed_struct` | 7 `A / GO` | admitted product guard rows |
| non-JSON generated parser | no admitted baseline | first material SK-V12 target, outside this P1-C diagnostic |
| overall | `N-direct / NoGo` | unchanged seed outcome |

The 13 direct residual rows remain closed by REDRESS 119 unless later passes
name fresh material evidence beyond REDRESS 114-119:

| Row | Track 1 | Track 2 | sonic direct | floor | P1-C disposition |
|---|---:|---:|---:|---:|---|
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 | pre-blocked by W5/W7/W8 fixpoint |
| `canada/direct_to_struct` | 10316 | 9819 | 11700 | 10637 | pre-blocked by W3/W8 fixpoint |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 | pre-blocked by W5/W7/W8 fixpoint |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 | pre-blocked by W5/W7/W8 fixpoint |
| `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 8675 | REDRESS 114 measured numeric route below floor |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 | REDRESS 115 measured container-tail route below floor |
| `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3737 | pre-blocked by W5/W7/W8 fixpoint |
| `instruments/direct_to_struct` | 11569 | 10736 | 9865 | 8969 | W0-clamped; docs-only admission pre-blocked |
| `numbers/direct_to_struct` | 4479 | 2366 | 2667 | 2425 | W0-clamped; W3 numeric route rejected |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 | W0-clamped; W6 decoded-source route blocked |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 | W5/W6 and proof-only limits pre-block |
| `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 2658 | pre-blocked by W5/W7/W8 fixpoint |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 | pre-blocked by W5/W6/W8 fixpoint |

The fresh `/tmp/skv12-p1` parse PMU rows are useful recapture evidence for
P1-D/P1-E, but they do not update the bench-gate row surface and do not create
a Mode III call-stack profile.

## Section 4 - Anomalies and masking signals

1. Eager decode remains the clearest masking probe. It is intentionally gross:
   it parses and walks string materialization through host calls. Because it
   fires on all 17 corpora, it is evidence that eager decode/materialization is
   expensive, not evidence that an eager route should be implemented.
2. `numbers` is the strongest Mode III anomaly: eager 8.37x, alternate scalar
   13.34x, cold 4.27x, structural SIMD 18514 Mbps. It remains a diagnostic row
   only; REDRESS 119 keeps the `numbers/direct_to_struct` residual pre-blocked
   by W0 clamp plus rejected W3 numeric evidence.
3. `y_string_unicode` is the only `alternate_scalar_plan` row faster than W0
   Track 1, but the row is string/unicode heavy and direct residual movement is
   pre-blocked by W5/W6/W8. This is not a scalar-plan dispatch.
4. Structural scan does not clear the current aarch64 floor. More importantly,
   prior W3 attempts already measured parser-retained structural substrates as
   losing routes; a structural-scan observation cannot reopen a sidecar,
   retained vector, class-column, streaming cursor, `UnionTape`, or class-lane
   route.
5. Lazy-tape notes in `skinny/RESULTS.md` show zero payload bytes across all
   17 corpora. The largest allocated/input ratios are `y_string_unicode` 0.75x,
   `mesh` 0.72x, `marine_ik` 0.70x, `random` 0.51x, and `unicode_basic` 0.50x.
   That is substrate-shape evidence only.

No behavior route is proposed here. S-P1 measures; S-P2/S-P3 may only use this
as evidence after respecting the SK-V12 priority order: generated non-JSON
baseline first, same-row grammar-generalized intervention second, JSON direct
residuals only after the REDRESS 119/120 reopen burden is met.

## Section 5 - Pre-blocks carried forward

- Parse-only remains diagnostic. It cannot admit product rows, close SK-V12,
  or count as SOTA.
- W3 substrate routes remain pre-blocked: union/event/class-column,
  streaming-cursor, class-lane-only, sidecar structural projection,
  retained structural vector, `UnionTape`, parser-owned side table, and
  W4-through-W3 cascade-lock.
- PMU, cycles, structural-scan, masking-probe, sidecar freshness, and parser
  inventory rows are nonproducers. The report validator recognizes only the
  nonproducer status `structural_scan+masking_probes+pmu+cycles:nonproducer`
  for the W0 JSON surface.
- JSON direct residual row movement remains pre-blocked by REDRESS 119/120
  unless later passes provide fresh hot-leaf evidence, a materially different
  source delta, scalar/oracle proof, same-host microbench, independent Track 2,
  strict same-run sonic-rs direct floor evidence, and same-wave gate
  consumption.
- x86 implementation work is out of scope for this aarch64 SK-V12 profile.

## Section 6 - Sources

- Profile contract: `restart/prompts/skinny/PASS-1-PROFILE.md`.
- SK-V12 authorities: `restart/skinny/tranches/sk-v12/SYNTHESIS.md`,
  `restart/skinny/tranches/sk-v12/HANDOFF.md`,
  `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`.
- Result authority: `skinny/RESULTS.md`.
- Redress authority: `skinny/REDRESS.md`, especially REDRESS 119 and 120.
- Prior accepted Mode III precedent:
  `restart/skinny/tranches/sk-v11/research/p1/p1c-samply-mode-3.md`.
- Prior hardening precedent:
  `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`,
  `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-V3-CONSOLIDATED.md`,
  `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`.
- W0 Criterion root:
  `/tmp/skv11-open-criterion-3ce75df`.
- Fresh SK-V12 P1 capture root:
  `/tmp/skv12-p1`.
- SK-V12 P1 capture manifest:
  `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`.
- Fresh PMU rows:
  `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`,
  `/tmp/skv12-p1/pmu/product_pmu_rows.tsv`,
  `/tmp/skv12-p1/pmu/capture_status.tsv`.
- Fresh samply captures:
  `/tmp/skv12-p1/samply/{parse,direct,typed}`.
