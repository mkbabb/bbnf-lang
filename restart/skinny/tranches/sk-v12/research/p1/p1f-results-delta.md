# SK-V12 P1-F: RESULTS Extraction And Delta

Pass: S-P1 Profile. Cycle: V12.
Date: 2026-05-20.
Scope: extract the live `skinny/RESULTS.md` surface, compare SK-V12-open to
SK-V11 close, classify outcomes, and flag stale or absent telemetry honestly.
Output: this file.
Baseline: SK-V12-open freshness binding over unchanged SK-V11 close
(`db2c999b0b3e564b13cf2f5b8aa9858c8b16cb3a`); current HEAD
`50bd1648918e01b70c5061b8739e6f1ea4f77a90`; measured W0 source anchor
`3ce75df4e06b46eab8ca758c5ac32422aaad767c`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`.
Profile tool: RESULTS extraction plus git/status/source-read checks; fresh
PMU/samply/xctrace inventory under `/tmp/skv12-p1`.
Corpus coverage: 17/17 corpora; 41/41 main RESULTS rows; 41/41 telemetry
manifest rows.

Shared capture provenance:

- Live RESULTS run id remains
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`. No SK-V12-specific
  Criterion render is present in `skinny/RESULTS.md`.
- Fresh capture root `/tmp/skv12-p1` exists. Its
  `/tmp/skv12-p1/pmu/capture_status.tsv` has 328 PASS rows: 82 PMU captures
  (34 parse, 34 direct, 14 typed), 82 samply captures, and 164 xctrace captures
  (82 time-profiler plus 82 CPU-counters).
- Log/file inventory observed: 752 log files, including 260 `pmu-*`, 164
  `samply-*`, and 328 `xctrace-*` log files by filename; PMU result TSVs carry
  34 parse rows and 48 product rows after headers.
- These fresh capture artifacts are profiling evidence only in this P1-F
  artifact. They are not consumed by `skinny/RESULTS.md` as row movement, hot
  leaf symbol resolution, or direct/typed admission evidence.

## Section 1 - Method

Commands run:

```bash
git status --short
nl -ba restart/prompts/skinny/PASS-1-PROFILE.md | sed -n '1,280p'
nl -ba restart/skinny/tranches/sk-v12/SYNTHESIS.md | sed -n '1,420p'
nl -ba restart/skinny/tranches/sk-v12/HANDOFF.md | sed -n '1,320p'
nl -ba restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md | sed -n '1,360p'
nl -ba restart/skinny/tranches/sk-v11/research/close/close-redress.md | sed -n '1,260p'
nl -ba restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md | sed -n '1,260p'
nl -ba skinny/RESULTS.md | sed -n '1,520p'
nl -ba skinny/REDRESS.md | sed -n '3200,3565p'
python3 restart/skinny/tranches/sk-v10/research/p1/tools/extract_results_main_table.py \
  skinny/RESULTS.md /tmp/skv12-p1f-current-results.csv
git diff --name-status db2c999b..HEAD -- skinny/RESULTS.md skinny/REDRESS.md
git diff --name-status 3ce75df4..HEAD -- skinny
git diff --name-status 3ce75df4..HEAD -- \
  skinny/crates/codegen skinny/crates/runtime skinny/crates/bbnf-simd \
  skinny/crates/parse-that skinny/crates/bbnf-bench/src/bin/gate.rs \
  skinny/crates/bbnf-bench/src/report.rs skinny/RESULTS.md skinny/REDRESS.md
rg -n "ensure_runtime_profile|runtime emission currently supports grammar profile|emit_from_source|emit_typed_from_source|json_provider::ensure_runtime_profile" \
  skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/json_provider.rs
find skinny/crates/runtime/src/grammars -maxdepth 3 -type f | sort
awk 'NR>1{fam[$1]++; status[$1":"$4]++} END{for (f in fam) print f, fam[f]; print "-- statuses --"; for (s in status) print s,status[s]}' \
  /tmp/skv12-p1/pmu/capture_status.tsv | sort
rg -o 'freshness=[^,\]]+' skinny/RESULTS.md | sort | uniq -c
rg -o 'sidecar=[^,\]]+' skinny/RESULTS.md | sort | uniq -c
```

`git status --short` was clean before writing this artifact. The SK-V11 close
comparison command produced no `skinny/RESULTS.md` or `skinny/REDRESS.md` diff
from `db2c999b` to HEAD.

## Section 2 - Current RESULTS Surface

The live row table is unchanged from SK-V11 close. Physical labels still say
`SK-V9-open` because the report was not re-rendered for SK-V12; S-P1 should
treat the SK-V12-open surface as a freshness binding over that close evidence,
not as new row telemetry.

| Family | Live extraction | SK-V12 role | Delta vs SK-V11 close |
|---|---:|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | diagnostic only | none |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | guard rows plus pre-blocked residual fixpoint | none |
| `real_typed_struct` | 7 `A / GO` | product-plane guard rows | none |
| generated non-JSON | no admitted generated baseline row | first material SK-V12 target | absent |
| Overall | `N-direct / NoGo` | seed outcome | none |

Observed main-table outcomes are `A`, `L`, `N-direct`, and `S`; no current row
renders `C`, `G`, or `K`. `S` and `L` remain parse-only diagnostics, not SOTA
admission.

## Section 3 - Per-Row Extraction And Delta

Every SK-V12-open delta below is zero because `skinny/RESULTS.md` is unchanged
from SK-V11 close. `Delta vs sonic` is the live Track 1 comparison in the row.

| Row | Outcome | Verdict | T1 | T2 | sonic strict | Delta vs sonic | Delta vs SK-V11 close |
|---|---|---|---:|---:|---:|---:|---|
| `twitter/parse_only` | `S` | `NO-GO` | 10474 | 7757 | 16988 | -38.3% | unchanged |
| `twitter/direct_to_struct` | `N-direct` | `NO-GO` | 11613 | 10816 | 15113 | -23.2% | unchanged |
| `twitter/real_typed_struct` | `A` | `GO` | 17740 | 15912 | 15010 | +18.2% | unchanged |
| `citm_catalog/parse_only` | `S` | `NO-GO` | 26791 | 18271 | 21564 | +24.2% | unchanged |
| `citm_catalog/direct_to_struct` | `A` | `GO` | 18563 | 17787 | 15530 | +19.5% | unchanged |
| `citm_catalog/real_typed_struct` | `A` | `GO` | 30539 | 17675 | 20726 | +47.3% | unchanged |
| `canada/parse_only` | `L` | `NO-GO` | 15544 | 16215 | 13462 | +15.5% | unchanged |
| `canada/direct_to_struct` | `N-direct` | `NO-GO` | 10316 | 9819 | 11700 | -11.8% | unchanged |
| `apache_builds/parse_only` | `S` | `NO-GO` | 12733 | 12196 | 17291 | -26.4% | unchanged |
| `apache_builds/direct_to_struct` | `A` | `GO` | 11254 | 10189 | 10995 | +2.4% | unchanged |
| `apache_builds/real_typed_struct` | `A` | `GO` | 8478 | 6892 | 8106 | +4.6% | unchanged |
| `github_events/parse_only` | `S` | `NO-GO` | 14805 | 12791 | 22578 | -34.4% | unchanged |
| `github_events/direct_to_struct` | `N-direct` | `NO-GO` | 11918 | 10596 | 14743 | -19.2% | unchanged |
| `github_events/real_typed_struct` | `A` | `GO` | 11871 | 12275 | 12224 | -2.9% | unchanged |
| `update_center/parse_only` | `S` | `NO-GO` | 11493 | 9033 | 18962 | -39.4% | unchanged |
| `update_center/direct_to_struct` | `N-direct` | `NO-GO` | 8187 | 7474 | 11064 | -26.0% | unchanged |
| `update_center/real_typed_struct` | `A` | `GO` | 11851 | 10358 | 12467 | -4.9% | unchanged |
| `mesh/parse_only` | `S` | `NO-GO` | 13325 | 12128 | 11679 | +14.1% | unchanged |
| `mesh/direct_to_struct` | `N-direct` | `NO-GO` | 8561 | 8652 | 9542 | -10.3% | unchanged |
| `mesh/real_typed_struct` | `A` | `GO` | 9403 | 7897 | 8923 | +5.4% | unchanged |
| `random/parse_only` | `S` | `NO-GO` | 7747 | 7554 | 14172 | -45.3% | unchanged |
| `random/direct_to_struct` | `N-direct` | `NO-GO` | 7693 | 6949 | 8665 | -11.2% | unchanged |
| `gsoc-2018/parse_only` | `S` | `NO-GO` | 4887 | 4544 | 8472 | -42.3% | unchanged |
| `gsoc-2018/direct_to_struct` | `N-direct` | `NO-GO` | 2665 | 2578 | 4110 | -35.2% | unchanged |
| `marine_ik/parse_only` | `S` | `NO-GO` | 10675 | 11700 | 9376 | +13.9% | unchanged |
| `marine_ik/direct_to_struct` | `A` | `GO` | 8938 | 9437 | 8473 | +5.5% | unchanged |
| `marine_ik/real_typed_struct` | `A` | `GO` | 11788 | 10096 | 9010 | +30.8% | unchanged |
| `instruments/parse_only` | `S` | `NO-GO` | 16574 | 11587 | 19055 | -13.0% | unchanged |
| `instruments/direct_to_struct` | `N-direct` | `NO-GO` | 11569 | 10736 | 9865 | +17.3% | unchanged |
| `numbers/parse_only` | `S` | `NO-GO` | 17941 | 18328 | 13198 | +35.9% | unchanged |
| `numbers/direct_to_struct` | `N-direct` | `NO-GO` | 4479 | 2366 | 2667 | +67.9% | unchanged |
| `unicode_mixed/parse_only` | `S` | `NO-GO` | 1883 | 7326 | 15137 | -87.6% | unchanged |
| `unicode_mixed/direct_to_struct` | `N-direct` | `NO-GO` | 3753 | 2427 | 2846 | +31.9% | unchanged |
| `unicode_escapes/parse_only` | `S` | `NO-GO` | 3733 | 2421 | 7235 | -48.4% | unchanged |
| `unicode_escapes/direct_to_struct` | `N-direct` | `NO-GO` | 1345 | 1341 | 3785 | -64.5% | unchanged |
| `unicode_basic/parse_only` | `S` | `NO-GO` | 3217 | 2985 | 4354 | -26.1% | unchanged |
| `unicode_basic/direct_to_struct` | `A` | `GO` | 2299 | 2227 | 2353 | -2.3% | unchanged |
| `distinct_values/parse_only` | `S` | `NO-GO` | 2335 | 1675 | 4883 | -52.2% | unchanged |
| `distinct_values/direct_to_struct` | `N-direct` | `NO-GO` | 1750 | 1625 | 2923 | -40.1% | unchanged |
| `y_string_unicode/parse_only` | `S` | `NO-GO` | 1965 | 2695 | 6227 | -68.4% | unchanged |
| `y_string_unicode/direct_to_struct` | `N-direct` | `NO-GO` | 1983 | 1029 | 4344 | -54.4% | unchanged |

## Section 4 - REDRESS 119/120 Fixpoint

REDRESS 119 is the direct residual authority. It closes the 13 residual
`direct_to_struct` rows as a measured fixpoint, not as direct `GO`; W8 selected
no behavior source intervention, no W8a split, no gate semantic change, and no
`skinny/RESULTS.md` row movement.

| Row | T1 | T2 | sonic direct | floor | REDRESS 119 disposition |
|---|---:|---:|---:|---:|---|
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 | W5 string-span and W7 digest routes blocked; no W8a source candidate remains |
| `canada/direct_to_struct` | 10316 | 9819 | 11700 | 10637 | W3 numeric route measured-rejected on `mesh`; no W8a numeric candidate remains |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 | W5 blocked; W7 visible-bucket math cannot close both tracks |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 | W5 blocked; W7 digest route floor-insufficient |
| `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 8675 | W3 `number_span_emit_slot` measured 3835 / 3614 and was reverted |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 | W4 `container_tail_next` probe measured 3518 / 3498 and was reverted |
| `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3737 | W5/W7 leave no accepted source authority |
| `instruments/direct_to_struct` | 11569 | 10736 | 9865 | 8969 | numerically above floor but W0-clamped; docs-only admission pre-blocked |
| `numbers/direct_to_struct` | 4479 | 2366 | 2667 | 2425 | Track 2 misses floor and row is W0-clamped; W3 rejected |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 | Track 2 misses floor and row is W0-clamped; W6 blocked |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 | W5/W6 and prior proof-only limits block the route |
| `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 2658 | W5 blocked; W7 digest bucket insufficient |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 | W5/W6 and prior proof-only limits block the route |

REDRESS 120 closes SK-V11 as a measured fixpoint and Alpha feedback packet:
no behavior source, generated runtime, benchmark body, gate semantic, or
`skinny/RESULTS.md` change; final surface stays `parse_only` 16 `S / NO-GO`
plus 1 `L / NO-GO`, `direct_to_struct` 4 `A / GO` plus 13 `N-direct / NO-GO`,
`real_typed_struct` 7 `A / GO`, and overall `N-direct / NoGo`.

## Section 5 - SK-V12 Alpha/G-Alpha Goalset

SK-V12 is not a JSON direct retry. The binding priority order from
SYNTHESIS/HANDOFF/G-Alpha is:

1. Stand up exactly one generated non-JSON direct or typed parser baseline
   first, preferred order CSS L4 declaration values, Sheets, then BBNF-self.
2. Admit one measured grammar-generalized intervention against that same
   baseline and output plane, clearing at least `ceil(baseline_mbps * 1.01)`
   unless S-P3 tightens the threshold.
3. Preserve the 4 direct `A / GO` guard rows and 7 typed `A / GO` guard rows.
4. Keep `parse_only` diagnostic only.
5. Keep JSON direct residual rows pre-blocked by REDRESS 119/120 unless a later
   pass names fresh material evidence beyond REDRESS 114-119, after the
   non-JSON priority succeeds or explicitly blocks.

The generated non-JSON baseline is therefore the first material target. The
only admitted non-JSON surface today is the REDRESS 111 companion report lane
for `--w1a-non-json-report`; it is not a generated baseline, not an admission
row, and not row movement in `skinny/RESULTS.md`.

Fresh source reads preserve the REDRESS 112 blocker: `emit_from_source` and
`emit_typed_from_source` still route through
`json_provider::ensure_runtime_profile`, and
`skinny/crates/runtime/src/grammars/` contains generated `json` plus
`sheets_witness`, with no generated `css_l4` or `css_l4_declaration_values`
runtime.

## Section 6 - Telemetry Freshness And Absence

- SK-V12-open vs SK-V11 close: no `skinny/RESULTS.md` or `skinny/REDRESS.md`
  diff from `db2c999b` to HEAD. All per-row outcome and Mbps deltas are
  unchanged.
- Source delta since `3ce75df4`: under `skinny/`, the diff is limited to
  `skinny/REDRESS.md`, `skinny/RESULTS.md`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`, and
  `skinny/crates/bbnf-bench/src/report.rs`. A parser-source filter over
  codegen/runtime/bbnf-simd/parse-that shows no changed paths; the only source
  code delta is gate/report consumer code in `bbnf-bench`.
- The row manifest still has 41 JSON rows and zero CSS L4, Sheets, or BBNF-self
  generated baseline rows.
- Run identity is stale by SK-V12 name: every manifest row still carries
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`; no `SK-V12-open` run id is
  rendered.
- Main-table hot leaves are Criterion slope artifact bindings, not resolved
  samply symbols with `% self-time` and source file:line. The fresh samply
  capture root exists, but P1-E must resolve and bind symbols before those
  fields are profile-complete.
- Strictness remains mostly deferred: 39 rows are `deferred` /
  `view-boundary`; only `apache_builds/direct_to_struct` and
  `github_events/real_typed_struct` are `strict` / `measured-row`.
- Comparator freshness counts in the manifest are 99 `same-run-native`, 25
  `historical:sk-v7-sidecar-profile`, and 221 absent sidecar entries split as
  77 `absent:not-collected-for-parse_only`, 102
  `absent:not-collected-for-direct_to_struct`, and 42
  `absent:not-collected-for-real_typed_struct`.
- `CostFacts` remains `none:pre-W1:none:pre-W1:none:pre-W1` for every manifest
  row. The row-level `Redress` field is `none` across the manifest.
- The manifest explicitly marks
  `structural_scan+masking_probes+pmu+cycles:nonproducer`; PMU, cycles,
  structural scan, masking probes, and Criterion slope metadata are diagnostic
  unless a same-wave gate consumes them as admitted behavior evidence.

## Section 7 - Classification

| Classification | Rows | SK-V12 disposition |
|---|---:|---|
| `A / GO` direct guards | 4 | preserve; no silent demotion |
| `A / GO` typed guards | 7 | preserve; no silent demotion |
| `S / NO-GO` parse diagnostics | 16 | profile/health signal only |
| `L / NO-GO` parse diagnostic | 1 | `canada/parse_only`; diagnostic only |
| `N-direct / NO-GO` residual direct | 13 | pre-blocked by REDRESS 119/120 |
| generated non-JSON baseline | 0 | first material target |

Bottom line: SK-V12-open is a freshness rebinding of the SK-V11 measured close,
not a new JSON row movement. The material work is the generated non-JSON
baseline; JSON residual direct rows are already pre-blocked unless future
evidence exceeds the REDRESS 114-119 reopen bar.

## Section 8 - Sources

- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`
- `restart/skinny/tranches/sk-v11/research/close/close-redress.md`
- `restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 120
- `/tmp/skv12-p1/pmu/capture_status.tsv`
- `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`
- `/tmp/skv12-p1/pmu/product_pmu_rows.tsv`
