# SK-V11 P1-F: RESULTS Extraction And Delta

Pass: S-P1 Profile. Cycle: V2 fold.
Date: 2026-05-19.
Scope: extract every live `skinny/RESULTS.md` main row, compare each row to
SK-V10 close where the close snapshot is available, classify current outcomes,
and flag stale or absent schema-v3 telemetry.
Output: this file.
Baseline: SK-V11-open capture commit `3ce75df4`; documentation freeze commit
`9c8da194`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`.
Profile tool: RESULTS extraction plus git snapshot diff, not samply.
Corpus coverage: 17/17 corpora; 41/41 main rows; 41/41 telemetry manifest rows.

Shared capture provenance:

- Run id: `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- Capture root: `/tmp/skv11-p1`; W0 Criterion root:
  `/tmp/skv11-open-criterion-3ce75df`.
- Host/toolchain: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`;
  `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM 22.1.2.
- Source SHA for `xctrace_probe` and `profile_direct`: `3ce75df4`, the last
  behavior/probe source commit before profiling. Documentation/results freeze:
  `9c8da194`. This V2 fold edits docs only.
- Build profile: release with debug symbols, `RUSTFLAGS="-C target-cpu=native"`,
  target directory `/tmp/skv11-profile-target-9c8da194`.
- Binary paths:
  `/tmp/skv11-profile-target-9c8da194/release/xctrace_probe` and
  `/tmp/skv11-profile-target-9c8da194/release/profile_direct`.

Exact build command for the sibling P1 PMU/xctrace binaries:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv11-profile-target-9c8da194 \
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin xctrace_probe --bin profile_direct
```

## Section 1 - Method

Commands:

```bash
git status --short
sed -n '1,240p' restart/prompts/skinny/PASS-1-PROFILE.md
sed -n '1,260p' restart/skinny/tranches/sk-v11/SYNTHESIS.md
sed -n '1,260p' restart/skinny/tranches/sk-v11/HANDOFF.md
sed -n '1,260p' restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md
sed -n '1,360p' skinny/RESULTS.md
sed -n '1,360p' skinny/REDRESS.md
sed -n '1,260p' restart/skinny/tranches/sk-v10/research/close/close-redress.md
sed -n '65,155p' restart/skinny/tranches/sk-v8/SPEC.md
git log --oneline --decorate --all -- skinny/RESULTS.md
git show c16cc915:skinny/RESULTS.md >/tmp/skv10-close-results.md
python3 restart/skinny/tranches/sk-v10/research/p1/tools/extract_results_main_table.py \
  skinny/RESULTS.md /tmp/skv11-p1f-current.csv
python3 restart/skinny/tranches/sk-v10/research/p1/tools/extract_results_main_table.py \
  /tmp/skv10-close-results.md /tmp/skv11-p1f-sk-v10-close.csv
```

The per-row delta tables below join current `skinny/RESULTS.md` to the SK-V10
close snapshot at commit `c16cc915` by `Corpus/Workload`. Numeric deltas are
current minus SK-V10 close in Mbps. The close authority for the prior snapshot
is run id `sk-v9-open:criterion-fnv64-6f007527061ee26d`; current W0 reports
run id `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.

## Section 2 - Findings

The pre-fold dispatch surface said SK-V11-open was 17 parse `S / NO-GO`, 4
direct `A / GO` plus 13 `N-direct / NO-GO`, and 7 typed `A / GO`. The live
extraction from `skinny/RESULTS.md` differed on one row:
`canada/parse_only` renders `L / NO-GO`, not `S / NO-GO`. The orchestrator
folded that correction into `SYNTHESIS.md`, `HANDOFF.md`, and the W0 baseline
document after this P1-F finding.

| Family | Live extraction | Pre-fold dispatch/W0 stated surface | SK-V10 close | Delta vs close |
|---|---:|---:|---:|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | 17 `S / NO-GO` | 17 `S / NO-GO` | one parse outcome downgrade: `canada` `S -> L` |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | 4 `A / GO`, 13 `N-direct / NO-GO` | 6 `A / GO`, 11 `N-direct / NO-GO` | `A / GO` -2; `N-direct / NO-GO` +2 |
| `real_typed_struct` | 7 `A / GO` | 7 `A / GO` | 7 `A / GO` | unchanged |

Observed schema-v3 outcomes are `A`, `L`, `N-direct`, and `S`. No current row
renders `C`, `G`, or `K`. `S` is treated as a valid parse-only diagnostic enum
per `restart/skinny/tranches/sk-v8/SPEC.md` Section 0.3; it is not SOTA
admission evidence.

### Parse Rows

| Row | Now | SK-V10 close | T1 | Delta T1 | T2 | Delta T2 | sonic | Delta sonic | Delta vs sonic |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|
| `twitter/parse_only` | `S / NO-GO` | `S / NO-GO` | 10474 | -5264 | 7757 | -4488 | 16988 | -4225 | -38.3% |
| `citm_catalog/parse_only` | `S / NO-GO` | `S / NO-GO` | 26791 | -3254 | 18271 | -2764 | 21564 | -3796 | +24.2% |
| `canada/parse_only` | `L / NO-GO` | `S / NO-GO` | 15544 | -1741 | 16215 | -582 | 13462 | -659 | +15.5% |
| `apache_builds/parse_only` | `S / NO-GO` | `S / NO-GO` | 12733 | +28 | 12196 | -137 | 17291 | -201 | -26.4% |
| `github_events/parse_only` | `S / NO-GO` | `S / NO-GO` | 14805 | -744 | 12791 | -513 | 22578 | -708 | -34.4% |
| `update_center/parse_only` | `S / NO-GO` | `S / NO-GO` | 11493 | -16 | 9033 | -334 | 18962 | -840 | -39.4% |
| `mesh/parse_only` | `S / NO-GO` | `S / NO-GO` | 13325 | -227 | 12128 | -181 | 11679 | -248 | +14.1% |
| `random/parse_only` | `S / NO-GO` | `S / NO-GO` | 7747 | -2150 | 7554 | -233 | 14172 | -1349 | -45.3% |
| `gsoc-2018/parse_only` | `S / NO-GO` | `S / NO-GO` | 4887 | -18304 | 4544 | -17384 | 8472 | -41018 | -42.3% |
| `marine_ik/parse_only` | `S / NO-GO` | `S / NO-GO` | 10675 | -2380 | 11700 | -566 | 9376 | -633 | +13.8% |
| `instruments/parse_only` | `S / NO-GO` | `S / NO-GO` | 16574 | -321 | 11587 | -249 | 19055 | -589 | -13.0% |
| `numbers/parse_only` | `S / NO-GO` | `S / NO-GO` | 17941 | -1112 | 18328 | -149 | 13198 | -199 | +35.9% |
| `unicode_mixed/parse_only` | `S / NO-GO` | `S / NO-GO` | 1883 | -5998 | 7326 | -763 | 15137 | -2968 | -87.6% |
| `unicode_escapes/parse_only` | `S / NO-GO` | `S / NO-GO` | 3733 | -7817 | 2421 | -9472 | 7235 | -11584 | -48.4% |
| `unicode_basic/parse_only` | `S / NO-GO` | `S / NO-GO` | 3217 | -8672 | 2985 | -7754 | 4354 | -11552 | -26.1% |
| `distinct_values/parse_only` | `S / NO-GO` | `S / NO-GO` | 2335 | -7464 | 1675 | -4538 | 4883 | -13021 | -52.2% |
| `y_string_unicode/parse_only` | `S / NO-GO` | `S / NO-GO` | 1965 | -4424 | 2695 | -3371 | 6227 | -7567 | -68.5% |

### Direct Rows

| Row | Now | SK-V10 close | T1 | Delta T1 | T2 | Delta T2 | sonic | Delta sonic | Delta vs sonic |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|
| `twitter/direct_to_struct` | `N-direct / NO-GO` | `N-direct / NO-GO` | 11613 | -292 | 10816 | -152 | 15113 | -131 | -23.2% |
| `citm_catalog/direct_to_struct` | `A / GO` | `A / GO` | 18563 | -3032 | 17787 | -2805 | 15530 | -4506 | +19.5% |
| `canada/direct_to_struct` | `N-direct / NO-GO` | `N-direct / NO-GO` | 10316 | -274 | 9819 | -467 | 11700 | -457 | -11.8% |
| `apache_builds/direct_to_struct` | `A / GO` | `A / GO` | 11254 | -215 | 10189 | -179 | 10995 | -195 | +2.4% |
| `github_events/direct_to_struct` | `N-direct / NO-GO` | `N-direct / NO-GO` | 11918 | -521 | 10596 | -834 | 14743 | -1463 | -19.2% |
| `update_center/direct_to_struct` | `N-direct / NO-GO` | `N-direct / NO-GO` | 8187 | -238 | 7474 | -146 | 11064 | -122 | -26.0% |
| `mesh/direct_to_struct` | `N-direct / NO-GO` | `N-direct / NO-GO` | 8561 | -1 | 8652 | +56 | 9542 | +120 | -10.3% |
| `random/direct_to_struct` | `N-direct / NO-GO` | `N-direct / NO-GO` | 7693 | -194 | 6949 | -183 | 8665 | -283 | -11.2% |
| `gsoc-2018/direct_to_struct` | `N-direct / NO-GO` | `N-direct / NO-GO` | 2665 | -12391 | 2578 | -11956 | 4110 | -19327 | -35.2% |
| `marine_ik/direct_to_struct` | `A / GO` | `A / GO` | 8938 | -128 | 9437 | +412 | 8473 | +238 | +5.5% |
| `instruments/direct_to_struct` | `N-direct / NO-GO` | `A / GO` | 11569 | -471 | 10736 | -430 | 9865 | -2809 | +17.3% |
| `numbers/direct_to_struct` | `N-direct / NO-GO` | `A / GO` | 4479 | -8140 | 2366 | -9930 | 2667 | -10371 | +67.9% |
| `unicode_mixed/direct_to_struct` | `N-direct / NO-GO` | `N-direct / NO-GO` | 3753 | -947 | 2427 | -2129 | 2846 | -7634 | +31.9% |
| `unicode_escapes/direct_to_struct` | `N-direct / NO-GO` | `N-direct / NO-GO` | 1345 | -3724 | 1341 | -3881 | 3785 | -10362 | -64.5% |
| `unicode_basic/direct_to_struct` | `A / GO` | `A / GO` | 2299 | -6731 | 2227 | -6133 | 2353 | -6587 | -2.3% |
| `distinct_values/direct_to_struct` | `N-direct / NO-GO` | `N-direct / NO-GO` | 1750 | -4553 | 1625 | -4029 | 2923 | -9055 | -40.1% |
| `y_string_unicode/direct_to_struct` | `N-direct / NO-GO` | `N-direct / NO-GO` | 1983 | -3084 | 1029 | -2717 | 4344 | -4867 | -54.3% |

### Typed Rows

| Row | Now | SK-V10 close | T1 | Delta T1 | T2 | Delta T2 | sonic | Delta sonic | Delta vs sonic |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|
| `twitter/real_typed_struct` | `A / GO` | `A / GO` | 17740 | -501 | 15912 | -580 | 15010 | -626 | +18.2% |
| `citm_catalog/real_typed_struct` | `A / GO` | `A / GO` | 30539 | -5596 | 17675 | -1570 | 20726 | -1340 | +47.4% |
| `apache_builds/real_typed_struct` | `A / GO` | `A / GO` | 8478 | -56 | 6892 | -187 | 8106 | -215 | +4.6% |
| `github_events/real_typed_struct` | `A / GO` | `A / GO` | 11871 | -1266 | 12275 | -580 | 12224 | -702 | -2.9% |
| `update_center/real_typed_struct` | `A / GO` | `A / GO` | 11851 | -218 | 10358 | -245 | 12467 | -260 | -4.9% |
| `mesh/real_typed_struct` | `A / GO` | `A / GO` | 9403 | -287 | 7897 | -175 | 8923 | -330 | +5.4% |
| `marine_ik/real_typed_struct` | `A / GO` | `A / GO` | 11788 | -398 | 10096 | +111 | 9010 | -312 | +30.8% |

## Section 3 - Delta vs SK-V10 Close

SK-V10 close seed is available from `git show c16cc915:skinny/RESULTS.md`.
The largest W0 movements are not uniform small variance: several unicode and
string rows were re-rendered against materially different comparator and
Track 1/Track 2 numbers. The per-row result classification changes are:

| Row | SK-V10 close | SK-V11 W0 live | Note |
|---|---|---|---|
| `canada/parse_only` | `S / NO-GO` | `L / NO-GO` | contradicted pre-fold dispatch/W0 stated 17 parse `S / NO-GO` surface; now folded |
| `instruments/direct_to_struct` | `A / GO` | `N-direct / NO-GO` | W10 REDRESS-109 admission is not carried into live W0 telemetry; W0 labels row clamped non-admission |
| `numbers/direct_to_struct` | `A / GO` | `N-direct / NO-GO` | W2 direct row reclamation is not carried into live W0 telemetry; Track 2 is 59 Mbps below current 1.10x floor |

Current direct floor gaps, using `ceil(sonic-rs strict direct / 1.10)`, are:

| Row | Outcome | T1 | T2 | sonic | floor | T1 gap | T2 gap | Status |
|---|---|---:|---:|---:|---:|---:|---:|---|
| `twitter` | `N-direct / NO-GO` | 11613 | 10816 | 15113 | 13740 | -2127 | -2924 | residual |
| `citm_catalog` | `A / GO` | 18563 | 17787 | 15530 | 14119 | +4444 | +3668 | GO guard |
| `canada` | `N-direct / NO-GO` | 10316 | 9819 | 11700 | 10637 | -321 | -818 | residual |
| `apache_builds` | `A / GO` | 11254 | 10189 | 10995 | 9996 | +1258 | +193 | GO guard |
| `github_events` | `N-direct / NO-GO` | 11918 | 10596 | 14743 | 13403 | -1485 | -2807 | residual |
| `update_center` | `N-direct / NO-GO` | 8187 | 7474 | 11064 | 10059 | -1872 | -2585 | residual |
| `mesh` | `N-direct / NO-GO` | 8561 | 8652 | 9542 | 8675 | -114 | -23 | residual |
| `random` | `N-direct / NO-GO` | 7693 | 6949 | 8665 | 7878 | -185 | -929 | residual |
| `gsoc-2018` | `N-direct / NO-GO` | 2665 | 2578 | 4110 | 3737 | -1072 | -1159 | residual |
| `marine_ik` | `A / GO` | 8938 | 9437 | 8473 | 7703 | +1235 | +1734 | GO guard |
| `instruments` | `N-direct / NO-GO` | 11569 | 10736 | 9865 | 8969 | +2600 | +1767 | W0-clamped non-admission |
| `numbers` | `N-direct / NO-GO` | 4479 | 2366 | 2667 | 2425 | +2054 | -59 | W0-clamped non-admission |
| `unicode_mixed` | `N-direct / NO-GO` | 3753 | 2427 | 2846 | 2588 | +1165 | -161 | W0-clamped non-admission |
| `unicode_escapes` | `N-direct / NO-GO` | 1345 | 1341 | 3785 | 3441 | -2096 | -2100 | residual |
| `unicode_basic` | `A / GO` | 2299 | 2227 | 2353 | 2140 | +159 | +87 | GO guard |
| `distinct_values` | `N-direct / NO-GO` | 1750 | 1625 | 2923 | 2658 | -908 | -1033 | residual |
| `y_string_unicode` | `N-direct / NO-GO` | 1983 | 1029 | 4344 | 3950 | -1967 | -2921 | residual |

## Section 4 - Anomalies And Telemetry Flags

- Baseline commit distinction: the measured W0 capture ran at source commit
  `3ce75df4`; the documentation freeze commit is `9c8da194`. S-P1 should cite
  the capture commit for performance evidence and the freeze commit for doc
  provenance.
- Parse surface mismatch folded: pre-fold dispatch, handoff, synthesis, and W0
  said 17 parse `S / NO-GO`; live `skinny/RESULTS.md` renders 16 `S / NO-GO`
  plus `canada/parse_only` as `L / NO-GO`. `SYNTHESIS.md`, `HANDOFF.md`, and
  `research/w0/W0-open-baseline.md` have been updated to match.
- W10 close telemetry is absent from current manifest: manifest wave counts are
  `SK-V9-open` 39, `SK-V10-W2` 1, and `SK-V10-W6` 1. There is no
  `SK-V10-W10` or `REDRESS-109` row, even though SK-V10 close admitted
  `instruments/direct_to_struct` under REDRESS-109.
- W2 `numbers/direct_to_struct` telemetry is absent or stale in current W0:
  SK-V10 close carried it as `A / GO`, while current W0 renders it as
  `N-direct / NO-GO` with `Redress=none`.
- `Strictness=deferred` and `parse_utf8=view-boundary` appear on 39/41 rows.
  Strict measured-row telemetry appears only on `apache_builds/direct_to_struct`
  and `github_events/real_typed_struct`. Direct guard rows
  `citm_catalog`, `marine_ik`, and `unicode_basic` remain inherited/deferred.
- CostFacts is stale for all rows: every manifest row renders
  `none:pre-W1:none:pre-W1:none:pre-W1` instead of populated rule id, chosen
  shape, and rejected alternative ids.
- PMU/cycles telemetry is absent from this artifact and from the live
  `RESULTS.md` row surface. The manifest provides `ns_per_byte`, `track1_ns`,
  and `bytes`, which satisfies "equivalent sample cost" for P1-F extraction,
  but not cycles-per-byte. P1-D must supply real c/B if S-P1 needs it.
- The required `SK-V8-open delta` field is stale or absent by name. The current
  manifest column is `SK-V9-open delta`; the main table still renders
  `Delta vs SK-V6`, which is non-machine-readable for this pass.
- Sidecar freshness is mostly absent or stale. Current comparator evidence
  counts are: 99 `same-run-native`, 25 `historical:sk-v7-sidecar-profile`,
  77 `absent:not-collected-for-parse_only`,
  102 `absent:not-collected-for-direct_to_struct`, and
  42 `absent:not-collected-for-real_typed_struct`. Historical or absent
  sidecars remain planning signals only.
- Comparator id, plane, strictness, freshness, sidecar freshness, and source
  are present only inside the combined `Comparator evidence` manifest field,
  not as separate top-level columns. This can still be gate-consumed, but it is
  a structured-field packing dependency for S-P1 consumers.
- All 41 manifest rows are JSON domain rows. No CSS L4, Sheets, or BBNF-self
  grammar-domain telemetry exists in the W0 result surface.
- `Diagnostic nonproducer` is uniformly
  `structural_scan+masking_probes+pmu+cycles:nonproducer`; those signals are
  not behavior evidence and must not be used to admit direct or typed rows.

## Section 5 - Sources

- `skinny/RESULTS.md`, current W0 extraction, run id
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- `git show c16cc915:skinny/RESULTS.md`, SK-V10 close seed, run id
  `sk-v9-open:criterion-fnv64-6f007527061ee26d`.
- `restart/skinny/tranches/sk-v10/research/close/close-redress.md`.
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v11/HANDOFF.md`.
- `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md`.
- `skinny/REDRESS.md` through REDRESS 110.
- `restart/skinny/tranches/sk-v8/SPEC.md` Sections 0.3 and 0.4.
- `restart/skinny/tranches/sk-v10/research/p1/tools/extract_results_main_table.py`.
