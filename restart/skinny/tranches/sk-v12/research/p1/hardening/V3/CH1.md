# SK-V12 S-P1 Hardening V3 - CH1 Correctness

Verdict: REVISE.

Lens: CH1 correctness after commit `ffe5553d6b38c629e7213fd3b67e2beb9785181c`.
Scope: `PASS-1-PROFILE.md`, the six S-P1 artifacts, the capture manifest,
`skv12-p1-replay.tsv`, V1/V2 hardening and fold ledgers, `skinny/RESULTS.md`,
`skinny/REDRESS.md`, and retained `/tmp/skv12-p1` artifacts.

## Findings

1. BLOCKING - The V2 line-zero fold is incomplete in the cited self-time
   tables.

   V2 CH1 rejected the packet because the derived Time Profiler tables still
   contained line-zero source anchors. The V2 fold ledger claims the regenerated
   summary and details tables have no `:0` or unresolved line-zero markers
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V2/FOLD-REVISIONS.md:11`,
   `restart/skinny/tranches/sk-v12/research/p1/hardening/V2/FOLD-REVISIONS.md:24`).
   The dedicated source columns are now concrete: summary column 16 has 82/82
   concrete nonzero source paths and details column 9 has 410/410 concrete
   nonzero source paths.

   The cited leaf-symbol columns still contain `:0`, however. The summary table
   has 6/82 rows with `:0` in `top_leaf`; examples include
   `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv:45`,
   `:51`, `:55`, `:67`, `:69`, and `:71`. The details table has 12/410 rows
   with `:0` in `symbol`; examples include
   `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv:217`,
   `:247`, `:267`, `:337`, and `:347`. These are not harmless side data:
   P1-A, P1-B, and P1-E cite those TSVs as the exact top-leaf symbol,
   percent-self-time, and file:line authority
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:134`,
   `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:216`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:223`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:252`).
   Under the requested V3 correctness lens, "no `:0` ... in cited self-time
   tables" is not yet true.

2. PASS - The replay ledger is internally valid enough for CH1 correctness.

   `skv12-p1-replay.tsv` has 506 data rows, 14 fields per row, no duplicate
   lane/family/plane/corpus/mode keys, and its lane counts match the V2 fold:
   82 PMU, 82 samply, 82 xctrace CPU counters, 82 primary Time Profiler, 34
   parse Time Profiler exports, 48 primary product exports, 48 product-v2
   records, and 48 product-v2 exports. Every referenced output artifact and
   status artifact exists in `/tmp/skv12-p1`. The ledger also carries the
   `update_center` / `update-center` launch alias split, matching the manifest's
   alias boundary
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:41`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:62`).

3. PASS - PMU aggregate arithmetic is consistent.

   Recomputing weighted aggregates from `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`
   and `/tmp/skv12-p1/pmu/product_pmu_rows.tsv` yields parse 34 rows at
   `12274.872 Mbps`, `2.920217 c/B`, `0.204887 CPI`; direct 34 rows at
   `8278.039 Mbps`, `4.290305 c/B`, `0.183717 CPI`; and typed 14 rows at
   `11338.859 Mbps`, `3.123172 c/B`, `0.185056 CPI`. These match P1-D, P1-E,
   the manifest, and the V2 fold ledger
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:99`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:112`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:178`,
   `restart/skinny/tranches/sk-v12/research/p1/hardening/V2/FOLD-REVISIONS.md:72`).

4. PASS - Mode III is correctly bounded as absent call-stack authority.

   P1-C states 0/17 fresh Mode III samply call-stack rows and no fresh
   structural-scan capture under `/tmp/skv12-p1`
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:15`,
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:54`).
   Its absence table keeps eager decode, alternate scalar, cold-first-parse, and
   fresh structural scan at 0/17
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:145`).
   The manifest repeats that no S-P2/S-P3 wave may use Mode III symbols as
   fresh SK-V12 hot-leaf authority without a later capture
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:192`).

5. PASS - No unsupported row movement is present.

   `skinny/RESULTS.md` still has 41 main rows: 17 `parse_only`, 17
   `direct_to_struct`, and 7 `real_typed_struct`; outcomes remain 16
   `parse_only S / NO-GO`, 1 `parse_only L / NO-GO`, 4
   `direct_to_struct A / GO`, 13 `direct_to_struct N-direct / NO-GO`, and 7
   `real_typed_struct A / GO`. P1-F records zero delta from SK-V11 close and
   keeps the overall `N-direct / NoGo` surface
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:77`,
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:91`).
   REDRESS 120 remains explicit that SK-V11 closed as a measured fixpoint with
   no direct row or W0-clamped row admitted
   (`skinny/REDRESS.md:3531`, `skinny/REDRESS.md:3542`).

## Evidence Commands

```bash
awk -F '\t' 'NR>1 {n++; if ($16 ~ /:0$/) z++; if ($16 !~ /:[1-9][0-9]*$/) bad++}
  END {print n, z+0, bad+0}' /tmp/skv12-p1/time_profile_hot_leaf_summary.tsv
# 82 0 0

awk -F '\t' 'NR>1 {n++; if ($9 ~ /:0$/) z++; if ($9 !~ /:[1-9][0-9]*$/) bad++}
  END {print n, z+0, bad+0}' /tmp/skv12-p1/time_profile_hot_leaf_details.tsv
# 410 0 0

awk -F '\t' 'NR>1 {for (i=1;i<=NF;i++) if ($i ~ /:0([^0-9]|$)/) {bad++; break}}
  END {print bad+0}' /tmp/skv12-p1/time_profile_hot_leaf_summary.tsv
# 6

awk -F '\t' 'NR>1 {for (i=1;i<=NF;i++) if ($i ~ /:0([^0-9]|$)/) {bad++; break}}
  END {print bad+0}' /tmp/skv12-p1/time_profile_hot_leaf_details.tsv
# 12

awk -F '\t' 'NR==1 {next} {rows++; lane[$1]++; if (NF!=14) bad_nf++}
  END {print rows, bad_nf+0; for (l in lane) print l, lane[l]}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv

awk -F '|' '/^\| [^|]+ \| (parse_only|direct_to_struct|real_typed_struct) \|/{
  gsub(/^ +| +$/, "", $3); gsub(/^ +| +$/, "", $4); gsub(/^ +| +$/, "", $5);
  fam[$3]++; out[$3":"$4":"$5]++; total++
} END {print total; for (k in fam) print k, fam[k]; for (k in out) print k, out[k]}' \
  skinny/RESULTS.md
```

## Minimal Required Fold

Regenerate or post-process `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv` and
`/tmp/skv12-p1/time_profile_hot_leaf_details.tsv` so the cited `top_leaf` /
`symbol` fields no longer contain line-zero pseudo-symbols such as
`direct_struct::direct_struct.rs:0`, `lib.rs:0`, or `serde_json::lib.rs:0`.
Either replace them with resolved function/symbol names paired with the existing
concrete source columns, or mark them unresolved and update P1-A/P1-B/P1-E plus
the manifest so they no longer claim those rows satisfy the exact symbol-path
contract. No behavior source, `RESULTS`, or `REDRESS` edit is required.
