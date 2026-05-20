# SK-V12 P1-B: Direct And Real-Typed Product-Plane Profile

Pass: S-P1 Profile. Cycle: V12 pin re-bracket.
Date: 2026-05-20.
Scope: product-plane profiling for JSON `direct_to_struct` Track 1/Track 2
and JSON `real_typed_struct` Track 1/Track 2 guard rows at current HEAD.
Output: this file.
Baseline: SK-V12 pin-aware G-Alpha current HEAD `cf7848b2`.
Host triple: `aarch64-apple-darwin`; Apple Silicon only per USER PIN.
Build flags: release profile, `RUSTFLAGS=-C target-cpu=native`, profile target
`/tmp/skv12-pin-profile-target-cf7848b2`.
Profile tool: fresh `profile_direct` PMU replay under `/tmp/skv12-pin-p1/pmu`;
samply and xctrace Time Profiler artifacts under `/tmp/skv12-pin-p1`.
Corpus coverage: JSON `direct_to_struct` PMU 17/17 corpora x Track 1/Track 2;
JSON `real_typed_struct` PMU 7/7 guard corpora x Track 1/Track 2. Product
samply/xctrace hot-leaf coverage is 48/48 rows.

## Final Orchestrator Fold - 2026-05-20

This fold records the final pin-era product profile root,
`/tmp/skv12-pin-p1`:

| Artifact | Coverage | Authority |
|---|---:|---|
| PMU direct rows | 34/34 PASS | `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv` |
| PMU typed rows | 14/14 PASS | `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv` |
| samply product captures | 48/48 PASS | `/tmp/skv12-pin-p1/samply/capture_status.tsv` |
| xctrace product-v2 Time Profiler | 48/48 PASS | `/tmp/skv12-pin-p1/xctrace/capture_status.tsv` |
| product-v2 XML exports | 48/48 present; export status TSV records `SKIP` for already-existing XML | `/tmp/skv12-pin-p1/time_profile_export_status.tsv` |
| derived product hot leaves | 48 summary rows, 240 detail rows | `/tmp/skv12-pin-p1/time_profile_hot_leaf_{summary,details}.tsv` |

Product hot-leaf authority is the `plane=direct` and `plane=typed` subsets of
`/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv`, plus the readable
tables `/tmp/skv12-pin-p1/time_profile_direct_table.md` and
`/tmp/skv12-pin-p1/time_profile_typed_table.md`. Leading final families are
split by mode so oracle work is not folded into generated Track 1 antecedents:

| Plane/mode | Leading families |
|---|---|
| `direct/track1` | `output_digest_hash` 17 |
| `direct/track2` | `runtime_support` 14; `string_escape_decode` 2; `allocation_support` 1 |
| `typed/real_typed_track1` | `typed_direct_projection` 6; `string_full_scan` 1 |
| `typed/real_typed_track2` | `serde_json_oracle_read_parse` 7 |

Track 2/oracle-only families are guard and comparator context; they are not
generated Track 1 optimization antecedents.

JSON product rows are guard and diagnostic evidence under the user pin. They
do not satisfy the CSS L4 `> lightningcss_mbps + 1` admission target.

Shared capture provenance:

- Capture root: `/tmp/skv12-pin-p1`.
- Product PMU authority:
  `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv`.
- Capture status:
  `/tmp/skv12-pin-p1/pmu/capture_status.tsv`.
- Replay command ledger:
  `/tmp/skv12-pin-p1/pmu/pmu-commands.sh`.
- Completion stamp: `/tmp/skv12-pin-p1/pmu/done.txt` records
  `done 2026-05-20T18:05:34Z`.
- Fresh PMU row status: `pmu-direct` 34/34 PASS, `pmu-typed` 14/14 PASS,
  and `pmu-parse` 34/34 PASS. P1-B consumes only the direct and typed product
  rows.

## Section 1 - Method

This P1-B agent did not run cargo, samply, xctrace, or `profile_direct`.
The parent-owned replay completed before this artifact was patched. The
verbatim command surface is in `/tmp/skv12-pin-p1/pmu/pmu-commands.sh`; its
product command shape is:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
/tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct \
  <iters> <corpus-or-update-center-alias> <track1-or-track2>

cd /Users/mkbabb/Programming/bbnf-lang/skinny
/tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct \
  <iters> <corpus-or-update-center-alias> \
  <real_typed_track1-or-real_typed_track2>
```

Verification commands used by this artifact, without launching a profiler:

```bash
awk -F '\t' 'NR>1{count[$2]++; status[$2":"$7]++}
  END{for(k in count) print k, count[k]; for(k in status) print k, status[k]}' \
  /tmp/skv12-pin-p1/pmu/capture_status.tsv

sed -n '1,120p' /tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv

find /tmp/skv12-pin-p1 -maxdepth 4 -type f \
  -name '*samply*' -o -name '*.trace' -o -name '*.time-profile.xml'
```

The PMU rows are hot-loop `profile_direct` rows, not Criterion admissions and
not samply/xctrace self-time evidence. They may be cited for product-plane PMU
truth, cycles-per-byte, and Track 1/Track 2 guard interpretation. They may not
be used to name a source hot leaf by percent self-time.

## Section 2 - Findings

Notation:

- `T1` is generated Track 1. `T2` is the independent Track 2 or oracle.
- Mbps and cycles-per-byte are from
  `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv`.
- Tables round Mbps to the nearest whole number and c/B to two decimals; the
  TSV is the precision authority.
- JSON product rows are guard and diagnostic evidence under the USER PIN. They
  do not satisfy the CSS L4 `> lightningcss_mbps + 1` admission target.

### Product PMU Summary - Direct

| Corpus | SK-V12 role | PMU Mbps T1/T2 | PMU c/B T1/T2 |
|---|---|---:|---:|
| `twitter` | direct residual guard/diagnostic | 8269 / 8362 | 2.98 / 3.22 |
| `citm_catalog` | direct guard | 15061 / 15335 | 1.63 / 1.72 |
| `canada` | direct residual guard/diagnostic | 8146 / 7084 | 3.26 / 3.39 |
| `apache_builds` | direct guard | 9396 / 8179 | 3.08 / 3.35 |
| `github_events` | direct residual guard/diagnostic | 9984 / 9428 | 2.83 / 3.08 |
| `update_center` | direct residual guard/diagnostic | 5721 / 5391 | 4.17 / 4.73 |
| `mesh` | direct residual guard/diagnostic | 6945 / 6341 | 3.99 / 3.94 |
| `random` | direct residual guard/diagnostic | 5457 / 4681 | 4.50 / 4.93 |
| `gsoc-2018` | direct residual guard/diagnostic | 12256 / 11506 | 2.35 / 2.45 |
| `marine_ik` | direct guard | 6966 / 7467 | 3.81 / 3.60 |
| `instruments` | W0-clamped guard/diagnostic | 9602 / 9146 | 2.88 / 3.11 |
| `numbers` | W0-clamped guard/diagnostic | 4984 / 6026 | 3.65 / 3.18 |
| `unicode_mixed` | W0-clamped guard/diagnostic | 3349 / 3226 | 7.90 / 8.10 |
| `unicode_escapes` | direct residual guard/diagnostic | 3683 / 3429 | 7.13 / 7.29 |
| `unicode_basic` | direct guard | 6799 / 6262 | 3.83 / 4.23 |
| `distinct_values` | direct residual guard/diagnostic | 4700 / 4516 | 5.48 / 6.21 |
| `y_string_unicode` | direct residual guard/diagnostic | 2538 / 2064 | 10.01 / 11.37 |

Direct synthesis:

- Fresh direct PMU coverage is complete: 17/17 corpora x Track 1/Track 2.
- The highest direct c/B rows are still the escaped/unicode string surface:
  `y_string_unicode`, `unicode_mixed`, `unicode_escapes`, and
  `distinct_values`.
- The row roles remain inherited from `skinny/RESULTS.md` and
  `skinny/REDRESS.md` through REDRESS 119/120. Fresh PMU replay does not admit
  a direct row, does not reopen SK-V11 direct residuals by itself, and does not
  demote a JSON guard without same-wave gate disposition.

### Product PMU Summary - Real Typed

| Corpus | SK-V12 role | PMU Mbps T1/T2 | PMU c/B T1/T2 |
|---|---|---:|---:|
| `twitter` | typed guard | 16722 / 14959 | 1.85 / 2.13 |
| `citm_catalog` | typed guard | 23275 / 15162 | 0.98 / 1.82 |
| `apache_builds` | typed guard | 6652 / 4424 | 4.07 / 6.07 |
| `github_events` | typed guard | 10326 / 9016 | 2.81 / 3.00 |
| `update_center` | typed guard | 10936 / 8652 | 2.87 / 3.54 |
| `mesh` | typed guard | 7645 / 6540 | 3.78 / 4.71 |
| `marine_ik` | typed guard | 7717 / 8151 | 2.89 / 3.40 |

Typed synthesis:

- Fresh typed PMU coverage is complete for the seven JSON
  `real_typed_struct` guard rows x Track 1/Track 2.
- Typed rows are product-plane guard rows. They cannot admit a JSON direct row,
  cannot substitute for a CSS L4 generated baseline, and cannot satisfy the
  lightningcss admission bar.
- Track 2 is an independence/oracle surface for guard interpretation. It is
  not a same-output-plane CSS comparator.

### Hot-Leaf Attribution

Available for JSON product-plane diagnostics and guard interpretation.

The final pin root contains complete product samply and xctrace captures, 48
product-v2 Time Profiler traces, 48 product XML exports under
`/tmp/skv12-pin-p1/direct-xctrace/exports-v2`, and the derived summary/detail
tables. The direct/typed product subset contributes 48 summary rows and 240
detail rows with source file:line anchors. P1-E owns cross-plane hot-leaf
synthesis; P1-B records the product-plane split and preserves the Track 1 /
Track 2 boundary.

## Section 3 - Delta vs SK-V11

P1-F owns the full row-delta ledger. P1-B records only the product-plane
profile implications:

| Surface | SK-V11 close | SK-V12 pin PMU replay at `cf7848b2` |
|---|---|---|
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO`; direct residuals closed by REDRESS 119 | 17/17 direct corpora profiled x T1/T2 with fresh PMU and xctrace-derived product hot leaves. No row admission. |
| `real_typed_struct` | 7 `A / GO` typed guard rows | 7/7 typed guard corpora profiled x T1/T2 with fresh PMU. No demotion evidence in this artifact. |
| CSS L4 generated parser | no admitted generated CSS L4 row | no CSS L4 product row, no lightningcss comparator, no strict equality oracle, and no CSS output-plane evidence in P1-B. |

The profile confirms the pin-aware boundary: JSON product rows are guard and
diagnostic evidence; the SK-V12 admission target is a generated CSS L4 row
that beats `lightningcss_mbps + 1` on the same corpus and same output plane.

## Section 4 - Anomalies And Masking Signals

- Fresh product PMU replay exists and is complete for P1-B's JSON direct and
  typed guard surfaces.
- Fresh accepted product samply/xctrace Time Profiler attribution evidence is
  present for JSON direct and typed guard surfaces. CSS L4 remains absent.
- `profile_direct` is a hot-loop profiler. The PMU rows can differ materially
  from Criterion Mbps in `skinny/RESULTS.md`; `skinny/RESULTS.md` remains the
  row-admission authority.
- No observation here reopens JSON direct residual work before the CSS L4
  target. REDRESS 119 and 120 remain the JSON direct/fixpoint authority unless
  a later wave names a material differential with fresh profile, micro-proof,
  same-wave consumer, and gate-consumed measurement.
- No observation here satisfies USER PIN D1/D2. CSS L4 remains authoritative;
  Sheets/BBNF-self stay fallback-only after a measured CSS redress attempt.
- No SIMD, union-substrate, or ASM-gen route is scoped from this file. PMU row
  shape alone is insufficient under micro-prove-first and Lock 16.

## Section 5 - Sources

- `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv`
- `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv`
- `/tmp/skv12-pin-p1/pmu/capture_status.tsv`
- `/tmp/skv12-pin-p1/pmu/pmu-commands.sh`
- `/tmp/skv12-pin-p1/pmu/done.txt`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv`
- `/tmp/skv12-pin-p1/logs/pmu-direct-*.rerun.log.out`
- `/tmp/skv12-pin-p1/logs/pmu-direct-*.rerun.log.err`
- `/tmp/skv12-pin-p1/logs/pmu-typed-*.rerun.log.out`
- `/tmp/skv12-pin-p1/logs/pmu-typed-*.rerun.log.err`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
