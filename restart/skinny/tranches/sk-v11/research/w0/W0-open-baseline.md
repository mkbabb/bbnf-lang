# SK-V11 W0 Open Baseline

Date: 2026-05-19.

Status: closed. This W0 artefact freezes the fresh SK-V11-open JSON baseline
used by S-P1. It supersedes the SK-V10-close seed numbers in the Alpha-F
draft where they differ.

## Capture

- Commit: `3ce75df4`.
- Criterion root: `/tmp/skv11-open-criterion-3ce75df`.
- Target root: `/tmp/skv11-open-target-3ce75df`.
- Run id: `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- Command:
  `CARGO_TARGET_DIR=/tmp/skv11-open-target-3ce75df CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory`.
- Verification:
  `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results`.

`gate-json` returned zero. The cost-facts producer diagnostics are the existing
advisory missing-measurement diagnostics and do not invalidate the JSON result
table.

## Result Surface

| Family | SK-V11-open state | S-P1 role |
|---|---|---|
| `parse_only` | 17 `S / NO-GO` | diagnostic-only profile coverage |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | primary closure surface |
| `real_typed_struct` | 7 `A / GO` | guard surface |

Overall outcome remains `N-direct / NoGo`.

## Direct Residual Rows

Floor is `ceil(sonic-rs direct / 1.10)`.

| Row | Track 1 | Track 2 | sonic direct | Floor | Track 1 gap | Track 2 gap | Note |
|---|---:|---:|---:|---:|---:|---:|---|
| `twitter` | 11613 | 10816 | 15113 | 13740 | 2127 | 2924 | residual |
| `canada` | 10316 | 9819 | 11700 | 10637 | 321 | 818 | residual |
| `github_events` | 11918 | 10596 | 14743 | 13403 | 1485 | 2807 | residual |
| `update_center` | 8187 | 7474 | 11064 | 10059 | 1872 | 2585 | residual |
| `mesh` | 8561 | 8652 | 9542 | 8675 | 114 | 23 | near-floor residual |
| `random` | 7693 | 6949 | 8665 | 7878 | 185 | 929 | near-floor residual |
| `gsoc-2018` | 2665 | 2578 | 4110 | 3737 | 1072 | 1159 | residual after fresh comparator reset |
| `instruments` | 11569 | 10736 | 9865 | 8969 | -2600 | -1767 | W0-clamped non-admission |
| `numbers` | 4479 | 2366 | 2667 | 2425 | -2054 | 59 | W0-clamped non-admission; Track 2 still short |
| `unicode_mixed` | 3753 | 2427 | 2846 | 2588 | -1165 | 161 | W0-clamped non-admission; Track 2 still short |
| `unicode_escapes` | 1345 | 1341 | 3785 | 3441 | 2096 | 2100 | residual |
| `distinct_values` | 1750 | 1625 | 2923 | 2658 | 908 | 1033 | residual |
| `y_string_unicode` | 1983 | 1029 | 4344 | 3950 | 1967 | 2921 | residual |

`instruments`, `numbers`, and `unicode_mixed` are intentionally still
`N-direct / NO-GO`: W0 captures do not admit behavior rows. S-P3 must decide
whether those rows need a small measured admission wave, a maintain gate, or a
demotion proof.

## Guard Rows

Direct guard rows:

| Row | Track 1 | Track 2 | sonic direct |
|---|---:|---:|---:|
| `citm_catalog` | 18563 | 17787 | 15530 |
| `apache_builds` | 11254 | 10189 | 10995 |
| `marine_ik` | 8938 | 9437 | 8473 |
| `unicode_basic` | 2299 | 2227 | 2353 |

Typed guard rows:

| Row | Track 1 | Track 2 | sonic typed |
|---|---:|---:|---:|
| `twitter` | 17740 | 15912 | 15010 |
| `citm_catalog` | 30539 | 17675 | 20726 |
| `apache_builds` | 8478 | 6892 | 8106 |
| `github_events` | 11871 | 12275 | 12224 |
| `update_center` | 11851 | 10358 | 12467 |
| `mesh` | 9403 | 7897 | 8923 |
| `marine_ik` | 11788 | 10096 | 9010 |

## Dispatch Consequence

S-P1 profiles all 17 corpora. It must isolate the 13 direct residual rows,
the four direct guard rows, and the seven typed guard rows. Parse-only remains
diagnostic and cannot become a SOTA close target.
