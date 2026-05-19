# SK-V10 P1-F: RESULTS Extraction And Delta

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-19.
Scope: extract the current `skinny/RESULTS.md` state that SK-V10 Alpha inherits
and classify the live frontier.
Output: this file.
Baseline: SK-V10 Alpha inherits W1-rendered `SK-V9-open`, run
`sk-v9-open:criterion-fnv64-a1e8a51ae806d386`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`.
Profile tool: RESULTS extraction, not samply.
Corpus coverage: 40/40 main rows.

## Section 1 - Method

Commands:

```bash
git status --short --branch
git log --oneline --max-count=12 -- skinny/RESULTS.md
python3 restart/skinny/tranches/sk-v10/research/p1/tools/extract_results_main_table.py \
  skinny/RESULTS.md /tmp/skv10-p1/results-main.csv
```

The extractor parsed the main RESULTS table only. The SK-V10 Alpha contract
does not claim a new `SK-V10-open` run id; it explicitly inherits the current
W1-rendered `SK-V9-open` authority because no surviving behavior wave landed
after W1/W2 and REDRESS 98 retired W3.

## Section 2 - Findings

Main row counts:

| Family | Count | Outcome |
|---|---:|---|
| `parse_only` | 17 | all `S / NO-GO`; diagnostic only |
| `direct_to_struct` | 17 | 3 `A / GO`, 14 `N-direct / NO-GO` |
| `real_typed_struct` | 6 | all `A / GO` |

Direct rows:

| Corpus | Outcome | Verdict | Plane | T1 Mbps | T2 Mbps | sonic Mbps | Delta |
|---|---|---|---|---:|---:|---:|---:|
| `twitter` | `N-direct` | `NO-GO` | digest | 11931 | 11064 | 15224 | -21.6% |
| `citm_catalog` | `A` | `GO` | digest | 21129 | 19898 | 19959 | +5.9% |
| `canada` | `N-direct` | `NO-GO` | digest | 10466 | 10326 | 12074 | -13.3% |
| `apache_builds` | `N-direct` | `NO-GO` | digest | 11157 | 10145 | 11021 | +1.2% |
| `github_events` | `N-direct` | `NO-GO` | digest | 11983 | 11091 | 15800 | -24.2% |
| `update_center` | `N-direct` | `NO-GO` | digest | 8356 | 7561 | 11176 | -25.2% |
| `mesh` | `N-direct` | `NO-GO` | digest | 8431 | 8769 | 9807 | -14.0% |
| `random` | `N-direct` | `NO-GO` | digest | 7685 | 6927 | 8507 | -9.7% |
| `gsoc-2018` | `N-direct` | `NO-GO` | digest | 14676 | 14126 | 23078 | -36.4% |
| `marine_ik` | `A` | `GO` | digest | 9205 | 9075 | 8332 | +10.5% |
| `instruments` | `N-direct` | `NO-GO` | digest | 11708 | 10803 | 12194 | -4.0% |
| `numbers` | `N-direct` | `NO-GO` | digest | 12182 | 11803 | 12966 | -6.0% |
| `unicode_mixed` | `N-direct` | `NO-GO` | digest | 4609 | 4562 | 10245 | -55.0% |
| `unicode_escapes` | `N-direct` | `NO-GO` | digest | 5131 | 5025 | 13779 | -62.8% |
| `unicode_basic` | `A` | `GO` | digest | 8973 | 8278 | 8625 | +4.0% |
| `distinct_values` | `N-direct` | `NO-GO` | digest | 6052 | 5241 | 11024 | -45.1% |
| `y_string_unicode` | `N-direct` | `NO-GO` | digest | 4887 | 3669 | 8829 | -44.6% |

Typed rows:

| Corpus | Outcome | Verdict | Plane | T1 Mbps | T2 Mbps | sonic typed Mbps | Delta |
|---|---|---|---|---:|---:|---:|---:|
| `twitter` | `A` | `GO` | typed direct | 18302 | 16512 | 15866 | +15.3% |
| `citm_catalog` | `A` | `GO` | typed direct | 35102 | 19143 | 22058 | +59.1% |
| `apache_builds` | `A` | `GO` | typed direct | 8174 | 6728 | 8110 | +0.8% |
| `update_center` | `A` | `GO` | typed direct | 11847 | 10297 | 12501 | -5.2% |
| `mesh` | `A` | `GO` | typed direct | 10032 | 7854 | 9270 | +8.2% |
| `marine_ik` | `A` | `GO` | typed direct | 10728 | 8454 | 8105 | +32.4% |

Parse rows are all `S / NO-GO` and are excluded from SOTA admission in
SK-V10. Their current Track 1 deltas against sonic strict range from +34.5%
(`numbers`) to -53.6% (`gsoc-2018`), but SK-V10 treats this as diagnostic
profile evidence only.

## Section 3 - Delta vs SK-V9

There is no SK-V10 behavior delta yet. The live delta is contractual:

- W3 is retired by REDRESS 98; no `G-W3-UNION-SUBSTRATE` retry remains.
- Direct digest becomes the primary JSON frontier: 14 `N-direct / NO-GO` rows.
- Typed product rows remain the current SOTA-beat surface: six `A / GO` rows,
  five of six faster than the same-run sonic typed comparator and one
  (`update_center`) below sonic but inside the current 1.10 time-slack gate.
- Parse-only is retained only for diagnostic profiling and cannot close SK-V10.

## Section 4 - Anomalies + Masking Signals

- `apache_builds` and `numbers` direct rows have positive Track 1 deltas
  against sonic but remain `N-direct / NO-GO` because W0 clamped fresh direct
  guard passes as non-behavior evidence.
- `unicode_mixed`, `unicode_escapes`, `distinct_values`, and
  `y_string_unicode` are the largest direct losses and must not be paper-closed
  by a generic string/kernel promise without micro-proof and same-row gates.
- `update_center` is the lone typed row below sonic (`-5.2%`) and should be a
  maintain/diagnostic row for typed-plane S-P2.

## Section 5 - Sources

- `skinny/RESULTS.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `skinny/REDRESS.md` entries 94-98
