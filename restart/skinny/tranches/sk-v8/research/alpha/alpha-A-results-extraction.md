# Alpha-A Results Extraction: SK-V7 -> SK-V8

Date: 2026-05-16.
Scope: SK-V7 close extraction for Pass Alpha. This file summarizes only
evidence directly present in `skinny/RESULTS.md`, `skinny/REDRESS.md`,
`restart/skinny/tranches/sk-v7/SPEC.md` sections 13-15, SK-V7 wave research
reports, and `git log`.

## Current close status

`skinny/RESULTS.md` is the current authority. It uses the schema-v3
PASS-ALPHA-shaped table surface with columns for corpus, workload, outcome,
verdict, strictness, `parse_utf8`, `escape_complete`, flaw-probe text, output
plane, Track 1/2 Mbps, comparator Mbps, delta columns, hot leaf, and signal.
The report's own notes state the overall outcome is `N-direct / NoGo`.

Main table count:

| Family | Count | Status |
|---|---:|---|
| `parse_only` | 17 | all `K / NO-GO` |
| `direct_to_struct` | 11 | `N-direct / NO-GO` blockers |
| `direct_to_struct` | 6 | `A / GO` |
| `real_typed_struct` | 4 | all `A / GO` |

All current main rows have `Strictness=deferred`, `parse_utf8=view-boundary`,
and `escape_complete=yes`. Parse rows use output plane `borrowed view over
offset tape vs DOM`; direct rows use `digest`; real typed rows use
`typed direct`. The table records sonic-rs strict Mbps for all main rows.
Sonic lossy is populated only as a flaw probe for parse rows. C++ sidecar
columns are populated only where documented; missing sidecar values remain
`n/a`.

Two schema caveats matter for SK-V8 goalsetting:

- Current `Outcome` values include `K` and `N-direct`, which are not in the
  PASS-ALPHA enum template `A / C / G / L`.
- `Delta vs SK-V6` is explicitly `n/a (no machine-readable SK-V6 baseline in
  W0b)` for every current main row. Any SK-V7-vs-SK-V6 numeric delta not
  recorded by a wave report is not derivable from current artifacts.

## Current blocker rows

Legend: `view-vs-DOM` abbreviates the current parse output plane
`borrowed view over offset tape vs DOM`. `T1/S`, `T1/simd`, and `T1/yyjson`
are the current percent deltas printed in `RESULTS.md`; `n/a` means the value
is absent in the current table.

| Corpus | Workload | Outcome | Plane | T1 Mbps | T2 Mbps | sonic strict | simdjson DOM | yyjson | serde | T1/S | T1/simd | T1/yyjson |
|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| twitter | parse_only | K/NO-GO | view-vs-DOM | 15752 | 12285 | 21020 | 24522 | 30931 | 5974 | -25.1% | -35.8% | -49.1% |
| citm_catalog | parse_only | K/NO-GO | view-vs-DOM | 31784 | 20817 | 25509 | 35822 | 20956 | 7541 | +24.6% | -11.3% | +51.7% |
| canada | parse_only | K/NO-GO | view-vs-DOM | 17765 | 17070 | 13885 | 11493 | 13003 | 5215 | +27.9% | +54.6% | +36.6% |
| apache_builds | parse_only | K/NO-GO | view-vs-DOM | 12482 | 12151 | 17381 | 36014 | 16275 | 6051 | -28.2% | -65.3% | -23.3% |
| github_events | parse_only | K/NO-GO | view-vs-DOM | 15198 | 13046 | 23034 | 39642 | 21426 | 7686 | -34.0% | -61.7% | -29.1% |
| update_center | parse_only | K/NO-GO | view-vs-DOM | 11193 | 9227 | 19684 | 30593 | 18540 | 4244 | -43.1% | -63.4% | -39.6% |
| mesh | parse_only | K/NO-GO | view-vs-DOM | 14265 | 13287 | 11754 | 9414 | n/a | 4890 | +21.4% | +51.5% | n/a |
| random | parse_only | K/NO-GO | view-vs-DOM | 9838 | 7804 | 15457 | 20638 | n/a | 3579 | -36.4% | -52.3% | n/a |
| gsoc-2018 | parse_only | K/NO-GO | view-vs-DOM | 23026 | 21881 | 49292 | n/a | n/a | 16349 | -53.3% | n/a | n/a |
| marine_ik | parse_only | K/NO-GO | view-vs-DOM | 13797 | 12384 | 10070 | n/a | n/a | 4044 | +37.0% | n/a | n/a |
| instruments | parse_only | K/NO-GO | view-vs-DOM | 18038 | 11678 | 16312 | n/a | n/a | 4426 | +10.6% | n/a | n/a |
| numbers | parse_only | K/NO-GO | view-vs-DOM | 20609 | 18514 | 13626 | n/a | n/a | 6330 | +51.2% | n/a | n/a |
| unicode_mixed | parse_only | K/NO-GO | view-vs-DOM | 8035 | 7698 | 16180 | 13150 | n/a | 3887 | -50.3% | -38.9% | n/a |
| unicode_escapes | parse_only | K/NO-GO | view-vs-DOM | 12042 | 11146 | 18415 | 5637 | n/a | 4810 | -34.6% | +113.6% | n/a |
| unicode_basic | parse_only | K/NO-GO | view-vs-DOM | 11416 | 10653 | 15596 | 16276 | n/a | 3336 | -26.8% | -29.9% | n/a |
| distinct_values | parse_only | K/NO-GO | view-vs-DOM | 6655 | 5633 | 17148 | 22825 | n/a | 3881 | -61.2% | -70.8% | n/a |
| y_string_unicode | parse_only | K/NO-GO | view-vs-DOM | 6216 | 6038 | 13537 | 13627 | n/a | 5704 | -54.1% | -54.4% | n/a |
| twitter | direct_to_struct | N-direct/NO-GO | digest | 11832 | 10986 | 14885 | n/a | n/a | 10465 | -20.5% | n/a | n/a |
| canada | direct_to_struct | N-direct/NO-GO | digest | 10773 | 10296 | 12421 | n/a | n/a | 7469 | -13.3% | n/a | n/a |
| github_events | direct_to_struct | N-direct/NO-GO | digest | 12270 | 11366 | 16041 | n/a | n/a | 12799 | -23.5% | n/a | n/a |
| update_center | direct_to_struct | N-direct/NO-GO | digest | 8401 | 7667 | 11081 | n/a | n/a | 8193 | -24.2% | n/a | n/a |
| random | direct_to_struct | N-direct/NO-GO | digest | 7727 | 7123 | 8936 | n/a | n/a | 6536 | -13.5% | n/a | n/a |
| gsoc-2018 | direct_to_struct | N-direct/NO-GO | digest | 15097 | 14306 | 23407 | n/a | n/a | 19567 | -35.5% | n/a | n/a |
| instruments | direct_to_struct | N-direct/NO-GO | digest | 11972 | 11086 | 12673 | n/a | n/a | 9350 | -5.5% | n/a | n/a |
| unicode_mixed | direct_to_struct | N-direct/NO-GO | digest | 4579 | 4431 | 9679 | n/a | n/a | 4956 | -52.7% | n/a | n/a |
| unicode_escapes | direct_to_struct | N-direct/NO-GO | digest | 4866 | 4973 | 14028 | n/a | n/a | 5168 | -65.3% | n/a | n/a |
| distinct_values | direct_to_struct | N-direct/NO-GO | digest | 6105 | 5362 | 11344 | n/a | n/a | 8221 | -46.2% | n/a | n/a |
| y_string_unicode | direct_to_struct | N-direct/NO-GO | digest | 5029 | 3766 | 9019 | n/a | n/a | 7604 | -44.2% | n/a | n/a |

## Current GO rows

| Corpus | Workload | Outcome | Plane | T1 Mbps | T2 Mbps | sonic strict | serde | T1/S |
|---|---|---|---|---:|---:|---:|---:|---:|
| twitter | real_typed_struct | A/GO | typed direct | 18513 | 16193 | 15486 | 16332 | +19.5% |
| citm_catalog | direct_to_struct | A/GO | digest | 21438 | 20280 | 19966 | 13065 | +7.4% |
| apache_builds | direct_to_struct | A/GO | digest | 11116 | 10187 | 11122 | 9886 | -0.1% |
| update_center | real_typed_struct | A/GO | typed direct | 11879 | 10451 | 12627 | 10602 | -5.9% |
| mesh | direct_to_struct | A/GO | digest | 8259 | 8483 | 8789 | 7165 | -6.0% |
| mesh | real_typed_struct | A/GO | typed direct | 9466 | 8089 | 8696 | 6769 | +8.9% |
| marine_ik | direct_to_struct | A/GO | digest | 8943 | 9151 | 8147 | 6966 | +9.8% |
| marine_ik | real_typed_struct | A/GO | typed direct | 12020 | 9630 | 8750 | 9269 | +37.4% |
| numbers | direct_to_struct | A/GO | digest | 12615 | 12362 | 12838 | 8081 | -1.7% |
| unicode_basic | direct_to_struct | A/GO | digest | 8576 | 8059 | 8502 | 5482 | +0.9% |

## SK-V7 wave evidence and deltas

Only directly recorded current rows or wave-report measurements are included
below. Candidate measurements from rejected waves are not current `RESULTS.md`
state.

| Wave | Commit evidence | REDRESS | Directly supported extraction |
|---|---|---:|---|
| W0 | `ed923615` | 77 | Admitted sonic-rs strict feature repair. Row-flip forecast missed: `instruments` stayed NO-GO and moved 92.0% -> 91.6% Track 1/S in the W0 evidence; `unicode_basic` stayed NO-GO and moved 91.7% -> 76.2%. No parse row reclassified. |
| W0b | `0d2fab3f` | 78 | Admitted schema-v3 telemetry and same-run sonic strict/lossy provenance. `RESULTS.md` was regenerated, but the measured authority remained `N-direct / NoGo`. W0b explicitly made `Delta vs SK-V6` non-derivable. |
| W1 | `89f29768` | 79 | Admitted descriptor-preserving `TapeKind` rename. Redress says `RESULTS.md` had no diff, so there is no throughput delta to extract. |
| W2 | `78d83497` | 80 | Rejected zero-fallback mantissa widen. Canada profiling found 111126 numbers, 111080 f64 candidates, zero mantissa overflows, zero ambiguous Eisel-Lemire returns, and zero `str::parse::<f64>()` fallbacks. Current canada direct remains `10773/10296/12421 Mbps`, `N-direct / NO-GO`. |
| W3 | `41ecf187` | 81 | Admitted capacity-hinted numeric Vec real-typed expansion. Current admitted rows are `mesh real_typed_struct` `9466/8089/8696 Mbps`, `A/GO`, and `marine_ik real_typed_struct` `12020/9630/8750 Mbps`, `A/GO`. W3 also records guard rows `mesh direct_to_struct` `8259/8483/8789 Mbps`, `A/GO`, and `twitter real_typed_struct` `18513/16193/15486 Mbps`, `A/GO`. |
| W4 | `17bd39b1` | 82 | Rejected single-quartet Unicode escape classifier. Rejected-candidate measurements did not clear thresholds: `unicode_escapes parse_only` reached 14516 Mbps, 82.1% of sonic; `unicode_escapes direct_to_struct` reached 5118 Mbps, 39.4% of sonic; `y_string_unicode parse_only` reached 6331 Mbps, 49.9% of sonic; `y_string_unicode direct_to_struct` reached 5093 Mbps, 64.0% of sonic, with Track 2 regressed 6.6%. Current rows remain NO-GO in `RESULTS.md`. |
| W5 | `db761873` | 83 | Rejected generated-retained StringBlock16 tiny probe. The focused rejected run regressed all six named parse rows by more than guard: twitter -36.0%, update_center -34.1%, unicode_basic -37.2%, random -43.8%, unicode_mixed -17.3%, distinct_values -8.2%. Current parse rows remain NO-GO. |
| W6 | `58479e29` | 84 | Rejected object-pair value-byte control compaction. Candidate failed citm Track 2 threshold and instruments parse/direct targets; current `instruments direct_to_struct` remains `11972/11086/12673 Mbps`, `N-direct / NO-GO`. |
| W7 | `f786e597` | 85 | Admitted Lock 14 Phase A+B neutralization. Redress says `RESULTS.md` had no diff; no throughput delta to extract. |
| W8 | `7c6837b8` | 86 | Admitted Lock 14 Phase C+D codegen shell neutralization. Generated JSON outputs and `RESULTS.md` had no diff; no throughput delta to extract. |
| W9 | `51d8c8be` | 87 | Admitted CostFacts substrate projection. `gate-json --with-cost-facts --advisory` emitted schema `sk-v7-costfacts-v1` with 15 CostFacts entries; generated outputs and `RESULTS.md` had no diff. |
| W10 | `db913136` | 88 | Rejected consumed AArch64 bitmap bodies plus B6 fold. PMULL prefix-XOR passed correctness/asm proof but parse measurement regressed hard rows before a `RESULTS.md` refresh could be admitted: instruments Track 1 -4.62%, instruments Track 2 -4.19%, numbers Track 1 -10.04%, unicode_escapes Track 1 -12.66%, unicode_escapes Track 2 -15.52%. |
| W10b | `0cd00886` | 89 | Rejected narrowed CTZ bulk consumer plus B6 fold. Six Track 1/2 rows dropped more than the 2% maintain invariant: canada parse T1 -3.11% and T2 -4.14%, citm parse T1 -7.36%, instruments parse T1 -3.96%, marine_ik parse T1 -5.68%, mesh parse T1 -8.07% and T2 -7.46%, numbers parse T1 -6.44%. |
| W10c | `56e66ef5` | 90 | Admitted B6 stack-canary Stage 1 only. W10c made zero production or `RESULTS.md` diff; PMULL prefix-XOR remains rejected by item 88 and CSSC CTZ/bulk consumer remains rejected by item 89. |

## Close reading for SK-V8 framing

SPEC sections 13-15 dispatch Pass Alpha after W10 and name the likely SK-V8
framing as the hard residual around twitter parse and the yyjson gap through
Lock 15 fusion-quality work, remaining Lock 14 residue, and remaining bbnf.asm
primitive body fills. The current evidence supports that framing with caveats:

- Twitter parse is still a current blocker: `15752 Mbps` Track 1 vs `21020`
  sonic strict, `24522` simdjson DOM, and `30931` yyjson.
- The parser family as a whole is still not closed: every `parse_only` row is
  `K / NO-GO`, including rows where Track 1 is faster than sonic strict.
- Direct-to-struct remains the formal close blocker: 11 direct rows are
  `N-direct / NO-GO`, and the current report's overall authority is
  `N-direct / NoGo`.
- W10c closes only B6 Stage 1. The remaining bitmap body fills are explicitly
  routed, not admitted, because W10 and W10b measured regressions.

## Missing telemetry caveats

- No cycles-per-byte or `c/B` rows are present in current `RESULTS.md`.
  `Masking Probes` record selected ns/iter and Mbps probes, but not normalized
  cycles-per-byte.
- No `memory`, `parse_full_traversal`, `path_lookup`, or
  `unicode_string_float` workload rows are present in the current main table.
  Peak RSS appears only in notes for selected corpus probes and is not a
  schema row.
- `Hot leaf` is not usable for attribution in the current report. Main rows
  say `unprofiled in W0b; no kernel prescription from this row`, so top-symbol
  and percent self-time are not derivable.
- `simdjson On Demand`, `asmjson SWAR`, and `asmjson AVX-512` are absent in
  current main rows. RapidJSON default is present only for selected parse rows.
- Direct and real-typed rows have no simdjson or yyjson comparator values in
  the current table. Only sonic strict and serde_json are populated for those
  product planes.
- SK-V7 per-wave Mbps deltas are not globally reconstructible. Use only
  current `RESULTS.md` rows or wave-report measurements above; do not infer
  missing deltas from old baselines or rejected patches.
