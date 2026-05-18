# Alpha B Competitor Deltas - SK-V8 to SK-V9

Date: 2026-05-18.

Scope: PASS-ALPHA alpha-B extraction for SK-V9 planning. Sources read:
`restart/prompts/pass-contracts/PASS-ALPHA.md`, final
`skinny/RESULTS.md`, `restart/skinny/tranches/sk-v8/SPEC.md`,
`restart/skinny/tranches/sk-v8/HANDOFF.md`, and
`restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md`.
This artifact records competitor deltas only; it does not dispatch SK-V9 waves.

Delta formula: `(bbnf Track 1 Mbps / comparator Mbps - 1) * 100`. Positive
means bbnf Track 1 is faster. Negative means the comparator is faster. For
`sonic-rs strict`, `simdjson DOM`, and `yyjson default`, the already-rendered
delta cells from final `skinny/RESULTS.md` are preserved. For `sonic-rs lossy`,
`RapidJSON default`, and `serde_json`, deltas are computed from the displayed
integer Mbps cells because final RESULTS does not render those delta columns.
Every `n/a` means no value is present in final SK-V8 RESULTS.

## Evidence Rules

- Final `skinny/RESULTS.md` is the row-value authority. SK-V8 W6 states that
  W6 made no RESULTS change, so the W0-rendered 38-row `SK-V8-open` table is
  still the close-state authority.
- Current bbnf rows all report `Strictness=deferred`,
  `parse_utf8=view-boundary`, and `escape_complete=yes`.
- `sonic-rs strict` and `serde_json` are same-run native Rust comparators.
  They are the only complete 38-row comparator columns.
- `sonic-rs lossy` is same-run but permissive and is a flaw probe only. It is
  populated only on the 17 `parse_only` rows.
- C++ comparators (`simdjson DOM`, `yyjson default`, `RapidJSON default`) are
  historical sidecar planning signals when populated. They are absent on all
  direct and real-typed rows and on several parse rows.
- `simdjson On Demand`, `asmjson SWAR`, and `asmjson AVX-512` have no Mbps
  values in final SK-V8 RESULTS.
- Strict admission remains strict-vs-strict, same-run, matching output plane,
  with measured validation inside the row. Deferred bbnf strictness, lossy
  comparators, sidecar values, historical freshness, and output-plane mismatch
  are planning evidence only.

## Strictness And Output-Plane Registry

| Comparator | Strictness plane in final RESULTS | Output plane in final RESULTS | Evidence status for SK-V9 planning |
|---|---|---|---|
| bbnf Track 1 | deferred; `parse_utf8=view-boundary`; `escape_complete=yes` | per row: borrowed view over offset tape vs DOM, digest, or typed direct | Baseline row; not strict-admission proof by itself. |
| sonic-rs strict | strict; same-run native | DOM on `parse_only`; digest on `direct_to_struct`; typed direct on `real_typed_struct` | Complete 38-row strict anchor candidate, but admission still requires bbnf row strictness and plane match. |
| sonic-rs lossy | permissive; same-run native | DOM on populated `parse_only` rows | Flaw probe only; never a strict anchor. |
| simdjson DOM | strict in RESULT capsules | DOM | Historical SK-V7 C++ sidecar where populated; planning only until refreshed as same-run matching-plane telemetry. |
| simdjson On Demand | strict in absence capsules | DOM / iterator-style comparator family | No Mbps values; absent on all rows. |
| yyjson default | strict in RESULT capsules | DOM | Historical SK-V7 C++ sidecar where populated; planning only until refreshed as same-run matching-plane telemetry. |
| yyjson minify | not present in final RESULTS | not present in final RESULTS | No SK-V8 close-state values. |
| asmjson SWAR | not measured; PASS-ALPHA treats SWAR as permissive/flaw-probe unless re-proven | DOM-class comparator family | No Mbps values; absent on all rows. |
| asmjson AVX-512 | not measured in final SK-V8 RESULTS | DOM-class comparator family | No Mbps values; absent on all rows; x86-only future telemetry would need matched strictness and plane. |
| RapidJSON default | RESULT capsules label populated rows strict, but PASS-ALPHA treats default RapidJSON cautiously as flaw-probe/permissive until re-proven | DOM | Historical SK-V7 C++ sidecar where populated; planning/flaw-probe signal only unless rerun under same-run strictness rules. |
| serde_json | strict; same-run native | DOM on `parse_only`; digest on `direct_to_struct`; typed direct on `real_typed_struct` | Complete 38-row strict reference baseline; admission still requires bbnf row strictness and plane match. |

## Coverage Summary

| Comparator | Populated rows | Missing rows |
|---|---:|---|
| sonic-rs strict | 38 / 38 | none |
| sonic-rs lossy | 17 / 38 | all `direct_to_struct` and `real_typed_struct` rows |
| simdjson DOM | 13 / 38 | all direct/typed rows; parse rows `gsoc-2018`, `marine_ik`, `instruments`, and `numbers` |
| simdjson On Demand | 0 / 38 | all rows |
| yyjson default | 6 / 38 | all direct/typed rows; all parse rows except `twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`, and `update_center` |
| asmjson SWAR | 0 / 38 | all rows |
| asmjson AVX-512 | 0 / 38 | all rows |
| RapidJSON default | 6 / 38 | all direct/typed rows; all parse rows except `twitter`, `citm_catalog`, `canada`, `apache_builds`, `random`, and `instruments` |
| serde_json | 38 / 38 | none |

## Delta Matrix

Cells are `Mbps (delta vs comparator)`.

| Corpus | Workload | Output plane | T1 | sonic strict | sonic lossy | simdjson DOM | yyjson default | RapidJSON default | serde_json |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|
| twitter | parse_only | borrowed view over offset tape vs DOM | 9581 | 18176 (-47.3%) | 14471 (-33.8%) | 24522 (-60.9%) | 30931 (-69.0%) | 4020 (+138.3%) | 3829 (+150.2%) |
| twitter | direct_to_struct | digest | 11859 | 12890 (-8.0%) | n/a | n/a | n/a | n/a | 6673 (+77.7%) |
| twitter | real_typed_struct | typed direct | 15333 | 13646 (+12.4%) | n/a | n/a | n/a | n/a | 15046 (+1.9%) |
| citm_catalog | parse_only | borrowed view over offset tape vs DOM | 28644 | 21717 (+31.9%) | 24830 (+15.4%) | 35822 (-20.0%) | 20956 (+36.7%) | 6760 (+323.7%) | 7401 (+287.0%) |
| citm_catalog | direct_to_struct | digest | 21151 | 18241 (+16.0%) | n/a | n/a | n/a | n/a | 12992 (+62.8%) |
| canada | parse_only | borrowed view over offset tape vs DOM | 15497 | 8729 (+77.5%) | 11316 (+36.9%) | 11493 (+34.8%) | 13003 (+19.2%) | 5187 (+198.8%) | 4050 (+282.6%) |
| canada | direct_to_struct | digest | 6586 | 12430 (-47.0%) | n/a | n/a | n/a | n/a | 7080 (-7.0%) |
| apache_builds | parse_only | borrowed view over offset tape vs DOM | 12694 | 16904 (-24.9%) | 17313 (-26.7%) | 36014 (-64.8%) | 16275 (-22.0%) | 3945 (+221.8%) | 4278 (+196.7%) |
| apache_builds | direct_to_struct | digest | 8306 | 8852 (-6.2%) | n/a | n/a | n/a | n/a | 6750 (+23.1%) |
| github_events | parse_only | borrowed view over offset tape vs DOM | 10689 | 16408 (-34.9%) | 16585 (-35.6%) | 39642 (-73.0%) | 21426 (-50.1%) | n/a | 4675 (+128.6%) |
| github_events | direct_to_struct | digest | 9088 | 9818 (-7.4%) | n/a | n/a | n/a | n/a | 8152 (+11.5%) |
| update_center | parse_only | borrowed view over offset tape vs DOM | 11926 | 18769 (-36.5%) | 19643 (-39.3%) | 30593 (-61.0%) | 18540 (-35.7%) | n/a | 4131 (+188.7%) |
| update_center | direct_to_struct | digest | 7863 | 10525 (-25.3%) | n/a | n/a | n/a | n/a | 8218 (-4.3%) |
| update_center | real_typed_struct | typed direct | 11958 | 11952 (+0.0%) | n/a | n/a | n/a | n/a | 10296 (+16.1%) |
| mesh | parse_only | borrowed view over offset tape vs DOM | 9367 | 8143 (+15.0%) | 8318 (+12.6%) | 9414 (-0.5%) | n/a | n/a | 4123 (+127.2%) |
| mesh | direct_to_struct | digest | 8640 | 9967 (-13.3%) | n/a | n/a | n/a | n/a | 7176 (+20.4%) |
| mesh | real_typed_struct | typed direct | 9623 | 9305 (+3.4%) | n/a | n/a | n/a | n/a | 8212 (+17.2%) |
| random | parse_only | borrowed view over offset tape vs DOM | 10011 | 15639 (-36.0%) | 15653 (-36.0%) | 20638 (-51.5%) | n/a | 3526 (+183.9%) | 3486 (+187.2%) |
| random | direct_to_struct | digest | 7751 | 8141 (-4.8%) | n/a | n/a | n/a | n/a | 5922 (+30.9%) |
| gsoc-2018 | parse_only | borrowed view over offset tape vs DOM | 23209 | 49101 (-52.7%) | 49192 (-52.8%) | n/a | n/a | n/a | 10741 (+116.1%) |
| gsoc-2018 | direct_to_struct | digest | 15042 | 23356 (-35.6%) | n/a | n/a | n/a | n/a | 19398 (-22.5%) |
| marine_ik | parse_only | borrowed view over offset tape vs DOM | 13100 | 9921 (+32.1%) | 9930 (+31.9%) | n/a | n/a | n/a | 4091 (+220.2%) |
| marine_ik | direct_to_struct | digest | 9357 | 8559 (+9.3%) | n/a | n/a | n/a | n/a | 7018 (+33.3%) |
| marine_ik | real_typed_struct | typed direct | 11783 | 6951 (+69.5%) | n/a | n/a | n/a | n/a | 7450 (+58.2%) |
| instruments | parse_only | borrowed view over offset tape vs DOM | 13320 | 17976 (-25.9%) | 19713 (-32.4%) | n/a | n/a | 7477 (+78.1%) | 3028 (+339.9%) |
| instruments | direct_to_struct | digest | 8494 | 9872 (-14.0%) | n/a | n/a | n/a | n/a | 7576 (+12.1%) |
| numbers | parse_only | borrowed view over offset tape vs DOM | 12818 | 9854 (+30.1%) | 9691 (+32.3%) | n/a | n/a | n/a | 4422 (+189.9%) |
| numbers | direct_to_struct | digest | 9773 | 7953 (+22.9%) | n/a | n/a | n/a | n/a | 5753 (+69.9%) |
| unicode_mixed | parse_only | borrowed view over offset tape vs DOM | 6390 | 9943 (-35.7%) | 11072 (-42.3%) | 13150 (-51.4%) | n/a | n/a | 2654 (+140.8%) |
| unicode_mixed | direct_to_struct | digest | 3596 | 10077 (-64.3%) | n/a | n/a | n/a | n/a | 4911 (-26.8%) |
| unicode_escapes | parse_only | borrowed view over offset tape vs DOM | 12731 | 13851 (-8.1%) | 15141 (-15.9%) | 5637 (+125.9%) | n/a | n/a | 4040 (+215.1%) |
| unicode_escapes | direct_to_struct | digest | 4020 | 13999 (-71.3%) | n/a | n/a | n/a | n/a | 3720 (+8.1%) |
| unicode_basic | parse_only | borrowed view over offset tape vs DOM | 11189 | 15797 (-29.2%) | 15832 (-29.3%) | 16276 (-31.3%) | n/a | n/a | 3611 (+209.9%) |
| unicode_basic | direct_to_struct | digest | 9363 | 8971 (+4.4%) | n/a | n/a | n/a | n/a | 6002 (+56.0%) |
| distinct_values | parse_only | borrowed view over offset tape vs DOM | 10279 | 18282 (-43.8%) | 16905 (-39.2%) | 22825 (-55.0%) | n/a | n/a | 3158 (+225.5%) |
| distinct_values | direct_to_struct | digest | 4438 | 8950 (-50.4%) | n/a | n/a | n/a | n/a | 5598 (-20.7%) |
| y_string_unicode | parse_only | borrowed view over offset tape vs DOM | 5577 | 12009 (-53.6%) | 13014 (-57.1%) | 13627 (-59.1%) | n/a | n/a | 5657 (-1.4%) |
| y_string_unicode | direct_to_struct | digest | 4828 | 9065 (-46.7%) | n/a | n/a | n/a | n/a | 7599 (-36.5%) |

## Caveats For Absent And Sidecar Values

- `simdjson On Demand` is entirely absent. Do not inherit old DOM deltas or
  infer On Demand behavior from DOM values.
- `asmjson SWAR` and `asmjson AVX-512` are entirely absent. Any SK-V9 asmjson
  comparison needs fresh matched telemetry; AVX-512 additionally needs x86
  silicon and host-feature disclosure.
- Populated `simdjson DOM`, `yyjson default`, and `RapidJSON default` values
  are historical `sk-v7-sidecar-profile` signals in the SK-V8 manifest. They
  may rank candidate rows, but they cannot support strict SOTA admission.
- Missing sidecar cells are explicit absences, not zeros and not failed
  comparators. Treat them as telemetry gaps for SK-V9 if those comparator
  families remain in scope.
- `RapidJSON default` is kept in the matrix because final RESULTS has Mbps
  cells for six parse rows. Its sidecar freshness prevents strict admission;
  any future strict claim needs same-run strictness proof and output-plane
  parity.

## Strict Comparator Discipline For SK-V9

The only deltas that can become admission evidence are strict-vs-strict,
same-run, and matching-plane rows whose measured bbnf path performs validation
inside the row. In the SK-V8 close table, every bbnf row is still
`Strictness=deferred` and `parse_utf8=view-boundary`, so this file contains
planning deltas, not strict SOTA wins. SK-V9 must not count lossy sonic,
historical sidecars, absent sidecars, or parse-row DOM-vs-borrowed-view plane
mismatches as strict wins. Any SK-V9 goalset derived from this matrix must name
the comparator id, strictness, freshness, output plane, and validation path in
the row telemetry before it can pass the strict comparator gate.
