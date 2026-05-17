# Alpha B Competitor Deltas - SK-V7 to SK-V8

Date: 2026-05-16.

Scope: Pass Alpha alpha-B extraction for SK-V8 planning. Sources read:
`restart/prompts/pass-contracts/PASS-ALPHA.md`, `skinny/RESULTS.md`,
`restart/skinny/tranches/sk-v7/SPEC.md`,
`restart/skinny/tranches/sk-v7/SYNTHESIS.md`,
`restart/skinny/tranches/sk-v7/HANDOFF.md`, and the SK-V7 comparator
reports: `wave-0-strict-baseline.md`, `wave-0-r1-comparator-plane.md`,
`wave-0b-r1-report-schema.md`, `wave-0b-r2-sonic-provenance.md`,
`wave-0b-r3-contract.md`, `wave-0b-schema-v3-close.md`,
`skv7-A1-comparator-repair.md`, `skv7-A2-sota-strict-beat.md`,
`skv7-C5-correlation.md`, and `skv7-C6-sidecars.md`.

Delta formula: `(bbnf Track 1 Mbps / comparator Mbps - 1) * 100`.
Positive means bbnf Track 1 is faster. Negative means the comparator is faster.
Every `n/a` below means no value is present in the read artifacts.

## Evidence Rules

- `skinny/RESULTS.md` is the measured authority for row values.
- `sonic-rs strict`, `sonic-rs lossy`, and `serde_json` are same-run rows in
  the schema-v3 RESULTS table.
- `sonic-rs lossy` is a flaw probe only. It is populated only where the
  current artifacts report it.
- C++ `simdjson`, `yyjson`, `RapidJSON`, and `asmjson` columns are documented
  sidecar profile values when populated. They are planning signals, not
  same-run strict anchors.
- `simdjson On Demand`, `asmjson SWAR`, and `asmjson AVX-512` have no per-row
  Mbps values in current `skinny/RESULTS.md`.
- Row output plane is the bbnf row output plane from RESULTS. Comparator output
  plane and strictness are disclosed in the plane registry below.

## Comparator Plane Registry

| Comparator | Strictness plane | Output plane | Evidence status for SK-V8 planning |
|---|---|---|---|
| bbnf Track 1 | deferred; `parse_utf8=view-boundary`; `escape_complete=yes` | per row: borrowed view over offset tape vs DOM; digest; typed direct | baseline row in RESULTS |
| sonic-rs strict | strict; `parse_utf8=scan-boundary`; `escape_complete=yes` | `from_slice::<Value>` DOM on parse rows; serde/direct workload rows where reported | same-run strict anchor; complete for all 38 rows |
| sonic-rs lossy | permissive; `parse_utf8=none`; not S-anchor eligible | same sonic parse plane when populated | same-run flaw probe on parse rows only |
| simdjson DOM | strict by default | C++ DOM/tape over structural index | sidecar values only; sparse parse-only coverage |
| simdjson On Demand | strict iterator over structural index | lazy forward-only iterator | no per-row Mbps in current artifacts |
| yyjson default | strict by default | DOM-class typed-cell tree | sidecar values only; sparse parse-only coverage |
| yyjson minify | not established by current artifacts | not established by current artifacts | no values in current artifacts |
| asmjson SWAR | permissive flaw probe | DOM/SAX architecture; current M5 notes are synth only | no per-row RESULTS values |
| asmjson AVX-512 | no SK-V7 matched row; future x86 strict match required before claims | flat DOM/SAX architecture | no per-row RESULTS values |
| RapidJSON default | permissive default | `GenericDocument` DOM, copy-on-decode strings in profiled driver | sidecar values only; sparse parse-only flaw probe |
| serde_json | strict | Value DOM floor on parse rows; typed serde direct where workload rows report it | same-run strict reference; complete for all 38 rows |

## Delta Matrix

Cells are `Mbps (delta vs comparator)`.

| Corpus | Workload | Output plane | T1 | sonic strict | sonic lossy | simdjson DOM | yyjson | RapidJSON | serde_json |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|
| twitter | parse_only | borrowed view over offset tape vs DOM | 15752 | 21020 (-25.1%) | 20919 (-24.7%) | 24522 (-35.8%) | 30931 (-49.1%) | 4020 (+291.8%) | 5974 (+163.7%) |
| twitter | direct_to_struct | digest | 11832 | 14885 (-20.5%) | n/a | n/a | n/a | n/a | 10465 (+13.1%) |
| twitter | real_typed_struct | typed direct | 18513 | 15486 (+19.5%) | n/a | n/a | n/a | n/a | 16332 (+13.4%) |
| citm_catalog | parse_only | borrowed view over offset tape vs DOM | 31784 | 25509 (+24.6%) | 23834 (+33.4%) | 35822 (-11.3%) | 20956 (+51.7%) | 6760 (+370.2%) | 7541 (+321.5%) |
| citm_catalog | direct_to_struct | digest | 21438 | 19966 (+7.4%) | n/a | n/a | n/a | n/a | 13065 (+64.1%) |
| canada | parse_only | borrowed view over offset tape vs DOM | 17765 | 13885 (+27.9%) | 13792 (+28.8%) | 11493 (+54.6%) | 13003 (+36.6%) | 5187 (+242.5%) | 5215 (+240.7%) |
| canada | direct_to_struct | digest | 10773 | 12421 (-13.3%) | n/a | n/a | n/a | n/a | 7469 (+44.2%) |
| apache_builds | parse_only | borrowed view over offset tape vs DOM | 12482 | 17381 (-28.2%) | 17397 (-28.3%) | 36014 (-65.3%) | 16275 (-23.3%) | 3945 (+216.4%) | 6051 (+106.3%) |
| apache_builds | direct_to_struct | digest | 11116 | 11122 (-0.1%) | n/a | n/a | n/a | n/a | 9886 (+12.4%) |
| github_events | parse_only | borrowed view over offset tape vs DOM | 15198 | 23034 (-34.0%) | 23023 (-34.0%) | 39642 (-61.7%) | 21426 (-29.1%) | n/a | 7686 (+97.7%) |
| github_events | direct_to_struct | digest | 12270 | 16041 (-23.5%) | n/a | n/a | n/a | n/a | 12799 (-4.1%) |
| update_center | parse_only | borrowed view over offset tape vs DOM | 11193 | 19684 (-43.1%) | 19660 (-43.1%) | 30593 (-63.4%) | 18540 (-39.6%) | n/a | 4244 (+163.7%) |
| update_center | direct_to_struct | digest | 8401 | 11081 (-24.2%) | n/a | n/a | n/a | n/a | 8193 (+2.5%) |
| update_center | real_typed_struct | typed direct | 11879 | 12627 (-5.9%) | n/a | n/a | n/a | n/a | 10602 (+12.0%) |
| mesh | parse_only | borrowed view over offset tape vs DOM | 14265 | 11754 (+21.4%) | 11782 (+21.1%) | 9414 (+51.5%) | n/a | n/a | 4890 (+191.7%) |
| mesh | direct_to_struct | digest | 8259 | 8789 (-6.0%) | n/a | n/a | n/a | n/a | 7165 (+15.3%) |
| mesh | real_typed_struct | typed direct | 9466 | 8696 (+8.9%) | n/a | n/a | n/a | n/a | 6769 (+39.8%) |
| random | parse_only | borrowed view over offset tape vs DOM | 9838 | 15457 (-36.4%) | 15471 (-36.4%) | 20638 (-52.3%) | n/a | 3526 (+179.0%) | 3579 (+174.9%) |
| random | direct_to_struct | digest | 7727 | 8936 (-13.5%) | n/a | n/a | n/a | n/a | 6536 (+18.2%) |
| gsoc-2018 | parse_only | borrowed view over offset tape vs DOM | 23026 | 49292 (-53.3%) | 49278 (-53.3%) | n/a | n/a | n/a | 16349 (+40.8%) |
| gsoc-2018 | direct_to_struct | digest | 15097 | 23407 (-35.5%) | n/a | n/a | n/a | n/a | 19567 (-22.8%) |
| marine_ik | parse_only | borrowed view over offset tape vs DOM | 13797 | 10070 (+37.0%) | 10100 (+36.6%) | n/a | n/a | n/a | 4044 (+241.2%) |
| marine_ik | direct_to_struct | digest | 8943 | 8147 (+9.8%) | n/a | n/a | n/a | n/a | 6966 (+28.4%) |
| marine_ik | real_typed_struct | typed direct | 12020 | 8750 (+37.4%) | n/a | n/a | n/a | n/a | 9269 (+29.7%) |
| instruments | parse_only | borrowed view over offset tape vs DOM | 18038 | 16312 (+10.6%) | 18747 (-3.8%) | n/a | n/a | 7477 (+141.2%) | 4426 (+307.5%) |
| instruments | direct_to_struct | digest | 11972 | 12673 (-5.5%) | n/a | n/a | n/a | n/a | 9350 (+28.0%) |
| numbers | parse_only | borrowed view over offset tape vs DOM | 20609 | 13626 (+51.2%) | 13578 (+51.8%) | n/a | n/a | n/a | 6330 (+225.6%) |
| numbers | direct_to_struct | digest | 12615 | 12838 (-1.7%) | n/a | n/a | n/a | n/a | 8081 (+56.1%) |
| unicode_mixed | parse_only | borrowed view over offset tape vs DOM | 8035 | 16180 (-50.3%) | 16659 (-51.8%) | 13150 (-38.9%) | n/a | n/a | 3887 (+106.7%) |
| unicode_mixed | direct_to_struct | digest | 4579 | 9679 (-52.7%) | n/a | n/a | n/a | n/a | 4956 (-7.6%) |
| unicode_escapes | parse_only | borrowed view over offset tape vs DOM | 12042 | 18415 (-34.6%) | 18828 (-36.0%) | 5637 (+113.6%) | n/a | n/a | 4810 (+150.4%) |
| unicode_escapes | direct_to_struct | digest | 4866 | 14028 (-65.3%) | n/a | n/a | n/a | n/a | 5168 (-5.8%) |
| unicode_basic | parse_only | borrowed view over offset tape vs DOM | 11416 | 15596 (-26.8%) | 15625 (-26.9%) | 16276 (-29.9%) | n/a | n/a | 3336 (+242.2%) |
| unicode_basic | direct_to_struct | digest | 8576 | 8502 (+0.9%) | n/a | n/a | n/a | n/a | 5482 (+56.4%) |
| distinct_values | parse_only | borrowed view over offset tape vs DOM | 6655 | 17148 (-61.2%) | 17166 (-61.2%) | 22825 (-70.8%) | n/a | n/a | 3881 (+71.5%) |
| distinct_values | direct_to_struct | digest | 6105 | 11344 (-46.2%) | n/a | n/a | n/a | n/a | 8221 (-25.7%) |
| y_string_unicode | parse_only | borrowed view over offset tape vs DOM | 6216 | 13537 (-54.1%) | 13551 (-54.1%) | 13627 (-54.4%) | n/a | n/a | 5704 (+9.0%) |
| y_string_unicode | direct_to_struct | digest | 5029 | 9019 (-44.2%) | n/a | n/a | n/a | n/a | 7604 (-33.9%) |

## Incomplete Evidence To Route To SK-V8 Telemetry

| Comparator | Populated evidence | Rows that are telemetry work, not claims |
|---|---|---|
| sonic-rs strict | all 38 RESULTS rows | none for Mbps coverage; continue preserving strict feature provenance |
| sonic-rs lossy | 17 parse_only rows | all direct_to_struct and real_typed_struct rows if SK-V8 wants lossy flaw-probe coverage there |
| serde_json | all 38 RESULTS rows | none for Mbps coverage; output-plane mismatch vs bbnf parse rows remains disclosed |
| simdjson DOM | 13 parse_only rows: twitter, citm_catalog, canada, apache_builds, github_events, update_center, mesh, random, unicode_mixed, unicode_escapes, unicode_basic, distinct_values, y_string_unicode | all direct_to_struct and real_typed_struct rows; parse rows gsoc-2018, marine_ik, instruments, numbers; same-run freshness for every sidecar row |
| simdjson On Demand | no per-row values | all rows |
| yyjson default | 6 parse_only rows: twitter, citm_catalog, canada, apache_builds, github_events, update_center | all direct_to_struct and real_typed_struct rows; all other parse rows; same-run freshness for every sidecar row |
| yyjson minify | no values | all rows if retained in the SK-V8 comparator contract |
| RapidJSON default | 6 parse_only rows: twitter, citm_catalog, canada, apache_builds, random, instruments | all direct_to_struct and real_typed_struct rows; all other parse rows; strict/permissive JSONTestSuite roll-call before any claim |
| asmjson SWAR | no per-row RESULTS values | all rows; current M5 evidence is synth/flaw-probe only |
| asmjson AVX-512 | no per-row RESULTS values | all rows; requires x86 silicon plus matched strictness/output-plane telemetry |

## Planning Signals From Populated Rows

- Same-run strict sonic losses are largest on unicode_escapes direct_to_struct
  (-65.3%), distinct_values parse_only (-61.2%), y_string_unicode parse_only
  (-54.1%), gsoc-2018 parse_only (-53.3%), and unicode_mixed direct_to_struct
  (-52.7%). These are valid strict-sonic planning gaps.
- The hard yyjson sidecar gap remains twitter parse_only: Track 1 15752 Mbps
  vs yyjson 30931 Mbps, delta -49.1%. This may guide SK-V8 fusion work, but it
  is still sidecar evidence until yyjson is rerun as same-run telemetry.
- bbnf has populated sidecar wins against yyjson on citm_catalog parse_only
  (+51.7%) and canada parse_only (+36.6%).
- bbnf has populated sidecar wins against simdjson DOM on canada parse_only
  (+54.6%), mesh parse_only (+51.5%), and unicode_escapes parse_only
  (+113.6%).
- RapidJSON default is permissive and much slower on every populated parse row;
  keep it as a flaw probe/floor, not a SOTA claim.
- Workload rows currently support strict claims only against sonic-rs strict
  and serde_json. Any workload comparison to simdjson, yyjson, RapidJSON, or
  asmjson must be new SK-V8 telemetry rather than a carried claim.
