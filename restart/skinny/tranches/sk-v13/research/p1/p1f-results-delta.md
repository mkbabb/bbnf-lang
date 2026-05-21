# SK-V13 P1-F: RESULTS Extraction + Delta

Pass: S-P1 Profile. Cycle: V13.
Date: 2026-05-21.
Scope: Extract `skinny/RESULTS.md`, refresh row ledger from `/tmp/skv13-p1`, and record delta vs SK-V12 close where prior close numbers exist.
Output: this file.
Baseline: SK-V13-open (`f8be692068e9e464b6ed24027ab26edfd05303fd` profile identity; repo HEAD observed `1f06f3847d0c1409837a7e9f5944698323ec296f`).
Host triple: aarch64-apple-darwin.
Build flags: release profile; profile binary root `/tmp/skv13-profile-target-0a7b41c5/release`; native CPU per run scripts.
Profile tool: `/tmp/skv13-p1/pmu/pmu_rows.tsv` plus `/tmp/skv13-p1/css/css_l4_declaration_values_measurement.tsv`; checked authority from `skinny/RESULTS.md`.
Corpus coverage: JSON 51/51 conceptual rows; PMU has 17/17 parse, 17/17 direct, 7/17 typed; 10 typed rows are explicit missing rows.

## §1 — Method

Commands/materials consumed:

```sh
sed -n '1,260p' restart/prompts/skinny/PASS-1-PROFILE.md
sed -n '1,260p' skinny/RESULTS.md
sed -n '1,260p' skinny/REDRESS.md
sed -n '1,280p' restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md
sed -n '1,260p' restart/skinny/tranches/sk-v13/HANDOFF.md
sed -n '1,240p' restart/skinny/tranches/sk-v13/SYNTHESIS.md
sed -n '1,240p' restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md
sed -n '1,40p' /tmp/skv13-p1/pmu/pmu_rows.tsv
sed -n '1,80p' /tmp/skv13-p1/css/css_l4_declaration_values_measurement.tsv
```

Classification here is an S-P1 extraction signal, not a gate admission. For
direct and typed rows, `A` means fresh PMU Track 1 and Track 2 are both within
the old direct slack envelope (`>= sonic / 1.10`). `N-direct` means at least one
direct/typed BBNF track misses that envelope. For parse rows, `A` means fresh
PMU Track 1 and Track 2 meet the same envelope against the checked-in
`skinny/RESULTS.md` sonic strict number; `G` means the parse substrate/track
signal still misses. Parse sonic PMU was not captured, so parse `A/G` uses a
stale comparator anchor and is flagged as such.

## §2 — Findings

Checked `skinny/RESULTS.md` contains 41 JSON rows plus the admitted CSS row:
17 `parse_only`, 17 `direct_to_struct`, and 7 `real_typed_struct`. SK-V13's
addendum requires 51 JSON rows, so the 10 absent typed rows are part of the
current extraction surface, not out of scope.

Fresh PMU coverage from `/tmp/skv13-p1/pmu/pmu_rows.tsv`:

| Plane | Fresh PMU rows | Comparator coverage |
|---|---:|---|
| `parse_only` | 17 Track 1 + 17 Track 2 | no fresh sonic/serde PMU; sonic/serde only from checked `RESULTS.md` |
| `direct_to_struct` | 17 Track 1 + 17 Track 2 + 17 sonic + 17 serde | fresh PMU for Rust comparators; C++ sidecars absent for direct plane |
| `real_typed_struct` | 7 Track 1 + 7 Track 2 + 7 sonic + 7 serde | 10 typed rows missing entirely |
| CSS L4 declaration values | 1 generated + cssparser + lightningcss measurement | same row as SK-V12 close, but fresh harness is not the W1b Criterion gate |

JSON row ledger:

| Row | RESULTS outcome | Fresh T1 Mbps | Fresh T2 Mbps | Fresh sonic strict Mbps | T1 c/B | T2 c/B | Fresh class | Delta vs SK-V12 close | Telemetry note |
|---|---|---:|---:|---:|---:|---:|---|---|---|
| json/twitter/parse_only/main | S/NO-GO | 15094 | 11772 | 18716 | 2.26 | 2.87 | G | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/twitter/direct_to_struct/main | N-direct/NO-GO | 11434 | 10483 | 10285 | 2.98 | 3.24 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/twitter/real_typed_struct/main | A/GO | 18492 | 16187 | 15304 | 1.86 | 2.13 | A | n/a (SK-V12 close gives no JSON row numbers) | fresh typed PMU covered generated row; simdjson/yyjson/asmjson/RapidJSON n/a for typed plane |
| json/citm_catalog/parse_only/main | S/NO-GO | 30057 | 20169 | 20645 | 1.14 | 1.69 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/citm_catalog/direct_to_struct/main | A/GO | 21099 | 20058 | 14853 | 1.62 | 1.70 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/citm_catalog/real_typed_struct/main | A/GO | 35379 | 19179 | 21889 | 0.97 | 1.79 | N-direct | n/a (SK-V12 close gives no JSON row numbers) | fresh typed PMU covered generated row; simdjson/yyjson/asmjson/RapidJSON n/a for typed plane |
| json/canada/parse_only/main | S/NO-GO | 17414 | 16529 | 4302 | 1.94 | 2.06 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/canada/direct_to_struct/main | N-direct/NO-GO | 10509 | 10201 | 12174 | 3.25 | 3.34 | N-direct | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/canada/real_typed_struct/main | missing/missing | missing | missing | missing | missing | missing | G | n/a (SK-V12 close gives no JSON row numbers) | missing typed generated coverage in `/tmp/skv13-p1/pmu/pmu_rows.tsv` |
| json/apache_builds/parse_only/main | S/NO-GO | 12000 | 12095 | 8919 | 2.82 | 2.85 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/apache_builds/direct_to_struct/main | A/GO | 10895 | 10064 | 9298 | 3.09 | 3.36 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/apache_builds/real_typed_struct/main | A/GO | 8550 | 5717 | 6695 | 4.02 | 5.96 | N-direct | n/a (SK-V12 close gives no JSON row numbers) | fresh typed PMU covered generated row; simdjson/yyjson/asmjson/RapidJSON n/a for typed plane |
| json/github_events/parse_only/main | S/NO-GO | 13476 | 12642 | 12263 | 2.41 | 2.71 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/github_events/direct_to_struct/main | N-direct/NO-GO | 12010 | 11048 | 11539 | 2.84 | 3.09 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/github_events/real_typed_struct/main | A/GO | 12485 | 11285 | 11643 | 2.74 | 3.03 | A | n/a (SK-V12 close gives no JSON row numbers) | fresh typed PMU covered generated row; simdjson/yyjson/asmjson/RapidJSON n/a for typed plane |
| json/update_center/parse_only/main | S/NO-GO | 11102 | 8894 | 13836 | 3.06 | 3.82 | G | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/update_center/direct_to_struct/main | N-direct/NO-GO | 8197 | 7321 | 8034 | 4.14 | 4.65 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/update_center/real_typed_struct/main | A/GO | 12132 | 9666 | 11398 | 2.83 | 3.53 | N-direct | n/a (SK-V12 close gives no JSON row numbers) | fresh typed PMU covered generated row; simdjson/yyjson/asmjson/RapidJSON n/a for typed plane |
| json/mesh/parse_only/main | S/NO-GO | 13020 | 11489 | 8980 | 2.63 | 2.98 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/mesh/direct_to_struct/main | N-direct/NO-GO | 8778 | 8441 | 9633 | 3.89 | 4.05 | N-direct | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/mesh/real_typed_struct/main | A/GO | 9083 | 7109 | 8505 | 3.79 | 4.83 | N-direct | n/a (SK-V12 close gives no JSON row numbers) | fresh typed PMU covered generated row; simdjson/yyjson/asmjson/RapidJSON n/a for typed plane |
| json/random/parse_only/main | S/NO-GO | 9847 | 7725 | 7116 | 3.48 | 4.41 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/random/direct_to_struct/main | N-direct/NO-GO | 7607 | 6893 | 5712 | 4.44 | 4.90 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/random/real_typed_struct/main | missing/missing | missing | missing | missing | missing | missing | G | n/a (SK-V12 close gives no JSON row numbers) | missing typed generated coverage in `/tmp/skv13-p1/pmu/pmu_rows.tsv` |
| json/gsoc-2018/parse_only/main | S/NO-GO | 18943 | 17695 | 16925 | 1.60 | 1.83 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/gsoc-2018/direct_to_struct/main | N-direct/NO-GO | 9982 | 12425 | 19948 | 2.92 | 2.57 | N-direct | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/gsoc-2018/real_typed_struct/main | missing/missing | missing | missing | missing | missing | missing | G | n/a (SK-V12 close gives no JSON row numbers) | missing typed generated coverage in `/tmp/skv13-p1/pmu/pmu_rows.tsv` |
| json/marine_ik/parse_only/main | S/NO-GO | 13000 | 12599 | 7333 | 2.63 | 2.72 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/marine_ik/direct_to_struct/main | A/GO | 9238 | 9242 | 7610 | 3.68 | 3.67 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/marine_ik/real_typed_struct/main | A/GO | 12037 | 9349 | 8833 | 2.85 | 3.54 | A | n/a (SK-V12 close gives no JSON row numbers) | fresh typed PMU covered generated row; simdjson/yyjson/asmjson/RapidJSON n/a for typed plane |
| json/instruments/parse_only/main | S/NO-GO | 17119 | 11730 | 15207 | 2.01 | 2.94 | G | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/instruments/direct_to_struct/main | N-direct/NO-GO | 11961 | 11017 | 7961 | 2.87 | 3.11 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/instruments/real_typed_struct/main | missing/missing | missing | missing | missing | missing | missing | G | n/a (SK-V12 close gives no JSON row numbers) | missing typed generated coverage in `/tmp/skv13-p1/pmu/pmu_rows.tsv` |
| json/numbers/parse_only/main | S/NO-GO | 18569 | 18336 | 10231 | 1.87 | 1.89 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/numbers/direct_to_struct/main | A/GO | 12369 | 12167 | 12792 | 2.78 | 2.82 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/numbers/real_typed_struct/main | missing/missing | missing | missing | missing | missing | missing | G | n/a (SK-V12 close gives no JSON row numbers) | missing typed generated coverage in `/tmp/skv13-p1/pmu/pmu_rows.tsv` |
| json/unicode_mixed/parse_only/main | S/NO-GO | 7302 | 7327 | 6942 | 4.71 | 4.46 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/unicode_mixed/direct_to_struct/main | N-direct/NO-GO | 4559 | 4452 | 8890 | 7.54 | 7.71 | N-direct | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/unicode_mixed/real_typed_struct/main | missing/missing | missing | missing | missing | missing | missing | G | n/a (SK-V12 close gives no JSON row numbers) | missing typed generated coverage in `/tmp/skv13-p1/pmu/pmu_rows.tsv` |
| json/unicode_escapes/parse_only/main | S/NO-GO | 10518 | 11229 | 14603 | 3.26 | 3.05 | G | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/unicode_escapes/direct_to_struct/main | N-direct/NO-GO | 5018 | 4840 | 13491 | 6.83 | 7.05 | N-direct | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/unicode_escapes/real_typed_struct/main | missing/missing | missing | missing | missing | missing | missing | G | n/a (SK-V12 close gives no JSON row numbers) | missing typed generated coverage in `/tmp/skv13-p1/pmu/pmu_rows.tsv` |
| json/unicode_basic/parse_only/main | S/NO-GO | 11702 | 10765 | 12757 | 2.92 | 3.18 | G | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/unicode_basic/direct_to_struct/main | A/GO | 9039 | 8117 | 6625 | 3.79 | 4.23 | A | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/unicode_basic/real_typed_struct/main | missing/missing | missing | missing | missing | missing | missing | G | n/a (SK-V12 close gives no JSON row numbers) | missing typed generated coverage in `/tmp/skv13-p1/pmu/pmu_rows.tsv` |
| json/distinct_values/parse_only/main | S/NO-GO | 9361 | 5914 | 17080 | 3.66 | 5.83 | G | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/distinct_values/direct_to_struct/main | N-direct/NO-GO | 6256 | 5582 | 8034 | 5.51 | 6.17 | N-direct | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/distinct_values/real_typed_struct/main | missing/missing | missing | missing | missing | missing | missing | G | n/a (SK-V12 close gives no JSON row numbers) | missing typed generated coverage in `/tmp/skv13-p1/pmu/pmu_rows.tsv` |
| json/y_string_unicode/parse_only/main | S/NO-GO | 6081 | 5302 | 13842 | 5.67 | 6.25 | G | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde from RESULTS, not fresh PMU; simdjson/yyjson/asmjson/RapidJSON stale or absent per RESULTS |
| json/y_string_unicode/direct_to_struct/main | N-direct/NO-GO | 3232 | 2919 | 8635 | 10.62 | 11.73 | N-direct | n/a (SK-V12 close gives no JSON row numbers) | sonic/serde fresh PMU; simdjson/yyjson/asmjson/RapidJSON absent for direct plane |
| json/y_string_unicode/real_typed_struct/main | missing/missing | missing | missing | missing | missing | missing | G | n/a (SK-V12 close gives no JSON row numbers) | missing typed generated coverage in `/tmp/skv13-p1/pmu/pmu_rows.tsv` |

CSS admitted row:

| Row | SK-V12 close Track 1 | Fresh Track 1 | SK-V12 close lightningcss | Fresh lightningcss | Delta Track 1 | Delta lightningcss | Fresh class | Telemetry note |
|---|---:|---:|---:|---:|---:|---:|---|---|
| css_l4/declaration_values/direct_to_struct/main | 429.344 Mbps | 48.863 Mbps | 168.930 Mbps | 17.592 Mbps | -380.482 Mbps (-88.6%) | -151.337 Mbps (-89.6%) | A | strict equality still `pass`; fresh `/tmp` harness differs from W1b Criterion gate, so this is a stale-gate/comparator-method anomaly, not an automatic demotion |

## §3 — Delta vs SK-V12

SK-V12 close gives prior numeric values only for the CSS L4 declaration-values
row. It does not publish per-JSON-row close numbers in
`CAMPAIGN-CLOSE-SK-V12-V12.md`; JSON deltas are therefore `n/a` in the row
ledger. The prior JSON authority is the checked `skinny/RESULTS.md` table and
REDRESS history, not a numeric SK-V12 close table.

CSS delta against SK-V12 close is negative under the fresh `/tmp` harness:
Track 1 `429.344 -> 48.863 Mbps`, cssparser `217.427 -> 24.334 Mbps`, and
lightningcss `168.930 -> 17.592 Mbps`. All three moved down by roughly the
same order of magnitude, while strict equality remained pass. That points to a
measurement-method mismatch or scaling artifact in the S-P1 CSS harness, not a
semantic failure.

Fresh JSON PMU changed several direct classifications relative to checked
`RESULTS.md` because the fresh direct sonic PMU values differ materially from
the checked Criterion sonic values. These rows must be treated as profile
signals until a gate-consumed Criterion/PMU reconciliation reruns the same
workload and comparator command surface.

## §4 — Anomalies + Masking Signals

- `skinny/RESULTS.md` still exposes `S` for parse rows, but P1-F was asked to
  classify against the schema-v3 subset `A/C/G/K/L/N-direct`. This artifact
  maps parse misses to `G` and parse passes to `A` for extraction purposes.
- Parse PMU lacks fresh sonic-rs and serde comparator rows. Parse `Fresh sonic
  strict Mbps` is inherited from `skinny/RESULTS.md`, so parse delta-vs-SOTA is
  stale until P1-D or a later gate captures strict sonic parse PMU.
- `simdjson On Demand`, `asmjson SWAR`, and `asmjson AVX-512` are `n/a` or
  absent in the checked results for these planes. They are comparator gaps, not
  proof that the generated parser beats those libraries.
- C++ sidecars for direct/typed planes are absent by plane; direct/typed SOTA
  accounting depends on sonic-rs strict and serde_json only in the fresh PMU.
- Typed coverage is incomplete by construction: fresh PMU covers only
  `twitter`, `citm_catalog`, `apache_builds`, `github_events`,
  `update_center`, `mesh`, and `marine_ik`. The 10 missing typed rows are
  recorded explicitly in §2.
- CSS fresh measurement regresses the absolute Mbps for generated,
  cssparser, and lightningcss together while preserving equality. This is a
  stale/heterogeneous telemetry field against the SK-V12 W1b Criterion close,
  and S-P2/S-P3 should not treat it as a CSS row demotion without a same-harness
  rerun.
- Hot leaf fields in `skinny/RESULTS.md` remain Criterion slope placeholders
  for many rows. P1-E must replace those with resolved samply/xctrace symbols;
  P1-F records them as stale profile telemetry, not resolved hot leaves.

## §5 — Sources

- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `/tmp/skv13-p1/artifacts/identity.txt`
- `/tmp/skv13-p1/pmu/pmu_rows.tsv`
- `/tmp/skv13-p1/pmu/capture_status.tsv`
- `/tmp/skv13-p1/pmu/run-pmu.sh`
- `/tmp/skv13-p1/css/css_l4_declaration_values_measurement.tsv`
