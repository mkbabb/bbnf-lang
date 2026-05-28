# SK-V15 W0-B Gate-Json Parser and Field Consumption

Date: 2026-05-28

Scope: research-only gate-json parser / field-consumption audit for SK-V15 W0.
This report names where RESULTS is parsed today, which fields are currently
consumed, and the code paths, tests, and fixtures needed to fail closed on
missing SK-V15 telemetry, hidden CSS broadcast groups, and producer-only
telemetry.

## 1. Findings

### 1.1 Current authority requires gate consumption, not producer-only fields

SK-V15 makes ten telemetry fields mandatory at every row boundary:
`measurement_row_id`, `measurement_origin`, `value_plane`,
`css_comparator_workload`, `generator_source`, `lock14_scan_scope`,
`lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, and
`broadcast_group_id` (`restart/skinny/tranches/sk-v15/SPEC.md:100-122`).
The same block requires the gate-json path, or its successor, to parse every
emitted field and reject missing fields, producer-only telemetry, hidden
one-to-N CSS measurement stamps, self-exempting exclusions, and native mismatch
closures (`restart/skinny/tranches/sk-v15/SPEC.md:119-122`).

W0 exit specifically requires gate-json to reject missing SK-V15 fields and
hidden broadcast; CSS W8R broadcast rows must carry `broadcast_group_id` and a
non-admit diagnostic status (`restart/skinny/tranches/sk-v15/SPEC.md:246-263`).
The non-JSON proof receiver matrix assigns the JSON guard plus CSS L4
diagnostic/typed rows to `gate.rs` and `report.rs`; that receiver must block
producer-only telemetry and self-exempting reports
(`restart/skinny/tranches/sk-v15/SPEC.md:206-218`).

### 1.2 Where gate-json parses RESULTS today

The xtask command dispatch wires `gate-json` to `gate_json(&root, args.collect())`
(`skinny/xtask/src/main.rs:39`). When invoked with `--check-results`, xtask runs
`validate_w0_results_snapshot(root)` before forwarding to the bench gate binary
(`skinny/xtask/src/main.rs:285-306`).

The RESULTS snapshot parser reads `RESULTS.md` and runs three validators:
`validate_skv14_w0_manifest`, `validate_skv14_w7_redress_triads`, and rolling
delta validation (`skinny/xtask/src/main.rs:400-415`). The SK-V14 manifest path
then parses the `## SK-V14 W0 Telemetry Manifest` section and validates row
counts, duplicates, JSON row presence, and CSS feature presence
(`skinny/xtask/src/main.rs:486-523`).

The actual xtask manifest parser is `parse_skv14_w0_manifest`. It starts at the
SK-V14 W0 manifest heading, stops at the next level-two heading, accepts only
rows starting with `json/` or `css_l4/`, requires exactly 32 Markdown cells, and
maps a subset of those cells into `Skv14ManifestRow`
(`skinny/xtask/src/main.rs:526-584`). It does not parse any SK-V15 field today.

The bench gate binary has a second RESULTS path. `skinny/crates/bbnf-bench/src/bin/gate.rs`
reads `RESULTS.md` for `--skv14-existing-results-capture`, calls
`skv14_existing_results_capture_markdown`, and writes or stale-fails the rendered
capture (`skinny/crates/bbnf-bench/src/bin/gate.rs:407-429`). The normal bench
report path also merges retained non-JSON CSS rows from existing RESULTS when
generated report rows have no CSS rows (`skinny/crates/bbnf-bench/src/bin/gate.rs:778-800`;
`skinny/crates/bbnf-bench/src/bin/gate.rs:816-850`). That merge path is a
critical SK-V15 owner because it can otherwise keep old CSS broadcast rows alive
without new telemetry validation.

### 1.3 Which RESULTS fields xtask currently consumes

The xtask manifest parser requires 32 cells but only stores and validates these
manifest cells:

| Manifest cell | Current xtask field | Current use |
| --- | --- | --- |
| 0 | `row_id` | Row identity, duplicate detection, expected JSON/CSS feature presence. |
| 1 | `grammar_id` | Nonempty and JSON workload validation. |
| 2 | `domain` | Nonempty and JSON workload validation. |
| 3 | `wave_id` | Nonempty; SK-V14 sustained row branch selection. |
| 4 | `run_id` | Nonempty; SK-V14 sustained row expectations. |
| 5 | `track1_entry_point` | Nonempty; same-entry rejection; SK-V14 row expectations. |
| 6 | `track2_entry_point` | Nonempty; same-entry and private tape rejection; SK-V14 row expectations. |
| 7 | `comparator_plane` | Nonempty; eager DOM and sustained row checks. |
| 8 | `per_iter_equality` | Nonempty; pass/fail semantics. |
| 9 | `audit_overlay_verdict` | Nonempty; `AUDIT-FALSIFIED`/sustained checks. |
| 10 | `audit_overlay_reference` | Nonempty; sustained row references. |
| 11 | `sidecar_freshness` | Nonempty; same-run/stale checks. |
| 12 | `substrate_target` | Nonempty and allowlist. |
| 13 | `retention_lifetime` | Nonempty and allowlist. |
| 14 | `policy_owner` | Nonempty and allowlist. |
| 18 | `sample_count` | Nonempty and JSON sample validation. |
| 23 | `redress_entry` | Nonempty and sustained row expectations. |
| 24 | `sk_v14_open_delta` | Nonempty and visible-admit checks. |
| 28 | `same_wave_consumer_class` | Nonempty and sustained row expectations. |
| 29 | `track2_independence_status` | Nonempty and sustained row expectations. |
| 31 | `comparator_evidence` | Nonempty and sustained row expectations. |

This mapping is visible in the `Skv14ManifestRow` struct
(`skinny/xtask/src/main.rs:461-484`) and in `parse_skv14_w0_manifest`
(`skinny/xtask/src/main.rs:526-584`). The row validator checks only this subset
for nonempty or semantic constraints (`skinny/xtask/src/main.rs:587-681`).

The following current 32-cell manifest fields are not consumed by xtask even
though `report.rs` can render/capture them: measured validation path, profile
artifact, sample cost, build flags, host triple, feature mask, CostFacts, substrate
surface, structural projection status, substrate cardinality, and diagnostic
nonproducer status. Adding SK-V15 fields only to the producer side would repeat
this producer-only pattern unless xtask also consumes the new cells.

Visible schema parsing is narrower. The rolling JSON metric parser only consumes
visible table cells needed for JSON throughput deltas, including row id, grammar,
Track 1 Mbps, and sonic Mbps (`skinny/xtask/src/main.rs:1587-1620`). The CSS
metric parser looks for `css_l4/.../direct_to_struct/main` rows and extracts
`track1_generated` and `lightningcss_strict` Mbps from comparator-evidence text
(`skinny/xtask/src/main.rs:1622-1660`). It does not consume measurement ids,
origin, value plane, comparator workload, generator source, lock status,
exclusion reports, or broadcast groups.

### 1.4 Current report/schema carrier lacks SK-V15 fields

The report renderer defines `SKV14_W0_MANIFEST_HEADER` as a 32-column manifest
with no SK-V15 fields (`skinny/crates/bbnf-bench/src/report.rs:8-11`).
`SkV8Telemetry` is the JSON report carrier, uses `#[serde(deny_unknown_fields)]`,
and carries SK-V8/SK-V14 fields but none of the SK-V15 fields
(`skinny/crates/bbnf-bench/src/report.rs:149-198`). `TelemetryRow` also denies
unknown fields and nests `sk_v8` but has no `sk_v15` member
(`skinny/crates/bbnf-bench/src/report.rs:200-222`).

The bench-side parser captures all 32 current SK-V14 manifest cells in
`SkV14ManifestRow` (`skinny/crates/bbnf-bench/src/report.rs:244-278`) and can
parse existing 32-cell rows through `skv14_manifest_row_from_skv14_cells`
(`skinny/crates/bbnf-bench/src/report.rs:5020-5117`). Rendering still emits only
the 32 SK-V14 columns (`skinny/crates/bbnf-bench/src/report.rs:5186-5230`).

Report-level validation consumes many SK-V8/SK-V14 fields through
`TelemetryRow::validate_sk_v8_w0` (`skinny/crates/bbnf-bench/src/report.rs:3443-3573`)
and `Report::validate_sk_v8_w0`
(`skinny/crates/bbnf-bench/src/report.rs:4524-4688`). It does not validate any
SK-V15 telemetry field because no such carrier exists yet.

### 1.5 Hidden CSS broadcast is currently admitted, not rejected

The current xtask sustained-row validator has a special CSS W8R branch for
`SK-V14-W8R`. It requires the generated CSS full-parse Track 1 entry point,
cssparser full-parse probe, lightningcss full-parse comparator, same-run sidecar,
`css_l4_full_parse` substrate, `full_parse_summary` retention, REDRESS-215
supersession, same-wave consumer `gate_css_l4_w8_full_parse_contract`,
`independent_verified:lightningcss+cssparser`, and strict equality evidence
(`skinny/xtask/src/main.rs:1004-1037`). That branch sustains the broadcasted CSS
rows that SK-V15 now requires to be diagnostic-only.

The current RESULTS file has 24 CSS L4 manifest rows under the SK-V14 W0
manifest, all with `AUDIT-SUSTAINED`, the same run id
`SK-V14-W8R:css-full-parse-profile-cold-8`, and the same CSS throughput tuple in
sample cost/comparator evidence (`skinny/RESULTS.md:112-135`). RESULTS notes
also state CSS L4 24/24 admitted and that gate-json consumes the SK-V14 manifest
(`skinny/RESULTS.md:139-152`).

This is exactly the SK-V15 hidden-broadcast risk. W0 must reject either an
explicit one-to-N `broadcast_group_id` with admitting rows, or an implicit
signature where many CSS rows have unique row ids but the same measurement
origin/run id/sample cost/profile/comparator evidence. The P3 schema research
calls out the current 24 CSS rows sharing `track1_mbps=2319.041`,
`cssparser_mbps=2362.037`, and `lightningcss_mbps=929.281` and allows only an
aggregate diagnostic or independently retimed feature rows
(`restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:72-80`).

### 1.6 Existing producer-only protections are partial precedents

JSON companion reports already use `serde(deny_unknown_fields)`, for example
`Report`, `SkV8ComparatorEvidence`, `SkV8Telemetry`, and `TelemetryRow`
(`skinny/crates/bbnf-bench/src/report.rs:114-121`;
`skinny/crates/bbnf-bench/src/report.rs:137-150`;
`skinny/crates/bbnf-bench/src/report.rs:200-222`). There is also an explicit
unknown-producer-field test for an SK-V13 CSS comparator report
(`skinny/crates/bbnf-bench/src/report.rs:9305-9310`).

Those protections do not currently cover the Markdown RESULTS manifest. The
xtask parser rejects any row that is not exactly 32 cells
(`skinny/xtask/src/main.rs:548-554`), but it also ignores several in-schema
cells after counting them. SK-V15 must make the new fields gate-consumed, not
merely rendered by `report.rs`.

## 2. Required code paths and owners

### 2.1 `skinny/xtask/src/main.rs`

Owner role: authoritative `cargo xtask gate-json --check-results` fail-closed
parser for the checked-in RESULTS snapshot.

Required changes:

1. Replace or extend `validate_skv14_w0_manifest` with an SK-V15-aware validator
   called from `validate_w0_results_snapshot` before the bench gate binary is
   invoked (`skinny/xtask/src/main.rs:400-415`).
2. Extend `Skv14ManifestRow` or add `Skv15ManifestRow` so the parser stores the
   ten SK-V15 fields, not just counts their columns
   (`skinny/xtask/src/main.rs:461-584`).
3. Require an exact SK-V15 manifest shape. If the carrier stays Markdown, reject
   rows that do not have the current 32 cells plus the 10 SK-V15 cells. If the
   carrier moves to a JSON payload, use strict typed structs and reject unknown
   fields. Either way, every emitted SK-V15 field must be read by the validator.
4. Add `validate_skv15_telemetry_row` predicates:
   - all ten SK-V15 fields nonempty and non-placeholder except documented
     non-applicable values,
   - `measurement_row_id` uniqueness for admitting rows,
   - `measurement_origin` present and tied to row/workload,
   - `value_plane` compatible with the row's admission claim,
   - `css_comparator_workload` compatible with CSS typed/direct/full parse
     claims,
   - `generator_source` rejects hand-written or `CSS_GENERATED_RS` live CSS
     admission,
   - `lock14_scan_scope` rejects incomplete or exclusion-silent scans,
   - `lock16_status` rejects source-present-unwired or non-native claims,
   - `checkasm_or_parity_status` rejects pending/smoke-only/commandless native
     claims,
   - `gate_exclusion_report` rejects missing, incomplete, or self-exempting
     reports,
   - `broadcast_group_id` is mandatory for aggregate CSS measurements and cannot
     be used to admit many feature rows from one measurement.
5. Add `validate_skv15_css_broadcast_groups` over all CSS rows. It must build
   both an explicit `broadcast_group_id` map and a hidden signature map using at
   least row class, measurement origin, run id, sample count, profile artifact,
   sample-cost throughput tuple, and comparator evidence profile. Reject multiple
   admitting rows per group or signature unless they are demoted to diagnostic
   non-admission.
6. Replace the SK-V14 CSS W8R sustained branch that currently admits W8R rows
   (`skinny/xtask/src/main.rs:1004-1037`) with SK-V15 diagnostic-only handling.
7. Extend `validate_skv14_visible_admits`, which currently scans JSON visible
   rows only (`skinny/xtask/src/main.rs:1091-1125`), so visible CSS A/GO or
   `AUDIT-SUSTAINED` claims cannot bypass the SK-V15 broadcast validator.
8. Consider replacing xtask's simple `markdown_cells` helper
   (`skinny/xtask/src/main.rs:1699-1708`) with the escaped-pipe-aware parser used
   by the report capture path (`skinny/crates/bbnf-bench/src/report.rs:5233-5266`)
   if SK-V15 fields remain Markdown text.

### 2.2 `skinny/crates/bbnf-bench/src/report.rs`

Owner role: telemetry carrier, renderer, existing RESULTS capture, JSON report
validation, and report-level tests.

Required changes:

1. Add an SK-V15 carrier. Preferred shape is a nested `SkV15Telemetry` with
   `#[serde(deny_unknown_fields)]`, stored on `TelemetryRow`, instead of
   overloading `SkV8Telemetry` (`skinny/crates/bbnf-bench/src/report.rs:149-222`).
2. Extend the manifest header and renderer from the current 32 SK-V14 columns to
   include the ten SK-V15 fields (`skinny/crates/bbnf-bench/src/report.rs:8-11`;
   `skinny/crates/bbnf-bench/src/report.rs:5186-5230`).
3. Extend existing RESULTS capture so `skv14_existing_results_capture_markdown`
   and `skv14_manifest_row_from_skv14_cells` either parse the new SK-V15 row
   shape or explicitly reject stale SK-V14-only rows
   (`skinny/crates/bbnf-bench/src/report.rs:4803-4880`;
   `skinny/crates/bbnf-bench/src/report.rs:5020-5117`).
4. Add `TelemetryRow::validate_sk_v15_w0` and call it from report-level W0
   validation next to `validate_sk_v8_w0`
   (`skinny/crates/bbnf-bench/src/report.rs:3443-3573`;
   `skinny/crates/bbnf-bench/src/report.rs:4524-4688`).
5. Extend manifest semantic validation so the SK-V15 host requirement is exact
   enough for W0. Current host validation requires structured arch/cpu fields but
   does not enforce Apple M5 Max/aarch64 specifically
   (`skinny/crates/bbnf-bench/src/report.rs:6525-6603`).
6. Add report-level broadcast validation for generated/captured CSS rows. The
   SK-V13 CSS comparator oracle report can remain a companion report, but its
   current validation has no SK-V15 measurement row id or broadcast semantics
   (`skinny/crates/bbnf-bench/src/report.rs:3113-3222`).

### 2.3 `skinny/crates/bbnf-bench/src/bin/gate.rs`

Owner role: gate binary orchestration, `--check-results`, existing capture, and
retained non-JSON row merge.

Required changes:

1. Ensure both normal report validation and `--skv14-existing-results-capture`
   invoke the SK-V15 report/manifest validators before writing RESULTS
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:407-429`;
   `skinny/crates/bbnf-bench/src/bin/gate.rs:778-800`).
2. Prevent `merge_retained_non_json_results` from carrying old CSS rows forward
   unless those rows have passed SK-V15 field and broadcast validation
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:816-850`).
3. Keep companion report flags behind the same `--check-results` gate for
   RESULTS-affecting validation. Existing companion paths show this pattern for
   CSS comparator reports and other reports
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:56-405`).

## 3. Required rejection predicates

The SK-V15 gate-json successor should reject all of these conditions:

1. Missing or blank SK-V15 field. Every row must carry all ten fields named in
   SK-V15 (`restart/skinny/tranches/sk-v15/SPEC.md:100-122`).
2. Producer-only SK-V15 telemetry. A field rendered by `report.rs` but absent
   from xtask parsing or report validation is a failure. Unknown JSON keys must
   fail through `serde(deny_unknown_fields)`; unknown Markdown manifest cells
   must fail by exact header/cell-count matching.
3. Multiple admitting CSS rows sharing one `measurement_row_id`. This is legal
   only for an aggregate diagnostic row or non-admitting diagnostic rows with an
   explicit `broadcast_group_id`.
4. Hidden broadcast signatures. Unique `measurement_row_id` values do not make
   24 rows independent if measurement origin, run id, sample count, profile,
   sample cost, and comparator tuple are identical.
5. CSS `value_plane` mismatch. `fact_stream`, `full_parse_summary`, brace-counter
   probes, or other audit planes cannot close a live CSS typed/direct admission.
6. CSS comparator workload mismatch. A lightningcss or cssparser full-parse
   comparator cannot close a feature-specific typed admission unless the row is
   explicitly diagnostic.
7. Generator-source contrivance. Live CSS admission must reject hand-written,
   `CSS_GENERATED_RS`, string-literal, or missing Pattern-H provenance.
8. Lock14 incomplete scan or self-exempting exclusion report. The exclusion
   schema requires included roots, excluded roots, reason, owner, self-scan
   status, primitive status, gate consumer, affected rows, and disposition
   (`restart/skinny/tranches/sk-v15/SPEC.md:233-244`).
9. Lock16/native mismatch. `source-present-unwired`, non-native SIMD/ASM claims,
   pending checkasm, smoke-only parity, or commandless parity cannot close.
10. Host mismatch. W0 requires Apple M5 Max/aarch64 authority; current generic
    arch/cpu validation is not enough for SK-V15 W0
    (`restart/skinny/tranches/sk-v15/SPEC.md:246-263`;
    `skinny/crates/bbnf-bench/src/report.rs:6525-6603`).

## 4. Required tests and fixtures

### 4.1 xtask tests in `skinny/xtask/src/main.rs`

Add focused tests beside the existing gate-json passthrough, CostFacts, and
rolling delta tests (`skinny/xtask/src/main.rs:1968-2323`):

1. `skv15_results_manifest_rejects_missing_required_telemetry`: mutate each of
   the ten SK-V15 fields to blank/missing in a fixture row; expect
   `validate_w0_results_snapshot` or the new manifest validator to fail.
2. `skv15_results_manifest_rejects_producer_only_telemetry`: add an extra
   unknown SK-V15-like Markdown cell/header or JSON key; expect strict rejection.
3. `skv15_results_manifest_rejects_hidden_css_broadcast_signature`: create 24
   CSS rows with unique row ids and `broadcast_group_id=none`, but identical
   measurement origin/run id/profile/sample cost/comparator tuple and admitting
   status; expect fail.
4. `skv15_results_manifest_rejects_visible_css_broadcast_admission`: create CSS
   rows with an explicit shared `broadcast_group_id` and A/GO or
   `AUDIT-SUSTAINED`; expect fail.
5. `skv15_results_manifest_accepts_css_broadcast_only_as_diagnostic`: one
   aggregate diagnostic row, or multiple non-admitting diagnostic rows, with a
   shared `broadcast_group_id`; expect pass.
6. `skv15_results_manifest_rejects_css_wrong_value_plane_admit`: set
   `value_plane=full_parse_summary` or `fact_stream` on an admitting CSS typed
   row; expect fail.
7. `skv15_results_manifest_rejects_generator_source_contrivance`: set
   `generator_source=CSS_GENERATED_RS`, `hand_written`, or missing Pattern-H
   provenance on an admitting CSS row; expect fail.
8. `skv15_results_manifest_rejects_self_exempting_gate_exclusion`: set a
   self-exempting or incomplete `gate_exclusion_report`; expect fail.
9. `skv15_results_manifest_rejects_unwired_or_pending_lock16`: set
   `lock16_status=source-present-unwired` or an ASM/SIMD claim with pending
   checkasm/parity; expect fail.

### 4.2 report.rs tests in `skinny/crates/bbnf-bench/src/report.rs`

Add tests near the existing W0 schema/manifest tests
(`skinny/crates/bbnf-bench/src/report.rs:9328-9434`) and producer-only unknown
field precedent (`skinny/crates/bbnf-bench/src/report.rs:9305-9310`):

1. `skv15_telemetry_rejects_unknown_producer_fields`: prove `SkV15Telemetry`
   uses `deny_unknown_fields` and rejects unknown keys.
2. `skv15_telemetry_rejects_missing_required_fields`: deserialize or validate a
   row missing each required SK-V15 field; expect fail.
3. `skv15_manifest_renders_required_fields`: render markdown and assert the ten
   SK-V15 columns are present and populated.
4. `skv15_existing_results_capture_rejects_skv14_only_manifest`: feed a current
   32-cell SK-V14 manifest row into capture; expect stale/missing SK-V15 failure.
5. `skv15_existing_results_capture_rejects_hidden_css_broadcast`: feed the W8R
   24-row hidden-broadcast shape; expect fail.
6. `skv15_report_validation_rejects_css_producer_only_fields`: ensure generated
   report telemetry cannot carry fields that are not consumed by validation.

### 4.3 gate.rs tests in `skinny/crates/bbnf-bench/src/bin/gate.rs`

Add orchestration tests near the existing companion-report flag tests
(`skinny/crates/bbnf-bench/src/bin/gate.rs:4833-5075`):

1. `update_results_retained_css_rows_require_skv15_manifest`: prove
   `merge_retained_non_json_results` cannot retain old CSS rows unless SK-V15
   fields and broadcast validation pass.
2. `existing_results_capture_runs_skv15_validation_before_write`: prove
   `--skv14-existing-results-capture --update-results` does not write a stale
   SK-V14-only or hidden-broadcast RESULTS snapshot.
3. `check_results_rejects_companion_report_with_producer_only_skv15_fields`:
   prove companion JSON report validation cannot bypass strict SK-V15 schema.

### 4.4 Fixture shapes

The tests can use inline Markdown/JSON strings or checked-in fixtures. The
minimal fixture set should cover:

1. `skv15-json-51-complete`: 51 JSON rows with all SK-V15 fields populated and
   non-applicable CSS fields set to explicit gate-consumed values.
2. `skv15-css-w8r-hidden-broadcast`: 24 CSS rows mirroring the current W8R tuple,
   unique row ids, no explicit broadcast group, admitting status; must fail.
3. `skv15-css-w8r-visible-broadcast-admit`: shared `measurement_row_id` or
   `broadcast_group_id` across admitting CSS rows; must fail.
4. `skv15-css-w8r-diagnostic-aggregate`: aggregate diagnostic broadcast row, or
   non-admitting diagnostic rows, with explicit `broadcast_group_id`; must pass.
5. `skv15-producer-only-extra-field`: JSON unknown key or Markdown extra column;
   must fail.
6. `skv15-self-exempting-exclusion`: `gate_exclusion_report` missing required
   schema members or exempting the gate itself; must fail.
7. `skv15-generator-source-css-generated-rs`: admitting CSS row sourced from
   `CSS_GENERATED_RS`/hand-written/string literal; must fail.

## 5. Risks to pre-block

1. Leaving the CSS W8R sustained branch intact will preserve the exact broadcast
   shape SK-V15 W0 is meant to reject (`skinny/xtask/src/main.rs:1004-1037`;
   `skinny/RESULTS.md:112-135`).
2. Extending only `report.rs` would create producer-only SK-V15 telemetry because
   xtask currently parses only selected SK-V14 manifest cells
   (`skinny/xtask/src/main.rs:526-584`).
3. Retaining non-JSON CSS rows during RESULTS updates can bypass SK-V15
   validation unless `merge_retained_non_json_results` is guarded
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:816-850`).
4. The current JSON-visible-admit validator scans JSON rows only, so CSS visible
   admission needs an explicit SK-V15 path (`skinny/xtask/src/main.rs:1091-1125`).
5. Current host validation is structured but not authority-exact for Apple M5
   Max/aarch64 (`skinny/crates/bbnf-bench/src/report.rs:6525-6603`).

## 6. Sources

- `restart/skinny/tranches/sk-v15/SPEC.md`
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v7/SPEC.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/research/w1a/skv11-W1a-CH5-report.md`
- `skinny/xtask/src/main.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
