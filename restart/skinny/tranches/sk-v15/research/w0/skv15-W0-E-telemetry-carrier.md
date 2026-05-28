# SK-V15 W0-E Telemetry Carrier Mapping

Scope: research-only carrier decision for SK-V15 W0. This report names the
carrier for the ten SK-V15 telemetry fields, the representation format, parser
compatibility, and the proof obligations for report/gate consumption. No source,
generated output, RESULTS, REDRESS, gate, or corpus file is edited by this
report.

## Authority Read

- G-Omega V9 is closed by user authorization, and the next sequenced step is
  SK-V15 W0 Baseline and Telemetry Lock through `DISPATCH-PROMPT.md`
  (`restart/audit/totality/astral/V9/G-OMEGA-SIGNOFF.md:5-17`,
  `restart/audit/totality/astral/V9/G-OMEGA-SIGNOFF.md:68-74`).
- W0 research scope is exactly RESULTS schema, SK-V8/SK-V15 telemetry carrier,
  `gate-json` parser, CSS broadcast rows, and JSON 51 guard rows
  (`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:119-128`).
- SK-V15 preserves the visible schema-v3 RESULTS surface and SK-V8 telemetry
  carrier, then requires ten additional gate-consumed fields
  (`restart/skinny/tranches/sk-v15/SPEC.md:100-122`).
- W0 exits only when the gate consumes SK-V15 telemetry, CSS W8R broadcast rows
  are diagnostic/non-admit, `gate-json` rejects missing fields and hidden
  broadcast, and admission host telemetry is Apple M5 Max / aarch64
  (`restart/skinny/tranches/sk-v15/SPEC.md:246-262`).

## Carrier Choice

Use `skinny/RESULTS.md`'s W0 telemetry manifest as the canonical SK-V15 carrier.
Do not add the ten fields to the visible schema-v3 table, and do not make a
separate JSON sidecar the only authority.

Exact carrier:

```text
## SK-V15 W0 Telemetry Manifest

| Row id | Grammar | Domain | Wave | Run id | Track 1 entry | Track 2 entry | Comparator plane | Per-iter equality | Audit overlay | Audit reference | Sidecar freshness | Substrate target | Retention lifetime | Policy owner | Validation | Profile artifact | Sample cost | Sample count | Build flags | Host triple | Feature mask | CostFacts | Redress | SK-V14-open delta | Substrate | Structural projection | Cardinality | Consumer | Track 2 | Diagnostic nonproducer | Comparator evidence | measurement_row_id | measurement_origin | value_plane | css_comparator_workload | generator_source | lock14_scan_scope | lock16_status | checkasm_or_parity_status | gate_exclusion_report | broadcast_group_id |
```

This is the existing 32-cell SK-V14 manifest with the ten SK-V15 fields appended
as cells 33-42, using the exact snake_case field names from P3-D. Appending is
intentional: the current manifest parser indexes the existing 32 cells directly,
so old field offsets remain stable while the SK-V15 parser can require the final
ten cells (`skinny/xtask/src/main.rs:526-579`).

The visible schema-v3 table remains unchanged because SK-V15 explicitly preserves
that surface (`restart/skinny/tranches/sk-v15/SPEC.md:102-104`). The current
header constant is the 26-column rendered schema-v3 surface
(`skinny/crates/bbnf-bench/src/report.rs:8-11`), matching the P3-D note that
the live rendered surface is 26 columns even though older prompt shorthand said
24 (`restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:14-18`).

## Representation Format

Each SK-V15 field is one Markdown table cell. Values are non-empty ASCII tokens
or semicolon-delimited `key=value` subfields. Literal `|` is forbidden in cell
values unless escaped by the renderer, because the live xtask parser splits
Markdown rows by pipe (`skinny/xtask/src/main.rs:1699-1708`). Non-applicable
values must be explicit, e.g. `n/a:not-css`, `not-applicable:no-simd-or-asm`,
or `none:full-surface-scan`, matching P3-D's no-omission rule
(`restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:34-40`).

Field mapping:

| Field | Carrier cell | Required value shape |
|---|---:|---|
| `measurement_row_id` | 33 | Stable timing-row id. JSON independent rows may use the full `row_id`; CSS W8R broadcast rows must share one explicit aggregate id or collapse to one diagnostic row. |
| `measurement_origin` | 34 | Semicolon-delimited command/artifact/run tuple, including source artifact and run id. Empty or unresolved artifact rejects. |
| `value_plane` | 35 | Machine token matching the measured Track 1 semantic plane; examples: `json_parse_only`, `json_direct_strict_product`, `json_typed_direct`, `css_typed_value`, `css_document_value_view`, `diagnostic_aggregate`, `full_parse_summary`. |
| `css_comparator_workload` | 36 | CSS comparator workload or `n/a:not-css`; examples: `cssparser:stylesheet_full_parse`, `cssparser:typed_value`, `lightningcss:cssom`. |
| `generator_source` | 37 | Grammar source and generator id, or explicit hand-written diagnostic status. Live CSS admission must not use `hand_written:*` or `CSS_GENERATED_RS`. |
| `lock14_scan_scope` | 38 | Included roots and exclusions as `included=...;excluded=...;owner=...`; `incomplete:*` cannot close. |
| `lock16_status` | 39 | One of the P3-D primitive statuses: `not-applicable`, `scalar-only`, `simd-claimed`, `asm-claimed`, `source-present-unwired`, `deleted`, or `architectural-block-with-redress`. |
| `checkasm_or_parity_status` | 40 | Executable parity/checkasm command or explicit non-applicable reason; `pending` and smoke-only strings reject. |
| `gate_exclusion_report` | 41 | Machine-readable empty-list proof or reported exclusions with owner/reason/disposition; `self-exempting:*` rejects. |
| `broadcast_group_id` | 42 | `none:independent` for independent measurements, or a shared group id for aggregate/broadcast diagnostics. Shared groups cannot emit multiple independent `A / GO` CSS admits. |

The CSS W8R rows in current RESULTS are the negative fixture. They repeat the
same Track 1, cssparser, and lightningcss tuple across 24 CSS conceptual rows
(`skinny/RESULTS.md:112-116`, `skinny/RESULTS.md:132-135`). P3-D makes that
legal only as one aggregate diagnostic row or as non-admission rows with an
explicit shared `broadcast_group_id`; assigning fresh row ids to the one timing
tuple is illegal (`restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:72-81`).

## Compatibility

Markdown RESULTS compatibility:

- The schema-v3 table at the top of `RESULTS.md` remains untouched. Existing
  metric readers that index `Corpus`, `Workload`, Track 1, and comparator cells
  continue to operate on that table (`skinny/RESULTS.md:3-4`,
  `skinny/xtask/src/main.rs:1587-1619`).
- The manifest remains a Markdown table under a W0 telemetry heading. Current
  RESULTS already carries that shape at `## SK-V14 W0 Telemetry Manifest`
  (`skinny/RESULTS.md:57-60`), and the renderer already appends the manifest
  after the visible table (`skinny/crates/bbnf-bench/src/report.rs:4718-4767`,
  `skinny/crates/bbnf-bench/src/report.rs:5186-5228`).
- The SK-V15 parser should accept the 32-cell SK-V14 section only as a legacy
  pre-W0 snapshot. After W0, `gate-json --check-results` must require the
  `## SK-V15 W0 Telemetry Manifest` heading and exactly 42 cells per row. A
  dual-heading transition may be used only during the implementation patch, but
  close evidence must prove the SK-V15 heading and ten fields are present.

JSON/internal parser compatibility:

- Existing companion JSON reports use typed `serde` structs with
  `deny_unknown_fields`, so producer-only JSON fields already fail in that path
  (`skinny/crates/bbnf-bench/src/report.rs:149-202`,
  `skinny/crates/bbnf-bench/src/report.rs:9236-9241`,
  `skinny/crates/bbnf-bench/src/report.rs:9277-9281`,
  `skinny/crates/bbnf-bench/src/report.rs:9304-9309`).
- Do not silently add defaults for the ten SK-V15 fields. Several existing
  SK-V14 fields have legacy defaults for backward compatibility
  (`skinny/crates/bbnf-bench/src/report.rs:169-190`); SK-V15 fields must be
  required in the SK-V15 manifest struct or in a required nested
  `sk_v15` object for any JSON payload.
- To avoid breaking older companion report fixtures that alias `TelemetryRow`,
  prefer a dedicated `SkV15ManifestRow` / `SkV15Telemetry` validator for the
  manifest path rather than adding ten required fields to every historical
  `TelemetryRow` user (`skinny/crates/bbnf-bench/src/report.rs:201-222`,
  `skinny/crates/bbnf-bench/src/report.rs:314-315`).
- If W0 adds an equivalent JSON payload for internal checks, it must be a
  typed schema such as `schema_id = "sk-v15-w0-telemetry-v1"` with `rows[]`
  containing the 32 inherited manifest fields plus a required `sk_v15` object
  with the ten exact fields. Unknown fields and missing fields must fail parse
  or validation, not downgrade to warnings.

## Gate Consumption Proof

W0 report/gate should prove consumption with fail-closed tests and command
evidence, not by showing rendered text alone.

Required proof shape:

1. `gate-json --check-results` reads `skinny/RESULTS.md`, finds the SK-V15 W0
   manifest, parses exactly 42 cells, and validates all 75 expected rows: 51
   JSON guard rows plus 24 CSS diagnostic/collapsed rows. Current SK-V14 code
   already validates row count, uniqueness, missing rows, required non-empty
   fields, and sustained-row semantics for the 32-cell manifest; W0 extends
   that validator instead of adding a producer-only renderer
   (`skinny/xtask/src/main.rs:486-524`, `skinny/xtask/src/main.rs:587-681`).
2. Negative fixtures remove each of the ten SK-V15 cells one at a time and
   assert `gate-json --check-results` fails before verdict computation. P3-D
   requires missing SK-V15 fields to reject after W0
   (`restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:57-70`).
3. Broadcast fixtures cover both visible and hidden one-to-N measurement reuse:
   24 CSS `A / GO` rows sharing `measurement_row_id` fail, and 24 CSS rows with
   unique ids but identical origin/metrics/profile signatures also fail
   (`restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:87-97`).
4. CSS plane fixtures prove that `value_plane=full_parse_summary`,
   `fact_stream`, or brace-counter output cannot satisfy CSS Value API or CSS
   SOTA close, and lightningcss is diagnostic until Track 1 emits comparable
   CSSOM/value output (`restart/skinny/tranches/sk-v15/SPEC.md:86-98`,
   `restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:48-55`).
5. Gate-exclusion and primitive fixtures prove `gate_exclusion_report` with
   `self-exempting:*` rejects, `lock16_status=asm-claimed` requires a strict
   Apple M5 Max / aarch64 parity/checkasm command, and `source-present-unwired`
   cannot close without deletion, scalar delegation, or REDRESS
   (`restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:52-55`,
   `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:93-103`).
6. Report-code tests prove the renderer emits the 42-column header and all ten
   values, while parser tests prove an extra `producer_only_field` or unknown
   JSON key fails in any JSON/internal payload. This mirrors the existing
   producer-only rejection tests for SK-V12/SK-V13 JSON reports
   (`skinny/crates/bbnf-bench/src/report.rs:9236-9241`,
   `skinny/crates/bbnf-bench/src/report.rs:9277-9281`,
   `skinny/crates/bbnf-bench/src/report.rs:9304-9309`).

Minimum close command evidence for W0:

```sh
cargo xtask gate-json --check-results
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench skv15_w0
cargo test --manifest-path skinny/Cargo.toml -p xtask skv15_w0
```

The exact test names can differ, but the output must show missing-field,
producer-only-field, hidden-broadcast, CSS-plane-mismatch, self-exempting
exclusion, and Lock 16/checkasm negative fixtures. `gate-json` already gates
companion report paths behind `--check-results`, so W0 should keep that pattern
for any SK-V15 JSON payload (`skinny/crates/bbnf-bench/src/bin/gate.rs:95-125`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:351-395`,
`skinny/xtask/src/main.rs:285-292`, `skinny/xtask/src/main.rs:400-406`).

## Decision

The exact SK-V15 carrier is a 42-cell Markdown W0 telemetry manifest in
`skinny/RESULTS.md`, headed `## SK-V15 W0 Telemetry Manifest`, preserving the
visible schema-v3 table and appending the ten snake_case SK-V15 fields after
`Comparator evidence`. Internal JSON support is allowed only as a typed,
gate-consumed mirror; it is not the sole authority unless `gate-json
--check-results` consumes it and rejects unknown/missing fields. Close proof is
the gate parser and negative fixtures proving every emitted SK-V15 field is
load-bearing.
