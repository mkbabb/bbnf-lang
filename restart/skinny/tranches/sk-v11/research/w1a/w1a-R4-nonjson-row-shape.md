# SK-V11 W1a R4: Non-JSON Row Shape And Codegen Constraints

Scope: research the gate/report shape W1a must admit for future W1b non-JSON
baseline rows, without creating that baseline and without changing generated
behavior.

Output: this file only.

## Finding

W1a is a gate/report schema lane, not a parser or baseline wave. The row shape it
prepares must be narrow enough that W1b can later render exactly one generated
non-JSON baseline row, but strict enough that W1a itself cannot be counted as
Lock 14 proof, behavior admission, or generated authority.

The controlling split is:

- W1a: accept/reject non-JSON telemetry shape in fixtures and validators only.
- W1b: create one generated non-JSON baseline row plus independent oracle.
- W2: consume the W1b baseline for the first non-JSON intervention admission.

This is consistent with `SPEC.md` Section 4, which says W1a has no parser row
moves, must reject missing non-JSON fields and producer-only telemetry, must keep
JSON `gate-json --with-cost-facts --check-results` green, and must not claim a
generated non-JSON baseline. Section 5 then assigns the generated baseline and
oracle to W1b. Section 6 says W2 consumes that baseline and may not create the
first measurable non-JSON row.

## Current State

The live skinny code does not yet have a non-JSON generated runtime lane.

- `skinny/crates/codegen/src/json_provider.rs` accepts only
  `backend.grammar_name == "json"` in `ensure_runtime_profile`.
- Both normal and typed codegen call that profile guard before emission in
  `skinny/crates/codegen/src/lib.rs`.
- Normal codegen emits JSON provider files (`generated.rs`, `parser.rs`,
  `scan.rs`, `sink.rs`, `value.rs`, `view.rs`, `visitor.rs`) and appends
  sink-only lowering to the JSON template.
- Typed codegen emits a typed direct module, but it still passes through the
  JSON profile guard.
- Runtime exports only `generated_json` as `grammars::json`; `sheets_witness` is
  proof/test gated and is not a generated parser baseline.
- The report/gate implementation is JSON-shaped today: `validate_sk_v8_w0`
  requires `grammar_id == "json"` and `domain == "json_bench"`, `parse_row_id`
  rejects non-`json/.../.../main`, and `expected_profile_path` only knows
  `parse_only`, `direct_to_struct`, and `real_typed_struct`.

Therefore W1a must not use successful JSON emission, `sheets_witness`, or any
old non-skinny runtime as evidence that a non-JSON generated parser exists.

## Row Shape W1a Should Allow

W1a should allow only fixture-level or validator-level non-JSON rows shaped like
future W1b rows. It should not add a real baseline row to `skinny/RESULTS.md`.

Minimum row identity:

| Field | Allowed W1a shape for future W1b |
|---|---|
| `row_id` | `<grammar_id>/<corpus>/<workload>/main` |
| `grammar_id` | `css_l4`, `sheets`, or `bbnf_self` |
| `domain` | `css_l4_bench`, `sheets_bench`, or `bbnf_self_bench` |
| preferred CSS row | `css_l4/declaration_values/direct/main` or `css_l4/declaration_values/typed/main` |
| Sheets fallback | `sheets/formula/direct/main` or `sheets/formula/typed/main` |
| BBNF fallback | `bbnf_self/grammar/direct/main` or `bbnf_self/grammar/typed/main` |

The P3-A C6 text names the fallback as `google_sheets/formula/{direct,typed}`,
while P3-D's allowed `grammar_id` is `sheets`. W1a should choose one canonical
gate value before fixtures land. The stricter choice is to use `sheets` in the
gate/report schema and reject `google_sheets` unless a same-wave SPEC update
admits it.

Minimum evidence fields:

| Field family | W1a requirement |
|---|---|
| main table | Keep the schema-v3 26-column table shape. Do not add a non-JSON-only column. |
| Track 1 | Required numeric generated-parser Mbps field, but fixture-only in W1a. |
| Track 2/oracle | Required numeric independent Track 2 or oracle Mbps field. |
| output plane | Require an exact plane match between row and comparator/oracle; allowed values are `digest`, `typed direct`, or a SPEC-named non-JSON direct/typed plane. |
| comparator/oracle | Require `comparator_id`, plane, strictness, freshness, value, and source artifact in consumed manifest evidence. |
| profile/sample | Require non-empty profile artifact, `ns_per_byte`, sample count, build flags, host triple, and feature mask. |
| consumer | Reject `gate_only` for non-JSON baseline-shaped rows; require a generated non-JSON direct/typed consumer class name. |
| independence | Require `independent_verified` or a SPEC-named equivalent oracle proof; reject coupled Track 2/oracle evidence. |
| status | Use existing outcome/verdict vocabulary only; no new outcome variant in W1a. |

JSON-specific competitor cells (`sonic-rs strict`, `serde_json`, deltas against
JSON comparators) are not natural non-JSON oracle fields. W1a has two safe
options:

1. Keep non-JSON rows out of the main `RESULTS.md` table until W1b and validate a
   companion fixture/report with the same required identifiers.
2. If W1a extends the main table validator, allow JSON comparator columns to be
   `n/a` only for whitelisted non-JSON rows and require the manifest comparator
   evidence to carry the independent oracle. This must be explicit gate logic,
   not a generic relaxation of `validate_schema_v3`.

The first option has lower risk because the current `gate-json --check-results`
snapshot logic counts JSON manifest rows by lines starting `| json/` and the
current generated `Report` still expects the SK-V8/SK-V9 JSON baseline set.

## Required Rejections

W1a should fail closed on:

- a non-JSON rendered row whose field is not consumed by the same-wave gate;
- a gate requirement for a field the report does not emit;
- any `grammar_id` or `domain` outside the bounded allowlist;
- a row id that does not match rendered corpus/workload;
- missing Track 1, Track 2/oracle, output plane, comparator/oracle source,
  profile artifact, sample cost, sample count, run id, host, feature mask, or
  same-wave consumer class;
- `same_wave_consumer_class = gate_only` on non-JSON baseline-shaped rows;
- direct/typed plane mismatch between row and comparator/oracle;
- coupled Track 2/oracle evidence or a source artifact that calls Track 1;
- old hand non-JSON runtimes used as generated-parser authority;
- `json_provider` emission used as generic proof;
- new report columns, hidden directives, BIR/backend variants, or sidecar fields;
- W1a fixture rows counted as admissions, baselines, or close evidence.

## Strict Boundaries

W1a may edit gate/report/fixture logic in its owner paths, but this research
packet does not. Future W1a implementation must preserve these boundaries:

- No source behavior edits in `skinny/crates/codegen`, `skinny/crates/runtime`,
  grammar files, parser crates, or generated runtime modules.
- No change to `json_provider::ensure_runtime_profile`.
- No generated CSS/Sheets/BBNF runtime output.
- No `skinny/RESULTS.md` row movement.
- No non-JSON baseline authority.
- No generic crate JSON policy under a grammar-neutral name.
- No producer-only telemetry, including PMU/cycles/profile/probe fields that the
  gate does not consume.

The narrow W1a deliverable is a validator contract: "these are the exact
non-JSON fields W1b will need, and missing or extra unconsumed evidence fails."
It must leave all parser, oracle, and baseline creation work to W1b.

## Sources

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 4 - W1a non-JSON gate/report
  schema lane, no parser row moves, no generated baseline authority.
- `restart/skinny/tranches/sk-v11/SPEC.md` Section 5 - W1b generated non-JSON
  baseline and oracle lane.
- `restart/skinny/tranches/sk-v11/SPEC.md` Section 6 - W2 consumes the baseline
  and cannot create the first measurable non-JSON row.
- `restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md` -
  C6 generated FIRST/prefix non-JSON dispatch and C9 accounting-only status.
- `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md` -
  schema-v3 field set, allowed non-JSON values, outcome enum, and producer-only
  rejection rules.
- `skinny/crates/codegen/src/json_provider.rs` - JSON-only runtime profile guard.
- `skinny/crates/codegen/src/lib.rs` - normal and typed emission both call the
  JSON profile guard.
- `skinny/crates/runtime/src/lib.rs` - runtime export surface currently exposes
  generated JSON only.
- `skinny/crates/bbnf-bench/src/report.rs` - schema-v3 header, W0 manifest,
  JSON-only row identity/profile/comparator validation.
- `skinny/xtask/src/main.rs` - `gate-json --with-cost-facts --check-results`
  snapshot checks over the current JSON manifest.
- `grammar/css/l4/values.bbnf`, `grammar/css/l4/color.bbnf`, and
  `grammar/google-sheets/google-sheets.bbnf` - preferred and fallback non-JSON
  grammar surfaces for W1b.

Self-verdict: research-only. No source files edited; no baseline row created.
