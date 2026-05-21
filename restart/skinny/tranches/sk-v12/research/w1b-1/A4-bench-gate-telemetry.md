# SK-V12 W1b-1 A4 - Bench And Gate Telemetry

Scope: read-only inspection of `bbnf-bench` report/gate surfaces and the
SK-V12 non-JSON companion report. No source edits.

## Findings

`bbnf-bench` already has a companion non-JSON gate path:

- schema: `sk-v12-nonjson-generated-v1`
- flag: `gate --skv12-non-json-report <path>`
- accepted grammars: `css_l4`, `sheets`, `bbnf_self`
- rejects `json`, `parse_only`, stale/coupled oracle paths, unknown fields,
  non-finite/sub-1 Mbps Track 1 or oracle Mbps, sample count `<30`, non-`pass`
  equality, and non-`GO` verdict.

Current gap: the companion schema is too thin for W1b-1. It validates Track
1/oracle Mbps, generated paths, strict equality, host/build/sample context,
JSON guard state, and consumer class, but it does not gate-consume several
SPEC-required generated-size/provenance fields.

`RESULTS.md` is still JSON-schema-v3 plus the SK-V9 W0 telemetry manifest. It
has no CSS/non-JSON row and no lightningcss column. W1b-1 should not extend the
main JSON table for a scaffold-only row.

## Minimal W1b-1 Telemetry Additions

Extend `SkV12NonJsonRow` and its validator, not the JSON `TelemetryRow` table,
with these fields:

```text
strictness
grammar_checksum
input_checksum
input_bytes
measured_validation_path
profile_artifact
generated_loc
generated_module_bytes
grammar_size_guard
lock14_status
lock16_status
scalar_reference_status
checkasm_or_parity_status
```

For W1b-1 CSS scaffold, require:

- `row_id = css_l4/declaration_values/direct_to_struct/main`
- `output_plane = css_l4_declaration_value_fact_stream`
- finite `track1_mbps >= 1.0`
- finite `track2_or_oracle_mbps >= 1.0`
- `sample_count >= 30`
- `strict_output_equality = pass`
- `track2_independence_status = independent_verified`
- `same_wave_consumer_class = companion_gate_generated_baseline`
- `generated_loc > 0`
- `generated_module_bytes > 0`
- `grammar_size_guard = pass`
- `lock14_status = pass`
- `lock16_status = not_applicable:scalar_only` unless SIMD is touched
- `checkasm_or_parity_status = parity_pass`

## Outcome Discipline

Do not add a new outcome enum or new main `RESULTS.md` telemetry variant. Use
existing supported outcomes only.

For W1b-1, prefer `outcome_id = C`, `verdict = GO` for generated scaffold plus
strict oracle parity accepted. Reserve `A` for W1b-2 only, when lightningcss
evidence proves the CSS SOTA admission bar.

W1b-1 should produce a companion report and REDRESS entry, but not claim CSS
ADMIT and not add lightningcss placeholders as fake evidence.
