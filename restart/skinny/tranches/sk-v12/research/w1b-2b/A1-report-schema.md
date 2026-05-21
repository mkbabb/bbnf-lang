# SK-V12 W1b-2b A1 - CSS L4 SOTA Report Schema

Date: 2026-05-20.
Phase: W1b-2b research.
Scope: minimal `sk-v12-css-l4-sota-v1` report contract for
`G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`.

## Disposition

Use a W1b-2b companion schema, not `sk-v12-nonjson-generated-v1`. The existing
non-JSON schema admits a generated baseline/intervention row; W1b-2b must gate
the user-pin SOTA bar:

```text
track1_mbps > lightningcss_mbps + 1
```

The report consumes the already-landed W1b-2a Criterion and equality artifacts
for the single CSS L4 row:

```text
css_l4/declaration_values/direct_to_struct/main
```

## Minimal Report Shape

Top-level fields:

- `schema_id = sk-v12-css-l4-sota-v1`
- `wave_id = SK-V12-W1b-2b`
- `run_id`
- `rows`

The report is valid only with exactly one row for W1b-2b. Additional rows should
fail closed until a later plan expands the gate.

## Minimal Row Fields

Identity and plane:

- `row_id = css_l4/declaration_values/direct_to_struct/main`
- `grammar_id = css_l4`
- `domain = non_json_generated:css_l4:declaration_values`
- `corpus_or_workload = declaration_values`
- `workload = direct_to_struct`
- `output_plane = css_l4_declaration_value_fact_stream`
- `strictness = strict`
- `outcome_id`
- `verdict`
- `gate_status`

Generated Track 1 provenance:

- `generated_track1_source_path`
- `generated_runtime_path`
- `generated_input_provenance`
- `grammar_checksum`
- `input_checksum`
- `input_bytes`
- `generated_loc`
- `generated_module_bytes`
- `grammar_size_guard`

Measurements and artifacts:

- `track1_mbps`
- `track1_artifact`
- `track2_or_oracle_mbps`
- `track2_or_oracle_source_path`
- `track2_independence_status`
- `cssparser_artifact_path`
- `lightningcss_mbps`
- `lightningcss_version`
- `lightningcss_command`
- `lightningcss_artifact`
- `lightningcss_fact_artifact_path`
- `benchmark_artifact_path`
- `measured_validation_path`
- `profile_artifact`
- `sample_count`
- `sample_cost`

Admission math:

- `threshold_mbps`
- `admission_margin_mbps`
- `admission_status`

Equality and guard context:

- `strict_output_equality`
- `three_way_equality`
- `lightningcss_sequence_status`
- `host_triple`
- `feature_mask`
- `build_flags`
- `lock14_status`
- `lock16_status`
- `scalar_reference_status`
- `checkasm_or_parity_status`
- `json_guard_state`
- `same_wave_consumer_class`
- `redress_entry`

## Validation Invariants

Report identity:

- `schema_id` is exactly `sk-v12-css-l4-sota-v1`.
- `wave_id` is exactly `SK-V12-W1b-2b`.
- `run_id` is non-empty, passes the SK-V12 run-id shape, and is not an
  inherited W1b-1-only id.
- `rows.len() == 1`.

Row identity:

- `row_id`, `grammar_id`, `corpus_or_workload`, `workload`, and `output_plane`
  match the CSS L4 declaration-values row exactly.
- `domain` contains `non_json_generated`, `css_l4`, and `declaration_values`.
- `strictness == strict`.
- `workload` is not `parse_only`.
- `same_wave_consumer_class ==
  companion_gate_css_l4_lightningcss_sota`.
- `redress_entry == REDRESS-124`.

Generated provenance:

- Track 1 source path names the generated CSS L4 declaration-values source, not
  JSON, W1a, hand-only, or oracle code.
- Runtime path names `generated_css_l4_declaration_values::parser::parse`.
- Input provenance and `input_checksum` match the W1b CSS fixture checksum
  `cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374`.
- `input_bytes == 187`.
- `grammar_checksum` is a 64-hex checksum.
- `generated_loc > 0`, `generated_module_bytes > 0`, and
  `grammar_size_guard == pass:generated_loc<=360`.

Comparator and equality:

- Track 2/oracle source is the existing independent cssparser oracle and does
  not name generated Track 1 code.
- `track2_independence_status == independent_verified`.
- lightningcss evidence uses `lightningcss =1.0.0-alpha.71` and records the
  command/artifact that produced the measured comparator row.
- Direct cssparser calls are not part of the lightningcss comparator path.
- `strict_output_equality == pass`.
- `three_way_equality == pass:track1=cssparser=lightningcss`.
- `lightningcss_sequence_status` proves the source scanner sequence matches
  lightningcss AST declaration traversal before the fact stream is accepted.

Measurement:

- `track1_mbps`, `track2_or_oracle_mbps`, and `lightningcss_mbps` are finite
  positive values.
- `sample_count >= 30`.
- `sample_cost` records enough cost detail to reproduce Mbps derivation.
- Track 1, oracle, lightningcss, and benchmark artifact paths include or bind
  to the same `run_id`; stale W1b-1-only artifact identity fails.
- `threshold_mbps` is derived as `lightningcss_mbps + 1`, not trusted as an
  independent input.
- `admission_margin_mbps` is derived as `track1_mbps - threshold_mbps`.

Gate context:

- `host_triple`, `feature_mask`, and `build_flags` identify the same host and
  native build used by Criterion.
- `lock14_status` is passing.
- `lock16_status` is explicit; CSS scalar-only rows may use an `n/a` status
  only when no SIMD/ASM admission is claimed.
- `scalar_reference_status` and `checkasm_or_parity_status` prove scalar or
  parity coverage for the measured equality path.
- `json_guard_state` is either a no-behavior-drift proof or a refreshed
  guards-pass/demoted status from a populated accepted JSON guard root, not an
  empty CSS-only Criterion directory.
- Companion report validation must fail if combined with write/probe flags.

## Outcome Classification

The gate derives `admission_status`; the JSON must not be trusted as authority.

`PASS-ADMIT-CANDIDATE` requires all validation invariants plus:

```text
track1_mbps > lightningcss_mbps + 1
admission_margin_mbps > 0
verdict = GO
gate_status = pass
```

This is a CSS SOTA admission candidate for later SK-V12 close reconciliation.
It may move `skinny/RESULTS.md` only when the wave intentionally records an
actual CSS ADMIT surface and the gate consumes the RESULTS update.

`PASS-MEASURED-BASELINE` requires all non-admission validation invariants plus:

```text
track1_mbps <= lightningcss_mbps + 1
admission_margin_mbps <= 0
strict_output_equality = pass
three_way_equality = pass:track1=cssparser=lightningcss
```

This is a measured CSS miss, not ADMIT. It records REDRESS 124 evidence and
unlocks W3/W4 or later FIXPOINT reasoning, but it must not move
`skinny/RESULTS.md`. The miss stays in the report/REDRESS trail; RESULTS
remains unchanged unless there is a real CSS ADMIT surface or a JSON guard
demotion.

`BLOCKED/FAIL` covers missing comparator evidence, stale run identity, failed
equality, oracle coupling, missing generated-size/provenance fields, invalid
throughput, unconsumed telemetry, invalid JSON guard state, or mixed write/probe
gate invocation. A blocked or failed row needs REDRESS disposition and does not
move RESULTS.
