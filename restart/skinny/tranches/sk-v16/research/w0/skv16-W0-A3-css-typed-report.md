# SK-V16 W0-A3 CSS Typed Report Surface

Status: read-only research. No files edited, staged, or committed.

## Verdict

Current CSS proof is legacy diagnostic only. W0 can add and validate a fail-closed SK-V16 CSS typed report consumer surface, but it cannot honestly validate typed equality, cssparser same-typed-workload speed, or CSS admission until W4-W6 build the grammar-derived provider and typed API.

## Authority

SK-V16 requires CSS provider proof from `grammar/css/l4/*.bbnf`, and explicitly quarantines `CSS_GENERATED_RS`, fact streams, brace summaries, FNV metadata, stale sidecars, and W8R broadcast rows as diagnostic only. Typed equality must precede speed, and cssparser same typed workload must precede admission.

W0's dispatch scope is report consumers and negative fixtures, not typed API construction.

## Current CSS Proof Paths

The benchmark entrypoint is `bench_nonjson_css_l4` in `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`. It reads fixture inputs, asserts Track 1 equality against cssparser/golden and lightningcss fact streams, writes legacy reports, calls `validate_gate`, then times fact-stream lanes in Criterion groups.

Current Track 1 CSS functions all return fact-stream text through generated runtime parser modules, not typed documents. cssparser hooks emit the same fact text for some fixtures. lightningcss is a source-sidecar/fact projection, not a cssparser same-typed-workload comparator.

Legacy equality functions compare strings. Legacy report producers write `track1-facts.txt`, `oracle-facts.txt`, and `lightningcss-facts.txt`, then report `output_plane` as fact stream and `measured_validation_path` as byte-identical fact stream.

Current report schemas are legacy SK-V12/SK-V13 CSS schemas with `deny_unknown_fields`, not SK-V16 typed schema. The 24 CSS feature rows are already enumerated in `report.rs`.

Gate wiring accepts legacy flags through xtask and bench gate only. `--skv16-css-typed-report` is not currently in `validate_gate_json_passthrough` or in the bench gate companion flags.

## W0 Can Validate Now

W0 can honestly validate only the report-consumer contract:

- `--skv16-css-typed-report <path>` is accepted only with `--check-results`.
- The report schema is parsed with `deny_unknown_fields`.
- All 24 CSS row IDs are present and unique.
- Every row is `admission_status=OPEN` or `typed_api_status=not_built:w5`.
- Required SK-V16 fields exist: `css_track1_typed_passes`, `css_cssparser_typed_passes`, `css_typed_summary_equal`, `css_provider_source`, `dirty_generated_state`, `typed_materialization_invariant`.
- Legacy proof sources are rejected for typed admission.
- CSS remains 0/24 admitted and JSON 51 guard still passes.

W0 cannot validate typed equality, typed pass/error counts beyond "not built", same-workload cssparser speed, or grammar-derived provider source beyond a fail-closed placeholder.

## Future CSS Typed Report Fields

Proposed top-level schema: `sk-v16-css-typed-report-v1`.

Required top-level fields: `schema_version`, `wave_id`, `run_id`, `host_triple`, `feature_mask`, `build_flags`, `dirty_generated_state`, `json_guard_state`, `css_admitted_row_count`, `css_open_row_count`, `rows`.

Required per-row fields:

- Identity: `row_id`, `grammar_id=css_l4`, `workload=direct_to_struct`, `corpus_or_workload`, `output_plane=css_l4_typed_summary`.
- Provider: `css_provider_source`, `provider_artifact_sha256`, `provider_derivation_status`.
- Typed surface: `typed_document_api`, `typed_value_api`, `typed_view_api`, `typed_visitor_api`, `typed_materialization_invariant`.
- Comparator: `cssparser_entrypoint`, `cssparser_workload=css_l4_typed_summary`, `comparator_plane=css_l4_typed_summary`, `comparator_strictness=strict`, `comparator_freshness=same-run-native`.
- Equality: `css_track1_typed_passes`, `css_cssparser_typed_passes`, `css_track1_typed_errors`, `css_cssparser_typed_errors`, `css_typed_summary_equal`, `track1_typed_summary_sha256`, `cssparser_typed_summary_sha256`.
- Speed, W6 only: `track1_typed_mbps`, `cssparser_typed_mbps`, `threshold_mbps=cssparser_typed_mbps+1.000`, `admission_margin_mbps`.
- Gate: `admission_status`, `redress_entry`, `same_wave_consumer_class`, `legacy_source_quarantine`, `gate_status`.

For W0-open, typed fields should be present but non-admitting: `css_track1_typed_passes=0`, `css_cssparser_typed_passes=0`, `css_typed_summary_equal=false`, `css_provider_source=not_built:w4`, `typed_materialization_invariant=not_built:w5`, `admission_status=OPEN`.

## Negative Legacy Checks

Required W0 negative fixtures:

- Reject `output_plane` containing `fact_stream`, `full_parse_summary`, or `css_l4_full_parse`.
- Reject `css_provider_source` or generated Track 1 source path pointing at `CSS_GENERATED_RS`, `runtime_generator.rs`, or `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`.
- Reject typed-summary hash equality when typed summaries are absent or equality is hash-only.
- Reject `input_fnv64`, `stream_fnv64`, checksum/FNV-only, or `fact_stream_sha256` as typed equality proof.
- Reject `lightningcss` as the CSS typed comparator; W6 requires cssparser same typed workload.
- Reject W8R broadcast provenance, `SK-V14-W8R-css-l4-full-parse`, and `sk-v15-W0:broadcast-diagnostic`.
- Reject any `admission_status` other than `OPEN` before W5 equality and W6 typed-speed threshold.

## Suggested W0 Consumer Tests

- `skv16_css_typed_report_accepts_open_non_admission_surface`
- `skv16_css_typed_report_rejects_fact_stream_or_full_parse_plane`
- `skv16_css_typed_report_rejects_css_generated_rs_provider`
- `skv16_css_typed_report_rejects_fnv_or_checksum_only_equality`
- `skv16_css_typed_report_rejects_lightningcss_or_w8r_broadcast_comparator`
- `skv16_css_typed_report_rejects_admission_before_typed_equality`
- `gate_json_passthrough_accepts_skv16_css_typed_report_flag`
- `gate_json_passthrough_rejects_skv16_css_typed_without_check_results`
