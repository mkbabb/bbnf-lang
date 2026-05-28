# SK-V16 W0-A2 Report Schema Consumers

Status: read-only research. No files edited, staged, or committed.

## Verdict

W0 should mirror the typed `report.rs` side-report pattern, not the SK-V15 xtask `serde_json::Value` pattern, for all new SK-V16 side reports. The `Value` validators are useful as gate plumbing precedent, but they do not intrinsically reject unknown producer-only fields.

## Contract Evidence

SK-V16 requires W0 to create the `SK-V16-open` baseline and report-consumer lock, with CSS old proof diagnostic only. Required W0 side flags are `--skv16-css-typed-report`, `--skv16-dirty-generated-report`, `--skv16-pattern-h-roundtrip-report`, and optional SIMD. Every emitted field must be gate-parsed and validated in the same wave. W0 tasks explicitly require missing or producer-only fields to reject.

P3-D states schema-v3 must stay visible, side reports must use typed JSON with unknown-field rejection, and producer-only fields reject.

## Existing Patterns To Mirror

Base schema-v3 structs already use `#[serde(deny_unknown_fields)]`: `Report`, `ComparatorSet`, `SkV8ComparatorEvidence`, `SkV8Telemetry`, `TelemetryRow`, and `ProbeReportRow`.

The strongest side-report model is typed top-level report plus typed row plus `from_json_str` plus `validate_gate`, all `deny_unknown_fields`. Examples include `SkV12CssL4SotaReport`, `SkV13CssStylesheetSelectorsReport`, and `SkV13CssComparatorOracleReport`.

The CSS 24-row validator precedent is `SkV13CssComparatorOracleReport::validate_gate`, which checks exact 24 feature rows, duplicate rows, row identity, and stale coverage totals.

The gate binary pattern is to import report types, extract companion path, run `from_json_str(...).and_then(validate_gate)`, then artifact/workspace validation, while requiring `--check-results` for admission-sensitive reports.

## Producer-Only Rejection Pattern

Mirror the existing unknown-field rejection tests:

- `w1a_non_json_report_rejects_unknown_producer_fields`
- `skv12_non_json_report_rejects_unknown_producer_fields`
- `skv12_css_l4_sota_report_rejects_unknown_producer_fields`
- `skv13_css_comparator_report_rejects_unknown_producer_fields`

SK-V16 should add equivalent tests for every new report and nested row. The SK-V15 xtask JSON validators require known fields but do not reject unknown keys by construction; do not use that style unless an explicit allowed-key audit is added.

## SK-V16 Integration Points

Add typed structs and validators in `skinny/crates/bbnf-bench/src/report.rs`:

- `SkV16CssTypedReport` / `SkV16CssTypedRow`: exact 24 CSS rows, `css_track1_typed_passes`, `css_cssparser_typed_passes`, `css_typed_summary_equal`, `css_provider_source`, `typed_materialization_invariant`, admitted count zero for W0, reject legacy generated/fact stream/full parse/FNV/broadcast sources as admission.
- `SkV16DirtyGeneratedReport` / `SkV16DirtyGeneratedEntry`: `dirty_generated_state`, `git_status_short`, broad command, owner, disposition, proof command; reject `dirty_unrouted` and missing manifest context.
- `SkV16PatternHRoundtripReport`: exact count 67, `-mindepth 2` census evidence, generator-owned roundtrip proof; reject header-only provenance.
- `SkV16NativeSimdReport`: `native_simd_status`; require scalar reference, parity/checkasm, same-wave consumer, cold aarch64 evidence when scoped; reject x86/AVX evidence.

Add flag plumbing in `skinny/crates/bbnf-bench/src/bin/gate.rs` and `is_companion_report_flag`. Add xtask pass-through flags in `skinny/xtask/src/main.rs`.

## Tests To Add

- `skv16_css_typed_report_rejects_unknown_producer_fields`
- `skv16_css_typed_report_rejects_old_css_admission_sources`
- `skv16_css_typed_report_rejects_false_equality_or_bad_counts`
- `skv16_dirty_generated_report_rejects_dirty_unrouted_or_missing_manifest`
- `skv16_pattern_h_roundtrip_report_rejects_non_67_or_header_only`
- `skv16_native_simd_report_rejects_x86_or_missing_scalar_checkasm_consumer`
- `gate_json_passthrough_accepts_skv16_report_flags`
- `skv16_report_flags_require_check_results`

Keep existing guard tests green, including schema-v3 render/reject tests, SK-V15 manifest tests, and opening-baseline tests.

## No Behavior Drift Evidence

Safe W0 write scope is limited to `skinny/crates/bbnf-bench/src/report.rs`, `skinny/crates/bbnf-bench/src/bin/gate.rs`, `skinny/xtask/src/main.rs`, and new SK-V16 W0 fixtures/docs. Do not stage the current unrelated dirty generated CSS/runtime files, prior SK-V12/SK-V13 CSS report JSON files, `docs/precepts`, or `skinny/crates/bbnf-bench/src/generated_real_typed.rs`.
