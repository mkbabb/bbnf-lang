# SK-V16 W0 Plan: Report Consumer Baseline

Date: 2026-05-28.
Scope: W0 plan-only artifact. No source, generated output, runtime,
provider, SIMD, `RESULTS.md`, or `REDRESS.md` files are edited by this plan.
Output: this file.

## Inputs

- W0 authority: `restart/skinny/tranches/sk-v16/SPEC.md` Section 3 and
  `restart/skinny/tranches/sk-v16/DISPATCH-PROMPT.md` W0.
- Research cohort:
  - `skv16-W0-A1-gate-json-report-flags.md`
  - `skv16-W0-A2-report-schema-consumers.md`
  - `skv16-W0-A3-css-typed-report.md`
  - `skv16-W0-A4-dirty-generated-report.md`
  - `skv16-W0-A5-pattern-h-roundtrip-report.md`
  - `skv16-W0-A6-native-simd-and-baseline.md`
- Plan-review REVISE set from A1-A6:
  - `xtask` owns passthrough and arity only.
  - `bbnf-bench` gate owns `--check-results` enforcement and multi-report
    composition.
  - `report.rs` owns typed `deny_unknown_fields` schemas and validation.
  - Dirty-generated state is inherited and manifested in W0; W1 owns
    disposition.
  - Pattern H rejects the `-maxdepth 2` trap and header-only proof.
  - Native SIMD must be explicitly `not_in_scope` in W0, not silently absent.
- Current measured ledger: `skinny/RESULTS.md` and `skinny/REDRESS.md`.

## Intervention

W0 adds fail-closed report consumers for the four SK-V16 report classes and
records a no-behavior baseline. It does not implement CSS typed APIs, dispose
dirty generated files, collapse Pattern H, or scope native SIMD.

1. Add typed SK-V16 side-report schemas in `report.rs`:
   `SkV16CssTypedReport`, `SkV16CssTypedRow`,
   `SkV16DirtyGeneratedReport`, `SkV16DirtyGeneratedEntry`,
   `SkV16ExcludedDirtyEntry`, `SkV16DirtyBroadCommand`,
   `SkV16PatternHRoundtripReport`, and `SkV16NativeSimdReport`.
   Every top-level and nested struct uses `#[serde(deny_unknown_fields)]`.
   No `flatten`, `extra`, `Map`, or `BTreeMap<String, Value>` escape hatch is
   allowed.
2. Add bench gate companion flags:
   `--skv16-css-typed-report`, `--skv16-dirty-generated-report`,
   `--skv16-pattern-h-roundtrip-report`, and
   `--skv16-native-simd-report`.
   The bench gate requires `--check-results` for every supplied SK-V16 report,
   rejects write/probe combinations, validates every supplied typed report,
   prints a specific `G-SK-V16-W0-* PASS` line, and then continues into the
   normal JSON results check.
3. Add xtask passthrough/arity for the same four flags. Xtask must not perform
   independent SK-V16 schema validation.
4. Add W0 fixture JSON files under `restart/skinny/tranches/sk-v16/research/w0/fixtures/`
   for the admitting open/non-scope reports. Negative fixtures live in Rust
   tests, not as self-attesting proof fields.
5. Add `skv16-W0-redress.md` recording commands, touched files, no-behavior
   evidence, and W0 disposition.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
  - Define the typed SK-V16 report schemas and `validate_gate` methods.
  - Add focused unit tests for accepting W0-open reports and rejecting
    missing, unknown, or producer-only fields.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
  - Add report path helpers and companion flag registration.
  - Require `--check-results` for SK-V16 report flags.
  - Validate all supplied SK-V16 reports without early-returning before the
    normal JSON gate.
- `skinny/xtask/src/main.rs`
  - Add passthrough/arity acceptance for the four SK-V16 flags.
  - Keep SK-V16 schema validation out of xtask.
- `restart/skinny/tranches/sk-v16/research/w0/fixtures/*.json`
  - Positive W0 fixture reports consumed by `cargo xtask gate-json`.
- `restart/skinny/tranches/sk-v16/research/w0/skv16-W0-redress.md`
  - Redress evidence and disposition.

Protected in W0:

- `crates/core/src/runtime/**`
- `skinny/crates/runtime/**`
- `skinny/crates/codegen/**`
- `skinny/crates/bbnf-simd/**`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- inherited dirty SK-V12/SK-V13 CSS JSON reports
- `docs/precepts`

These can be read for evidence but not staged or modified by W0 redress.

## CSS Typed Report Contract

W0 accepts only a fail-closed open CSS report:

- `schema_version=sk-v16-css-typed-report-v1`
- `wave_id=SK-V16-W0`
- `css_admitted_row_count=0`
- `css_open_row_count=24`
- exactly 24 unique CSS L4 row IDs
- each row has `admission_status=OPEN`
- each row has `typed_api_status=not_built:w5`
- each row has `css_track1_typed_passes=0`
- each row has `css_cssparser_typed_passes=0`
- each row has `css_track1_typed_errors=0`
- each row has `css_cssparser_typed_errors=0`
- each row has `css_typed_summary_equal=false`
- each row has `css_provider_source=not_built:w4`
- each row has `typed_materialization_invariant=not_built:w5`
- pre-W6 speed, threshold, and SOTA fields are absent

Reject any CSS row with admission, nonzero typed counts, true typed equality,
speed fields usable for admission, SOTA/threshold language, fact-stream or
full-parse planes, `CSS_GENERATED_RS`, `runtime_generator.rs`, generated CSS
runtime files as provider proof, FNV/checksum/hash-only equality, lightningcss
typed comparator claims, W8R broadcast provenance, or
`sk-v15-W0:broadcast-diagnostic` as admission evidence.

## Dirty Generated Report Contract

W0 accepts inherited dirty generated state only when it is exact,
manifested, and routed to W1:

- `dirty_generated_state=inherited_and_manifested`
- exactly eight generated paths:
  - `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
  - `skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs`
  - `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
  - `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs`
  - `skinny/crates/runtime/src/grammars/css_l4_nested_layout/generated.rs`
  - `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs`
  - `skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs`
  - `skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs`
- unrelated dirty paths appear only under `excluded_dirty_state`
- broad commands record current expected failures and owner path
- each generated row has `disposition=w1_pending`

Reject `dirty_unrouted`, `accepted`, free-form "clean" language, missing broad
commands, missing owners, unrelated dirty paths in `generated_manifest`, and
self-attesting proof fields such as `producer_only_rejection_proof`.

W1, not W0, chooses clean regen, retirement, or intrinsic block per file.

## Pattern H Report Contract

W0 accepts Pattern H only as exact census plus roundtrip proof:

- `schema_version=sk-v16-pattern-h-roundtrip-v1`
- `wave_id=SK-V16-W0`
- exact census command:
  `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`
- `pattern_h_count=67`
- forbidden maxdepth command present as a trap:
  `find crates/core/src/runtime -mindepth 2 -maxdepth 2 -type f -name '*.rs' | wc -l`
- `forbidden_maxdepth_count=63`
- exact four depth-3 Google Sheets document paths
- `line1_provenance_count=67`
- `roundtrip_status=pass`
- `generator_owner_status=roundtrip_proven`
- `header_only_proof=false`
- `generated_source_edits_in_w0=false`

Reject the wrong command, the maxdepth trap as proof, missing depth-3 paths,
`roundtrip_status=not_run_in_readonly_research`, failed roundtrip, header-only
proof, generated source edits in W0, and unknown producer-only fields.

## Native SIMD Report Contract

W0 requires explicit native non-scope evidence:

- `schema_version=sk-v16-native-simd-report-v1`
- `wave_id=SK-V16-W0`
- `native_simd_status=not_in_scope`
- `native_simd_scope_reason=W0 is report/gate/no-behavior baseline only`
- `host_arch=aarch64`
- `x86_touched=false`
- `avx_evidence_cited=false`
- scalar/checkasm/same-wave/cold fields are absent for W0 non-scope

If a future scoped status appears, the validator requires the full tuple:
fresh S-P1 hot leaf, S-P2 survivor artifact, scalar reference, strict
checkasm/parity, same-wave production consumer, cold measurement, aarch64
host, Lock 14/16 pass, and zero orphan kernels. W0 rejects wildcard wave IDs,
`wired_and_measured` placeholder strings, x86/AVX admission evidence, missing
scalar/checkasm/same-wave/cold tuple members, and orphan/native-only kernels.

## Required Tests

`skinny/xtask/src/main.rs`:

- `gate_json_passthrough_accepts_skv16_report_flags`
- `gate_json_passthrough_accepts_skv16_css_typed_report_flag`
- `gate_json_passthrough_accepts_skv16_dirty_generated_report`
- `gate_json_passthrough_accepts_skv16_pattern_h_roundtrip_report_flag`
- `gate_json_passthrough_accepts_skv16_native_simd_report_flag`
- `gate_json_passthrough_rejects_skv16_report_flag_missing_path`
- `gate_json_passthrough_rejects_unknown_skv16_report_flag`

`skinny/crates/bbnf-bench/src/bin/gate.rs`:

- `skv16_report_arg_allows_multiple_read_only_reports`
- `skv16_report_arg_rejects_write_probe_and_flag_paths`
- `skv16_report_flags_require_check_results`
- `skv16_pattern_h_roundtrip_report_arg_allows_json_check_only`
- `skv16_native_simd_report_arg_is_optional_but_validates_when_present`

`skinny/crates/bbnf-bench/src/report.rs`:

- `skv16_css_typed_report_accepts_open_non_admission_surface`
- `skv16_css_typed_report_rejects_unknown_producer_fields`
- `skv16_css_typed_report_rejects_missing_required_fields`
- `skv16_css_typed_report_rejects_non_24_or_duplicate_rows`
- `skv16_css_typed_report_rejects_false_equality_or_bad_counts`
- `skv16_css_typed_report_rejects_fact_stream_or_full_parse_plane`
- `skv16_css_typed_report_rejects_css_generated_rs_provider`
- `skv16_css_typed_report_rejects_fnv_or_checksum_only_equality`
- `skv16_css_typed_report_rejects_lightningcss_or_w8r_broadcast_comparator`
- `skv16_css_typed_report_rejects_pre_w6_speed_fields`
- `skv16_dirty_generated_report_accepts_exact_manifest_with_routed_failures`
- `skv16_dirty_generated_report_accepts_inherited_manifest_without_w0_behavior_drift`
- `skv16_dirty_generated_report_rejects_missing_manifest_path`
- `skv16_dirty_generated_report_rejects_unrelated_dirty_as_generated`
- `skv16_dirty_generated_report_rejects_unknown_producer_fields`
- `skv16_dirty_generated_report_rejects_dirty_unrouted_or_accepted_state`
- `skv16_dirty_generated_report_rejects_missing_broad_command_or_owner`
- `skv16_dirty_generated_report_rejects_self_attesting_proof_field`
- `skv16_dirty_generated_report_rejects_unmanifested_runtime_codegen_diff`
- `skv16_pattern_h_roundtrip_report_accepts_exact_67_generator_roundtrip`
- `skv16_pattern_h_roundtrip_report_rejects_unknown_producer_fields`
- `skv16_pattern_h_roundtrip_report_rejects_wrong_census_command`
- `skv16_pattern_h_roundtrip_report_rejects_maxdepth_trap`
- `skv16_pattern_h_roundtrip_report_rejects_missing_depth3_paths`
- `skv16_pattern_h_roundtrip_report_rejects_header_only_without_roundtrip`
- `skv16_pattern_h_roundtrip_report_rejects_roundtrip_not_run_or_failed`
- `skv16_pattern_h_roundtrip_report_rejects_generated_source_edits_in_w0`
- `skv16_native_simd_report_accepts_w0_not_in_scope`
- `skv16_w0_baseline_requires_native_simd_not_in_scope`
- `skv16_native_simd_report_rejects_unknown_producer_fields`
- `skv16_native_simd_report_rejects_x86_or_avx_evidence`
- `skv16_native_simd_report_rejects_wildcard_wave_id_or_wired_status`
- `skv16_native_simd_report_rejects_missing_scalar_reference`
- `skv16_native_simd_report_rejects_missing_strict_checkasm`
- `skv16_native_simd_report_rejects_missing_same_wave_consumer`
- `skv16_native_simd_report_rejects_missing_cold_measurement`
- `skv16_native_simd_report_rejects_orphan_kernel`
- `skv16_native_simd_report_rejects_x86_admission_evidence_but_allows_rejection_policy_text`

## Falsifiability Gate

Run from `skinny` unless noted:

```sh
cargo test -p xtask gate_json_passthrough_accepts_skv16_report_flags -- --nocapture
cargo test -p bbnf-bench --bin gate skv16_report -- --nocapture
cargo test -p bbnf-bench --lib skv16_ -- --nocapture
cargo xtask gate-json --check-results
cargo xtask gate-json --check-results \
  --skv16-css-typed-report ../restart/skinny/tranches/sk-v16/research/w0/fixtures/skv16-css-typed-open.json \
  --skv16-dirty-generated-report ../restart/skinny/tranches/sk-v16/research/w0/fixtures/skv16-dirty-generated-inherited.json \
  --skv16-pattern-h-roundtrip-report ../restart/skinny/tranches/sk-v16/research/w0/fixtures/skv16-pattern-h-roundtrip.json \
  --skv16-native-simd-report ../restart/skinny/tranches/sk-v16/research/w0/fixtures/skv16-native-simd-not-in-scope.json
```

No-behavior-drift checks from repo root:

```sh
git diff --name-only -- crates/core/src/runtime skinny/crates/runtime/src/grammars
git status --short -- crates/core/src/runtime skinny/crates/runtime/src/grammars
git diff --name-only -- skinny/crates/codegen skinny/crates/bbnf-simd skinny/crates/bbnf-bench/src/generated_real_typed.rs
git diff --name-only
```

The first command may show inherited dirty generated files under
`skinny/crates/runtime/src/grammars`; the redress artifact must compare against
the entry manifest and prove W0 did not stage or modify protected behavior
surfaces. No x86/AVX proof may be a raw text scan over research docs because
policy text intentionally mentions those tokens.

## Revert Protocol

Revert the W0 redress slice if any of these occur:

- A SK-V16 report flag can pass without `--check-results`.
- A supplied SK-V16 report bypasses the normal JSON results check.
- Unknown or producer-only fields parse successfully.
- CSS can admit before W5/W6 typed equality and speed gates.
- Dirty generated files are cleaned, regenerated, retired, or staged in W0.
- Pattern H accepts the `-maxdepth 2` trap or header-only proof.
- Native SIMD is implied by absent report data or x86/AVX evidence.
- Any parser, runtime, codegen, SIMD, or generated behavior path changes in
  the W0 redress diff.

Rollback scope is limited to:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- W0 fixtures and redress artifact

Do not revert unrelated dirty files, inherited generated files, SK-V12/SK-V13
research JSON, or `docs/precepts`.

## Same-Wave Consumer

Same-wave consumer is `cargo xtask gate-json --check-results` from the skinny
workspace with all four W0 SK-V16 report fixtures supplied. Rendering or
emitting report JSON is not sufficient unless the bench gate parses every
field, validates the typed schemas, and continues through the JSON results
gate.

DISPOSITION: PLAN-ACCEPT. Redress is authorized only for the owner paths,
fixtures, and falsifiability gates above.
