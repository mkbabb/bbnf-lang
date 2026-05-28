# SK-V16 W0-A6 Native SIMD And Baseline

Status: read-only research. No files edited, staged, or committed.

## Conclusion

W0 must treat native SIMD as `native_simd_status=not_in_scope` for the opening baseline, while authoring the executable consumer and negative fixtures needed for later scoped SIMD proof. W0 cannot cite existing `primitive-checkasm`, existing aarch64 code, or existing x86/AVX files as admission proof. The current code does not yet implement the `--skv16-*` gate flags, so W0 must add the consumer before any SK-V16 report class can close.

## Current Workspace Observation

HEAD during research: `acb1a3241`.

`git diff --name-only` shows existing dirty generated/docs state, including `docs/precepts`, SK-V12/SK-V13 CSS oracle JSON files, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, and seven CSS generated runtime files. The relevant W0 code subset scan found no current SK-V16/native/x86 keywords in diffs under `skinny/crates/bbnf-simd`, `skinny/xtask/src`, or `skinny/crates/bbnf-bench/src`.

This dirty state must be recorded as inherited input, not hidden and not converted into W0 success.

## Required `native_simd_status` Shape

Allowed values, matching S-P3 telemetry: `not_in_scope`, `profile_first_scalar_ref_checkasm_same_wave`, `blocked`.

W0 baseline value:

```json
{
  "native_simd_status": "not_in_scope",
  "native_simd_scope_reason": "W0 is report/gate/no-behavior baseline only",
  "native_simd_report_required": false,
  "native_simd_report_consumer_status": "must_exist_or_be_negative-fixtured-before-use"
}
```

If any native report is scoped later, the report must use unknown-field rejection and include:

```json
{
  "schema_version": "sk-v16-native-simd-report-v1",
  "wave_id": "SK-V16-W10-or-scoped-wave",
  "native_simd_status": "profile_first_scalar_ref_checkasm_same_wave",
  "host_arch": "aarch64",
  "host_policy": "Apple M5 Max / aarch64 only",
  "selected_primitive": "...",
  "s_p1_hot_leaf_artifact": "...",
  "s_p2_survivor_artifact": "...",
  "scalar_reference_path": "...",
  "scalar_reference_status": "pass",
  "checkasm_command": "BBNF_SIMD_STRICT=1 cargo xtask primitive-checkasm",
  "checkasm_status": "pass",
  "same_wave_consumer_path": "...",
  "same_wave_consumer_status": "wired_and_measured",
  "cold_measurement_row_ids": ["..."],
  "x86_touched": false,
  "avx_evidence_cited": false,
  "lock14_status": "pass",
  "lock16_status": "pass",
  "orphan_count_after": 0
}
```

Reject if any production claim lacks the scalar/checkasm/same-wave tuple, or if x86/AVX evidence appears.

## No-X86/AVX Proof

W0 redress must record these commands after W0 edits:

```sh
git diff --name-only -- skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86
git diff -- skinny/crates/bbnf-simd skinny/xtask/src skinny/crates/bbnf-bench/src | rg -n 'x86|x86_64|AVX|avx|PEXT|pdep|target_feature|_mm|ymm|zmm'
rg -n 'x86|x86_64|AVX|avx|PEXT|pdep|target_feature|_mm|ymm|zmm' restart/skinny/tranches/sk-v16/research
```

Expected W0 result: no W0 diff touches x86 paths, and no W0 evidence cites x86/AVX as SK-V16 proof. Existing x86 modules are diagnostic context only.

## Scalar/Checkasm/Same-Wave Tuple

`cargo xtask primitive-checkasm` exists and runs strict bbnf-simd checkasm tests with `BBNF_SIMD_STRICT=1`. That is necessary but not sufficient.

A native SIMD claim is admissible only when all are present in the same scoped wave:

- fresh S-P1 hot leaf tied to the selected primitive;
- scalar reference path and passing scalar oracle;
- strict checkasm/parity artifact;
- same-wave production consumer, not producer-only source;
- cold per-parse row measurement;
- no x86/AVX touched or cited;
- Lock 14/16 pass over the touched roots.

W0 should not select a native primitive. Its valid tuple state is `not_applicable:not_scoped_in_w0`.

## W0 No-Behavior Drift Checks

W0 is a report/gate baseline wave. Required W0 redress evidence:

```sh
git status --short
git diff --name-only
git diff --stat -- skinny/crates/runtime skinny/crates/codegen skinny/crates/bbnf-simd skinny/crates/bbnf-bench/src/generated_real_typed.rs
(cd skinny && cargo xtask gate-json --check-results)
```

Record:

```json
{
  "no_behavior_drift": {
    "parser_runtime_codegen_behavior_touched": false,
    "bbnf_simd_behavior_touched": false,
    "generated_runtime_touched_by_w0": false,
    "dirty_generated_state": "inherited_and_manifested",
    "json_51_guard": "must_remain_admitted",
    "css_24_status": "open_not_admitted"
  }
}
```

Any parser/runtime/codegen/native-kernel behavior diff in W0 routes to `blocked`, unless it is explicitly reverted before redress.

## Lock 14/16 Interaction

Current `lock14_baseline.rs` allowlists generic and telemetry surfaces, including `crates/bbnf-simd/src/lib.rs`, but W2 is the wave tasked with broader primitive/checkasm scan expansion. Therefore W0 must not claim full native Lock 16 coverage from the current Lock 14 baseline alone.

W0 acceptable stance:

```json
{
  "lock14_status": "required_for_w0_report_consumer_paths",
  "lock16_status": "not_applicable:no_native_candidate_scoped_w0",
  "native_scan_status": "deferred_to_scoped_native_wave_or_W2_expansion",
  "gate_exclusion_report": "must list roots, exclusions, owner, consumer, affected rows, disposition"
}
```

## W0 Disposition Rules

ADMIT W0 baseline only if SK-V16 report consumers/fixtures exist, JSON 51 remains guarded, CSS 24 remains open, dirty generated state is manifested, `native_simd_status=not_in_scope`, and no W0 behavior drift is present.

REJECT if W0 emits producer-only SK-V16 fields, accepts unknown fields, claims native SIMD without the full tuple, cites x86/AVX evidence, or treats existing `primitive-checkasm` as production proof.

BLOCK if the inherited dirty generated state cannot be separated from W0 edits, or if the SK-V16 gate flags remain unimplemented while W0 redress tries to use them as close evidence.
