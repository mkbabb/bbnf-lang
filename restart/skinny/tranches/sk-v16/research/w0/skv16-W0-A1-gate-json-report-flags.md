# SK-V16 W0-A1 Gate JSON Report Flags

Status: read-only research. No files edited, staged, or committed.

## Verdict

Use the existing `bbnf-bench` companion-report architecture for SK-V16 W0, with `xtask` limited to passthrough validation. Do not model SK-V16 after the SK-V15 xtask-only early-return validators: W0 has three mandatory report consumers plus one conditional native SIMD consumer, and it needs combined read-only gate consumption.

## Authority

W0 is the first legal SK-V16 implementation wave and is gate/report only. It must create `SK-V16-open`, bind report consumers, prove missing/producer-only fields reject, keep JSON 51 strict, keep CSS 0/24 admitted, and show no parser/runtime/codegen drift.

Required W0 report flags are exact:

- `--skv16-css-typed-report <path>`
- `--skv16-dirty-generated-report <path>`
- `--skv16-pattern-h-roundtrip-report <path>`
- `--skv16-native-simd-report <path>` only when SIMD is scoped

Native SIMD remains conditional, aarch64-only, and must have fresh S-P1 hot leaf, scalar reference, strict checkasm/parity, same-wave consumer, and cold measurement before admission.

## Current Accepted Pattern

`cargo xtask gate-json` first validates passthrough, validates the current RESULTS snapshot when `--check-results` is present, then spawns `cargo run -p bbnf-bench --bin gate -- ...`.

`xtask` already whitelists simple flags and path-valued report flags, rejects missing path arguments, and rejects unsupported arguments.

The bench gate companion-report helper is the better W0 model: it permits multiple read-only report flags in one command, rejects duplicate or missing path arguments, and rejects `--update-results`, `--write-results`, and `--include-volatile-probes`.

`report.rs` already uses typed serde structs with `deny_unknown_fields`, and existing fixtures reject producer-only fields. SK-V16 should extend that pattern.

## Recommended Implementation Shape

`skinny/xtask/src/main.rs`:

- Add the four SK-V16 flags to `validate_gate_json_passthrough`.
- Add passthrough unit tests for each flag, combined mandatory flags, missing path, duplicate path, and unknown flag.
- Do not add SK-V16 xtask early-return validators unless the implementation validates all supplied SK-V16 reports together.

`skinny/crates/bbnf-bench/src/bin/gate.rs`:

- Add helper functions for the four SK-V16 report paths.
- Add all four flags to `is_companion_report_flag`.
- For each supplied SK-V16 report, require `--check-results`, parse typed report JSON, run `validate_gate`, print a specific `G-SK-V16-W0-* PASS` line, and then continue into the normal JSON check.
- Keep `--skv16-native-simd-report` optional. If supplied, it must validate; if absent, W0 must not imply native SIMD is scoped.

`skinny/crates/bbnf-bench/src/report.rs`:

- Add typed SK-V16 report structs with `#[serde(deny_unknown_fields)]`.
- Reuse the existing 24 CSS feature canon where possible.
- Validators must consume the SPEC fields: `css_track1_typed_passes`, `css_cssparser_typed_passes`, `css_typed_summary_equal`, `css_provider_source`, `dirty_generated_state`, `native_simd_status`, and `typed_materialization_invariant`.

## Required Negative Fixtures

CSS typed report must reject missing required fields, unknown producer-only fields, fewer or more than 24 CSS rows, legacy generated/fact-stream/W8R/FNV/stale-sidecar admission sources, `css_typed_summary_equal=false` with any admit/SOTA status, and wrong-plane comparators.

Dirty-generated report must reject missing exact dirty manifest, unowned generated files, `dirty_generated_state=accepted`, failed broad command without intrinsic-block proof, and any stale sidecar treated as admission evidence.

Pattern H roundtrip report must reject count other than 67, header-only provenance, missing generator-owned roundtrip proof, wrong census command, and any byte-diff not explicitly admitted or intrinsically blocked.

Native SIMD report must reject x86/AVX scope, missing scalar reference, missing strict checkasm/parity, missing same-wave consumer, missing cold measurement, stale S-P1 hot leaf, and orphan/native-only kernels.

## Falsifiability Gates

Minimum W0 redress commands:

```sh
cargo test -p bbnf-bench --bin gate skv16_report_flags -- --nocapture
cargo test -p bbnf-bench --lib skv16_report -- --nocapture
cargo xtask gate-json --check-results
cargo xtask gate-json --check-results \
  --skv16-css-typed-report <pass-css-typed-fixture.json> \
  --skv16-dirty-generated-report <pass-dirty-generated-fixture.json> \
  --skv16-pattern-h-roundtrip-report <pass-pattern-h-fixture.json>
```

Conditional native SIMD gate, only when scoped:

```sh
cargo xtask gate-json --check-results \
  --skv16-native-simd-report <pass-native-simd-fixture.json>
```

No behavior drift proof must show redress touched only gate/report/test/doc surfaces, with no parser/runtime/codegen changes.
