# SK-V8 W0 Lock 14 Baseline Allowlist Research

## Scope

Research file for W0 research agent D. This covers only the Lock 14 baseline allowlist and no-behavior-change gate needed by SPEC Section 2.1 and Section 3. It proposes storage and validation inside W0 owner paths without touching parser, scanner, SIMD, asm, codegen behavior, product-plane behavior, generated parser output, `RESULTS`, `REDRESS`, `SPEC`, or `HANDOFF`.

## Current Surface

SPEC Section 2.1 allows JSON-specific material only in grammar inputs, generated JSON output, per-grammar providers/templates, tests, and host/API schema facts. SPEC Section 3 makes W0 telemetry-only: capture `SK-V8-open`, add required telemetry, validate sidecar freshness/malformed manifests, create the Lock 14 baseline allowlist, and close only if every current main row is schema-bound and throughput remains within +/-1.0 percent of `SK-V8-open`.

Existing code already has useful gate hooks:

- `skinny/crates/bbnf-bench/src/gate.rs` validates row metadata presence and classifies invalid schema as `J`.
- `skinny/crates/bbnf-bench/src/report.rs` validates the rendered schema-v3 table fields and rejects missing required report values.
- `skinny/crates/bbnf-bench/src/bin/gate.rs` reads Criterion metadata, writes `skinny/RESULTS.md`, validates schema-v3, and emits same-run strict/lossy/sidecar notes.
- `skinny/xtask/src/main.rs` exposes `check-json`, `check-real-typed`, `check-conformance`, `bench-json`, and `gate-json`.

Gap for W0: there is no dedicated Lock 14 baseline allowlist consumed by `gate-json`, and current sidecar comparator values in `skinny/crates/bbnf-bench/src/bin/gate.rs` are hard-coded planning signals rather than manifest-validated strict anchors.

## Minimal Telemetry-Only Allowlist

The allowlist should be a classification manifest, not a write permit. W0 may store the manifest and validator in W0 owner paths, but the listed parser/codegen/runtime files remain read-only for W0 unless a later behavior wave owns them.

| Class | Exact files / rows | W0 validation |
|---|---|---|
| Grammar input | `skinny/grammars/json.bbnf` | Hash/path recorded as JSON grammar input. No W0 edit. `cargo xtask check-json` must still regenerate byte-identical runtime output. |
| Fixture inputs | `skinny/crates/test-fixtures/corpus/json/manifest.toml`; `skinny/crates/test-fixtures/corpus/json/twitter.json`; `skinny/crates/test-fixtures/corpus/json/citm_catalog.json`; `skinny/crates/test-fixtures/corpus/json/canada.json`; `skinny/test_data/apache_builds.json`; `skinny/test_data/github_events.json`; `skinny/test_data/update-center.json`; `skinny/test_data/mesh.json`; `skinny/test_data/random.json`; `skinny/test_data/gsoc-2018.json`; `skinny/test_data/marine_ik.json`; `skinny/test_data/instruments.json`; `skinny/test_data/numbers.json`; `skinny/test_data/unicode_mixed.json`; `skinny/test_data/unicode_escapes.json`; `skinny/test_data/unicode_basic.json`; `skinny/test_data/distinct_values.json`; `skinny/test_data/y_string_unicode.json` | Manifest order and hashes must match `test-fixtures::CANONICAL_JSON_FIXTURES`. No fixture edits in W0. All 17 fixtures produce `parse_only` and `direct_to_struct` rows; `twitter`, `update_center`, `mesh`, and `marine_ik` also produce `real_typed_struct` rows, for 38 main rows total. |
| Embedded conformance fixtures | `skinny/crates/test-fixtures/src/lib.rs` | Allowed as test fixture facts only. `cargo xtask check-conformance` must pass; no parser behavior change may be inferred from embedded fixture edits in W0. |
| Generated JSON runtime output | `skinny/crates/runtime/src/grammars/json/generated.rs`; `host.rs`; `mod.rs`; `parser.rs`; `scan.rs`; `sink.rs`; `value.rs`; `view.rs`; `visitor.rs` under `skinny/crates/runtime/src/grammars/json/` | Read-only in W0. `cargo xtask check-json` must pass, and `git diff --exit-code -- skinny/crates/runtime/src/grammars/json` must remain clean after W0 telemetry edits. |
| Generated typed output | `skinny/crates/bbnf-bench/src/generated_real_typed.rs` | Read-only in W0. `cargo xtask check-real-typed` must pass, and generated file diff must be zero. |
| Per-grammar templates/providers | `skinny/crates/codegen/src/json_templates/generated.rs`; `parser.rs`; `value.rs`; `view.rs`; `visitor.rs`; existing provider functions in `skinny/crates/codegen/src/lib.rs` that include JSON templates and enforce the `json` runtime profile | Allowed as existing JSON-specific template/provider surfaces only. No W0 edit. Lock 14 scan should not treat these exact paths as generic JSON leakage, but any new generic branch outside the allowlist rejects. |
| Runtime template inputs reused by codegen | `skinny/crates/runtime/src/grammars/json/scan.rs`; `skinny/crates/runtime/src/grammars/json/sink.rs` | Read-only in W0 and covered by both generated-output zero-diff and `check-json`. |
| Bench tests and metadata schema | `skinny/crates/bbnf-bench/src/metadata.rs`; `skinny/crates/bbnf-bench/src/report.rs`; `skinny/crates/bbnf-bench/src/gate.rs`; `skinny/crates/bbnf-bench/src/bin/gate.rs`; `skinny/crates/bbnf-bench/benches/json_parity.rs`; `skinny/crates/bbnf-bench/benches/simd_scan.rs` | W0 may edit only report/gate/schema/test/doc logic needed for telemetry validation. No benchmarked parser/codegen behavior edits. `gate-json` must consume every emitted W0 telemetry field. |
| Host/API schema facts | `skinny/xtask/src/real_typed_schema.rs`; `skinny/crates/bbnf-bench/src/real_typed_struct.rs`; the API-symbol facts in `skinny/crates/bbnf-bench/src/metadata.rs` | W0 may classify these as host/API schema facts. W0 should not change real typed schema or structs; W2 owns typed expansion. Existing four real typed schemas remain maintain guards only. |

## Storage Plan

Store the allowlist in W0 owner paths, preferably as a small gate-consumed module:

- Add `skinny/crates/bbnf-bench/src/lock14_baseline.rs` with a static slice of `AllowlistEntry { path, class, w0_mutability, behavior_surface }`.
- Add one `pub mod lock14_baseline;` line in `skinny/crates/bbnf-bench/src/lib.rs`.
- Call `lock14_baseline::validate(workspace_root())` from `skinny/crates/bbnf-bench/src/bin/gate.rs` before `report.validate_schema_v3()`.
- Keep `skinny/xtask/src/main.rs` unchanged unless the implementation wants `gate-json` to print a clearer failure context; `cargo xtask gate-json` already delegates to the bench gate.

The manifest should record exact paths and classes, not semantic permissions. W0 should use class names like `grammar_input`, `fixture_input`, `generated_json_output`, `generated_typed_output`, `per_grammar_template`, `test_fixture`, `bench_gate_schema`, and `host_api_schema_fact`. Avoid adding a directive, BIR variant, `BackendShape`, public substrate type, or a new parser/codegen API.

## Validation Checks

Required W0 checks:

- Allowlist completeness: every JSON-specific path found by the Lock 14 scan is either in the allowlist or is rejected as `lock14_generic_leak`.
- Allowlist minimality: entries outside the five allowed SPEC Section 2.1 classes reject.
- Generated/runtime no-diff: `cargo xtask check-json`, `cargo xtask check-real-typed`, and `git diff --exit-code -- skinny/crates/runtime/src/grammars/json skinny/crates/bbnf-bench/src/generated_real_typed.rs`.
- Conformance no-drift: `cargo xtask check-conformance`.
- Gate consumption: `cargo xtask gate-json --advisory` must call the allowlist validator and fail closed on missing required W0 telemetry, unsupported outcome, stale sidecar strict claim, or malformed sidecar manifest.
- Sidecar policy: populated simdjson, yyjson, RapidJSON, and asmjson cells require manifest coverage for corpus identity, binary identity, hardware, build flags, run id/date, comparator plane, comparator strictness, and freshness. Missing values must be rendered as `absent:<reason>`. Historical hard-coded sidecar values must remain planning signals unless manifest-validated as same-run strict on the same output plane.
- No behavior drift: after `SK-V8-open` capture, every current main row must stay within +/-1.0 percent of the opening throughput cell. Any larger movement rejects W0 or forces a split before close.

Suggested focused tests:

- `cargo test -p bbnf-bench gate::tests::schema_rejects_simd_scan_without_hash`
- `cargo test -p bbnf-bench report::tests::schema_v3_rejects_missing_required_comparator`
- `cargo test -p bbnf-bench metadata::tests::row_metadata_has_required_fields`
- `cargo test -p bbnf-bench real_typed_struct::tests`
- New `lock14_baseline` unit tests: accepts the exact allowlist; rejects an unlisted generic `json` helper; rejects a new public JSON-named generic API path; rejects a malformed sidecar manifest; rejects missing `absent:<reason>` for an unavailable sidecar.
- `cargo test -p codegen emits_expected_file_set_in_order emission_is_deterministic direct_parser_is_authored_from_sink_only_lowering emits_typed_direct_consumer_module` or equivalent targeted codegen tests if the W0 validator fingerprints generated/template surfaces.

## LOC And Cost Estimate

Expected implementation inside W0 budget:

- `lock14_baseline.rs`: 110-150 LOC for the static table, path scanner, class validation, and sidecar-manifest helper.
- `lib.rs` and `bin/gate.rs` integration: 5-20 LOC.
- Unit tests in `lock14_baseline.rs` or `gate.rs`: 60-90 LOC.
- Optional clearer xtask error plumbing: 0-15 LOC.

Total expected source/test LOC: 175-275 LOC, under W0's <=350 report/gate/schema/test/doc LOC budget and with 0 production behavior LOC. Implementation plus verification should fit 60-80 minutes if it reuses existing `check-json`, `check-real-typed`, `check-conformance`, and focused unit tests. A full `bench-json` capture is the cost risk; if it cannot fit the 90-minute W0 cap with gate refresh and RESULTS update, W0 must split before dispatch and cannot close on a partial telemetry-only report.

## No-New-Surface Guard

The allowlist is telemetry and audit metadata only. It must not authorize parser/codegen/runtime behavior edits, new directives, new BIR variants, new `BackendShape` values, a public substrate API, `UnionTape`, sidecar/cursor substrate, or strict-admission promotion from stale sidecars. `gate-json` is the same-wave consumer: if the allowlist is printed but not consumed, W0 fails as producer-only telemetry.

Recommended W0 close rule: accept only when the allowlist validator passes, generated outputs are byte-identical, conformance passes, all 38 current main rows have required W0 telemetry, malformed sidecar evidence is rejected, and no throughput cell moves beyond +/-1.0 percent of `SK-V8-open`.
