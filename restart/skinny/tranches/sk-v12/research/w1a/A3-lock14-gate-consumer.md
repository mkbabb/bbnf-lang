# SK-V12 W1a A3 - Lock 14 Gate Consumer And Report Plumbing

Date: 2026-05-20.
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
Scope: read-only research for `skinny/xtask/src/main.rs`,
`skinny/crates/bbnf-bench/src/bin/gate.rs`,
`skinny/crates/bbnf-bench/src/report.rs`, `skinny/RESULTS.md`, and
`skinny/REDRESS.md`.

## Authority Read

SPEC Section 2.1 makes every generic-crate edit subject to Lock 14:
generic crates must not branch on grammar names, must not contain JSON/CSS/Sheets
parser names, and must not embed JSON structural alphabet, JSON string escape,
JSON number policy, JSON object-key policy, JSON `OffsetFlags` meaning, or
`JsonSink` shape. Per-grammar generated modules own alphabets, FIRST/follow
tables, escape and number policy, flag semantics, sink/view/kind wrappers, and
output facts. CSS L4 must be exercised by benchmark/equality, and generated
size must be tracked.

SPEC Section 4 makes W1a a legality gate before CSS L4 emission: introduce a
`GrammarConfig` or equivalent generated metadata surface, move JSON policy out
of generic code, add a Lock 14 scan/gate consumer for generic crates, preserve
JSON parity and guard floors, and do not add a directive, BIR variant,
`BackendShape`, or public substrate API.

The user pin keeps Lock 14 and the seven value/API leaks in force while making
CSS L4 authoritative and raising the eventual CSS admission bar to
`lightningcss_mbps + 1`. The pin does not authorize a schema-only or prose-only
generality close.

`skv12-value-api-audit.md` Section 5 says existing `ValueRef`/tape lifetimes can
remain, but W1 needs minimal generic surface additions: grammar escape/number
config, generated per-grammar metadata modules, and parametrized view/value
generation. It specifically warns that JSON `JsonSink`, JSON dispatch, JSON
string/number/escape policy, and JSON wrappers are not reusable for non-JSON
grammars as generic behavior.

## Current Consumer Shape

`bbnf-bench --bin gate` runs `lock14_baseline::validate(&workspace)` before
either companion non-JSON report returns and before normal JSON result rendering.
This is the right consumption point for a generic-crate scan because it is a
gate precondition, not a measured row outcome.

`xtask gate-json` accepts and forwards `--w1a-non-json-report <path>` and
`--skv12-non-json-report <path>`. The passthrough validator accepts only:
`--advisory`, `--check-results`, `--update-results`, `--write-results`,
`--include-volatile-probes`, and the two companion report flags with one path.

The normal JSON report path still uses the existing `Outcome` enum only for JSON
row classification. Companion report validation returns `Ok(())` or a process
error; it does not classify into `Outcome`. Therefore the W1a generic-crate scan
should be consumed by extending the Lock 14 validator, not by adding an outcome.

## Add The Generic-Crate Scan

Recommended implementation path:

1. Extend `skinny/crates/bbnf-bench/src/lock14_baseline.rs` with a
   `validate_generic_crate_neutrality(root)` call inside `validate(root)` after
   entry/freeze validation and before `validate_backend_shape_surface`.
   That module is the existing Lock 14 validator consumed by the named gate
   plumbing; no change to the report schema is needed just to consume the scan.
2. Scan only generic-crate owner roots, not generated or per-grammar template
   roots. The initial root set should align with Section 2.1 and the current
   frozen roots: `crates/runtime/src/tape`, generic `crates/runtime/src/lib.rs`
   surfaces outside `grammars/json`, `crates/ir/src`, `crates/passes/src`,
   generic `crates/codegen/src` surfaces outside `json_provider.rs` and
   `json_templates`, and `crates/bbnf-simd/src` if W1a touches shared scanner
   substrate.
3. Fail on forbidden tokens in generic roots: grammar parser names
   (`JsonParser`, `CssL4Parser`, `GoogleSheetsParser`, `BbnfBootstrap`),
   grammar-name branch keys, JSON structural alphabet constants, JSON escape or
   number policy helpers, JSON object-key policy, JSON-specific `OffsetFlags`
   meanings, and `JsonSink` shape. Allow those only in per-grammar generated or
   template roots that are explicitly not generic.
4. Keep the failure as `Err("Lock 14 ...")` from `lock14_baseline::validate`.
   `bin/gate.rs` already wraps this as `Lock 14 baseline validation failed:
   ...`, and `xtask gate-json` already propagates the nonzero gate failure.
5. Do not change `Outcome`, `SCHEMA_V3_HEADER`, `SkV12NonJsonReport`, or
   `RESULTS.md` for this scan. The scan is a prerequisite for accepting a report,
   not a row in the report. Add schema fields only if a future W1a/W1b contract
   must persist a named scan artifact in a machine-readable report; otherwise
   the gate status is enough.

This avoids changing the JSON outcome model or report schema while still making
the Lock 14 scan same-command evidence for `--skv12-non-json-report`.

## Exact Current Validation Constraints

Lock 14 baseline validation currently requires:

- allowlist paths are unique, use only supported classes, use `read_only` or
  `telemetry_only` mutability, do not name `UnionTape` or `directive`, and exist;
- frozen roots have empty `git status --porcelain` and clean `git diff --quiet`;
- parent commit diffs under frozen roots are rejected unless the commit subject
  matches one of the hardcoded historical wave scopes and every changed path is
  in that wave's owner allowlist;
- `BackendShape` in `crates/ir/src/lib.rs` contains exactly five variants:
  `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage`;
- `UnionTape` or `union_tape` in the IR surface is rejected.

The SK-V11 `--w1a-non-json-report` lane currently requires:

- schema `sk-v11-w1a-nonjson-v1`, wave `SK-V11-W1a`, and run id
  `sk-v11-w1a:fixture-fnv64-` plus 16 lowercase hex characters;
- nonempty unique rows whose row-level wave/run identity matches the report;
- grammar `css_l4`, `sheets`, or `bbnf_self`, with domains
  `css_l4_bench`, `sheets_bench`, or `bbnf_self_bench`;
- only `css_l4/declaration_values`, `sheets/formula`, or `bbnf_self/grammar`;
- workload `direct` maps to `digest`, workload `typed` maps to `typed direct`;
- outcome/verdict exactly `S / NO-GO`;
- strictness `strict`, `parse_utf8=measured-row`, `escape_complete=yes`,
  `measured_validation_path=schema-only`,
  `same_wave_consumer_class=non_json_gate_schema_only`,
  `track2_independence_status=independent_verified`, and
  `diagnostic_nonproducer_status=pmu+cycles+profiles:nonproducer`;
- positive Track 1/Track 2 Mbps, nonzero sample count, and `ns_per_byte=` in
  sample cost;
- exact structured context:
  `none:w1a-schema`, `none:w1a-schema-only`,
  `nonjson-schema-only`, `zero_or_inert`, fixture profile prefix, native bench
  build flags, host arch/cpu fields, and feature arch/os/simd/target_cpu fields;
- exactly one `internal_oracle` comparator, same output plane, strict,
  `same-run-oracle`, sidecar `n/a`, positive Mbps, and source
  `oracle:w1a:<grammar>:<corpus>:<workload>:<plane>`.

The SK-V12 `--skv12-non-json-report` lane currently requires:

- in `bbnf-bench --bin gate`, the companion flag must be the only companion
  flag, must appear first, must have exactly one path argument, and cannot be
  combined with `--update-results`, `--write-results`, or
  `--include-volatile-probes`;
- schema `sk-v12-nonjson-generated-v1`, wave id starting `SK-V12-W`, and a run
  id starting `sk-v12-` or `sk-v12:` that does not contain `sk-v11`;
- nonempty unique rows with row id `<grammar>/<corpus-or-workload>/<workload>/main`;
- grammar `css_l4`, `sheets`, or `bbnf_self`; `json` is rejected;
- domain starts with `non_json_generated:` and contains the grammar id;
- `direct_to_struct` maps to `direct_sink`; `real_typed_struct` maps to
  `typed_direct`; `parse_only` is rejected;
- outcome id must parse through the existing JSON `Outcome` id parser, but the
  measurement gate also requires verdict `GO`;
- generated Track 1 source, runtime, and input provenance must contain the
  grammar id and must not contain `json`, `sheets_witness`, `w1a`, or `hand_only`;
- oracle/Track 2 source must be independent: not the generated source/runtime,
  not `runtime::generated_json::parse`, and not containing `track1`;
- oracle status must contain `same-plane`, `strict`, `independent`, and `fresh`;
- Track 1 artifact and benchmark artifact must contain the report run id;
- Track 1 Mbps is finite and at least `1.0`; Track 2/oracle Mbps is finite and
  at least `1.0`; sample count is at least `30`;
- `strict_output_equality=pass`, `gate_status=pass`, and `verdict=GO`;
- `workload_class=baseline` requires `baseline_row_id=none` and no baseline or
  threshold Mbps; `workload_class=intervention` requires a baseline row,
  baseline Mbps, and `threshold_mbps >= ceil(baseline_mbps * 1.01)`;
- same-wave consumer is `companion_gate_generated_baseline` for baselines and
  `companion_gate_generated_intervention` for interventions;
- JSON guard state is exactly `not_refreshed:no_behavior_drift` or starts with
  `refreshed:`;
- host/build context must include `arch=`, `target_cpu=native`,
  `target-cpu=native`, and `ns_per_byte=`.

The normal JSON `Report` path requires schema-v3 rows to have nonempty text
fields, Track 1/Track 2 Mbps, sonic strict Mbps, serde_json Mbps, and delta vs
sonic strict. `validate_sk_v8_w0` then requires unique known JSON row ids,
uniform valid `sk-v9-open:criterion-fnv64-<16hex>` run id, JSON grammar/domain,
expected W0 manifest semantics, expected comparator evidence, and exactly the
baseline row set plus the optional W6 github-events typed row.

## Generated RESULTS Exactness

`bbnf-bench --bin gate` renders a deterministic markdown report. Unless
`--update-results` or `--write-results` is present, it reads `skinny/RESULTS.md`
and requires byte-for-byte equality with `report.render_markdown()`. Any drift
prints a stale-results message and exits with the invalid verdict code before
normal verdict handling.

When updating, `Report::write_markdown` writes exactly the rendered string. The
main table header and alignment are fixed by `SCHEMA_V3_HEADER` and
`SCHEMA_V3_ALIGN`; the SK-V9 W0 telemetry manifest is emitted whenever main
rows exist; masking probes are emitted only when probe rows exist; notes are
emitted in accumulated order.

Companion non-JSON report gates return before JSON report rendering. They do
not read or write `skinny/RESULTS.md` and cannot be combined with
`--update-results`, `--write-results`, or volatile probes. Keeping the W1a
generic-crate scan inside Lock 14 preserves this exactness contract.

`xtask gate-json --with-cost-facts --check-results` adds an independent
`RESULTS.md` snapshot check: the manifest row count must be 40 or 41 depending
on the W6 typed row, W0 snapshot markers must exist, diagnostic nonproducer
status must exist, and all manifest rows must share one valid SK-V9 run id.

## Verification Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny`:

```sh
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench lock14_baseline --lib -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench skv12_non_json_report --lib -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench w1a_non_json_report --lib -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate skv12_non_json_report_arg -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p xtask gate_json_passthrough -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/skv12-W0-nonjson-pass.json
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
git diff --exit-code -- RESULTS.md REDRESS.md xtask/src/main.rs crates/bbnf-bench/src/bin/gate.rs crates/bbnf-bench/src/report.rs
```

After a W1a implementation adds the generic-crate scan, add negative tests that
mutate a generic root with each forbidden Lock 14 class and positive tests that
the same tokens remain allowed in per-grammar generated/template roots. Those
tests should live with `lock14_baseline` and should not require `RESULTS.md`
movement.

## Notes For Handoff

The current SK-V12 companion schema is still a generated-baseline gate lane, not
the final CSS SOTA admission schema. The user pin's `lightningcss_mbps + 1`
floor must be consumed by the later CSS admission/report surface; the current
intervention threshold check still uses the historical `ceil(baseline * 1.01)`
formula.

`skinny/REDRESS.md` documents the historical SK-V11 non-JSON fixture lane and
the W0 SK-V12 companion gate admission. W1a should not move `RESULTS.md` merely
to prove generic-crate neutrality. Only a generated CSS benchmark/equality row
or a later CSS admission schema should change report semantics.
