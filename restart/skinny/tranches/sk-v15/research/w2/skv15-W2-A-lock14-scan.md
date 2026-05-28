# SK-V15 W2-A - Lock 14 Scan Root Audit

Scope: read-only audit of `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
for W2 Lock 14 gate restoration.

## Findings

The current Lock 14 close path is validator-only. `gate.rs` calls
`lock14_baseline::validate(&workspace)` before companion report handling, and
`validate()` returns only pass/fail text. There is no gate-consumed root and
exclusion report surface (`skinny/crates/bbnf-bench/src/bin/gate.rs:51`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:585`).

The current generic forbidden-token scan roots are narrower than the SK-V15 W2
receiver matrix. `GENERIC_SCAN_ROOTS` names only:

- `crates/bbnf-regex/src`
- `crates/codegen/src/lib.rs`
- `crates/codegen/src/lower`
- `crates/codegen/src/grammar_profile.rs`
- `crates/passes/src`
- `crates/runtime/src/lib.rs`
- `crates/runtime/src/tape`
- `crates/ir/src`

This omits the leak-bearing roots named by S-P0 A3:
`crates/codegen/src/runtime_generator.rs`,
`crates/codegen/src/grammar_provider.rs`,
`crates/codegen/src/json_sink_direct.rs`,
`crates/codegen/src/json_typed_direct.rs`, and
`crates/codegen/src/json_templates/`
(`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A3-lock14-lock16-generic-scan.md:38`).

The token universe is JSON-shaped. It covers `JsonSink`, `JsonValue`,
`serde_json`, JSON helper names, `HAS_ESC`, `HAS_CONTROL`, and JSON parse
error variants, but not CSS/profile/template/decision leak terms such as
`RuntimeGenerationMode`, `CSS_GENERATED_RS`, `CssFullParseSummary`, `json_`,
`css_`, `RuntimeProvider`, or `JSON-CSS`
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2381`,
`restart/skinny/tranches/sk-v15/SPEC.md:227`).

The omitted roots are not empty. `runtime_generator.rs` contains
`RuntimeGenerationMode`, `CSS_GENERATED_RS`, CSS profile rosters, JSON template
includes, and JSON error/value exports. `json_sink_direct.rs`,
`json_typed_direct.rs`, and `json_templates/` contain JSON-specific direct
parsers and value APIs. W2 must expose these as scanned roots, or as explicit
blocked/routed exclusions; silent omission is the defect.

## Existing Coverage

Existing tests cover allowlist sanity, unknown classes, duplicate paths,
frozen-root status, JSON forbidden-token detection, generated-header baseline,
and frozen-root coverage. No test currently asserts W2 report columns,
self-exempting exclusion rejection, required scan-root presence, or the broader
forbidden-token universe.

## W2 Redress Shape

W2 should add a gate-consumed Lock 14 scan report with the dispatch schema:
included roots, excluded roots, reason, owner, self-scan status, primitive
status, gate consumer, affected rows, and disposition.

The smallest safe implementation is to make `lock14_baseline::validate()`
construct and validate that report internally, with tests that reject missing
required roots, self-exempting exclusions, and forbidden leak tokens. W2 must
not delete provider/template/CSS surfaces; deletion routes to W3/W6.
