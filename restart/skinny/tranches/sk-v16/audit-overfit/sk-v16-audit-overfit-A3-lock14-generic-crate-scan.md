# SK-V16 S-P0 A3 - Lock 14 Generic-Crate Scan

Date: 2026-05-28.
HEAD: `fc16919d4`.
Axis: A3 Lock 14 generic-crate scan.
Disposition: FAIL-A3-TARGET.

## Critical Findings

### A3-C1 - Production skinny codegen still hard-codes JSON and CSS families

`skinny/crates/codegen/src/runtime_generator.rs` still embeds JSON templates,
JSON parse-only logic, CSS `RequestFacts`, `CSS_GENERATED_RS`, CSS output-plane
match arms, and CSS full-parse/fact-stream proof paths in nominally generic
codegen. This violates Lock 14's ban on hand-coded JSON/CSS runtime families and
profile matches in generic code.

Representative evidence:

- `skinny/crates/codegen/src/lib.rs` exposes `json_sink_direct` and
  `json_typed_direct`.
- `skinny/crates/codegen/src/runtime_generator.rs` inserts `CSS_GENERATED_RS`
  into generated CSS runtime output.
- `skinny/crates/codegen/src/runtime_generator.rs` contains `CssFullParseSummary`
  and CSS output-plane match arms.

Representative scan:

```sh
rg -n '\b(json|css_l4|CSS|JSON|RuntimeGenerationMode|CSS_GENERATED_RS|JSON_PARSE_ONLY|lightningcss|cssparser)\b' \
  skinny/crates/codegen/src \
  skinny/crates/passes/src \
  skinny/crates/ir/src \
  skinny/xtask/src \
  skinny/crates/bbnf-bench/src \
  -g '!**/generated.rs'
```

### A3-C2 - Lock 14 baseline does not scan the highest-risk emitters

The Lock 14 baseline gate scans a limited generic root set but does not perform
token-neutrality scanning over `runtime_generator.rs`, `grammar_provider.rs`,
`json_*`, `json_templates`, `xtask`, `skinny/crates/bbnf-bench/src/report.rs`,
`skinny/crates/bbnf-bench/src/css_l4_w8.rs`, and
`skinny/crates/bbnf-bench/src/gate.rs`. Those areas are mostly existence/report
coverage or admission/gate surfaces, not grammar-token neutrality. Therefore
`cargo xtask gate-json --check-results` can pass while A3-C1 remains live.

## High Findings

### A3-H1 - `xtask` remains a grammar-name router

`skinny/xtask/src/main.rs` and `skinny/xtask/src/regen_css.rs` retain direct
JSON/CSS commands, JSON target config, CSS profile arrays, CSS row ids, output
dirs, and output planes. This needs manifest-driven ownership or historical-only
quarantine before A3 can close.

### A3-H2 - Root core still carries per-grammar runtime families

`crates/core/src/runtime/mod.rs` exports per-grammar runtime modules and
`crates/core/src/css_types.rs` / `crates/core/src/runtime/css_l4/**` remain CSS
specific. This matches the SK-V16 Alpha read/delete/replace-only remainder, not
a generic close state.

### A3-H3 - Old CSS fact-stream gates remain diagnostic risk

CSS retained fact-stream/lightningcss fixtures still encode CSS row ids and
same-plane sidecar evidence. They remain diagnostic non-admission and must not
satisfy SK-V16 typed CSS admission.

## Prune Candidates

1. Replace `runtime_generator.rs` JSON/CSS constants with grammar-derived
   provider/template input. Split CSS request-facts/provider replacement from
   JSON parse-only/template quarantine.
2. Remove or quarantine `CSS_GENERATED_RS`, `JSON_PARSE_ONLY_*`, CSS
   `OUTPUT_PLANE` match arms, `json_sink_direct`, `json_typed_direct`, and
   `json_templates` from generic API dependencies.
3. Expand Lock 14 token scanning to cover `runtime_generator.rs`,
   `grammar_provider.rs`, `json_*`, `json_templates`, `xtask`, `report.rs`,
   `css_l4_w8.rs`, and `gate.rs`.
4. Convert `xtask/src/regen_css.rs` and JSON target setup to manifest-driven
   runtime targets, or mark them historical-only with no SK-V16 close authority.
5. Keep old CSS fact-stream equality validators diagnostic-only.

CH4 split note: every A3 prune row must include owner paths, manual source/test
LOC budget, generated-output status, docs/ledger LOC budget, phase hard cap,
split trigger, and same-commit consumer or executable gate.
