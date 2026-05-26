# SK-V14 Wave W5A Redress: Source-Consuming Runtime Generation Request

Date: 2026-05-26.
Wave: W5A.
Phase: redress close.
Disposition: ADMITTED.

## Scope

W5A introduces the grammar-neutral `RuntimeGenerationRequest` boundary that
passes grammar source and workspace metadata into codegen before runtime
emission. The wave admits source-consuming capability only: CSS provider module
and template deletion remains W5B-owned, and root runtime collapse remains
W6-owned.

W5A does not edit `skinny/RESULTS.md`,
`restart/skinny/ROLLING-SOTA-DELTA.md`, `crates/core/src/runtime/css_l4/`, or
`grammar/css/l4/`. It does not delete, rename, or add legacy CSS provider
modules or CSS template directories.

## Landed Files

- `skinny/crates/grammar/src/lib.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_provider.rs`
- `skinny/xtask/src/regen.rs`
- `skinny/xtask/src/regen_css.rs`
- `skinny/xtask/src/main.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`

## Runtime Contract

- Grammar runtime-source facts now cover the W5A-required CSS L4 constructs:
  import metadata, directives, token declarations, whitespace/pretty hooks,
  comma sequence, whitespace modifiers, projection arrows, typed projection
  metadata, and host capture spans.
- `emit_runtime_from_request` consumes source facts plus workspace metadata and
  validates output shape before delegating to existing byte-compatible runtime
  renderers.
- `regen-css`, all seven CSS L4 companion checks, and JSON checks now construct
  the same request shape rather than calling
  `emit_runtime_profile(target.profile)` at the `regen.rs` boundary.
- Sheets and BBNF-self use the same request path and fail closed with named,
  source-located unsupported constructs.
- The temporary Lock 14 W5A guard allows this source-consuming slice and rejects
  provider/template add, delete, or rename before W5B.

## Executable Evidence

Commands run at HEAD during W5A redress:

```sh
cd skinny && cargo test -p grammar w5a_css_l4_constructs_parse_as_source_facts -- --exact --nocapture
cd skinny && cargo test -p grammar w5a_named_unsupported_constructs_are_source_located -- --exact --nocapture
cd skinny && cargo test -p codegen w5a_runtime_contract_consumes_source_and_metadata -- --exact --nocapture
cd skinny && cargo test -p codegen w5a_json_request_matches_emit_from_source -- --exact --nocapture
cd skinny && cargo test -p codegen w5a_sheets_bbnf_fail_closed_through_runtime_contract -- --exact --nocapture
cd skinny && cargo xtask check-json
cd skinny && cargo xtask regen-css
cd skinny && cargo xtask check-css-l4-at-rules-and-media
cd skinny && cargo xtask check-css-l4-declaration-values
cd skinny && cargo xtask check-css-l4-declaration-values-extended
cd skinny && cargo xtask check-css-l4-nested-layout
cd skinny && cargo xtask check-css-l4-stylesheet-selectors
cd skinny && cargo xtask check-css-l4-vendor-and-custom-atrules
cd skinny && cargo xtask check-css-l4-visual-functions
cd skinny && cargo xtask gate-json --check-results --skv14-existing-results-capture
cd skinny && cargo test -p bbnf-bench lock14_baseline -- --nocapture
git diff --exit-code HEAD -- skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md
git diff --exit-code -- crates/core/src/runtime/css_l4 grammar/css/l4
```

Observed results:

- All exact W5A grammar/codegen tests passed with nonzero test-count guards.
- JSON check and all seven CSS L4 companion checks passed through the request
  path.
- `gate-json --check-results --skv14-existing-results-capture` passed.
- Lock 14 baseline tests passed: 42 passed, 0 failed.
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` remained
  byte-identical to `HEAD`.
- `crates/core/src/runtime/css_l4/` and `grammar/css/l4/` remained unchanged.
- `emit_runtime_profile(target.profile)` is absent from
  `skinny/xtask/src/regen.rs`.
- Provider count excluding `grammar_provider.rs` remained 8; CSS template
  directory count remained 7.
- W5A source/test LOC delta was 888 under the converged plan path list and 921
  with the JSON `xtask/src/main.rs` request-routing edit included; both are
  below the <=1000 C-1 part-A cap.

## Exit Gate

W5A satisfies the amended SPEC Section 8 exit gate. It admits the
source-consuming runtime generator contract and unlocks W5B only. W6, W7, W8,
W9, and W10 remain blocked until W5B and the rest of the PRUNE chain close.
