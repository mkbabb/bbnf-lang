# SK-V14 W5B.4 Close: Request Consumer

Date: 2026-05-26.
Disposition: ADMIT.

## Commits

- Plan: `e1a2ebc87`
- Redress: `8e99b129f`

## Landed Surface

W5B.4 makes `emit_runtime_from_request` consume the W5A request boundary and
the W5B frontend closure before provider rendering. The new validation checks
that the closure carries the request source set, matching source hashes, import
arcs, layout facts, discard facts, pretty directives, host captures,
projections, and typed projections for non-JSON runtime generation.

JSON stays on the unchanged-output path. Unsupported runtime constructs still
fail closed through the existing diagnostic surface. A scanner repair for
postfix optional groups before `<<` keeps live CSS source valid without
loosening malformed discard-operator checks.

## Evidence

- `rustfmt --edition 2021 --check --config skip_children=true skinny/crates/codegen/src/lib.rs`
- `rustfmt --edition 2021 --check skinny/crates/codegen/src/grammar_provider.rs skinny/crates/grammar/src/lib.rs`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5b_frontend_request_consumes_lowered_ir_before_provider_rendering --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5b_frontend_request_rejects_missing_closure_materiality --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_runtime_contract_consumes_source_and_metadata --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_json_request_matches_emit_from_source --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_sheets_bbnf_fail_closed_through_runtime_contract --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_discard_operator_accepts_postfix_group_left_operand --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_malformed_discard_operator_fails_closed --profile ax-iter -- --exact`
- Carry checks: W5B.1 import closure, W5B.2 layout/discard, W5B.3
  pretty/span/projection, and Lock 14 W5B owner-path exact tests.
- `cargo xtask regen-css`
- Seven CSS L4 companions:
  `check-css-l4-at-rules-and-media`,
  `check-css-l4-declaration-values`,
  `check-css-l4-declaration-values-extended`,
  `check-css-l4-nested-layout`,
  `check-css-l4-stylesheet-selectors`,
  `check-css-l4-vendor-and-custom-atrules`,
  `check-css-l4-visual-functions`.
- `cargo xtask check-json`
- Provider/template topology remains W5D residue:
  `find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l` returned `8`;
  `find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l` returned `7`.
- `skinny/RESULTS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md`, and generated
  runtime outputs had no W5B.4 maintain diff after regen/checks.

## Routed Remainder

W5C-GEN owns provider-free generator-body replacement and live provider-dispatch
retirement. W5D-DELETE owns CSS provider/template deletion and the Lock 14
baseline close.
