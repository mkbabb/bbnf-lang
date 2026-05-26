# SK-V14 W5C-GEN Close Packet

Date: 2026-05-26.

Disposition: ADMIT.

## Commits

- Research: `d9fe58ced`
- Plan: `895a70bbb`
- Lock 14 owner routing: `7eb455df0`
- Redress / implementation: `b19475486`

## Landed

W5C-GEN replaced live provider-backed runtime emission with a request/frontend
IR driven generator body:

- `skinny/crates/codegen/src/runtime_generator.rs` now owns production runtime
  emission for request-driven profiles.
- `emit_runtime_from_request` routes through the W5A request and W5B frontend
  facts boundary, then delegates to the neutral generator.
- `RuntimeProvider`, `render_runtime_profile`, live `json_provider`, and live
  `css_l4_*_provider` dispatch were removed from `lib.rs` /
  `grammar_provider.rs`.
- The seven skinny CSS L4 runtime trees were regenerated from frontend facts
  and now embed request/profile/source hash proof data.
- Provider modules and CSS template directories remain present only as
  W5D-DELETE residue.

JSON remains on the compiled JSON emission mode and still consumes
`json_templates/` directly from `runtime_generator.rs`. W5D may delete
`json_provider.rs`, but `json_templates/` is not deletion-owned until a later
wave proves it is no longer production-consumed.

## Evidence

- `rustfmt --edition 2021 --check skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/grammar_profile.rs skinny/crates/codegen/src/grammar_provider.rs skinny/crates/codegen/src/runtime_generator.rs`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5c_gen_rejects_profile_only_css_emission --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5c_gen_css_runtime_output_depends_on_frontend_source_hash --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_json_request_matches_emit_from_source --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p runtime css_l4 --profile ax-iter -- --nocapture`
- `cd skinny && cargo xtask regen-css`
- `cd skinny && cargo xtask check-css-l4-at-rules-and-media && cargo xtask check-css-l4-declaration-values && cargo xtask check-css-l4-declaration-values-extended && cargo xtask check-css-l4-nested-layout && cargo xtask check-css-l4-stylesheet-selectors && cargo xtask check-css-l4-vendor-and-custom-atrules && cargo xtask check-css-l4-visual-functions`
- `cd skinny && cargo xtask check-json`
- `cd skinny && cargo xtask gate-json --check-results --skv14-existing-results-capture`
- `cd skinny && ! rg -n '\b(render_runtime_profile|RuntimeProvider|GrammarProfile|json_provider|css_l4_.*provider)\b' crates/codegen/src/lib.rs crates/codegen/src/grammar_provider.rs`
- `cd skinny && ! rg -nU 'match\s+[^{]+\{[^}]*\b(Json|CssL4\w*|Bbnf\w*|GoogleSheets\w*)\b\s*=>' crates/{codegen,runtime,passes,bbnf,grammar}/src --glob '!**/tests/**'`

Topology at close:

- `find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l` = 8.
- `find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l` = 7.

Both non-zero counts are expected W5D-DELETE residue.

## Routed Remainder

W5D-DELETE owns:

- Deleting the seven CSS L4 provider modules.
- Deleting the seven CSS L4 template directories.
- Deleting `json_provider.rs` after confirming no production module imports it.
- Closing the post-W5 Lock 14 baseline and parent-diff route for deletion.
- Re-running `regen-css`, seven CSS companions, `check-json`, W5C provider
  reachability greps, and the Lock 14 baseline gate.

W6 remains blocked until W5D-DELETE admits.
