# SK-V14 W5D-DELETE Close Packet

Date: 2026-05-26.

Disposition: ADMIT.

## Commits

- Research + plan: `32441927e`
- Lock 14 owner routing: `b6f4d231b`
- Redress / deletion: `b39681a6f`

## Landed

W5D-DELETE removed the old per-grammar provider/template residue after W5C-GEN
made the request/frontend driven generator body production-load-bearing:

- The seven CSS L4 provider modules were deleted.
- The seven CSS L4 template directories were deleted.
- `json_provider.rs` was deleted after production code no longer imported it.
- `json_templates/` remains because `runtime_generator.rs` still production-
  consumes it for compiled JSON runtime emission.
- `lock14_baseline.rs` now enforces the post-W5 topology: zero old provider
  modules and zero CSS L4 template directories, while retaining the JSON
  template exception until a later proof removes that production dependency.
- `emit_runtime_from_request` still emits the seven CSS L4 skinny runtime trees
  through the W5A request, W5B frontend facts, and W5C provider-free generator
  body.

## Evidence

- `rustfmt --edition 2021 --check skinny/crates/codegen/src/lib.rs skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5c_gen --profile ax-iter -- --nocapture`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen css_l4_frontend_profiles_are_request_generated --profile ax-iter -- --nocapture`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen css_l4_generated_runtimes_reproducible_from_request --profile ax-iter -- --nocapture`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5d_delete_owner_paths_admit --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5d_delete_owner_paths_preserve_json_templates --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5d_post_w5_provider_template_topology_accepts_zero_providers_and_css_templates --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14 --profile ax-iter -- --nocapture`
- `cargo check --manifest-path skinny/Cargo.toml -p codegen -p xtask -p runtime`
- `cd skinny && cargo xtask regen-css`
- `cd skinny && cargo xtask check-css-l4-at-rules-and-media`
- `cd skinny && cargo xtask check-css-l4-declaration-values`
- `cd skinny && cargo xtask check-css-l4-declaration-values-extended`
- `cd skinny && cargo xtask check-css-l4-nested-layout`
- `cd skinny && cargo xtask check-css-l4-stylesheet-selectors`
- `cd skinny && cargo xtask check-css-l4-vendor-and-custom-atrules`
- `cd skinny && cargo xtask check-css-l4-visual-functions`
- `cd skinny && cargo xtask check-json`
- `cd skinny && cargo xtask gate-json --check-results --skv14-existing-results-capture`
- `find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l` = `0`
- `find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l` = `0`
- `test -d skinny/crates/codegen/src/json_templates`
- `cd skinny && ! rg -n '\b(RuntimeProvider|render_runtime_profile|json_provider|css_l4_.*provider)\b' crates/codegen/src`
- `cd skinny && ! rg -nU 'match\s+[^{]+\{[^}]*\b(Json|CssL4\w*|Bbnf\w*|GoogleSheets\w*)\b\s*=>' crates/{codegen,runtime,passes,bbnf,grammar}/src --glob '!**/tests/**'`

## Routed Remainder

W6.0 owns the first root-runtime collapse: `crates/core/src/runtime/css_l4/`.
That work must emit or collapse the root CSS L4 runtime tree without borrowing
W2/W5D scope, must pass the destructive CSS L4 root-runtime round trip, and
must preserve the post-W5 provider/template topology.

The retained `json_templates/` directory is not a W5D failure. It remains a
production dependency of the request/frontend generator body until a later
wave proves a provider-free JSON runtime path no longer consumes it.
