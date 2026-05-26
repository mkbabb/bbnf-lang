# SK-V14 W5D-DELETE Research

Date: 2026-05-26.

Disposition: W5D dispatchable after W5C-GEN close.

## Entry Evidence

- W5C implementation commit: `b19475486`.
- W5C close packet commit: `747d79170`.
- W5C close leaves provider/template residue intentionally:
  - 8 provider modules: seven CSS L4 providers plus `json_provider.rs`.
  - 7 CSS L4 template directories.
  - `json_templates/` remains production-consumed by
    `runtime_generator.rs`, so it is not deletion-owned by W5D.

## Deletion Roster

Provider modules safe to delete:

- `skinny/crates/codegen/src/css_l4_at_rules_and_media_provider.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_extended_provider.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_provider.rs`
- `skinny/crates/codegen/src/css_l4_nested_layout_provider.rs`
- `skinny/crates/codegen/src/css_l4_stylesheet_selectors_provider.rs`
- `skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_provider.rs`
- `skinny/crates/codegen/src/css_l4_visual_functions_provider.rs`
- `skinny/crates/codegen/src/json_provider.rs`

CSS template directories safe to delete:

- `skinny/crates/codegen/src/css_l4_at_rules_and_media_templates/`
- `skinny/crates/codegen/src/css_l4_declaration_values_extended_templates/`
- `skinny/crates/codegen/src/css_l4_declaration_values_templates/`
- `skinny/crates/codegen/src/css_l4_nested_layout_templates/`
- `skinny/crates/codegen/src/css_l4_stylesheet_selectors_templates/`
- `skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/`
- `skinny/crates/codegen/src/css_l4_visual_functions_templates/`

Non-owned retained residue:

- `skinny/crates/codegen/src/json_templates/`, because
  `runtime_generator.rs` includes JSON template files directly.
- `skinny/crates/codegen/src/grammar_profile.rs`, because the generated
  file roster and generation mode table remain live production metadata.

## Lock 14 Gate Shape

Current `lock14_baseline.rs` already has W5C routing but not W5D routing.
W5D therefore needs a route-first patch:

- Add `SK_V14_W5D_DELETE_OWNER_PATHS`.
- Add `is_w5d_delete_subject`.
- Allow W5D parent diff paths before deleting providers/templates.
- Keep legacy topology expectations unchanged in the route-first commit.

After route-first admission, the deletion commit updates the topology gate:

- Provider modules excluding `grammar_provider.rs`: expected `0`.
- CSS L4 template directories: expected `0`.
- `json_templates/`: allowed while production-consumed.
- Current worktree/cached/provider-template status rejects new providers or
  CSS template dirs; parent-diff deletion authorization is handled by
  `validate_authorized_parent_diff`.

## Exit Commands

- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5d_delete_owner_paths_admit --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5d_post_w5_provider_template_topology_accepts_zero_providers_and_css_templates --profile ax-iter -- --exact`
- `find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l`
- `find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l`
- `cd skinny && ! rg -n '\b(RuntimeProvider|render_runtime_profile|json_provider|css_l4_.*provider)\b' crates/codegen/src`
- `cd skinny && cargo xtask regen-css`
- `cd skinny && cargo xtask check-css-l4-at-rules-and-media && cargo xtask check-css-l4-declaration-values && cargo xtask check-css-l4-declaration-values-extended && cargo xtask check-css-l4-nested-layout && cargo xtask check-css-l4-stylesheet-selectors && cargo xtask check-css-l4-vendor-and-custom-atrules && cargo xtask check-css-l4-visual-functions`
- `cd skinny && cargo xtask check-json`
- `cd skinny && cargo xtask gate-json --check-results --skv14-existing-results-capture`
