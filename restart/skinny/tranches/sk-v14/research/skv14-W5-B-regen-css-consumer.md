# SK-V14 W5-B: Regen-CSS Consumer Surface

Date: 2026-05-26.
Wave: W5.
Phase: research.
Agent: Maxwell.
Scope: read-only inspection of `xtask` regen/check paths.

## Question

Determine how W5 should migrate `regen-css` while preserving command names,
byte determinism, and all seven CSS L4 companion checks.

## Finding

`regen-css` is command-stable today:

- `skinny/xtask/src/main.rs` dispatches `regen-css` and seven
  `check-css-l4-*` companions.
- `skinny/xtask/src/regen_css.rs` owns the CSS L4 roster: 15 grammar-source
  inputs, two metadata inputs, and seven runtime targets.
- `skinny/xtask/src/regen.rs` hashes target source/metadata inputs, prints the
  digest, then calls `codegen::emit_runtime_profile(target.profile)`.

The grammar and metadata inputs are freshness inputs only; they are not
currently passed into codegen. Emission still comes from static codegen
templates.

## W5 Consumer Requirement

For W5 to be real, `regen.rs` must pass the loaded source/metadata bundle into
codegen's replacement provider dispatch. Otherwise `cargo xtask regen-css`
does not consume the claimed `GrammarProvider` abstraction.

The command surface should remain:

- `cargo xtask regen-css`
- `cargo xtask check-css-l4-at-rules-and-media`
- `cargo xtask check-css-l4-declaration-values`
- `cargo xtask check-css-l4-declaration-values-extended`
- `cargo xtask check-css-l4-nested-layout`
- `cargo xtask check-css-l4-stylesheet-selectors`
- `cargo xtask check-css-l4-vendor-and-custom-atrules`
- `cargo xtask check-css-l4-visual-functions`

## Deletion Dependency

The seven CSS provider modules and template directories cannot be deleted
until `codegen` no longer references:

- `mod css_l4_*_provider`;
- `grammar_profile` provider imports;
- `include_str!("css_l4_*_templates/...")`.

Deleting them first reproduces REDRESS-184's intrinsic provider-deletion
cycle.

## Expected Verification If Implementable

```sh
cd skinny
cargo test -p xtask css_l4_roster
cargo test -p codegen css_l4
cargo xtask regen-css
git diff --exit-code -- crates/runtime/src/grammars/css_l4_*
cargo xtask check-css-l4-at-rules-and-media
cargo xtask check-css-l4-declaration-values
cargo xtask check-css-l4-declaration-values-extended
cargo xtask check-css-l4-nested-layout
cargo xtask check-css-l4-stylesheet-selectors
cargo xtask check-css-l4-vendor-and-custom-atrules
cargo xtask check-css-l4-visual-functions
```

W5 must also update `skinny/crates/bbnf-bench/src/lock14_baseline.rs`, because
the current baseline still cites the pre-W5 CSS provider/template paths.
