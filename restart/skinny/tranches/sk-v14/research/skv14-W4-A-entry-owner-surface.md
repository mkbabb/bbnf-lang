# SK-V14 W4-A: PRUNE-2 Entry and Owner Surface

Date: 2026-05-26.
Wave: W4 PRUNE-2.
Phase: research.

## Entry Gate

W4 is open only because amended W2 and W3 are admitted:

- W2 admitted skinny-side `regen-css` at commit `45568e669` and closed at
  `restart/skinny/tranches/sk-v14/research/skv14-W2R-close.md`.
- W3 admitted the production CSS L4 corpus loader at commit `b0a864f0b`.

W4 remains non-admitting. CSS L4 row re-admission is W8 work after the full
PRUNE chain closes.

## Required Deletion Surface

`restart/skinny/tranches/sk-v14/SPEC.md:568-608` names these owner paths:

- `skinny/crates/codegen/src/css_l4_*_templates/`
- `skinny/crates/codegen/src/css_l4_*_provider.rs`
- `skinny/crates/runtime/src/grammars/css_l4_*/`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md` (SPEC text says
  `skinny/ROLLING-SOTA-DELTA.md`, but the live file is under `restart/skinny/`)
- `skinny/REDRESS.md`

Live file census at W4 entry:

```text
skinny/crates/codegen/src/css_l4_at_rules_and_media_provider.rs
skinny/crates/codegen/src/css_l4_declaration_values_provider.rs
skinny/crates/codegen/src/css_l4_declaration_values_extended_provider.rs
skinny/crates/codegen/src/css_l4_nested_layout_provider.rs
skinny/crates/codegen/src/css_l4_stylesheet_selectors_provider.rs
skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_provider.rs
skinny/crates/codegen/src/css_l4_visual_functions_provider.rs

skinny/crates/codegen/src/css_l4_at_rules_and_media_templates/
skinny/crates/codegen/src/css_l4_declaration_values_templates/
skinny/crates/codegen/src/css_l4_declaration_values_extended_templates/
skinny/crates/codegen/src/css_l4_nested_layout_templates/
skinny/crates/codegen/src/css_l4_stylesheet_selectors_templates/
skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/
skinny/crates/codegen/src/css_l4_visual_functions_templates/

skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/
skinny/crates/runtime/src/grammars/css_l4_declaration_values/
skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/
skinny/crates/runtime/src/grammars/css_l4_nested_layout/
skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/
skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/
skinny/crates/runtime/src/grammars/css_l4_visual_functions/
```

## Row Surface

`skinny/RESULTS.md` already carries the 24 CSS L4 rows as
`AUDIT-FALSIFIED` and `not_admitted:pre-W8-css-full-parse-equality`.
`restart/skinny/ROLLING-SOTA-DELTA.md` still lists those 24 rows as
`ADMITTED`; W4 must revert that remaining rolling delta state.

## Immediate Risk

W4's deletion surface includes the provider modules that the current W2
`regen-css` path still compiles through. That makes the W4 exit gate suspect
before any source edit: deleting the providers may remove the emitter needed
to regenerate the runtime twins.
