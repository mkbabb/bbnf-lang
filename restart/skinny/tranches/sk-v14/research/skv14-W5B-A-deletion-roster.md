# SK-V14 W5B-A: Provider And Template Deletion Roster

Date: 2026-05-26.
Scope: W5B research agent A, deletion topology and provider/template roster.
Output: `restart/skinny/tranches/sk-v14/research/skv14-W5B-A-deletion-roster.md`.
HEAD: `286233fa2` (`feat(sk-v14-waveW5A-redress): route runtime generation through source request`).

## Findings

W5A is admitted at HEAD, so the W5B entry dependency is satisfied. SPEC Section
8B makes W5B the PRUNE-3B deletion wave: delete the old per-grammar provider
mesh, delete seven CSS L4 template directories, close the post-W5 Lock 14
baseline, and keep W6/W7/W8/W9/W10 blocked until W5B admits.

Current provider topology:

```sh
find skinny/crates/codegen/src -maxdepth 1 -type f -name '*_provider.rs' | sort
```

Result:

```text
skinny/crates/codegen/src/css_l4_at_rules_and_media_provider.rs
skinny/crates/codegen/src/css_l4_declaration_values_extended_provider.rs
skinny/crates/codegen/src/css_l4_declaration_values_provider.rs
skinny/crates/codegen/src/css_l4_nested_layout_provider.rs
skinny/crates/codegen/src/css_l4_stylesheet_selectors_provider.rs
skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_provider.rs
skinny/crates/codegen/src/css_l4_visual_functions_provider.rs
skinny/crates/codegen/src/grammar_provider.rs
skinny/crates/codegen/src/json_provider.rs
```

The W5B exit count excludes only `grammar_provider.rs`, so the deletion/retire
set is eight legacy provider files: the seven `css_l4_*_provider.rs` files plus
`json_provider.rs`. No `bbnf_provider.rs`, `google_sheets_provider.rs`,
`csv_provider.rs`, `ebnf_provider.rs`, `math_provider.rs`, `bnf_provider.rs`, or
`css_pretty_provider.rs` exists at HEAD.

Current CSS template topology:

```sh
find skinny/crates/codegen/src -maxdepth 1 -type d -name 'css_l4_*_templates' | sort
```

Result:

```text
skinny/crates/codegen/src/css_l4_at_rules_and_media_templates
skinny/crates/codegen/src/css_l4_declaration_values_extended_templates
skinny/crates/codegen/src/css_l4_declaration_values_templates
skinny/crates/codegen/src/css_l4_nested_layout_templates
skinny/crates/codegen/src/css_l4_stylesheet_selectors_templates
skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_templates
skinny/crates/codegen/src/css_l4_visual_functions_templates
```

Those seven directories are W5B deletion-owned by SPEC Section 8B. The
`json_templates/` directory remains a live old-mesh dependency through
`json_provider.rs`; SPEC names only CSS template directories in the W5B exit
gate, but deleting `json_provider.rs` requires either retiring that directory
too or moving JSON generation to a non-provider request/generator path without
preserving a per-grammar template residue.

## Counts

```sh
find skinny/crates/codegen/src -maxdepth 1 -type f -name '*_provider.rs' | wc -l
# 9

find skinny/crates/codegen/src -maxdepth 1 -type f -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l
# 8

find skinny/crates/codegen/src -maxdepth 1 -type f -name 'css_l4_*_provider.rs' | wc -l
# 7

find skinny/crates/codegen/src -maxdepth 1 -type d -name 'css_l4_*_templates' | wc -l
# 7
```

## Risk

Deleting files by path is not sufficient. `grammar_provider.rs` still delegates
to `render_runtime_profile`, which still consumes `RuntimeProvider` and
provider-backed renderers. A W5B delete sweep without a provider-free generator
would break `regen-css`, JSON checks, and the seven companions.
