# SK-V14 W4-B: Provider Deletion Cycle

Date: 2026-05-26.
Wave: W4 PRUNE-2.
Phase: research.

## Finding

W4 as written has a dependency cycle:

1. W4 must delete seven CSS L4 provider modules and seven template
   directories.
2. W4 must then run `cargo xtask regen-css`.
3. The current W2 `regen-css` implementation calls
   `codegen::emit_runtime_profile(...)`.
4. `codegen::emit_runtime_profile(...)` compiles and dispatches through the
   same seven CSS L4 provider modules W4 is required to delete.
5. The generic provider collapse that removes those modules is W5's task, but
   W5 requires W4 admitted.

This is the same structural class as REDRESS-183: a wave requires a generator
capability whose owner is sequenced after that wave.

## Live Source Evidence

- `skinny/xtask/src/regen.rs:18` calls
  `codegen::emit_runtime_profile(target.profile)`.
- `skinny/crates/codegen/src/lib.rs:1-7` imports the seven CSS provider
  modules.
- `skinny/crates/codegen/src/lib.rs:166-208` dispatches every CSS profile to
  one of those provider modules.
- `skinny/crates/codegen/src/grammar_profile.rs:1-5` imports the same seven
  providers.
- `skinny/crates/codegen/src/grammar_profile.rs:100-110` registers the CSS
  profiles by calling each provider's `runtime_profile()`.
- `restart/skinny/tranches/sk-v14/SPEC.md:633-658` assigns the
  trait-dispatch / grammar-agnostic provider collapse to W5.
- `restart/skinny/tranches/sk-v14/SPEC.md:646-648` makes W5 conditional on
  W4 admission.

## Executable Probe

The failure was reproduced in a throwaway worktree at
`/Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe`, then the worktree was
removed.

Probe command:

```sh
git worktree add --detach /Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe HEAD
rm -rf /Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe/skinny/crates/codegen/src/css_l4_*_templates \
  /Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe/skinny/crates/codegen/src/css_l4_*_provider.rs \
  /Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe/skinny/crates/runtime/src/grammars/css_l4_*
(cd /Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe/skinny && cargo xtask regen-css)
```

Failure:

```text
error[E0583]: file not found for module `css_l4_at_rules_and_media_provider`
 --> crates/codegen/src/lib.rs:1:1

error[E0583]: file not found for module `css_l4_declaration_values_extended_provider`
 --> crates/codegen/src/lib.rs:2:1

error[E0583]: file not found for module `css_l4_declaration_values_provider`
 --> crates/codegen/src/lib.rs:3:1

error[E0583]: file not found for module `css_l4_nested_layout_provider`
 --> crates/codegen/src/lib.rs:4:1

error[E0583]: file not found for module `css_l4_stylesheet_selectors_provider`
 --> crates/codegen/src/lib.rs:5:1

error[E0583]: file not found for module `css_l4_vendor_and_custom_atrules_provider`
 --> crates/codegen/src/lib.rs:6:1

error[E0583]: file not found for module `css_l4_visual_functions_provider`
 --> crates/codegen/src/lib.rs:7:1
```

## Disposition

The W4 deletion cannot be forced without either:

- preserving the provider modules W4 is required to delete,
- deleting them and breaking the same-wave consumer,
- or hand-patching a replacement generator outside the W4 owner model.

All three routes are pre-blocked by `[no-workarounds]`, Lock 14, and the
SK-V14 clean-regen discipline.
