# SK-V14 Wave W5C-GEN Plan: Provider-Free Runtime Generator Body

Date: 2026-05-26.
Wave: W5C-GEN PRUNE-3C.
Phase: plan.

## Inputs

- `restart/skinny/tranches/sk-v14/SPEC.md` Section 8C.
- `restart/skinny/tranches/sk-v14/research/skv14-W5C-A-lock14-owner-gate.md`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5C-B-provider-dispatch-graph.md`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5C-C-proof-gates.md`.
- W5B.4 close: `restart/skinny/tranches/sk-v14/research/skv14-W5B4-close.md`.

## Intervention

Replace live provider-backed runtime generation with one request/frontend-facts
generator body for request-owned runtime targets, while leaving provider and
template files as unreachable W5D-DELETE residue.

## Owner Paths

- `skinny/crates/bbnf-bench/src/lock14_baseline.rs` for the required W5C owner
  roster and parent-diff test.
- `skinny/crates/codegen/src/lib.rs`.
- `skinny/crates/codegen/src/grammar_profile.rs`.
- `skinny/crates/codegen/src/grammar_provider.rs`.
- `skinny/crates/codegen/src/runtime_generator.rs` as the new neutral generator
  module named by this plan.
- `skinny/xtask/src/main.rs`, `skinny/xtask/src/regen.rs`, and
  `skinny/xtask/src/regen_css.rs` only if request/verification routing needs
  adjustment.
- Generated runtime files under `skinny/crates/runtime/src/grammars/` only from
  fresh `cargo xtask regen-css`.
- `skinny/RESULTS.md` only if attribution is refreshed; otherwise no row
  maintain edit.

Provider modules and template directories are explicit non-owner paths for W5C.

## Falsifiability Gate

The redress slice must pass:

```sh
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w5c_gen_owner_paths_admit --profile ax-iter -- --exact
cargo test --manifest-path skinny/Cargo.toml -p codegen w5c_gen_rejects_profile_only_css_emission --profile ax-iter -- --exact
cargo test --manifest-path skinny/Cargo.toml -p codegen w5c_gen_css_runtime_output_depends_on_frontend_source_hash --profile ax-iter -- --exact
cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_json_request_matches_emit_from_source --profile ax-iter -- --exact
```

Then:

```sh
cd skinny
cargo xtask regen-css
cargo xtask check-css-l4-at-rules-and-media
cargo xtask check-css-l4-declaration-values
cargo xtask check-css-l4-declaration-values-extended
cargo xtask check-css-l4-nested-layout
cargo xtask check-css-l4-stylesheet-selectors
cargo xtask check-css-l4-vendor-and-custom-atrules
cargo xtask check-css-l4-visual-functions
cargo xtask check-json
cargo xtask gate-json --check-results --skv14-existing-results-capture
! rg -n '\b(render_runtime_profile|RuntimeProvider|GrammarProfile|json_provider|css_l4_.*provider)\b' crates/codegen/src/lib.rs crates/codegen/src/grammar_provider.rs
! rg -nU 'match\s+[^{]+\{[^}]*\b(Json|CssL4\w*|Bbnf\w*|GoogleSheets\w*)\b\s*=>' crates/{codegen,runtime,passes,bbnf,grammar}/src --glob '!**/tests/**'
find crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l
find crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l
```

The provider/template count commands may remain non-zero in W5C; they document
W5D residue.

## Hard Cap

W5C-GEN carries the SPEC Section 8C cap: <=1.0k C-1 part-A source/test LOC and
<=90 minutes redress. It cannot borrow W5D-DELETE or W6 budget.

## Revert Protocol

If provider reachability, source-derived byte proof, or the companion checks
fail, revert the W5C source slice as one patch, preserve W5A and W5B-FRONTEND,
and record the rejected patch path in REDRESS. Provider/template files stay
untouched.

## Same-Wave Consumer

`emit_runtime_from_request` is the same-wave production consumer. `regen-css`,
the seven CSS L4 companion checks, `check-json`, `gate-json`, the provider
reachability grep, and the Lock 14 parent-diff exact test must all consume the
new W5C generator body before the wave can close.

## Pre-Blocked Routes

- Provider/template deletion before W5D-DELETE.
- Static centralization of old provider output.
- Reading committed generated runtime output as source truth.
- Preserving live `RuntimeProvider` / `GrammarProfile` / provider dispatch for
  compatibility.
- New grammar-name branches in generic crates.
- Borrowing W5D-DELETE or W6 budget.
