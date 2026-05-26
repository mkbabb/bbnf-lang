# SK-V14 W5B-FRONTEND A3: xtask CSS Runtime Dispatch

Date: 2026-05-26.
Scope: read-only inspection of W5B-FRONTEND §8B, `xtask` regen/check paths, and CSS L4 command gates.
Output: this file.

## §1 — Findings (concrete, file:line cited)

W5B-FRONTEND is a frontend/import/IR closure wave, not a deletion or
provider-free generator wave. SPEC requires `regen-css` and all seven CSS
companions to pass through the closure while provider/template topology remains
unchanged (`restart/skinny/tranches/sk-v14/SPEC.md:710`,
`restart/skinny/tranches/sk-v14/SPEC.md:738`,
`restart/skinny/tranches/sk-v14/SPEC.md:748`).

Current `regen-css` path:

- `cd skinny && cargo xtask regen-css` dispatches in `main.rs` to
  `regen_css::regen_css` (`skinny/xtask/src/main.rs:22`).
- `regen_css::regen_css` calls `regen::write_targets(root, TARGETS)`
  (`skinny/xtask/src/regen_css.rs:99`).
- `write_targets` validates, builds `runtime_request`, calls
  `codegen::emit_runtime_from_request`, and writes to each target output dir
  (`skinny/xtask/src/regen.rs:17`).
- Checks use the same request/emitter path and compare against generated dirs
  via `check_target` (`skinny/xtask/src/regen.rs:34`).

The frontend closure must be consumed at the `RuntimeGenerationRequest`
boundary before non-JSON materiality validation and provider rendering. Today
`emit_runtime_from_request` converts request sources into `grammar::RuntimeSource`,
calls `grammar::parse_runtime_source_facts`, validates materiality, then still
renders the profile through provider dispatch
(`skinny/crates/codegen/src/grammar_provider.rs:31`,
`skinny/crates/codegen/src/grammar_provider.rs:41`,
`skinny/crates/codegen/src/grammar_provider.rs:77`).

Current frontend support is only a fact scanner for W5A constructs, not full
parsing/lowering. `parse_grammar` only admits `@import` and `@token`; `@ws` and
`@pretty` remain unavailable as public syntax (`skinny/crates/grammar/src/lib.rs:309`).
The scanner detects `@ws`, `@pretty`, `?w`, `>>`, `<<`, projections, typed
projections, and host capture as runtime facts (`skinny/crates/grammar/src/lib.rs:141`,
`skinny/crates/grammar/src/lib.rs:188`).

## §2 — Recommendations (named falsifiability gates)

Run command gates from the skinny workspace:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo xtask regen-css
cargo xtask check-css-l4-at-rules-and-media
cargo xtask check-css-l4-declaration-values
cargo xtask check-css-l4-declaration-values-extended
cargo xtask check-css-l4-nested-layout
cargo xtask check-css-l4-stylesheet-selectors
cargo xtask check-css-l4-vendor-and-custom-atrules
cargo xtask check-css-l4-visual-functions
```

The cargo alias is the checked-in surface (`.cargo/config.toml:128`), and
`workspace_root()` searches for skinny metadata (`skinny/xtask/src/main.rs:1369`).
The seven companion commands are registered in `main.rs`
(`skinny/xtask/src/main.rs:23`) and rostered as seven `RuntimeTarget`s in
`regen_css.rs` (`skinny/xtask/src/regen_css.rs:26`). The companion roster unit
test confirms exactly seven distinct commands (`skinny/xtask/src/regen_css.rs:147`).

## §3 — Risks (REDRESS entries to pre-block)

- Do not delete or rename providers/templates in W5B. Current production
  rendering still has CSS provider arms (`skinny/crates/codegen/src/lib.rs:180`),
  and the profile registry still enumerates the seven CSS providers plus JSON
  (`skinny/crates/codegen/src/grammar_profile.rs:100`).
- Do not implement the provider-free generator body in W5B. W5B closes
  frontend/import/IR lowering and keeps existing provider-backed emission
  load-bearing for the same command gates.
- Do not make `@ws` or `@pretty` new public BBNF syntax.

## §4 — Sources (every external citation)

Local repository files only; no external sources used.
