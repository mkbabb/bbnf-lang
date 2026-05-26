# SK-V14 W6.0 A5: Root Xtask Regeneration

Date: 2026-05-26.
Scope: probe the root xtask shape needed for the W6.0 destructive regen command.
Output: this file.

## §1 — Findings (concrete, file:line cited)

1. Root `xtask` is clap-based and currently has a single `Regen` subcommand. `xtask/src/main.rs:23-66` dispatches only `regen::run(...)`.
2. `regen::run` either writes generated parser source or compares generated parser source in a tempdir. `xtask/src/regen.rs:196-222` has no runtime-output branch and no grammar-specific command.
3. The manifest helper already resolves the workspace grammar list. `xtask/src/regen.rs:225-236` reads `[workspace.metadata.bbnf.grammars]`, so a root `regen-css` should reuse that authority instead of duplicating grammar path resolution.
4. The generated parser output path is parser-specific. `xtask/src/regen.rs:204-213` writes `<ident>.rs` under `crates/core/src/grammar/generated`, not `crates/core/src/runtime/css_l4`.

## §2 — Recommendations (named falsifiability gates)

- Add a root `regen-css` subcommand that writes the root CSS L4 runtime collapse product.
- Keep parser regen separate: `cargo xtask regen --grammar css_l4 --check` continues to own `crates/core/src/grammar/generated/css_l4.rs` and sidecar JSON.
- Add a non-destructive unit/integration test for the root command shape if practical, but the decisive W6.0 gate is destructive: delete root CSS runtime, run `cargo xtask regen-css`, and require clean diff.

## §3 — Risks (REDRESS entries to pre-block)

- A root `regen-css` that shells into skinny `cargo xtask regen-css` would regenerate skinny fact streams and leave the root runtime absent.
- A root `regen-css` that edits parser generated files conflates W6.0 runtime collapse with parser regen and makes revert boundaries ambiguous.
- Duplicating grammar-manifest parsing outside `xtask::regen` risks drift between parser regen and runtime regen inputs.

## §4 — Sources (every external citation)

- Local repository only; no external sources.
