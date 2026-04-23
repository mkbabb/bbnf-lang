# Era II — Foundations (2026-02-26 → 2026-03-15)

Formal tranche discipline does not yet exist in Era II. The era's signature
is *monorepo scaffolding* and *first-generation IR extraction*. It begins
with commit `cc499979` (2026-02-26) `refactor: restructure as bbnf-lang
monorepo` and closes at `ba07ffc7` (2026-03-15) `docs: update bbnf-lang
CLAUDE.md and README.md for IR architecture and new exports`.

Commit count: roughly 264 commits across the 18-day span (February: 30,
March 1–15: ~234), per `git log --all --date=short` with the date
window filter. No `docs/tranches/` directory exists yet. No `PROGRESS.md`,
no `FINAL.md`, no wave schedules. The engineering cadence is linear
feature work; the "tranche" concept as a plan-document + wave-executed
unit emerges only at the Era-II / Era-III boundary.

## Preceding prelude — Era I

Era I is 25 commits from March 2023 (`0b1e1f3e` through `9ea91380`),
followed by a three-year hiatus. The 2023 work is a TextMate grammar
port plus first-pass LSP — entirely vestigial to what came later, but
the LSP + prettier-plugin surface area reappears in Era II's scaffolding
(`f5e1b241` extracts the prettier plugin, `4f75dad4` strengthens LSP
integration tests). Era I is not a tranche era; its artefact is the
existence of `extension/` + `parse-that` + a grammar notation that
survived verbatim into the bbnf-lang monorepo.

## The Era II architectural thesis

Grammar is a standalone input language; the Rust compiler is one backend
among several (TypeScript, WASM, LSP). Grammar authorship and codegen
are to be decoupled by an *intermediate representation* — a canonical,
interner-backed, DAG-stable form from which every backend emits.

Evidence:

- `1710d6f7` (2026-03-15) `feat: add bbnf-ir crate with canonical grammar
  IR, bytecode compiler, and interpreter`.
- `29b17895` (2026-03-15) `feat: add IR lowering pipeline and refactor
  Rust codegen to IR-based architecture`.
- `e1ffce5e` (2026-03-15) `feat: add TS dispatch codegen, WASM bridge,
  and import-loader extraction`.

The crate layout crystallised on the last day of Era II:
`bbnf-ir` (canonical grammar IR + interpreter), `bbnf-derive` (proc-macro),
`extension/` (LSP + prettier), `playground/` (TS + WASM demo),
`crates/core/` (Rust codegen), `parse-that` (runtime combinators).

## What landed durably

- **Monorepo skeleton.** The `crates/` hierarchy, `parse-that` git-
  submodule wiring, and workspace Cargo.toml persist to 2026-04-22
  essentially unchanged in shape.
- **IR as the single source of truth.** `29b17895` cements the rule
  that all backends lower through `bbnf-ir`. This thesis is upheld
  through every subsequent era and is explicitly restored every time
  it drifts (Tranche AQ.5's `PayloadKind → TypeDesc` consolidation
  in Era IV is a direct re-anchoring to Era II's invariant).
- **Grammar notation frozen.** The `.bbnf` surface language — EBNF with
  `->` type annotations, `@import`, `@host`, regex-bearing leaves —
  stabilises in Era II and is treated as inviolable by every later
  tranche.
- **Google Sheets formula grammar.** `0708455e` adds Sheets as a third
  grammar fixture alongside JSON and CSS. It remains in the bench
  matrix through Era VI.
- **Playground composables.** `4b7eb1df` splits the Vue playground into
  reusable composables. The `bbnf-buddy` mascot and later playground
  work all build on this refactor.

## What was reverted or superseded

- **Bytecode interpreter inside bbnf-ir.** `1710d6f7` shipped a bytecode
  compiler + interpreter. The interpreter was never the production
  path; `29b17895` introduced direct Rust codegen on the same day.
  Vestiges of the bytecode surface linger until the e-graph rewrite
  substrate of Era III supersedes them wholesale.
- **WASM VM module.** `980eef55` split `wasm/src/lib.rs` into analysis,
  gorgeous, lsp, and vm submodules. The vm submodule becomes a dead
  artifact in Era IV once tape-first codegen takes over — but the
  split itself survives.

## Salvageable artefacts still present at 2026-04-22

- `crates/ir/src/` (the Era-II crate name was `bbnf-ir`; renamed to
  `crates/ir/` during the AX.W0b crate-prefix purge).
- `crates/core/src/backend/` structure (backend-agnostic codegen was
  established in Era II and held).
- `grammar/json/*.bbnf`, `grammar/css/*.bbnf`, `grammar/google-sheets/
  *.bbnf` — the grammar tree shape is Era-II.
- The Sheets formula grammar itself (`grammar/google-sheets/formula.bbnf`)
  exists because of `0708455e`.
- `extension/` (LSP + prettier) layout.

## Transition into Era III

Era III opens on 2026-04-08 with `a3fadf56` `refactor(backend):
pre-solve delim_scan + key_dispatch per-grammar (Tranche F)` — the
first commit that carries a *tranche letter* in its subject. The
intervening gap (2026-03-16 → 2026-04-07, roughly three weeks) is
real calendar work on IR optimiser + e-graph substrate whose commits
bundle under tranches F–U once tranche discipline lands retroactively.

The decision that opened Era III is invisible in any single commit but
readable in shape: the IR-as-source-of-truth invariant forced a
tranche-structured approach because optimiser passes, CSP substrate,
and regex HIR all needed independent planning surfaces. Era III names
each optimiser concern with its own letter.
