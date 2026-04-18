# AX Progress Log

## 2026-04-18 — Tranche open

AX opens on master at commit `2faedca5` (`docs(instructions): tranche/
subdir + WAVE_SPEC per-wave format`). Successor to AW-V close
(`a12618ea`). Plan `AX.md` + eighteen `waves/W<N>.md` specs live on
master prior to first execution commit, per the plan-on-master
invariant.

**State at open.** 0/17 parse entries exceed post-AU per AW-V.W6
close. JSON routes through shape dispatcher; CSS/Sheets/BBNF
delegate to `__dta_walker_inline::run`. `has_w4_classified` gate at
`crates/core/src/backend/rust/emitter/grammar.rs:718` over-restricts
JSON's visitor path. Eighteen waves target repair, interpreter
deletion, value API, parity harnesses, lever portfolio, e-graph
grammar rewriting, closing with AY handoff.

**Sequencing.** W0a opens immediately. W0a is 2-serial (gate narrow
→ routing + wire-contract). W0b is 4-parallel (interpreter deletion
+ emit/ purge + crate renames + test carve). W0c is 1-serial
(AW-V.md rewrite). W1 onwards proceeds per the per-wave specs.

**Orchestration ledger starts here.** Every wave boundary appends an
entry with commit hashes, verification artefacts cited, and re-plan
notices if any. Hard-gate closures cite the artefact, not the claim.

---

## 2026-04-18 — W0a.1 dispatch

Agent dispatched to `bbnf-wt-ax-w0a-1` for
`has_w4_classified` narrowing at
`crates/core/src/backend/rust/emitter/shapes/dispatcher.rs:836`.
Scope: replace the `is_w4_classified()` call with explicit
`matches!(tag, ShapeTag::Pratt | ShapeTag::Unordered)`; verify JSON
re-emits `parse_with_visitor` via `cargo expand`; regen bootstrap
idempotent.
