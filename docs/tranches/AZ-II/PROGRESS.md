# AZ-II — Progress Log

**Status**: planned (gated on AZ-I close)

**Date**: 2026-04-23

Dated execution log for tranche AZ-II. Execution begins after AZ-II
opens on AZ-I close (seven-point handoff contract verified).

AZ-II completes the direct-to-struct migration. BBNF's own grammar
moves to the `project_types`-derived struct path via a two-stage
bootstrap cutover (Stage A: tape-compiler builds struct-compiler
candidate; Stage B: candidate rebuilds itself; byte-equal output
is the close gate). Once BBNF parses into a derived struct, the
tape crate has no remaining consumers and is deleted.

Wave plan (three waves + FINAL): W0 bootstrap-cutover research +
classifier extension + AZ-II baseline → W1 Stage A (tape-compiler
builds struct-compiler candidate) → W2 Stage B (candidate rebuilds
itself + byte-equal close gate) → W3 FINAL — `crates/tape/`
deletion + parity recode + BA handoff.

Parent plan: `docs/tranches/AZ-II/AZ-II.md`.
Research: `docs/tranches/AZ-II/RESEARCH.md`.
Cutover design: `docs/tranches/AZ-II/BOOTSTRAP-CUTOVER.md` (lands W0).

## Gate summary

- **Byte-equal reproducibility**: Stage A output = Stage B output
  across every `.bbnf` fixture in the tree; zero byte differences.
- **Throughput**: Full 17-entry matrix at AU baseline or better on
  the struct-only path across JSON, CSS L4, Sheets, BBNF.
- **Coverage**: 100% `->` coverage fleet-wide; `StructRegistry`
  closed on every Named rule in every production grammar.
- **Tape-deleted**: `rg '^crates/tape/'` and `rg 'use bbnf_tape'`
  return zero matches; `cargo build -p bbnf --no-default-features`
  succeeds without `crates/tape/` existing.
- **Workspace**: ≥ 967 pass / ≤ 33 fail / ≤ 30 ignored.
- **Parity harnesses**: struct-vs-external-native on all four
  grammars; `tests/bbnf_bootstrap_reproducibility.rs` as
  permanent CI gate.

## Escape clause

Byte-equal failure at W2 close is a re-plan trigger, not a
partial-close. Full tape abrogation is binding repo policy; there
is no pre-declared "shrunken-tape" floor. On W2 failure, the wave
reverts its substrate, records drift evidence, and authors a
re-plan brief against that evidence. `feedback_no-workarounds-arch`
and `feedback_no-orthogonal-codepaths` forbid retaining a
tape-bearing substrate for BBNF alongside struct-only data
grammars, even under W2 pressure.

## 2026-04-28 — wave plan refined per W2-CLOSE-AUDIT

The original W0 / W1 / W2 / W3 four-wave shape collapses into a
single **AZ-II.cutover** wave per `docs/tranches/AZ-I/audit/W2-CLOSE-AUDIT.md`
§9. The W2-act activation pattern is reusable for BBNF without
further substrate work; Stage A / Stage B is two regen invocations
rather than a wave's worth of ceremony; tape deletion is mechanical
once `crates/tape/` has zero remaining consumers. Wave doc lands at
`docs/tranches/AZ-II/waves/cutover.md`. The W0 / W1 / W2 wave docs
carry supersede notices and stay on disk as historical record.

The cutover wave runs in three sequential sub-stages:

- **AZ-II.cutover.A** (cap 120 min) — `tape::dta` hoist to
  `bbnf-ir::dta` per `audit/AUDIT-6` §8.2; `tape::visitor` family
  deletion (746 LOC) per §8.3; tape driver dead-helper deletion per
  `audit/AUDIT-3` §6; BBNF typed-leaf authoring closes
  `StructRegistry` for BBNF; `crates/core/src/runtime/bbnf/`
  authored; resolver-arm extension for `BbnfBootstrap`. IR-side
  decay: `crates/ir/src/passes/recognizers/dta.rs` ~900 LOC
  amputation per `audit/AUDIT-3` §1.
- **AZ-II.cutover.B** (cap 60 min) — Stage A regen + Stage B
  byte-equal cycle. Permanent CI gate at
  `crates/core/tests/bbnf_bootstrap_reproducibility.rs`.
- **AZ-II.cutover.C** (cap 120 min) — `crates/tape/` deletion;
  cross-crate severance; view / pprint / @debug recode; parity
  harness recode; AZ-II FINAL.md; `docs/benchmarks/post-AZ-II.json`
  archive.

## Wave status

| Wave | Status | Headline |
|---|---|---|
| W0 | superseded (2026-04-28) | Folded into cutover.A (substrate hoist + BBNF runtime + decay sweep) |
| W1 | superseded (2026-04-28) | Folded into cutover.B (Stage A + Stage B byte-equal cycle) |
| W2 | superseded (2026-04-28) | Folded into cutover.C (`crates/tape/` deletion + recode + FINAL) |
| cutover | planned | BBNF self-host + tape deletion ([waves/cutover.md](waves/cutover.md)) |

## Handoff

- Opens on: AZ-I close (seven-point handoff contract verified).
- Closes into: BA (pointer queries on struct tree).
- BB opens on AZ-II close independently of BA's progress.
