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

Byte-equal failure at W2 close → `bbnf-tape-mini` escape: BBNF
bootstrap retains a shrunken tape (~4-5 modules instead of ~17);
full tape deletion deferred to follow-on tranche; remaining AZ-II
work (three-data-grammar path already landed via AZ-I; partial BBNF
migration; classifier extension) is retained.

## Wave status

| Wave | Status | Headline |
|---|---|---|
| W0 | planned | Bootstrap-cutover research + classifier extension |
| W1 | planned | Stage A — tape-compiler builds candidate |
| W2 | planned | Stage B — candidate rebuilds itself, byte-equal gate |
| W3 | planned | FINAL — tape deletion + parity recode + BA handoff |

## Handoff

- Opens on: AZ-I close (seven-point handoff contract verified).
- Closes into: BA (pointer queries on struct tree).
- BB opens on AZ-II close independently of BA's progress.
