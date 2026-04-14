# Tranche AU — Progress Log

Operational protocol: see `/INSTRUCTIONS.md` at repo root.

## Pre-AU (AT audit findings)

### Audit methodology

6 parallel worktree-isolated agents performed deep analysis:

1. **Codegen regression audit** — expanded AQ vs AT JSON parsers,
   identified dead payload captures, quantified overhead sources
2. **Projection activation audit** — all 4 grammars push counts,
   payload firing, KvPair status, .map(|_| ()) catalog
3. **Prior tranche gates** — full test suite, bootstrap idempotency
4. **JSON hot path profile** — instruction counts, push method costs,
   `branch_pushes_children` misclassification root cause
5. **CSS+Sheets+BBNF profiles** — scanner activation, fused scanner
   absence, per-grammar bench numbers
6. **Deferred items + arch debt** — 11-tranche ParsedGrammar deferral,
   dead StructRegistry, schema stubs

### Root cause of JSON regression

`branch_pushes_children()` in `alt.rs:67-69` has `_ => true` catch-all
that misclassifies nested leaf Alt nodes (inlined `bool` rule) as
compound. Tape surgery forces `mark_children + push_compound` on ALL
branches, making ALL payload captures dead stores. The f64/bool/u8
payloads are computed but never stored — a correctness bug.

### Architectural verdicts from audit

- **kind_meta packing: KEEP** — strictly better than meta Vec
- **__payload_tag match: KEEP** — clean architecture, negligible cost
- **payload Vec pre-alloc: REVERT** — 2.1MB wasted per JSON parse
- **branch_pushes_children: FIX** — single root cause of regression
- **ParsedGrammar: ELIMINATE** — 11 tranches, BLOCKING
- **StructRegistry: IMPLEMENT OR DELETE** — dead scaffold

## Phase 1 — Fix projection activation

Status: NOT STARTED

## Phase 2 — CSS scanner activation

Status: NOT STARTED

## Phase 3 — String decode + honest JSON bench

Status: NOT STARTED

## Phase 4 — Accumulated debt elimination

Status: NOT STARTED

## Phase 5 — Profile-driven optimization + bench parity

Status: NOT STARTED
