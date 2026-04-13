# Tranche AT — Progress Log

Operational protocol: see `/INSTRUCTIONS.md` at repo root.

## Pre-AT (AS audit findings)

- **f64 payload does not fire for JSON** — scan_number_strict_f64
  runs but result discarded with `.map(|_| ())`. Number stored as
  compound Rule record, not scalar leaf with f64 payload.
- **bool payload does not fire for JSON** — same inlining issue.
- **KvPair never emits for JSON pair** — layout exists but emitter
  doesn't connect it.
- **Span payload admitted but unused** — no grammar triggers it.
- **StructRegistry always empty** — no pass populates it.
- **JSON regression AQ→AR: -14% to -39%** — meta Vec overhead,
  NOT modifier fix (JSON IR unchanged).
- **JSON bench not apples-to-apples with sonic-rs** — no string
  materialization.

## Phase 1 — Projection truth

Status: NOT STARTED

## Phase 2 — Regression redress

Status: NOT STARTED

## Phase 3 — String decode kernel + bench parity

Status: NOT STARTED

## Phase 4 — Profile-driven optimization

Status: NOT STARTED

## Phase 5 — Named struct ABI + cleanup

Status: NOT STARTED
