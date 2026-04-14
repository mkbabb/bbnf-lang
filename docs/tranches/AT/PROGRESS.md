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
- **11/22 tape_parity golden fixtures stale** — root_variant_idx
  changed in AS enum reordering. Not functional regression.
- **Dead code from AS**: has_scalar_payload_type (never called),
  META_IDX_ZERO in repeat.rs, unreachable Span arms in grammar.rs.
- **cssparser bench misleading**: `parse_declarations() -> false`
  means it tokenizes only. lightningcss comparison (4.2x) is real.
- **CSS L4 edge cases**: 47/49 pass. `|=` attr selector ambiguity
  and ASCII-only identifiers are spec gaps.
- **Capacity heuristic regressed** — Span admission (8258576)
  reverted AR.5.1's `len/2+2` back to `saturating_mul(4)`.
- **regex_classify test has stale Identifier field** — references
  `allows_escapes: false` on Identifier but field was restructured.
- **Prior tranche gates**: all 12 PRESENT. 336 tests pass, 0 fail.
  Egraph has 6 clones (claim was 5).

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
