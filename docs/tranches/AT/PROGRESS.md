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

### Profiling results (pre-AT baseline)

**JSON** (from prior analysis):
- 10 `.map(|_| ())`, f64 payload never fires, bool never fires
- KvPair never emits, 8 push_compound, 1 push_leaf, 1 push_leaf_with_u8

**CSS L4** (133K expanded lines, 76 functions):
- 234 push_compound, 21 push_leaf, 1 push_leaf_with (91% compound)
- 202 `.map(|_| ())` — mostly WS discard
- 319 scan_ws_block_comments (89% of scanner calls)
- Zero CSS fused scanners (css_ident_fast, css_number_scan_f64 absent)
- Beats lightningcss 4-6x, cssparser 1.1-1.4x

**Google Sheets** (17K expanded lines, 56 functions):
- 37 push calls, ALL push_compound — zero push_leaf, zero typed payloads
- 50 `.map(|_| ())`
- Only digit scanners — no scan_ident, no scan_quoted
- 97-130 MB/s parse, 42-49 MB/s format

**BBNF self-hosting** (25K lines, 106 functions):
- 90 push_compound, 15 push_leaf — 86/14 split
- 106 `.map(|_| ())` — one per function
- Parse is 1-2% of compile time — not a bottleneck
- ebnf slowest (172 MB/s), css_pretty fastest (490 MB/s)

**Key insight**: the projection system does not fire for ANY grammar.
Every grammar uses >85% push_compound with near-zero typed payloads.
The `.map(|_| ())` pattern is universal (10-202 per grammar).

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
