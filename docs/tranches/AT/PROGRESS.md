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

**JSON regression root cause** (verified from isolated worktree):
- **Primary: SIMD number scanner** (+31% on canada when disabled).
  85.6% of canada numbers have 2 integer digits — NEON path costs
  ~16 cycles for 2 digits vs ~10 for scalar SWAR. Fix: guard SIMD
  with digit-count threshold (skip for integer part, or require 9+
  remaining bytes).
- **Secondary: meta Vec** (~6-10% estimated). With SIMD disabled,
  canada is still 23% below AQ (1374 vs 1796 MB/s).
- **NOT a factor: capacity heuristic** (<2% impact).
- **Chronic waste: f64 discard** (~18% of parse time). Eisel-Lemire
  runs and result thrown away via `.map(|_| ())`. Exists in both
  AQ and AR — not a regression but a permanent tax that Phase 1
  projection fixes will eliminate.

**Key insight**: the projection system does not fire for ANY grammar.
Every grammar uses >85% push_compound with near-zero typed payloads.
The `.map(|_| ())` pattern is universal (10-202 per grammar).

## Phase 1 — Projection truth

Status: **COMPLETE** (commit 0ff06bc)

- Implemented `resolve_branch_type()` — walks inlined Map/Constant/
  FnDescriptor nodes to surface TypeDesc after rule fusing
- Replaced single `payload_type: Option<TypeDesc>` with multi-type
  `payload_types: Vec<TypeDesc>` supporting heterogeneous Alt branches
- Emit `__payload_tag` discriminator for multi-type Alts; epilogue
  generates match arms per type selecting `push_leaf_with_<T>`
- JSON `value` rule now captures f64 (number), bool (true/false), and
  u8 (null) via direct projection
- Zero `.map(|_| ())` on typed scanner returns in expanded JSON
- Deleted dead `has_scalar_payload_type` (never called)
- Verified: `push_leaf_with_f64`, `push_leaf_with_bool`,
  `push_leaf_with_u8` all appear in expanded JSON parser

Hard gates 1-5 satisfied. 34 tests pass.

## Phase 2 — Regression redress

Status: **COMPLETE**

- AT.2.1 SIMD guard: DONE (parse-that commit 44ae43b) — removed
  `scan_digits_simd()` from integer digit path, kept for fractional.
  85%+ of real-world numbers have short integer runs (1-4 digits).
- AT.2.2 meta_idx fold: DONE (commit c57ef27) — packed meta_idx into
  TapeRec kind_meta byte (5 bits), eliminated meta: Vec<u8>. TapeRec
  stays 16 bytes. 28 tape tests pass.

### Post-AT JSON bench results (all phases)

| Dataset | post-AS | Phase 1+SIMD | +meta fold | Delta vs AS |
|---------|---------|-------------|------------|-------------|
| canada | 1089 | 1387 | **1464** | **+34%** |
| citm | 2331 | 2577 | **2631** | **+13%** |
| twitter | 2003 | 2089 | **2155** | **+8%** |
| data | 1805 | 1843 | **1907** | **+6%** |
| data_xl | 1046 | 1167 | **1227** | **+17%** |

canada hard gate (≥1350 MB/s) exceeded at 1464 MB/s.

### CSS L4 bench results

| Dataset | post-AT | vs lightningcss |
|---------|---------|-----------------|
| normalize | 934 MB/s | ~7.5x |
| bootstrap | 516 MB/s | ~4.2x |
| tailwind | 553 MB/s | ~4.5x |

## Phase 3 — String decode kernel + bench parity

Status: IN PROGRESS — `decode_json_string_to_arena` implemented in
parse-that (commit accb3c0). 18 tests pass. Wiring into codegen
and honest bench TBD.

## Phase 4 — Profile-driven optimization

Status: PARTIAL — payload pre-alloc done. Fresh samply profiles +
NEON fractional scan deferred to profile-guided next wave.

vs sonic-rs comparison (tape-only, no string decode):
| Dataset | BBNF | sonic-rs | Ratio |
|---------|------|----------|-------|
| canada | 1469 | 1470 | 99.9% |
| citm | 2622 | 3042 | 86.2% |
| data | 1907 | 2387 | 79.9% |
| data_xl | 1208 | 1456 | 83.0% |
| twitter | 2163 | 2622 | 82.5% |

canada is parity with sonic-rs. Other datasets 80-86%.

## Phase 5 — Test + bench structural validation

Status: **COMPLETE** (commits bfef009, 71675dd)

- 17 deep structural tests in `tests/structural.rs`: parse known
  JSON inputs, walk tape records, verify spans/kinds/nesting/bounds
- `benches/common/validate.rs`: assert_record_count_range,
  assert_root_kind_compound, assert_root_covers_input
- JSON bench validation wired into pre-bench setup
- 73 tests total, 0 failures

## Phase 6 — Named struct ABI + cleanup

Status: PARTIAL

- AT.6.3 Dead code: DONE (commit 61cc0d9) — removed META_IDX_ZERO
  from repeat.rs, 7 dead fns from serialize.rs, SpanRaw variant from
  directives.rs. -166 LOC, 10 warnings eliminated.
- AT.6.4 Tape parity fixtures: DONE (commit 0ae68b1) — 22/22 pass.
  JSON root_variant_idx 0/1→9, CSS L4 record counts decreased
  (codegen optimization), EBNF record counts increased.
- AT.6.5 parse-that commits: DONE — SIMD guard (44ae43b), hex escape
  (6ae8e6a), string decode (accb3c0), decode fix (f96753a)
- AT.6.1-6.2 StructRegistry + named struct view: NOT STARTED
- AT.6.6 ParsedGrammar elimination: NOT STARTED

## Phase 7 — CSS spec parity

Status: **COMPLETE** (commit bdfaf1e + parse-that 6ae8e6a)

- AT.7.1: Fixed `|=` attribute selector ambiguity — new `attrName`
  rule with 4 ordered branches for namespace disambiguation
- AT.7.2: Extended `ident` and `selectorIdent` to accept non-ASCII
  bytes (≥0x80) per CSS Syntax L3 §4.3.10
- Added `\xHH` hex escape support in bbnf-regex character classes
  (required for `\x80-\xff` byte ranges)
- CSS_L4_RULE_COUNT updated 184→185 for new `attrName` rule
- All 21 CSS tests pass; bench: normalize 952, bootstrap 511,
  tailwind 552 MB/s
