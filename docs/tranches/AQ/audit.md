# Tranche AQ — Audit of Prior Tranches (AA → AP)

## Executive Summary

Three systemic gaps survive the AA→AP arc:

1. **Structural dispatch is dead code**. Built across AO.0.1–0.6, attempted in AP.1b, gated OFF at `crates/core/src/generate/mod.rs:61`. The reason given ("pre-scan overhead without WS elision") is TRUE for the current implementation but NOT inherent: `filter_quote_parity` is a scalar backscan loop (~4ms on citm — 4× parse time), the hybrid dispatch duplicates match arms, and WS elision (the thing that would pay for the pre-scan) is explicitly disabled in ws.rs:41-45. Fix all three and structural dispatch is profitable.

2. **Direct-to-struct projection does not exist**. What we have is "tape + f64/bool/u8 side-channel payloads + lazy cursor views." The three-tier AF/AI architecture was deleted in AM.1 (−2,306 LOC) because the separate-ABI approach killed itself via `reconcile_cross_component_tiers`. AL-prototype-2's diagnosis was correct: unified ABI with tape pass-through. Never implemented. Payload coverage today: 1/7 JSON rules (~14%), ~1/265 CSS L4 rules (<0.4%).

3. **Language-specific overfitting persists**. 5 of 13 `RegexClass` variants are language-named (`JsonString`, `JsonNumber`, `WsBlockComment`, `CssIdent`, `CssQuotedString`). The `classify_known_pattern` fast path uses exact-string dictionary match on JSON/CSS canonical patterns. Kernel emission has `emit_json_call`. 7 `sp_json_*`/`sp_css_*` SpanParser constructors still live in `parse-that/src/span_parser/constructors.rs`. Scanner functions `scan_json_number_span`/`scan_json_number_fused`/`scan_json_number_f64` are nominally JSON when the distinguishing parameter is just "reject leading zero."

## Tranche-by-tranche status

| Tranche | Key deliverable | Landed | Deferred |
|---------|----------------|--------|----------|
| AA | Substrate awakening, tape scaffolding | TypeDescInterner, GrammarAnalysis, Lattice, csp-solver, `bbnf-tape` leaf crate | Structural bitmap, cross-rule CSP coupling (substrates without consumers) |
| AB | MaterializationClass, CSP joint solve | AB.0-AB.2a substrate | Emitter rewrite split out |
| AC | Atomic tape transposition | Every rule→`Option<TapeOffset>`, view layer | Clean bootstrap regen (deferred indefinitely) |
| AE | Shape-agnostic lowering | 4 tape walk primitives, directive API consolidation | Clean regen |
| AF | EmissionTier, universal cost model, 3-tier emission | AF.1-AF.5 infrastructure | **AF.6 Tier B emitter + DirectSlot** (deferred to AI) |
| AG | Build parallelism, CSP tier variables | 33→4 binaries, CSP active | **AG.2/AG.3 Tier B emitter** (deferred to AI) |
| AI | Wire EmissionTier into emitter | AI.1-AI.6 landed | **Strategically dead** — reconcile_cross_component_tiers widens all Direct→Tape |
| AJ | Zero-alloc TapeCursor, leaf-like Alt promotion | AJ.0-AJ.3 landed | — |
| AK | Flat Vec tape, per-branch `__branch_idx` | AK.0-AK.2 landed (+10-14%) | — |
| AL-prototypes 1-4 | Discussion docs | **No AL execution tranche** — subsumed into AM/AN/AO/AP | Entire AL plan |
| AM | Tape purity + SIMD parity | EmissionTier deleted (-2,306 LOC), payload buffer, per-branch surgery, SIMD string | AM.5.3 structural bitmap integration, AM.6 cost calibration |
| AN | Correctness + f64 payload + @ws SIMD | AN.0-AN.1, AN.4.2, Phase 0 F64 payload projection, serialize tape-first rewrite | **AN.2 scanner generalization**, AN.3 single-pass string scan, AN.5 32-byte SIMD, AN.6 instrumentation |
| AO | Structural dispatch primary lever + scanner generalization | AO.0.1 IR pass, AO.0.4-0.6 codegen, AO.2.2 number scanner consolidation | **Structural mode NEVER ACTIVATED** (AO.md header says "code complete, never exercised"). AO.2.1 CSS re-export delete partial. AO.2.5 sp_json/sp_css delete NOT landed. AO.2.6 nibble-LUT dedup NOT landed. AO.3 SIMD widening, AO.4 cost calibration, AO.4.2 global CSP, AO.5.1 CSS tailwind (landed in AP) all deferred. |
| AP | Correctness + enrichment + profile | AP.0.1-0.4 fixes, AP.1b peek-only redesign, AP.2 Bool/U8 payload, AP.3.1 SIMD WS bitmap, AP.3.3 SIMD string, AP.4 CSS L4 key dispatch (+35-50%), AP.5.1-5.3 scanner surgery | **AP.1 activation FAILED/DISABLED**, AP.3.2 trim elision, AP.3.4 SIMD filter_quote_parity, AP.4.2 pattern hoist, AP.4.3 CSS L4 type errors, AP.4.4 CSS L4 structural investigation, AP.5.4 UTF-8 defer, AP.5.5 TapeBuilder default prealloc, AP.6.4 cost sweep, AP.6.5 global CSP |

## Critical Deferred Items

### 1. Structural dispatch — dead infrastructure

Per-dispatch cost (measured when activated in prior agent runs): **15-25 cycles/dispatch**. Pre-scan overhead on citm: **~4-5ms** (filter_quote_parity scalar backscan is ~4ms alone). Current parse time: **~920µs**. Pre-scan costs 5× the parse budget.

But: WS is **50% of citm runtime**. Eliminating it via structural index jumps would dwarf the pre-scan cost. The current implementation refuses to elide WS (see `ws.rs:41-45` comment: *"NOTE: structural mode only accelerates Alt dispatch points... the whitespace trim is still required"*). This comment is **wrong** — between two structural positions, by construction all bytes are non-dispatch and, via `filter_quote_parity`, non-string-interior. So WS is the only possibility and is safe to skip by jumping `state.offset = structural_index[cursor]`.

Three specific bugs in the implementation:
- `filter_quote_parity` uses a scalar backwards-backslash scan per quote (O(input × quotes)). simdjson does the same work in SIMD via `prefix_xor(quote_bits) ^ prev_instring`.
- `alt.rs:121-150` emits hybrid dispatch with duplicated match arms AND always calls `sync_structural_cursor_to_offset`, even when cursor is already synced.
- `alt.rs:193-204` checkpoint mode saves `state.offset` but NOT `state.structural_cursor`. On backtrack the cursor desyncs and every subsequent Alt falls through to the slow path.

### 2. Clean bootstrap regen (deferred across AE/AF/AG/AI/AM/AN/AP)

`crates/core/src/grammar/generated.rs` still hand-patched. AP.0.1 fixed consumer code but not regen itself. Grammar roundtrip gate (`crates/core/tests/grammar_roundtrip.rs:47-52`) still has `usize::MAX` sentinels; tests still `#[ignore]`-gated.

### 3. Direct-to-struct projection

See Executive Summary #2. AL-prototype-2 identified the correct fix (unified ABI with tape pass-through) but it was never implemented. AM chose the simpler path (delete Tier B entirely, use payload side-channel). Today we have payload coverage on ~14% of JSON rules, <1% of CSS L4 rules.

### 4. Language-specific overfitting

Five `RegexClass` nominal variants. Six scanner functions with language prefixes. Seven `sp_json_*`/`sp_css_*` wrappers. Two hardcoded byte sets (`STRUCTURAL_PUNCTS: &[u8] = b",:{}[]"` in `punct_ws_region.rs:30`). `emit_json_call` kernel entry. Full inventory in §5 of this audit.

### 5. Generalization debt in IR walkers

`single_byte_literal` duplicated 3× (delim_scan, balanced_wrap, separator_list). `unwrap_wrap` duplicated 2×. `extract_leading_literals`, `extract_leading_regex_pattern`, `resolve_to_seq` live as private helpers in `key_dispatch.rs` but are fully general IR walkers.

### 6. Global CSP solve — never landed

AL-prototype-1, AL-prototype-2, AO.4.2, AP.6.5 all describe per-component solve as inadequate. CSS L4 still hits the 1M node budget fallback in compile (11.95s → fixed by AM.0 to 9.94ms but architectural pathology remains).

### 7. Cost model grid sweep — never landed

AM.6, AO.4.1, AP.6.4 all propose it. Never executed.

## Current Benchmark State (post-AP)

### JSON
| Dataset | MB/s | sonic-rs | Gap |
|---------|------|----------|-----|
| canada | 1,797 | 1,540 | **+17% BEAT** |
| citm | 2,712 | 3,097 | -12% |
| twitter | 2,173 | 2,736 | -21% |
| data | 1,900 | 2,450 | -22% |
| data_xl | 1,341 | 1,520 | -12% |

### CSS L4
| Dataset | MB/s |
|---------|------|
| normalize | 978 |
| bootstrap | 505 |
| tailwind | 534 |

### Hot Paths (samply, citm citm 3382 samples)
| Self% | Function |
|-------|----------|
| 56.4% | `<JsonParser>::__value` |
| 28.2% | `<JsonParser>::__pair` |
| 11.9% | `trim_leading_whitespace_scan_and_cache` |

Note: post-AP samply shows WS scan reduced from 50.4% (pre-AP) to 11.9% thanks to AP.3.1 SIMD WS bitmap and AP.3.3 SIMD string scanner. Most citm time now is in the two parser functions themselves.

### CSS L4 Hot Paths
| Dataset | Top function | Self% |
|---------|-------------|-------|
| bootstrap | `__declaration` | 33.2% |
| normalize | `__compoundSelector` | 40.2% |
| tailwind | `__compoundSelector` | 36.6% |
| tailwind | `__declaration` | 31.0% |

CSS L4 dispatch is 60-70% of runtime — `__compoundSelector` does hand-rolled byte-by-byte identifier parsing instead of calling `scan_ident`. This is the #1 CSS L4 optimization opportunity.

## sonic-rs Architectural Findings

Key insights from static analysis + nm of the competitors bench:

1. **sonic-rs REJECTS the two-stage (simdjson) model** explicitly. Their README states: "we do not use the two-stage SIMD algorithms from simd-json." Single-pass recursive descent.
2. **Direct-to-Value via JsonVisitor trait** — no intermediate tape, no token list. Visitor callbacks directly append to a linear buffer.
3. **Arena via bumpalo** — entire DOM lives in a `Bump`. Free is O(1) (drop the arena).
4. **Pre-allocated TLS Vec** — `nodes ≤ json.len()/2 + 2` heuristic. Thread-local recycled, so second+ parse allocation cost is zero.
5. **64-byte padded input buffer** with `x"x\0...` pattern — SIMD loads at EOF without OOB.
6. **`skip_space` bitmap caching** — after first SIMD scan, caches non-space bitmap + start offset; next call reuses via `trailing_zeros()`. HUGE win on pretty JSON.
7. **In-place string unescape** — modifies input buffer. Zero allocation. 16-byte NEON SIMD unescape-memcpy.
8. **16-byte tagged Value** — single 16-byte record, tag + meta + payload union. Only 7 dynamic types so fits cleanly.
9. **`simd_str2int` for float fractions** — x86_64 only (no NEON). Parses exactly 17 significant digits (enough for f64 mantissa). Truncates rest.

**Portable techniques (HIGH-VALUE):**
- Pre-size output Vec with grammar-derived constant
- TLS-recycled scratch
- `skip_space` bitmap caching (port to our `comment_ws` kernel)
- 2-byte scalar WS fast path before SIMD
- Port `simd_str2int` to NEON (sonic-rs doesn't have it — if we do, we beat canada outright)

**JSON-specific (SHOULD NOT port — overfitting):**
- 16-byte tagged Value union (JSON has only 7 dynamic types)
- Object as `Vec<Pair>` with O(n) lookup
- `from_slice_unchecked` UTF-8 skip
- `LazyValue` / `RawStr` / JSONPath model

## Recommended AQ Scope

Based on the 6-agent audit, the AQ tranche must close in priority order:

### P0 — Close the dead infrastructure
1. Structural dispatch: **activate with proper WS elision OR delete entirely**. AP gated it OFF as a compromise; AQ must either ship the complete feature (elision + fused filter + proper checkpoint save) OR delete `compute_structural_bytes`, `sync_structural_cursor_to_offset`, `current_structural_byte`, `scan_structural`, `filter_quote_parity`, pre-scan codegen, structural-aware dispatch, WS elision — the entire infrastructure.
2. Clean bootstrap regen. End the deferral chain. Un-ignore `grammar_roundtrip` tests, freeze the rule counts.

### P1 — Deoverfit
3. Collapse `RegexClass::JsonString`/`JsonNumber`/`WsBlockComment`/`CssIdent`/`CssQuotedString` into structural variants with parameters (`reject_leading_zero`, `allows_u_escapes`, `allows_leading_dash`, etc.).
4. Delete `classify_known_pattern` exact-string dict + `*_PATTERNS` constants.
5. Rename `scan_json_number_*` to `scan_number_strict_*`. Move `quoted_string_scan_full` out of `parsers/json.rs`.
6. Delete 7 `sp_json_*`/`sp_css_*` SpanParser constructors. Use structural forms.
7. Parameterize `STRUCTURAL_PUNCTS` byte set from `ir.structural_bytes`.
8. Deduplicate IR walkers into `bbnf_ir::passes::inspect::{walk, unwrap, resolve, literal, leading}`.

### P2 — Ship direct-to-struct
9. Unified ABI direct-to-struct per AL-prototype-2. All rules take `(state, tape)`. "Direct" rules emit `push_leaf` without `mark_children`; Tape rules emit `push_compound`. No separate ABI. View accessors return typed structs/enums via tape+payload.
10. Extend `PayloadKind` to `U16`/`U32` for hex colors and larger enum discriminants.

### P3 — CSS L4 performance
11. Refactor `__compoundSelector` to call `scan_ident` kernel (instead of hand-rolled byte-by-byte loop). Expected +30-50% on CSS parse.
12. `__declaration` key dispatch ladder — length-bucketed perfect hash instead of linear array-equality chain.
13. SIMD-ify `scan_ws_block_comments_slow` (currently 9-13% self-time cold path).

### P4 — Port sonic-rs techniques
14. Port `simd_str2int` to NEON for float fractions. Direct win on canada/data_xl.
15. Add `skip_space` bitmap caching to `comment_ws` kernel.
16. Pre-size Vec allocations with grammar-derived constants.

### P5 — Operational closure
17. Global CSP solve.
18. Cost model grid sweep with instrumentation.
19. `BBNF_EGRAPH_REPORT=1`, `BBNF_CSP_REPORT=1` release-build instrumentation.
