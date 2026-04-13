# Tranche AP — Correctness-First, Profile-Driven Optimization

## Audit Synopsis (Post-AO, 4-agent)

Profiling (samply 4000 Hz), cargo expand (JSON 2859 LOC, CSS L4
103,378 LOC), full tranche recapitulation (AA–AO), and structural
dispatch failure analysis. All findings cite concrete evidence.

---

## Profiling Results (samply, release, arm64)

### JSON twitter (631 KB, string-heavy) — 1,445 MB/s

| Rank | Self% | Function |
|------|-------|----------|
| 1 | **39.3%** | `trim_leading_whitespace_scan_and_cache` |
| 2 | 29.5% | `JsonParser::__value` |
| 3 | 25.4% | `JsonParser::__pair` |
| 4 | 4.0% | `memchr::memchr` (string scan) |
| 5 | 0.9% | `core::str::from_utf8` |

**Whitespace is 39% of JSON parse time.** The cold-path bitmap
construction (`scan_and_cache`) is scalar (8 bytes/iter). SIMD
(16-byte NEON `cmeq`) would halve this.

### JSON citm (1.7 MB, number+indent-heavy) — 1,818 MB/s

| Rank | Self% | Function |
|------|-------|----------|
| 1 | **54.1%** | `trim_leading_whitespace_scan_and_cache` |
| 2 | 27.5% | `JsonParser::__value` |
| 3 | 16.5% | `JsonParser::__pair` |

**Whitespace is 54% of citm.** Pretty-printed JSON with deep nesting
has proportionally more whitespace between tokens.

### CSS L4 bootstrap (280 KB) — 333 MB/s

| Rank | Self% | Function |
|------|-------|----------|
| 1 | **29.5%** | `__declaration` (7,409 instructions, 28-way backtrack) |
| 2 | **23.9%** | `__compoundSelector` (4,245 instructions) |
| 3 | 6.7% | `__value` |
| 4 | 6.3% | `scan_ws_block_comments_slow` |
| 5 | 3.4% | `__alignDecl` |

**CSS L4 is dominated by dispatch overhead (92%)**, not scanning.
`__declaration` alone is 29.5% — a 28-branch sequential checkpoint
cascade with no dispatch table.

---

## Cargo Expand Findings

### JSON (2,859 lines)

- **Structural dispatch: NOT ACTIVE.** Zero `advance_to_structural`
  calls. The structural mode codegen is wired but not triggering for
  the JSON derive test (structural_bytes populated at compile time
  but the runtime conditional `has_structural_index()` prevents
  activation — cursor is null because the pre-scan runs only when
  the emitted `parse()` wires it, which happens for the json_slab
  test struct).

- **Payload projection: DECLARED BUT NOT WIRED.** `__value` declares
  `__payload_f64`, `__has_payload` but **never writes to them**. The
  number branch calls `scan_json_number_span` (span only), not
  `scan_number_f64` (f64 capture). The `emit_regex_match_impl`
  payload detection fires at the `value` rule level (`ctx.payload_kind
  == F64`) but the inlined `number` body's regex is classified by
  `classify_regex` — and the consolidated `scan_json_number_span`
  function name no longer matches the classifier's expectation of
  `scan_number_f64`.

  **Root cause**: AO Phase 2.2 renamed `number_span_scan_strict` →
  `scan_json_number_span` in the kernel, but the payload wiring in
  `leaves.rs:emit_regex_match_impl` still calls `scan_number_f64`
  (the old standalone function). The classifier detects the pattern
  correctly but the emitted function name is from the kernel path
  (span-only), not the payload path (f64 capture).

- **24 trim_leading_whitespace_mut calls** (10 parse, 14 prettify).
  Structural WS elision is NOT active because structural dispatch
  is not triggering.

### CSS L4 (103,378 lines)

- **222 scan_ws_block_comments calls** — @ws kernel correctly routed.
- **113 is_ascii_whitespace uses** in inline separator patterns —
  these BYPASS the @ws kernel. In CSS, comments can appear in
  separator positions, so this is a **correctness bug**: `?w` around
  `:` and `,` in declarations uses plain ASCII WS instead of
  comment-aware scanning.
- **__namedColor: 4,970 lines** — trie-style dispatch of all 135 CSS
  named colors. Single largest function.
- **__declaration: 28-way sequential backtrack, NO dispatch table.**
  Each branch tries a property name, backtracks on failure. This is
  the #1 hot path (29.5% of CSS L4 runtime).
- **43 copies** of inline `ws + ':' + ws` scanner pattern.
- **42 copies** of `!important` pattern.
- **24 functions** declare `__payload_f64` but never use it.

### CSS L4 Tailwind Failure: Root Cause Found

Offset 387594: The value regex `/[^;!}]*/` in `properties.bbnf`
stops at `!` inside CSS comments like `/*!*/`. Tailwind uses
`var(--tw-empty,/*!*/ /*!*/)` — the scanner stops at the first `!`
in the comment, leaving the parser mid-value. The `!important`
optional fails to match `!*/`, so the declaration appears truncated.

**Fix**: Change value regex to `/(?:[^;!}]|!(?!important))*/` or
integrate the @ws comment scanner into value scanning.

**File**: `grammar/css/l4/properties.bbnf` line ~118.

---

## Tranche Recapitulation (AA–AO Consolidated)

### COMPLETE (stable, no action needed)

| Tranche | Deliverable |
|---------|-------------|
| AB–AC | Tape substrate + full transposition |
| AI | View-layer typed accessors |
| AJ–AK | Zero-copy tape + dispatch emission |
| AL | Self-hosting proof, serialize rewrite |
| AM | Per-branch surgery, EmissionTier deletion, payload buffer |

### PARTIAL (critical items deferred)

| Tranche | Done | Deferred |
|---------|------|----------|
| AF | Three-tier lattice infra (0–5) | **AF.6: Tier B direct emission** |
| AN | Payload projection, @ws SIMD, WS cache, LSP fix | Scanner generalization, SIMD hyper-opt, L4 tailwind |
| AO | Structural bytes pass, ParserState cursor, CSS wrappers deleted, number scanners consolidated | Structural dispatch v2, padded buffer, SIMD widening, cost calibration |

### BROKEN (must fix)

1. **Bootstrap compile error**: `BbnfBootstrapRuleKind::rhs` and
   `::value_expr` referenced in `deps.rs`/`metadata.rs` but not in
   generated parser enum. LSP agent (AN.0.4 commit `acb915e`)
   introduced EBNF rule names into BBNF bootstrap consumer code.
   
   **Fix**: Replace `rhs` → `alternation`, `value_expr` → check
   actual grammar rule kind names.

2. **Payload projection not activating**: `emit_regex_match_impl`
   emits `scan_number_f64` for payload capture but the kernel now
   emits `scan_json_number_span` (renamed in AO.2.2). The two paths
   diverge — payload path is dead.

3. **CSS L4 tailwind**: value regex `/[^;!}]*/` chokes on `/*!*/`
   comment syntax in Tailwind's `var()` expressions.

4. **CSS L4 separator correctness**: 113 `?w` separator positions
   use `is_ascii_whitespace` instead of the @ws comment-aware kernel.

---

## AP Phase 0: Fix ALL Regressions (BLOCKING)

### 0.1 Fix bootstrap compile error

Replace `BbnfBootstrapRuleKind::rhs` with `::alternation` in
`deps.rs` (line 105) and `metadata.rs` (lines 48, 76). Remove
`::value_expr` references in `expression.rs` line 551 — use the
existing `dispatch_value_expr` fallback that handles unknown kinds
via span-text inspection.

**Files**: `crates/core/src/graph/deps.rs`,
`crates/core/src/graph/metadata.rs`

### 0.2 Fix payload projection wiring

The number branch in `__value` calls `scan_json_number_span` (span
only) instead of `scan_number_f64` (f64 capture). The root cause:
AO.2.2 renamed the kernel function but the payload path in
`emit_regex_match_impl` (`leaves.rs`) still emits `scan_number_f64`.

**Fix**: In `emit_regex_match_impl`, when payload_kind is F64 and
the regex classifies as numeric, emit `scan_json_number_f64` (the
consolidated f64 scanner) instead of the deprecated `scan_number_f64`.

**Files**: `crates/core/src/backend/rust/emitter/leaves.rs`

### 0.3 Fix CSS L4 tailwind value regex

Change `/[^;!}]*/` → `/(?:[^;!}]|!(?!important))*/` in the
declaration value fallback pattern.

**File**: `grammar/css/l4/properties.bbnf`

### 0.4 Fix CSS L4 separator @ws bypass

The 113 `is_ascii_whitespace` calls in inline separator patterns
must use the @ws kernel when the grammar has a custom @ws pattern.
The `emit_with_ws_trim_impl` is correct but the inline separator
codegen path in `seq.rs` or `repeat.rs` hardcodes ASCII WS.

**Files**: `crates/core/src/backend/rust/emitter/seq.rs`,
`crates/core/src/backend/rust/emitter/repeat.rs`

### 0.5 Revert structural dispatch to byte-at-a-time

The AO.0 structural dispatch adds 15-25 cycles/dispatch via hybrid
branching. Revert to pure byte-at-a-time for now. Structural v2
redesign in Phase 2.

**Files**: `crates/core/src/backend/rust/emitter/alt.rs`,
`crates/core/src/backend/rust/emitter/grammar.rs`,
`crates/core/src/backend/rust/emitter/ws.rs`

### Verification: Phase 0
```bash
cargo test --workspace  # all tests compile and pass
cargo expand -p bbnf --test json_slab | grep push_leaf_with_f64  # payload activates
cargo test -p bbnf --test tape_parity -- css_tailwind  # tailwind parses
```

---

## AP Phase 1: Whitespace Optimization (39-54% of JSON time)

Samply shows whitespace is the #1 bottleneck for JSON. The cold-path
`trim_leading_whitespace_scan_and_cache` is scalar (8 bytes/iter).

### 1.1 SIMD bitmap construction in scan_and_cache

Replace the scalar `while i + 8 <= window_len` loop with NEON
`cmeq.16b` comparisons (16 bytes/iter). The existing Tier 1 fast
path already uses SIMD inline — extend the pattern to Tier 3.

**File**: `parse-that/rust/parse_that/src/scanners.rs`
**Expected impact**: -50% of scan_and_cache time → **-20-27% total JSON time**

### 1.2 Reduce trim call count

24 trim calls in JSON parse path. Many are in sep_by loops where
consecutive trims are redundant (trimming after a comma already
consumed the whitespace before the next value). Elide consecutive
trims via a `last_trim_offset` guard.

**File**: `crates/core/src/backend/rust/emitter/repeat.rs`
**Expected impact**: -30% of trim calls → **-5-10% total JSON time**

### Verification: Phase 1
```bash
samply record ... json_twitter  # trim_leading_whitespace < 20%
cargo bench --bench json_monolithic  # citm target: 2,500+ MB/s
```

---

## AP Phase 2: CSS L4 Performance (28-way backtrack → dispatch)

### 2.1 Dispatch table for __declaration

The 28-branch sequential backtrack in `__declaration` (29.5% of
CSS L4 time) can be converted to O(1) dispatch if the property
names have disjoint first bytes. Analyze the FIRST sets:
`color`, `display`, `margin`, `padding`, `font-size`, etc. — many
share first bytes (`m` for margin/max-width, `f` for font-*/flex-*).

Use **key dispatch** (scan ident → compare against known keys) to
reduce 28 sequential backtracks to 1 ident scan + hash lookup.

**Files**: `grammar/css/l4/properties.bbnf` (grammar restructure),
`crates/ir/src/passes/sets/dispatch/` (key dispatch enablement)
**Expected impact**: -20% CSS L4 time

### 2.2 Hoist duplicated patterns

43 copies of `ws + ':' + ws` and 42 copies of `!important` in CSS
L4 expanded output. Extract into shared helper functions.

**Files**: `grammar/css/l4/properties.bbnf`, codegen dedup

### 2.3 Delete CSS monolithic bench

Per user directive. CSS L4 is the only CSS bench that matters.

**File**: DELETE `crates/core/benches/css/monolithic.rs`

### Verification: Phase 2
```bash
cargo bench --bench css_l4  # bootstrap target: 500+ MB/s
```

---

## AP Phase 3: Structural Dispatch v2

Redesign based on AO.0 failure analysis. Key insight from samply:
whitespace is 39-54% of JSON time, not dispatch overhead. So the
real optimization is WS acceleration (Phase 1), not structural
pre-scan.

### 3.1 Entry-rule-only structural dispatch

Apply structural dispatch only to the entry rule's top-level Alt.
Nested Alts use byte-at-a-time. Eliminates hybrid branching at
every nesting level.

### 3.2 Size gating (>4KB only)

Pre-scan overhead is amortized for large inputs but dominates for
small inputs. Gate on `input.len() > 4096`.

### 3.3 Checkpoint cursor restore

Save/restore `structural_cursor` alongside `state.offset` in
checkpoint Alts. Currently broken — cursor desync on backtrack.

---

## AP Phase 4: Tier B Direct Emission (AF.6)

### 4.1 Emit __rule_direct for leaf rules

For rules classified as Tier B (leaf with scalar type, e.g.
`number → f64`), emit a second function:
```rust
fn __number_direct(state) -> Option<f64> {
    scan_json_number_f64(state)
}
```

### 4.2 View accessor fast path

The view's `.value()` calls the Tier B function when available,
falling back to span-text parse. Three-tier dispatch:
1. Payload buffer (O(1) read) — already wired
2. Tier B direct function — new
3. Span-text parse — existing fallback

---

## AP Phase 5: SIMD + Cost Calibration

### 5.1 32-byte SIMD chunks (x86_64 AVX2)
### 5.2 SIMD digit-to-integer
### 5.3 Cost model grid sweep
### 5.4 Global CSP solve for CSS L4

---

## Performance Targets (samply-validated)

### JSON (target: WS < 20% of profile)

| Dataset | Current | AP Target | sonic-rs | Key lever |
|---------|---------|-----------|----------|-----------|
| canada  | 1,792   | 2,000+    | 1,540    | WS SIMD |
| citm    | 2,042   | 3,000+    | 3,097    | WS SIMD + trim elision |
| data    | 1,650   | 2,300+    | 2,450    | WS SIMD |
| data_xl | 1,034   | 1,500+    | 1,520    | WS SIMD |
| twitter | 1,650   | 2,400+    | 2,736    | WS SIMD + trim elision |

### CSS L4 (target: __declaration < 15% of profile)

| Dataset    | Current | AP Target | cssparser |
|------------|---------|-----------|-----------|
| normalize  | 632     | 850+      | 732       |
| bootstrap  | 358     | 550+      | 476       |

---

## Execution Waves

### Wave 1: Fix regressions (AP.0)
- Agent A: Fix bootstrap `rhs`/`value_expr` error (0.1)
- Agent B: Fix payload wiring + revert structural dispatch (0.2, 0.5)
- Agent C: Fix CSS L4 tailwind regex + separator @ws (0.3, 0.4)

### Wave 2: Whitespace SIMD (AP.1)
- Agent A: SIMD bitmap construction in scan_and_cache (1.1)
- Agent B: Trim call reduction (1.2)

### Wave 3: CSS L4 dispatch (AP.2)
- Agent A: Declaration dispatch table (2.1)
- Agent B: Pattern dedup + delete monolithic bench (2.2, 2.3)

### Wave 4: Structural v2 + Tier B (AP.3, AP.4)
- Agent A: Entry-only + size-gated structural dispatch (3.1-3.3)
- Agent B: Tier B direct emission (4.1, 4.2)

### Wave 5: SIMD + Calibration (AP.5)
- Full bench + samply validation

## Critical Files

| File | Issue | Phase |
|------|-------|-------|
| `crates/core/src/graph/deps.rs` | `rhs` not in enum | 0.1 |
| `crates/core/src/graph/metadata.rs` | `rhs` not in enum | 0.1 |
| `crates/core/src/backend/rust/emitter/leaves.rs` | Payload: wrong fn name | 0.2 |
| `grammar/css/l4/properties.bbnf` | Value regex `!` bug | 0.3 |
| `parse-that/src/scanners.rs` | Scalar bitmap loop | 1.1 |
| `crates/core/src/backend/rust/emitter/alt.rs` | Structural hybrid | 0.5/3.1 |
| `grammar/css/l4/properties.bbnf` | 28-way backtrack | 2.1 |
