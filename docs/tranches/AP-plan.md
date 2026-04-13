# Tranche AP — Correctness-First, Then Performance

## Audit Synopsis (Post-AO)

Four-agent audit reveals critical correctness regressions masking
performance measurements, a structural dispatch that adds overhead
instead of removing it, and accumulated deferred items from AN/AO
that must be addressed before further optimization.

### What WORKS (stable infrastructure)

- Tape substrate (flat Vec<TapeRec>, 16 bytes/record): bulletproof
- TapeCursor zero-copy view layer: correct and performant
- CSP strategy solver: all systems active, properly wired
- E-graph: 5 rewrite rules firing, shared CostWeights
- Recognizer mining: 10 miners, all consumed
- Dispatch tables: O(1) byte-dispatch for disjoint Alts
- Bootstrap self-hosting: regeneration produces valid parser
- JSON canada: 1,792 MB/s (+16% BEAT sonic-rs)
- CSS monolithic: 2,328 MB/s normalize (+230% vs cssparser)

### What's BROKEN (must fix before optimizing)

1. **Bootstrap compile error**: `BbnfBootstrapRuleKind::rhs` not
   found — LSP agent's AN changes to `deps.rs`/`metadata.rs`
   reference a rule_kind that doesn't exist in the generated parser.
   Blocks ALL cargo expand + regen workflows.

2. **Structural dispatch regression**: AO.0 adds overhead instead of
   removing it. Root causes:
   - Hybrid branching (`has_structural_index()` check on EVERY dispatch)
   - Cursor desynchronization on checkpoint backtrack
   - Pre-scan overhead (scan_structural + filter_quote_parity) not
     amortized for dense JSON
   - Applied to ALL dispatch Alts, not just entry rule

3. **CSS L4 tailwind parse failure**: offset 387594 — grammar
   coverage gap. L4 bench is the ONLY CSS bench that matters.

4. **CSS L4 performance**: 358 MB/s bootstrap (-25% vs cssparser).
   Import cross-module overhead + @ws kernel cost + function call
   overhead on 100+ dispatch tables.

5. **Payload projection NOT activating**: cargo expand shows ZERO
   `push_leaf_with_f64` or `__payload_f64` in the JSON expanded
   output. The three-tier projection (Phase 0) codegen is wired but
   the IR Map stripping + regex classification path has a cache
   issue that prevents activation.

### Consolidated Deferred Items (ALL tranches)

| Item | Origin | Priority | Status |
|------|--------|----------|--------|
| AF.6: Tier B direct emission | AF | HIGH | Infrastructure done, emitter deferred |
| CSS L4 tailwind grammar | AN 1.3 | HIGH | Offset 387594 not covered |
| AF.0b: Clean bootstrap regen | AF | MEDIUM | Codegen bug in __grammar body |
| Delete CSS monolithic bench | User directive | LOW | Replace with L4 as sole CSS bench |
| parse-that: consolidate number scanners | AN 2.2 | DONE (AO) | Landed: 2035985 |
| parse-that: parameterize WS/quote | AN 2.3-2.4 | DONE (AO) | Landed: 2035985 |
| parse-that: delete CSS wrappers | AN 2.1 | DONE (AO) | Landed: 3d5e20d |
| parse-that: delete SpanParser wrappers | AN 2.5 | DONE (AO) | Landed: 3d5e20d |
| SIMD 32-byte chunks | AN 4.3 | MEDIUM | Not started |
| SIMD digit-to-integer | AN 4.4 | LOW | Not started |
| Global CSP solve | AN 5.3 | MEDIUM | Not started |
| Release-build instrumentation | AN 5.1 | LOW | Not started |
| Branch frequency ordering | AO 5.3 | LOW | Not started |
| Padded buffer mode | AO 1 | MEDIUM | Not started |

---

## Current Benchmarks

### JSON Cold (MB/s)

| Dataset | BBNF | sonic-rs | Gap |
|---------|------|----------|-----|
| canada  | 1,792 | 1,540  | +16% BEAT |
| citm    | 2,042 | 3,097  | -34% |
| data    | 1,650 | 2,450  | -33% |
| data_xl | 1,034 | 1,520  | -32% |
| twitter | 1,650 | 2,736  | -40% |

### CSS L4 (MB/s)

| Dataset    | BBNF L4 | cssparser | Gap |
|------------|---------|-----------|-----|
| normalize  | 632     | 732       | -14% |
| bootstrap  | 358     | 476       | -25% |

### CSS Monolithic (MB/s) — for reference only

| Dataset    | BBNF | cssparser |
|------------|------|-----------|
| normalize  | 2,328 | 732 |
| bootstrap  | 1,510 | 476 |

---

## AP Phase 0: Fix Regressions (BLOCKING)

### 0.1 Fix bootstrap compile error

The `BbnfBootstrapRuleKind::rhs` reference in `deps.rs` and
`metadata.rs` uses a rule_kind name that doesn't exist in the
generated parser. Either:
a) Add `rhs` to the bootstrap grammar's rule set, OR
b) Remove the `rhs` references from deps.rs/metadata.rs

**Files:** `crates/core/src/graph/deps.rs`,
`crates/core/src/graph/metadata.rs`,
`crates/core/src/grammar/generated.rs`

### 0.2 Revert or gate structural dispatch

The AO.0 structural dispatch adds overhead on dense JSON. Options:
a) Gate on input size (>4KB only)
b) Apply only to entry rule's top-level dispatch
c) Fix checkpoint cursor restore (correctness bug)
d) Revert entirely and redesign

Recommended: (b) + (c) + gate on input size.

**Files:** `crates/core/src/backend/rust/emitter/alt.rs`,
`crates/core/src/backend/rust/emitter/grammar.rs`

### 0.3 Fix payload projection

The three-tier payload (push_leaf_with_f64) is not activating in
the expanded JSON output. Debug why: check if ctx.payload_kind is
set correctly for the value rule's Alt, and verify the regex
classification detects the number pattern.

**Files:** `crates/core/src/backend/rust/emitter/leaves.rs`,
`crates/core/src/backend/rust/emitter/mod.rs`

---

## AP Phase 1: CSS L4 Performance

### 1.1 Fix CSS L4 tailwind parse (offset 387594)

Expand the L4 grammar to cover the failing CSS constructs.

### 1.2 Delete CSS monolithic bench

Per user directive: CSS L4 is the only bench that matters. Delete
`crates/core/benches/css/monolithic.rs` and the monolithic grammar.

### 1.3 Profile CSS L4 hot paths

Run samply on CSS L4 bootstrap. Identify:
- Cross-module function call overhead
- @ws kernel cost per invocation
- Dispatch table overhead (100+ tables)
- Host type function overhead

### 1.4 Inline hot CSS L4 paths

Based on profile, inline the hottest cross-module calls. Consider:
- Inlining @token rules across module boundaries
- Reducing @ws kernel overhead via caching
- Flattening dispatch tables for common patterns

---

## AP Phase 2: Structural Dispatch v2

Redesign structural dispatch based on the AO.0 failure analysis:

### 2.1 Entry-rule-only structural dispatch

Only the entry rule's top-level Alt uses structural dispatch.
Nested Alts use byte-at-a-time. This eliminates the hybrid
branching overhead at every nesting level.

### 2.2 Fuse structural cursor into offset

Replace separate cursor tracking with a single state variable.
Remove `has_structural_index()` runtime check.

### 2.3 Input size gating

Pre-scan only for inputs > 4KB. Below that threshold, the
prescan overhead exceeds the dispatch savings.

### 2.4 Fix checkpoint cursor restore

Save/restore `structural_cursor` alongside `state.offset` in
checkpoint Alts.

---

## AP Phase 3: Direct-to-Struct Projection (AF.6)

The three-tier system's Tier B (direct-to-struct) was deferred in
AF.6. Complete it:

### 3.1 Tier B emitter for leaf rules

For rules classified as Tier B (leaf rules with scalar types),
emit a second function:
```rust
fn __number_direct(state) -> Option<f64> {
    scan_number_f64(state)
}
```

### 3.2 View accessor wiring

The view layer's `.value()` accessor should call the Tier B
function when available, falling back to span-text parse.

### 3.3 Lazy evaluation dispatch

The three-tier dispatch: payload (O(1)) → Tier B (direct scan) →
span-text parse (lazy fallback). Wire all three tiers.

---

## AP Phase 4: SIMD Widening + Cost Calibration

### 4.1 32-byte SIMD chunks (AN 4.3)
### 4.2 Cost model grid sweep
### 4.3 Global CSP solve for CSS L4

---

## Performance Targets

| Dataset  | Current | AP Target | sonic-rs |
|----------|---------|-----------|----------|
| canada   | 1,792   | 1,900+    | 1,540    |
| citm     | 2,042   | 2,500+    | 3,097    |
| data     | 1,650   | 2,200+    | 2,450    |
| data_xl  | 1,034   | 1,400+    | 1,520    |
| twitter  | 1,650   | 2,200+    | 2,736    |

CSS L4:
| Dataset    | Current | AP Target | cssparser |
|------------|---------|-----------|-----------|
| normalize  | 632     | 800+      | 732       |
| bootstrap  | 358     | 500+      | 476       |
