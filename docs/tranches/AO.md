# Tranche AO — Structural Dispatch + Scanner Generalization + Global Optimization

## Context

Post-AN audit (3 parallel agents: profiling, optimization wiring, sonic-rs
architecture) reveals that ALL optimization systems are properly wired and
activating (CSP solver, e-graph, recognizer mining, dispatch tables, shared
cost model). No dead code, no ghost variants. The 27-40% string-heavy gap
to sonic-rs is NOT from slow scanners — `quoted_simd.rs` is near-optimal.

**Root cause**: BBNF dispatches one byte at a time via
`match state.src_bytes[state.offset]`. For every character inside a
JSON string (60-70% of string-heavy data), the recursive descent model
still executes `?w` calls, Alt entry checks, and Seq step checks. sonic-rs
avoids this by pre-scanning the ENTIRE buffer for structural bytes, then
walking a compact index of positions.

**Key finding**: The structural scanner infrastructure (`scan_structural`
in parse-that) already exists and is tested — it is COMPLETELY UNUSED by
the Rust codegen. The task is to wire it in.

Also incorporates remaining AN items: parse-that generalization (delete
CSS re-exports, consolidate number scanners, parameterize WS/quote),
SIMD widening, CSS L4 tailwind grammar fix, and global CSP solve.

---

## Phase 0: Structural Index Integration (P0, closes 15-20% of gap)

Pre-scan the input for structural bytes, filter quote parity, wire the
resulting index into the dispatch emitter. Whitespace becomes implicit.

### 0.1 Compile-time structural byte set derivation

New IR pass after `generate_dispatch_tables`. Walk all `AltDispatch`
tables, collect the union of dispatch bytes. Store as
`GrammarIR::structural_bytes: Option<Vec<u8>>`. Gate: ≤8 unique bytes
AND grammar has dispatch tables.

**Files:**
- `crates/ir/src/types/grammar.rs` — add field
- `crates/ir/src/passes/mod.rs` — new `compute_structural_bytes`
- `crates/core/src/pipeline/compile.rs` — invoke after dispatch tables

### 0.2 ParserState structural cursor

Add `structural_index: *const u32`, `structural_len: u32`,
`structural_cursor: u32` to `ParserState`. Helper methods:
`advance_to_next_structural()`, `peek_structural_byte()`.

**Files:**
- `parse-that/rust/parse_that/src/state.rs`

### 0.3 Quote-parity filtering

New `filter_quote_parity(input: &[u8], positions: &mut Vec<u32>)`.
Walks the structural index, tracks `in_string` boolean, handles
escaped backslashes. In-place compaction.

**Files:**
- New: `parse-that/rust/parse_that/src/parsers/scan/quote_parity.rs`

### 0.4 Pre-scan in codegen entry point

Emitted `parse()` calls `scan_structural` + `filter_quote_parity`
before entering recursive descent. Structural byte set emitted as
`const` array from `GrammarIR::structural_bytes`.

**Files:**
- `crates/core/src/backend/rust/emitter/grammar.rs` — modify `emit_grammar_impl`

### 0.5 Structural-aware dispatch emission

When structural index active, dispatch jumps to next structural
position instead of checking byte-at-a-time. Compile-time mode
selection (structural vs byte-at-a-time). No runtime branch.

**Files:**
- `crates/core/src/backend/rust/emitter/alt.rs` — conditional structural dispatch
- `crates/core/src/backend/driver/alt.rs` — thread structural mode

### 0.6 Whitespace elimination via structural index

When structural mode active + no custom `@ws` pattern, `?w` becomes
a no-op (structural index already skips whitespace). The 24
`trim_leading_whitespace_mut` calls in JSON drop to 0.

**Files:**
- `crates/core/src/backend/rust/emitter/ws.rs` — conditional elision
- `crates/core/src/backend/driver/seq.rs` — thread structural flag

### Verification: Phase 0
```
cargo test --workspace
cargo bench --bench json_monolithic --bench json_competitors  # ≥15% on twitter/citm
cargo bench --bench css_monolithic  # no regression on CSS L4
```

---

## Phase 1: Padded Buffer Mode (+3-5%)

### 1.1 Padded input construction

`ParserState::new_padded()` allocates `input.len() + 16` bytes,
copies input, fills padding with NUL. Sets `state.end = input.len()`
but allows SIMD reads up to `+16` without bounds checks.

**Files:**
- `parse-that/rust/parse_that/src/state.rs` — new constructor
- `crates/core/src/backend/rust/emitter/grammar.rs` — emit padded entry

### 1.2 Remove SIMD boundary checks

When padded mode active (`state.end < state.src_bytes.len()`), SIMD
loops skip `offset + 16 <= len` check. NUL padding terminates scans.

**Files:**
- `parse-that/rust/parse_that/src/parsers/scan/quoted_simd.rs`
- `parse-that/rust/parse_that/src/scanners.rs`

### Verification: Phase 1
```
cargo test --workspace
cargo bench --bench json_monolithic  # +3-5%
```

---

## Phase 2: parse-that Generalization (remaining from AN)

NO legacy code. NO re-exports. NO overfit items.

### 2.1 Delete CSS re-export wrappers

DELETE `parsers/css/scan.rs` entirely. Update callers to use base
scanner names directly.

### 2.2 Consolidate JSON number scanners

4 functions → 2:
1. `scan_number_parts(state, config) -> Option<(Span, NumberParts)>`
2. Conversion at call site

### 2.3 Parameterize whitespace scanning

Accept `&WhitespaceConfig` or byte set. Provide `ASCII_WS_LUT` and
`CSS_WS_LUT` as constants.

### 2.4 Parameterize quoted string SIMD

Accept `quote: u8` parameter in `scan_quoted_string_simd`. Supports
both `"` and `'` delimiters.

### 2.5 Delete SpanParser wrapper anti-pattern

DELETE 7 `sp_json_*` and `sp_css_*` functions. Codegen calls scanners
directly.

### 2.6 Dedup nibble-LUT SIMD (AN 3.3)

Extract shared SIMD chunk processing primitive from `scanners.rs` and
`structural.rs`.

### Verification: Phase 2
```
cargo test -p parse_that
cargo test --workspace
cargo bench --bench json_monolithic --bench css_monolithic  # no regression
```

---

## Phase 3: SIMD Widening + Numeric SIMD (remaining from AN)

### 3.1 32-byte SIMD chunks (AN 4.3)

Gate on `cfg(target_feature = "avx2")` for `u8x32` path in
`quoted_simd.rs` and `structural.rs`. Keep `u8x16` as default.

**Expected impact:** +5-10% on x86_64 for string-heavy workloads.

### 3.2 SIMD digit-to-integer (AN 4.4)

`simd_str2int` in `parsers/scan/number.rs` using `portable_simd`
multiply + pairwise add for fraction digit conversion.

**Expected impact:** +2-5% on numeric workloads (canada, data_xl).

### Verification: Phase 3
```
cargo test -p parse_that
cargo bench --bench json_monolithic
```

---

## Phase 4: Cost Model Calibration + Global CSP

### 4.1 Grid sweep over CostWeights

Sweep `dispatch_bonus` [-4.0, -3.0, -2.0, -1.0], `call_overhead`
[2.0, 4.0, 6.0, 8.0], `inline_body_size_penalty` [0.25, 0.5, 1.0].
Maximize geometric mean across JSON (5) + CSS L4 (1). No individual
bench regresses >1%.

**Files:**
- `crates/egraph/src/cost_weights.rs` — update defaults

### 4.2 Global CSP solve (AN 5.3)

Currently per-component. For CSS L4 (many import modules = many
components), cross-component optimization opportunities are missed.
Implement optional global solve with increased node budget.

**Files:**
- `crates/ir/src/passes/csp_strategy/mod.rs` — add global mode

### 4.3 Release-build instrumentation (AN 5.1)

`BBNF_EGRAPH_REPORT=1` and `BBNF_CSP_REPORT=1` in release builds.

### Verification: Phase 4
```
cargo bench --bench json_monolithic --bench css_monolithic --bench compile_pipeline
```

---

## Phase 5: Correctness + Self-Hosting + Polish

### 5.1 CSS L4 tailwind parse failure (AN 1.3)

Expand L4 grammar to cover the construct at offset 387594.

### 5.2 Self-hosting audit

Re-run bootstrap regen. Verify BBNF grammar compiles correctly with
structural mode (may exceed 8-byte limit → graceful fallback).

### 5.3 Branch frequency ordering

Reorder dispatch `match` arms by expected frequency. Strings dominate
in typical JSON → string branch first for branch prediction.

**Files:**
- `crates/ir/src/passes/sets/dispatch/build.rs` — frequency hints

### 5.4 Structural position prefetch

Before processing current structural position, prefetch next via
platform-specific prefetch intrinsics.

### Verification: Phase 5
```
cargo test --workspace
cargo bench --bench json_monolithic --bench css_monolithic
```

---

## Performance Targets (MB/s, cold)

| Dataset  | Current | AO Target | sonic-rs | Goal   |
|----------|---------|-----------|----------|--------|
| canada   | 1,768   | 1,900+    | 1,540    | BEAT   |
| citm     | 2,052   | 2,900+    | 3,097    | CLOSE  |
| data     | 1,718   | 2,400+    | 2,450    | PARITY |
| data_xl  | 1,108   | 1,500+    | 1,520    | PARITY |
| twitter  | 1,650   | 2,500+    | 2,736    | CLOSE  |

Phase 0 (structural dispatch) is the primary lever: +15-20% on
string-heavy datasets. Combined with padded buffer (+3-5%), SIMD
widening (+5-10%), and whitespace elimination (+8-12%), the cumulative
improvement targets sonic-rs parity on data/data_xl and within 10% on
citm/twitter.

---

## Execution Waves

### Wave 1 (Structural Foundation)
- Agent A: IR pass — compute_structural_bytes (0.1)
- Agent B: ParserState structural cursor (0.2) + quote parity (0.3)
- Agent C: parse-that generalization — delete CSS wrappers + SpanParser (2.1, 2.5)

### Wave 2 (Codegen Integration)
- Agent A: Pre-scan entry + dispatch emission (0.4, 0.5)
- Agent B: Whitespace elimination (0.6)
- Agent C: Consolidate number scanners + parameterize WS/quote (2.2-2.4)

### Wave 3 (SIMD + Padded Buffer)
- Agent A: Padded buffer mode (1.1, 1.2)
- Agent B: 32-byte SIMD chunks (3.1) + nibble-LUT dedup (2.6)
- Agent C: SIMD digit-to-integer (3.2)

### Wave 4 (Calibration + Correctness)
- Agent A: Cost model grid sweep (4.1)
- Agent B: Global CSP solve (4.2) + instrumentation (4.3)
- Agent C: CSS L4 tailwind fix (5.1) + self-hosting (5.2) + branch freq (5.3)

### Wave 5 (Polish + Verification)
- Full bench suite + samply profiling
- Structural position prefetch (5.4)
- Documentation + tranche doc

## Critical Files

| File | Changes |
|------|---------|
| `crates/ir/src/types/grammar.rs` | `structural_bytes` field |
| `crates/ir/src/passes/mod.rs` | `compute_structural_bytes` pass |
| `crates/core/src/pipeline/compile.rs` | invoke new pass |
| `crates/core/src/backend/rust/emitter/grammar.rs` | pre-scan entry |
| `crates/core/src/backend/rust/emitter/alt.rs` | structural dispatch |
| `crates/core/src/backend/rust/emitter/ws.rs` | whitespace elision |
| `parse-that/src/state.rs` | structural cursor + padded mode |
| `parse-that/src/parsers/scan/quote_parity.rs` | NEW |
| `parse-that/src/parsers/scan/structural.rs` | widened SIMD |
| `parse-that/src/parsers/scan/quoted_simd.rs` | widened + parameterized |
| `parse-that/src/parsers/json.rs` | consolidated number scanners |
| `parse-that/src/parsers/css/scan.rs` | DELETE |
| `crates/egraph/src/cost_weights.rs` | calibrated defaults |
| `crates/ir/src/passes/csp_strategy/mod.rs` | global solve mode |
