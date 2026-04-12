# Tranche AN — Correctness, Generalization, Hyper-Optimization

Post-AM audit reveals three categories of debt: correctness bugs in
the tape surgery codegen, architectural overfitting in the scanner
layer, and untapped optimization opportunities in the cost model
and SIMD infrastructure.

Target: fix ALL correctness bugs, generalize ALL overfit scanners,
close the sonic-rs gap via single-pass string scanning and
whitespace bitmap caching.

## AN.0 — Correctness Bugs (CRITICAL)

### AN.0.1: `__has_children` never set (AM.3 regression)
`cargo expand` shows `__has_children` initialized `false` and never
set to `true` in `__value`. All values (including objects/arrays)
are pushed as `push_leaf(TapeKind::Span)` — views that call
`child(0)` on these records will fail to find children.

**Files:** `crates/core/src/backend/rust/emitter/alt.rs` — the
per-branch mark_children insertion must SET `__has_children = true`.

### AN.0.2: `__branch_idx` clobbered by inner dispatch
The bool branch in `__value` sets `__branch_idx = 4`, then the
inner false/true dispatch overwrites it to 0/1 (colliding with
object/array discriminants). Same pattern in CSS `__atRule`.

**Root cause:** Nested Alts share the same `__branch_idx` variable.
Inner dispatch should use a LOCAL variable, not the outer one.

**Files:** `crates/core/src/backend/rust/emitter/alt.rs`,
`dispatch.rs`

### AN.0.3: Serialize roundtrip namespace pollution
Multiple `#[derive(Parser)]` in one file produce duplicate free
functions (`cst_identifier_text`) and view types. The CST schema
emitter needs per-parser submodule wrapping.

**Files:** `crates/core/src/grammar/schema/emit/rust/`

### AN.0.4: LSP integration 21/46 failures
Analysis layer regressions — diagnostics, completions, hints, goto
all returning empty. Likely `try_compile_ir` failing silently after
AM.1 EmissionTier deletion changed the pipeline return types.

**Files:** `crates/analysis/src/state/diagnostics/ir_analysis.rs`

### AN.0.5: CSS L4 tailwind parse failure (offset 387594)
Grammar coverage gap — specific CSS constructs not handled.

### AN.0.6: Gorgeous `Debug` derive gap
`#[derive(Parser)]` with `prettify` doesn't emit `Debug` on the
generated type.

## AN.1 — CSS `@ws` SIMD Fast Path (HIGH IMPACT)

The CSS monolithic parser emits 15 copies of a 147-line inline HIR
expansion for the `@ws` comment-aware whitespace regex. Total: 2190
lines (36.5% of parse code). The grammar comments say "single
regex hits `sp_css_ws_comment()` SIMD fast path" but codegen does
NOT emit the SIMD call — it falls through to inline HIR.

**Root cause:** The recognizer/kernel system doesn't detect the `@ws`
pattern as a `WsBlockComment` recognizer, OR the kernel emit path
doesn't fire for `@ws`-defined patterns.

**Fix:** Route `@ws` patterns through the existing
`css_ws_comment_fast` SIMD scanner in parse-that.

**Expected impact:** -2190 lines of generated code, +15-25% CSS
parse throughput from eliminating 15 per-byte inline loops.

## AN.2 — Scanner Generalization (ARCHITECTURAL)

### Current state: overfit, duplicated, poorly named

parse-that has grammar-specific scanners that should be generalized:
- `quoted_string_scan_full` → `scan_delimited(delim, escape)`
- `css_ident_fast` → `scan_class_prefix_then_class(prefix_class, tail_class)`
- `css_ws_comment_fast` → `scan_ws_with_block_comment(comment_open, comment_close)`
- `css_string_fast` → `scan_delimited`
- `number_fused_scan_convert` → `scan_number(config) -> (Span, f64)`
- `number_span_scan_strict` → `scan_number_span(config)`
- `scan_digits_mut`, `scan_alnum_mut`, `scan_hex_mut` → `scan_char_class(lut)`

### Principle: classify, don't name

Scanner selection should be driven by `RegexClass` from the HIR
classifier (`bbnf-regex::classify`), not by pattern-string
comparison. The classifier already detects Numeric, HexDigits,
Identifier, QuotedString. The scanner dispatch should be:

```
RegexClass::QuotedString → scan_delimited(quote, b'\\')
RegexClass::Identifier → scan_class_prefix_then_class(...)
RegexClass::Numeric → scan_number(config)
RegexClass::HexDigits → scan_char_class(HEX_LUT)
RegexClass::CharClassQuantified → scan_char_class(lut) or memchr
```

### Nibble-LUT deduplication

`scanners.rs:210-224` and `structural.rs:53-75` contain identical
SIMD nibble-LUT scanning logic. Extract into a shared primitive.

## AN.3 — Single-Pass String Scanning (PERF)

Current `quoted_simd.rs` does two passes:
1. Find closing quote via SIMD escape-parity
2. Validate escapes via separate `memchr` + `validate_json_escapes`

sonic-rs does ONE pass: `StringBlock` produces 3 bitmasks per SIMD
load (quote, backslash, control), determines priority via
`trailing_zeros` comparison. No second pass.

**Implementation:** Merge validation into the SIMD loop. Each chunk
already computes quote and backslash masks. Add control-char mask
(`< 0x20`). Validate inline: if control-char comes before quote →
error. If backslash comes before quote → handle escape in same loop.

## AN.4 — Whitespace Bitmap Caching (PERF)

sonic-rs's 3-tier `skip_space`:
1. Scalar 2-byte check (handles 0-1 space chars — 80%+ of cases)
2. Cached bitmap reuse (64 bytes window, no SIMD re-scan)
3. Full SIMD scan (64 bytes/iteration)

**Implementation:** Add `ws_bitmap: u64, ws_start: usize` to
`ParserState`. `trim_leading_whitespace_mut` checks cached bitmap
first; only re-scans if position is outside the cached window.

## AN.5 — 32-Byte SIMD Chunks

Current portable_simd uses `u8x16` (16-byte chunks). sonic-rs uses
`u8x32` on x86 AVX2 (32 bytes). Doubling chunk size halves
iteration count for long strings.

`std::simd` supports `u8x32` on nightly. Gate on
`cfg(target_feature = "avx2")` for 32-byte path, keep 16-byte as
default.

## AN.6 — Cost Model & CSP Instrumentation

Add `BBNF_EGRAPH_REPORT=1` and `BBNF_CSP_REPORT=1` environment
variable reporting to the compilation pipeline. This enables
visibility into:
- How many e-graph rewrites fire per grammar
- What CSP solutions are found
- Whether optimization mode finds non-trivial answers
- Per-rule Alt strategy decisions (ByteDispatch vs Checkpoint)

## What Landed

### AN.0 — Correctness Fixes
- `__has_children` never set (AM.3 regression): fixed via stack-based
  context save/restore in Alt emitter
- `__branch_idx` clobbered by inner dispatch: fixed by scoping inner
  Alt's branch_idx_ident
- Serialize namespace pollution: per-parser `mod __<name>_emit_impl {}`
  wrapping via derive macro (AN.0.3)
- LSP regression: `is_value_keyword()` guard in nonterminal ref fallback,
  `peel_anonymous_wrapper()` for tape-first compounds (AN.0.4)
- Gorgeous compile: `split_pretty_hint_tokens` for `sep(", ")` hints

### AN Phase 0 — Three-Tier Payload Projection
- `PayloadKind` enum (F64/Bool/U8) on `RustEmitCtx` for per-rule
  payload eligibility detection
- Regex classification detects number patterns at `emit_regex_match_impl`
  (IR passes strip Map nodes, so detection uses `classify_regex`, not body
  structure)
- `scan_number_f64` emitted for payload-eligible number branches,
  captures f64 in `__payload_f64` + `__has_payload`
- MustTape Alt epilogue: three-way branch — `push_compound` for compound,
  `push_leaf_with_f64` when `__has_payload`, `push_leaf` otherwise
- TapeSpanOnly non-Alt: `push_leaf_with_f64` epilogue for standalone
  number rules
- View layer `.value()` accessor: reads `payload_f64` from tape O(1),
  falls back to span-text `parse::<f64>()` when no payload present

### Phase 1.1 — Serialize Codegen Tape-First Rewrite
- Complete rewrite: old `syn::Type`-based dispatch (enum pattern matching)
  replaced with tape cursor navigation (span_text/children/variant_idx)
- 675 compilation errors eliminated; 22 round-trip tests pass
- TransparentElide branches serialize via span_text fallback

### Phase 1.4 — Gorgeous Debug Derive
- `#[derive(Debug)]` added to `GoogleSheetsParser` for `Parsed<R>` Debug
  bound

### AN.1 — CSS @ws SIMD Routing
- Root cause: DFA-compatible `@ws` regex variant not in classifier's
  `WS_BLOCK_COMMENT_PATTERNS`; only lazy-quantifier form was recognized
- Fix: added DFA patterns to parse-that classifier + direct
  `scan_ws_block_comments` kernel routing in `ws.rs`
- Result: 15 inline HIR copies replaced by 15 kernel calls (-2190 LOC
  in expanded CSS code, -29% total expanded output)
- CSS pretty tests: 16/16 pass

### Post-AN Baseline (JSON cold, MB/s)

| Dataset  | Pre-AM | Post-AM | Post-AN | sonic-rs | vs sonic |
|----------|--------|---------|---------|----------|----------|
| canada   | 1,453  | 1,689   | 1,745   | 1,540    | +13% BEAT|
| citm     | 2,001  | 2,138   | 1,985   | 3,097    | -36%     |
| data     | 1,502  | 1,613   | 1,509   | 2,450    | -38%     |
| data_xl  | 1,121  | 1,153   | 1,089   | 1,520    | -28%     |
| twitter  | 1,672  | 1,671   | 1,636   | 2,736    | -40%     |

### Post-AN CSS Monolithic (cold, MB/s)

| Dataset    | Post-AN | cssparser | vs cssparser |
|------------|---------|-----------|--------------|
| normalize  | 2,438   | 732       | +233% BEAT   |
| bootstrap  | 1,618   | 476       | +240% BEAT   |
| tailwind   | 1,417   | 446       | +218% BEAT   |

## Execution Order

Phase 1: AN.0 correctness bugs (must fix before any optimization) — DONE
Phase 2: AN.1 CSS @ws SIMD (high impact, isolated change)
Phase 3: AN.2 scanner generalization (architectural cleanup)
Phase 4: AN.3 + AN.4 + AN.5 (SIMD hyper-optimization)
Phase 5: AN.6 instrumentation
