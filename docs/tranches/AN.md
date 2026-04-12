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

## Execution Order

Phase 1: AN.0 correctness bugs (must fix before any optimization)
Phase 2: AN.1 CSS @ws SIMD (high impact, isolated change)
Phase 3: AN.2 scanner generalization (architectural cleanup)
Phase 4: AN.3 + AN.4 + AN.5 (SIMD hyper-optimization)
Phase 5: AN.6 instrumentation
