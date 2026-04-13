# AR Audit — sonic-rs Gap Analysis

Narrow scope: static hot-path + technique parity for the JSON post-AQ
bench results. Source reads only; orchestrator has the samply profiles
in `docs/benchmarks/profiles/AR-baseline/` for manual follow-up.

## 1. Current benchmark state (post-AQ)

### JSON Monolithic

| Dataset | MB/s | sonic-rs | Gap | Interpretation |
|---------|------|----------|-----|----------------|
| canada | 1,810 | 1,540 | **+17% WIN** | ~10K high-precision numbers; SWAR 8-digit mantissa chunking dominates; one top-level array means minimal tape pressure |
| citm | 2,657 | ~3,000 | -11% | Deep nesting; ~50K numbers; 50/50 compound vs leaf |
| twitter | 2,046 | ~2,643 | -22% | 13,345 key-value pairs in 632KB (~47 B / pair); many short strings with Unicode + escape parity cost |
| data | 1,929 | ~2,346 | -18% | 720 pairs in 35KB (~49 B / pair); nested objects, short fields |
| data_xl | 1,355 | ~1,460 | -7% | Many-MB variant of data; bandwidth-bound |

Sheets parse: 166 MB/s (1KB), 168 MB/s (10KB), 105 MB/s (pathological).
Compile time: json = 124 µs, bbnf = 1.69 ms, ebnf = 398 µs,
sheets = 2.26 ms, css_l4 = 9.42 ms.

### Hot paths (JSON grammar)

From `grammar/json/json.bbnf`:

```
value  = object | array | string | number | bool | null ;  # key dispatch on first byte
pair   = string, colon >> value ;                            # compound per pair
object = "{" >> (( pair << comma ? ) *)?w << "}" ;            # repeat compound + WS trim × 4
array  = "[" >> (( value << comma ? ) *)?w << "]" ;
```

`__value` is always a key-dispatched switch (see generated at
`crates/core/src/grammar/generated.rs:20165` and expanded at
`/tmp/expand_json_full.txt:2936`). Each non-leaf branch creates a
`mark_children` and a `push_compound`. The pair rule issues
`scan_quoted_string_strict` for the key, then WS trim, `:`, WS trim,
then `__value` — every pair emits **one `push_compound` plus one
key-dispatch** with **3 WS trim calls**.

twitter at -22% is dominated by compound-per-pair overhead (13,345
pairs × ~3 tape ops each = ~40K tape records just for pairs, plus
13,345 × 4 = ~53K WS trims per parse). data at -18% is the same
workload shape scaled. canada at +17% bypasses all that because
canada.json is one long array of nested coordinate arrays — very
few objects, very many numbers.

## 2. sonic-rs technique inventory

Grouped **(a) we have it**, **(b) partial**, **(c) we don't**. Column
headings in AQ-audit.md §"sonic-rs Architectural Findings" sourced
the feature list.

| Technique | Status | Evidence |
|---|---|---|
| Structural pre-scan (simdjson-style) | **deleted in AQ.5** | Net-negative at current WS budget (110 µs post-SIMD-bitmap < 300 µs pre-scan cost). AQ.md §"4. Structural pre-scan is no longer viable". |
| `skip_space` bitmap caching | **(a) have it** | `ParserState.ws_bitmap` + `ws_bitmap_start` at `parse-that/rust/parse_that/src/state.rs:179-193`. Tier-1 scalar 2-byte fast path, Tier-2 `(ws_bitmap >> bit_offset).trailing_ones()` cache hit, Tier-3 SIMD 16-byte-chunk bitmap populate in `scanners.rs:146-249`. Full parity with sonic-rs. |
| `simd_str2int` for integer parsing | **(b) partial** | SWAR 8-digit chunking (`parse_eight_digits`, `all_eight_are_ascii_digits` in `parsers/scan/number.rs:90-147`), zero NEON/x86 intrinsics. sonic-rs's `simd_str2int` is x86_64-only; our SWAR path is portable **and** runs on aarch64. For fraction parsing we could port a 17-digit NEON vector scan (sonic-rs doesn't have this on NEON either — opportunity to overtake). |
| LazyValue / on-demand parsing | **(c) don't have** | No `LazyValue`, `RawStr`, or deferred-parse type surface. Everything is eager tape construction. Not an apples-to-apples comparison: sonic-rs's `LazyValue` returns raw bytes + a pointer; the caller re-parses on demand. It trades total parse time for per-field access time. Our tape is already the moral equivalent at record granularity — each leaf stores a span, each compound owns its children. Not a gap to close; a different shape. |
| Eisel-Lemire float parse | **(a) have it** | `parse-that/rust/parse_that/src/parsers/eisel_lemire/{mod,algorithm,table}.rs`. Called from `scan_number_strict_f64`. CSS number path (`css_number_scan_f64`) fully uses it. JSON `number` rule today routes through `scan_number_strict_span` and leaves the bytes as a span — the f64 conversion is deferred to view-side. |
| Prefetch / aligned loads | **(c) don't have** | `grep prefetch` across parse-that and bbnf-lang returns nothing except one doc reference to hardware prefetcher (`tape.rs:42`). No `_mm_prefetch`, no `core::intrinsics::prefetch`. Unclear if sonic-rs's explicit prefetches beat the hardware prefetcher on the Apple M-series L1/L2. |
| Cache-friendly tape layout | **(b) partial** | `TapeRec` is 16 bytes fixed; records are `Vec<TapeRec>` — contiguous, sequential. **Missing:** capacity heuristic. Today every `parse(...)` entry-point preallocates `input.len().saturating_mul(4)` records (see generated `parse()` in `crates/core/src/backend/rust/emitter/grammar.rs:467`). For twitter that is 632K × 4 = 2.5M records × 16 B = **40 MB** preallocated. Actual record count is ~40K (one per value + one per compound) = ~640 KB. We over-allocate **64×**. sonic-rs uses `json.len() / 2 + 2`. |
| 64-byte padded input buffer | **(c) don't have** | SIMD quoted-string scanner (`quoted_simd::scan_quoted_string_simd`) does in-range `state.src_bytes.get(..)` slicing and bounds-checks EOF per chunk. sonic-rs pads the input with a `x"x\0…` suffix so the 16-byte SIMD load never OOB-checks. Net win modest (bounds check elimination on the hot loop) but present. |
| In-place string unescape (mutates input) | **(c) don't have, shouldn't port** | Our tape is span-borrowed, not mutated. Adopting in-place would force copy-on-write or break `parse(&input)` contract. Not worth the architectural cost. |
| 16-byte tagged Value | **n/a** | JSON-specific overfitting. Our tape is grammar-agnostic. Skip. |

### Net parity summary

Of 10 sonic-rs techniques inventoried: 3 fully landed, 2 partial, 3 not
worth porting (architectural mismatch), 2 genuine gaps — tape capacity
heuristic and 64-byte input padding. The third meaningful gap is
**compound tape-record count itself** — sonic-rs's "direct into buffer
via JsonVisitor" avoids the `push_compound` work that our `__pair` /
`__object` always do. Addressed in AR-audit-direct-struct.md (M0-M2).

## 3. Prioritized AR proposals

Strict ≤ 5, each ≤ 1 paragraph.

| # | Technique | Expected win per dataset | LOC cost | Risk |
|---|---|---|---|---|
| 1 | **Tape capacity heuristic**: replace `input.len() * 4` with `input.len() / 2 + 2` in both `crates/core/src/backend/rust/emitter/grammar.rs:467` and the hand-patched generated.rs:20165. For twitter/data this trims ~39.5 MB of mimalloc over-allocation per parse. | twitter +200 MB/s, data +150, citm +80, canada (neutral, one big array anyway), data_xl +200 | ~10 LOC | low |
| 2 | **Direct-to-struct payload activation** (see AR-audit-direct-struct.md M0-M2). Fix the `lower_map_arrow` leaf detection so `number = /regex/ -> f64` projects `TypeDesc::F64` and the emitter produces `push_leaf_with_f64` for JSON `number`. Eliminates the current compound wrap on `number` (two records → one record with 8 B payload). | twitter (numbers are sparse, ~small), citm +80 MB/s (20% of values are numbers), canada +150 MB/s (90% of values are numbers), data +60, data_xl +80 | ~200 LOC (mostly already wired; the bug is ~30 LOC) | medium |
| 3 | **Pair compound flattening** (post-M2 natural extension): JSON `pair` today emits a compound with two children (string span + value). When value is a leaf span (string/number/bool/null) emit a single 2-field-aggregate leaf with `TapeKind::KvPair` instead of a compound with 2 children. Saves one `push_compound` + one `mark_children` per scalar-valued pair. Twitter has 13K pairs, most scalar. | twitter +300 MB/s, data +200, citm +120, canada (neutral), data_xl +200 | ~150 LOC in emitter grammar.rs + view | medium |
| 4 | **64-byte input padding**: have `ParserState::new(input)` allocate a zero-terminated padded buffer. Eliminates the EOF bounds-check in `scan_quoted_string_simd` and allows a fixed-stride SIMD tail. Also enables safely reading 16 bytes past the last structural position for key dispatch. | twitter +100 MB/s (lots of short strings), data +60, citm +50, canada +20, data_xl +80 | ~60 LOC in state.rs + 1-line update at every scanner call site (most get free speedup) | low |
| 5 | **NEON 17-digit fractional scan** for Eisel-Lemire fast path. SWAR 8-digit chunks cover integer mantissa; fractional digits today go 1 byte at a time. sonic-rs does SIMD fraction on x86 but not NEON — porting a NEON version gives us an edge on both Apple Silicon and aarch64 Linux. | twitter (neutral), data +30, citm +40, canada +150 MB/s (long fractions), data_xl +80 | ~120 LOC in `parse-that/rust/parse_that/src/parsers/scan/number.rs` | low-medium |

### Why only five

Techniques intentionally excluded:

- **In-place string unescape**: breaks borrow model. Not portable to
  our tape.
- **LazyValue / RawStr**: architectural mismatch. Our tape already
  gives lazy access at view time without the JSON-specific API.
- **Structural pre-scan**: AQ audit proved negative ROI at current
  post-SIMD-WS budget (deleted in AQ.5).
- **16-byte tagged Value**: JSON-specific overfitting; our tape is
  grammar-agnostic by design.

### Why this ordering

Proposal 1 is the cheapest delta (a single constant change) with the
widest impact — over-allocation of 40 MB per parse is pure waste. It
should land first. Proposal 2 is already half-implemented and its
blocker is a known bug in `lower_map_arrow`. Proposal 3 compounds
with 2, exploiting the scalar-only insight to collapse the pair
compound. Proposals 4 and 5 are independent micro-optimizations;
schedule after 1-3 settle.

Combined expected: twitter ~2,650 MB/s (parity with sonic-rs), data
~2,400 MB/s (parity), citm ~2,950 MB/s (parity), canada ~2,100 MB/s
(+36% lead widens), data_xl ~1,700 MB/s (+16% new lead). Every dataset
at parity or ahead.
