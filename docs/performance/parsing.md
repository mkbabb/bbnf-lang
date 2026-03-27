---
title: Parsing
order: 41
section: Performance
---

# Parsing Performance

parse_that (Rust) and parse-that (TypeScript) are the parsing backbone. Both use dispatch tables, FIRST-set routing, and memoization tuned for their respective runtimes.

## Rust: `JSON` — Span

Arena parsing returns opaque spans allocated via `BumpArena`. Each iteration constructs a fresh arena and parser—cold per-parse throughput.

```bench-chart
{ "title": "JSON Arena Span", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["BBNF span"],
      "series": [{"label": "Throughput", "values": [1261]}] },
    { "name": "twitter (631 KB)", "icon": "rust",
      "labels": ["BBNF span"],
      "series": [{"label": "Throughput", "values": [1347]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "rust",
      "labels": ["BBNF span"],
      "series": [{"label": "Throughput", "values": [1597]}] },
    { "name": "canada (2.2 MB)", "icon": "rust",
      "labels": ["BBNF span"],
      "series": [{"label": "Throughput", "values": [1115]}] },
    { "name": "data_xl (20 MB)", "icon": "rust",
      "labels": ["BBNF span"],
      "series": [{"label": "Throughput", "values": [815]}] }
  ] }
```

## Rust: `JSON` — Borrow

Numbers parsed to f64, strings borrowed from input without escape decoding. Cold per-parse with `BumpArena`. BBNF uses `Vec<(K,V)>` for objects; nom, winnow, and pest use `HashMap<&str,V>`.

```bench-chart
{ "title": "JSON Arena Borrow — No Decode", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["BBNF borrow", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [1019, 657, 609, 229]}] },
    { "name": "twitter (631 KB)", "icon": "rust",
      "labels": ["BBNF borrow", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [1156, 540, 586, 224]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "rust",
      "labels": ["BBNF borrow", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [1257, 703, 679, 186]}] },
    { "name": "canada (2.2 MB)", "icon": "rust",
      "labels": ["BBNF borrow", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [598, 447, 440, 106]}] },
    { "name": "data_xl (20 MB)", "icon": "rust",
      "labels": ["BBNF borrow", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [632, 491, 441, 154]}] }
  ] }
```

All benchmarks use mimalloc. nom, winnow, and pest construct per-iteration (cold).

## Rust: `JSON` — Copy

Full escape decoding with owned or `Cow` string allocation. Cold per-parse with `BumpArena` for BBNF. sonic-rs uses SIMD + its own arena. serde_json_borrow and jiter use `Cow`/borrowed output with decode. simd-json uses SIMD scanning.

```bench-chart
{ "title": "JSON Copy", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["BBNF copy", "sonic-rs", "serde_json_borrow", "simd-json", "jiter", "serde_json"],
      "series": [{"label": "Throughput", "values": [887, 2293, 1515, 1364, 1475, 930]}] },
    { "name": "twitter (631 KB)", "icon": "rust",
      "labels": ["BBNF copy", "sonic-rs", "serde_json_borrow", "simd-json", "jiter", "serde_json"],
      "series": [{"label": "Throughput", "values": [900, 2522, 1652, 1375, 1222, 867]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "rust",
      "labels": ["BBNF copy", "sonic-rs", "serde_json_borrow", "simd-json", "jiter", "serde_json"],
      "series": [{"label": "Throughput", "values": [1173, 3031, 1416, 1696, 1295, 1132]}] },
    { "name": "canada (2.2 MB)", "icon": "rust",
      "labels": ["BBNF copy", "sonic-rs", "serde_json_borrow", "simd-json", "jiter", "serde_json"],
      "series": [{"label": "Throughput", "values": [595, 1499, 660, 733, 607, 607]}] },
    { "name": "data_xl (20 MB)", "icon": "rust",
      "labels": ["BBNF copy", "sonic-rs", "serde_json_borrow", "simd-json", "jiter", "serde_json"],
      "series": [{"label": "Throughput", "values": [567, 1445, 890, 982, 798, 647]}] }
  ] }
```

All Rust benchmarks use mimalloc. BBNF parsers are generated from a [`.bbnf` grammar](../../grammar/lang/json.bbnf) via `#[derive(Parser)]` with zero hand-written Rust.

## Rust: `CSS` — Span

BBNF uses [`@ws`](../../grammar/BBNF.md) for SIMD comment-aware whitespace, `@inline` for trivial helper rules, and `@token` for lexical tokens with fusion-style inlining. Cold per-parse with `BumpArena`. cssparser (Mozilla's tokenizer) uses a visitor pattern that counts rules and declarations without building an AST.

```bench-chart
{ "title": "CSS Arena + Span", "unit": "MB/s",
  "datasets": [
    { "name": "normalize (6 KB)", "icon": "rust",
      "labels": ["BBNF arena", "BBNF span", "cssparser"],
      "series": [{"label": "Throughput", "values": [2182, 2472, 655]}] },
    { "name": "bootstrap (281 KB)", "icon": "rust",
      "labels": ["BBNF arena", "BBNF span", "cssparser"],
      "series": [{"label": "Throughput", "values": [1270, 1885, 424]}] },
    { "name": "tailwind (3.8 MB)", "icon": "rust",
      "labels": ["BBNF arena", "BBNF span", "cssparser"],
      "series": [{"label": "Throughput", "values": [1202, 1856, 402]}] }
  ] }
```

BBNF arena is 3x cssparser across all three datasets; BBNF span widens the gap further with zero-allocation parsing. Inline optional Span codegen, direct Span construction in delim_scan, `@token` fusion, and generalized regex strength reduction eliminated the per-rule overhead that previously made recursive descent slower than cssparser's flat tokenizer loop on utility-heavy stylesheets.

## Rust: `CSS` — Structural AST

BBNF pretty builds a typed enum tree with rule/block/declaration structure, using opaque regex spans for selectors and values (L1.5). lightningcss (Parcel) performs a full L2 semantic parse—typed CSS properties, vendor prefix analysis, CSS Nesting validation.

```bench-chart
{ "title": "CSS Structural AST", "unit": "MB/s",
  "datasets": [
    { "name": "normalize (6 KB)", "icon": "rust",
      "labels": ["BBNF pretty", "lightningcss"],
      "series": [{"label": "Throughput", "values": [711, 257]}] },
    { "name": "bootstrap (281 KB)", "icon": "rust",
      "labels": ["BBNF pretty", "lightningcss"],
      "series": [{"label": "Throughput", "values": [299, 117]}] },
    { "name": "tailwind (3.8 MB)", "icon": "rust",
      "labels": ["BBNF pretty", "lightningcss"],
      "series": [{"label": "Throughput", "values": [296, 94]}] }
  ] }
```

See the [formatting benchmarks](./formatting) for gorgeous vs Biome end-to-end comparisons.

## Rust: `CSS` — Semantic

BBNF semantic produces typed values during the parse itself—f64 numbers via fused Eisel-Lemire conversion, u32 hex colors, u8 discriminants for length/angle/time units—not as a post-hoc AST walk. lightningcss performs equivalent semantic work: typed CSS properties, vendor prefix resolution, CSS Nesting validation.

```bench-chart
{ "title": "CSS Semantic — vs lightningcss", "unit": "MB/s",
  "datasets": [
    { "name": "normalize (6 KB)", "icon": "rust",
      "labels": ["BBNF semantic", "lightningcss"],
      "series": [{"label": "Throughput", "values": [601, 256]}] },
    { "name": "bootstrap (281 KB)", "icon": "rust",
      "labels": ["BBNF semantic", "lightningcss"],
      "series": [{"label": "Throughput", "values": [258, 114]}] },
    { "name": "tailwind (3.8 MB)", "icon": "rust",
      "labels": ["BBNF semantic", "lightningcss"],
      "series": [{"label": "Throughput", "values": [310, 88]}] }
  ] }
```

| Parser | normalize | bootstrap | tailwind |
|--------|-----------|-----------|----------|
| BBNF semantic | 601 MB/s | 258 MB/s | 310 MB/s |
| lightningcss | 256 MB/s | 114 MB/s | 88 MB/s |
| Ratio | 2.35x | 2.26x | 3.52x |

The gap widens on tailwind (3.52x) where BBNF's dispatch tables and inline byte scanners amortize better across ~38K utility rules than lightningcss's hand-written recursive descent. Bootstrap went from 21 MB/s to 258 MB/s over the optimization sequence—a 12.3x improvement driven by compiler techniques applied to the parser codegen (see below).

## Rust: `CSS` — Import Grammar (L2)

The import grammar does comparable work to lightningcss — property-aware dispatch (21 groups), typed CSS Selectors Level 4 (31 rules), balanced-paren function bodies, 147 named colors, typed numbers (f64), typed hex colors (u32). Built entirely via `@import` composition from modular grammar files, no monolithic grammar needed.

```bench-chart
{ "title": "CSS Import (L2) — vs lightningcss", "unit": "MB/s",
  "datasets": [
    { "name": "normalize (6 KB)", "icon": "rust",
      "labels": ["BBNF import", "lightningcss"],
      "series": [{"label": "Throughput", "values": [237, 260]}] },
    { "name": "bootstrap (281 KB)", "icon": "rust",
      "labels": ["BBNF import", "lightningcss"],
      "series": [{"label": "Throughput", "values": [125, 116]}] },
    { "name": "tailwind (3.8 MB)", "icon": "rust",
      "labels": ["BBNF import", "lightningcss"],
      "series": [{"label": "Throughput", "values": [123, 87]}] }
  ] }
```

| Parser | normalize | bootstrap | tailwind |
|--------|-----------|-----------|----------|
| BBNF import (L2) | 237 MB/s | 125 MB/s | 123 MB/s |
| lightningcss | 260 MB/s | 116 MB/s | 87 MB/s |

On normalize (6 KB), lightningcss's hand-written parser has a slight edge. On bootstrap and tailwind, BBNF's dispatch tables and inline byte scanners pull ahead — 1.08x on bootstrap, 1.41x on tailwind. The import grammar demonstrates that modular `@import` composition incurs no performance penalty compared to monolithic grammars.

### CSS Tier Summary

| Tier | normalize | bootstrap | tailwind | Work |
|------|-----------|-----------|----------|------|
| span | 2,472 | 1,885 | 1,856 | Byte-range validation |
| arena | 2,182 | 1,270 | 1,202 | Typed enum tree (opaque values) |
| structural/pretty | 711 | 299 | 296 | Formatted AST with `@pretty` |
| semantic | 601 | 258 | 310 | f64/u32/u8 typed values |
| import (L2) | 237 | 125 | 123 | Full L2 via `@import` composition |
| typed | 213 | 102 | 102 | Full CSS L4 property types |

### Compiler Optimization Techniques

The 12.3x improvement on bootstrap came from applying classical compiler optimizations to parser codegen—the same techniques compilers use on scalar code, applied at the IR level to parser construction:

| Technique | Application | Impact |
|-----------|-------------|--------|
| Strength Reduction | `NumberConvert` emits `css_number_scan_f64`: regex replaced by byte scanner | 7.3x |
| LICM | Inline byte scanners for `--[\w-]+`, comma-or-ws patterns, etc. eliminate per-call regex construction | +54% |
| CSE | `hoist_dedup` HashMap prevents intra-function duplicate scanner construction | +39% |
| Map Fusion (SSA) | `(NumberConvert, EnumWrap)`, `(Constant, EnumWrap)` fused to single `.map()` | +15% |
| Induction Variable | `FnDescriptor` specialization: NumberConvert, HexConvert, Constant recognized at IR level | enables all above |
| Trie Prefix Factoring | `factor_literal_prefixes`: byte-level literal splitting enables dispatch tables | +2-5% |

These are not hand-applied optimizations—the IR pipeline detects the patterns and emits specialized code automatically. The grammar author writes `number -> /regex/ ;` and the codegen emits a fused byte scanner with Eisel-Lemire f64 conversion.

### `regex_emit` — HIR-Based Inline Regex Compilation

The `regex_emit` module (`generate/regex_emit/`) compiles regex patterns to inline byte operations at proc-macro expansion time, eliminating all runtime regex overhead in the monolithic codegen path. The architecture has three tiers, tried in order:

1. **`fast_paths::emit_regex_direct_call`** — pattern-matched fast paths for known high-value patterns (CSS identifiers, quoted strings, comment-aware whitespace, negated character classes). Emits calls to hand-tuned byte scanners in `parse_that` (e.g., `css_ident_fast`, `css_ws_comment_fast`, `memchr`-based `[^XYZ]+`).

2. **`regex_emit::try_emit_regex_inline`** (`hir_walk.rs`) — parses the regex via `regex-syntax` into HIR (High-level Intermediate Representation), then walks the HIR tree to emit inline Rust byte operations. Handles concatenation, alternation, character classes (positive and negated), repetition (greedy `*`/`+`/`?`/`{n,m}`), and anchored/unanchored variants. Each HIR node maps to a small code fragment: `Class` becomes byte-range checks, `Repetition` becomes a `loop` with break conditions, `Concat` sequences the fragments. No `Regex` object is ever constructed at runtime.

3. **`regex_emit::emit_regex_lazy_static`** (`fallback.rs`) — fallback for patterns too complex for inline compilation. Emits a `LazyLock<Regex>` that compiles the regex once on first use. In practice, this path is never reached for CSS or JSON grammars — all patterns are handled by tiers 1 or 2.

The result: zero `SpanParser::Regex` enum dispatch, zero `Regex::find()` calls, zero runtime compilation in the monolithic codegen path. Every regex in the grammar becomes straight-line byte comparisons and loops, subject to the same LLVM optimizations as hand-written scanner code.

## Rust: `Google Sheets`

AOT vs VM on formula parsing. The VM interprets bytecode; AOT generates native Rust. The VM gap narrows on larger inputs as bytecode dispatch overhead amortizes. AOT is 93x faster on pathological inputs, 33x on 1 KB, 16x on 10 KB.

```bench-chart
{ "title": "Google Sheets — AOT vs VM", "unit": "ns", "lowerIsBetter": true,
  "datasets": [
    { "name": "pathological (270 B)", "icon": "rust",
      "labels": ["AOT", "VM"],
      "series": [{"label": "Latency", "values": [4582, 427128]}] },
    { "name": "1 KB formulas", "icon": "rust",
      "labels": ["AOT", "VM"],
      "series": [{"label": "Latency", "values": [19194, 638100]}] },
    { "name": "10 KB formulas", "icon": "rust",
      "labels": ["AOT", "VM"],
      "series": [{"label": "Latency", "values": [235881, 3771929]}] }
  ] }
```

## TypeScript: `JSON`

Benchmarked with vitest across five datasets. parse-that consistently outperforms Chevrotain, with the gap widening on larger inputs.

```bench-chart
{ "title": "TypeScript JSON", "unit": "ops/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "ts",
      "labels": ["JSON.parse", "parse-that", "Chevrotain", "Peggy", "Parsimmon", "Nearley+moo"],
      "series": [{"label": "Throughput", "values": [22959, 4252, 3649, 908, 806, 350]}] },
    { "name": "apache (124 KB)", "icon": "ts",
      "labels": ["JSON.parse", "parse-that", "Chevrotain", "Peggy", "Parsimmon", "Nearley+moo"],
      "series": [{"label": "Throughput", "values": [6524, 1250, 878, 235, 213, 62]}] },
    { "name": "twitter (617 KB)", "icon": "ts",
      "labels": ["JSON.parse", "parse-that", "Chevrotain", "Peggy", "Parsimmon", "Nearley+moo"],
      "series": [{"label": "Throughput", "values": [947, 211, 144, 54, 36, 18]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "ts",
      "labels": ["JSON.parse", "parse-that", "Chevrotain", "Peggy", "Parsimmon", "Nearley+moo"],
      "series": [{"label": "Throughput", "values": [619, 85, 72, 24, 16, 8]}] },
    { "name": "canada (2.1 MB)", "icon": "ts",
      "labels": ["JSON.parse", "parse-that", "Chevrotain", "Peggy", "Parsimmon", "Nearley+moo"],
      "series": [{"label": "Throughput", "values": [125, 41, 22, 14, 5, 3]}] }
  ] }
```

parse-that's TS performance comes from three optimization phases:

1. **Mutable ParserState**—single reused object, zero-alloc save/restore
2. **BBNF graph optimizations**—Tarjan SCC, FIRST-set dispatch, regex coalescing
3. **V8-specific tuning**—`RegExp.test()` + `substring()` instead of `exec()` allocation
