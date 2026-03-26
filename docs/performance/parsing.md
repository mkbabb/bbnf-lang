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
