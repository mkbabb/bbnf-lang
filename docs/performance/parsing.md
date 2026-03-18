---
title: Parsing
order: 41
section: Performance
---

# Parsing Performance

parse_that (Rust) and parse-that (TypeScript) are the parsing backbone. Both use dispatch tables, FIRST-set routing, and memoization tuned for their respective runtimes.

## Rust: `JSON` — Span

Span parsing returns borrowed byte slices without decoding strings or parsing numbers. No tree is built—structural validation only.

```bench-chart
{ "title": "JSON Span Parsing", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["BBNF span"],
      "series": [{"label": "Throughput", "values": [3402]}] },
    { "name": "apache (127 KB)", "icon": "rust",
      "labels": ["BBNF span"],
      "series": [{"label": "Throughput", "values": [4237]}] },
    { "name": "twitter (631 KB)", "icon": "rust",
      "labels": ["BBNF span"],
      "series": [{"label": "Throughput", "values": [4080]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "rust",
      "labels": ["BBNF span"],
      "series": [{"label": "Throughput", "values": [3916]}] },
    { "name": "canada (2.2 MB)", "icon": "rust",
      "labels": ["BBNF span"],
      "series": [{"label": "Throughput", "values": [2175]}] }
  ] }
```

## Rust: `JSON` — Borrowed

Numbers parsed to f64, strings borrowed from input (no escape decoding). Builds a Vec/Object tree. All parsers in this tier borrow unescaped strings directly from the input buffer.

```bench-chart
{ "title": "JSON Borrowed", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["sonic-rs", "BBNF borrow", "simd-json", "jiter", "serde_json_borrow", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [2323, 2077, 1922, 1465, 1421, 576, 524, 255]}] },
    { "name": "apache (127 KB)", "icon": "rust",
      "labels": ["BBNF borrow", "sonic-rs", "simd-json", "serde_json_borrow", "jiter", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [2524, 1949, 1889, 1316, 1286, 690, 645, 272]}] },
    { "name": "twitter (631 KB)", "icon": "rust",
      "labels": ["sonic-rs", "BBNF borrow", "simd-json", "serde_json_borrow", "jiter", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [2515, 2325, 1749, 1585, 1178, 496, 525, 222]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "rust",
      "labels": ["sonic-rs", "BBNF borrow", "simd-json", "serde_json_borrow", "jiter", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [3037, 2129, 1878, 1539, 1274, 607, 581, 250]}] },
    { "name": "canada (2.2 MB)", "icon": "rust",
      "labels": ["sonic-rs", "BBNF borrow", "simd-json", "serde_json_borrow", "jiter", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [1521, 776, 754, 713, 643, 391, 390, 154]}] }
  ] }
```

nom, winnow, and pest all produce borrowed output (`&'a str` strings, no escape decoding). sonic-rs and simd-json use SIMD-accelerated string scanning. The nom/winnow/pest numbers are pre-mimalloc and will be updated in a future run.

## Rust: `JSON` — Owned

Full escape decoding via `Cow<'a, str>`—borrows when clean, allocates when escaped. Numbers parsed to f64. Full deserialization with Vec/Object tree.

```bench-chart
{ "title": "JSON Owned", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["BBNF owned", "serde_json"],
      "series": [{"label": "Throughput", "values": [1583, 875]}] },
    { "name": "apache (127 KB)", "icon": "rust",
      "labels": ["BBNF owned", "serde_json"],
      "series": [{"label": "Throughput", "values": [1619, 851]}] },
    { "name": "twitter (631 KB)", "icon": "rust",
      "labels": ["BBNF owned", "serde_json"],
      "series": [{"label": "Throughput", "values": [1545, 794]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "rust",
      "labels": ["BBNF owned", "serde_json"],
      "series": [{"label": "Throughput", "values": [1630, 1241]}] },
    { "name": "canada (2.2 MB)", "icon": "rust",
      "labels": ["BBNF owned", "serde_json"],
      "series": [{"label": "Throughput", "values": [783, 642]}] }
  ] }
```

All Rust benchmarks use mimalloc as the global allocator for consistent results. The BBNF parsers are generated from a `.bbnf` grammar via `#[derive(Parser)]`—zero hand-written Rust.

## Rust: `CSS` — Tier 1: Structural Scan

**BBNF fast** (css-fast.bbnf) returns opaque spans—selectors and values are captured as raw byte slices without interpretation. **cssparser** (Mozilla's tokenizer) uses a visitor pattern that counts rules and declarations without building an AST. Both do minimal work—fair head-to-head.

```bench-chart
{ "title": "CSS Structural Scan", "unit": "MB/s",
  "datasets": [
    { "name": "normalize (1.8 KB)", "icon": "rust",
      "labels": ["BBNF fast", "cssparser"],
      "series": [{"label": "Throughput", "values": [956, 326]}] },
    { "name": "bootstrap (281 KB)", "icon": "rust",
      "labels": ["BBNF fast", "cssparser"],
      "series": [{"label": "Throughput", "values": [1174, 435]}] },
    { "name": "tailwind (3.8 MB)", "icon": "rust",
      "labels": ["BBNF fast", "cssparser"],
      "series": [{"label": "Throughput", "values": [194, 360]}] }
  ] }
```

BBNF fast is 2.7x faster than cssparser on bootstrap. On tailwind (3.8 MB), cssparser's tokenizer pulls ahead—the BBNF regex engine's per-match overhead amortizes less favorably on tailwind's repetitive utility classes.

## Rust: `CSS` — Tier 2: Structural AST

**BBNF pretty** (css-stylesheet-pretty.bbnf) builds a typed enum tree with rule/block/declaration structure, using opaque regex spans for selectors and values (L1.5). **lightningcss** (Parcel) performs a full L2 semantic parse—typed CSS properties, vendor prefix analysis, CSS Nesting validation. lightningcss does *more* work, so the comparison quantifies how much overhead semantic analysis adds.

```bench-chart
{ "title": "CSS Structural AST", "unit": "MB/s",
  "datasets": [
    { "name": "normalize (1.8 KB)", "icon": "rust",
      "labels": ["BBNF pretty", "lightningcss"],
      "series": [{"label": "Throughput", "values": [792, 91]}] },
    { "name": "bootstrap (281 KB)", "icon": "rust",
      "labels": ["BBNF pretty", "lightningcss"],
      "series": [{"label": "Throughput", "values": [854, 113]}] },
    { "name": "tailwind (3.8 MB)", "icon": "rust",
      "labels": ["BBNF pretty"],
      "series": [{"label": "Throughput", "values": [164]}] }
  ] }
```

BBNF pretty is 7.5x faster than lightningcss on bootstrap. lightningcss errors on synthetic tailwind output and is omitted. See the [formatting benchmarks](./formatting) for gorgeous vs Biome end-to-end comparisons.

## Rust: `Google Sheets`

AOT vs VM on formula parsing. The VM interprets bytecode; AOT generates native Rust. The VM gap narrows on larger inputs as bytecode dispatch overhead amortizes. AOT is 81x faster on pathological inputs, 34x on 1 KB, 16x on 10 KB.

```bench-chart
{ "title": "Google Sheets — AOT vs VM", "unit": "ns", "lowerIsBetter": true,
  "datasets": [
    { "name": "pathological (270 B)", "icon": "rust",
      "labels": ["AOT", "VM"],
      "series": [{"label": "Latency", "values": [5349, 434494]}] },
    { "name": "1 KB formulas", "icon": "rust",
      "labels": ["AOT", "VM"],
      "series": [{"label": "Latency", "values": [19735, 665921]}] },
    { "name": "10 KB formulas", "icon": "rust",
      "labels": ["AOT", "VM"],
      "series": [{"label": "Latency", "values": [247747, 4074964]}] }
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
