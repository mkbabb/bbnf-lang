---
title: Parsing
order: 41
section: Performance
---

# Parsing Performance

parse_that (Rust) and parse-that (TypeScript) are the parsing backbone. Both use dispatch tables, FIRST-set routing, and memoization tuned for their respective runtimes.

## Rust: `JSON` — Arena Span

Arena parsing returns opaque spans allocated via `BumpArena`. Each iteration constructs a fresh arena and parser—cold per-parse throughput.

```bench-chart
{ "title": "JSON Arena Span", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["BBNF arena"],
      "series": [{"label": "Throughput", "values": [1261]}] },
    { "name": "twitter (631 KB)", "icon": "rust",
      "labels": ["BBNF arena"],
      "series": [{"label": "Throughput", "values": [1347]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "rust",
      "labels": ["BBNF arena"],
      "series": [{"label": "Throughput", "values": [1597]}] },
    { "name": "canada (2.2 MB)", "icon": "rust",
      "labels": ["BBNF arena"],
      "series": [{"label": "Throughput", "values": [1115]}] },
    { "name": "data_xl (20 MB)", "icon": "rust",
      "labels": ["BBNF arena"],
      "series": [{"label": "Throughput", "values": [815]}] }
  ] }
```

## Rust: `JSON` — Arena Borrow (No Decode)

Numbers parsed to f64, strings borrowed from input without escape decoding. Cold per-parse with `BumpArena`. BBNF uses `Vec<(K,V)>` for objects; nom, winnow, and pest use `HashMap<&str,V>`.

```bench-chart
{ "title": "JSON Arena Borrow — No Decode", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["BBNF arena borrow", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [1019, 657, 609, 229]}] },
    { "name": "twitter (631 KB)", "icon": "rust",
      "labels": ["BBNF arena borrow", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [1156, 540, 586, 224]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "rust",
      "labels": ["BBNF arena borrow", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [1257, 703, 679, 186]}] },
    { "name": "canada (2.2 MB)", "icon": "rust",
      "labels": ["BBNF arena borrow", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [598, 447, 440, 106]}] },
    { "name": "data_xl (20 MB)", "icon": "rust",
      "labels": ["BBNF arena borrow", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [632, 491, 441, 154]}] }
  ] }
```

All benchmarks use mimalloc. nom, winnow, and pest construct per-iteration (cold).

## Rust: `JSON` — Borrow (With Decode)

Zero-copy output with escape decoding during parse. These parsers borrow from the input when possible but decode escape sequences, doing significantly more work than the no-decode tier.

```bench-chart
{ "title": "JSON Borrow — With Decode", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["jiter", "serde_json_borrow", "simd-json"],
      "series": [{"label": "Throughput", "values": [1475, 1515, 1364]}] },
    { "name": "twitter (631 KB)", "icon": "rust",
      "labels": ["jiter", "serde_json_borrow", "simd-json"],
      "series": [{"label": "Throughput", "values": [1222, 1652, 1375]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "rust",
      "labels": ["jiter", "serde_json_borrow", "simd-json"],
      "series": [{"label": "Throughput", "values": [1295, 1416, 1696]}] },
    { "name": "canada (2.2 MB)", "icon": "rust",
      "labels": ["jiter", "serde_json_borrow", "simd-json"],
      "series": [{"label": "Throughput", "values": [607, 660, 733]}] },
    { "name": "data_xl (20 MB)", "icon": "rust",
      "labels": ["jiter", "serde_json_borrow", "simd-json"],
      "series": [{"label": "Throughput", "values": [798, 890, 982]}] }
  ] }
```

serde_json_borrow returns a zero-copy borrowed `Value` with full escape decoding. jiter (Pydantic's parser) uses `Cow` for selective decode. simd-json uses SIMD-accelerated scanning but pays a `.to_vec()` copy cost per iteration.

## Rust: `JSON` — Arena Owned (Full Decode)

Full escape decoding with `Cow<'a, str>` strings (borrows when clean, allocates for escapes). Cold per-parse with `BumpArena`. sonic-rs uses SIMD + its own arena allocation.

```bench-chart
{ "title": "JSON Arena Owned — Full Decode", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["BBNF arena owned", "sonic-rs", "serde_json"],
      "series": [{"label": "Throughput", "values": [887, 2293, 930]}] },
    { "name": "twitter (631 KB)", "icon": "rust",
      "labels": ["BBNF arena owned", "sonic-rs", "serde_json"],
      "series": [{"label": "Throughput", "values": [900, 2522, 867]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "rust",
      "labels": ["BBNF arena owned", "sonic-rs", "serde_json"],
      "series": [{"label": "Throughput", "values": [1173, 3031, 1132]}] },
    { "name": "canada (2.2 MB)", "icon": "rust",
      "labels": ["BBNF arena owned", "sonic-rs", "serde_json"],
      "series": [{"label": "Throughput", "values": [595, 1499, 607]}] },
    { "name": "data_xl (20 MB)", "icon": "rust",
      "labels": ["BBNF arena owned", "sonic-rs", "serde_json"],
      "series": [{"label": "Throughput", "values": [567, 1445, 647]}] }
  ] }
```

All Rust benchmarks use mimalloc. BBNF parsers are generated from a `.bbnf` grammar via `#[derive(Parser)]` with zero hand-written Rust.

## Rust: `CSS` — Arena Structural Scan

**BBNF fast** (css-fast.bbnf) uses `@ws` for SIMD comment-aware whitespace and `@inline` for trivial helper rules. Cold per-parse with `BumpArena`. **cssparser** (Mozilla's tokenizer) uses a visitor pattern that counts rules and declarations without building an AST.

```bench-chart
{ "title": "CSS Arena Structural Scan", "unit": "MB/s",
  "datasets": [
    { "name": "normalize (6 KB)", "icon": "rust",
      "labels": ["BBNF fast arena", "cssparser"],
      "series": [{"label": "Throughput", "values": [760, 655]}] },
    { "name": "bootstrap (281 KB)", "icon": "rust",
      "labels": ["BBNF fast arena", "cssparser"],
      "series": [{"label": "Throughput", "values": [331, 424]}] },
    { "name": "tailwind (3.8 MB)", "icon": "rust",
      "labels": ["BBNF fast arena", "cssparser"],
      "series": [{"label": "Throughput", "values": [28, 402]}] }
  ] }
```

On normalize (6 KB), BBNF arena is 1.2x cssparser. On bootstrap and tailwind the per-rule overhead of recursive descent grammar parsing (alternation dispatch, whitespace scanning, arena allocation per rule) exceeds cssparser's flat tokenizer loop. Tailwind's ~65K tiny utility classes (~40 bytes each) hit this hardest—fixed per-rule costs don't amortize on small rules.

## Rust: `CSS` — Structural AST

**BBNF pretty** (css-stylesheet-pretty.bbnf) builds a typed enum tree with rule/block/declaration structure, using opaque regex spans for selectors and values (L1.5). **lightningcss** (Parcel) performs a full L2 semantic parse—typed CSS properties, vendor prefix analysis, CSS Nesting validation.

```bench-chart
{ "title": "CSS Structural AST", "unit": "MB/s",
  "datasets": [
    { "name": "normalize (6 KB)", "icon": "rust",
      "labels": ["BBNF pretty", "lightningcss"],
      "series": [{"label": "Throughput", "values": [1969, 257]}] },
    { "name": "bootstrap (281 KB)", "icon": "rust",
      "labels": ["BBNF pretty", "lightningcss"],
      "series": [{"label": "Throughput", "values": [1000, 117]}] },
    { "name": "tailwind (3.8 MB)", "icon": "rust",
      "labels": ["BBNF pretty", "lightningcss"],
      "series": [{"label": "Throughput", "values": [968, 94]}] }
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
