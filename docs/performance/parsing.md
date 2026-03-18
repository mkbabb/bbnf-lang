---
title: Parsing
order: 41
section: Performance
---

# Parsing Performance

parse_that (Rust) and parse-that (TypeScript) are the parsing backbone. Both use dispatch tables, FIRST-set routing, and memoization tuned for their respective runtimes.

## Rust: JSON — Span Parsing (Zero-Copy Validation)

Span parsing returns borrowed byte slices without decoding strings or parsing numbers. No tree is built. This measures raw grammar traversal speed—structural validation only.

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

## Rust: JSON — Borrowed Parsing (f64 + Borrowed Strings)

Borrowed parsing decodes numbers to f64 and borrows strings from the input buffer (no escape handling). Builds a Vec/Object tree. Comparable to sonic-rs, simd-json, and serde_json_borrow, which also borrow unescaped strings from input.

```bench-chart
{ "title": "JSON Borrowed Parsing", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["sonic-rs", "BBNF borrow", "simd-json", "jiter", "serde_json_borrow"],
      "series": [{"label": "Throughput", "values": [2323, 2077, 1922, 1465, 1421]}] },
    { "name": "apache (127 KB)", "icon": "rust",
      "labels": ["BBNF borrow", "sonic-rs", "simd-json", "serde_json_borrow", "jiter"],
      "series": [{"label": "Throughput", "values": [2524, 1949, 1889, 1316, 1286]}] },
    { "name": "twitter (631 KB)", "icon": "rust",
      "labels": ["sonic-rs", "BBNF borrow", "simd-json", "serde_json_borrow", "jiter"],
      "series": [{"label": "Throughput", "values": [2515, 2325, 1749, 1585, 1178]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "rust",
      "labels": ["sonic-rs", "BBNF borrow", "simd-json", "serde_json_borrow", "jiter"],
      "series": [{"label": "Throughput", "values": [3037, 2129, 1878, 1539, 1274]}] },
    { "name": "canada (2.2 MB)", "icon": "rust",
      "labels": ["sonic-rs", "BBNF borrow", "simd-json", "serde_json_borrow", "jiter"],
      "series": [{"label": "Throughput", "values": [1521, 776, 754, 713, 643]}] }
  ] }
```

## Rust: JSON — Owned Parsing (f64 + Decoded Strings)

Owned parsing decodes numbers to f64 and handles string escapes via Cow—borrows when clean, allocates when escaped. Full deserialization with Vec/Object tree construction.

```bench-chart
{ "title": "JSON Owned Parsing", "unit": "MB/s",
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

## Rust: CSS

Two grammar tiers: **fast** (css-fast.bbnf) returns opaque spans for maximum throughput; **pretty** (css-stylesheet-pretty.bbnf) builds a structural AST for formatting.

```bench-chart
{ "title": "Rust CSS Parsing", "unit": "MB/s",
  "datasets": [
    { "name": "bootstrap (281 KB)", "icon": "rust",
      "labels": ["BBNF fast", "BBNF pretty"],
      "series": [{"label": "Throughput", "values": [1106, 729]}] },
    { "name": "normalize (6 KB)", "icon": "rust",
      "labels": ["BBNF fast", "BBNF pretty"],
      "series": [{"label": "Throughput", "values": [2279, 1897]}] },
    { "name": "tailwind (3.8 MB)", "icon": "rust",
      "labels": ["BBNF fast", "BBNF pretty"],
      "series": [{"label": "Throughput", "values": [271, 89]}] }
  ] }
```

## Rust: Google Sheets

AOT vs VM on formula parsing. The VM interprets bytecode; AOT generates native Rust. The VM gap narrows on larger inputs as bytecode dispatch overhead amortizes. AOT is 81x faster on pathological inputs, 34x on 1 KB, 16x on 10 KB.

```bench-chart
{ "title": "Google Sheets Parsing — AOT vs VM", "unit": "ns", "lowerIsBetter": true,
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

## TypeScript: JSON

Benchmarked with vitest across five datasets. parse-that consistently outperforms Chevrotain, with the gap widening on larger inputs.

```bench-chart
{ "title": "TypeScript JSON Parsing", "unit": "ops/s",
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

## Dispatch Tables

When alternation branches have disjoint FIRST sets, the codegen emits an O(1) character-dispatch lookup:

```code-tabs
---rust---
// Generated by #[derive(Parser)]
fn value(&self, state: &mut ParserState<'a>) -> Parser<'a, Value<'a>> {
    dispatch! {
        b'"' => self.string(state),
        b'0'..=b'9' | b'-' => self.number(state),
        b'{' => self.object(state),
        b'[' => self.array(state),
        b't' => self.parse_true(state),
        b'f' => self.parse_false(state),
        b'n' => self.parse_null(state),
    }
}
---typescript---
// Generated by ASTToParser()
const value = dispatch({
    '"': jsonString,
    "0-9": jsonNumber,
    "-": jsonNumber,
    "{": jsonObject,
    "[": jsonArray,
    "t": string("true").map(() => true),
    "f": string("false").map(() => false),
    "n": string("null").map(() => null),
});
```

The leading byte selects the parser in constant time, eliminating sequential trial across branches.

## FIRST Sets

Every rule's FIRST set is a 128-bit `CharSet` covering ASCII, computed iteratively to fixed point over cyclic rules:

```code-tabs
---rust---
use bbnf::analysis::CharSet;

// FIRST set computation — iterates to fixed point
let first_sets = grammar.compute_first_sets();
// first_sets["value"] = CharSet { '"', '0'..='9', '-', '{', '[', 't', 'f', 'n' }

// Use in dispatch table generation
let table = grammar.generate_dispatch_table(&first_sets);
---typescript---
import { computeFirstSets } from "@mkbabb/bbnf-lang";

// FIRST set computation — iterates to fixed point
const firstSets = computeFirstSets(grammar);
// firstSets.get("value") = Set { '"', '0'-'9', '-', '{', '[', 't', 'f', 'n' }

// Use in dispatch table generation
const table = generateDispatchTable(firstSets);
```

```
value: {", 0-9, -, {, [, t, f, n}
object: {{}
array: {[}
string: {"}
number: {0-9, -}
```

When FIRST sets overlap between alternation branches, the codegen falls back to sequential `any()` and the LSP emits an ambiguity warning.
