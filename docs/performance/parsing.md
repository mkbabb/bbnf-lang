---
title: Parsing
order: 41
section: Performance
---

# Parsing Performance

parse_that (Rust) and parse-that (TypeScript) are the parsing backbone. Both use dispatch tables, FIRST-set routing, and memoization tuned for their respective runtimes.

## Rust: JSON

Benchmarked across six JSON datasets with ten parsers using the `bencher` crate. All runs validate output. SIMD parsers (sonic-rs, simd-json) use vectorized string scanning and are not combinator-based. Each dataset is sorted by throughput descending.

```bench-chart
{ "title": "Rust JSON Parsing", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["sonic-rs", "simd-json", "jiter", "serde_json_borrow", "BBNF AOT", "parse-that", "nom", "winnow", "serde_json", "pest"],
      "series": [{"label": "Throughput", "values": [2277, 1543, 1443, 1254, 1183, 663, 566, 519, 508, 244]}] },
    { "name": "apache (127 KB)", "icon": "rust",
      "labels": ["sonic-rs", "BBNF AOT", "simd-json", "jiter", "serde_json_borrow", "parse-that", "nom", "winnow", "serde_json", "pest"],
      "series": [{"label": "Throughput", "values": [1853, 1638, 1413, 1120, 1118, 735, 689, 613, 501, 270]}] },
    { "name": "citm_catalog (1.7 MB)", "icon": "rust",
      "labels": ["sonic-rs", "BBNF AOT", "serde_json_borrow", "simd-json", "jiter", "parse-that", "serde_json", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [2958, 1520, 1281, 1235, 1004, 837, 746, 611, 571, 244]}] },
    { "name": "canada (2.2 MB)", "icon": "rust",
      "labels": ["sonic-rs", "BBNF AOT", "serde_json_borrow", "serde_json", "jiter", "simd-json", "winnow", "nom", "parse-that", "pest"],
      "series": [{"label": "Throughput", "values": [1494, 1260, 614, 569, 562, 477, 385, 377, 376, 153]}] },
    { "name": "twitter (631 KB)", "icon": "rust",
      "labels": ["sonic-rs", "BBNF AOT", "simd-json", "serde_json_borrow", "jiter", "parse-that", "serde_json", "winnow", "nom", "pest"],
      "series": [{"label": "Throughput", "values": [2416, 1599, 1468, 1304, 1017, 786, 535, 509, 488, 229]}] },
    { "name": "data_xl (39 MB)", "icon": "rust",
      "labels": ["sonic-rs", "simd-json", "jiter", "serde_json_borrow", "BBNF AOT", "parse-that", "nom", "winnow", "serde_json", "pest"],
      "series": [{"label": "Throughput", "values": [2680, 1519, 1351, 1223, 1052, 946, 596, 563, 549, 247]}] }
  ] }
```

The BBNF-generated parser outperforms the hand-rolled version on string-heavy inputs because the codegen emits `memchr2`-accelerated string scanning.

## Rust: CSS

BBNF AOT and parse-that produce a typed AST; lightningcss and cssparser are Mozilla-derived. lightningcss does not have a tailwind benchmark in the suite.

```bench-chart
{ "title": "Rust CSS Parsing", "unit": "MB/s",
  "datasets": [
    { "name": "bootstrap (281 KB)", "icon": "rust",
      "labels": ["BBNF AOT", "cssparser", "parse-that", "lightningcss"],
      "series": [{"label": "Throughput", "values": [661, 414, 249, 100]}] },
    { "name": "normalize (6 KB)", "icon": "rust",
      "labels": ["cssparser", "BBNF AOT", "parse-that", "lightningcss"],
      "series": [{"label": "Throughput", "values": [651, 488, 493, 220]}] },
    { "name": "tailwind (3.8 MB)", "icon": "rust",
      "labels": ["BBNF AOT", "cssparser", "parse-that"],
      "series": [{"label": "Throughput", "values": [382, 257, 224]}] }
  ] }
```

## Rust: Google Sheets

AOT vs VM on formula parsing. The VM interprets bytecode; AOT generates native Rust. The VM gap narrows on larger inputs as bytecode dispatch overhead amortizes. AOT is 78x faster on pathological inputs, 34x on 1 KB, 17x on 10 KB.

```bench-chart
{ "title": "Google Sheets Parsing — AOT vs VM", "unit": "ns", "lowerIsBetter": true,
  "datasets": [
    { "name": "pathological (270 B)", "icon": "rust",
      "labels": ["AOT", "VM"],
      "series": [{"label": "Latency", "values": [5562, 434494]}] },
    { "name": "1 KB formulas", "icon": "rust",
      "labels": ["AOT", "VM"],
      "series": [{"label": "Latency", "values": [19620, 665921]}] },
    { "name": "10 KB formulas", "icon": "rust",
      "labels": ["AOT", "VM"],
      "series": [{"label": "Latency", "values": [242117, 4074964]}] }
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
