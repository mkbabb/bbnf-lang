---
title: Overview
order: 10
section: parse-that
---

# parse-that

A parser combinator library with isomorphic TypeScript and Rust implementations. Build parsers by composing small, reusable pieces—string matchers, regex patterns, and combinators like `then`, `or`, and `many`—into complex grammars.

## Features

- **Dual implementation**—TypeScript (npm) and Rust (crates.io) with matching APIs
- **Zero-copy spans**—parse without allocating substrings via the span combinator system
- **Error recovery**—`recover()` combinator collects multiple diagnostics in a single pass
- **Memoization**—built-in packrat memoization with left-recursion support
- **O(1) dispatch**—first-character lookup tables for fast alternation
- **Bespoke regex engine**—NFA→DFA compilation with Hopcroft minimization, no `regex` crate at runtime (Rust)
- **BumpSlab**—byte-based bump allocator for zero-overhead arena parsing (Rust)
- **Zero runtime deps**—the TypeScript package has no dependencies; Rust has no `regex` crate dependency

## Installation

```code-tabs
---bash---
# TypeScript / npm
npm install @mkbabb/parse-that
---toml---
# Rust / Cargo
[dependencies]
parse_that = "0.4"
```

## Quick Example

Build a parser for comma-separated integers wrapped in brackets:

```code-tabs
---typescript---
import { Parser, string, regex, any } from "@mkbabb/parse-that";

const integer = regex(/-?\d+/).map(Number);
const comma = string(",").trim();
const intList = integer
    .sepBy(comma, 1)
    .wrap(string("["), string("]"))
    .trim();

const result = intList.parse("[10, -20, 30]");
// result: [10, -20, 30]
---rust---
use parse_that::{string, regex};

let integer = regex(r"-?\d+").map(|s: &str| s.parse::<i64>().unwrap());
let comma = string(",").trim();
let int_list = integer
    .sep_by(comma, 1..)
    .wrap(string("["), string("]"))
    .trim();

let result = int_list.parse("[10, -20, 30]");
// result: [10, -20, 30]
```

## Core Concepts

### Parser\<T\>

Every parser is an instance of `Parser<T>`, where `T` is the type of value it produces on success. Parsers are immutable—each combinator method returns a new `Parser` rather than mutating the original.

### ParserState

Parsing operates on a mutable `ParserState` that tracks the current `offset` into the source string, the most recent `value`, and whether an error occurred (`isError`). Backtracking is handled automatically by combinators—on failure, the offset rewinds to where the combinator started.

### Composition

Parsers compose through method chaining:

```code-tabs
---typescript---
// Sequence: parse A then B, return both values
const pair = parserA.then(parserB);       // Parser<[A, B]>

// Alternative: try A, fall back to B
const either = parserA.or(parserB);       // Parser<A | B>

// Transform the result
const mapped = integer.map(n => n * 2);   // Parser<number>
---rust---
// Sequence: parse A then B, return both values
let pair = parser_a.then(parser_b);       // Parser<(A, B)>

// Alternative: try A, fall back to B
let either = parser_a.or(parser_b);       // Parser<A | B>

// Transform the result
let mapped = integer.map(|n| n * 2);     // Parser<i64>
```

### Recursive Grammars

Use `Parser.lazy()` (TypeScript) or the free function `lazy()` (Rust) to define recursive parsers without forward-declaration issues:

```code-tabs
---typescript---
const expr: Parser<any> = Parser.lazy(() =>
    any(number, expr.wrap(string("("), string(")")))
);
---rust---
let expr = lazy(|| {
    number.or(expr.wrap(string("("), string(")")))
});
```

## Performance

parse-that drives the parsing stage for all BBNF-generated parsers. Throughput on JSON (data.json, 35 KB):

```bench-chart
{ "title": "JSON Parsing Throughput (Rust)", "unit": "MB/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "rust",
      "labels": ["BBNF borrow", "serde_json", "nom", "winnow", "pest"],
      "series": [{"label": "Throughput", "values": [3430, 959, 673, 603, 232]}] }
  ] }
```

BBNF AOT generates parse-that combinators with dispatch tables and span parsing, reaching 3,430 MB/s on data.json — 3.6x faster than serde_json and 5.1x faster than nom.

See [Parsing Performance](/docs/performance/parsing) for full benchmarks across file sizes and languages.

## Next Steps

- [Combinators](./combinators)—all combinator methods on `Parser<T>`
- [Leaf Parsers](./leaf-parsers)—primitive parser constructors
- [Span Combinators](./span-combinators)—zero-copy parsing with spans
