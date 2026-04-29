---
title: Performance Overview
order: 40
section: Performance
---

# Performance Overview

Each pipeline stage is benchmarked independently.

## Pipeline

```flow-chart
{ "title": "Formatting Pipeline",
  "nodes": [
    {"label": "Source text", "color": "cyan"},
    {"label": "parse_that / parse-that", "detail": "289–2,571 MB/s (Rust slab+span, cold) · 4,252–22,959 ops/s (TS)", "color": "blue"},
    {"label": "to_doc()", "detail": "158–356 MB/s (CSS)", "color": "green"},
    {"label": "pprint::render", "detail": "179–254 MB/s (CSS)", "color": "purple"},
    {"label": "Formatted output", "detail": "22–115 MB/s end-to-end", "color": "amber"}
  ] }
```

Current codegen paths feeding this pipeline:

| Path | When | Compile | Runtime |
|------|------|---------|---------|
| **Rust AOT** | `cargo xtask regen` checked-in generated source | xtask regen + rustc compile | Native speed |
| **Rust VM** | WASM playground | ~2ms (bytecode compile) | ~6–50x slower than AOT |
| **TS Interpreter** | `ASTToParser()` | ~1ms (combinator gen) | V8 JIT speed |

Post-B7, divan is the live Rust benchmark harness and nextest is the
live test runner. Post-AZ-II partial-close performance numbers are not
terminal: the next publishable baseline is the post-`cutover.O` matrix
after EBNF activation, tape deletion, and parity refresh.

## Aggregate Throughput

```bench-chart
{ "title": "Aggregate Throughput", "unit": "MB/s",
  "datasets": [
    { "name": "Parsing — Slab (Rust, cold)", "icon": "rust",
      "labels": ["BBNF slab", "nom", "winnow", "pest"],
      "series": [{"label": "data.json 35 KB", "values": [1197, 417, 416, 112]}] },
    { "name": "Formatting (Rust)", "icon": "rust",
      "labels": ["gorgeous E2E", "gorgeous (cached)", "Biome"],
      "series": [{"label": "bootstrap.css 281 KB", "values": [205, 409, 16]}] },
    { "name": "Google Sheets", "icon": "rust",
      "labels": ["AOT parse", "AOT format", "VM parse"],
      "series": [{"label": "1 KB formulas", "values": [34, 8, 1]}] }
  ] }
```

## Deep Dives

- [Parsing Performance](./parsing)—throughput across JSON and CSS tiers
- [Optimization Timeline](./timeline)—the full arc from combinators to monolithic codegen
- [Regex Codegen](./regex-codegen)—three-tier emission pipeline
- [Formatting Performance](./formatting)—gorgeous end-to-end, to_doc + render phases
- [pprint Performance](./pprint)—render throughput, inline text variants
- [LSP Performance](./lsp)—latency by operation, analysis pipeline
- [WASM Performance](./wasm)—module size, init time, live browser benchmarks
