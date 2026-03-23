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
    {"label": "parse_that / parse-that", "detail": "815–1,597 MB/s (Rust arena, cold) · 4,252–22,959 ops/s (TS)", "color": "blue"},
    {"label": "to_doc()", "detail": "1,026 MB/s (CSS bootstrap)", "color": "green"},
    {"label": "pprint::render", "detail": "1,115 MB/s (CSS bootstrap)", "color": "purple"},
    {"label": "Formatted output", "detail": "20–409 MB/s end-to-end", "color": "amber"}
  ] }
```

Three codegen paths feed this pipeline:

| Path | When | Compile | Runtime |
|------|------|---------|---------|
| **Rust AOT** | `#[derive(Parser)]` | ~2s rustc (8-rule grammar) | Native speed |
| **Rust VM** | WASM playground | ~2ms (bytecode compile) | ~6–50x slower than AOT |
| **TS Interpreter** | `ASTToParser()` | ~1ms (combinator gen) | V8 JIT speed |

## Aggregate Throughput

```bench-chart
{ "title": "Aggregate Throughput", "unit": "MB/s",
  "datasets": [
    { "name": "Parsing — Arena (Rust, cold)", "icon": "rust",
      "labels": ["BBNF arena", "nom", "winnow", "pest"],
      "series": [{"label": "data.json 35 KB", "values": [1261, 657, 609, 229]}] },
    { "name": "Formatting (Rust)", "icon": "rust",
      "labels": ["gorgeous E2E", "gorgeous (cached)", "Biome"],
      "series": [{"label": "bootstrap.css 281 KB", "values": [205, 409, 16]}] },
    { "name": "Google Sheets", "icon": "rust",
      "labels": ["AOT parse", "AOT format", "VM parse"],
      "series": [{"label": "1 KB formulas", "values": [34, 8, 1]}] }
  ] }
```

## Deep Dives

- [Parsing Performance](./parsing)—parse_that throughput, dispatch tables, FIRST sets
- [Formatting Performance](./formatting)—gorgeous end-to-end, to_doc + render phases
- [pprint Performance](./pprint)—render throughput, inline text variants
- [LSP Performance](./lsp)—latency by operation, analysis pipeline
- [WASM Performance](./wasm)—module size, init time, live browser benchmarks
