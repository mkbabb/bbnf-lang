---
title: Formatting
order: 42
section: Performance
---

# Formatting Performance

gorgeous generates formatters from `@pretty` directives in BBNF grammars. Layout logic is defined in the grammar, not hand-written.

## Pipeline Phases

```flow-chart
{ "title": "Formatting Pipeline Phases (bootstrap.css 281 KB)",
  "nodes": [
    {"label": "Parse (parse_that)", "detail": "L0 tokenization", "color": "cyan"},
    {"label": "to_doc (AST → Doc)", "detail": "1,026 MB/s", "color": "blue"},
    {"label": "Render (pprint)", "detail": "1,115 MB/s", "color": "green"},
    {"label": "End-to-end", "detail": "205 MB/s · cached 409 MB/s", "color": "amber"}
  ] }
```

The parse phase uses an L0 tokenization pass—it needs a flat token stream for formatting, not a typed AST. The `to_doc` phase (1,026 MB/s) is the throughput-limiting stage; `render` runs at 1,115 MB/s.

## End-to-End: CSS

gorgeous vs Biome on real-world CSS files:

```bench-chart
{ "title": "CSS Formatting Throughput", "unit": "MB/s",
  "datasets": [
    { "name": "app.css (6 KB)", "icon": "rust",
      "labels": ["gorgeous", "gorgeous (cached)", "Biome"],
      "series": [{"label": "Throughput", "values": [30, 56, 10]}] },
    { "name": "bootstrap.css (281 KB)", "icon": "rust",
      "labels": ["gorgeous", "gorgeous (cached)", "Biome"],
      "series": [{"label": "Throughput", "values": [205, 409, 16]}] },
    { "name": "tailwind.css (3.8 MB)", "icon": "rust",
      "labels": ["gorgeous", "gorgeous (cached)", "Biome"],
      "series": [{"label": "Throughput", "values": [20, 46, 14]}] }
  ] }
```

| File | Size | gorgeous | gorgeous (cached) | Biome | Speedup |
|------|------|----------|-------------------|-------|---------|
| app.css | 6 KB | 30 MB/s | 56 MB/s | 10 MB/s | 3x |
| bootstrap.css | 281 KB | 205 MB/s | 409 MB/s | 16 MB/s | 13x |
| tailwind.css | 3.8 MB | 20 MB/s | 46 MB/s | 14 MB/s | 1.4x |

"Cached" means the parse result is reused across formatting calls. The 13x speedup on bootstrap occurs because fixed overhead is amortized at this file size while the working set fits in cache.

For parse-only throughput comparisons against cssparser and lightningcss, see the [parsing benchmarks](./parsing#rust-css--tier-1-structural-scan).

## Google Sheets Formatting

AOT and VM both support formula formatting. Phase-split benchmarks on 1 KB formula inputs:

```bench-chart
{ "title": "Google Sheets Formatting — 1 KB", "unit": "ns",
  "datasets": [
    { "name": "Parse", "icon": "rust",
      "labels": ["AOT", "VM"],
      "series": [{"label": "Time", "values": [19620, 665921]}] },
    { "name": "Full format", "icon": "rust",
      "labels": ["AOT", "VM"],
      "series": [{"label": "Time", "values": [1305446, 15459312]}] },
    { "name": "Full (cached)", "icon": "rust",
      "labels": ["AOT"],
      "series": [{"label": "Time", "values": [100173]}] }
  ] }
```

The VM does not expose phase-split timing. Full format includes parse + to_doc + render.

## Formatter Invocation

```code-tabs
---rust---
use gorgeous::format_css;

// Format CSS with 80-column width, 2-space indent
let formatted = format_css(source, 80, 2, false);
// Returns formatted string — parse + to_doc + render in one call

// Or use the pipeline directly:
use gorgeous::css::{CssParser, CssDoc};
let ast = CssParser::new().parse(source)?;
let doc = ast.to_doc();
let output = pprint::render(doc, 80);
---typescript---
import { formatCss } from "@mkbabb/bbnf-lang";

// Format CSS with 80-column width, 2-space indent
const formatted = formatCss(source, 80, 2, false);
// Returns formatted string — parse + to_doc + render in one call

// Or use the pipeline directly:
import { cssParser, cssToDoc, render } from "@mkbabb/bbnf-lang";
const ast = cssParser.parse(source);
const doc = cssToDoc(ast);
const output = render(doc, 80);
```

## [pprint Render Engine](/docs/pprint/overview)

pprint uses a Wadler-Lindig algorithm with several throughput optimizations:

- **Stack-based rendering**—no recursion, no stack overflow on deep trees
- **Inline text variants**—`Char`, `DoubleChar`, `SmallBytes` avoid heap allocation for strings ≤24 bytes
- **FxHashMap width cache**—pre-allocated at 256 capacity, avoids rehashing
- **LinearJoin**—forward-scan break decisions with no pre-pass
- **SmartJoin**—text-justification via greedy bin-packing, O(n) uniform

pprint renders at 1,115 MB/s on CSS. The `to_doc` phase (1,026 MB/s) is the throughput-limiting stage.

## `@pretty` Directive Overhead

`@pretty` directives are processed at compile time (AOT) or bytecode-compile time (VM). At runtime, the generated `to_doc()` method is a straightforward tree walk—no directive interpretation happens during formatting.
