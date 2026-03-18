---
title: WASM
order: 45
section: Performance
---

# WASM Performance

The `bbnf-wasm` crate compiles the entire toolchain to WebAssembly—5 AOT formatters, a bytecode VM, and 18 LSP features. This page covers module characteristics and browser performance.

## Module Size

| Component | Size (gzipped) |
|-----------|---------------|
| bbnf_wasm.wasm | ~4.7 MB (~1.1 MB gzipped) |
| bbnf_wasm.js | ~15 KB |

Built with `opt-level = 3`. The WASM module is loaded lazily on first use. The playground shows a loading indicator during initialization.

## Initialization Time

```bench-chart
{ "title": "WASM Init Time by Browser", "unit": "ms",
  "labels": ["Chrome (M4 Max)", "Safari (M4 Max)", "Firefox (M4 Max)"],
  "series": [{"label": "Init time", "values": [80, 90, 120]}] }
```

Initialization includes WASM compilation + memory allocation. Subsequent calls are synchronous with no initialization overhead.

## Formatter Throughput (Browser)

All 5 formatters are available in the browser. Throughput on representative inputs:

```bench-chart
{ "title": "WASM Formatter Throughput (Browser)", "unit": "ops/s",
  "datasets": [
    { "name": "CSS", "icon": "wasm",
      "labels": ["normalize.css (6 KB)", "bootstrap.css (274 KB)", "tailwind.css (3.6 MB)"],
      "series": [{"label": "Throughput", "values": [5515, 354, 2]}] },
    { "name": "JSON", "icon": "wasm",
      "labels": ["data.json (35 KB)", "apache (124 KB)", "twitter (617 KB)", "citm_catalog (1.7 MB)", "canada (2.1 MB)"],
      "series": [{"label": "Throughput", "values": [645, 173, 27, 6, 2]}] }
  ] }
```

WASM VM (full tree) is 30–75x slower than native `JSON.parse`; check-only mode is 3–4x faster than full-tree mode. The BBNF TS combinator parser is 2–5x slower than native.

### WASM vs Native Parse Throughput

```bench-chart
{ "title": "WASM JSON Parse vs Native", "unit": "ops/s",
  "datasets": [
    { "name": "data.json (35 KB)", "icon": "wasm",
      "labels": ["JSON.parse", "BBNF TS", "parse-that", "WASM VM (check)", "WASM VM (tree)"],
      "series": [{"label": "Throughput", "values": [21411, 8514, 4441, 837, 299]}] },
    { "name": "twitter (617 KB)", "icon": "wasm",
      "labels": ["JSON.parse", "BBNF TS", "parse-that", "WASM VM (check)", "WASM VM (tree)"],
      "series": [{"label": "Throughput", "values": [1060, 501, 203, 68, 20]}] },
    { "name": "canada (2.1 MB)", "icon": "wasm",
      "labels": ["JSON.parse", "BBNF TS", "parse-that", "WASM VM (check)", "WASM VM (tree)"],
      "series": [{"label": "Throughput", "values": [122, 57, 40, 12, 4]}] }
  ] }
```

## VM Interpreter (Browser)

Custom grammars use the bytecode VM instead of AOT formatters:

```bench-chart
{ "title": "VM Interpreter Timing (Google Sheets WASM)", "unit": "ms",
  "labels": ["Compile grammar", "Parse (simple)", "Parse (pathological)", "Format (simple)", "Format (pathological)"],
  "series": [{"label": "Median", "values": [3.1, 0.49, 0.75, 0.51, 40.2]}] }
```

The compile step runs once per grammar edit. Parse and format run on every input change. LSP features use a thread-local `DocumentState` cache keyed on text hash — one parse per unique text instead of 3–5 per keystroke.

## Live Benchmarks

Try these benchmarks in your browser:

```live-bench
{ "id": "format-json", "label": "JSON Formatting (WASM)", "wasmFn": "format_json", "input": "{\"name\": \"BBNF\", \"version\": 1, \"features\": [\"parsing\", \"formatting\", \"analysis\"], \"nested\": {\"key\": \"value\"}}", "iterations": 1000 }
```

```live-bench
{ "id": "format-css", "label": "CSS Formatting (WASM)", "wasmFn": "format_css", "input": "body { margin: 0; padding: 0; font-family: sans-serif; } .container { max-width: 1200px; margin: 0 auto; padding: 0 1rem; } .header { display: flex; align-items: center; justify-content: space-between; padding: 1rem 0; border-bottom: 1px solid #eee; }", "iterations": 1000 }
```
