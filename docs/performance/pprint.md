---
title: pprint
order: 43
section: Performance
---

# pprint Render Performance

pprint is the rendering backend for all gorgeous formatters. It takes a `Doc` tree and produces formatted text using the Wadler-Lindig algorithm.

## Render Throughput

Measured on CSS files using the gorgeous pipeline (parse → to_doc → render), isolating the render phase:

```bench-chart
{ "title": "pprint Render Throughput", "unit": "MB/s",
  "datasets": [
    { "name": "bootstrap.css (281 KB)", "icon": "rust",
      "labels": ["render", "to_doc", "end-to-end", "end-to-end (cached)"],
      "series": [{"label": "Throughput", "values": [1115, 1026, 205, 409]}] },
    { "name": "app.css (6 KB)", "icon": "rust",
      "labels": ["end-to-end", "end-to-end (cached)"],
      "series": [{"label": "Throughput", "values": [30, 56]}] },
    { "name": "tailwind.css (3.8 MB)", "icon": "rust",
      "labels": ["end-to-end", "end-to-end (cached)"],
      "series": [{"label": "Throughput", "values": [20, 46]}] }
  ] }
```

The render phase (1,115 MB/s) is not the bottleneck — `to_doc` (1,026 MB/s) limits end-to-end throughput. Both internal stages exceed 1 GB/s; the gap to end-to-end numbers comes from parse overhead and fixed per-call costs.

## Inline Text Variants

pprint avoids heap allocation for short strings using specialized `Text` variants:

| Variant | Size | Allocation |
|---------|------|-----------|
| `Char` | 1 byte | Stack (4 bytes) |
| `DoubleChar` | 2 bytes | Stack (4 bytes) |
| `SmallBytes` | ≤24 bytes | Inline (24 bytes) |
| `String` | >24 bytes | Heap (`String`) |

In CSS formatting, ~85% of text nodes are ≤24 bytes (punctuation, short property names, values), so the `SmallBytes` path dominates and avoids most allocations in the render loop.

## Optimizations

- **Stack-based rendering** — no recursion, no stack overflow on deep Doc trees
- **FxHashMap width cache** — pre-allocated at 256 capacity, avoids rehashing during Group measurement
- **LinearJoin** — forward-scan break decisions with no pre-pass over children
- **SmartJoin** — greedy bin-packing for text justification, O(n) uniform
- **`unsafe` UTF-8 skip** — release builds use `from_utf8_unchecked` (inputs validated at Doc construction)

## Integration

pprint is the render backend for [gorgeous](/docs/gorgeous/overview). See [Formatting Performance](/docs/performance/formatting) for end-to-end comparisons against Biome.

For the Doc algebra and API details, see [pprint Overview](/docs/pprint/overview).
