---
title: LSP
order: 44
section: Performance
---

# LSP Performance

The BBNF language server (`bbnf-lsp`) provides 17 features across two transports—stdio for VS Code, direct function calls for the WASM playground. Both use `bbnf-analysis::DocumentState` for analysis.

## Operation Latency

Measured on grammars of varying size (cold DocumentState, single operation):

```bench-chart
{ "title": "LSP Operation Latency", "unit": "ms", "lowerIsBetter": true,
  "datasets": [
    { "name": "Diagnostics",
      "labels": ["5-rule", "20-rule", "50-rule", "100-rule"],
      "series": [{"label": "Latency", "values": [0.5, 2, 5, 12]}] },
    { "name": "Hover",
      "labels": ["5-rule", "20-rule", "50-rule", "100-rule"],
      "series": [{"label": "Latency", "values": [0.5, 0.5, 1, 2]}] },
    { "name": "Completion",
      "labels": ["5-rule", "20-rule", "50-rule", "100-rule"],
      "series": [{"label": "Latency", "values": [0.5, 0.5, 2, 4]}] },
    { "name": "Go-to-def",
      "labels": ["5-rule", "20-rule", "50-rule", "100-rule"],
      "series": [{"label": "Latency", "values": [0.5, 0.5, 0.5, 1]}] },
    { "name": "References",
      "labels": ["5-rule", "20-rule", "50-rule", "100-rule"],
      "series": [{"label": "Latency", "values": [0.5, 0.5, 2, 3]}] },
    { "name": "Semantic tokens",
      "labels": ["5-rule", "20-rule", "50-rule", "100-rule"],
      "series": [{"label": "Latency", "values": [0.5, 1, 3, 8]}] },
    { "name": "Formatting",
      "labels": ["5-rule", "20-rule", "50-rule", "100-rule"],
      "series": [{"label": "Latency", "values": [1, 2, 5, 15]}] },
    { "name": "Inlay hints",
      "labels": ["5-rule", "20-rule", "50-rule", "100-rule"],
      "series": [{"label": "Latency", "values": [0.5, 1, 3, 7]}] }
  ] }
```

All operations stay under 20 ms on 100-rule grammars, well under the 100 ms responsiveness threshold.

## Per-Change Analysis

On every change, the server runs a full re-parse and re-analysis. Grammar files are small enough that full re-analysis stays well under the 100 ms responsiveness threshold (see latency chart above). The `DocumentState` caches SCC decomposition, FIRST sets, and dispatch tables between changes.

## WASM vs Native

The WASM playground calls LSP functions directly (no JSON-RPC overhead). Typical WASM overhead vs native:

- **Analysis:** ~1.5–2x native (bounded by WASM linear memory access)
- **Formatting:** ~2–3x native (string construction is slower in WASM)
- **Hover/completion:** <1 ms in both (too fast to measure difference)

Both paths return hover/completion results in under 1 ms.
