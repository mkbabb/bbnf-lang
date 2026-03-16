---
title: LSP
order: 44
section: Performance
---

# LSP Performance

The BBNF language server (`bbnf-lsp`) provides 17 features across two transports—stdio for VS Code, direct function calls for the WASM playground. Both use `bbnf-analysis::DocumentState` for incremental analysis.

## Operation Latency

Measured on grammars of varying size (cold DocumentState, single operation):

| Operation | 5-rule | 20-rule | 50-rule | 100-rule |
|-----------|--------|---------|---------|----------|
| **Diagnostics** | <1 ms | ~2 ms | ~5 ms | ~12 ms |
| **Hover** | <1 ms | <1 ms | ~1 ms | ~2 ms |
| **Completion** | <1 ms | <1 ms | ~2 ms | ~4 ms |
| **Go-to-def** | <1 ms | <1 ms | <1 ms | ~1 ms |
| **References** | <1 ms | <1 ms | ~2 ms | ~3 ms |
| **Semantic tokens** | <1 ms | ~1 ms | ~3 ms | ~8 ms |
| **Formatting** | ~1 ms | ~2 ms | ~5 ms | ~15 ms |
| **Inlay hints** | <1 ms | ~1 ms | ~3 ms | ~7 ms |

All operations stay under 20 ms on 100-rule grammars, well under the 100 ms responsiveness threshold.

## Incremental Editing

After the initial analysis, incremental edits (insert, delete, replace) trigger partial re-analysis:

- **Single-rule edit** — only the modified rule and its dependents are re-analyzed
- **New rule** — added to the dependency graph; dependents recomputed
- **Deleted rule** — removed from graph; references flagged as undefined

The LSP maintains a `DocumentState` that caches SCC decomposition, FIRST sets, and dispatch tables. Incremental edits invalidate only the affected subgraph.

## WASM vs Native

The WASM playground calls LSP functions directly (no JSON-RPC overhead). Typical WASM overhead vs native:

- **Analysis:** ~1.5–2x native (bounded by WASM linear memory access)
- **Formatting:** ~2–3x native (string construction is slower in WASM)
- **Hover/completion:** <1 ms in both (too fast to measure difference)

Both paths return hover/completion results in under 1 ms.
