# CLAUDE.md — crates/lsp/

BBNF language server. Binary: `bbnf-lsp`. Communicates over stdio (JSON-RPC).

Thin wrapper around `bbnf-analysis`—the LSP crate handles protocol transport,
import graph management, and diagnostic re-publication. All analysis logic,
feature providers, and state management live in `bbnf-analysis`.

## Structure

```
lsp/
├── Cargo.toml
├── src/
│   ├── main.rs                 Tokio entry point, stdio server, --dap flag
│   ├── lib.rs                  Re-exports for tests
│   ├── server/
│   │   ├── mod.rs              BbnfLanguageServer struct, constructor, on_change
│   │   ├── imports.rs          Import graph updates, diagnostic filtering, incremental edits
│   │   └── protocol.rs         impl LanguageServer — all request/notification handlers
│   └── dap/
│       ├── mod.rs              serve_dap(), Content-Length transport, command dispatch
│       ├── adapter.rs          DapAdapter: compile grammar, run interpreter, build frames/variables
│       ├── protocol.rs         DAP message serde types (no library dependency)
│       └── mapping.rs          LineIndex, resolve_breakpoint(), rule_at_offset()
└── tests/
    ├── integration.rs          45+ JSON-RPC integration tests
    ├── analyze.rs              Analysis-layer integration tests
    ├── dap.rs                  DAP server integration tests
    └── bench_lsp.rs            Performance benchmarks
```

## Architecture

```
stdio → tower-lsp-server → BbnfLanguageServer
  ├── documents: Arc<RwLock<HashMap<Uri, DocumentState>>>
  ├── import_graph: Arc<RwLock<HashMap<Uri, Vec<Uri>>>>
  ├── importers: Arc<RwLock<HashMap<Uri, HashSet<Uri>>>>      (reverse)
  └── global_rules: Arc<RwLock<HashMap<String, Vec<GlobalRule>>>>
```

### State Management

- **DocumentState**: Defined in `bbnf-analysis`. Owns text + `OwnedAst` (self-referential via `self_cell`).
- **DocumentInfo**: Rules, diagnostics, semantic tokens, FIRST labels, nullable set, cycle paths, imports.
- **LineIndex**: Pre-computed line starts for O(log n) offset↔position conversion.
- On every `didChange`: full re-parse + re-analysis (no incremental). Acceptable for grammar file sizes.

### Analysis Pipeline (per document change)

1. Parse via `BBNFGrammar::grammar_with_imports()` (panic-caught)
2. Extract rules, references, semantic tokens
3. Detect duplicates, undefined refs, unused rules
4. Tarjan SCC → cycle detection + cycle path strings
5. FIRST set computation → ambiguous alternation detection
6. Alias detection, reachability (BFS from entry)
7. Filter diagnostics against import graph (suppress imported-but-undefined)
8. Re-publish diagnostics to reverse-dep files

### Cross-File Features

- `resolve_import_uri()`: Relative path → absolute URI, auto-appends `.bbnf`.
- Forward/reverse import graphs maintained on every change.
- Goto definition, find references, completion all query cross-file state.
- "Undefined rule" diagnostics suppressed for imported rules.

## Testing

```bash
cargo test -p bbnf-lsp --test integration -- --nocapture
```

Tests spawn `bbnf-lsp` as subprocess, send raw JSON-RPC, assert responses.
Coverage: all LSP features, cross-file imports, incremental edits, error recovery.
