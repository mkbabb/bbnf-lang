# CLAUDE.md — rust/lsp/

BBNF language server. Binary: `bbnf-lsp`. Communicates over stdio (JSON-RPC).

## Structure

```
lsp/
├── Cargo.toml
├── src/
│   ├── main.rs                 Tokio entry point, stdio server
│   ├── server/
│   │   ├── mod.rs              BbnfLanguageServer struct, constructor, on_change
│   │   ├── imports.rs          Import graph updates, diagnostic filtering, incremental edits
│   │   └── protocol.rs         impl LanguageServer — all request/notification handlers
│   ├── state/
│   │   ├── mod.rs              DocumentState struct, new/update/ast methods
│   │   ├── types.rs            RuleInfo, ReferenceInfo, SemanticTokenInfo, DocumentInfo, token_types
│   │   ├── parsing.rs          OwnedAst (self_cell), CachedParseResult, parse_once
│   │   ├── diagnostics.rs      analyze_from_cache — full diagnostic generation
│   │   └── ast_utils.rs        collect_references, semantic tokens, format_expression, cycle paths
│   ├── analysis.rs             LineIndex, symbol lookup utilities
│   └── features/
│       ├── mod.rs              Module declarations
│       ├── hover.rs            Rule definition + FIRST/nullable/cycle info
│       ├── goto_definition.rs  Local + cross-file + import path navigation
│       ├── references.rs       Local + cross-file reference finding
│       ├── rename.rs           Rule + reference rename (single document)
│       ├── completion.rs       Rule names, keywords, imported rules
│       ├── document_symbols.rs Outline of all rules
│       ├── code_lens.rs        Reference counts per rule
│       ├── folding.rs          Multi-line rule folding
│       ├── code_actions.rs     Remove unused / define undefined rules
│       ├── formatting.rs       Document, range, on-type (trigger: `;`)
│       ├── semantic_tokens.rs  ruleDefinition, ruleReference, string, regexp, keyword
│       ├── inlay_hints.rs      FIRST sets (non-trivial rules), nullable markers
│       └── selection_range.rs  Expression-level expand/shrink selection
└── tests/
    ├── integration.rs          45+ JSON-RPC integration tests
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

- **DocumentState**: Owns text + `OwnedAst` (self-referential via `self_cell`).
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
