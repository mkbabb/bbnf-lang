---
title: Language Server
order: 7
section: BBNF
---

# Language Server

The BBNF language server (`bbnf-lsp`) provides IDE support for `.bbnf` grammar files. It is written in Rust, communicates over stdio using JSON-RPC, and implements 17 LSP features. The same analysis pipeline powers the WASM playground via direct function calls (no JSON-RPC overhead).

## Features

### Navigation

**Go-to-definition** jumps to a rule's definition site. Works across files---Cmd+Click on a rule imported via `@import` navigates to its definition in the source file. Cmd+Click on an import path opens the referenced file.

**Find references** locates all references to a rule within the current document and across imported files.

**Document symbols** provides an outline of all rules in the file, accessible via Cmd+Shift+O.

### Editing

**Rename** (F2) renames a rule and all its references within a single document.

**Completion** suggests rule names, keywords, and imported rule names as you type. Inside `@recover` directives, it suggests the `@recover` keyword and target rule names.

**Code actions** offer two quick fixes:

- Remove unused rules (rules with zero references that are not the entry rule)
- Define undefined rules (stub out a rule body for an undefined reference)

### Display

**Hover** shows the rule's definition expression, FIRST set, nullable status, cycle information (with the full cycle path), and reference count.

**Inlay hints** display FIRST sets for non-trivial rules and nullable markers inline in the editor.

**Code lens** shows reference counts above each rule definition.

**Semantic tokens** provides syntax highlighting for rule definitions, rule references, string literals, regex patterns, and keywords (including `@recover` and `@import` directives).

**Folding** collapses multi-line rules.

**Selection range** expands and shrinks selection at the expression level (Cmd+Shift+Arrow).

### Formatting

**Document formatting** formats the entire file.

**Range formatting** formats a selected region.

**On-type formatting** auto-formats when typing `;`, keeping rules consistently formatted as you write.

## Diagnostics

The language server produces diagnostics at five severity levels:

| Severity | Diagnostic | Description |
|----------|------------|-------------|
| ERROR | Parse errors | Syntax issues, incomplete input |
| ERROR | Duplicate rule definitions | Two rules with the same name |
| WARNING | Undefined rule references | Reference to a rule not defined in the file or imports |
| WARNING | Empty rule body | `rule = ;` with no expression |
| WARNING | Ambiguous alternations | Overlapping FIRST sets between alternation branches |
| INFO | Left recursion | Cycle path reported (e.g., `expr -> term -> factor -> expr`) |
| HINT | Unused rules | Zero references, not the entry rule |
| HINT | Unreachable rules | Not reachable from the entry rule via any path |
| HINT | Alias rules | `A = B ;` suggests using `B` directly |

Undefined-rule warnings are import-aware: if a rule name is provided by an `@import`, the warning is suppressed.

## Recovery Support

`@recover` directives integrate with the language server. The LSP provides:

- Semantic tokens for `@recover` directive syntax
- Hover information on `@recover` target rules
- Completion of the `@recover` keyword and target rule names
- ERROR diagnostic when the `@recover` target rule is undefined

```bbnf
@recover declaration /[;}]/ ;
```

The sync expression (here `/[;}]/`) can be any valid BBNF expression---regex, alternation, concatenation, or any other form.

## Import System

BBNF grammars compose via `@import` directives:

```bbnf
@import "other.bbnf" ;                        (* import all rules *)
@import { number, integer } from "lib.bbnf" ; (* selective import *)
```

Import directives may appear at any position in a file. Selective imports automatically bring transitive dependencies---importing `percentage` also brings `number` and `percentageUnit` if `percentage` references them.

The language server maintains forward and reverse import graphs, updated on every file change. Cross-file features affected:

- **Go-to-definition** resolves to the source file for imported rules
- **Find references** includes references across importing files
- **Completion** includes rule names from imported files
- **Diagnostics** suppress "undefined rule" warnings for imported names and re-publish diagnostics to reverse-dependency files when an imported file changes

Circular imports are handled via partial initialization: a module's rules are registered before recursing into its own imports.

## Architecture

The server is built on `tower-lsp-server` with a Tokio async runtime.

### State

```
BbnfLanguageServer
  ├── documents:    HashMap<Uri, DocumentState>
  ├── import_graph: HashMap<Uri, Vec<Uri>>         (forward)
  ├── importers:    HashMap<Uri, HashSet<Uri>>      (reverse)
  └── global_rules: HashMap<String, Vec<GlobalRule>>
```

`DocumentState` owns the source text and an `OwnedAst` (self-referential via `self_cell`). A `LineIndex` precomputes line starts for O(log n) offset-to-position conversion.

### Analysis Pipeline

On every `textDocument/didChange` notification, the server runs a full re-parse and re-analysis (no incremental parsing---acceptable for grammar file sizes):

1. Parse via `BBNFGrammar::grammar_with_imports()` (panic-caught)
2. Extract rules, references, semantic tokens
3. Detect duplicates, undefined references, unused rules
4. Tarjan SCC for cycle detection and cycle path strings
5. FIRST set computation and ambiguous alternation detection
6. Alias detection, reachability (BFS from entry)
7. Filter diagnostics against the import graph (suppress imported-but-undefined)
8. Re-publish diagnostics to reverse-dependency files

### File Layout

```
lsp/src/
├── main.rs              Tokio entry point, stdio server
├── server/
│   ├── mod.rs           BbnfLanguageServer struct, on_change
│   ├── imports.rs       Import graph updates, diagnostic filtering
│   └── protocol.rs      LanguageServer trait implementation
├── state/
│   ├── mod.rs           DocumentState struct
│   ├── types.rs         RuleInfo, ReferenceInfo, SemanticTokenInfo
│   ├── parsing.rs       OwnedAst, CachedParseResult, parse_once
│   ├── diagnostics.rs   Full diagnostic generation
│   ├── pretty.rs        @pretty extraction and validation
│   └── ast_utils.rs     Reference collection, semantic tokens
├── analysis.rs          LineIndex, symbol lookup
└── features/            One module per LSP feature (17 total)
```

## Binary Path Resolution

The VS Code extension locates the `bbnf-lsp` binary in this priority order:

1. VS Code setting `BBNF.server.path`
2. Environment variable `BBNF_SERVER_PATH`
3. Bundled binary at `<extensionPath>/server/bbnf-lsp`
4. Dev fallback at `<extensionPath>/../server/bbnf-lsp`
