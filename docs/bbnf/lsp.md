---
title: Language Server
order: 7
section: BBNF
---

# Language Server

The BBNF language server (`bbnf-lsp`) provides IDE support for `.bbnf` grammar files. It implements 17 LSP features, written in Rust on `tower-lsp-server` with a Tokio async runtime. The same analysis pipeline powers the WASM playground via direct function calls.

## Two Transports

The language server runs in two environments with different transport mechanisms.

```code-tabs
---vscode---
# VS Code — stdio transport

VS Code extension spawns `bbnf-lsp` as a subprocess.
Communication uses JSON-RPC over stdin/stdout.

    VS Code ←→ stdio (JSON-RPC) ←→ bbnf-lsp process

Full LSP protocol: initialize, capabilities, notifications, requests.
Cross-file features use the filesystem for import resolution.
---wasm---
# Playground — direct WASM calls

The playground loads `bbnf-wasm` and calls analysis functions directly.
No JSON-RPC overhead — function calls return results immediately.

    Monaco Editor → bbnf-wasm → { analyze, hover, completions, ... }

Same analysis pipeline as the native LSP, compiled to WebAssembly.
10 Monaco providers: hover, completion, semantic tokens, inlay hints,
definition, document symbols, folding, selection ranges, code actions, code lens.
```

## Diagnostics

The language server produces diagnostics at five severity levels. Undefined-rule warnings are import-aware—imported rule names suppress the warning.

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
| HINT | Alias rules | `A = B ;`—suggests using `B` directly |

## Navigation

**Go-to-definition** jumps to a rule's definition site. Works across files—Cmd+Click on a rule imported via `@import` navigates to its definition in the source file. Cmd+Click on an import path opens the referenced file.

**Find references** locates all references to a rule within the current document and across imported files.

**Document symbols** provides an outline of all rules in the file, accessible via Cmd+Shift+O.

## Editing

**Rename** (F2) renames a rule and all its references within a single document.

**Completion** suggests rule names, keywords, and imported rule names as you type. Inside `@recover` directives, it suggests the `@recover` keyword and target rule names. Inside `@pretty` directives, it suggests hint vocabulary.

**Code actions** offer two quick fixes: remove unused rules (rules with zero references that are not the entry rule) and define undefined rules (stub out a rule body for an undefined reference).

## Display

**Hover** shows the rule's definition expression, FIRST set, nullable status, cycle information (with the full cycle path), and reference count.

**Inlay hints** display FIRST sets for non-trivial rules and nullable markers inline in the editor.

**Code lens** shows reference counts above each rule definition.

**Semantic tokens** provides syntax highlighting for rule definitions, rule references, string literals, regex patterns, and keywords (including `@recover`, `@import`, and `@pretty` directives).

**Folding** collapses multi-line rules.

**Selection range** expands and shrinks selection at the expression level (Cmd+Shift+Arrow).

## Formatting

**Document formatting** formats the entire `.bbnf` file.

**Range formatting** formats a selected region.

**On-type formatting** auto-formats when typing `;`, keeping rules consistently formatted as you write.

## Recovery & Imports

### `@recover` integration

`@recover` directives are fully supported by the language server. The LSP provides semantic tokens for directive syntax, hover information on target rules, completion of the `@recover` keyword and target rule names, and an ERROR diagnostic when the target rule is undefined.

```bbnf
@recover declaration /[;}]/ ;
```

The sync expression can be any valid BBNF expression—regex, alternation, concatenation, or any other form.

### Cross-file imports

BBNF grammars compose via `@import` directives. Import directives may appear at any position in a file.

```bbnf
@import "other.bbnf" ;                        (* import all rules *)
@import { number, integer } from "lib.bbnf" ; (* selective import *)
```

Selective imports automatically bring transitive dependencies—importing `percentage` also brings `number` and `percentageUnit` if `percentage` references them. Circular imports are handled via partial initialization: a module's rules are registered before recursing into its own imports.

The server maintains forward and reverse import graphs, updated on every file change. Cross-file features affected:

- **Go-to-definition** resolves to the source file for imported rules
- **Find references** includes references across importing files
- **Completion** includes rule names from imported files
- **Diagnostics** suppress "undefined rule" warnings for imported names and re-publish diagnostics to reverse-dependency files when an imported file changes

## Analysis Pipeline

On every `textDocument/didChange` notification, the server runs a full re-parse and re-analysis. No incremental parsing—grammar files are small enough that full re-analysis is acceptable.

```flow-chart
{ "title": "Per-Change Analysis Pipeline",
  "nodes": [
    {"label": "Parse", "detail": "BBNFGrammar::grammar_with_imports() (panic-caught)", "color": "cyan"},
    {"label": "Extract", "detail": "Rules, references, semantic tokens", "color": "blue"},
    {"label": "Detect duplicates", "detail": "Undefined refs, unused rules", "color": "blue"},
    {"label": "Tarjan SCC", "detail": "Cycle detection + cycle path strings", "color": "green"},
    {"label": "FIRST sets", "detail": "Ambiguous alternation detection", "color": "green"},
    {"label": "Alias + reachability", "detail": "BFS from entry rule", "color": "purple"},
    {"label": "Filter diagnostics", "detail": "Suppress imported-but-undefined", "color": "amber"},
    {"label": "Re-publish", "detail": "Diagnostics to reverse-dependency files", "color": "amber"}
  ] }
```

## Binary Resolution

The VS Code extension locates the `bbnf-lsp` binary in this priority order:

1. VS Code setting `BBNF.server.path`
2. Environment variable `BBNF_SERVER_PATH`
3. Bundled binary at `<extensionPath>/server/bbnf-lsp`
4. Dev fallback at `<extensionPath>/../server/bbnf-lsp`
