# <img src="extension/icons/bbnf-small.png" height="32" align="top" /> bbnf-lang

**Better Backus-Naur Form**—a monorepo for the BBNF grammar ecosystem.

BBNF extends EBNF for defining context-free grammars, used by the
[`parse-that`](https://github.com/mkbabb/parse-that) parser combinator library.

---

## Structure

```
rust/                   Rust workspace
  bbnf/                 BBNF grammar framework (lib)
  bbnf-derive/          Proc-macro derive for BBNF
  lsp/                  Language Server Protocol server
typescript/             TS library (@mkbabb/bbnf-lang)
prettier-plugin-bbnf/   Prettier plugin for .bbnf files
extension/              VS Code extension (LSP client)
grammar/                Example grammars + language specification
server/                 Compiled LSP binary (copied by Makefile)
```

## Language

BBNF extends [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form)
with features for practical parser generation: regex terminals, skip/next operators,
mapping functions, and an import system. See
[grammar/BBNF.md](grammar/BBNF.md) for the full specification.

Quick orientation:

```bbnf
(* Rules: name = expression ; *)
value = object | array | string | number | "true" | "false" | "null" ;

(* Regex terminals *)
number = /\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?/ ;

(* EBNF operators: [] optional, {} repetition, () grouping *)
array = "[", [ value, { ",", value } ], "]" ;

(* Imports *)
@import { number, integer } from "css-value-unit.bbnf" ;
```

## VS Code Extension

Full `.bbnf` language support via an LSP server written in Rust.

### Diagnostics

The LSP produces the following diagnostics:

| Severity | Diagnostic |
|----------|-----------|
| ERROR | Parse errors (syntax issues, incomplete input) |
| ERROR | Duplicate rule definitions |
| WARNING | Undefined rule references (import-aware) |
| WARNING | Empty rule body (`rule = ;`) |
| WARNING | Ambiguous alternations (overlapping FIRST sets between branches) |
| INFO | Left recursion with cycle path (e.g., `expr → term → factor → expr`) |
| HINT | Unused rules (zero references, not the entry rule) |
| HINT | Unreachable rules (referenced but not reachable from entry) |
| HINT | Alias rules (`A = B` — suggests using `B` directly) |

### Navigation

- **Go-to-definition** — jump to a rule's definition (Cmd+Click), including import paths
- **Find references** — all references to a rule across the document
- **Document symbols** — outline of all rules (Cmd+Shift+O)

### Editing

- **Rename** — rename a rule and all its references (F2)
- **Completion** — rule names and keywords
- **Code actions** — remove unused rules, define undefined rules

### Display

- **Hover** — rule definition, FIRST set, nullable, cycle info, reference count
- **Inlay hints** — FIRST sets for non-trivial rules, nullable markers
- **Code lens** — reference counts above each rule
- **Semantic tokens** — rule definitions, references, strings, regex, keywords
- **Folding** — collapse multi-line rules
- **Selection range** — expand/shrink selection (Cmd+Shift+Arrow)

### Formatting

- **Document formatting** — format the entire file
- **Range formatting** — format a selection
- **On-type formatting** — auto-format on `;`

### Imports

BBNF supports `@import` directives for composing grammars:

```bbnf
@import "other.bbnf" ;                        (* import all rules *)
@import { number, integer } from "lib.bbnf" ; (* selective import *)
```

Cmd+Click on import paths opens the referenced file. Diagnostics are
import-aware—imported rule names suppress "undefined rule" warnings.

## Build & Test

A `Makefile` automates the most common workflows:

```bash
make build          # Build release LSP binary + extension
make dev            # Quick debug build + extension (fast iteration)
make test           # Run all Rust and TypeScript tests
make bench          # Run LSP performance benchmarks
make install        # Build, package .vsix, and install into VS Code
make package        # Build and create bbnf-lang.vsix (no install)
make watch          # Continuous rebuild on save (cargo watch)
make clean-vsix     # Remove old .vsix files
```

### Manual builds

```bash
# Rust (requires nightly)
cd rust && cargo test --workspace && cargo build --release -p bbnf-lsp

# TypeScript library
cd typescript && npm ci && npm test

# Prettier plugin (must build TS library first)
npm ci && cd typescript && npm run build && cd ../prettier-plugin-bbnf && npm test

# Extension
cd extension && npm ci && npm run build
```

## Development

### Quick start

```bash
make build-lsp-debug   # Fast debug build of the LSP binary
make build-ext         # Bundle the extension
```

Then open this repo in VS Code and press **F5** to launch the Extension Development
Host with the BBNF extension loaded.

### Testing locally (without F5)

To install the extension into your regular VS Code instance:

```bash
make install    # Builds everything, packages a .vsix, installs it
```

Reload VS Code after installation. The extension will use the LSP binary
bundled in `server/bbnf-lsp`.

### VS Code launch configurations

Two configs are provided in `.vscode/launch.json`:

| Config | What it does |
|--------|-------------|
| **Launch Extension** | Builds the extension, uses the release binary in `server/` |
| **Launch Extension (Debug LSP)** | Builds both LSP (debug) and extension, uses `rust/target/debug/bbnf-lsp` |

The extension reads the server path from (in priority order):

1. VS Code setting `BBNF.server.path`
2. Environment variable `BBNF_SERVER_PATH` (set by launch configs)
3. Bundled binary at `<extensionPath>/server/bbnf-lsp`
4. Dev fallback at `<extensionPath>/../server/bbnf-lsp`

### Developing the LSP

The Rust LSP server at `rust/lsp/` communicates over stdin/stdout using the
[LSP protocol](https://microsoft.github.io/language-server-protocol/). The
development loop:

```bash
# Edit rust/lsp/src/**/*.rs

# Run unit + integration tests (no VS Code needed)
cd rust && cargo test --workspace

# Rebuild and test in VS Code
cargo build -p bbnf-lsp
# Then F5 in VS Code to relaunch the extension host
```

**Integration tests** (`rust/lsp/tests/integration.rs`) spawn the compiled
`bbnf-lsp` binary as a subprocess, send raw JSON-RPC messages, and assert on
responses—full end-to-end coverage without VS Code:

```bash
cargo test -p bbnf-lsp --test integration -- --nocapture
```

Current test coverage (45 integration tests):

- Initialize & capability negotiation
- Diagnostics: valid grammar, unused rules, undefined rules, parse errors, regex panics
- Diagnostics: FIRST set conflicts, cycle paths, alias hints, unreachable rules
- Hover (basic + enhanced with FIRST/nullable info), go-to-definition, references, rename, completion
- Document symbols, code lens, folding, code actions
- Full document formatting, range formatting, on-type formatting
- Semantic tokens
- Inlay hints (FIRST sets, nullability, range filtering)
- Selection range (single & multiple positions)
- Incremental text sync (insert, delete, replace)
- Cross-file: go-to-definition, references, completion via `@import`
- Large grammar (8-rule JSON grammar, all features combined)

### Developing the Prettier Plugin

```bash
# From repo root (npm workspaces resolve @mkbabb/bbnf-lang locally)
npm ci
cd typescript && npm run build   # must build bbnf-lang first
cd ../prettier-plugin-bbnf && npm test
```

### VS Code tasks

Available via **Terminal > Run Task**:

| Task | Description |
|------|-------------|
| Build Extension | `npm run build` in extension/ |
| Build LSP (Release) | `cargo build --release -p bbnf-lsp` |
| Build LSP (Debug) | `cargo build -p bbnf-lsp` |
| Build All (Debug) | LSP + extension sequentially |
| Test LSP | `cargo test --workspace` |
| Test All | Runs Test LSP (expandable) |

## Publishing

Releases are automated via GitHub Actions. The pipeline builds platform-specific
LSP binaries (linux-x64, linux-arm64, darwin-x64, darwin-arm64, win32-x64),
packages platform-specific `.vsix` files, and publishes to the VS Code Marketplace.

```bash
# 1. Bump the version (choose one)
make bump-patch     # 1.0.0 → 1.0.1
make bump-minor     # 1.0.0 → 1.1.0
make bump-major     # 1.0.0 → 2.0.0

# 2. Push the tag to trigger the release pipeline
make release        # git push --follow-tags
```

**Prerequisites:** The `VSCE_PAT` secret must be configured in the GitHub repo
settings (Settings > Secrets > Actions). Generate a Personal Access Token at
https://dev.azure.com with the "Marketplace (Manage)" scope.

## Sources, acknowledgements, &c.

- [Extended Backus-Naur form](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form) — ISO 14977. BBNF's ancestor.
- Wheeler, D. A. [Don't Use ISO 14977 EBNF](https://dwheeler.com/essays/dont-use-iso-14977-bbnf.html). — Motivation for BBNF's syntactic deviations.
- Aho, A. V., Lam, M. S., Sethi, R., & Ullman, J. D. (2006). *Compilers: Principles, Techniques, and Tools* (2nd ed.). Addison-Wesley. — Left recursion, left factoring, FIRST/FOLLOW sets.
- Tarjan, R. E. (1972). Depth-first search and linear graph algorithms. *SIAM Journal on Computing*. — SCC detection used for cycle analysis, FIRST-set propagation, and build ordering.
- [Language Server Protocol](https://microsoft.github.io/language-server-protocol/). Microsoft. — The protocol implemented by `bbnf-lsp`.
- [`parse-that`](https://github.com/mkbabb/parse-that) — The parser combinator library that consumes BBNF grammars.

## License

MIT
