# CLAUDE.md — bbnf-lang

Monorepo for the BBNF (Better Backus-Naur Form) grammar ecosystem.
BBNF extends EBNF for defining context-free grammars, used by the
[`parse-that`](https://github.com/mkbabb/parse-that) parser combinator library.

## Structure

```
bbnf-lang/
├── rust/                       Rust workspace (Cargo)
│   ├── bbnf/                   Core grammar parser, analysis, codegen (lib)
│   ├── bbnf-derive/            Proc-macro: #[derive(Parser)] from .bbnf files
│   └── lsp/                    Language server (bbnf-lsp binary)
├── typescript/                 @mkbabb/bbnf-lang — runtime parser + codegen
├── prettier-plugin-bbnf/       Prettier plugin for .bbnf formatting
├── extension/                  VS Code extension (LSP client)
├── grammar/                    Example grammars + language specification
├── server/                     Compiled LSP binary (copied by Makefile)
├── .github/workflows/          CI (ci.yml) + release pipeline (release.yml)
├── .vscode/                    Launch configs, tasks, settings
├── Makefile                    Build automation
└── package.json                NPM workspaces root
```

## Build Commands

```bash
make build          # Release LSP + extension
make dev            # Debug LSP + extension (fast iteration)
make test           # All Rust + TypeScript tests
make bench          # LSP performance benchmarks
make install        # Build, package .vsix, install into VS Code
make package        # Build + create bbnf-lang.vsix
make watch          # Continuous rebuild (cargo watch)
make clean          # Remove artifacts
```

## Manual Builds

```bash
# Rust (requires nightly)
cd rust && cargo test --workspace && cargo build --release -p bbnf-lsp

# TypeScript
cd typescript && npm ci && npm test

# Prettier plugin (build TS first)
npm ci && cd typescript && npm run build && cd ../prettier-plugin-bbnf && npm test

# Extension
cd extension && npm ci && npm run build
```

## Development

- **F5 workflow**: `make dev` then F5 in VS Code
- **Install locally**: `make install` then reload VS Code
- **Integration tests**: `cargo test -p bbnf-lsp --test integration -- --nocapture`
- **LSP binary path resolution** (priority order):
  1. VS Code setting `BBNF.server.path`
  2. Environment variable `BBNF_SERVER_PATH`
  3. Bundled binary `extension/server/bbnf-lsp`

## Release

```bash
make bump-patch     # 1.0.x
make bump-minor     # 1.x.0
make bump-major     # x.0.0
make release        # Push tags → GitHub Actions builds 5 platforms → Marketplace
```

Platforms: linux-x64, linux-arm64, darwin-x64, darwin-arm64, win32-x64.
Requires `VSCE_PAT` secret in GitHub repo settings.

## Conventions

- **Rust**: Nightly toolchain, edition 2024. Clippy with `-D warnings`.
- **TypeScript**: ES2022 target, strict mode, ESM. Vite for bundling, vitest for tests.
- **Extension**: esbuild, CommonJS output (Node.js), `vscode` external.
- **Grammars**: `.bbnf` extension. `@import` for composition. `@recover` for error recovery. `;` terminators.
- **Local crate deps**: `parse_that` and `pprint` are local path dependencies.
- **Lifetimes**: Borrowed `'a` throughout Rust AST; `Box::leak()` for import module graphs.
- **Import system**: Cyclic imports handled via partial-init before recursion. Selective imports expand transitive local deps automatically. `@import` directives can appear at any position in a file.
- **Recovery**: `@recover rule syncExpr ;` — per-rule annotation specifying a sync expression for multi-error parsing. Any valid BBNF expression (regex, alternation, concatenation, etc.) is valid as the sync. Emits `.recover(syncParser, null)` in TS codegen and a `Recovered` enum variant in Rust proc-macro codegen.
- **Analysis pipeline**: Tarjan SCC → topological sort → FIRST sets (128-bit `CharSet`) → dispatch tables (constant-time alternation selection by leading character).
- **Recursive SpanParser codegen**: `try_generate_span_parser()` handles all expression types (concat, alt, many, skip/next, minus, nonterminal refs). Iterative fixed-point loop on `sp_method_rules` — start empty, try generating for all eligible rules, add successes, repeat until convergence (2–3 iterations). Literal unescape via `unescape_literal()` + `proc_macro2::Literal::string()`.
- **File decomposition**: `codegen.rs` → `codegen.rs` + `alternation.rs` + `concatenation.rs`; `prettify.rs` → `prettify/mod.rs` + `prettify/prettify_utils.rs`; `lib.rs` → `lib.rs` + `span_codegen.rs`.
- **Prettify codegen**: `@pretty` directives (`group`, `block`, `indent`, `blankline`, `softbreak`, `nobreak`, `fast`) control Doc emission. `generate_prettify()` produces `to_doc()` + `source_range()` impls. Sub-variant coercion for heterogeneous alternation branches.
