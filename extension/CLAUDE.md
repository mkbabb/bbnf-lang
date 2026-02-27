# CLAUDE.md — extension/

VS Code extension: LSP client for `.bbnf` files. Thin wrapper around `bbnf-lsp`.

## Structure

```
extension/
├── package.json                    Extension manifest (v1.0.3, publisher: mkbabb)
├── esbuild.mjs                     Build config (CJS bundle, vscode external)
├── tsconfig.json                   ES2022, strict, bundler resolution
├── language-configuration.json     Comments (// and /* */), brackets, auto-close
├── .vscodeignore                   Excludes src/, node_modules/ from .vsix
├── src/
│   └── extension.ts                Activate/deactivate, server path resolution, restart command
├── syntaxes/
│   ├── bbnf.tmLanguage.json        TextMate grammar (syntax highlighting)
│   └── inlineLanguageString.tmLanguage.json  Inline CSS/JS/BBNF injection
├── server/
│   └── bbnf-lsp                    Compiled LSP binary (copied by Makefile)
├── icons/                          Extension icons (PNG)
└── dist/
    └── extension.js                Bundled output (361KB)
```

## Server Path Resolution (priority order)

1. VS Code setting: `BBNF.server.path`
2. Environment variable: `BBNF_SERVER_PATH`
3. Bundled binary: `<extensionPath>/server/bbnf-lsp`
4. Dev fallback: `<extensionPath>/../server/bbnf-lsp`

## Contributes

- **Language**: `bbnf` → `.bbnf` files
- **Command**: `bbnf.restartServer`
- **Settings**: `BBNF.server.path`, `BBNF.trace.server` (off/messages/verbose)
- **Semantic tokens**: `ruleDefinition` (superType: function), `ruleReference` (superType: variable)
- **TextMate grammars**: Main BBNF syntax + inline language injection (CSS, JS, BBNF in strings via `/* css */` annotations)

## Build

```bash
npm ci
npm run build    # esbuild → dist/extension.js
npm run watch    # Watch mode (no minification)
npm run lint     # tsc --noEmit
npm run package  # vsce package → .vsix
```
