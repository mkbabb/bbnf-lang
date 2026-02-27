# CLAUDE.md — prettier-plugin-bbnf/

Prettier plugin for formatting `.bbnf` grammar files.

## Structure

```
prettier-plugin-bbnf/
├── package.json        v0.1.0, ESM, peer: prettier ^3.0.0
├── tsconfig.json       ES2022, strict
├── vite.config.ts      Library build (ES only), externals: prettier, @mkbabb/bbnf-lang
├── src/
│   ├── index.ts        Plugin registration, language/parser/printer config
│   ├── parser.ts       BBNF text → AST via BBNFToAST, optional topological sort
│   └── printer.ts      AST → Prettier Doc IR, expression-level formatting
└── test/
    └── format.test.ts  6 tests: json, csv, math, bbnf self-format, idempotency, error
```

## Formatting Rules

- **Print width**: 66 (hardcoded).
- **Tab width**: 4 spaces.
- **Literals**: Double-quoted (single-quoted if value contains `"`).
- **Epsilon**: `ε` (Unicode).
- **Operators**: Spaced (`a | b`, `a , b`, `a << b`, `a >> b`, `a - b`).
- **Quantifiers**: No space (`expr?`, `expr*`, `expr+`, `expr?w`).
- **Groups**: Spaced parens (`( expr )`), indented on wrap.
- **Concatenation/Alternation**: Soft-break at delimiters, indented when wrapped.
- **Rules**: `name = expr ;` with hardline between rules.
- **Comments**: Preserved above/below/inline.
- **Complex rules**: Extra blank line after concatenation/alternation rules.

## Exports

- **`BBNFPlugin`** — Prettier plugin object.
- **`formatBBNF(text, options?)`** — Convenience function.
- **`printBBNFAST(ast)`** — Direct AST → Doc conversion.
- **`printExpressionToString(expr)`** — Single expression → formatted string.

## Build

```bash
# Must build @mkbabb/bbnf-lang first (npm workspace dependency)
cd ../typescript && npm run build
npm test        # vitest
npm run build   # vite → dist/prettier-plugin-bbnf.js
```
