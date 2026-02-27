# CLAUDE.md — typescript/

`@mkbabb/bbnf-lang` — TypeScript library for runtime BBNF parsing and parser generation.

## Structure

```
typescript/
├── package.json        @mkbabb/bbnf-lang v0.1.0, ESM
├── tsconfig.json       ES2022, strict
├── vite.config.ts      Library build (ES + CJS), vite-plugin-dts
├── src/
│   ├── index.ts        Public API re-exports
│   ├── types.ts        Expression union type, AST, ProductionRule, ImportDirective
│   ├── grammar.ts      BBNFGrammar class — BBNF parser via parse-that combinators
│   ├── generate.ts     ASTToParser — compile AST to executable parsers
│   ├── optimize.ts     Left-recursion elimination + prefix factoring
│   ├── analysis.ts     Dependency graphs, Tarjan SCC, ref counts
│   └── first-sets.ts   CharSet (128-bit), FIRST/nullable computation, dispatch tables
└── test/
    ├── bbnf.test.ts    13 test cases (JSON, CSV, CSS, BBNF self-parse, etc.)
    └── utils.ts        Test helpers (math eval, random whitespace injection)
```

## Key Exports

- **`BBNFGrammar`** — Parser class. `grammar()`, `grammarWithImports()`.
- **`BBNFToAST(text)`** — Parse BBNF text → AST.
- **`BBNFToASTWithImports(text)`** — Parse with import directives.
- **`ASTToParser(ast, analysis?, firstNullable?)`** — Compile AST → `Nonterminals` (rule name → Parser).
- **`BBNFToParser(text)`** — End-to-end: text → executable parsers.
- **`analyzeGrammar(ast)`** — Full analysis: dep graphs, SCCs, topo order, ref counts.
- **`computeFirstSets(ast, analysis)`** — FIRST sets + nullable flags.
- **`removeAllLeftRecursion(ast)`** — Left-recursion elimination.
- **`CharSet`** — 128-bit ASCII bitset.
- **`buildDispatchTable(alts, firstSets, nullable)`** — O(1) dispatch for alternations.

## Dependency

- **`@mkbabb/parse-that`** (^0.6.0) — Parser combinator library. Provides `Parser<T>`, `string()`, `regex()`, `all()`, `any()`, `dispatch()`, `.trim()`, `.opt()`, `.many()`, `.sepBy()`, `.wrap()`, `.skip()`, `.next()`, etc.

## Codegen Optimizations

- **Dispatch tables**: Disjoint FIRST sets → O(1) character dispatch via `dispatch()`.
- **Pattern recognition**: regex coalescing, sepBy detection, wrap detection, all-literal alternation.
- **Lazy nonterminal refs**: Enable post-generation parser customization.
- **Alias chain resolution**: Eliminate indirection for `A = B` rules.

## Build

```bash
npm ci && npm test       # Install + test
npm run build            # Vite library build → dist/bbnf.js + dist/bbnf.cjs
```
