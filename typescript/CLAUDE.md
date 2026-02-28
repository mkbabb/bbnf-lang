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
│   ├── parse.ts        BBNFToAST, BBNFToASTWithImports, BBNFToASTFromFiles
│   ├── generate.ts     ASTToParser, BBNFToParser — compile AST to executable parsers
│   ├── imports.ts      Module graph loading, cycle-tolerant DFS, selective dep expansion
│   ├── optimize.ts     Left-recursion elimination + prefix factoring
│   └── analysis/
│       ├── index.ts    Re-exports + analyzeGrammar facade
│       ├── deps.ts     collectDependencies, buildDepGraphs, traverseAST, dedupGroups
│       ├── scc.ts      SCCResult, tarjanSCC, classifyAcyclicDeps
│       ├── charset.ts  CharSet (128-bit ASCII bitset)
│       ├── first-sets.ts  FirstNullable, computeFirstSets
│       ├── regex-first.ts regexFirstChars + helpers
│       ├── dispatch.ts DispatchTable, buildDispatchTable, findFirstSetConflicts
│       └── metadata.ts computeRefCounts, AnalysisCache, findAliases, findTransparentAlternations
└── test/
    ├── helpers/
    │   └── ast-builders.ts  Shared: rule(), nonterminal(), literal(), alternation(), etc.
    ├── bbnf.test.ts       13 end-to-end grammar tests (JSON, CSS, BBNF self-parse, etc.)
    ├── imports.test.ts    13 import system tests (cyclic, transitive, selective, merge)
    ├── analysis.test.ts   16 analysis tests (SCC, ref counts, dep graphs, FIRST conflicts)
    ├── optimize.test.ts   13 optimization tests (left-recursion, topological sort, prefix)
    ├── first-sets.test.ts 17 FIRST set tests (regex dispatch, CharSet, dispatch tables)
    └── utils.ts           Test helpers (math eval, random whitespace injection)
```

## Key Exports

- **`BBNFGrammar`** — Parser class. `grammar()`, `grammarWithImports()`.
- **`BBNFToAST(text)`** — Parse BBNF text → AST. (from `parse.ts`)
- **`BBNFToASTWithImports(text)`** — Parse with import directives. (from `parse.ts`)
- **`ASTToParser(ast, analysis?, firstNullable?)`** — Compile AST → `Nonterminals` (rule name → Parser).
- **`BBNFToParser(text)`** — End-to-end: text → executable parsers.
- **`loadModuleGraphSync(path, reader?)`** — DFS-load a module and its transitive `@import` graph into a `ModuleRegistry`.
- **`mergeModuleAST(registry, path)`** — Merge a module's local + imported rules into a single AST.
- **`analyzeGrammar(ast)`** — Dep graphs, Tarjan SCC, topo order, ref counts, alias detection, acyclicity classification.
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
