# CLAUDE.md — typescript/

`@mkbabb/bbnf-lang` — TypeScript library for runtime BBNF parsing and parser generation.

## Structure

```
typescript/
├── package.json        @mkbabb/bbnf-lang, ESM, on @mkbabb/parse-that ^2
├── VALUE-SEMANTICS.md  The one value semantics every face answers (the conformance corpus is its executable form)
├── tsconfig.json       ES2022, strict
├── vite.config.ts      Library build (ES + CJS), vite-plugin-dts
├── src/
│   ├── index.ts        Public API re-exports
│   ├── types.ts        Expression union type, AST, ProductionRule, ImportDirective, RecoverDirective
│   ├── grammar.ts      BBNFGrammar class — BBNF parser via parse-that combinators
│   ├── parse.ts        BBNFToAST, BBNFToASTWithImports, BBNFToASTFromFiles
│   ├── generate.ts     ASTToParser, BBNFToParser — compile AST to executable parsers (applies @recover)
│   ├── imports.ts      Import types, AST merging, grammarFromModules(files, entry)
│   ├── imports-loader.ts Module-graph DFS; reads ONLY through the host's reader (no node builtin)
│   ├── posix-path.ts   resolve/dirname/extname over module IDs (no node:path, no cwd)
│   ├── optimize.ts     Left-recursion elimination + prefix factoring
│   └── analysis/
│       ├── index.ts    Re-exports + analyzeGrammar facade
│       ├── deps.ts     collectDependencies, buildDepGraphs, traverseAST, dedupGroups
│       ├── scc.ts      SCCResult, tarjanSCC, classifyAcyclicDeps
│       ├── regex.ts    CharSet (128 ASCII bits + a non-ASCII bit), regexFirst (flag-aware, sound), singleClassRun
│       ├── first.ts    analyzeFirst (first/nullable/eofOk per node and rule, fixpoint), routes, findFirstSetConflicts
│       └── metadata.ts computeRefCounts, AnalysisCache, findAliases, findTransparentAlternations
└── test/
    ├── helpers/
    │   └── ast-builders.ts  Shared: rule(), nonterminal(), literal(), alternation(), etc.
    ├── bbnf.test.ts           13 end-to-end grammar tests (JSON, CSS, BBNF self-parse, etc.)
    ├── imports.test.ts        13 import system tests (cyclic, transitive, selective, merge)
    ├── analysis.test.ts       16 analysis tests (SCC, ref counts, dep graphs, FIRST conflicts)
    ├── optimize.test.ts       13 optimization tests (left-recursion, topological sort, prefix)
    ├── first-sets.test.ts     17 FIRST set tests (regexFirst, analyzeFirst, routes)
    ├── first.test.ts          flag/escape/lookaround/non-ASCII/EOF laws + T-4 brute force over value.js's 110 regexes
    ├── modules.test.ts        posix-path, the host-reader loader, grammarFromModules
    ├── value-js-modules.test.ts value.js's five modules via @import = 0.1.4's AST of their concatenation (160 rules)
    ├── conformance.test.ts    VALUE-SEMANTICS.md: conformance/semantics.json (laws) + grammars.json (own grammars)
    ├── conformance/           the corpus + freeze-grammars.ts
    ├── fixtures/              the grammars in the TS front end's dialect (e91428ce1), data.json, value-js/
    ├── recover.test.ts        8 tests — @recover parsing, codegen (.recover() wrapping), error collection
    ├── css-stylesheet.test.ts 11 tests — css-stylesheet.bbnf with @recover + multi-error recovery
    └── utils.ts               Test helpers (math eval, random whitespace injection)
```

## Key Exports

- **`BBNFGrammar`** — Parser class. `grammar()`, `grammarWithImports()`.
- **`BBNFToAST(text)`** — Parse BBNF text → AST. (from `parse.ts`)
- **`BBNFToASTWithImports(text)`** — Parse with import directives. (from `parse.ts`)
- **`ASTToParser(ast, analysis?, first?)`** — Compile AST → `Nonterminals` (rule name → Parser). Applies `@recover` directives by wrapping target parsers with `.recover(syncParser, null)`.
- **`BBNFToParser(text)`** — End-to-end: text → executable parsers.
- **`loadModuleGraphSync(path, reader)`** — DFS-load (the host's reader is required; no filesystem is assumed) a module and its transitive `@import` graph into a `ModuleRegistry`.
- **`mergeModuleAST(registry, path)`** — Merge a module's local + imported rules into a single AST.
- **`analyzeGrammar(ast)`** — Dep graphs, Tarjan SCC, topo order, ref counts, alias detection, acyclicity classification.
- **`analyzeFirst(ast)`** — the one sound analysis: first units (flag-aware), nullable, eofOk; **`routes(infos)`** routes an ordered choice.
- **`grammarFromModules(files, entry)`** — merged rule table from a files map through `@import`.
- **`removeAllLeftRecursion(ast)`** — Left-recursion elimination.
- **`CharSet`** — 128 ASCII bits + one non-ASCII bit.

## Dependency

- **`@mkbabb/parse-that`** (^2.0.0) — Parser combinator library. Provides `Parser<T>`, `string()`, `regex()`, `all()`, `any()`, `dispatch()`, `.trim()`, `.opt()`, `.many()`, `.sepBy()`, `.wrap()`, `.skip()`, `.next()`, etc.

## Codegen Optimizations

- **Routing**: every ordered choice is routed by the unit at the cursor (ASCII table, non-ASCII route, EOF route) to the ordered sub-choice that can start there; sound, so it never changes an answer (VALUE-SEMANTICS.md).
- **Pattern recognition**: regex coalescing, sepBy detection, wrap detection, all-literal alternation.
- **Lazy nonterminal refs**: Enable post-generation parser customization.
- **Alias chain resolution**: Eliminate indirection for `A = B` rules.

## Build

```bash
npm ci && npm test       # Install + test
npm run build            # Vite library build → dist/bbnf.js + dist/bbnf.cjs
```
