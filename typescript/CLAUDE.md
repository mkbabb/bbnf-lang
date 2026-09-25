# CLAUDE.md — typescript/

`@mkbabb/bbnf-lang` — the TypeScript BBNF front end and THE staged emitter (grammar → ES module + `.d.ts`); value.js's output of record. README.md is the user contract (bbnf gen, Actions, nesting depth).

## Structure

```
typescript/
├── package.json        @mkbabb/bbnf-lang, ESM, on @mkbabb/parse-that ^2
├── README.md           The user contract: one emitter / three faces, bbnf gen + --check, Actions (pure, total, non-reentrant), nesting depth
├── VALUE-SEMANTICS.md  The one value semantics every face answers (the conformance corpus is its executable form)
├── tsconfig.json       ES2022, strict
├── vite.config.ts      Library build (ES + CJS): index, ./gen (node), bin cli; vite-plugin-dts
├── src/
│   ├── index.ts        Public API re-exports
│   ├── types.ts        Expression union type, AST, ProductionRule, ImportDirective, RecoverDirective
│   ├── grammar.ts      BBNFGrammar class — BBNF parser via parse-that combinators
│   ├── parse.ts        BBNFToAST, BBNFToASTWithImports, BBNFToASTFromFiles
│   ├── emit.ts         THE emitter: value + recognize modes, routed switch, class-run loops, leaf guards, direct calls, hoisted alts, interned constants, back-edge depth fault; module + .d.ts
│   ├── compile.ts      runtime compile(): evaluates the emitted body once (+ the evidence-only routing audit)
│   ├── facade.ts       toParser / fromParser: emitted rules ⇄ parse-that 2.x Parsers
│   ├── generate.ts     BBNFToParser, BBNFToParserFromFile, loadGrammar — the parse-that façade over compile() (applies @recover)
│   ├── gen.ts          (node, ./gen) generate/check: module + .d.ts headed sha256(grammar ⊕ emitter)
│   ├── cli.ts          (bin bbnf) bbnf gen <grammar> --actions <module> --out <file.js> [--check]
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
    │   ├── ast-builders.ts  Shared: rule(), nonterminal(), literal(), alternation(), etc.
    │   └── bbnf-0.1.4/      0.1.4's ASCII-only dispatch (e91428ce1 verbatim) — the stock-ASCII proof's instrument ONLY
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
    ├── gen.test.ts            bbnf gen determinism + --check drift; the module runs
    ├── types.test.ts          E-5: module (checkJs) + .d.ts pass tsc --strict; Actions rejects bad tables
    ├── depth.test.ts          the back-edge depth fault: refusal, exact limit, no counter off the back-edges
    ├── stock-ascii.test.ts    E-3: 0.1.4's dispatch reproduces 0.1.4; the shipped routing differs only in F-b-4
    ├── recover.test.ts        8 tests — @recover parsing, codegen (.recover() wrapping), error collection
    ├── css-stylesheet.test.ts 11 tests — css-stylesheet.bbnf with @recover + multi-error recovery
    └── utils.ts               Test helpers (math eval, random whitespace injection)
```

## Key Exports

- **`BBNFGrammar`** — Parser class. `grammar()`, `grammarWithImports()`.
- **`BBNFToAST(text)`** — Parse BBNF text → AST. (from `parse.ts`)
- **`BBNFToASTWithImports(text)`** — Parse with import directives. (from `parse.ts`)
- **`emitGrammar(ast, options)`** — THE emitter → `{ body, module, dts, … }`. **`compile(ast, options)`** — runtime face (same text, evaluated once).
- **`BBNFToParser(text, options)`** — the parse-that façade: each rule a parse-that 2.x `Parser` over the compiled functions (`toParser`).
- **`generate` / `check`** (`@mkbabb/bbnf-lang/gen`, node) — what `bbnf gen` writes and whether files on disk are current.
- **`loadModuleGraphSync(path, reader)`** — DFS-load (the host's reader is required; no filesystem is assumed) a module and its transitive `@import` graph into a `ModuleRegistry`.
- **`mergeModuleAST(registry, path)`** — Merge a module's local + imported rules into a single AST.
- **`analyzeGrammar(ast)`** — Dep graphs, Tarjan SCC, topo order, ref counts, alias detection, acyclicity classification.
- **`analyzeFirst(ast)`** — the one sound analysis: first units (flag-aware), nullable, eofOk; **`routes(infos)`** routes an ordered choice.
- **`grammarFromModules(files, entry)`** — merged rule table from a files map through `@import`.
- **`removeAllLeftRecursion(ast)`** — Left-recursion elimination.
- **`CharSet`** — 128 ASCII bits + one non-ASCII bit.

## Dependency

- **`@mkbabb/parse-that`** (^2.0.0) — the combinators the grammar front end (`grammar.ts`) parses BBNF with, and the `Parser` the façade answers. The emitted module imports nothing.

## Codegen Optimizations

- **Routing**: every ordered choice is routed by the unit at the cursor (ASCII table, non-ASCII route, EOF route) to the ordered sub-choice that can start there; sound, so it never changes an answer (VALUE-SEMANTICS.md).
- **Staging** (emit.ts): recognize mode wherever a value is discarded; single-class runs as code-unit loops; first-unit guards before a regex or call; direct calls; routed alternatives hoisted once; constants interned (329 → 197 on value.js's grammar). Memoization off.
- **Depth fault**: a counter on the DFS back-edges only; `maxDepth` (default 256) refuses, never throws.

## Build

```bash
npm ci && npm test       # Install + test
npm run build            # Vite library build → dist/bbnf.js + dist/bbnf.cjs
```
