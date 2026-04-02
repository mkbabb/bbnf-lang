---
title: API Reference
order: 6
section: BBNF
---

# API Reference

## Quick Start

```code-tabs
---typescript---
import { BBNFToASTWithImports, ASTToParser, analyzeGrammar, computeFirstSets } from "@mkbabb/bbnf-lang";

const grammar = `value = "hello" | "world" ;`;
const [errors, parsed] = BBNFToASTWithImports(grammar);
const analysis = analyzeGrammar(parsed.rules);
const firstNullable = computeFirstSets(parsed.rules, analysis);
const parsers = ASTToParser(parsed.rules, analysis, firstNullable, parsed.recovers);
const result = parsers.value.parse("hello");
---rust---
use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "path/to/grammar.bbnf")]
struct MyParser;

let result = MyParser::value().parse("hello");
```

## TypeScript API

### Grammar Compilation

#### `BBNFToAST(source: string)`

Parses a BBNF grammar string. Returns `[errors, result]` where result contains:
- `rules` — `Map<string, RuleAST>` of production rules
- `pretties` — `@pretty` directive metadata
- `recovers` — `@recover` directive metadata

#### `BBNFToASTWithImports(source: string)`

Like `BBNFToAST` but resolves `@import` directives. Handles cyclic imports via partial initialization.

#### `ASTToParser(ast, analysis?, firstNullable?, recovers?, tagAlternations?)`

Compiles an AST into a record of `Parser<T>` combinators keyed by rule name. Applies these optimizations:

- **Dispatch tables** — disjoint FIRST sets → O(1) character dispatch
- **Pattern recognition** — regex coalescing, `sepBy` detection, wrap coalescing
- **Lazy references** — enable post-generation parser customization
- **Recovery wrapping** — `@recover` rules get `.recover(syncParser, null)`

#### `analyzeGrammar(ast)`

Static analysis: dependency graphs, Tarjan SCC, topological sort, reference counts, alias detection, FIRST-set conflict identification.

#### `computeFirstSets(ast, analysis)`

Iterative fixed-point computation of per-rule FIRST sets (`CharSet`) and nullable flags.

#### `removeAllLeftRecursion(ast)`

Direct left recursion via tail-rule extraction (Paull's algorithm). Indirect via substitution on topological order.

#### `buildDispatchTable(alts, firstSets, nullable)`

Builds an O(1) dispatch lookup for alternations with disjoint FIRST sets. Returns a `DispatchTable` mapping characters to branch indices.

### Module System

#### `loadModuleGraphSync(path, reader?)`

DFS-load a `.bbnf` file and its transitive `@import` graph. Returns a `ModuleRegistry`.

#### `mergeModuleAST(registry, path)`

Merge a module's local + imported rules into a single AST. Local rules shadow imports.

### Convenience

#### `BBNFToParser(text)`

End-to-end: text → executable parsers. Equivalent to parsing, analyzing, and compiling in one call.

#### `BBNFGrammar`

Parser class with `grammar()` and `grammarWithImports()` methods for programmatic access to the BBNF parser itself.

## Rust API

### Derive Macro

```rust
use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "path/to/grammar.bbnf")]
struct MyParser;
```

Generates an enum and per-rule parser methods. Available attributes:

| Attribute | Effect |
|-----------|--------|
| `path` | Grammar file path |
| `prettify` | Emit `to_doc()` for formatting |
| `skip_recover` | Omit `@recover` codegen |
| `remove_left_recursion` | Apply Paull transform |
| `ignore_whitespace` | Auto-trim whitespace |
| `debug` | Instrument all rules for trace output |
| `slab` | Monolithic slab codegen with BumpSlab |
| `span` | Span-only monolithic codegen (zero allocation) |

### Parser Combinators

```rust
use parse_that::Parser;

let p = Parser::string("hello")
    .then(Parser::regex(r"\s+"))
    .then(Parser::string("world"));
```

See the [parse-that docs](/docs/parse-that/overview) for the full combinator API.

### Pretty-Printing

```rust
use pprint::{Doc, pprint, Printer};

let doc = Doc::from("{")
    + (Doc::Hardline + items.join(Doc::from(",") + Doc::Hardline)).indent()
    + Doc::Hardline
    + Doc::from("}");

let output = pprint(doc.group(), Printer::new(80, 2, false));
```

See the [pprint docs](/docs/pprint/overview) for the full Doc API.

## WASM

The `bbnf-wasm` crate provides WebAssembly formatters via gorgeous. Six AOT formatters are available:

```typescript
import init, { format_json, format_css, format_bbnf, format_bnf, format_ebnf, format_google_sheets } from "bbnf-wasm";

await init();
const output = format_json(input, 80, 4, false);
```

Each formatter takes `(input: string, max_width: number, indent: number, use_tabs: boolean)` and returns `string | undefined` (undefined if parsing fails).

| Formatter | Language |
|-----------|---------|
| `format_json` | JSON |
| `format_css` | CSS |
| `format_bbnf` | BBNF |
| `format_bnf` | BNF |
| `format_ebnf` | EBNF |
| `format_google_sheets` | Google Sheets formulas |

Beyond formatters, the WASM module exports grammar analysis (`analyze_grammar`), hover (`hover_at_offset`), completions, semantic tokens, and a full VM interpreter for custom grammars.

### Debug Exports

| Export | Description |
|--------|-------------|
| `compile_grammar_debug(grammar, entry_rule?)` | Compile with `DebugBreak` instrumentation |
| `debug_step(handle, input, mode, breakpoint_rules_json)` | Step through parse execution |
