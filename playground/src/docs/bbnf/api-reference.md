---
title: API Reference
order: 6
section: BBNF
---

# API Reference

## TypeScript

### Grammar Compilation

```typescript
import { BBNFToASTWithImports, ASTToParser, analyzeGrammar, computeFirstSets, dedupGroups } from "@mkbabb/bbnf-lang";
```

#### `BBNFToASTWithImports(source: string)`

Parses a BBNF grammar string and returns `[errors, result]` where result contains:
- `rules` — `Map<string, RuleAST>` of parsed production rules
- `pretties` — `Array` of @pretty directive metadata
- `recovers` — `Array` of @recover directive metadata

#### `ASTToParser(ast, analysis, firstNullable, recovers, tagAlternations?)`

Compiles an AST into a record of parser combinators keyed by rule name. Each parser has a `.parse(input)` method.

#### `analyzeGrammar(ast)`

Performs static analysis: dependency graph, FIRST/FOLLOW sets.

#### `prettify(result, entryRule, ast, pretties, options)`

Formats a parse result using @pretty directives.

**Options:**
- `maxWidth: number` — target line width (default: 80)
- `indent: number` — indentation width (default: 2)
- `useTabs: boolean` — use tabs instead of spaces

### WASM (gorgeous)

```typescript
import init, { format_json, format_css } from "@mkbabb/bbnf-wasm";

await init();
const output = format_json(input, 80, 4, false);
```

Available formatters: `format_json`, `format_css`, `format_bbnf`, `format_bnf`, `format_ebnf`.

## Rust

### Derive Macro

```rust
use bbnf::BBNF;

#[derive(BBNF)]
#[grammar = "path/to/grammar.bbnf"]
#[entry = "value"]
struct MyParser;
```

Generates a `parse()` method on the struct that returns the parsed AST.

### Parser Combinators

```rust
use parse_that::Parser;

let p = Parser::string("hello")
    .then(Parser::regex(r"\s+"))
    .then(Parser::string("world"));
```

### Pretty-Printing

```rust
use pprint::Doc;

let doc = Doc::group(Doc::concat(vec![
    Doc::text("{"),
    Doc::indent(2, Doc::concat(vec![/* ... */])),
    Doc::text("}"),
]));

let output = doc.render(80);
```
