---
title: Getting Started
order: 1
section: BBNF
---

# Getting Started with BBNF

BBNF (Better Backus-Naur Form) is a grammar notation for defining parsers. It extends EBNF with operators for whitespace handling, value projection, set difference, and formatting directives.

## Quick Example

A simple JSON value grammar in BBNF:

```bbnf
value = object | array | string | number | "true" | "false" | "null" ;

object = "{" , members? , "}" ;
members = member , ("," , member)* ;
member = string , ":" , value ;

array = "[" , elements? , "]" ;
elements = value , ("," , value)* ;

string = /"[^"]*"/ ;
number = /-?\d+(\.\d+)?([eE][+-]?\d+)?/ ;
```

## How It Works

1. **Write a grammar** — define production rules using BBNF syntax
2. **Parse input** — the grammar compiles to a parser combinator tree that processes text
3. **Format output** — add `@pretty` directives to generate a formatter from the same grammar

## Try It

Head to the [Playground](/playground) to write and test grammars interactively. The playground compiles grammars in real-time and shows the parsed AST and formatted output side by side.

## Installation

### TypeScript

```bash
npm install @mkbabb/bbnf-lang
```

```typescript
import { BBNFToASTWithImports, ASTToParser } from "@mkbabb/bbnf-lang";

const grammar = `value = "hello" | "world" ;`;
const [, parsed] = BBNFToASTWithImports(grammar);
const parsers = ASTToParser(parsed.rules, ...);
const result = parsers.value.parse("hello");
```

### Rust

```toml
[dependencies]
bbnf = "0.3"
```

```rust
#[derive(BBNF)]
#[grammar = "path/to/grammar.bbnf"]
struct MyParser;
```
