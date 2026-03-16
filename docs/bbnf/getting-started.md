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

Run this grammar directly in the browser — click **Run** to parse the input with the WASM VM:

```runnable-code
{ "grammar": "value = object | array | string | number | \"true\" | \"false\" | \"null\" ;\n\nobject = \"{\" , members? , \"}\" ;\nmembers = member , (\",\" , member)* ;\nmember = string , \":\" , value ;\n\narray = \"[\" , elements? , \"]\" ;\nelements = value , (\",\" , value)* ;\n\nstring = /\"[^\"]*\"/ ;\nnumber = /-?\\d+(\\.\\d+)?([eE][+-]?\\d+)?/ ;", "input": "{\"name\": \"BBNF\", \"version\": 1, \"features\": [\"parsing\", \"formatting\"]}", "language": "bbnf" }
```

Or head to the [Playground](/playground) to write and test grammars interactively with full editor support.

## Installation

```code-tabs
---bash---
# TypeScript / npm
npm install @mkbabb/bbnf-lang
---toml---
# Rust / Cargo
[dependencies]
bbnf = "0.3"
```

### Usage

```code-tabs
---typescript---
import { BBNFToParser } from "@mkbabb/bbnf-lang";

const grammar = `value = "hello" | "world" ;`;
const parsers = BBNFToParser(grammar);
const result = parsers.value.parse("hello");
---rust---
use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "grammar.bbnf")]
struct MyParser;

let result = MyParser::value().parse("hello");
```
