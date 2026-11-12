---
title: Grammar Syntax
order: 2
section: BBNF
---

# Grammar Syntax

A BBNF file consists of **import directives** and **production rules** in any order, interleaved with comments. Each rule defines a named nonterminal in terms of an expression built from terminals, nonterminal references, and operators.

## Production Rules

A production rule binds a name to an expression, terminated by `;` or `.`:

```bbnf
name = expression ;
```

The left-hand side is an **identifier**: `[_a-zA-Z][_a-zA-Z0-9-]*`. Hyphens are allowed (e.g. `color-value`).

The first rule in a grammar is treated as the start symbol by convention.

## Terminal Expressions

### String Literals

Match an exact sequence of characters. Delimited by double quotes, single quotes, or backticks:

```bbnf
"hello"
'world'
`template`
```

Inside a string literal, `\n`, `\t`, `\r`, `\f` and `\b` are the line feed, tab, carriage return, form feed and backspace characters, and `\\` is a backslash.

### Regular Expressions

Match a pattern. Delimited by forward slashes:

```bbnf
/[0-9]+/
/[_a-zA-Z][_a-zA-Z0-9]*/
```

Inside a pattern, `\` escapes the next character: `\/` is a slash in the pattern, `\\` is a backslash, and the pattern ends at the first unescaped `/`, so `/a\\/` is the pattern `a\\`.

A pattern may begin with an inline flag group, `(?flags)`, whose flags (`i`, `m`, `s`, `u`) apply to the whole pattern.

### Epsilon

The keyword `epsilon` (or `ε`) matches the empty string:

```bbnf
empty = epsilon ;
maybe = "x" | ε ;
```

## Span Capture

`@{ expr }` matches `expr` and yields the source span it matched as its value:

```bbnf
urlFunction = @{ "url" , "(" >> /[^)]+/ << ")" } ;
```

## Nonterminal References

A bare identifier refers to another rule:

```bbnf
value = object | array | string | number ;
```

References are resolved lazily — rules may appear in any order and mutual recursion is supported.

## Comments

```bbnf
// Line comment
/* Block comment */
```

## Import Directives

Import rules from other `.bbnf` files:

```bbnf
@import "path/to/base.bbnf" ;
@import { number, integer } from "path/to/common.bbnf" ;
```

- Paths are relative to the importing file
- **Glob imports** are non-transitive—A importing all of B doesn't expose B's own imports to A
- **Selective imports** auto-unfurl transitive dependencies—`@import { x } from "B.bbnf"` pulls in any local rules that `x` depends on
- Local rules shadow imports

## Directives Summary

BBNF defines eight directives, all prefixed with `@` and terminated by `;`:

| Directive | Syntax | Purpose |
|-----------|--------|---------|
| `@import` | `@import "path" ;` / `@import { names } from "path" ;` | Compose grammars from other files |
| `@recover` | `@recover ruleName syncExpr ;` | Per-rule error recovery with sync expression |
| `@pretty` | `@pretty ruleName hints ;` | Control pretty-printing Doc emission |
| `@no_collapse` | `@no_collapse ruleName ;` | Preserve rule identity in AST (prevent Span compression) |
| `@ws` | `@ws /regex/ ;` | Override `?w` whitespace operator grammar-wide |
| `@inline` | `@inline ruleName ;` | Force-inline rule body at every call site |
| `@token` | `@token ruleName ;` | Mark rule as lexical token (span-eligible, fusion-inlined, variant preserved) |
| `@debug` | `@debug ruleName ;` / `@debug * ;` | Instrument rules for debug tracing across compiled and VM paths |

## Operators

Operators from lowest to highest precedence:

| Level | Operator(s) | Description |
|-------|-------------|-------------|
| 1 | `\|` | Alternation (ordered choice) |
| 2 | `,` | Concatenation (comma optional) |
| 3 | `<<` `>>` `-` | Skip, next, minus |
| 4 | `*` `+` `?` `?w` `->` | Quantifiers, whitespace trim, mapping |
| 5 | `()` `[]` `{}` | Grouping, optional group, repetition group |

### Per-Expression Mapping `->` (Rust only)

The `->` operator maps a factor's parse result through a Rust function. Three shorthand forms:

```bbnf
// Closure — inline conversion
number = /[0-9]+/ -> |s: Span| -> f64 { s.as_str().parse().unwrap() } ;

// Function path — delegate to a named function
color = /[0-9a-fA-F]{6}/ -> crate::parse_hex_color ;

// Constant — discard the matched text, emit a fixed value
lengthUnit = "px" -> 0u8 | "em" -> 1u8 | "rem" -> 2u8 ;
```

The right side of `->` is a value expression, optionally followed by `: Type`; its grammar is `grammar/bbnf/expressions.bbnf` (literals, paths, `input` and its fields, calls, unary and binary operators, and closures `|a, b| expr`). A front end that does not emit Rust keeps the expression as text, and a mapping never changes what the factor matches.

The legacy `=>` operator applies a mapping to the entire rule RHS. `->` is preferred because it attaches to individual factors, enabling per-branch mapping in alternations.
