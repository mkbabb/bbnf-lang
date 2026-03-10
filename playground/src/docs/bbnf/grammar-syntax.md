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

### Regular Expressions

Match a pattern. Delimited by forward slashes:

```bbnf
/[0-9]+/
/[_a-zA-Z][_a-zA-Z0-9]*/
```

### Epsilon

The keyword `epsilon` (or `ε`) matches the empty string:

```bbnf
empty = epsilon ;
maybe = "x" | ε ;
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
- Non-transitive: A importing B importing C does not give A access to C
- Local rules shadow imports
