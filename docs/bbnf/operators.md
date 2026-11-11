---
title: Operators
order: 3
section: BBNF
---

# Operators

Operators are listed from **lowest** to **highest** precedence.

## 1. Alternation `|`

Ordered choice — tries each alternative left to right:

```bbnf
bool = "true" | "false" ;
```

When all alternatives are string literals, an optimized dispatch table may be emitted.

## 2. Concatenation `,`

Sequence — matches each operand in order:

```bbnf
pair = key , ":" , value ;
```

The comma is optional when operands are unambiguous, but recommended for clarity.

## 3. Skip `<<` and Next `>>`

Value projection — match two sub-expressions but discard one side:

- `A << B` — keep A, discard B
- `A >> B` — keep B, discard A

```bbnf
array = "[" >> elements << "]" ;
```

## 4. Minus `-`

Set difference — match A only if B does not match at the same position:

```bbnf
non_digit = /\w/ - /\d/ ;
```

## 5. Quantifiers `*` `+` `?`

Postfix repetition and optionality:

| Operator | Name | Meaning |
|----------|------|---------|
| `*` | Many | Zero or more |
| `+` | Many1 | One or more |
| `?` | Optional | Zero or one |

```bbnf
digits = /[0-9]/ + ;
items  = item * ;
sign   = ("+" | "-") ? ;
```

### Separated lists

Every expression means what it says; no backend rewrites one form into
another. A strict separated list is written with the separator between
elements:

```bbnf
args  = arg , ( "," >> arg ) * ;                         // one or more
array = "[" >> ( value , ( "," >> value ) * ) ? << "]" ;  // zero or more
```

`( x << sep ? ) *` is not a separated list. It is a repetition of `x`,
each optionally followed by `sep`, so it accepts `x x` and a trailing
`sep`. Backends lower the strict spellings above, and only those, to a
separated-list loop, which accepts the same language.

## 6. Optional Whitespace `?w`

Wraps the preceding term so optional whitespace is consumed before and after:

```bbnf
comma = "," ?w ;
```

Equivalent to `ws* >> expr << ws*` where `ws` matches `\s`.

## 7. Grouping

| Syntax | Meaning |
|--------|---------|
| `( expr )` | Parenthesized group |
| `[ expr ]` | Optional group — `( expr ) ?` |
| `{ expr }` | Repetition group — `( expr ) *` |

## Precedence Summary

| Level | Operator(s) | Description |
|-------|-------------|-------------|
| 1 | `\|` | Alternation |
| 2 | `,` | Concatenation |
| 3 | `<<` `>>` `-` | Skip, next, minus |
| 4 | `*` `+` `?` `?w` | Quantifiers |
| 5 | `()` `[]` `{}` | Grouping |
