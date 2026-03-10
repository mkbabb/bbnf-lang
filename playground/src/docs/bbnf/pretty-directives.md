---
title: Pretty Directives
order: 4
section: BBNF
---

# Pretty Directives

`@pretty` directives attach formatting hints to production rules. The prettify code generator uses them to emit `Doc` nodes for the pretty-printer.

## Syntax

```bbnf
@pretty ruleName hint1 hint2 ... ;
```

## Available Hints

| Hint | Effect |
|------|--------|
| `group` | Wrap in a Group — content breaks as a unit |
| `indent` | Increase indentation inside the group |
| `dedent` | Decrease indentation |
| `block` | Block-level formatting (newlines between children) |
| `blankline` | Insert a blank line before |
| `nobreak` | Prevent line breaks inside |
| `softbreak` | Allow breaking here if needed |
| `hardbreak` | Force a line break |
| `compact` | Minimize whitespace |
| `fast` | Use fast-path formatting |
| `off` | Disable formatting for this rule |

## Separator Directive

`sep("...")` specifies a custom separator between repeated elements:

```bbnf
@pretty object group indent sep(", ") ;
@pretty member sep(": ") ;
```

With `group`, the separator trims trailing spaces on break (e.g. `", "` becomes `","` + newline).

## Split Directive

`split("...")` performs format-time balanced splitting for opaque spans:

```bbnf
@pretty selectorSpan split(",") group sep(", ") ;
```

This splits the span's text at unquoted, unnested occurrences of the delimiter.

## Global Meta-Directive

Control heuristic inference for un-annotated rules:

```bbnf
@pretty * auto ;     // auto (default), minimal, or off
```

## Example

```bbnf
value = object | array | string | number ;
@pretty value group ;

object = "{" , members? , "}" ;
@pretty object group indent sep(", ") ;

members = member , ("," , member)* ;
member = string , ":" , value ;
@pretty member sep(": ") ;

array = "[" , elements? , "]" ;
@pretty array group indent sep(", ") ;
```

This produces formatted JSON output:

```json
{
  "name": "BBNF",
  "version": 1,
  "items": [1, 2, 3]
}
```
