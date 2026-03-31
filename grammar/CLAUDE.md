# CLAUDE.md—grammar/

Example grammars and language specification.

## Structure

```
grammar/
├── BBNF.md                     BBNF language specification
├── about.md                    Grammar index and descriptions
├── css/
│   ├── pretty.bbnf             Formatting grammar with @pretty hints (opaque spans)
│   └── l4/                     Full CSS L4 spec via @import composition
│       ├── stylesheet.bbnf     Entry point (composes properties, selectors, media)
│       ├── value-unit.bbnf     Numeric values + units (canonical base)
│       ├── color.bbnf          Color values (imports value-unit)
│       ├── values.bbnf         Composite value types (imports all value grammars)
│       ├── selectors.bbnf      CSS Selectors Level 4
│       ├── keyframes.bbnf      @keyframes syntax
│       ├── media.bbnf          Media Queries Level 5
│       ├── properties.bbnf     Property-aware declaration dispatch
│       ├── easing.bbnf         Easing functions
│       ├── filters.bbnf        Filter functions
│       ├── gradients.bbnf      Gradient functions
│       ├── transforms.bbnf     Transform functions
│       ├── tokens.bbnf         Shared tokens (ident, string)
│       ├── keywords.bbnf       Keyword enumerations
│       └── func-body.bbnf      Function body & math expressions
├── json/
│   └── json.bbnf               JSON (RFC 8259) with -> mappings and @pretty hints
├── ebnf/
│   └── ebnf.bbnf               ISO 14977 EBNF
├── bnf/
│   └── bnf.bbnf                Backus-Naur Form
├── bbnf/
│   └── bbnf.bbnf               Self-hosting BBNF grammar
├── google-sheets/
│   └── google-sheets.bbnf      Google Sheets formula parser
├── misc/                       Miscellaneous/toy grammars
│   ├── csv.bbnf                CSV (RFC 4180)
│   ├── math.bbnf               Arithmetic with precedence
│   ├── math-ambiguous.bbnf     Deliberately ambiguous arithmetic
│   ├── regex.bbnf              Regular expression syntax
│   ├── json-commented.bbnf     JSON with comments
│   ├── emoji.bbnf              Emoji token toy language
│   └── g4.bbnf                 English sentence structure
└── tests/
    ├── json/
    │   ├── valid.jsonl         Valid JSON test cases
    │   └── invalid.jsonl       Invalid JSON test cases
    └── css/
        └── complex-errors.css  CSS test vector with intentional parse errors (recovery)
```

## BBNF Language Quick Reference

```
rule = expression ;                             (* production rule *)
@import "file.bbnf" ;                          (* glob import *)
@import { a, b } from "file.bbnf" ;            (* selective import *)
@recover rule syncExpr ;                        (* recovery directive (any BBNF expr as sync) *)
@pretty rule hint1 hint2 ;                      (* prettify hints for codegen *)
@pretty * auto ;                                (* grammar-wide heuristic mode: auto|minimal|off *)
@token ruleName ;                               (* mark rule as lexical token: span eligible + fusion inline *)
```

**Terminals**: `"string"`, `'string'`, `` `string` ``, `/regex/`, `epsilon` / `ε`

**Operators** (lowest -> highest precedence):
1. `|` alternation
2. `,` concatenation (comma optional)
3. `<<` skip, `>>` next, `-` minus
4. `*` many, `+` many1, `?` optional, `?w` optional whitespace, `->` mapping (Rust only)
5. `()` group, `[]` optional group, `{}` repetition group

**Comments**: `// line`, `/* block */`

## Import System

- Cyclic imports: allowed (Python-style partial-init—module registered before recursing).
- Selective imports: transitive local dependencies are automatically unfurled.
- Non-transitive scope: A imports B, B imports C—A can't see C's rules.
- Name conflicts: error if same rule imported from multiple sources.
- Path resolution: relative to importing file, `.bbnf` auto-appended.
- Imports may appear at any position (after comments, between rules, etc.).

## CSS Grammar Dependency Chain

```
css/l4/value-unit.bbnf   <- canonical base (numbers, units, dimensions)
      |
css/l4/color.bbnf        <- imports value-unit
      |
css/l4/values.bbnf       <- imports value-unit + color + gradients + transforms + filters + easing
css/l4/keyframes.bbnf    <- imports value-unit
css/l4/selectors.bbnf    <- imports tokens
      |
css/l4/stylesheet.bbnf   <- imports properties, selectors, media
```

**Dispatch table caveat:** Separator rules must have disjoint static FIRST sets
for correct dispatch; runtime `.trim()` overrides that expand a branch's
effective FIRST set will break dispatch routing.
