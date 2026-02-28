# CLAUDE.md—grammar/

Example grammars and language specification.

## Structure

```
grammar/
├── BBNF.md                     BBNF language specification
├── about.md                    Grammar index and descriptions
├── css/                        CSS grammar family
│   ├── css-value-unit.bbnf     CSS numeric values + units (base)
│   ├── css-color.bbnf          CSS color values (imports css-value-unit)
│   ├── css-values.bbnf         CSS composite value types
│   ├── css-selectors.bbnf      CSS selectors Level 4
│   └── css-keyframes.bbnf      CSS @keyframes syntax
├── lang/                       Language/format grammars
│   ├── bbnf.bbnf               Self-hosting BBNF grammar
│   ├── json.bbnf               JSON (RFC 8259)
│   ├── json-commented.bbnf     JSON with comments
│   ├── csv.bbnf                CSV (RFC 4180)
│   ├── math.bbnf               Arithmetic with precedence
│   ├── math-ambiguous.bbnf     Deliberately ambiguous arithmetic
│   ├── regex.bbnf              Regular expression syntax
│   ├── ebnf.bbnf               ISO 14977 EBNF
│   ├── emoji.bbnf              Emoji token toy language
│   └── g4.bbnf                 English sentence structure
└── tests/
    └── json/
        ├── valid.jsonl         Valid JSON test cases
        └── invalid.jsonl       Invalid JSON test cases
```

## BBNF Language Quick Reference

```
rule = expression ;                             (* production rule *)
@import "file.bbnf" ;                          (* glob import *)
@import { a, b } from "file.bbnf" ;            (* selective import *)
```

**Terminals**: `"string"`, `'string'`, `` `string` ``, `/regex/`, `epsilon` / `ε`

**Operators** (lowest -> highest precedence):
1. `|` alternation
2. `,` concatenation (comma optional)
3. `<<` skip, `>>` next, `-` minus
4. `*` many, `+` many1, `?` optional, `?w` optional whitespace
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
css/css-value-unit.bbnf  <- canonical base (numbers, units, dimensions)
      |
css/css-color.bbnf       <- glob imports css-value-unit
      |
css/css-values.bbnf      <- glob imports css-value-unit + css-color
css/css-keyframes.bbnf   <- glob imports css-value-unit
css/css-selectors.bbnf   <- standalone (no imports)
```

**Dispatch table caveat:** Separator rules must have disjoint static FIRST sets
for correct dispatch; runtime `.trim()` overrides that expand a branch's
effective FIRST set will break dispatch routing.
