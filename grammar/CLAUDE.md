# CLAUDE.md — grammar/

Example grammars and language specification.

## Structure

```
grammar/
├── BBNF.md                 Full BBNF language specification
├── about.md                Grammar index and descriptions
├── bbnf.bbnf               Self-hosting BBNF grammar
├── json.bbnf               JSON (RFC 8259)
├── json-commented.bbnf     JSON with comments
├── csv.bbnf                CSV (RFC 4180)
├── math.bbnf               Arithmetic with precedence
├── math-ambiguous.bbnf     Deliberately ambiguous arithmetic
├── regex.bbnf              Regular expression syntax
├── ebnf.bbnf               ISO 14977 EBNF
├── emoji.bbnf              Emoji token toy language
├── g4.bbnf                 English sentence structure
├── css-value-unit.bbnf     CSS numeric values + units (base)
├── css-values.bbnf         CSS value types
├── css-color.bbnf          CSS color values (imports css-value-unit)
├── css-selectors.bbnf      CSS selectors Level 3+
├── css-keyframes.bbnf      CSS @keyframes syntax
└── tests/
    └── json/
        ├── valid.jsonl     Valid JSON test cases
        └── invalid.jsonl   Invalid JSON test cases
```

## BBNF Language Quick Reference

```
rule = expression ;                             (* production rule *)
@import "file.bbnf" ;                          (* glob import *)
@import { a, b } from "file.bbnf" ;            (* selective import *)
```

**Terminals**: `"string"`, `'string'`, `` `string` ``, `/regex/`, `epsilon` / `ε`

**Operators** (lowest → highest precedence):
1. `|` alternation
2. `,` concatenation (comma optional)
3. `<<` skip, `>>` next, `-` minus
4. `*` many, `+` many1, `?` optional, `?w` optional whitespace
5. `()` group, `[]` optional group, `{}` repetition group

**Comments**: `// line`, `/* block */`

## Import System

- Non-transitive: must import explicitly at each level.
- Circular imports: detected and rejected.
- Name conflicts: error if same rule imported from multiple sources.
- Path resolution: relative to importing file, `.bbnf` auto-appended.

## CSS Grammar Dependency Chain

```
css-value-unit.bbnf  (base: number, integer, units)
      ↑
css-color.bbnf       (imports { number, integer })
```
