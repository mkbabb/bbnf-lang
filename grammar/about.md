# Grammars

Example grammars demonstrating BBNF's syntax and capabilities. Some are trivial,
some are more involved. Used to show the flexibility of BBNF's syntax.

See [BBNF.md](./BBNF.md) for the full language specification.

## Index

| Grammar | Description |
|---------|-------------|
| [bbnf.bbnf](./bbnf.bbnf) | BBNF's own grammar defined in BBNF (self-hosting) |
| [json.bbnf](./json.bbnf) | JSON data format (RFC 8259) |
| [json-commented.bbnf](./json-commented.bbnf) | JSON extended with single/multi-line comments |
| [csv.bbnf](./csv.bbnf) | CSV format (RFC 4180) |
| [math.bbnf](./math.bbnf) | Arithmetic expressions with correct precedence |
| [math-ambiguous.bbnf](./math-ambiguous.bbnf) | Deliberately ambiguous arithmetic grammar |
| [regex.bbnf](./regex.bbnf) | Regular expression syntax with flags |
| [ebnf.bbnf](./ebnf.bbnf) | ISO 14977 EBNF grammar |
| [emoji.bbnf](./emoji.bbnf) | Toy programming language using emoji tokens |
| [g4.bbnf](./g4.bbnf) | Simple English sentence structure grammar |

### CSS Family

A suite of grammars for CSS syntax, using `@import` to share common definitions:

| Grammar | Description |
|---------|-------------|
| [css-value-unit.bbnf](./css-value-unit.bbnf) | CSS numeric values and length units |
| [css-values.bbnf](./css-values.bbnf) | CSS value types (lengths, percentages, etc.) |
| [css-color.bbnf](./css-color.bbnf) | CSS color values (`rgb()`, `hsl()`, hex, named colors) |
| [css-selectors.bbnf](./css-selectors.bbnf) | CSS selectors Level 3+ |
| [css-keyframes.bbnf](./css-keyframes.bbnf) | CSS `@keyframes` animation syntax |
