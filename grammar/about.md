# Grammars

Example grammars written in BBNF. Some are trivial, some aren't.

See [BBNF.md](./BBNF.md) for the language specification.

## Index

| Grammar | Description |
|---------|-------------|
| [lang/bbnf.bbnf](./lang/bbnf.bbnf) | BBNF's own grammar defined in BBNF (self-hosting) |
| [lang/json.bbnf](./lang/json.bbnf) | JSON data format (RFC 8259) |
| [lang/json-commented.bbnf](./lang/json-commented.bbnf) | JSON extended with single/multi-line comments |
| [lang/csv.bbnf](./lang/csv.bbnf) | CSV format (RFC 4180) |
| [lang/math.bbnf](./lang/math.bbnf) | Arithmetic expressions with correct precedence |
| [lang/math-ambiguous.bbnf](./lang/math-ambiguous.bbnf) | Deliberately ambiguous arithmetic grammar |
| [lang/regex.bbnf](./lang/regex.bbnf) | Regular expression syntax with flags |
| [lang/ebnf.bbnf](./lang/ebnf.bbnf) | ISO 14977 EBNF grammar |
| [lang/emoji.bbnf](./lang/emoji.bbnf) | Toy programming language using emoji tokens |
| [lang/g4.bbnf](./lang/g4.bbnf) | Simple English sentence structure grammar |

### CSS Family

A suite of grammars for CSS syntax, using `@import` to share common definitions.
See [PROGRESS.md](./PROGRESS.md) for revision notes and the dispatch-table design
constraints that shape separator rules.

```
css/css-value-unit.bbnf  <- canonical base (numbers, units, dimensions)
      |
css/css-color.bbnf       <- imports css-value-unit (150 color entries, hex, rgb/hsl/hwb/lab/lch/oklab/oklch, color-mix)
      |
css/css-values.bbnf      <- imports css-value-unit + css-color (calc/min/max/clamp, var, env, url)
css/css-keyframes.bbnf   <- imports css-value-unit (@keyframes, declarations, functions)
css/css-selectors.bbnf   <- standalone (Level 4: :is/:where/:not/:has, An+B, namespaces)
```

| Grammar | Description |
|---------|-------------|
| [css/css-value-unit.bbnf](./css/css-value-unit.bbnf) | CSS Values Level 4—numeric primitives, dimension types (length, angle, time, frequency, resolution, flex, percentage), viewport/container/font-relative units |
| [css/css-color.bbnf](./css/css-color.bbnf) | CSS Color Level 4—hex, 148 named colors + `transparent`/`currentcolor`, `rgb()`/`hsl()`/`hwb()`/`lab()`/`lch()`/`oklab()`/`oklch()`, `color()`, `color-mix()` |
| [css/css-values.bbnf](./css/css-values.bbnf) | CSS Values Level 4—composite values: recursive `calc()`/`min()`/`max()`/`clamp()`, `var()`, `env()`, `url()` |
| [css/css-selectors.bbnf](./css/css-selectors.bbnf) | CSS Selectors Level 4—type/class/id/attribute selectors, `:is()`/`:where()`/`:not()`/`:has()`, An+B microsyntax, namespaces, relative selectors |
| [css/css-keyframes.bbnf](./css/css-keyframes.bbnf) | CSS Animations Level 1—`@keyframes` with import-based numeric/unit primitives |
