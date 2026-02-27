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

A suite of grammars for CSS syntax, using `@import` to share common definitions.
See [PROGRESS.md](./PROGRESS.md) for revision notes and the dispatch-table design
constraints that shape separator rules.

```
css-value-unit.bbnf  ← canonical base (numbers, units, dimensions)
      ↑
css-color.bbnf       ← imports css-value-unit (148 named colors, hex, rgb/hsl/hwb/lab/lch/oklab/oklch, color-mix)
      ↑
css-values.bbnf      ← imports css-value-unit + css-color (calc/min/max/clamp, var, env, url)
css-keyframes.bbnf   ← imports css-value-unit (@keyframes, declarations, functions)
css-selectors.bbnf   ← standalone (Level 4: :is/:where/:not/:has, An+B, namespaces)
```

| Grammar | Description |
|---------|-------------|
| [css-value-unit.bbnf](./css-value-unit.bbnf) | CSS Values Level 4 — numeric primitives, all dimension types (length, angle, time, frequency, resolution, flex), viewport/container/font-relative units |
| [css-color.bbnf](./css-color.bbnf) | CSS Color Level 4 — hex, named colors (full set), `rgb()`/`hsl()`/`hwb()`/`lab()`/`lch()`/`oklab()`/`oklch()`, `color()`, `color-mix()` |
| [css-values.bbnf](./css-values.bbnf) | CSS Values Level 4 — composite value grammar with recursive `calc()`/`min()`/`max()`/`clamp()`, `var()`, `env()`, `url()` |
| [css-selectors.bbnf](./css-selectors.bbnf) | CSS Selectors Level 4 — type/class/id/attribute selectors, `:is()`/`:where()`/`:not()`/`:has()`, An+B microsyntax, namespaces, relative selectors |
| [css-keyframes.bbnf](./css-keyframes.bbnf) | CSS Animations Level 1 — `@keyframes` with import-based numeric/unit primitives |
