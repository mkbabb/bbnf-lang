# CSS Grammar Revision — Progress Notes

## Overview

Comprehensive revision of the five CSS grammar files to improve spec compliance,
eliminate duplication, establish a proper import hierarchy, and document key
dispatch-table interactions.

## Import Hierarchy

```
css-value-unit.bbnf  ← canonical base (no imports)
      ↑
css-color.bbnf       ← glob imports css-value-unit
      ↑
css-values.bbnf      ← glob imports css-value-unit + css-color
css-keyframes.bbnf   ← glob imports css-value-unit
css-selectors.bbnf   ← standalone (no imports)
```

Selective imports (`@import { a, b } from ...`) were replaced with glob imports
because the TS import resolver copies only the named rules, not their transitive
dependencies. A selective import of `percentage` wouldn't bring `percentageUnit`,
causing undefined nonterminal references at runtime.

## Key Discovery: Dispatch Table vs `.trim()` Override

When the code generator builds a dispatch table for an alternation like
`alphaSep = div | sep`, it uses static FIRST sets computed from the grammar:

- `div` → FIRST = `{ '/' }`
- `sep` → FIRST = `{ ',', ' ', '\t', '\n', ... }`

These sets are disjoint, so dispatch routes `'/'` to branch 0 and `' '` to
branch 1. However, if the runtime test code overrides `nonterminals.div` with
`string("/").trim()`, the **effective** FIRST set of `div` expands to include
whitespace characters. The dispatch table doesn't know about this — it was built
at grammar-compile time.

Result: input `" / "` dispatches `' '` → sep (branch 1) instead of the trimmed
div (branch 0). The parse fails or returns the wrong branch.

**Mitigations:**
1. Override `alphaSep` at the alternation level, not `div` at the leaf level.
2. Avoid whitespace around `/` in test data when using the grammar's built-in
   dispatch tables.
3. Accept this as a fundamental limitation: dispatch tables optimize for the
   grammar's static structure, not runtime overrides.

## What Changed

### css-value-unit.bbnf
- Added viewport units: large (`lvw`/`lvh`/...), small (`svmin`/`svmax`/...),
  dynamic (`dvb`/`dvi`)
- Added font-relative units: `cap`, `ic`, `rcap`, `rex`, `rch`, `ric`
- Added container query units (already present, organized into named group)
- Added frequency (`Hz`, `kHz`), flex (`fr`), and resolution (`x`) types
- Moved `number`/`integer` to top of file (most commonly imported)

### css-color.bbnf
- Added complete CSS Color Level 4 named color set (148 colors + `transparent`
  + `currentcolor`)
- Added `rgba`/`hsla` to `colorType` (legacy forms)
- Hex digit patterns reordered longest-first (8→6→4→3) for unambiguous parsing
- Import directive moved above comments (TS parser limitation: comments before
  first item block `grammarWithImports`)
- Design-note comment documents the dispatch-table/trim interaction

### css-values.bbnf (rewritten)
- **Before:** Duplicated all of css-value-unit.bbnf + had catch-all regexes
  `/[^)]+/` for `calc()` and `var()` arguments.
- **After:** Imports from css-value-unit and css-color. Proper recursive
  `calc()`/`min()`/`max()`/`clamp()` grammar with `mathExpr`/`mathProduct`.
  Structured `var()` and `env()` rules. Added `url()` function.

### css-selectors.bbnf (rewritten)
- **Before:** Level 3 only, catch-all `/[^)]+/` for pseudo-class arguments,
  regexes for combinators and comma separators.
- **After:** Level 4 compliant. Proper `:is()`, `:where()`, `:not()`, `:has()`
  with recursive `selectorList` arguments. An+B microsyntax for `:nth-*()`.
  Namespace support (`nsPrefix`, `wqName`). Relative selectors for `:has()`.
  `::part()`, `::slotted()`, `::highlight()` functional pseudo-elements.

### css-keyframes.bbnf
- Imports from css-value-unit.bbnf instead of re-defining `NUMBER`/`PERCENTAGE`
- `?w` used consistently throughout

## Parser Limitation: Comments Before Imports

The TS `grammarWithImports()` parser wraps production rules with
`lineComment().trim().many()` but does not wrap import directives similarly.
Comments before the first `@import` directive prevent parsing (the `any(import,
rule)` matcher fails on both branches). **Workaround:** Always place `@import`
directives at the very top of the file, with comments after.

## Test Adjustments

- CSS keyframes test: Changed from `BBNFToParser(readFileSync(...))` to
  `BBNFToParserFromFile(path)` for import resolution. Added sign handling to
  number regex override.
- CSS values test: Same change to `BBNFToParserFromFile`. Removed hex color from
  test data (handled by css-color grammar, not css-values directly).
- CSS selectors test: Still uses `BBNFToParser` (standalone grammar, no imports).

## Verification

- 31 TS tests pass (+ 2 todo)
- 90 Rust tests pass (45 unit + 45 integration)
