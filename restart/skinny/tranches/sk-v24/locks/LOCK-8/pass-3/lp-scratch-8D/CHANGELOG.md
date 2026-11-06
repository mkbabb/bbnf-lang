# Changelog

## 0.2.1 — 2026-05-28 (G.W5 cohort)

The current published version, seeded as the initial CHANGELOG entry as part of the
muster tranche G release-engineering wave (G.W5 sub-wave D). Future entries accrete
from changesets.

`@mkbabb/latex-paper@0.2.1` ships the LaTeX → navigable-paper pipeline across five
entry points (`/`, `/transform`, `/vite`, `/vue`, `/theme`): `parseLatex()` AST build,
the `Transformer` AST → `PaperSectionData[]` transform, `flattenPaperSections()`
depth-first flattening, the `useVirtualSectionWindow()` bounded render window, and the
`latexPaperPlugin()` Vite virtual module emitting `paperSections` + `labelMap` +
`pageMap` + `totalPages` + `extractedMacros` through `virtual:paper-content`. The Vue
entry adds the rendering + windowing primitives (`PaperSection`, `MathBlock`,
`Theorem`, `useKatex`, `useSidebarFollow`, …). The page map is built from LaTeX TOC
artifacts — ordered, not title-matched. Consumed by `fourier-analysis` + `words`
through the npm registry.
