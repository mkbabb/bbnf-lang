# latex-paper

Input LaTeX and beget navigable paper data.

`latex-paper` parses `.tex` and `.bib`, resolves labels and citations, builds a typed section tree, and can expose the result as a Vite virtual module. The Vue entry point adds the rendering and windowing primitives used by the paper view in [`fourier-analysis`](https://github.com/mkbabb/fourier-analysis).

See [docs/virtual-paper.md](docs/virtual-paper.md) for the end-to-end path.

## Install

```bash
npm install @mkbabb/latex-paper
```

## Entry points

| Export | Purpose |
| --- | --- |
| `@mkbabb/latex-paper` | Pure parser types and helpers |
| `@mkbabb/latex-paper/transform` | AST → `PaperSectionData[]` transform |
| `@mkbabb/latex-paper/vite` | Build-time virtual module for paper content |
| `@mkbabb/latex-paper/vue` | Vue components and virtual-window composables |
| `@mkbabb/latex-paper/theme` | Base paper styles |

## Core path

1. `parseLatex()` builds the AST.
2. `Transformer` turns that AST into `PaperSectionData[]`, interleaving prose, display math, theorems, and figures as `content` blocks.
3. `flattenPaperSections()` derives a stable depth-first list with hierarchy metadata and height estimates.
4. `useVirtualSectionWindow()` turns that flat list into a bounded render window with top and bottom spacers.
5. `latexPaperPlugin()` emits `paperSections`, `labelMap`, `pageMap`, `totalPages`, and `extractedMacros` through `virtual:paper-content`.

The page map is ordered, not title-matched. It is built from LaTeX TOC artifacts, so math-heavy headings no longer fall back to page `1`.

## Quick start

```ts
import latexPaperPlugin from "@mkbabb/latex-paper/vite";

export default {
    plugins: [
        latexPaperPlugin({
            texPath: "paper/main.tex",
            bibPath: "paper/refs.bib",
        }),
    ],
};
```

```ts
import { paperSections, labelMap, pageMap, totalPages, extractedMacros } from "virtual:paper-content";
```

## Vue primitives

**Components**—`PaperSection`, `PaperSectionBlocks`, `PaperSectionContent`, `MathBlock`, `MathInline`, `Theorem`, `CodeBlock`. These cover headings, section bodies, display/inline math, theorems, and code listings.

**Composables**—`usePaperReader`, `useVirtualSectionWindow`, `useSidebarFollow`, `useKatex`, `flattenPaperSections`. Reader setup, virtual windowing, sidebar scroll tracking, KaTeX rendering, and tree flattening.

**Tracking primitives**—`useLazyLoader`, `useTreeIndex`, `useScrollTracker`, `useScrollTo`, `useClickDelegate`. Generic scroll and intersection utilities used by the composables above.

**Context**—`PAPER_CONTEXT` and `createRenderTitle` for dependency injection across the component tree.

## Output shape

```ts
interface PaperSectionData {
    id: string;
    number: string;
    title: string;
    sourceLevel?: number;
    starred?: boolean;
    content: ContentBlock[];
    theorems?: PaperTheoremData[];
    figures?: PaperFigureData[];
    codeBlocks?: PaperCodeBlockData[];
    proofs?: PaperProofData[];
    subsections?: PaperSectionData[];
    callout?: { text: string; link: string };
    summary?: string;
}
```

`ContentBlock` is one of:

- paragraph HTML string
- `MathBlockData`—display equation
- `TheoremBlock`—theorem, definition, lemma, etc.
- `FigureBlock`—figure with caption
- `CodeBlock`—code listing
- `ProofBlock`—proof body

## Notes

- KaTeX is optional at the package level and used by the Vue entry point.

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md). The README shape follows the perimeter-level
[canonical README shape](https://github.com/mkbabb/glass-ui/blob/master/docs/precepts/canonical-readme-shape.md).

## License

[MIT](./LICENSE) © 2026 Mike Babb.
