# VIRT scope amendment — the glass-ui twin (2026-07-02)

> **Owner directive (mid-pass-1):** "Does glass-ui not already have something like this, too, with our
> TOC and search items? Bring those up to spec, too. Properly abstract between the two." This amendment
> records the verified duplication and binds the loop's remaining passes to the expanded scope. Pass 1's
> `SPEC-PASS-1.md` was synthesized BEFORE this reveal — read its architecture decision as **provisional
> until the abstraction fork below is settled on evidence**.

## The verified duplication (measured on disk, 2026-07-02)

Two published forks of one virtualization/scroll-tracking core, diverged:

| Surface | `@mkbabb/latex-paper` v0.2.1 | `@mkbabb/glass-ui` v4.2.0 |
|---|---|---|
| `useVirtualSectionWindow.ts` | `src/vue/composables/` — **261 L**, coupled to `FlatPaperSection` | `src/composables/virtual/` — **357 L**, generic, uses its own `dom/useResizeObserver` |
| `virtualSectionLayout.ts` | **161 L**, `FlatPaperSection`-typed | **263 L**, generic + documented pure-layout header |
| tracking / sidebar suite | `src/vue/tracking/`: `useScrollTracker` (257 L), `useScrollTo`, `useLazyLoader`, `useClickDelegate`, `useTreeIndex`, `types` | `src/composables/sidebar/`: same-named files **plus `useSidebarFollow`** — every file differs from its twin |
| publish vehicle | `@mkbabb/latex-paper/vue` (one barrel) | subpaths `./virtual`, `./sidebar`, `./search` |
| search | (none — fourier-analysis hand-rolls `usePaperSearch` consumer-side) | `components/custom/search/`: `FuzzySearch.vue`, `SearchBar.vue`, composables, `searchVariants.ts` (365 L) |

Consumers split across the forks: **fourier-analysis** `PaperView.vue` imports `useVirtualSectionWindow`,
`useSidebarFollow`, `useClickDelegate`, `useTreeIndex` from `@mkbabb/latex-paper/vue` (verified,
`PaperView.vue:1–12`); consumers of glass-ui's `./virtual`/`./sidebar`/`./search` are **unmapped** — a
required pass-2 research finding (zero hits in a first grep over atlas/fourier-analysis/keyframes demo;
if glass-ui's fork has no consumer, that inverts the consolidation direction — precepts: a public surface
needs a current consumer).

## The expanded goal

ONE headless virtualization + scroll-tracking core at ONE root, brought to SOTA
(TanStack-Virtual-class), consumed by BOTH libraries; glass-ui's TOC/sidebar-follow and search surfaces
brought up to the same spec alongside latex-paper's paper window. No twin files remain anywhere in
either repo at tranche close (a gate-checkable condition: the twin filenames diff to *deleted*, one
owner survives).

## The abstraction fork (first-class, settle on evidence — pass 2)

- **(a) glass-ui owns the core** — its fork is already the generic one; latex-paper consumes
  `@mkbabb/glass-ui/{virtual,sidebar}` and keeps only paper semantics (`FlatPaperSection` adapters).
  Cost: latex-paper (3 runtime deps today) gains glass-ui as a dep — weigh dep-weight vs a peerDep vs
  subpath tree-shaking reality.
- **(b) a new tiny headless package** (`@mkbabb/virtual-core`-shaped) both consume — TanStack's own
  shape (headless core + thin adapters). Cost: a third repo/publish pipeline; precepts overfitting check
  (does a third consumer exist to justify it?).
- **(c) latex-paper owns, glass-ui consumes** — inverts the platform relationship; likely wrong (glass-ui
  is the shared UI platform) but must be refuted on evidence, not vibes (note: fourier-analysis, the only
  mapped consumer, imports from latex-paper today).
- Whichever wins: the SOTA engineering (measurement cache, scroll anchoring, semantics arms) lands ONCE
  at the owning root; the loser's fork is deleted and re-exported/consumed, never kept as a shim
  (no-legacy precept).

## Pass-2 binding additions

1. **R9-glass-ui research lane:** dissect glass-ui's `composables/virtual` + `composables/sidebar` +
   `search` component suite line-by-line vs latex-paper's twins (what diverged and why — the glass-ui
   fork's extra 200 lines are presumably the more evolved algorithm); map ALL consumers of the glass-ui
   subpaths across the portfolio (`~/Programming/*`); read glass-ui's tranche docs for the fork's
   provenance (which tranche copied which direction).
2. **The abstraction fork** enters the spec as the FIRST architecture decision (before the
   TanStack-vs-modernize-vs-native fork, which becomes a decision *about the shared core*).
3. **Search joins the requirement set:** glass-ui's FuzzySearch/SearchBar + fourier-analysis's
   consumer-side `usePaperSearch` → one search-over-virtualized-content story (find-in-page interplay,
   result navigation into unmounted windows) at the owning root.
4. **Prototypes** must include the extraction: the shared headless core carved out with both libraries'
   surfaces re-expressed over it (compile + tests green in both clones).
5. **Consumer contract grows:** fourier-analysis (latex-paper path), vft V4 (latex-paper path), any
   glass-ui-subpath consumers found by R9, and the atlas dock-TOC seam (`DockTocView` — the sci-report
   L-arc offers `glass-ui Drawer` for its a11y; the TOC *content* machinery is this core).
6. **Root-repo law:** each repo's work lands at its own root, published via its own vehicle (glass-ui
   semver cut; latex-paper changesets CI); the tranche's wave specs name owner + vehicle per wave.
