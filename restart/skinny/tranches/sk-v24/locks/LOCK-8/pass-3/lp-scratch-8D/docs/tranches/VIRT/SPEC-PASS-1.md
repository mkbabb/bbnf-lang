# VIRT — SOTA Window-Virtualization Pass — SPEC (PASS 1)

Status: PASS 1 of the convergence loop. Synthesized 2026-07-02 from research dossiers
R1 (TanStack Virtual internals), R2 (2025-2026 SOTA survey), R4 (math-rendering ×
measurement stability), R5 (consumer contracts), R6 (document semantics + a11y),
R7 (tranche/plan-shape + release infra), R8 (Vue-3 idioms). R3 (the dedicated
gap-analysis lane) returned a dud payload; its ground was re-covered from the code
reads embedded in R1/R4/R8 and from direct file verification during this pass —
see Open Fork F-8.

Target: `@mkbabb/latex-paper` v0.2.1 → v0.3.0, repo `/Users/mkbabb/Programming/latex-paper`
(HEAD `4775845`, clean). All work lands at the library root (ROOT-REPO LAW), published
via the changesets CI vehicle.

Every requirement is numbered `VR-n` for citation by critique lanes and later passes.
Open forks are numbered `F-n` and seed the PASS-2 prototype fleet.

---

## 1. The SOTA definition and the gap analysis

### 1.1 What SOTA means for THIS library

TanStack Virtual is the user-named reference, and R1 confirms it is the best-engineered
JS windowing core in existence (binary-search range extraction, versioned measurement
cache with a documented 1382× rebuild speedup, per-item ResizeObserver, above-viewport
scroll-delta compensation, 10-rAF scrollToIndex reconciliation). But R2's ten-point SOTA
definition and R6's five problem classes converge on a sharper claim: for a *document* —
as opposed to a feed or a data grid — SOTA is not "the best windowing math." It is the
full preservation of document semantics at rendering cost that scales sub-linearly with
document length. Concretely:

- **VR-1** Dynamic per-item height with zero upfront declarations, stable under KaTeX
  font arrival, image load, and container-width change (R2 §1, R4 all findings).
- **VR-2** No content jump when off-screen content above the viewport changes size
  (R2 §2 — the `shouldAdjustScrollPositionOnItemSizeChange` class of correctness).
- **VR-3** Reliable programmatic navigation to any section id (TOC click, citation,
  deep URL), settling across layout churn (R2 §3).
- **VR-4** Browser find-in-page (Cmd+F) works across the ENTIRE document (R2 §4, R6 §2).
- **VR-5** Native `#fragment` and `:~:text=` navigation resolves without JS interception
  (R2 §5, R6 §4).
- **VR-6** The full heading/landmark tree stays in the accessibility tree; screen-reader
  rotor and heading nav see the whole paper (R2 §6, R6 §1).
- **VR-7** `window.print()` produces the complete document (R6 §5).
- **VR-8** Reader mode (Readability-class extraction) captures the whole article (R6 §7).
- **VR-9** CSS scroll-driven animations (view-timeline reveals — the vft Beat register)
  remain possible on sections (R2 §8; motion canon P1-P6).
- **VR-10** Rendering work (layout + paint + Vue patch) for off-screen sections is
  deferred, keeping initial paint and scroll frame budget flat as documents grow.

A DOM-unmounting JS virtualizer — TanStack included — satisfies VR-1..3 and VR-10 and
*structurally breaks* VR-4..9 (R2 §9 pt.10; R6 §9: TanStack ships zero ARIA, zero print,
zero find-in-page, zero focus management; issue #158 open since 2021). The browser-native
primitive `content-visibility: auto` + `contain-intrinsic-size: auto Npx` satisfies
VR-4..9 *natively* and delivers the layout/paint half of VR-10, is Baseline Newly
Available since 2025-09-15 (~92% global), and the one historical blocker — Safari
find-in-page over skipped content, WebKit bug 283846 — was RESOLVED FIXED 2025-03-30
(R2 §5, R6 §2). Scale threshold: R2 places the crossover where JS unmounting becomes
*necessary* at ~500 sections / very large per-section DOM. Both live consumers are
far below it (a Fourier-analysis paper and the vft statistics propaedeutic — O(10–60)
sections each).

**VR-11** The SOTA target for latex-paper is therefore: all ten properties
simultaneously, at the O(10–200)-section scale of its actual consumers, with the
smallest architecture that gets there. "Beats TanStack" here means: TanStack-grade
measurement/navigation correctness (VR-1..3) *plus* the six document-semantics
properties TanStack cannot offer (VR-4..9).

### 1.2 Gap analysis vs the existing 1,098-line layer

Verified against the working tree this pass (file:line cites from
`src/vue/composables/useVirtualSectionWindow.ts` [261 L], `virtualSectionLayout.ts`
[161 L], `src/vue/tracking/*` [588 L across 7 files]):

| # | Gap | Where | Violates |
|---|-----|-------|----------|
| G1 | One-shot rAF `offsetHeight` read on mount; no per-element ResizeObserver, so KaTeX font arrival, late images, and highlight reflows are never re-measured | `useVirtualSectionWindow.ts:140-158` | VR-1 (R1 §11, R4 §1-2) |
| G2 | No above-viewport size-delta scroll compensation — content jumps on remeasure | whole layer (no analog to `applyScrollAdjustment`) | VR-2 (R1 §11) |
| G3 | `SESSION_HEIGHT_CACHE` is a module singleton keyed by bare section id: cross-document contamination in the multi-paper atlas, and no width key, so zoom / dock-collapse / breakpoint changes silently corrupt every cached height | `useVirtualSectionWindow.ts:32,70-76` | VR-1 (R4 §3, R8 §10) |
| G4 | No font-ready gate: measurements taken during the KaTeX FOUT window are cached as ground truth for the session | `useKatex.ts` (sync `renderToString`), no `document.fonts` use anywhere in `src/` | VR-1 (R4 §1) |
| G5 | `useScrollTo` retries `getBoundingClientRect()` up to 60 rAFs (reflow per attempt) instead of layout math; no hashchange/popstate listener anywhere — external deep links silently fail | `useScrollTo.ts:32-63` | VR-3, VR-5 (R1 §7, R6 §4) |
| G6 | Sections outside the window UNMOUNT: find-in-page, text fragments, reader mode, and the AT heading tree all see only the current slice | the spacer/slice model itself | VR-4..8 (R6 §1-2,7) |
| G7 | `PaperSection.vue` collapses all sub-levels to h3 (`depth > 0 ? 'h3' : 'h2'`); no DPUB-ARIA roles | `PaperSection.vue:26` | VR-6 (R6 §1) |
| G8 | `behavior:'smooth'` unconditional in `useScrollTo.ts:54` and `useScrollTracker.ts:202` — no `prefers-reduced-motion` gate | tracking layer | motion canon / WCAG 2.3.3 (R6 §8) |
| G9 | No print handling of any kind | — | VR-7 (R6 §5) |
| G10 | `findSectionOffset` is an O(n) scan per navigation lookup | `virtualSectionLayout.ts:151-160` | perf hygiene (R8 §9) |
| G11 | `useScrollTracker` resolves active section via `document.getElementById` — silently blind to any section the virtualizer has unmounted; duplicates the layout-based `resolveActiveSection` already in `useVirtualSectionWindow` | `useScrollTracker.ts:112,140-152` | correctness (R8 §11) |
| G12 | `estimatedHeight` heuristics are width-blind (hardcoded 180 chars/line) and systematically undershoot display math | `flattenPaperSections.ts:34-64` | VR-1 seeding (R4 §4) |
| G13 | 320 ms magic `warmTimer` where `scrollend` (+ debounce fallback) is the modern signal | `useVirtualSectionWindow.ts:119-125` | hygiene (R8 §8) |
| G14 | Manual rAF/observer cleanup in `onUnmounted` predating Vue 3.5 `onWatcherCleanup`/`useTemplateRef`; no `v-memo` guard on section roots (KaTeX subtrees re-diff on every slice change) | composables + components | hygiene (R8 §5,7) |

G6 is the structural gap: it cannot be patched inside the unmount model — it is the
unmount model. That fact drives §2.

---

## 2. Architecture decision

### 2.1 The decision

**VR-12** latex-paper v0.3.0 adopts the **DOCUMENT-NATIVE architecture**: every section
stays mounted in the DOM; rendering cost is deferred by the browser via
`content-visibility: auto` + `contain-intrinsic-size: auto var(--lp-section-size)` on
each section wrapper; a slim library-owned viewport core (`usePaperViewport`, §6)
supplies the four things CSS cannot — active-section resolution, deep-link/hash
navigation, settle/measurement hygiene, and print/PRM arms. The JS windowing layer
(`useVirtualSectionWindow` + `virtualSectionLayout`, 422 lines, plus the spacer
render-host contract) is **retired, not modernized**.

**VR-13** Initial Vue render cost (the one cost `content-visibility` cannot defer —
R2 §7: "the framework still processes all components") is governed by the retained
progressive-mount layer (`useLazyLoader`, generalized into the mount governor, §6.4)
*only if* the F-1 benchmark shows full eager mount blowing the budget on the largest
live document. The governor differs from virtualization in kind: it only ever mounts
*forward* (monotone reveal, nothing unmounts), so once the document is fully revealed
VR-4..9 hold unconditionally; a `revealAll()` escape (used by print, find/hash targets
past the frontier, and `document.fonts.ready` idle time) closes the pre-reveal window.

### 2.2 Why not the alternatives

**Adopt `@tanstack/virtual-core` — rejected.**
- It solves the wrong layer: its excellence is windowing math (VR-1..3 mechanics), and
  the windowing model itself is what breaks VR-4..9 for a document (R2 §9, R6 §9).
  Adopting it would obligate us to *rebuild* find-in-page (custom search index + UI),
  hash interception, print expansion, ARIA scaffolding, and focus management on top —
  hundreds of consumer-visible lines to recover what the browser gives the native path
  for free.
- Dependency cost: runtime deps go 3 → 4 for a library whose measured need (≤200
  sections) sits an order of magnitude below the windowing crossover. KISS/no-overfitting
  fails this.
- Template break: TanStack's absolute-position/translateY model forces every consumer to
  abandon the spacer template (R1 implication 2) — a migration cost with no consumer-
  visible benefit at this scale.
- What we DO take from TanStack (transposed, not imported): the shared-ResizeObserver
  measurement discipline, `data-*` index attribution, the settle-loop idea behind
  scrollToIndex reconciliation, and `scrollend`-based scroll-state (R1 §4,7; R8 §3,8).

**Modernize the hand-rolled windowing core — rejected.** Closing G1–G5, G10–G13 inside
the unmount model still leaves G6 (VR-4..9) structurally open, and R6's arms for the
unmount path (`hidden="until-found"` + beforematch, roving tabindex, beforeprint
expansion, aria-live announcers, hashchange force-mount) are a large permanent surface —
and `hidden="until-found"` keeps nodes in the DOM anyway, so it does not even recover
the Vue render cost that is unmounting's one advantage. Maximum machinery, minimum win.

**Pure browser-native with no JS at all — insufficient alone.** CSS cannot provide
activeId scroll-follow for the TOC, settle-aware `scrollTo(id)`, the fonts-ready gate,
or the initial Vue render governor. The native path needs the slim core; that core is
this tranche's real deliverable.

**Dependency arithmetic:** native path adds **zero** runtime dependencies (stays at 3);
TanStack adds one plus consumer template rewrites. The native path *deletes* ~420 lines
of layout math and replaces them with ~150 lines of viewport core + ~30 lines of CSS.
Smallest architecture that reaches SOTA: native.

**VR-14** Contingency, recorded now so later passes need not relitigate: if F-1 falsifies
the render-budget premise even with the mount governor (i.e., a live consumer document
where governed eager mounting cannot hold 60fps scroll + <200 ms initial-render on a
mid-tier device), the fallback is the R6 PATH-B hybrid (`hidden="until-found"` on
far-frontier sections with beforematch reveal), NOT TanStack — it preserves VR-4..5
while deferring Vue patch work. This fork stays open only until the F-1 benchmark lands.

---

## 3. The measurement model

The native architecture inverts the measurement problem: the browser owns true layout
(all sections mounted, `offsetTop` is always real), so JS measurement stops being the
scroll-math substrate and becomes (a) the intrinsic-size seed and (b) the activeId /
navigation input. The R4 epoch discipline still binds.

- **VR-15** `estimatedHeight` (from `flattenPaperSections`) seeds
  `contain-intrinsic-size: auto <estimatedHeight>px` per section wrapper (as an inline
  custom property `--lp-section-size`). The `auto` keyword makes the browser remember
  the last *rendered* height thereafter — the browser becomes the measured cache, with
  correct per-width invalidation for free (retires G3's width-key problem for layout
  purposes).
- **VR-16** `estimatePaperSectionHeight()` gains a `containerWidth` parameter:
  chars-per-line = `floor(width / 8)` replacing the hardcoded 180; display-math floor
  becomes `max(104, 60 + 20·operatorClusters)` per R4 §4. Estimates only have to be
  good enough to prevent scrollbar jitter and mis-landed anchors *before first render
  of a section* (R6 §3); after first render the `auto` memory takes over. (F-4 tests
  whether this refinement is even load-bearing.)
- **VR-17** One shared per-instance ResizeObserver observes every mounted section root
  (registered via the `registerSection` ref callback carrying `data-lp-id`), feeding a
  per-instance `Map<string, number>` measurement registry — TanStack's centralized
  `measureElement` discipline transposed (R1 §4, R8 §3). This registry serves activeId
  resolution and `getOffsetFor` staleness checks only; it is NOT a scroll-math cache.
  The module-level `SESSION_HEIGHT_CACHE` singleton is deleted (G3).
- **VR-18** Invalidation epoch (R4 implication 7): mount → RO initial size → registry
  write → font-ready / image-load reflows arrive as further RO callbacks and simply
  overwrite. No cache survives instance disposal; cross-session persistence is dropped
  (the browser's `auto` memory + estimates make it redundant — evidence: F-4).
- **VR-19** Math/font stability: the viewport core exposes `whenSettled(): Promise<void>`
  gating on `document.fonts.ready` (plus, when available, `document.fonts.load('1em KaTeX_Main')`
  raced with a 3 s ceiling). Initial hash restoration, `scrollTo` fine-alignment, and the
  scroll-position restore all await it. KaTeX HTML itself is synchronous (`renderToString`,
  module-cached in `useKatex`), so post-font reflow is the only async math event (R4 §1).
- **VR-20** Figure images: `PaperSectionBlocks` emits `width`/`height` (or
  `aspect-ratio` style) whenever the transform pipeline knows intrinsic dimensions, and
  `loading="lazy" decoding="async"` otherwise; unknown-dimension images reflow under the
  RO epoch (VR-18) rather than under a one-shot read (closes R4 §5's cache-poisoning,
  and browser scroll anchoring — `overflow-anchor` default — absorbs the shift per VR-21).
- **VR-21** VR-2 (no content jump) is served natively: with all sections mounted and
  sized, the browser's built-in scroll anchoring compensates for above-viewport growth.
  The library must not defeat it — the theme must not set `overflow-anchor: none`, and
  the scroll container must avoid `overflow: hidden` (use `clip`/`auto`; also required
  by scroll-driven animations, R2 §8). A regression test (Playwright: grow an
  above-viewport image, assert viewport text is stationary) gates this.

---

## 4. The document-semantics arms

Each arm is a mechanism, not a vow. All are near-free under the native architecture —
that is the point of §2.

- **VR-22 Find-in-page.** Mechanism: nothing to build — all text is in the DOM;
  `content-visibility: auto` content is findable by spec, browser renders the match
  target just-in-time. Gate: Playwright probe driving window.find/Cmd+F-equivalent
  across Chromium + WebKit asserting a match in the LAST section of the fixture paper
  scrolls into view. Pre-fix-Safari (≤ the 2025-03-30 WebKit fix) fallback decision: F-2.
  Under the mount governor, a find target beyond the reveal frontier is unreachable —
  therefore `whenSettled()` triggers `revealAll()` during idle time (VR-13), bounding
  the non-findable window to the first seconds of page life.
- **VR-23 Anchors / deep-links.** Mechanism: native `#id` and `:~:text=` resolution
  works because targets exist in the DOM with accurate intrinsic sizes (VR-15 keeps
  cumulative offsets honest — R6 §3). Additionally the library ships
  `useHashNavigation` (§5): `hashchange` + `popstate` listeners → `revealTo(id)` past
  the governor frontier if needed → `scrollTo(id)` with settle-await (VR-19) and a
  bounded fine-alignment loop (≤10 rAF, layout-read based — TanStack's reconciliation
  idea without the reflow-per-attempt of the current `useScrollTo`, closing G5).
- **VR-24 Print.** Mechanism: theme ships
  `@media print { .paper-article [data-lp-section] { content-visibility: visible !important; contain-intrinsic-size: none; } }`
  plus container `overflow: visible; height: auto`. The viewport core adds a
  `beforeprint` listener calling `revealAll()` synchronously (KaTeX is sync, so this is
  safe — R6 §5; beforeprint fires before print CSS applies, so the order is correct).
  Gate: print-to-PDF artifact of the fixture paper shows first AND last section content.
- **VR-25 SSR.** Declared out of scope for v0.3.0 with rationale: both live consumers
  are client-only SPAs (R8 §6); no consumer = no surface (no-overfitting law). The
  native architecture is SSR-*compatible* by construction (full DOM, no client-only
  layout math needed for first paint), so nothing in this tranche forecloses a later
  static-prerender wave. Recorded as ARCHIVE, not deferral.
- **VR-26 A11y — heading tree.** `PaperSection.vue` derives its heading tag from
  `sourceLevel` (clamped h1–h6) instead of the depth>0→h3 collapse (G7).
- **VR-27 A11y — document roles.** `<section role="doc-section">` on section wrappers
  (DPUB-ARIA 1.1); the consumer-facing guidance names `<main aria-label>` for the
  article host and mandates that the TOC be a persistent, never-virtualized `<nav>`
  with `aria-current` on the active item (R6 §1; both consumers' TOCs already comply
  structurally).
- **VR-28 A11y — motion.** All programmatic smooth scrolls (viewport core,
  `useSidebarFollow`, any retained tracker) gate `behavior` on
  `matchMedia('(prefers-reduced-motion: reduce)')` → `'instant'`; theme adds the
  `@media (prefers-reduced-motion: reduce) { scroll-behavior: auto !important }`
  CSS arm (G8; motion canon PRM arm).
- **VR-29 A11y — focus.** With no unmounting, Tab order and focus survival are native.
  Under the mount governor, unrevealed sections simply do not exist yet at the tab
  frontier (monotone reveal means focus can never be *destroyed* by the library). No
  roving tabindex, no `inert` management required — assert with an axe + keyboard-walk
  probe in the π lane.
- **VR-30 Reader mode / crawlers.** Full DOM satisfies Readability-class extractors
  natively (R6 §7). Gate: Firefox reader-view probe on the fixture paper extracts
  ≥95% of section text.

---

## 5. The public API surface for v0.3.0

### 5.1 Semver honesty

**VR-31** npm caret semantics for 0.x pin the minor: `^0.2.1` resolves to `<0.3.0`.
(R5's claim that caret admits 0.3.0 is incorrect and is hereby corrected.) Therefore
v0.3.0 is an **explicit opt-in upgrade** — no consumer is auto-broken, and the release
may change the virtualization seam without a shim. Per the no-legacy-compat precept,
`useVirtualSectionWindow`, `VirtualSectionWindowOptions`, and `virtualSectionLayout`
are **removed** in 0.3.0, not deprecated-and-kept: preserving `topSpacerPx`/
`bottomSpacerPx` as permanent zeros would be a stub. The changeset is authored as
**minor** with a MIGRATION section naming every removed symbol and its replacement.

**VR-32** Byte-compatible (the R5 floor, everything a consumer imports today except the
retired seam): all `.` types (`PaperSectionData`, `ContentBlock`, `MathBlockData`,
`PaperTheoremData`, `PaperFigureData`, `PaperProofData`, `PaperCodeBlockData`,
`PaperLabelInfo`); `/vue` exports `useKatex`, `PAPER_CONTEXT` + `PaperContext`,
`createRenderTitle`, `flattenPaperSections` + `FlatPaperSection`, `usePaperReader`,
`useSidebarFollow` + `SidebarFollowOptions`, `useClickDelegate`, `useTreeIndex` +
`TreeNode`, `useScrollTracker`, `useScrollTo`, `useLazyLoader`, and all seven
components + `PaperSectionContent`; the `/vite` plugin and the `virtual:paper-content`
shape (`paperSections`, `labelMap`, `totalPages`, `pageMap`, `extractedMacros`);
the `/theme` entry. Any field added to `PaperContext` is optional (R5 §3).

**VR-33** Breaks, exhaustively: (a) `useVirtualSectionWindow` /
`VirtualSectionWindowOptions` removed → `usePaperViewport`; (b) the spacer render-host
template contract (topSpacer/bottomSpacer divs + `visibleItems` slice) removed → render
ALL sections, wrapper carries `:ref="registerSection"` + `data-lp-id` + the CV class;
(c) internal `virtualSectionLayout` module removed (was exported only transitively —
verify none of its types leak through `dist/` before close). Migration cost is a net
*deletion* in fourier's `PaperArticleWindow` (spacers, measure plumbing, slice logic
all go away); vft is greenfield. Substrate + consumer land together (W5).

### 5.2 The new surface

```ts
// /vue — the viewport core (replaces useVirtualSectionWindow)
export interface PaperViewportOptions<T extends FlatPaperSection = FlatPaperSection> {
    items: MaybeRefOrGetter<readonly T[]>;
    /** null | HTMLElement ref => element scroller; 'window' => document scroller. */
    scrollContainer: Ref<HTMLElement | null> | "window";          // F-6
    /** px between scroller origin and the article top (scrollMargin semantics). */
    leadingOffsetPx?: MaybeRefOrGetter<number>;
    /** Mount governor. 'eager' mounts everything up front. Default settled by F-1. */
    mount?: "eager" | { batchSize?: number; idleRevealAll?: boolean };
    /** Fraction of viewport height used as the active-section probe line. */
    activeLine?: number; // default 0.2 (matches current behavior)
}

export interface PaperViewport<T extends FlatPaperSection = FlatPaperSection> {
    /** Ref callback for each section root: <section :ref="el => registerSection(item.id, el)" :data-lp-id="item.id"> */
    registerSection: (id: string, el: HTMLElement | null) => void;
    /** Sections the mount governor has revealed (=== items when eager/complete). */
    mountedItems: ComputedRef<readonly T[]>;
    activeId: ComputedRef<string | null>;
    activeRootId: ComputedRef<string | null>;
    getOffsetFor: (id: string) => number | null;   // O(1); real DOM offsetTop
    scrollTo: (id: string, opts?: { align?: "start" | "center"; behavior?: ScrollBehavior }) => Promise<void>; // PRM-gated, settle-aware
    revealTo: (id: string) => void;                // governor frontier advance
    revealAll: () => void;
    whenSettled: () => Promise<void>;              // fonts + pending RO flush
    isScrolling: Readonly<Ref<boolean>>;           // scrollend-derived
}

export function usePaperViewport<T extends FlatPaperSection>(
    options: PaperViewportOptions<T>,
): PaperViewport<T>;

// /vue — deep links (new; closes G5's missing listener)
export function useHashNavigation(viewport: PaperViewport, opts?: {
    resolveId?: (hash: string) => string | null;   // default: identity on decodeURIComponent
}): { dispose: () => void };
```

- **VR-34** `usePaperViewport` is the single virtualization-era entry point; its outputs
  are all `computed()` derivations of one `shallowRef` core state with a centralized
  change hook (`triggerRef`) — the TanStack/Vue reactivity bridge transposed (R8 §1-2).
- **VR-35** `scrollToId` on `PaperContext` becomes library-fulfilled: `usePaperReader`
  and the components wire it to `viewport.scrollTo` so consumers stop hand-rolling
  `navigateTo` (R5 §5 — the ≥2-consumer promote test passes for scroll-to; fourier's
  teleport-overlay/nav-stack, search, progress bar, and sidebar chrome all stay
  consumer-side — single-consumer presentation, no-overfitting).
- **VR-36** `useScrollTracker`/`useScrollTo`/`useLazyLoader`/`usePaperReader` remain
  exported and functional (VR-32) but `usePaperReader` internally rebases on
  `usePaperViewport` (layout-truth activeId replaces the getElementById scan as the
  authority — G11; IntersectionObserver stays as in-DOM precision enhancement).
  `useTreeIndex`'s existing `getChildren` override is documented as the
  `subsections→children` adapter so consumers can delete their `paperTree` shims (R5 §5).

### 5.3 The theme token seam (the vft blocker)

**VR-37** `theme.css` hard-wraps every token in `hsl(var(--x))`; the vft earth palette
is authored in `oklch()` — wrapping oklch in hsl() is invalid CSS, so the designed
re-skin cannot work today (R5 §4). Mechanism: every color consumption in `theme.css`
becomes a two-tier custom property —
`color: var(--lp-foreground, hsl(var(--foreground)))` — introducing the `--lp-*`
namespace (`--lp-foreground`, `--lp-primary`, `--lp-border`, `--lp-muted`,
`--lp-muted-foreground`, `--lp-card`, `--lp-section-heading`, `--lp-accent`,
`--lp-radius`, `--lp-section-size`) that accepts ANY full color value (oklch included),
falling back to the legacy HSL-triple contract. Additive, byte-compatible with
fourier's existing tokens; vft themes by setting `--lp-*` on its article scope. The
token table is documented in `docs/theming.md`.

### 5.4 Release vehicle

**VR-38** Per R7: author a `minor` changeset in the PR; merge to `master`; the
changesets bot cuts the Version Packages PR; merging tags `v0.3.0`; `release.yml`
publishes with `npm ci && npm run build && npm test`. No dev-machine publish. Gate
commands for every implementation wave: `npm run build` (tsup + vue-tsc IS the type
gate) and `npm test` (vitest); docs waves close on `git diff --check`.

---

## 6. The Vue architecture

- **VR-39 Single reactive root.** One `shallowRef` holding the viewport core state;
  a single internal `onChange()` fires `triggerRef`; every public output is a
  `computed()` off it. The scattered `scheduleRecalculate()` callsites disappear (R8 §1).
- **VR-40 Three injected seams.** The core is written against a tiny adapter interface
  `{ observeRect, observeOffset, scrollTo }` with an element implementation and a
  window implementation, making `scrollContainer: 'window'` a one-arg switch instead of
  a wrapper-div anti-pattern (R1 §1, R8 §2,4). `leadingOffsetPx` keeps its scrollMargin
  semantics in both modes.
- **VR-41 Scroll scheduling.** Passive scroll listener, rAF-coalesced state write;
  `isScrolling` derived from native `scrollend` with a 150 ms debounce fallback for
  engines below Safari 17.4 (kills the 320 ms `warmTimer` constant — G13, R8 §8).
- **VR-42 Vue 3.5 hygiene.** `onWatcherCleanup` owns every observer/rAF/listener
  disposal inside the effect that created it (no manual `onUnmounted` blocks);
  `useTemplateRef` for library-owned element refs; reactive props destructure in
  components (R8 §7).
- **VR-43 Render-cost discipline.** `v-memo="[item.id]"` co-located with the section
  `v-for` in the consumer-facing template guidance and in `PaperSectionBlocks`' internal
  block loop where content is static-per-id; `v-once` on pure-static KaTeX block
  interiors. `KeepAlive` is explicitly rejected for this pattern (R8 §5) — and under
  the native model nothing unmounts on scroll anyway, so KaTeX renders exactly once
  per section per document lifetime.
- **VR-44 O(1) lookups.** Section-id → element and id → index maps per instance;
  `getOffsetFor` reads live `offsetTop` (always correct under full mount) with the
  measurement registry as its staleness signal — `findSectionOffset`'s O(n) scan and
  the whole offset-math layer are deleted (G10).
- **VR-45 CSS substrate ownership.** The `content-visibility: auto` +
  `contain-intrinsic-size: auto var(--lp-section-size, 640px)` rule ships in
  `theme.css` under a `data-lp-section` attribute selector, so consumers who bypass
  `/theme` can reproduce it from the documented contract; `PaperSection.vue` emits the
  attribute + inline `--lp-section-size` itself. Sticky elements must not live inside
  a CV wrapper (containment breaks sticky — R2 §5); the docs name this trap.
- **VR-46 Tests.** Vitest: viewport-core unit tests (governor frontier monotonicity,
  activeId resolution against synthetic layouts, PRM gating, hash resolution). New
  Playwright fixture app (a generated ~120-section stress paper + the real fourier
  paper) drives the runtime gates in VR-21/22/24 and the benchmarks in F-1.

---

## 7. Draft wave shape (the eventual VIRT tranche plan)

Per R7's plan-shape bar: every wave numbered AND named; every wave and agent unit gets
a goal criterion (forward) and completion criterion (evidence); ≤6 parallel agents
(≤3 concurrent dispatch per the concurrency law); zero-deferral; hard gates are
build/test/runtime/benchmark artefacts only. The full plan lands as
`docs/tranches/VIRT/VIRT.md` (first tranche folder in this repo — R7 §4) once the
convergence loop reaches 100. Draft:

| Wave | Name | Agents | Goal criterion | Closes on (evidence) |
|------|------|--------|----------------|----------------------|
| VIRT.W1 | Fork-Settling Prototype Fleet | 3 | Every §8 fork F-1..F-7 settled by artefact so W2 starts with zero design forks | Benchmark JSON + Playwright traces from scratchpad clones; per-fork verdict table appended to this spec's PASS-2 revision |
| VIRT.W2 | The Document-Native Substrate | 2 | `usePaperViewport` + CV/CIS theme substrate + measurement registry + font gate replace the windowing core in-repo | `npm run build` + `npm test` exit 0; 422 windowing-layer lines deleted (diff proof); VR-21 anchor-stability Playwright trace |
| VIRT.W3 | Document-Semantics Arms | 3 | Find-in-page, hash/text-fragment deep links, print, heading/DPUB a11y, PRM gates all pass runtime probes | Playwright find/anchor/print artefacts (VR-22/23/24); axe + keyboard-walk report (VR-26..29); reader-mode extraction (VR-30) |
| VIRT.W4 | Theme Token Seam + Surface Close | 2 | `--lp-*` two-tier tokens land; exports/docs (`virtual-paper.md` rewrite, `theming.md`) match the shipped surface exactly | Build+test exit 0; oklch smoke fixture renders (screenshot); doc-vs-`dist/` export reconciliation |
| VIRT.W5 | Consumer Migrations (substrate+consumer land together) | 2 | fourier `PaperView`/`PaperArticleWindow` migrated (net deletion); vft V4 binding verified against 0.3.0-pre | fourier build+runtime probe on its real paper; vft dock-TOC/deep-link/earth-theme probe; consumer LOC diff |
| VIRT.W6 | Release + π Close | 2 (+π lane) | v0.3.0 cut via changesets CI; before/after benchmark ledger published | Minor changeset merged; `v0.3.0` tag + npm publish CI run green; π visual-runtime artefacts (≥3 viewports, PRM on/off, contrast); frame-budget before/after table |

Coordination: `docs/tranches/VIRT/coordination/` opens if the vft V4 wave runs
concurrently in sci-report (R7 §5). The π lane is binding (render-path change);
no tooling-contingency is claimed.

---

## 8. OPEN FORKS (seed the PASS-2 prototype fleet)

Each fork names the evidence that settles it. Nothing else in this spec is open.

- **F-1 — The render-budget premise (governs VR-12/13/14).** Does eager full mount of
  the largest live consumer document (fourier's paper; plus the 120-section stress
  fixture) hold <200 ms Vue render + first-paint budget and 60fps scroll on a 4×-CPU-
  throttled Chromium, with CV/CIS applied? Evidence: prototype in a scratchpad clone;
  Chrome trace JSON for (a) eager, (b) governed batchSize 4, (c) current 0.2.1 windowed
  baseline. Settles: `mount` default (`eager` vs governor), whether the governor
  survives at all, and the VR-14 contingency trigger.
- **F-2 — Safari find-in-page floor.** Verify Cmd+F over CV-skipped content in the
  currently-shipped stable Safari (the WebKit fix landed 2025-03-30; confirm it is in
  the release users actually run). Evidence: manual/WebKit-Playwright probe on the
  fixture. Settles: whether a pre-fix-Safari note (or `hidden="until-found"` micro-
  fallback) is needed in VR-22.
- **F-3 — View-timeline × content-visibility interplay.** Do scroll-driven view-timeline
  reveals on section wrappers fire at correct ranges when the section was in the skipped
  state (R2 §8 flags this as untested spec territory)? Evidence: prototype probe in
  Chromium + Safari 26 with a Beat-style reveal. Settles: vft's reveal-motion guidance
  in W5 (wrap-inner vs animate-wrapper).
- **F-4 — Estimate refinement load-bearing?** With `contain-intrinsic-size: auto`
  memory, does the VR-16 width-aware estimator measurably reduce scrollbar jitter /
  anchor mis-lands over the current width-blind estimates, or is `auto` self-calibration
  sufficient (making VR-16 deletable — KISS)? Evidence: CLS + anchor-landing-error
  measurements on cold loads at 3 widths, both estimators.
- **F-5 — activeId resolution mechanism.** Layout-read (`offsetTop` binary probe on
  rAF-coalesced scroll) vs IntersectionObserver-primary over the fully-mounted DOM:
  which gives fewer mis-highlights during fast drag and less main-thread time?
  Evidence: prototype both behind the same `usePaperViewport` API; scroll-jank trace +
  TOC-highlight correctness log. Settles the internal authority (VR-36 wiring).
- **F-6 — vft scroll model.** Does the vft propaedeutic scroll the window/document or
  an inner dock-panel element? Determines the default exercised path of the VR-40
  window adapter and the `leadingOffsetPx` wiring. Evidence: one confirmed line from
  the V-tranche D3/dock spec owners (design read, not prototype).
- **F-7 — Layout-containment edge cases in real papers.** CV containment vs the paper's
  own margin-collapsing/footnote/sticky-figure styles: does wrapping every section in a
  contained box shift vertical rhythm anywhere in the two real consumer papers?
  Evidence: pixel-diff screenshots (0.2.1 vs prototype) at 3 viewports.
- **F-8 — The missing R3 dossier.** The dedicated gap-analysis lane returned a stub.
  §1.2 re-derived the gap table from R1/R4/R8 plus direct file verification this pass,
  but an independent adversarial re-read of the 1,098-line layer (and of
  `useSidebarFollow`, 234 L, which no dossier read closely) should run in PASS 2 to
  confirm no gap class was missed. Evidence: the re-run lane's findings reconciled
  against the G1–G14 table.

---

## 9. Convergence self-assessment (PASS 1)

Settled from evidence: the SOTA definition, the architecture decision and its
contingency, the semver posture (including the caret correction), the byte-compat
floor, the theme seam mechanism, all six semantics arms, the Vue core shape. Open:
F-1..F-8 — of which F-1 is load-bearing for the mount-governor default and F-2/F-3
are load-bearing for two arms' fine print. Benchmarks claimed in §7/§8 are not yet
attached (PASS-2 prototype fleet). This pass is therefore not at 100 by construction;
the fork list above is the exact remaining distance.
