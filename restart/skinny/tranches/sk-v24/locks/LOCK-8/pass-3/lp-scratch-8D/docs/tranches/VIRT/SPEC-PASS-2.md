# VIRT — SOTA Virtualization + Cross-Repo Abstraction — SPEC (PASS 2)

Status: **SUPERSEDED by `SPEC-PASS-3.md` (pass 3, 2026-07-02) — historical; its F-1/F-3/F-5 "OPEN" statuses and all test counts are stale.** Was: PASS 2 of the convergence loop; superseded `SPEC-PASS-1.md`.
Synthesized 2026-07-02 from the pass-1 corpus (`PASS-1-SYNTHESIS.md`, `SPEC-PASS-1.md`
as corrected, `SCOPE-GLASS-UI.md`) plus the five pass-2 dossiers: R9 (the glass-ui twin
— divergence ledger, consumer map, provenance), R10 (F-8 adversarial re-read of the
latex-paper layer), R11 (WebKit/Safari mid-2026 reality), R12 (abstraction cost table +
ecosystem shapes), R13 (the search surface).

Targets, two roots (ROOT-REPO LAW, per-wave owner + vehicle in §6):

- `@mkbabb/glass-ui` 4.2.0 → next cut (semver, §VR-71), repo `/Users/mkbabb/Programming/glass-ui`
  — **the owning root for the headless core** (decision D-A, §0).
- `@mkbabb/latex-paper` v0.2.1 → v0.3.0 (changesets CI), repo `/Users/mkbabb/Programming/latex-paper`
  — consumes the core; keeps paper semantics + theme + adapters.

Register conventions: requirements are `VR-n` with **stable IDs** — VR-1..46 keep their
pass-1 numbers (corrections applied inline); VR-47..49 are the R10 additions; VR-50..58
the glass-ui core/sidebar family; VR-60..67 the search family; VR-70..74 the cross-repo
publish/gate family. Every VR carries a status — **SETTLED** / **PROVISIONAL** / **OPEN**
— and an evidence pointer. Open forks are `F-n` (§5). Full index table in §7.

---

## 0. DECISION D-A — the abstraction fork: **(a) glass-ui owns the headless core** — SETTLED

The first architecture decision of the tranche, per `SCOPE-GLASS-UI.md`. Settled on
R9 + R12 + R13 evidence; no further experiment needed. The deciding facts:

**1. The divergence ledger (R9): glass-ui is the more-evolved fork on every one of the
11 twin files.** `virtualSectionLayout`: glass-ui adds the `byId` sorted-entries view +
O(log n) binary-search `findSectionOffset` (glass-ui :92-103, 248-263) where latex-paper
linear-scans (:151-161 — this also closes gap G10 for free); its `FlatSection` extends the
shared `SectionHierarchy` base declared once across virtual+sidebar. `useVirtualSectionWindow`:
glass-ui routes observation through the house `useResizeObserver` leaf with scope-dispose
cleanup (:308-315), adds `contentEl`/`getContentScrollTop()`, the window-scroller path, and
`clearSessionHeightCache()`. `useScrollTracker`: glass-ui delegates to the ONE
`createScrollReader` core (the no-fourth-listener fence) and takes reactive roots;
latex-paper hand-rolls the rAF/listener pair. `useScrollTo`: glass-ui adds the
`onScopeDispose` rAF-leak fix. `useTreeIndex`: glass-ui adds the pure
`buildTreeIndex`/`isInActiveChain` API. `useSidebarFollow` is near-byte-identical (the
cleanest twin — drift with zero benefit). latex-paper's copies are the older,
paper-coupled ancestors.

**2. The consumer map (R9/R12) — the pass-2 headline premise "glass-ui fork consumers
unmapped / possibly zero" is FALSIFIED and struck.** Verified consumers:
`glass-ui/sidebar` — words (`ProgressiveSidebar.vue`), bbnf-lang (`DocsSidebar.vue`),
fourier-analysis (`PaperSidebar.vue`, `MobileFloatingToc.vue`). `glass-ui/search` —
value.js (×5 panes), bbnf-lang + ~11 variants. `glass-ui/virtual` — zero external yet
(demo + the atlas gate that asserts zero). latex-paper's fork — fourier-analysis
`PaperView.vue` (the live windowing consumer). **The engine exists in THREE repos**, not
two: glass-ui (home), latex-paper (ancestor), and a still-divergent
`words/frontend/src/composables/virtual/` copy (disposition: F-9).

**3. Provenance (R9): glass-ui is the origin AND the current home.** The engine was a
glass-ui primitive at v0.9.4 → retired at v1.0 ("0 consumers") → transposed into words →
re-homed into glass-ui at tranche BC (commit `c56586c0`, "BC Band 14 — latex-paper
abstractions"), generalized with a consumer-evidence doc, a `proof:virtual-window` gate,
and a byte-faithful fixture. Direction of history: glass-ui → words → glass-ui.

**4. The cost table (R12): option (a) wins on every axis.** glass-ui's virtual+sidebar
bundles are provably Vue-only (content probes: no reka-ui/Tailwind/lucide/cva/clsx) —
a 2-subpath consumer pays 5,722 B gzip total, externalized against the vue peer.
Zero new repos or publish pipelines. All three app consumers (fourier, words, bbnf-lang)
already depend on glass-ui. One blocker, one-line fix: glass-ui declares 5 mandatory
peers (`@lucide/vue`, `class-variance-authority`, `clsx`, `reka-ui`, `tailwindcss`) that
`./virtual`/`./sidebar` factually never touch → promote to `peerDependenciesMeta`
optional at glass-ui's own root (VR-51).

**5. Options (b) and (c) — refuted.** (b) A new `@mkbabb/virtual-core` package fails the
overfitting test: glass-ui already IS that package (published subpaths, gates, versioning);
a fourth repo would be an empty hub whose only consumers are two libraries. glass-ui's
own `virtual/index.ts` header records the identical ≥2-consumer-bar reasoning. (c)
latex-paper-owns is refuted on three axes: it inverts the platform relationship (every
glass-ui consumer would gain a transitive KaTeX/grammar dep); glass-ui has no
`@mkbabb/latex-paper` dep today; latex-paper's copies are the un-generalized ancestors
and latex-paper has no search surface at all (R13: nothing to migrate FROM latex-paper —
search is glass-ui-native already).

**Consequences (binding):**

- The SOTA document-native substrate — the slim viewport core, measurement epoch,
  settle gate, semantics arms — lands **ONCE at the glass-ui root** (`./virtual` +
  `./sidebar`). latex-paper's `usePaperViewport` (the pass-1 VR-34 surface) becomes a
  thin typed adapter over the generic glass-ui core (`FlatPaperSection extends
  FlatSection`); its API shape is unchanged from pass 1.
- The losing twins are **DELETED, never shimmed**: latex-paper's
  `src/vue/composables/{useVirtualSectionWindow,virtualSectionLayout,useSidebarFollow}.ts`
  and `src/vue/tracking/*`; fourier's `search/` directory; words' `composables/virtual/`
  copy (per F-9). Gate: VR-72.
- `leadingOffsetPx` — the ONE capability latex-paper's fork had that glass-ui's lacks —
  is **not dropped silently**: pass-1 P4 already deleted it in favor of real DOM geometry
  + CSS `scroll-margin-top` (probed 47.8px ≈ 48), and glass-ui's `contentEl` /
  `getContentScrollTop()` path subsumes the leading-content case (VR-58).
- Search is **glass-ui-internal hardening**, not a cross-repo abstraction (VR-60..67).
- A.0 (document-native) is re-expressed relative to the winner: **glass-ui ships the
  substrate; latex-paper's PaperView/PaperSection consume it** (VR-50/52).

---

## 1. SETTLED ARCHITECTURE STATEMENTS (the pass-1 forks, folded)

- **A.0 — document-native architecture — SETTLED** (VR-12). All sections mounted;
  `content-visibility: auto` + `contain-intrinsic-size: auto` defer layout/paint; the
  slim viewport core supplies activeId, deep links, settle hygiene, print/PRM arms; the
  422-line JS windowing layer retires from the document path. NO shim (v0.3.0 does not
  satisfy `^0.2.1` — verified). The decision rests on its ONE true premise: unmounting
  structurally breaks VR-4..9 (P1 measured ≤9/200 sections mounted; P2 measured all six
  semantics arms green natively). **The two falsified §2.2 arguments are STRUCK verbatim:**
  - ~~"TanStack's absolute-position/translateY model forces every consumer to abandon the
    spacer template"~~ — FALSE. P1 preserved fourier's spacer template unchanged;
    `measureElement` reads sizes only, never positions.
  - ~~"Adopting it would obligate us to rebuild … hundreds of consumer-visible lines"~~ —
    FALSE. P1's full adapter is a net −144 source-line deletion with zero consumer edits.
- **A.1 — VR-14 contingency — SETTLED as a ranked pair.** Fallback A = adopt
  `@tanstack/virtual-core` under the existing API (TESTED: P1 clone green, −144 lines,
  0.0px remeasure drift, +6,853 B gzip, zero transitive deps; loses VR-4..9). Fallback B
  = `hidden="until-found"` + `beforematch` (preserves VR-4..5; **floor now Safari 26.2**,
  2025-12-12, per R11 — the "never shipped in stable Safari" weakness is retired).
  Neither triggers unless F-1's rule R5 fires.
- **A.3 — core-alone — SETTLED** (Fallback-A-only): `virtual-core` alone suffices;
  no Vue adapter package.
- **A.4 — window-scroller — SETTLED**: the vft paper scrolls the document; the window
  `ViewportAdapter` has a real consumer; one-arg switch (VR-40). Residual G-l (vft
  binding doc records `.paper-article` scoping + `scroll-margin-top`) → W5.
- **A.5 — width-EPOCH measurement registry — SETTLED** (P3 unit-tested, incl. scrollbar
  width deltas). VR-17/18.
- **A.6 — estimator downgraded — SETTLED**: landing accuracy is seed-independent in
  Chromium (0.06–0.19px across 2–4× under- and 1.55× over-estimates); estimator quality
  affects scrollbar honesty only. Keep the simplest width-aware estimator; the RO→
  `--lp-section-size` writeback arm is dead (`auto` memory dominates). VR-15/16.
- **A.7 / F-5 — activeId — semantic SETTLED, mechanism OPEN (low-risk).** The canonical
  semantic is the current tracker's **deepest-visible-wins** (R10: a layout-offset probe
  gives first-section-past-a-line — a *different*, worse answer for nested docs). The
  offsetTop-vs-IntersectionObserver mechanism A/B stays F-5, judged on jank AND
  highlight-correctness against the deepest-visible semantic. VR-49.
- **D-A — abstraction fork (a) — SETTLED** (§0).

**Blocker dispositions:**
- **C-1 CLOSED** (→ VR-22): WebKit bug 283846 RESOLVED FIXED 2025-03-30, shipped Safari
  26.0 (2025-09-15, release notes verbatim: "Fixed content skipped with
  content-visibility: auto to be findable"); the 2026-05 contrary signal is
  unsubstantiated after adversarial search (most likely confusion with bug 277573,
  details/summary × CV — a separate issue); corroborated at runtime by P2's green
  find-over-CV probe on real WebKit 26.5.
- **C-2 CLOSED** (→ VR-73): the true suite count at pass-1 time is **145 passing +
  9 skipped = 154** (viewport = 27 of the 145). "172" is struck everywhere; acceptance
  criteria may cite only machine-derived counts with the reporter output committed.
- **C-3 CLOSED by design** (→ VR-19 epoch contract; implementation lands W2): the
  settleGate gets a real epoch — see VR-19.

---

## 2. THE VR REGISTER, PART I — the corrected pass-1 set (VR-1..49)

### SOTA definition (VR-1..11)

- **VR-1** [SETTLED] Dynamic per-item height stable under font/image/width change.
  Evidence: P3 registry + P2 CV `auto` memory. Width arm re-checked under R11 floors.
- **VR-2** [SETTLED — two-path, corrected by R11] No jump on above-viewport size change.
  Chromium: native scroll anchoring (P1/P2/P3 measured 0.0px). **Stable Safari through
  26.x has NO `overflow-anchor`** (caniuse + WebKit bug 171099; new in Safari 27 beta,
  WWDC26) → the `compensateScrollTop` net-delta fallback is the **default stable-Safari
  path, not optional**. Branch on `CSS.supports('overflow-anchor','auto')`. Runtime
  WebKit proof of the fallback = W3 gate (it is unit-tested, never yet run in a real
  engine).
- **VR-3** [SETTLED] Programmatic nav to any id: P3 LandingReport 0.06–0.19px; WebKit
  needs the VR-23 settle loop (257px-short without it).
- **VR-4** [SETTLED] Find-in-page whole-document: P2 green Chromium + WebKit 26.5;
  floor Safari 26.0 (VR-22). Only the native path satisfies it.
- **VR-5** [SETTLED] Native `#fragment`/`:~:text=`: Chromium exact; WebKit via VR-23
  (mandatory). Text-fragment-on-dynamic-pages fixed Safari 26.2 (R11).
- **VR-6** [SETTLED] Full heading/landmark tree, per VR-26/27 corrections.
- **VR-7** [SETTLED] Complete `window.print()`, per VR-24.
- **VR-8** [SETTLED] Reader mode whole article (P2 full innerText under CV).
- **VR-9** [OPEN → F-3] view-timeline reveals from the CV-skipped state. R11 settles
  support (Safari 26.0; threaded/compositor 26.4; `@supports (animation-timeline:
  scroll())` guard below); the skipped-state firing probe remains.
- **VR-10** [PROVISIONAL → F-1] Off-screen render deferral: layout/paint half settled
  (P2: native FCP 684ms vs eager 1636ms at 4×); the Vue-mount half is F-1 (P2: CV cannot
  defer mount — 555ms longtask, 220 uniquely-mathed sections, 4×).
- **VR-11** [SETTLED] All ten properties simultaneously at O(10–200) sections, smallest
  architecture that gets there.

### Architecture (VR-12..14)

- **VR-12** [SETTLED] Document-native (§1 A.0), re-homed: the generic viewport core ships
  at `@mkbabb/glass-ui/virtual`; latex-paper's windowing seam retires (VR-31/33).
- **VR-13** [PROVISIONAL → F-1] The mount governor is the **evidenced DEFAULT** (P2);
  monotone-forward reveal only, nothing unmounts; `revealAll()` escape for print/find/
  hash/idle; **client-render-only** (P4: governed mount over SSR HTML silently truncates
  — VR-25); frontier-monotonicity unit test (VR-46). F-1's pre-committed rule (§4) fills
  the eager auto-selection threshold; PROVISIONAL only until P5's traces land.
- **VR-14** [SETTLED] Contingency recorded as the §1 A.1 ranked pair; trigger condition
  = F-1 rule R5. Fallback-A-only cautions (private-API pin G-j, dual-scroll-authority
  G-c) activate only if A triggers.

### Measurement model (VR-15..21)

- **VR-15** [SETTLED, re-aimed] `estimatedHeight` seeds `contain-intrinsic-size: auto Npx`
  via `--lp-section-size`; acceptance metric = total-height/scrollbar honesty, not anchor
  error (A.6). Seeding level and the new `subtreeEstimatedHeight` field per VR-48.
- **VR-16** [SETTLED, corrected] Width-aware estimator KEPT at its simplest (chars-per-line
  from real width); the pass-1 math-model change is DROPPED (`operatorClusters` was
  undefined; the `104` floor already exists at `flattenPaperSections.ts:63` — G-b closed).
- **VR-17/18** [SETTLED] Shared per-instance ResizeObserver registry, `data-lp-id`
  identity, width-EPOCH invalidation (`read()` fresh-only / `readAny()` estimate-grade);
  delta accounting starts at the SECOND write; "no RO storm" = 0 nonzero-delta batches.
  The module `SESSION_HEIGHT_CACHE` singleton (G3 — cross-document contamination by bare
  id, no width key) is deleted with the twins. Ships in the glass-ui core (VR-50); the
  house `useResizeObserver` leaf is the only observer (VR-56).
- **VR-19** [SETTLED, corrected + C-3 epoch contract] Font gate:
  `document.fonts.ready` alone is INSUFFICIENT (P3: gate passed at 49ms; swap at 404ms) —
  **`document.fonts.load('1em KaTeX_Main', …)` over the transform-derived face list is
  REQUIRED.** `whenSettled()` contract (closes C-3): the gate keeps an integer **settle
  epoch**; every dirty event (RO delta, font arrival, image load) bumps it; each
  `whenSettled()` caller captures the epoch at call time and awaits a shared
  promise-per-epoch that resolves only when (fonts loaded) AND (epoch unchanged across
  one full rAF + RO flush). No shared mutable `dirty` flag; concurrent callers (initial
  hash landing + user nav) cannot clobber each other.
- **VR-20** [SETTLED] Figure images emit `width`/`height`/`aspect-ratio` when known,
  `loading="lazy" decoding="async"` otherwise; unknown-dimension reflows arrive under the
  RO epoch. Shift absorption per VR-2's two-path rule (not "browser anchoring" alone).
- **VR-21** [SETTLED design; W3 runtime gate] Anchor stability under above-viewport
  growth: Chromium 0.00px measured; stable-Safari path = `compensateScrollTop`
  (load-bearing, per VR-2); Playwright regression gate (grow an above-viewport image,
  assert stationary viewport text) runs on BOTH engines.

### Document-semantics arms (VR-22..30)

R11 version-floor table (binds every arm's feature detection):

| Capability | Floor |
|---|---|
| `contain-intrinsic-size: auto` | Safari 17.0 |
| `content-visibility: auto` | Safari 18.0 |
| find-in-page over CV content | **Safari 26.0** (bug 283846) |
| `hidden=until-found` + `beforematch` | Safari 26.2 |
| text-fragment `:~:text=` on dynamic pages | Safari 26.2 |
| scroll-driven animations (`view()`/`scroll()`) | Safari 26.0 (threaded 26.4) |
| native scroll anchoring (`overflow-anchor`) | **Safari 27 beta only** — absent all stable |

- **VR-22** [SETTLED — C-1 closed] Find-in-page floor = **Safari 26.0**. No runtime
  fallback for ≥26.0; Safari 18.x–25.x users may silently miss CV-hidden matches — an
  informative docs note, not a blocking arm. Evidence: R11 (bug 283846 + 26.0 release
  notes + no corroborated regression through 26.5/27-beta) + P2's real WebKit 26.5 green
  probe. Under the governor, `whenSettled()` → idle `revealAll()` bounds the
  non-findable window to first seconds of page life.
- **VR-23** [SETTLED, corrected + R10 fix] Settle-aware `scrollTo(id)` + `useHashNavigation`
  (`hashchange`/`popstate` — genuinely net-new; grep confirms zero such listeners today).
  Arrival detection **`scrollend`-primary with ≥3s fallback** (never the ~1.5s/90-frame
  cap, which truncated); long jumps (>~10 viewport-heights) instant; user-interrupt abort
  with the bottom-clamp exemption; gate `LandingReport{errorPx≤1, settleFrames≤10}`.
  **R10 corrections:** (1) the current retry loop can exit after 60 rAF having NEVER
  scrolled when content keeps growing (`useScrollTo.ts:42-61`) — the new scrollTo MUST
  commit a final scroll unconditionally after `whenSettled()`; (2) it MUST honor
  `scroll-margin-top: 5rem` (`theme.css:197`) or every deep-link lands 5rem off.
- **VR-24** [SETTLED, corrected] `revealTo`/`revealAll` return `Promise<void>`; consumers
  `await revealAll()` before `print()` (programmatic `window.print()` blocks its own
  task). CV half pure-CSS via `@media print` (P4 68-page PDF artifact). W3 re-proof binds
  to a real print dialog artifact (G-h).
- **VR-25** [SETTLED] SSR ARCHIVED (no consumer) with documented guards: full
  `renderToString` verified; eager hydration = zero mismatches; runtime arms
  post-hydration-only; **governed mount is client-render-only** (silent truncation
  otherwise). W3 re-proof on real hydration.
- **VR-26** [SETTLED, corrected] Heading level = **min-normalized `headingLevel`**, an
  ADDITIVE `FlatPaperSection` field (unit-tested {1,2}→{2,3}, {0,1,2}→{2,3,4}); raw
  `sourceLevel` clamp emits colliding h1s. **R10: this is multi-file plumbing, not a
  one-liner** — `sourceLevel` exists on `FlatPaperSection` but `PaperSection.vue` receives
  only `depth`; thread through `PaperSectionContent` (:26,:37). Budgeted in W4.
- **VR-27** [SETTLED, corrected] `role="doc-section"` does NOT exist in DPUB-ARIA 1.1 —
  replaced with **`aria-labelledby` named regions** (h2 `id` + section `aria-labelledby`),
  SSR-asserted.
- **VR-28** [SETTLED, extended by R10] PRM gates every programmatic smooth scroll AND
  **useSidebarFollow's rAF damping loop itself** (damping 0.22, `useSidebarFollow.ts:74-94`
  — JS-driven motion invisible to CSS `scroll-behavior`): when PRM, force the existing
  `queue(immediate=true)` path. Third smooth site inventoried: `useScrollTracker.ts:201`
  `scrollIntoView`. Theme adds the `@media (prefers-reduced-motion: reduce)` CSS arm.
- **VR-29** [SETTLED] Focus: native under full mount; monotone reveal never destroys
  focus; axe + keyboard-walk probe in the π lane.
- **VR-30** [SETTLED] Reader mode ≥95% extraction gate (Firefox reader-view probe).

### API surface + Vue architecture (VR-31..46)

- **VR-31** [SETTLED] Semver honesty: `^0.2.1` excludes 0.3.0 (re-verified with `semver`);
  v0.3.0 is explicit opt-in; `useVirtualSectionWindow`/`VirtualSectionWindowOptions`/
  `virtualSectionLayout` REMOVED, not deprecated; minor changeset with a MIGRATION section
  naming every removed symbol and its replacement.
- **VR-32** [SETTLED, amended by D-A] Byte-compat floor holds for everything a consumer
  imports today except the retired seam — with one D-A amendment: the tracking-suite
  exports (`useSidebarFollow`, `useScrollTracker`, `useScrollTo`, `useLazyLoader`,
  `useClickDelegate`, `useTreeIndex`, types) remain exported from `/vue` but become
  **re-exports of the glass-ui sidebar suite** (consumption of the winner, not a shim);
  signature deltas (useScrollTracker's reactive `MaybeRefOrGetter` roots + dropped
  `visibleCount`; `as EventListener` casts) are named in the MIGRATION section.
  `useSidebarFollow` is near-byte-identical, so fourier's live import is unaffected.
- **VR-33** [SETTLED] Breaks, exhaustively: (a) windowing seam → `usePaperViewport`;
  (b) spacer render-host contract → render ALL sections with
  `:ref="registerSection"` + `data-lp-id` + the CV class; (c) `virtualSectionLayout`
  removed (verify no type leaks through `dist/`). fourier migration = net deletion;
  substrate + consumer land together (W5). Double-containment trap → VR-48/G-f.
- **VR-34** [SETTLED] `usePaperViewport` surface exactly as pass-1 §5.2 (minus
  `leadingOffsetPx`, per VR-58), now a thin `FlatPaperSection`-typed adapter over the
  glass-ui generic core (VR-50). Single `shallowRef` root + `triggerRef` + computed
  outputs (VR-39).
- **VR-35** [SETTLED] `scrollToId` on `PaperContext` library-fulfilled (2 consumers);
  fourier's search/progress/nav-stack chrome stays consumer-side — amended by VR-60:
  fourier's search UI is replaced by the glass-ui surface, still consumer-composed.
- **VR-36** [SETTLED, sharpened by R10] `usePaperReader` rebases on the viewport core;
  **G11 is worse than "duplicates": two live activeId authorities RACE today**
  (IO/getElementById tracker vs layout-offset `resolveActiveSection`) with different
  algorithms and tie-breaks. Post-tranche there is exactly ONE authority (VR-49);
  `useTreeIndex`'s `getChildren` override documented as the `subsections→children`
  adapter.
- **VR-37** [SETTLED, corrected] Two-tier theme tokens **`var(--lp-x, var(--x))`
  full-color BOTH tiers** (the hsl-triple fallback tier is dead code — live-probed
  `rgb(0,0,0)`); resolves oklch exactly; unblocks the vft earth palette; also a 0.2.1
  bug fix. Token table in `docs/theming.md`.
- **VR-38** [SETTLED] latex-paper release vehicle: changesets CI, no dev-machine publish;
  gates `npm run build` + `npm test` per wave. Extended cross-repo by VR-70/71.
- **VR-39** [SETTLED] Single reactive root (`shallowRef` + one `onChange` → `triggerRef`).
- **VR-40** [SETTLED] Injectable `ViewportAdapter` seam `{observeRect, observeOffset,
  scrollTo}` — element + window implementations; deterministic testability; window mode
  is the vft consumer's path (A.4). The container-vs-window duality preserves the
  `getBoundingClientRect().top` containerTop math (R10 regression floor).
- **VR-41** [SETTLED] Passive scroll listener, rAF-coalesced; `isScrolling` from
  `scrollend` + 150ms debounce fallback; the 320ms `warmTimer` dies. In glass-ui the
  listener is `createScrollReader` — the no-fourth-listener fence (VR-56).
- **VR-42** [SETTLED] Vue 3.5 hygiene: `onWatcherCleanup`, `useTemplateRef`, reactive
  props destructure.
- **VR-43** [SETTLED, corrected] Render-cost discipline: `v-memo="[item.id]"` on section
  roots is the net-new work. **Correction (R10): `v-once` on KaTeX interiors is ALREADY
  SHIPPED** (`MathBlock.vue:17`, `MathInline.vue:11`) — not claimed as new. `KeepAlive`
  rejected.
- **VR-44** [SETTLED] O(1) id→element/index maps; `getOffsetFor` reads live `offsetTop`;
  glass-ui's binary-search `findSectionOffset` closes G10 at the owning root.
- **VR-45** [PROVISIONAL → F-7] CSS substrate ownership: the CV/CIS rule ships in
  latex-paper `theme.css` under `[data-lp-section]`; `PaperSection.vue` emits the
  attribute + inline `--lp-section-size`. **Amended by G-g:** the host wrapper owns
  inter-section spacing (`.paper-section:not(:last-child){margin-bottom}` is dead under
  wrapper-per-section, and containment un-collapses trailing margins 12–16px/section).
  PROVISIONAL pending F-7's sticky/nesting probes (VR-47/48).
- **VR-46** [SETTLED] Governor frontier-monotonicity unit test; plus the full vitest/
  Playwright gate set of pass-1 §6, counts per VR-73.

### The R10 additions (VR-47..49)

- **VR-47** [PROVISIONAL → F-7] **The sticky-header × containment trap (new defect class).**
  The shipped theme's two-tier sticky header stack (`theme.css:443-465`: chapter `top:0
  z:10`, sub `top:3.5rem z:9`) lives INSIDE `.paper-section` — the exact box VR-45 makes
  `content-visibility:auto`. CV implies `contain: layout paint`; a sticky element cannot
  stick outside its containing block → the design's cross-document sticky headers BREAK.
  Required resolution, probed by F-7: **(preferred) apply CV to an inner `.section-body`
  wrapper that excludes the header**, or move headers out of the contained box; dropping
  sticky headers is the last resort. F-7 pixel-diffs sticky behavior specifically.
- **VR-48** [PROVISIONAL → F-7] **The nesting model.** `PaperSectionContent` renders
  subsections as DOM DESCENDANTS of their parent (:31-40), yet `flattenPaperSections`
  emits flat per-node estimates covering own-content only (:79-89) — naive per-flat-node
  CV double-contains and under-seeds parents. Rule: **CV + `contain-intrinsic-size` on
  ROOT sections only (depth===0)**, seeded by a new subtree-inclusive
  `subtreeEstimatedHeight` field. F-7b probes root-only vs per-level on the real papers.
- **VR-49** [SETTLED] **ONE activeId authority + the regression floor.** The viewport
  core is the single authority; the semantic is **deepest-visible-wins** (canonical, per
  A.7). The replacement MUST preserve, verbatim from the current layer (R10): (1)
  deepest-visible tie-break (`useScrollTracker.ts:46-56`); (2) **closest-below fallback**
  when nothing is above the active zone (:108-131, :243-249 — else the TOC shows nothing
  at scrollTop 0); (3) `scroll-margin-top: 5rem` anchor landing (`theme.css:197`);
  (4) `activeId` initializes non-null to `roots[0].id` (:27) *within the embedded
  document* — reconciled with P4's vft finding (`activeId` null outside the article on
  mid-route): non-null inside, null outside; (5) container-vs-window duality math
  (:94-103); (6) the `useSidebarFollow` manual-override state machine (deadzone
  0.18·navHeight, damping 0.22, `programmaticScrollDepth` guard, wheel/touch/pointer/
  arrow-key suspension, resume on next content scroll) — a documented regression-floor
  black box, preserved at the glass-ui root (VR-54).

---

## 3. THE VR REGISTER, PART II — the new families

### The glass-ui core + sidebar family (VR-50..58) — owner: glass-ui; consumers named per VR

- **VR-50** [SETTLED] **The landing site.** The document-native viewport core ships at
  `@mkbabb/glass-ui/virtual` as the generic engine (`FlatSection`/`SectionHierarchy`
  typed): registerSection + measurement registry (VR-17/18), settle gate (VR-19),
  settle-aware scrollTo + hash nav (VR-23), mount governor (VR-13), activeId authority
  (VR-49), ViewportAdapter seam (VR-40). latex-paper's `usePaperViewport` adapts it
  (VR-34/52). Consumers: latex-paper (hence fourier + vft), words per F-9.
- **VR-51** [SETTLED] **The peer-meta prerequisite.** glass-ui `package.json` gains
  `peerDependenciesMeta` marking `@lucide/vue`, `class-variance-authority`, `clsx`,
  `reka-ui`, `tailwindcss` **optional** — factually correct (R12 bundle content probes:
  `./virtual`/`./sidebar` are Vue-only). Lands at glass-ui's own root before latex-paper
  consumes; non-breaking loosening (existing consumers keep their installed peers).
- **VR-52** [SETTLED] **The latex-paper adapter surface.** `FlatPaperSection extends
  FlatSection` (one-file type audit: the hierarchy fields subsume; paper-only fields —
  `sourceLevel`, `headingLevel`, `number`, `estimatedHeight` semantics — extend).
  glass-ui becomes a runtime **`dependency`** of latex-paper (precedent: `bbnf-lang`,
  `parse-that` are deps; latex-paper bundles its dist); `vue` stays a peerDependency.
  Runtime deps go 3 → 4; cost = 5,722 B gzip for both subpaths (R12) — accepted.
  `/vue` re-exports the sidebar suite per VR-32.
- **VR-53** [SETTLED] **Sidebar tracker rebase.** glass-ui's `useScrollTracker` (already
  on `createScrollReader`) consumes the viewport core's layout truth as its authority,
  keeping IO as in-DOM precision enhancement; the VR-49 regression floor binds. The
  `getElementById` fallback's blindness to unmounted sections (R10) becomes moot under
  full mount but the closest-below + deepest-visible semantics are gate-tested.
- **VR-54** [SETTLED] **`useSidebarFollow` — single home.** The near-byte-identical twin
  dedupes to glass-ui (`composables/sidebar/`); gains the PRM snap on its damping loop
  (VR-28) and `scrollSource:'window'` (P4); the manual-override state machine is
  documented and regression-tested (VR-49.6). latex-paper's 234-line copy deletes;
  fourier's import continues via the `/vue` re-export.
- **VR-55** [SETTLED] **glass-ui TOC surface to spec.** The same semantics bar latex-paper
  meets applies to glass-ui's sidebar consumers: TOC is a persistent never-virtualized
  `<nav>` with `aria-current` on the active item; PRM gates on `useScrollTo`/
  `scrollIntoView` sites; `useScrollTo` inherits the VR-23 final-commit + scroll-margin
  discipline. Consumers: words `ProgressiveSidebar`, bbnf-lang `DocsSidebar`, fourier
  `PaperSidebar`/`MobileFloatingToc`.
- **VR-56** [SETTLED] **Shared-leaf canon.** `useResizeObserver`, `createScrollReader`,
  and `useTextHighlight` are the ONLY observer/listener/highlight primitives (the
  no-fourth-listener fence); latex-paper's hand-rolled `new ResizeObserver` /
  rAF-listener pairs delete with the twins. glass-ui-only leaves — verified absent from
  latex-paper (R9).
- **VR-57** [OPEN → F-9] **The words copy disposition.** words/frontend carries the third
  divergent copy (`composables/virtual/`, "Transposed from glass-ui v0.9.4"). F-9's read
  decides: document-shaped consumer → migrate to the native core and DELETE the generic
  windower everywhere; feed/grid-shaped → `useVirtualSectionWindow` survives at the
  glass-ui root ONLY, words becomes the `./virtual` windower's first external consumer.
  Either branch closes the twin gate (VR-72). words' `@tanstack/vue-virtual` grid stays
  words-local regardless (glass-ui's own ≥2-consumer-bar record).
- **VR-58** [SETTLED] **`leadingOffsetPx` disposition (named, not silent).** Deleted per
  P4 (real DOM geometry + CSS `scroll-margin-top`, probed); glass-ui's `contentEl` +
  `getContentScrollTop()` covers non-virtualized leading content. The MIGRATION section
  names the replacement.

### The ONE search surface (VR-60..67) — owner: glass-ui `./search`; first consumer: fourier

- **VR-60** [SETTLED] **Ownership.** glass-ui `./search` (FuzzySearch.vue, SearchBar.vue,
  `useFuzzySearch`, `buildIndex`/`searchIndex`/`fuzzyMatch`/`clearSearchCache`,
  `searchVariants`, `useTextHighlight`) is the ONE search surface. latex-paper ships NO
  search source today (grep-verified) — nothing migrates FROM it; the scope directive
  resolves to hardening IN glass-ui. fourier DELETES its entire
  `web/src/components/paper/search/` directory (`paperSearchIndex.ts`,
  `usePaperSearch.ts`, `searchHelpers.ts`, `PaperSearchModal.vue`,
  `PaperSearchDropdown.vue`, `PaperSearchInput.vue`) — no shims. The deletion also
  eliminates fourier's module-singleton cache bug (cross-index contamination) and its
  v-html `<mark>` injection; glass-ui's WeakMap-per-index cache and reactive-index/
  scope-dispose discipline are already the evolved implementations (R13).
- **VR-61** [SETTLED] **The paper adapter.** latex-paper exports
  `toPaperSearchItems(sections: PaperSectionData[]): SearchableItem[]` — section title
  (number-prefixed, e.g. "3.2 Convergence Theorem") → `label`; plainText/rawTex → `text`;
  paper type union → `type` (badge display via FuzzySearch's `typeLabel` prop, preserving
  the full vocabulary); `id` → `id`. Domain coupling stays out of glass-ui.
- **VR-62** [SETTLED] **CSS Custom Highlight API** is the match-marking path (no DOM
  mutation, no `<mark>`, no XSS surface): `useTextHighlight` with the
  `::highlight(glass-search-mark)` rule (token-bridged, overridable); floor Chromium
  105+/Safari 17.2+/Firefox 140+; feature-detected safe no-op fallback (plain labels) —
  acceptable, not a shim.
- **VR-63** [SETTLED] **Keybinding law.** Cmd+K (consumer-wired, configurable) opens the
  custom surface; the library NEVER intercepts Cmd+F — browser find-in-page needs zero
  library code under the native architecture (VR-22). fourier's existing global-keydown
  wire is already correct and stays consumer-side.
- **VR-64** [SETTLED] **Result navigation contract.** `selectResult` → if governed and
  target beyond the frontier: `await revealTo(id)` then `scrollTo(id)` (the VR-23
  settle-aware contract, identical to deep-link nav); if eager/fully-revealed:
  `scrollTo(id)` directly. `scrollIntoView` on CV-skipped content auto-reveals per spec
  (P2 confirmed at 220 sections) — no separate "reveal" step exists. PRM applies to the
  scroll, not the modal.
- **VR-65** [SETTLED] **Index timing.** The index builds from the transform layer's
  `PaperSectionData` output (never the DOM), after transform, before first keystroke;
  dynamic updates ride `useFuzzySearch`'s reactive-index path (items as factory/computed).
- **VR-66** [SETTLED] **Cache + lifecycle discipline.** WeakMap-per-index result cache;
  `onScopeDispose` → `clearSearchCache(index)`; debounce timers cancelled via
  `onCleanup`. (All already shipped in glass-ui — gate-asserted, not rebuilt.)
- **VR-67** [SETTLED] **First consumer lands with the surface.** `./search` currently has
  live consumers for SearchBar/FuzzySearch (value.js, bbnf-lang ×~11) — the *paper*
  composition (FuzzySearch + adapter + viewport nav) lands with fourier as its first
  consumer at the same glass-ui cut that declares the surface stable (public-surface-
  needs-a-consumer precept).

### Cross-repo publish + gates (VR-70..74)

- **VR-70** [SETTLED] **Per-wave owner + vehicle** (§6 table). Migration order (R12):
  (1) glass-ui — peer-meta + the SOTA core + sidebar/search hardening → semver cut;
  (2) latex-paper — delete twins, add the dep, adapters, theme substrate → changesets
  v0.3.0; (3) fourier — windowing net-deletion + search-dir deletion + import swap;
  (4) words — F-9 outcome; (5) vft — binding doc (G-l).
- **VR-71** [SETTLED] **Semver.** glass-ui: peer-meta loosening + additive core = minor
  (4.3.0); if F-9's branch deletes `useVirtualSectionWindow` from the published surface,
  the cut is major (5.0.0). latex-paper: v0.3.0 minor changeset, opt-in (VR-31).
- **VR-72** [SETTLED] **The twin-deletion gate (tranche close).** Grep-checkable: each of
  the twin filenames — `useVirtualSectionWindow.ts`, `virtualSectionLayout.ts`,
  `useWindowedStore.ts`, `useSidebarFollow.ts`, `useScrollTracker.ts`, `useScrollTo.ts`,
  `useLazyLoader.ts`, `useClickDelegate.ts`, `useTreeIndex.ts`, sidebar `types.ts` — and
  the fourier search files (VR-60 list) exist in **exactly ONE repo** across
  {glass-ui, latex-paper, fourier-analysis, words} (node_modules/dist/worktrees
  excluded). The atlas `j0-absorb-leaf-census` gate (which today asserts
  `@mkbabb/glass-ui/virtual` has zero consumers) is updated in the same wave the first
  consumer lands, or it fires falsely.
- **VR-73** [SETTLED] **Test-count discipline (C-2).** Acceptance criteria cite only
  machine-derived counts with the vitest reporter output committed beside the claim.
  Corrected pass-1 baseline: 145 pass + 9 skip = 154 (viewport 27).
- **VR-74** [SETTLED] **Fleet hygiene (G-m).** Every Playwright lane runs in its own
  isolated `newContext()`; prototype lanes reuse/extend the pass-1 clones
  (`scratchpad/virt-p{1..4}`); nothing built in a clone travels to a real landing
  uncommitted-reviewed.

---

## 4. THE F-1 BENCHMARK PROTOCOL (pre-committed — P5 executes; the fork closes mechanically)

**Question.** The mount default for the viewport core: CV cannot defer Vue-mount +
KaTeX `renderToString` cost (P2: 555ms longtask, 220 uniquely-mathed sections, 4×
throttle). Governor is the evidenced default; what remains is the eager auto-selection
threshold and the governor's own PASS/FAIL.

**Fixtures.** (a) fourier's real paper (the live consumer,
`fourier-analysis/web/src/components/paper/PaperView.vue`, ~60 sections); (b) the P2
stress-fixture family with cache-defeating unique math at N ∈ {40, 60, 80, 120, 160, 220}
(reuse the `virt-p2` clone).

**Environment.** Prod builds; Chromium with 4× CPU throttle; isolated `newContext()` per
run (VR-74); 3 runs per cell, median reported; traces as JSON committed under
`docs/tranches/VIRT/traces/`.

**Arms.** (1) eager full mount + CV/CIS; (2) governed (batch-size sweep {2, 4, 8} on the
220 fixture; pick the smallest batch whose reveal-to-frontier latency stays under one
frame budget) + `idleRevealAll`; (3) the 0.2.1 windowed baseline — **reference trace
only** (see R1 below).

**Metrics.** Worst main-thread longtask during initial mount (ms); FCP; scripted-scroll
frame stats (avg frame ms, count >33ms, 20s run); time-to-full-reveal (governed);
far-target `revealTo`+`scrollTo` LandingReport.

**THE DECISION RULE (pre-committed; no re-litigation):**

- **R1.** The windowed baseline is **retired regardless of outcome** — it fails VR-4..9
  structurally. It appears only as the scroll-frame reference.
- **R2.** The shipped default is **`mount: governed`**.
- **R3 (eager auto-selection).** `S_cross` = the largest fixture N whose eager
  worst-longtask < 200ms at 4× throttle. The shipped rule:
  `items.length ≤ floor(0.8 × S_cross) → 'eager'`, else governed. The constant lands in
  code with the trace JSON cited in its doc comment.
- **R4 (governor PASS/FAIL).** On the 220 fixture at 4×, governed must: (i) keep EVERY
  longtask < 200ms including `idleRevealAll` batches; (ii) match the windowed reference's
  scroll frame stats within 10% avg with 0 frames >33ms; (iii) reach any far target via
  `revealTo`+`scrollTo` with LandingReport `errorPx ≤ 1`.
- **R5 (fallback trigger).** If R4(i) fails after the batch sweep → VR-14 fires:
  Fallback A where the consumer accepts VR-4..9 loss at the failing scale, else
  Fallback B (floor Safari 26.2). If only R4(ii) fails → tune batch scheduling
  (idle-callback chunking) before ANY fallback.

**Closure.** F-1 closes when the verdict table + `S_cross` constant + committed traces
land; VR-10/13 flip PROVISIONAL → SETTLED in the same edit.

---

## 5. OPEN FORKS

- **F-1 — mount default threshold** [OPEN — capping]. Protocol §4; P5 executes.
- **F-3 — view-timeline × CV skipped-state** [OPEN — narrowed]. Support settled
  (Safari 26.0, threaded 26.4, `@supports (animation-timeline: scroll())` guard); the
  remaining probe: do Beat-style reveals fire at correct ranges from the skipped state?
  Chromium + Safari 26 → vft reveal guidance (wrap-inner vs animate-wrapper).
- **F-5 — activeId mechanism A/B** [OPEN — low-risk, semantic decided]. offsetTop
  binary probe vs IntersectionObserver-primary, judged on jank trace AND
  highlight-correctness against the deepest-visible-wins canon (VR-49).
- **F-7 — containment × real-paper CSS** [OPEN — extended by R10]. Pixel-diff 0.2.1 vs
  prototype at 3 viewports, now with two named sub-probes: **F-7a** the sticky-header
  stack survives the chosen CV wrapper level (VR-47); **F-7b** root-only vs per-level CV
  on the real nested papers (VR-48); plus the G-f `.deferred-section` strip geometry
  check on fourier.
- **F-9 — the words virtual-copy disposition** [OPEN — new]. The ONE deciding read:
  what consumes `words/frontend/src/composables/virtual/` and is it document-shaped or
  feed-shaped? Rule pre-committed in VR-57; sets the glass-ui cut's semver (VR-71).

**Closed this pass:** F-2 (R11 + P2: floors pinned, §VR-22/2/21 two-path; residual
runtime proof = W3 gate, not a fork), F-4 (A.6), F-6 (A.4; G-l is a W5 work item),
F-8 (executed as R10 — four new defect classes found and folded: VR-47, VR-48, VR-49,
VR-23's never-scrolls fix). The abstraction fork is D-A (§0), settled.

---

## 6. WAVE SHAPE (draft plan; every wave names owner + vehicle)

≤3 concurrent dispatch (concurrency law); zero-deferral; gates are build/test/runtime/
benchmark artifacts only. Full `VIRT.md` lands post-100.

| Wave | Name | Owner repo → vehicle | Goal criterion | Closes on (evidence) |
|------|------|----------------------|----------------|----------------------|
| W1 | Fork-Settling Fleet | scratchpad clones → verdict tables in this folder | F-1 (P5, §4), F-3, F-5, F-7a/b, F-9 settled; PLUS the extraction proof (the shared headless core carved out, both libraries' surfaces re-expressed over it, compile+tests green in both clones — SCOPE item 4) | Trace JSONs + per-fork verdict table appended to this spec; extraction-clone test output |
| W2 | The glass-ui Substrate | glass-ui → semver cut (4.3.0 / 5.0.0 per F-9) | Peer-meta (VR-51); document-native core at `./virtual` (VR-50: settle-epoch VR-19, scrollTo VR-23, governor VR-13, two-path anchoring VR-2/21, width-epoch VR-17/18); sidebar rebase (VR-53/54/56); search hardening asserts (VR-62/65/66) | glass-ui build + vitest + `proof:virtual-window` gates green (counts per VR-73); consumer-evidence docs updated |
| W3 | Semantics Runtime Proofs | both repos' clones → committed artifacts | Real-WebKit: find-over-CV re-confirm, `compensateScrollTop` first real-engine run, fragment landing, smooth timing; real print dialog (VR-24); real hydration (VR-25); axe + keyboard walk; reader-mode extraction | Playwright artifacts per arm on Chromium AND WebKit; PDF artifact; axe report |
| W4 | latex-paper Consumption | latex-paper → changesets v0.3.0-pre | Twins deleted; glass-ui dep + adapters (VR-52, VR-61); theme substrate incl. sticky/nesting resolution (VR-45/47/48, per F-7) + `--lp-*` tokens (VR-37) + headingLevel plumbing (VR-26) + aria-labelledby (VR-27) | Build+test exit 0; the deletion diff; oklch smoke screenshot; MIGRATION section complete (VR-31/32/58) |
| W5 | Consumer Migrations | fourier, words, vft (sci-report docs) | fourier: windowing net-deletion + search-dir deletion, first `./search` paper consumer (VR-60/67); words: F-9 branch executed; vft: binding doc records `.paper-article` + `scroll-margin-top` + window model (G-l) | fourier build + runtime probe on its real paper; consumer LOC diffs; VR-72 grep dry-run green |
| W6 | Release + Gate Close | both roots → npm cuts | Both versions cut; twin-deletion gate (VR-72) + atlas census-gate update; π visual lane (≥3 viewports, PRM on/off, sticky-header pixel check) | Tags + CI publish runs green; VR-72 grep output committed; π artifacts; before/after frame-budget ledger incl. WebKit |

Coordination: `docs/tranches/VIRT/coordination/` opens if vft V4 runs concurrently in
sci-report. The π lane is binding (render-path + design-register change: search modal,
sticky headers, TOC states ride the glass surface/controlSize registers — no bespoke
chrome).

---

## 7. THE REGISTER INDEX

| VR | Subject | Status | Evidence pointer |
|----|---------|--------|------------------|
| 1 | dynamic height stability | SETTLED | P3 registry + P2 CV memory |
| 2 | no above-viewport jump (two-path) | SETTLED | P1/P2/P3 0.0px; R11 anchoring floors; W3 runtime gate |
| 3 | programmatic nav | SETTLED | P3 LandingReport |
| 4 | find-in-page | SETTLED | P2 both engines; R11 floor 26.0 |
| 5 | fragment/text-fragment nav | SETTLED | P2/P4; R11 26.2 |
| 6 | a11y heading/landmark tree | SETTLED | via VR-26/27 |
| 7 | complete print | SETTLED | P4 PDF; VR-24 |
| 8 | reader mode | SETTLED | P2 innerText |
| 9 | view-timeline reveals | OPEN | F-3 |
| 10 | off-screen deferral | PROVISIONAL | P2 split result; F-1 |
| 11 | the SOTA target statement | SETTLED | pass-1 §1.1 |
| 12 | document-native architecture | SETTLED | A.0; §2.2 strikes applied |
| 13 | mount governor default | PROVISIONAL | P2; F-1 rule §4 |
| 14 | ranked contingency pair | SETTLED | A.1; R11 26.2 floor |
| 15 | CIS seeding | SETTLED | A.6 re-aim; VR-48 level |
| 16 | simplest width-aware estimator | SETTLED | A.6; G-b closed |
| 17/18 | RO registry + width epoch | SETTLED | P3 unit tests |
| 19 | fonts.load gate + settle epoch | SETTLED | P3 falsification; C-3 design |
| 20 | image dimensions | SETTLED | R4 §5 + VR-2 |
| 21 | anchor stability | SETTLED (W3 runtime gate) | P3 0.00px; R11 compensate-path |
| 22 | WebKit find floor | SETTLED | C-1 closed: R11 + P2 26.5 |
| 23 | settle-aware scrollTo + hash nav | SETTLED | P2/P3; R10 final-commit + scroll-margin |
| 24 | Promise-print | SETTLED | P4; W3 real-dialog re-proof |
| 25 | SSR archived + guards | SETTLED | P4 |
| 26 | min-normalized headingLevel | SETTLED | P4; R10 plumbing note |
| 27 | aria-labelledby regions | SETTLED | P4 falsification |
| 28 | PRM incl. damping loop | SETTLED | P4 + R10 third site |
| 29 | focus | SETTLED | native under full mount |
| 30 | reader extraction gate | SETTLED | P2 |
| 31 | semver honesty / removals | SETTLED | semver re-run |
| 32 | byte-compat floor (amended) | SETTLED | R5 + D-A re-export note |
| 33 | breaks list | SETTLED | pass-1 + VR-48 trap |
| 34 | usePaperViewport surface | SETTLED | pass-1 §5.2 minus leadingOffsetPx |
| 35 | scrollToId promotion | SETTLED | 2-consumer test |
| 36 | ONE reader authority | SETTLED | R10 race finding; VR-49 |
| 37 | full-color two-tier tokens | SETTLED | P4 live probe |
| 38 | latex-paper release vehicle | SETTLED | R7 |
| 39–42 | Vue core hygiene | SETTLED | R8/P1/P4 |
| 43 | v-memo (v-once already shipped) | SETTLED | R10 correction |
| 44 | O(1) lookups / binary search | SETTLED | glass-ui :248-263 |
| 45 | CSS substrate + spacing (G-g) | PROVISIONAL | F-7 |
| 46 | test gates | SETTLED | VR-73 counts |
| 47 | sticky × containment | PROVISIONAL | R10; F-7a |
| 48 | root-only CV + subtree seed | PROVISIONAL | R10; F-7b |
| 49 | activeId semantic + regression floor | SETTLED | R10 floor list |
| 50 | glass-ui owns the core | SETTLED | D-A |
| 51 | peer-meta optional | SETTLED | R12 bundle probes |
| 52 | latex-paper adapter + dep | SETTLED | R12 |
| 53 | tracker rebase | SETTLED | R9 scrollReader |
| 54 | sidebarFollow single home | SETTLED | R9 byte-twin |
| 55 | glass-ui TOC to spec | SETTLED | R9 consumers |
| 56 | shared-leaf canon | SETTLED | R9 |
| 57 | words copy disposition | OPEN | F-9 |
| 58 | leadingOffsetPx subsumed | SETTLED | P4 + R9 contentEl |
| 60 | ONE search surface | SETTLED | R13 |
| 61 | toPaperSearchItems adapter | SETTLED | R13 |
| 62 | CSS Custom Highlight path | SETTLED | R13 |
| 63 | Cmd+K law | SETTLED | R13 |
| 64 | result-nav contract | SETTLED | P2 scrollIntoView + VR-23 |
| 65 | index from transform | SETTLED | R13 |
| 66 | cache/lifecycle discipline | SETTLED | R13 (shipped; gate-asserted) |
| 67 | first search consumer lands | SETTLED | precept + R13 map |
| 70 | owner/vehicle + migration order | SETTLED | R12 |
| 71 | semver per repo | SETTLED | R12; F-9 sets glass-ui major/minor |
| 72 | twin-deletion gate | SETTLED | R9 three-copy map |
| 73 | test-count discipline | SETTLED | C-2 re-run |
| 74 | fleet hygiene | SETTLED | G-m |

---

## 8. CONVERGENCE SELF-ASSESSMENT (PASS 2)

**88%.** Up from 82. What moved: the abstraction fork — the pass's first-class
architecture question — is SETTLED on multi-repo, on-disk evidence (D-A), and the
falsified zero-consumer premise was caught and struck rather than built on. All three
pass-1 blockers are closed (C-1 on R11 research + P2's real-engine probe; C-2 by re-run;
C-3 by a concrete epoch design that W2 implements). The seven pass-1 falsification
corrections and the two §2.2 strikes are folded verbatim. R10 surfaced four genuinely
new defect classes (sticky × containment, nesting double-containment, the two-authority
race, the never-scrolls retry loop) — each now a numbered requirement with a named probe.
R11 overturned one silent assumption (stable Safari has NO scroll anchoring) in time to
make the fallback path load-bearing by design rather than by incident.

What caps it below higher: **F-1 remains open** — its decision rule is now pre-committed
and mechanical (§4), so the *design* is closed, but the threshold constant and the
governor's PASS traces do not exist yet, and VR-10/13 gate a shipped default. F-7's
sticky/nesting probes gate the CSS substrate's final shape (VR-45/47/48). F-9 is a small
read but sets a semver-major/minor branch. The W3 runtime re-proofs (real-engine
compensateScrollTop, real print dialog, real hydration) are named gates with no
artifacts yet. Distance to 100 = W1's five fork verdicts + the W3 artifact set, then
implementation per §6.
