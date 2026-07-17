# W2-SKELETON — the consolidated substrate skeleton (SPEC-PASS-3 §7, executed)

Pass-3 landing-readiness lane. The proven clone commits are PROMOTED into ONE
coherent branch per clone — **clone-side only** (VR-74: no real-repo code
writes this pass), ready to cherry-pick when the tranche implements W2/W4.
Every count below is machine-derived (VR-73); reporter lines pasted verbatim.

Clone roots (scratchpad
`/private/tmp/claude-504/-Users-mkbabb-Programming-sci-report/61ef4d2b-22d2-4c2d-a271-4c36ee42c6b5/scratchpad/`):

| Side | Clone | Branch | HEAD |
|------|-------|--------|------|
| glass-ui | `p6-glass-ui` | `w2-skeleton` | `2133597d` |
| latex-paper | `p6-latex-paper` | `w2-skeleton` | `aff7c73` |

What the skeleton consolidates: the P6 extraction (`ea7fac8c`), the
P7-runtime-proven shapes from `virt-p4 @ e796519` (hydration guard,
fonts.load derivation, wall-clock scrollend scrollTo, print discipline), the
pass-3 E1 F-7 settled CSS (`traces/f7/F7-VERDICT.md`), the F-1 constants
(64/4/true), and the named residuals (VR-53 source-unify, VR-56 fence, VR-75
docs, VR-76 rule) — expressed in code, suites green, twin gate run across all
four repos.

---

## 1. THE CHERRY-PICK MANIFEST — glass-ui side

Target: the real `/Users/mkbabb/Programming/glass-ui` root (tranche line at
`9d68b85e` when the skeleton was cut). Apply IN ORDER; each commit is
self-contained and suite-green at its own tip.

| # | Commit | Content |
|---|--------|---------|
| 1 | `ea7fac8c` | **The P6 extraction** — the document-native viewport core at `./virtual` (9 modules: `useSectionViewport`, `measurementRegistry` + width epoch, `measurementPipeline` shared-RO, `settleGate` C-3 epoch contract, `settleLoop`, `anchorMath` (`compensateScrollTop`), `hashTarget`, `scrollToSection` + adapters, `activeResolution`); sidebar re-expressed over VR-49 `resolveActivePosition` + PRM snap + `scrollSource:'window'`; the structural `ElementRef` boundary contract; **VR-51 peer-meta** (`@lucide/vue`, `class-variance-authority`, `clsx`, `reka-ui`, `tailwindcss` → optional; verified in the clone `package.json`). |
| 2 | `b74875c3` | **The P7-proven navigation + settle shapes** — `scrollToSection`: wall-clock ≥3 s arrival bound (`maxTravelMs`, `MIN_TRAVEL_MS=3000` — the clone's 240-FRAME cap was a VR-23 regression: frames compress under throttle/high-refresh), unconditional pre-wait commit (R10 never-scrolls fix), `TargetRect.scrollMarginTop` geometry-derived by the adapters (VR-58; **closes G-c5 at the root** — `createElement/WindowViewportAdapter` read computed style themselves), `behavior` in `LandingReport`, injectable `now()`. `settleGate`: the leading GRACE frame (post-fonts RO wave lands its epoch bumps before the first comparison). Tests updated to the new contracts. |
| 3 | `fd516982` | **VR-13 F-1 constants** — `mount` default `'governed'`; `EAGER_AUTO_SELECT_THRESHOLD = 64` (exported; decomposed S_cross, `traces/F1-VERDICT.md` R3 + `f1-smalln.json`); `DEFAULT_BATCH_SIZE = 4`; `idleRevealAll: true`; explicit-object form = explicit governed opt-in (no auto-select). **VR-25 hydration-damage guard** ported from P7: governed is client-render-only; first-item-id-present at setup forces eager with a console.warn — never truncates. New tests: 80-item governed default, the 64/65 boundary, the guard (with SSR-marker + warn assertions). |
| 4 | `2b27d7c7` | **VR-53 source-unify** — the core exposes `getPositions()` (the live sorted scroller-coordinate table); `useScrollTracker` gains `positionSource`: when present, the core table is the ONE position authority and the per-tick `getElementById` walk never runs (test-asserted: `getElementById` uncalled on the scroll path); the DOM walk demotes to the standalone-sidebar fallback. `roots[0].id` init preserved (VR-49.4 regression floor). |
| 5 | `2133597d` | **The rules expressed** — VR-56 fence reworded at the owning pipeline ("ONE observer per CONCERN," never "one wrapper"); VR-76 promoted to the standing surface RULE in `sidebar/types.ts` and made mechanically true (`ScrollToOptions.visibleCount` `Ref` → `RefLike`, the last nominal input ref on the consumer surface); VR-75 timeline-host law documented at the `./virtual` barrel (Beat.vue named as the measured-correct reference). |

**Diffstat** (`9d68b85e..w2-skeleton`, i.e. the full pick set):
`17 files changed, 2623 insertions(+), 72 deletions(-)`
(of which the pass-3 delta `ea7fac8c..w2-skeleton`:
`9 files changed, 352 insertions(+), 85 deletions(-)`).

**Gates (machine-verified at `2133597d`):**

```
Test Files  123 passed (123)
     Tests  1176 passed (1176)
```

`npm run build` exit 0 · `npx vue-tsc --noEmit` exit 0 (zero errors) ·
`npm run proof:virtual-window` exit 0 (all clauses green).

## 2. THE CHERRY-PICK MANIFEST — latex-paper side

Target: real `master` (now `f246a11`; the base `4775845` + `952c897` are
already on it). Apply IN ORDER:

| # | Commit | Content |
|---|--------|---------|
| 1 | `62a18e7` | **The P6 consumption** — twins DELETED (windowing seam + the whole `tracking/` suite + `useSidebarFollow`, −1,582 LOC), glass-ui runtime dep, `usePaperViewport`/`useHashNavigation` adapters, `/vue` re-exports the sidebar suite (VR-32 floor), `FlatPaperSection extends FlatSection` (VR-52), `toPaperSearchItems` (VR-61, unit-tested). |
| — | `1616c6b` | **SKIP when landing** — this is the clone-side cherry-pick of `952c897` (the B-2 `validate.ts` control-space fix), already on real master. Kept in the clone so its suite is green standalone. |
| 2 | `12ed7e4` | **The paper substrate consumption** — `deriveFontFaceProbes` ported from P7 (VR-19: derived from transform output, never hardcoded; face-less papers derive `[]`; wired as the `usePaperViewport` default — the hardcoded `KATEX_FONT_PROBES` is REMOVED, superseded); `PaperSection` emits `data-lp-section` + `data-lp-id` + inline `--lp-section-size` + min-normalized `headingLevel` (VR-26) + `aria-labelledby` named regions (VR-27) + `ctx.viewport` self-registration (`PaperViewportRegistrar` on `PaperContext`, additive); `PaperSectionContent` carries `headingLevel` (+1 recursive) and `sizeHint` documented ROOT-only = `subtreeEstimatedHeight` (F-7b); `flattenPaperSections` gains `headingLevel` + subtree-inclusive `subtreeEstimatedHeight` (tested: per-root subtree sums ≡ per-node own totals — the F-7b honesty identity). |
| 3 | `aff7c73` | **The F-7 SETTLED CSS shape** (`traces/f7/F7-VERDICT.md`) in `theme.css`: `display: flow-root` on `[data-lp-section]` and `.section-body` in ALL states (G-g — CV toggle geometry-neutral by construction); `content-visibility: auto` + `contain-intrinsic-size: auto var(--lp-section-size, 640px)` on `.paper-article > [data-lp-section] > .section-body` ONLY (arm ii × arm A); `@media print` escape on `.section-body` + the W3a fragmentation hardening (`break-inside: avoid` on `.math-block`/`.paper-figure`); the VR-28 PRM CSS arm; the VR-37 full-color two-tier token seam demonstration sites. |

**Diffstat** (`4775845..w2-skeleton`, src+tests+package.json — dist churn
excluded): `27 files changed, 916 insertions(+), 1393 deletions(-)` — a NET
DELETION of 477 lines (of which the pass-3 delta `62a18e7..w2-skeleton`
src+tests: `11 files changed, 456 insertions(+), 25 deletions(-)`).

**Gates (machine-verified at `aff7c73`, with the sibling `fourier-analysis`
symlinked so the golden block RUNS — the former B-2 RED test executes against
the fix and passes):**

```
Test Files  15 passed (15)
     Tests  136 passed (136)
```

`npm run build` exit 0 · `vue-tsc --noEmit`: zero NEW errors (the
`src/grammar/compile.ts` bbnf-lang/`.bbnf`-module errors reproduce
byte-identically at the untouched base `62a18e7` — a pre-existing
environment drift, recorded, not VIRT's).

Count reconciliation (VR-73 honesty): the §2 pin `14 files / 127 passed` is
real master @ `952c897`. The skeleton's `15 / 136` differs by exactly the
known deltas: P6 deleted `virtualSectionLayout.test.ts` and added
`paperViewport.test.ts`, pass 3 added `fontFaceProbes.test.ts` (+4) and two
flatten tests (+2). Without the sibling symlink the 9 golden tests skip
(`describe.skipIf(!hasPaper)` — fixture lives in the fourier-analysis
checkout): `127 passed | 9 skipped (136)`.

## 3. THE VR-72 TWIN GATE (path-qualified, all four repos)

Run at skeleton cut: clones for glass-ui + latex-paper; REAL checkouts
read-only for fourier-analysis + words. `node_modules`/`dist` excluded.

| Twin | State | Where |
|------|-------|-------|
| `useSidebarFollow.ts` | **EXACTLY ONE** | glass-ui `composables/sidebar/` |
| `useScrollTracker.ts` | **EXACTLY ONE** | glass-ui `composables/sidebar/` |
| `useScrollTo.ts` | **EXACTLY ONE** | glass-ui `composables/sidebar/` |
| `useLazyLoader.ts` | **EXACTLY ONE** | glass-ui `composables/sidebar/` |
| `useClickDelegate.ts` | **EXACTLY ONE** | glass-ui `composables/sidebar/` |
| `useTreeIndex.ts` | **EXACTLY ONE** | glass-ui `composables/sidebar/` |
| sidebar `types.ts` (qualified) | **EXACTLY ONE** | glass-ui `composables/sidebar/` (latex-paper `vue/tracking/types.ts` = ZERO, deleted) |
| `useVirtualSectionWindow.ts` | glass-ui + words | **W5 pending** — F-9 branch (b): words deletes, becomes first external consumer |
| `virtualSectionLayout.ts` | glass-ui + words | W5 pending (same) |
| `useWindowedStore.ts` | glass-ui + words | W5 pending (same) |
| fourier `search/` (7 files) | fourier only | **W5 pending** — VR-60 net-deletion when fourier composes glass-ui `./search` |

Gate verdict: **PASS for the {glass-ui, latex-paper} pair** (every twin
resolves to at most one path, sidebar family exactly one); the two pendings
are exactly the F-9/VR-60 W5 migrations, named and bounded. `useVirtualGrid`
correctly absent from the list (not a twin; stays words-local).

## 4. RESIDUALS — what W2's implementing wave must still do

Landing mechanics:
1. **Cherry-pick order as manifested** (glass-ui 5 picks; latex-paper 3 picks,
   skipping `1616c6b`). Conflict surface: glass-ui pick 1 was cut on
   `9d68b85e` (tranche/BG line) — rebase-verify against the then-HEAD;
   latex-paper picks apply onto `f246a11+`.
2. **glass-ui semver cut per VR-71 × F-9 = 4.3.0** (the additive corner
   HOLDS in this skeleton: no published `./virtual` export removed or
   signature-broken vs 4.2.0 — the windower family is untouched, the core
   family is net-new; `EAGER_AUTO_SELECT_THRESHOLD`/`DEFAULT_BATCH_SIZE` are
   additive exports). 5.0.0 fires only if W5's words generalization breaks a
   windower signature.
3. **latex-paper v0.3.0 changeset + MIGRATION** (VR-31/32): every removed
   symbol + replacement; the G-c6 hydration-guard duplicate-id note; the
   G-c5 note NARROWED — the shipped adapters now derive `scroll-margin-top`
   themselves; only a fully-custom `ViewportAdapter` must supply
   `TargetRect.scrollMarginTop`.

Deferred by design (assigned waves):
4. **Consumer wiring of the new fields** (W4/W5): pass
   `:size-hint="item.subtreeEstimatedHeight"` at ROOT sections and
   `:heading-level="item.headingLevel"` where consumers render
   `PaperSection`/`PaperSectionContent` directly; docs/demo update.
5. **VR-53 wiring** (W4/W5): compose `positionSource: viewport.getPositions`
   where sidebar + core coexist (`usePaperReader`, fourier `PaperSidebar`).
   The seam exists and is tested; the composition is consumer-side.
6. **VR-15/16 estimator recalibration** (W4, from F-7): narrow-width +
   dense-math; gate = pre-settle/settled ≥ 0.85 at 390×844 on the real
   fourier paper (current 0.759).
7. **The W3b Reader-View finding** (W4 owner): `.section-header` matches
   Readability's `unlikelyCandidates` → Firefox Reader View strips all
   headings; the rename arm restores. Touches the theme sticky selectors +
   `PaperSection.vue` — consumer-coordinated, NOT expressed in the skeleton.
8. **VR-62/65/66 search gate-asserts** (W2 goal row): assert-don't-rebuild
   tests on the glass-ui `./search` surface (cache lifecycle, index-from-
   transform, Highlight-API fallback) — not part of the P6/P7 pick set.
9. **W5/W6**: words windower deletion (3 files, +3 import edits per
   F9-VERDICT), fourier `search/` deletion (7 files), the atlas
   `j0-absorb-leaf-census` gate update when the first `./virtual` consumer
   lands, the path-qualified VR-72 grep in CI.

New residual found BY this lane (recorded in-code at the cast site):
10. **The `MaybeRefOrGetter` nominal-Ref arm** —
    `SectionViewportOptions.items` carries vue's `MaybeRefOrGetter`, whose
    `Ref` arm is nominal and does NOT cross the `file:` boundary: consuming
    `toValue(options.items)` inside latex-paper needs a re-brand cast (see
    `usePaperViewport.ts`). VR-76's letter covers element refs; its spirit
    covers this — W2 should either accept the documented cast or add a
    structural `ItemsSource<T> = readonly T[] | { value: readonly T[] } |
    (() => readonly T[])` to the surface. Passing getters/arrays (every
    known consumer) is unaffected.

## 5. EVIDENCE INDEX

- Clone branches: `p6-glass-ui @ w2-skeleton (2133597d)`,
  `p6-latex-paper @ w2-skeleton (aff7c73)` — scratchpad path above.
- F-7 shape source: `docs/tranches/VIRT/traces/f7/F7-VERDICT.md` (120 lanes).
- F-9 branch source: `docs/tranches/VIRT/traces/F9-VERDICT.md`.
- W3 runtime evidence the ported shapes carry:
  `virt-p4 @ 761ba9d` (W3a: compensateScrollTop real WebKit, print),
  `virt-p4 @ 4391629` (W3b: hydration guard load-bearing, axe, PRM, reader).
- F-1 constants: `docs/tranches/VIRT/traces/F1-VERDICT.md` + JSONs.
