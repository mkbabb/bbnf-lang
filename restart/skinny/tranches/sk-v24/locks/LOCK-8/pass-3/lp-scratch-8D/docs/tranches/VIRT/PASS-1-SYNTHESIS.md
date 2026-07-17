# VIRT — SOTA Window-Virtualization Pass — PASS-1 SYNTHESIS (AGGLOMERATED)

Status: **PASS 1 of the convergence loop, agglomerated.** This file merges the PASS-1
SPEC (`SPEC-PASS-1.md`), four concrete/spec-augmented prototype lanes (P1 TanStack-core
adoption, P2 browser-native CV/CIS, P3 measurement+anchor seam, P4 API+SSR/print/a11y),
and four critique lanes, into one reconciled state-of-the-fork + corrected requirement
set + critique ledger + convergence verdict + PASS-2 reseed brief.

Target: `@mkbabb/latex-paper` v0.2.1 → v0.3.0, repo `/Users/mkbabb/Programming/latex-paper`
(HEAD `4775845`, clean). All work lands at the library root (ROOT-REPO LAW), published via
changesets CI. VR-n cite the SPEC's requirement numbers; F-n are the open forks; G-n the
gap table.

**Overall convergence: 82%.** Justification in §D. The architecture fork (VR-12,
document-native) is DECIDED with strong multi-lane evidence and no surviving blocker;
one open fork (F-1, the mount-default premise) caps the number because it gates a
consumer-facing default and a released contingency (VR-14), and three genuine blockers
(one factual error, two engine/contract-verification gaps) remain for PASS 2.

---

## A. STATE OF EVERY ARCHITECTURE FORK AFTER PROTOTYPING

### A.0 The keystone fork — adopt-TanStack vs document-native — DECIDED (document-native)

**Decision stands: DOCUMENT-NATIVE (VR-12).** Both the concrete adopt-lane (P1) and the
concrete native-lane (P2) were built, committed, and run; the decision survives on its
one load-bearing premise, and two of the SPEC's three supporting arguments for it are
now falsified and must be corrected (they do not change the decision — they were
over-claims in its favor).

- **The load-bearing premise HOLDS (confirmed by both lanes' DOM counts):** a
  DOM-unmounting virtualizer structurally breaks VR-4..9 (find-in-page, `#fragment`/
  `:~:text=`, AT heading tree, print, reader mode, view-timeline). P1's own real-Chromium
  probe shows **≤9 of 200 sections mounted** at any moment — no windowing-math quality
  reaches the unmounted sections. P2 shows the native path delivers all six semantics arms
  green by measurement (find lands in section 219 from the top of the document in Chromium
  AND WebKit 26.5; print emulation 220/220; cold `#fragment` lands exactly). This is the
  axis on which the fork was correctly decided.

- **SPEC §2.2 over-claim #1 — the "template break" argument — FALSIFIED by P1.** P1
  preserved fourier's spacer template unchanged: `topSpacerPx = firstItem.start −
  scrollMargin`, `bottomSpacerPx = getTotalSize() − (lastItem.end − scrollMargin)`;
  `measureElement` reads sizes only, never positions, so absolute/`translateY` positioning
  is NOT required. Verified against fourier's real `PaperArticleWindow.vue` template. The
  claim "TanStack's absolute-position model forces every consumer to abandon the spacer
  template" is FALSE. **PASS-2 must strike it from §2.2.**

- **SPEC §2.2 over-claim #2 — "hundreds of consumer-visible lines" — FALSIFIED by P1.** The
  full adopt-core-under-the-existing-API adapter is a **net −144 source-line deletion** with
  **zero consumer edits** (all 9 return members + 7 option fields of `useVirtualSectionWindow`
  mapped onto `@tanstack/virtual-core` 3.17.3; fourier's `PaperView.vue`/`useScrollNavigation`/
  `PaperArticleWindow.vue` run unchanged). Bundle SHRINKS (`dist/vue.js` 69,417→65–66 KB raw)
  because tsup externalizes the dep and 422 lines of hand-rolled math leave. **PASS-2 must
  strike the "hundreds of lines" framing.**

- **SPEC §2.2 argument #3 — dependency cost 3→4 — HOLDS but is small.** `virtual-core` is
  dependency-free, +6,853 B min+gzip consumer cost. Real, but not decisive on its own.

**Net effect on the fork:** the decision is correct; its justification must rest on the ONE
true premise (unmounting breaks VR-4..9), not the two false ones. The adopt-core path is
re-graded UP: it is a **TESTED, consumer-invisible, dependency-light VR-14 fallback** that
delivers VR-1..3 at TanStack grade (P1 measured **0.0px** remeasure drift when above-viewport
sections grew 80px each; far-nav landed section 190 at 0.0px). See A.1.

### A.1 The VR-14 contingency fork — RE-RANKED (two named fallbacks, ordered)

The SPEC named exactly one fallback (R6 PATH-B `hidden="until-found"` + beforematch). P1
supplies a second, cheaper, tested one. PASS-2 must record BOTH, ranked:

1. **Fallback A — adopt `@tanstack/virtual-core` under the existing/`usePaperViewport` API**
   (P1, TESTED): wins on migration cost (net LOC deletion, zero consumer edits, +6.9 KB
   gzip, zero transitive deps) and delivers VR-1..3 at measured TanStack grade. LOSES VR-4..9
   (unmounts). Use if F-1 falsifies the render budget AND semantics-arm preservation is
   negotiable at the failing scale.
2. **Fallback B — `hidden="until-found"` + beforematch on far-frontier sections** (R6
   PATH-B, NOT prototyped): keeps nodes in DOM so **preserves VR-4..5** (find + fragment
   nav) while deferring Vue patch. LOSES the Vue-render deferral that is unmounting's whole
   point (P4 flags `hidden=until-found` has never shipped in stable Safari — weakens it
   cross-engine).

Neither triggers unless F-1 falsifies the render-budget premise. P2's measurement makes
F-1 partial-settle *likely* (see A.2), so this fork is probably moot — but stays open until
F-1 lands throttled traces.

### A.2 The mount-default fork (F-1: eager vs governed vs baseline) — OPEN, NARROWED

This is the **one fork that caps convergence** (§D). Status after P2's benchmark:

- **What P2 SETTLED:** `content-visibility` defers the **layout+paint** half of the render
  budget exactly as claimed (native FCP 684ms vs eager 1636ms at 4× throttle, 220 sections)
  and matches the JS windower on scroll (native 9.57ms avg frame vs windowed 10.28ms, 0
  frames >33ms at 4× throttle — both beat eager-no-CV's 4 dropped frames).
- **What P2 did NOT settle (the central negative result, F-1's core):** CV **cannot** defer
  the Vue-mount + KaTeX `renderToString` cost. At 220 sections with unique (cache-defeating)
  math at 4× throttle, full eager mount produces a single **555ms blocking longtask** —
  over any 200ms budget, identical in dev and prod (so it is KaTeX + 68K-node insertion,
  not Vue dev overhead). Scaled to the real ~60-section consumers this is ~150–190ms at 4×
  (borderline) and ~45ms unthrottled (fine).
- **Consequence:** P2 recommends the **mount governor (VR-13) as the DEFAULT** (governed
  batches + `idleRevealAll`), with eager defensible only below ~100 sections. This is a
  *change* to the SPEC, which left the governor conditional on F-1.

**What still settles it in PASS-2 (F-1 experiment, must be pre-committed):** the three-way
comparison (eager vs governed batchSize-N vs 0.2.1 windowed baseline) on (a) fourier's real
paper and (b) a ≥120-section stress fixture, at 4× CPU throttle, with a **pre-committed
PASS/FAIL decision rule** — the SPEC's critique flagged that no rule currently exists for
"eager passes budget but is worse than the windowed baseline." Decision rule to adopt:
*governor is the default; eager is auto-selected below a section-count threshold derived
from the fixture where eager's longtask crosses 200ms at 4× throttle; the windowed
baseline is retired regardless (it fails VR-4..9).*

### A.3 core-vs-adapter (`virtual-core` alone vs `@tanstack/vue-virtual`) — DECIDED (core alone)

Only relevant to Fallback A. P1 SETTLED it: `virtual-core` alone suffices (one `shallowRef`
+ `onChange→triggerRef` + `onScopeDispose`); the Vue adapter package adds nothing needed.

### A.4 element-scroller vs window-scroller (vft scroll model, F-6) — DECIDED (both; window has a real consumer)

P4 SETTLED F-6 by design-read of the atlas source: `useDockStepper.ts` uses `scrollIntoView`
+ `useDocumentScrollProgress`, and `base.css:37` uses `overflow-x: clip` with an explicit
comment that it never establishes a scroll container — so the **vft paper scrolls the
document (window)**. The window `ViewportAdapter` therefore has a real consumer. P1 confirms
the injectable adapter seam makes window-mode a one-arg switch (VR-40). *Caveat (F-6
residual):* one confirmed line from the V-tranche dock-spec owner is still wanted, and vft's
`LATEX-PAPER-TOC.md` does not yet record the `.paper-article` scoping + `scroll-margin-top`
adaptations the substrate requires — a binding-doc gap, not an architecture gap.

### A.5 measurement-registry key shape (width-composite vs width-epoch) — DECIDED (width-epoch)

P3 SETTLED it: a single bumped-integer **width EPOCH** (entries epoch-tagged; `read()` =
fresh-only, `readAny()` = estimate-grade) beats the brief's composite `{sectionId,
containerWidth}` key, because old-width heights are unreadable dead weight (RO re-fires every
box on width change). Unit-tested including scrollbar-appearance width deltas.

### A.6 estimator load-bearing? (F-4) — DECIDED for Chromium (scrollbar-honesty only), WebKit open

P3 SETTLED F-4 for Chromium: **landing accuracy is seed-independent** (0.06–0.19px error
across a 2–4× underestimate AND a 1.55× overestimate seed). Estimator quality affects only
*scrollbar honesty* (bad seeds inflate total height 43,748→66,312px while reading). P2
independently confirms `contain-intrinsic-size: auto` self-calibration converges only ~95%
after fast scrolls (residual 4.7% deficit, first-sweep jitter to 2,480px). **Merged verdict:**
downgrade VR-16 from load-bearing-for-navigation to *optional scrollbar-honesty polish*, but
KEEP a simple width-aware estimator (not delete it) for scrollbar stability. P2 additionally
kills the RO→`--lp-section-size` layout writeback arm as inert (`auto` memory dominates).
**Residual:** F-4 is Chromium-only; re-check under WebKit (fold into F-2).

### A.7 activeId mechanism (F-5: layout-read offsetTop vs IntersectionObserver) — OPEN (leaning offsetTop)

Not directly prototyped as an A/B. P4 shipped a working `activeId` via rAF-coalesced binary
search over a real-DOM `offsetTop` position table (idiomatic, cheap). P3 notes native scroll
anchoring makes the settle loop nearly idle. No jank trace comparing the two mechanisms
exists yet. **Stays open** — but low-risk: P4's offsetTop approach is already green in tests,
so F-5 is a *polish/verify* fork, not a blocking one.

---

## B. MERGED, CORRECTED REQUIREMENT SET (VR-n with convergence notes)

Grouped by area. Each note: **[status]** + evidence + any correction PASS-2 must apply.

### SOTA definition (VR-1..11)
- **VR-1** dynamic per-item height stable under font/image/width change — **[converged]**
  P3 registry + P2 CV memory both deliver; the width arm re-check folds into F-2.
- **VR-2** no jump on above-viewport size change — **[converged, evidence-strong]** P1
  measured 0.0px (JS path); P2 measured +400px growth → scrollTop +400, 0px viewport shift
  (native anchoring). Both paths satisfy it.
- **VR-3** reliable programmatic nav to any id — **[converged]** P3 LandingReport 0.06–0.19px;
  P2 far-nav exact in Chromium (WebKit needs the settle loop — see VR-23).
- **VR-4** find-in-page across whole doc — **[converged for Chromium+WebKit26.5]** P2 window.find
  anchored in last section both engines; **only native path satisfies it** (P1 confirms unmount
  breaks it). Safari <26 degradation open (F-2).
- **VR-5** native `#fragment`/`:~:text=` — **[converged Chromium; WebKit short]** P2/P4 cold
  fragment lands at scroll-margin-top in Chromium; **WebKit lands 257px short without the
  settle loop** — makes VR-23 mandatory not optional.
- **VR-6** heading/landmark tree in a11y tree — **[converged, corrected]** requires the VR-26/27
  corrections below.
- **VR-7** `window.print()` complete — **[converged, corrected]** CV half pure-CSS (P4 68-page
  PDF artifact); requires the VR-24 Promise-print contract below.
- **VR-8** reader mode whole article — **[converged]** P2 full innerText present under CV.
- **VR-9** view-timeline reveals possible — **[OPEN]** F-3 not prototyped (see forks).
- **VR-10** off-screen render deferred, flat frame budget — **[PARTIAL]** layout/paint half
  converged (P2); the Vue-mount half is F-1 (the capping fork).
- **VR-11** all ten at O(10–200) scale — **[converged as the target statement]**.

### Architecture (VR-12..14)
- **VR-12** document-native architecture — **[DECIDED]** §A.0. Correction: strike the two
  false §2.2 arguments (template-break, hundreds-of-lines).
- **VR-13** mount governor — **[promote to DEFAULT]** per P2's F-1 partial result (§A.2);
  cross-reference VR-46 (frontier-monotonicity unit test) so the invariant is not orphaned;
  governor is **client-render-only** (P4: governed mount over SSR HTML silently truncates —
  see VR-25).
- **VR-14** contingency — **[re-ranked, two fallbacks]** §A.1; adopt-core is now the tested
  first fallback.

### Measurement model (VR-15..21)
- **VR-15** `estimatedHeight` seeds `contain-intrinsic-size: auto Npx` via `--lp-section-size`
  — **[converged]** P3/P2 both confirm; re-aim VR-15's acceptance metric at total-height/
  scrollbar honesty, not anchor error (F-4).
- **VR-16** width-aware estimator — **[DOWNGRADED to optional]** §A.6. Correction: the SPEC
  text describes a change to a math floor (`104`) that ALREADY exists at that value in
  `flattenPaperSections.ts:66`, and `operatorClusters` is undefined; PASS-2 must either
  define the counting rule or (preferred, per F-4) drop the math-model change and keep the
  simplest estimator.
- **VR-17/18** shared per-instance ResizeObserver registry with `data-lp-id` identity —
  **[converged]** P3 unit-tested; specify width-EPOCH semantics (§A.5), note delta accounting
  starts at the SECOND write, and tighten the "no RO storm" claim to "0 nonzero-delta
  batches" (the RO can still fire same-height callbacks — P3 critique).
- **VR-19** font-ready gate — **[CORRECTED, load-bearing falsification]** P3 proved
  `document.fonts.ready` alone does NOT gate late CV-triggered KaTeX face loads (gate passed
  at 49ms; swap at 404ms). **PASS-2 must make `document.fonts.load('1em KaTeX_Main', …)` over
  the transform-derived face list REQUIRED, not optional.**
- **VR-20** native scroll anchoring for jump-correction — **[converged Chromium]** P2/P3; add
  the Playwright regression gate; WebKit arm open (F-2).
- **VR-21** anchor stability under above-viewport growth — **[converged Chromium]** 0.00px
  (P3), 0.0px (P1/P2). **WebKit UNVERIFIED**; the prototyped `compensateScrollTop` net-delta
  fallback is load-bearing there — probe in F-2.

### Semantics arms (VR-22..30)
- **VR-22** WebKit find-fix version floor — **[BLOCKER — see C-1]** P4 pins it to Safari 26.0
  but the critic's independent search could not corroborate, and a 2026-05 report suggests CV
  content still missing in WebKit 26.2+. Downgrade from SETTLED to **PROVISIONAL** until the
  W3 real-WebKit gate. F-2 owns it.
- **VR-23** settle-aware fine-alignment scrollTo — **[CORRECTED, upgraded to mandatory]** P2
  (WebKit 257px short) + P3 (Chromium smooth-scroll 1515ms). Corrections: arrival detection
  **`scrollend`-primary with ≥3s time fallback** (never the prototyped ~1.5s/90-frame cap,
  which truncated); long jumps (>~10 viewport-heights) default to instant; codify the
  user-interrupt abort (unexplained scrollTop move aborts; corrections never re-animate); gate
  on `LandingReport{errorPx≤1, settleFrames≤10}`. Fix the near-document-bottom false-interrupt
  edge (clamp exemption).
- **VR-24** print — **[CORRECTED]** P4: programmatic `window.print()` blocks its own task, so
  `beforeprint` + Vue flush cannot land; `revealTo`/`revealAll` must return `Promise<void>`
  and consumers `await revealAll()` before `print()`. CV half is pure-CSS (verified). Bind the
  "last-section present" claim to the existing print-PDF Playwright artifact, not prose. Note:
  the 0.2.1 windowed layer's print output is rescued by an async recalc (timing-dependent, not
  correctness) — strengthens retire-not-modernize.
- **VR-25** SSR — **[UPGRADED to VERIFIED, with a corrected boundary]** P4: full `renderToString`
  of the dist artifact renders the complete doc; eager hydration = zero mismatches; **governed
  mount over SSR HTML SILENTLY destroys server content** (3 sections → 1, no warning). So:
  DOM/first-paint is SSR-safe; the viewport core's runtime arms (`document.fonts`,
  ResizeObserver, matchMedia, beforeprint) are **post-hydration-only**; governed mount is
  **client-render-only**. Keep SSR ARCHIVED (no consumer) but with these guards documented.
- **VR-26** heading level — **[CORRECTED]** P4: raw `sourceLevel` clamp emits colliding h1s;
  replace with **min-normalized `headingLevel`** as an ADDITIVE `FlatPaperSection` field
  (unit-tested chapterless {1,2}→{2,3} and chaptered {0,1,2}→{2,3,4}); legacy-default prop
  keeps 0.2.1 byte-compat.
- **VR-27** DPUB-ARIA — **[CORRECTED, falsification]** P4 + critic confirm `role="doc-section"`
  does NOT exist in DPUB-ARIA 1.1; replace with **`aria-labelledby` named regions** (h2 `id` +
  section `aria-labelledby`), SSR-asserted.
- **VR-28** PRM gating on all smooth scrolls — **[converged]** P4 exports PRM helpers; note
  `useSidebarFollow`'s damping loop needs its own PRM snap.
- **VR-29/30** reader-mode / focus — **[converged]** covered by full-DOM native path.

### API surface (VR-31..46)
- **VR-31** semver — **[VERIFIED, corrects R5]** independently re-ran `semver`: `0.3.0` does
  NOT satisfy `^0.2.1`; `0.2.9` does. v0.3.0 is opt-in; removing `useVirtualSectionWindow`/
  `virtualSectionLayout` without a shim is licensed (no-legacy precept). **Strongest evidence
  the pass reasoned from artefacts.**
- **VR-32/33** additive everywhere except the removed windowing seam — **[converged]** fourier's
  spacer-consumer break (VR-33) is real (`PaperArticleWindow.vue:12-16,65-129`), migration is a
  net deletion. Note the double-containment trap (C-2).
- **VR-35/36** promote `scrollTo(id)` (2 consumers) into the library; keep fourier's search/
  progress/nav-stack chrome consumer-side (1 consumer) — **[converged, no overfit]**.
- **VR-37** two-tier theme token seam — **[CORRECTED, falsification]** P4 live-probed:
  `hsl(var(--primary))` with fourier's FULL-color glass-ui token computes to inherited
  `rgb(0,0,0)` — the hsl-triple fallback tier is dead code. Corrected seam: **`var(--lp-x,
  var(--x))` full-color both tiers**, which is ALSO a 0.2.1 bug fix and resolves oklch exactly
  (verified `oklch(0.46 0.085 135)`). Unblocks vft's earth palette.
- **VR-40** injectable ViewportAdapter seam (`{observeRect, observeOffset, scrollTo}`) —
  **[converged, transposed from P1]** made the composable deterministically testable for the
  first time; window-mode is a one-arg switch.
- **VR-43** `v-memo`/`v-once` co-location — **[OPEN, untouched]** no lane exercised it.
- **VR-46** governor-frontier monotonicity unit test — **[converged as requirement]**
  cross-referenced from VR-13.
- Surface deltas adopted (P4): curried per-id-memoized `registerSection` accepting
  `Element|ComponentPublicInstance|null`; optional `PaperContext.viewport` self-registration
  (kills consumer ref plumbing, VR-32-additive); **`leadingOffsetPx` DELETED** in favor of real
  DOM geometry + CSS `scroll-margin-top` (native fragment nav honors it — probed 47.8px≈48);
  nested-anchor `scrollTo`; `activeId` null outside the embedded document (vft mid-route);
  `useSidebarFollow` needs `scrollSource:'window'`.

---

## C. CRITIQUE LEDGER (deduped; each → PASS-2 work or dismissed)

### Blockers (must close before 100)

- **C-1 [→ PASS-2 W3, F-2]** VR-22/F-2 stamped SETTLED on an unverified Safari-26.0 floor;
  P4's own note and the SPEC §4 both flag it needs a real-WebKit gate; a 2026-05 contrary
  signal exists. **Action:** downgrade F-2/VR-22 to PROVISIONAL; the closing evidence is a
  real-WebKit find-over-CV run in W3. (Source: P4 critique blocker; SPEC F-2.)

- **C-2 [→ PASS-2 W2/W3, re-run]** P3's "172 tests" figure is FALSE — independently re-verified
  the suite is **145 passing + 9 skipped = 154 total** (viewport = 27 of the 145). Any PASS-2
  acceptance criterion citing "172" would gate on a wrong number. **Action:** correct the count
  everywhere before it propagates. (Confirmed by re-run this pass.)

- **C-3 [→ PASS-2 W2]** P3 `settleGate` doc/impl contract mismatch: the comment claims a
  "shared promise per epoch" but no epoch exists in the code; concurrent `whenSettled()` callers
  race a shared module-level `dirty` flag (an initial-load hash landing + a user nav can
  overlap and clear each other's dirty signal, resolving the gate mid-reflow). **Action:** fix
  the sharing/epoch semantics (or the doc) before the P3 modules port to W2. (Source: P3
  critique blocker.)

### Gaps (assigned to PASS-2 work)

- **G-a [→ F-1, W1]** F-1 PASS/FAIL decision rule is undefined for the three-way comparison
  (eager passes budget but worse than baseline?). Pre-commit the rule in §A.2. (SPEC critique.)
- **G-b [→ W2]** VR-16 requirement text describes a change to the `104` math floor that already
  exists at that value, and `operatorClusters` is undefined; resolve per F-4 (prefer dropping
  the math-model change). (SPEC critique.)
- **G-c [→ W1]** DUAL SCROLL AUTHORITY (Fallback-A only): if adopt-core is used, `virtual-core`'s
  `applyScrollAdjustment` and fourier's `correct()` loop both write scrollTop; the concurrent
  passive-growth-during-scroll case on real variable heights was never exercised. Probe if
  Fallback A is pursued. (P1 critique.)
- **G-d [→ W1, F-2/F-4]** No WebKit/Firefox coverage for VR-21 native anchoring, VR-23 smooth
  timing, F-4 estimator; the `compensateScrollTop` fallback is load-bearing on non-anchoring
  engines and unit-tested but never run in a real one. (P3 critique; P2 partially covers WebKit
  26.5 for find/print/anchoring — merge results.)
- **G-e [→ W2]** P3 seam is wired into ZERO Vue components — the "serves both consumers" contract
  is untested against a real `PaperSection` render; do one real component integration in W2
  before judging the surface non-overfit. (P3 critique.) P4's viewport core already integrates
  `PaperSection` self-registration, so this is partly closed by cross-lane; unify the two shells.
- **G-f [→ W5]** Double-containment "resolution" (delete fourier's `.deferred-section`) MOVES
  the CV containment root from the wrapper div to the inner `<section>` — the exact geometry
  shift the note warns of; verify post-migration with a live fourier nesting probe. (P4 critique;
  overlaps F-7.)
- **G-g [→ W2]** Inter-section spacing: theme's `.paper-section:not(:last-child){margin-bottom}`
  is DEAD under any wrapper-per-section template, and containment un-collapses trailing margins
  by 12–16px/section — the substrate must own spacing at the host wrapper. (P2 finding.)
- **G-h [→ W2/W3]** VR-24 print task-ordering and VR-25 SSR-truncation are reasoned/probed in
  happy-dom, not a real print dialog / real hydration — bind to runtime artifacts in W3. (P4
  critique.)
- **G-i [→ F-8, W1]** The missing R3 dossier: G1-G14 is single-sourced (this pass's own reads,
  8/14 spot-verified true this pass — G1,G3,G4,G5,G7,G11,G12 + substrate-absence). `useSidebarFollow`
  (234 L) unread by any lane. Do the adversarial re-read to catch a missed gap CLASS. (SPEC F-8.)
- **G-j [→ W2]** Private-API dependency (Fallback-A only): P1 calls `virtual-core` internals
  (`_willUpdate`, `measurementsCache`, `scrollRect`) with no pin/contract test; `initialRect`
  defaults to {0,0} (degenerate cold frame); `rangeExtractor` closes over virtualizer state
  (off pure-Range contract). Add a pin + contract test only if Fallback A is kept. (P1 critique.)
- **G-k [→ W1]** All P1/P3/P4 numbers are un-throttled Apple Silicon; settle-loop budget
  (10 frames) and settleFrames≤3 observations may not hold at 4× throttle — re-measure in F-1.
  (P3 critique.)
- **G-l [→ W5]** vft's `LATEX-PAPER-TOC.md` records neither `.paper-article` scoping nor
  `scroll-margin-top`, both required by the substrate; the "binds cleanly" claim is incomplete
  until the binding doc records them + confirms the window scroll model with the V-tranche owner
  (F-6 residual). (P4 critique.)
- **G-m [→ W1]** Fleet hygiene: prototype lanes sharing the Playwright MCP must run probes in
  their own `newContext()` (P1 & P3 both lost cycles to shared-tab contention). (P1/P3.)

### Dismissed (with reasoning)

- **D-1** "TanStack adds hundreds of consumer-visible lines / forces a template break" — DISMISSED
  as FALSE (P1 falsified both); folded into the VR-12/§2.2 correction, not a live gap.
- **D-2** P3 built artifacts (`dist/vue.js{,.map}`) committed into the prototype clone — harmless
  in a throwaway clone; dismissed as a live issue, noted only as a porter caution (never travel
  into the real landing).
- **D-3** `hidden="until-found"` contingency needing its own prototype — DISMISSED for PASS 2:
  P2's F-1 partial-settle makes the governor-default likely sufficient, so neither VR-14 fallback
  is expected to trigger; prototype only if F-1 falsifies the budget. (Keeps KISS.)
- **D-4** P1's `rangeExtractor` off-contract memoization fidelity loss — DISMISSED as a
  Fallback-A-only micro-issue, invisible in the probe; only matters if adopt-core is chosen.

---

## D. OVERALL CONVERGENCE: 82% — JUSTIFICATION

Per-target critique numbers: SPEC 83, P1 82, P3 72, P4 84 (P2 not critiqued but its
measurements are the strongest single evidence set and settle F-1-partial/F-2-find/F-4/F-7).
These are NOT averaged blindly. The number is set by the fork state and blocker severity:

- **The keystone architecture fork (VR-12) is DECIDED with strong, multi-lane, on-disk
  evidence** — this is what keeps the number in the 80s rather than the 60s. Two concrete
  clones (P1 adopt, P2 native) were built and run; the decision survives on its true premise
  and the falsified over-claims strengthen (not weaken) confidence because they were
  self-corrected against artifacts.
- **F-1 (the mount-default fork) is OPEN and caps the number hard.** It gates a consumer-facing
  default and a released contingency (VR-14). P2 *narrowed* it decisively (governor-as-default
  is the evidenced recommendation) but did not *close* it — no throttled three-way trace with a
  pre-committed decision rule exists. An open fork touching a shipped default caps convergence
  below ~85 by the loop's own rule.
- **Three genuine blockers remain** (C-1 unverified WebKit floor, C-2 false test count, C-3
  settleGate race) — none architectural, all mechanically closable in PASS 2, but each is a
  load-bearing correctness/evidence item per "gates close on evidence."
- **Offsetting upward:** the semantics arms (VR-4..8) are measured green on real browsers; the
  six VR falsifications (19, 22, 24, 25, 26, 27, 37) are corrections of real errors that would
  otherwise have shipped — the spec is MORE correct after this pass than its 83 self-grade,
  but the corrections themselves are un-landed and F-2/print/hydration need runtime re-proof.

**82%** reflects: architecture settled (+), F-1 open on a shipped default (−, the hard cap),
three closable blockers (−), and an unusually evidence-grounded corrected requirement set (+).
Distance to 100 = close F-1 with a decision rule + throttled traces, close C-1/C-2/C-3, run
the runtime re-proofs (real WebKit, real print, real hydration), settle F-3/F-5/F-8, and land
the substrate + a consumer together.

---

## E. RESEED BRIEF FOR PASS 2

**Research questions still open:**
1. **F-1 (the capping question):** three-way render-budget benchmark (eager vs governed
   batchSize-N vs 0.2.1 windowed) on fourier's real paper + a ≥120-section stress fixture, 4×
   CPU throttle, **with the §A.2 pre-committed decision rule**. Output: the mount default + the
   section-count threshold for auto-eager + the VR-14 trigger condition. JSON traces committed.
2. **F-2 (WebKit floor, absorbs C-1 + VR-21/23 WebKit arm + F-4 WebKit):** real-WebKit run of
   find-over-CV, `#fragment` landing, native scroll anchoring (does `compensateScrollTop`
   fallback trigger?), smooth-scroll timing. Pin the actual Safari version where the find fix
   ships; accept/quantify Safari <that degradation.
3. **F-3 (view-timeline × CV, VR-9):** do Beat-style reveals fire at correct ranges from the
   CV-skipped state? Chromium + Safari 26 probe → vft reveal-motion guidance.
4. **F-5 (activeId authority):** A/B `offsetTop`-layout-read (P4's shipped mechanism) vs
   IntersectionObserver-primary — jank trace + TOC-highlight-correctness log. Low-risk (P4 green).
5. **F-8 / G-i (missing R3):** adversarial re-read of the full 1,098-line layer + the unread
   `useSidebarFollow` (234 L), reconciled against G1-G14 to catch a missed gap class.

**Prototypes to DEEPEN:**
- **P3 measurement+anchor seam** → the W2 substrate skeleton, AFTER fixing C-3 (settleGate
  epoch/sharing), VR-19 (required `document.fonts.load`), VR-23 (scrollend-primary, ≥3s
  fallback, long-jump-instant, bottom-clamp interrupt exemption). Unify with P4's viewport core
  into ONE common API shell (G-e) so fork lanes stay comparable.
- **P4 API/SSR/print/a11y** → land the six VR corrections verbatim (they are falsifications):
  VR-27 aria-labelledby, VR-26 min-normalized headingLevel, VR-37 full-color two-tier tokens,
  VR-24 Promise-print, VR-25 client-render-only governor, VR-22 provisional. Adopt the surface
  deltas (deleted leadingOffsetPx, self-registration, nested-anchor scrollTo). Re-prove print &
  hydration on real runtimes (G-h).
- **P2 native CV/CIS** → the reference for the W2 CSS substrate + the F-1/F-3 fixtures; add W2
  requirement G-g (host-wrapper owns inter-section spacing) and the WebKit-in-the-ledger note.

**Prototypes to DISCARD (as primary) / hold as fallback:**
- **P1 adopt-core** → DO NOT pursue as the primary architecture. RECORD it as the tested,
  ranked-first VR-14 Fallback A (§A.1), and TRANSPOSE two validated mechanisms into
  `usePaperViewport` regardless of fork: (1) the injectable adapter seam (VR-40, deterministic
  testability + one-arg window scroller), (2) the default-adjust remeasure discipline
  (first-measure always compensates; skip during backward scroll) as the VR-21 anchor-stability
  gate reference. Its private-API/dual-scroll gaps (G-c, G-j) only matter if Fallback A triggers.

**What the PASS-2 fleet should do (six-wave draft, per the R7 plan-shape bar):**
- **W1 Fork-Settling Prototype Fleet** (≤3 concurrent): F-1 benchmark w/ decision rule, F-2
  real-WebKit, F-3 view-timeline×CV, F-5 activeId A/B, F-8 adversarial re-read; fleet hygiene
  G-m (isolated `newContext()` per lane); closes on benchmark JSON + Playwright traces.
- **W2 Document-Native Substrate:** port the unified P3+P4 core (fixes C-3, VR-19, VR-23),
  CV/CIS CSS + host-wrapper spacing (G-g), measurement registry; one real `PaperSection`
  integration (G-e); closes on build/test exit 0 + the 422-line deletion diff + anchor trace.
- **W3 Document-Semantics Arms:** find/hash/print/a11y/PRM runtime probes on REAL browsers
  (C-1, G-h) + axe + reader-mode artifacts; fix the "172" count (C-2).
- **W4 Theme Token Seam + Surface Close:** `--lp-*` full-color two-tier tokens (VR-37), oklch
  smoke fixture, doc-vs-dist export reconciliation.
- **W5 Consumer Migrations:** fourier net-deletion migration incl. the `.deferred-section`
  double-containment strip (G-f); vft binding verified against 0.3.0-pre incl. the recorded
  `.paper-article`/`scroll-margin-top` adaptations (G-l); substrate + consumer land together.
- **W6 Release + pi Close:** minor changeset → CI v0.3.0; pi visual-runtime artifacts;
  before/after frame-budget ledger (include WebKit).

Full `VIRT.md` lands post-100. The convergence stop condition is a TRUE 100 — honesty over
optimism: this pass is 82.
