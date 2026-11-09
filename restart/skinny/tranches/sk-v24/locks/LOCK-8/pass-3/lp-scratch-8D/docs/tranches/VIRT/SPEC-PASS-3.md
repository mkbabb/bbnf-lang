# VIRT — SOTA Virtualization + Cross-Repo Abstraction — SPEC (PASS 3)

Status: **RE-CUT, PASS 4 — the spec-of-record, reconciled with its own committed
artifacts (the B-3/B-4 coherence edit; `PASS-3-SYNTHESIS.md §D` checklist 1–5, executed).
SUPERSEDES `SPEC-PASS-2.md` and folds `PASS-2-SYNTHESIS.md` into one self-coherent
register.** The architecture and every directional fork are DECIDED; nothing in §0–§4 is
open for re-litigation. What pass 3 EXECUTED (all done on disk, cited herein): the two
remaining SHAPE forks — F-7 settled per §5.1 (`traces/f7/F7-VERDICT.md`, 120 real-engine
lanes) and F-9 settled per §5.2 (`traces/F9-VERDICT.md`, 4/4 consumers feed/grid) — the
W3 runtime-gate evidence set (§6, lanes E3/E4, artifacts at `traces/w3/`), and the
promotion of the proven clone commits into ONE consolidated W2 skeleton (§7,
`W2-SKELETON.md`: `p6-glass-ui @ w2-skeleton 2133597d` + `p6-latex-paper @ w2-skeleton
aff7c73`). **The open fork set is EMPTY.**

Targets, two roots (ROOT-REPO LAW, per-wave owner + vehicle in §7):

- `@mkbabb/glass-ui` 4.2.0 → **4.3.0** (the additive corner is PRIMARY per the VR-71
  mechanical rule × the F-9 verdict; 5.0.0 is CONTINGENT — fires only if W2/W5 removes or
  signature-breaks a currently-published `./virtual` export), repo
  `/Users/mkbabb/Programming/glass-ui` — **the owning root for the
  headless core** (decision D-A, §0).
- `@mkbabb/latex-paper` v0.2.1 → v0.3.0 (changesets CI), repo
  `/Users/mkbabb/Programming/latex-paper` — consumes the core; keeps paper semantics +
  theme + adapters.

Register conventions: requirements are `VR-n` with **stable IDs** — VR-1..46 keep their
pass-1 numbers; VR-47..49 the R10 additions; VR-50..58 the glass-ui core/sidebar family;
VR-60..67 the search family; VR-70..74 the cross-repo publish/gate family; **VR-75..76
the pass-2 additions** (timeline-host law; ElementRef surface rule). Every VR carries a
status — **SETTLED** / **PROVISIONAL** / **OPEN** — and an evidence pointer to an
on-disk artifact (G-prov discipline: direct disk citations, never phantom dossiers).
Open forks are `F-n`; **the open set is EMPTY** — F-7 and F-9 settled this pass with
committed evidence (§5, executed). Full index in §8.

Evidence roots cited throughout:

- `docs/tranches/VIRT/traces/` — `F1-VERDICT.md`, `METHOD-P5.md`, `f1-results.json`,
  `f1-smalln.json`, `f1-verdict.json`; `p8/` (`p8-f3-results.json`, `p8-f3b-results.json`,
  `p8-f5-results.json`, `p8-search-results.json`, screenshots); **`f7/`
  (`F7-VERDICT.md`, `f7a-results.json`, `f7b-results.json`, `gf-results.json`,
  `f7-verdict.json`, screenshots); `F9-VERDICT.md`; `w3/` (`W3-INDEX.md`,
  `W3A-VERDICT.md` + `W3A-REPORT.json`, `W3B-VERDICT.md` + `W3B-REPORT.json`,
  `w3b-axe-report.json` — binaries clone-cited per the index)**.
- `docs/tranches/VIRT/P8-BEHAVIOR-PROBES.md`, `PASS-2-SYNTHESIS.md`,
  **`PASS-3-SYNTHESIS.md`, `W2-SKELETON.md`** (the cherry-pick manifest).
- Scratchpad clones (fleet hygiene VR-74; the P5 clone is `virt-p2 @ 67cb0ca` — the
  earlier `31b39e3` citation was a bookkeeping slip, corrected):
  `p6-glass-ui @ ea7fac8c` (the extraction, glass-ui side), `p6-latex-paper @ 62a18e7`
  (the consumption, twins deleted), `virt-p2 @ 67cb0ca` (P5 F-1 traces),
  `virt-p4 @ e796519` (the P7 unified substrate shell), **`virt-p4 @ 761ba9d` (W3a
  harness + binaries) / `@ 4391629` (W3b harness + binaries), `p6-glass-ui @
  w2-skeleton (2133597d)` / `p6-latex-paper @ w2-skeleton (aff7c73)` (the promoted W2
  skeleton branches)**.

---

## 0. DECISION D-A — glass-ui owns the headless core — SETTLED + EXTRACTION-PROVEN

The first architecture decision of the tranche, settled in pass 2 and **survived a real
two-clone extraction** (the strongest evidence short of landing):

1. **The engine exists in THREE repos** (glass-ui `src/composables/virtual/`; latex-paper
   `src/vue/composables/` + `tracking/`; words `frontend/src/composables/virtual/`) and
   glass-ui's sidebar/search surfaces have live external consumers (words
   `ProgressiveSidebar`, bbnf-lang `DocsSidebar`, fourier `PaperSidebar`; value.js +
   bbnf-lang search). glass-ui is both origin and current home (re-homed at tranche BC
   `c56586c0`). The pass-1 "zero-consumer glass-ui fork" premise is FALSIFIED and struck.
2. **P6 executed the decision for real**: the document-native core landed ONCE at
   glass-ui `./virtual` (9 modules, `p6-glass-ui @ ea7fac8c`); latex-paper's
   `usePaperViewport` became a ~15-line typed adapter (`FlatPaperSection extends
   FlatSection`, vue-tsc-validated across the `file:` package boundary);
   **all 10 twin filenames resolve to exactly ONE path** in the clone pair
   (`p6-latex-paper @ 62a18e7` — twins physically deleted, no shims, no dangling
   imports); both clones build, both suites green.
3. Options (b) new-package and (c) latex-paper-owns are refuted on evidence
   (overfitting; inverted platform relationship; latex-paper has no search source).

**Binding consequences (carried forward, unchanged):** the substrate ships at glass-ui
`./virtual` + `./sidebar` + `./search`; latex-paper consumes and keeps paper adapters
(`FlatPaperSection extends FlatSection`; `toPaperSearchItems`); words + fourier migrate;
losing twins are DELETED, never shimmed (gate VR-72); search is glass-ui-internal
hardening; `leadingOffsetPx` stays deleted (VR-58).

**Binding amendment from the extraction (now VR-76):** nominal `Ref<T>` does NOT cross
the `file:`-linked package boundary — every consumer-facing glass-ui option MUST use the
structural `ElementRef` contract P6 landed in `sidebar/types.ts`. A surface RULE, not a
one-off fix.

---

## 1. THE FORK LEDGER — fully closed; the open set is EMPTY

### Closed (cite, consume, build on — no re-litigation)

- **D-A — the abstraction fork → (a) glass-ui owns.** §0. Extraction-proven.
- **F-1 — the mount default → CLOSED (P5).** Committed throttled three-way traces
  (`traces/F1-VERDICT.md` + the three JSONs); the pre-committed rule fired mechanically:
  **R2 shipped default = `mount: 'governed'`; R4 PASS on all three clauses** (worst
  governed longtask 133ms < 200 budget; scroll avg +0.2% vs the windowed reference, 0
  frames > 33ms; far-target errorPx 0.09–0.31 ≤ 1); **R5 not triggered**. The decisive
  real-consumer datum: fourier's ACTUAL paper (library-parsed, **98 flat sections**)
  eager-mounts at **891ms** worst longtask vs governed **123ms**, FCP 260ms.
  **Constants (picked by pass-2 synthesis, binding for W2):** eager auto-select
  threshold **64** (the decomposed mount-attributable `S_cross = 80 → floor(0.8×80)`;
  the literal 25 is a demo-fixture artifact — the fused bundle-eval+mount task — and
  every real consumer code-splits), **batchSize 4** ("largest batch under one frame
  budget": p95 14.2ms < 16.7, reveal 1.11s vs batch-2's 2.0s), **idleRevealAll: true**.
  Both constants land in code with the trace citation. VR-10/13 SETTLED.
- **F-2 — WebKit floors → CLOSED** (R11 + P2). Floors pinned in the VR-22 table;
  residual = the W3 runtime gate (an evidence item, not a fork).
- **F-3 — view-timeline × CV skipped-state → CLOSED (P8), new law VR-75.** Real Chromium
  + real WebKit 26.5: reveals fire ON SCHEDULE from the skipped state in both engines
  **when the ViewTimeline is declared ON the CV host**; anonymous `view()` on a skipped
  descendant is BROKEN in both (Chromium: timeline inactive, `fill` void; WebKit:
  late-firing, deviation to 0.945). `traces/p8/p8-f3{,b}-results.json`. VR-9 SETTLED.
- **F-4 — estimator → CLOSED** (A.6: landing accuracy is seed-independent; simplest
  width-aware estimator kept).
- **F-5 — activeId mechanism → CLOSED (P8): offsetTop-primary wins.** 0% transient
  TOC-highlight mismatch vs IntersectionObserver-primary's 22.7% under fast scroll +
  remeasure storms at 4×, at indistinguishable jank (8.84 vs 9.01ms avg). IO demotes to
  optional in-DOM enhancement, never authoritative. `traces/p8/p8-f5-results.json`.
  VR-49 fully SETTLED. Residual (low-risk, W3 spot-check): the A/B was Chromium +
  element-scroller; a vft window-scroller arm + WebKit spot-check ride the W3 lane.
- **F-6 — window-scroller → CLOSED** (A.4; the vft binding doc is a W5 work item G-l).
- **F-8 — adversarial re-read → EXECUTED as R10** (yielded VR-47/48/49 + the VR-23
  never-scrolls fix).
- **F-7 — containment × real-paper CSS → CLOSED (pass-3 lane E1, by the §5.1
  escalation firing mechanically).** Verdict `traces/f7/F7-VERDICT.md` (120 real-engine
  lanes: 48 F-7a + 60 F-7b + 12 G-f; prod builds, Chromium 149.0.7827.55 + WebKit 26.5,
  3 viewports, CV-off reference in every diff). **Arm A-ii ships: root-only CV
  (depth===0) on the inner `.section-body`, seeded by `subtreeEstimatedHeight`.**
  Sticky POSITIONING survives CV in both engines (10,560 samples, worst offset Δ
  0.00px); arm i rejected on Chromium header-repaint ghosting (0.66–2.32%, 6/6 Chromium
  lanes); arm B (per-level) hard-rejected on structural scrollbar dishonesty
  (0.063–0.131) + real WebKit layout divergence up to 414px on the real fourier paper.
  VR-45/47/48 SETTLED (§3); the two W4 findings lifted into VR-15/16 + VR-30.
- **F-9 — the words virtual-copy disposition → CLOSED (pass-3 lane E2): branch (b)
  survive-at-root.** Verdict `traces/F9-VERDICT.md` (committed `f246a11`): 4/4 importers
  feed/grid-shaped, zero document-shaped — the §5.2 rule fired unambiguously. VR-57
  SETTLED (§4); VR-71's value pins **4.3.0 additive corner PRIMARY** (the disk-truth
  correction: 4.2.0 `./virtual` already ships the windower unchanged and words consumes
  it unchanged — see §5.2 + VR-71).

### Open — NONE

The open fork set after pass 3 is **EMPTY**. The §5.1 escalation opened and closed
within F-7 (the pre-committed mechanical shape); the VR-71 semver VALUE was re-opened by
the F-9 disk-truth finding and re-closed to 4.3.0 in the same verdict — a value
correction under the existing mechanical rule, not a new fork.

---

## 2. BLOCKERS B-1 / B-2 — CLOSED

- **B-1 — spec staleness → CLOSED BY THIS SPEC.** `SPEC-PASS-2.md` disagreed with its
  own committed artifacts (F-1/F-3/F-5 listed OPEN while `traces/` + P8 closed them;
  no VR-75). This document IS the reconciliation: every closure is folded into §1 and
  the register; `SPEC-PASS-2.md` carries a superseded banner.
- **B-2 — test-count truth + the RED test → CLOSED at `952c897`.** The pre-existing
  defect (repo-health, unrelated to VIRT): `src/transform/validate.ts` classed the LaTeX
  control-space `\ ` (e.g. "vs.\ outside" in `tests/fixtures/`… `fourier_paper.tex`) as
  an unprocessed unbraced accent — the accent character class contained a literal space.
  Fixed in its own commit `952c8972f69814449036e6c933e5719b370447cf` ("fix(validate):
  stop flagging LaTeX control-space \\  as an unbraced accent"), verified twice
  independently + re-run at spec-cut time.
  **The ONE citable suite count (machine-verified, latex-paper @ `952c897`):**

  ```
  Test Files  14 passed (14)
       Tests  127 passed (127)
  ```

  **Every prior pinned count is STRUCK as historical**: "172", "145 pass + 9 skip = 154
  (viewport 27)", "126 + 1 failed", "118 + 9". None reproduces at HEAD; the VR-73 RULE
  (machine-derived counts only, reporter output committed beside the claim) stands and
  this line is its current pin. **Skeleton pins (machine-verified at the W2-skeleton
  cut, reporter lines verbatim in `W2-SKELETON.md`):** glass-ui `w2-skeleton
  (2133597d)` — `Test Files 123 passed (123)` / `Tests 1176 passed (1176)`, build +
  `vue-tsc --noEmit` + `proof:virtual-window` all exit 0; latex-paper `w2-skeleton
  (aff7c73)` — `Test Files 15 passed (15)` / `Tests 136 passed (136)` with the sibling
  fourier-analysis symlinked so the golden block RUNS (`127 passed | 9 skipped (136)`
  without it). The 14/127 → 15/136 delta is fully reconciled in `W2-SKELETON.md §2`
  (twin test deleted, adapter + fontFaceProbes + flatten tests added).

---

## 3. THE VR REGISTER, PART I — VR-1..49

### SOTA definition (VR-1..11)

- **VR-1** [SETTLED] Dynamic per-item height stable under font/image/width change.
  P3 registry + P2 CV `auto` memory.
- **VR-2** [SETTLED — two-path] No jump on above-viewport size change. Chromium: native
  scroll anchoring (P1/P2/P3: 0.0px). **Stable Safari through 26.x has NO
  `overflow-anchor`** (R11; new in Safari 27 beta) → **`compensateScrollTop` is the
  DEFAULT stable-Safari path, not optional**; branch on
  `CSS.supports('overflow-anchor','auto')`. **The fallback is now REAL-WebKit
  runtime-proven** (§6.1 executed, `traces/w3/W3A-VERDICT.md`): green on WebKit 26.5
  under the shimmed stable-Safari arm (jumps ≤0.25px across rendered-band AND
  CV-skipped growth; absorbed/applied 1000/1000 — exactly-one-corrector) and the native
  branch verified on both engines.
- **VR-3** [SETTLED] Programmatic nav to any id: P3 LandingReport 0.06–0.19px; WebKit
  requires the VR-23 settle loop.
- **VR-4** [SETTLED] Find-in-page whole-document: P2 green both engines; floor Safari
  26.0 (VR-22). Only the native path satisfies it.
- **VR-5** [SETTLED] Native `#fragment`/`:~:text=`: Chromium exact; WebKit via VR-23
  (mandatory). Text-fragment on dynamic pages fixed Safari 26.2.
- **VR-6** [SETTLED] Full heading/landmark tree, per VR-26/27.
- **VR-7** [SETTLED] Complete `window.print()`, per VR-24; the §6.2 re-proof EXECUTED
  (`traces/w3/W3A-VERDICT.md`: governed PDF ≡ eager, 18 = 18 pages; manual Cmd+P is
  the last inch).
- **VR-8** [SETTLED — evidence wording corrected by P7] Reader mode whole article. The
  sentinel is absent from `innerText` under CV-skip but PRESENT in `textContent`, BOTH
  engines — cite **`textContent`**, never `innerText`.
- **VR-9** [SETTLED — was OPEN→F-3] view-timeline reveals fire from the CV-skipped state
  when the timeline obeys VR-75. `traces/p8/p8-f3{,b}-results.json`, both engines.
- **VR-10** [SETTLED — was PROVISIONAL] Off-screen render deferral: layout/paint via CV
  (P2); the Vue-mount half via the governor. `traces/f1-verdict.json` R4 PASS.
- **VR-11** [SETTLED] All ten properties simultaneously at O(10–200) sections, smallest
  architecture that gets there.

### Architecture (VR-12..14)

- **VR-12** [SETTLED] Document-native: all sections mounted; `content-visibility: auto`
  + `contain-intrinsic-size` defer layout/paint; the slim viewport core supplies
  activeId, deep links, settle hygiene, print/PRM arms; the 422-line JS windowing layer
  retires from the document path. NO shim (v0.3.0 does not satisfy `^0.2.1`). The two
  falsified pass-1 §2.2 arguments remain struck.
- **VR-13** [SETTLED — was PROVISIONAL; F-1 closed] **The mount governor is the shipped
  DEFAULT**: `mount: 'governed'`, **eager auto-select at `items.length ≤ 64`** (the
  decomposed `S_cross`, `traces/F1-VERDICT.md` R3 + `f1-smalln.json`), **`batchSize: 4`**
  (largest batch under one frame budget: p95 14.2ms, `F1-VERDICT.md` batch table),
  **`idleRevealAll: true`**. Monotone-forward reveal only; nothing unmounts;
  `revealAll()` escape for print/find/hash/idle; client-render-only (VR-25);
  frontier-monotonicity unit test (VR-46). Constants ship in the doc comment with the
  trace citation. **EXPRESSED IN CODE in the promoted skeleton (pick `fd516982`):
  `EAGER_AUTO_SELECT_THRESHOLD = 64`, `DEFAULT_BATCH_SIZE = 4`, `idleRevealAll: true`,
  governed default — the 64/65 boundary unit-tested.**
- **VR-14** [SETTLED] Contingency = the ranked pair (A = `@tanstack/virtual-core` under
  the existing API; B = `hidden=until-found` + `beforematch`, floor Safari 26.2).
  Trigger = F-1 rule R5, which **did not fire**; the pair stays recorded, dormant.

### Measurement model (VR-15..21)

- **VR-15** [SETTLED — F-7b shape final; **+ a named W4 recalibration gate**]
  `estimatedHeight` seeds `contain-intrinsic-size: auto Npx` via `--lp-section-size`;
  acceptance metric = scrollbar honesty; seeding level = ROOT-only
  `subtreeEstimatedHeight` per the settled VR-48. **W4 work item (F7-VERDICT):** the
  narrow-width/dense-math estimator measured pre-settle honesty **0.759 at 390×844 on
  the real fourier paper** (the reference's own settle-growth floor is 0.918 at
  ≥1280px); **acceptance gate = pre-settle/settled ≥ 0.85 at 390×844 on the real paper
  after recalibration.** A calibration deficit, not a level-choice defect — no level
  choice changes these numbers (F7-VERDICT clause-2 re-probe).
- **VR-16** [SETTLED] Simplest width-aware estimator kept; the pass-1 math-model change
  dropped (G-b closed). **The narrow-width + dense-math recalibration (W4) rides
  VR-15's 0.85@390px gate** — same estimator, same acceptance artifact.
- **VR-17/18** [SETTLED — implemented in the skeleton] Shared per-instance RO registry,
  `data-lp-id` identity, width-EPOCH invalidation (`read()` fresh-only / `readAny()`
  estimate-grade), delta accounting from the SECOND write; the module
  `SESSION_HEIGHT_CACHE` singleton deletes with the twins. Lives in the glass-ui core
  (`p6-glass-ui @ ea7fac8c`); the house `useResizeObserver` leaf discipline per VR-56.
- **VR-19** [SETTLED — implemented + unit-tested] Font gate: `document.fonts.ready`
  alone is INSUFFICIENT (P3: gate passed at 49ms; swap at 404ms) —
  **explicit `document.fonts.load('1em KaTeX_Main', …)` over the transform-derived face
  list is REQUIRED** (`deriveFontFaceProbes` derives faces from transform output).
  `whenSettled()` = the settle-EPOCH gate (closes pass-1 C-3): monotone integer epoch,
  every dirty event bumps it, shared promise-per-epoch, no mutable dirty flag; the C-3
  clobber scenario is a green unit test against the byte-verbatim legacy race (P6/P7).
  **Expressed in the promoted skeleton:** `deriveFontFaceProbes` is wired as the
  `usePaperViewport` DEFAULT and the old unconditional `KATEX_FONT_PROBES` is REMOVED
  (`W2-SKELETON.md` latex-paper pick `12ed7e4`); the settle gate gains the leading
  GRACE frame (glass-ui pick `b74875c3`).
- **VR-20** [SETTLED] Figure images emit `width`/`height`/`aspect-ratio` when known,
  `loading="lazy" decoding="async"` otherwise; unknown-dimension reflows ride the RO
  epoch; shift absorption per VR-2's two-path rule.
- **VR-21** [SETTLED — REAL-WebKit runtime-proven, §6.1 executed] Anchor stability
  under above-viewport growth: Chromium 0.00px measured; stable-Safari path =
  `compensateScrollTop` (load-bearing per VR-2) — green on WebKit 26.5, native +
  shimmed-stable-Safari branches, exactly-one-corrector proof (absorbed == applied ==
  1000). Artifact: `traces/w3/W3A-VERDICT.md` + `W3A-REPORT.json`.

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

- **VR-22** [SETTLED] Find-in-page floor = Safari 26.0 (C-1 closed: R11 + P2's real
  WebKit 26.5 green probe); Safari 18.x–25.x = informative docs note. Under the
  governor, `whenSettled()` → idle `revealAll()` bounds the non-findable window to the
  first seconds of page life. The WebKit re-confirm (§6.5) rides the W3 wave's re-run
  against the landed W2 substrate — low-risk, P2 already green on real WebKit 26.5.
- **VR-23** [SETTLED — implemented + P8-hardened] Settle-aware `scrollTo(id)` +
  `useHashNavigation` (`hashchange`/`popstate`, net-new). Arrival detection
  **`scrollend`-primary with ≥3s fallback** (the ~1.5s/90-frame cap is gone); long
  jumps (>~10 viewport-heights) instant; user-interrupt abort with the bottom-clamp
  exemption; unconditional final commit after `whenSettled()` (the R10 never-scrolls
  fix); computed `scroll-margin-top` honored; `leadingOffsetPx` removed. **P8
  correction:** WebKit's +257px arrives at frame 1 AFTER a correct frame-0 read —
  arrival detection MUST be whenSettled-epoch-based, never a single frame-0 rect read.
  Gate `LandingReport{errorPx ≤ 1, settleFrames ≤ 10}`. **Expressed in the promoted
  skeleton (pick `b74875c3`): the wall-clock ≥3s bound is `MIN_TRAVEL_MS = 3000` —
  replacing the clone's 240-FRAME cap, a live VR-23 regression the promotion lane
  caught and fixed (frames compress under throttle/high-refresh); `scroll-margin-top`
  is geometry-derived by the adapters (closes G-c5 at the root).**
- **VR-24** [SETTLED — §6.2 executed] `revealTo`/`revealAll` return `Promise<void>`;
  consumers `await revealAll()` before `print()`; `beforeprint` → `revealAll()`; CV
  half pure-CSS via `@media print` (P4 68-page PDF artifact). **The re-proof ran
  (`traces/w3/W3A-VERDICT.md`):** `beforeprint` ALONE reveals 8 → 60 on both engines;
  governed PDF ≡ eager reference (18 = 18 pages, byte-identical extracted text, 14,929
  chars); the OS dialog itself is unautomatable headlessly — a manual Cmd+P spot-check
  on the landed W2 substrate is the acknowledged last inch (W3-wave residual).
  **Hardening finding (W4, VR-24 amendment):** `theme.css` ships no fragmentation
  rules — add `@media print { .math-block, .paper-article figure { break-inside:
  avoid } }` (expressed in the skeleton's F-7 CSS pick `aff7c73`).
- **VR-25** [SETTLED — §6.3 executed, the guard runtime-proven LOAD-BEARING] SSR
  ARCHIVED with documented guards: full `renderToString` verified; eager hydration =
  zero mismatches; runtime arms post-hydration-only; **governed mount is
  client-render-only** (silent truncation otherwise). The guard heuristic
  ("first-item-id already present at setup") can false-positive into eager on a
  pre-existing duplicate id — safe direction, never truncates; named in the v0.3.0
  MIGRATION notes (G-c6). **The re-proof ran (`traces/w3/W3B-VERDICT.md`):**
  zero-mismatch hydrate on Chromium + WebKit with a 25ms no-truncation sampler
  (minimum 60/60), and a **blinded-guard NEGATIVE control** (server mutilates the
  guard's probe id) truncates 60 → 8 with the sentinel lost — the guard is
  load-bearing, the pass-1 silent-reconcile hazard is real. Guard ported to the
  skeleton (`useSectionViewport.ts:182`, pick `fd516982`).
- **VR-26** [SETTLED] Heading level = min-normalized `headingLevel`, additive
  `FlatPaperSection` field (unit-tested); multi-file plumbing through
  `PaperSectionContent` budgeted in W4.
- **VR-27** [SETTLED] `role="doc-section"` does not exist in DPUB-ARIA 1.1 →
  `aria-labelledby` named regions (h2 `id` + section `aria-labelledby`), SSR-asserted.
- **VR-28** [SETTLED] PRM gates every programmatic smooth scroll AND `useSidebarFollow`'s
  rAF damping loop (the PRM snap is implemented at the glass-ui root, `p6-glass-ui`);
  third smooth site (`useScrollTracker` `scrollIntoView`) inventoried; theme adds the
  `@media (prefers-reduced-motion: reduce)` arm.
- **VR-29** [SETTLED — §6.4 executed] Focus: native under full mount; monotone reveal
  never destroys focus. **Artifacts (`traces/w3/W3B-VERDICT.md` + `w3b-axe-report.json`):**
  axe-core 4.12.1 zero critical / zero serious on Chromium + WebKit; keyboard walk
  green — Enter on a link to an UNMOUNTED section (8/60 mounted) reveals + lands
  (errorPx 0.53) + focuses the section root, and focus survives `revealTo`/`revealAll`
  (never lands on body).
- **VR-30** [SETTLED (governor) — §6.4 executed; **+ a named W4 markup finding**]
  Reader mode ≥95% extraction gate met with margin on REAL Firefox 151: Readability
  parses governed ≡ eager at **extraction ratio 1.0000** (14071/14071 chars), asserted
  on `textContent` per VR-8 (`traces/w3/W3B-VERDICT.md`). **W4 finding (markup, NOT a
  governor defect): the shipped `.section-header` class matches Readability's
  `unlikelyCandidates` regex → Reader View strips ALL headings (eager and governed
  identically); the rename arm restores 60-missing → 0.** W4 renames the class
  ("heading" is regex-safe, "header" is not) — naturally coincident with the F-7a
  re-shaping of the same box; touches the theme sticky selectors + `PaperSection.vue`.

### API surface + Vue architecture (VR-31..46)

- **VR-31** [SETTLED] Semver honesty: `^0.2.1` excludes 0.3.0; v0.3.0 explicit opt-in;
  `useVirtualSectionWindow`/`VirtualSectionWindowOptions`/`virtualSectionLayout` REMOVED
  from latex-paper, not deprecated; minor changeset with a MIGRATION section naming
  every removed symbol + replacement, including the G-c5 note: headless
  `scrollToSection` ignores CSS `scroll-margin-top` unless the caller forwards
  `scrollMarginTopPx` (the pure adapter has no DOM to read computed style).
- **VR-32** [SETTLED — proven in `p6-latex-paper @ 62a18e7`] Byte-compat floor holds for
  everything a consumer imports today except the retired seam; the tracking-suite
  exports remain on `/vue` as **re-exports of the glass-ui sidebar suite** (consumption,
  not a shim); signature deltas named in MIGRATION. Holds the fourier import floor.
- **VR-33** [SETTLED] Breaks, exhaustively: (a) windowing seam → `usePaperViewport`;
  (b) spacer render-host contract → render ALL sections with `:ref="registerSection"` +
  `data-lp-id` + the CV class; (c) `virtualSectionLayout` removed (no type leaks through
  `dist/`). fourier migration = net deletion; substrate + consumer land together (W5).
- **VR-34** [SETTLED — implemented] `usePaperViewport` = the ~15-line
  `FlatPaperSection`-typed adapter over the glass-ui core (vue-tsc-validated across the
  `file:` boundary, the D-A falsifier). Single `shallowRef` root + `triggerRef` (VR-39).
- **VR-35** [SETTLED] `scrollToId` on `PaperContext` library-fulfilled; fourier's
  search/progress/nav chrome stays consumer-side, composed over the VR-60 surface.
- **VR-36** [SETTLED] `usePaperReader` rebases on the viewport core; the two-authority
  activeId RACE (R10 G11) ends: exactly ONE authority post-tranche (VR-49);
  `useTreeIndex.getChildren` documented as the `subsections→children` adapter.
- **VR-37** [SETTLED] Two-tier theme tokens `var(--lp-x, var(--x))` full-color BOTH
  tiers (the hsl-triple tier is dead code — live-probed); unblocks the vft earth
  palette; also a 0.2.1 bug fix. Token table in `docs/theming.md`.
- **VR-38** [SETTLED] latex-paper release vehicle: changesets CI, no dev-machine
  publish; gates `npm run build` + `npm test` per wave (counts per VR-73).
- **VR-39** [SETTLED] Single reactive root (`shallowRef` + one `onChange` → `triggerRef`).
- **VR-40** [SETTLED] Injectable `ViewportAdapter` seam `{observeRect, observeOffset,
  scrollTo}` — element + window implementations; window mode is the vft path (A.4);
  container-vs-window duality preserves the `containerTop` math (R10 regression floor).
- **VR-41** [SETTLED] Passive scroll listener, rAF-coalesced; `isScrolling` from
  `scrollend` + 150ms debounce fallback; the 320ms `warmTimer` dies; the listener is
  `createScrollReader` — the no-fourth-listener fence (VR-56).
- **VR-42** [SETTLED] Vue 3.5 hygiene: `onWatcherCleanup`, `useTemplateRef`, reactive
  props destructure.
- **VR-43** [SETTLED] `v-memo="[item.id]"` on section roots is the net-new render-cost
  work; `v-once` on KaTeX interiors is ALREADY SHIPPED (not claimed); `KeepAlive`
  rejected.
- **VR-44** [SETTLED] O(1) id→element/index maps; `getOffsetFor` reads live `offsetTop`;
  glass-ui's binary-search `findSectionOffset` closes G10 at the owning root.
- **VR-45** [SETTLED — F-7 executed, `traces/f7/F7-VERDICT.md`] CSS substrate
  ownership: the CV/CIS rule ships in latex-paper `theme.css`; `PaperSection.vue` emits
  `data-lp-section` + inline `--lp-section-size` (ROOT sections only). **The final
  shape, verbatim in the verdict:** `display: flow-root` on `[data-lp-section]` AND
  `.section-body` in ALL states (the G-g normalization — makes CV-on ≡ CV-off geometry
  EXACTLY: height Δ 0.00%, boundary gaps Δ 0.00px, per-section tops Δ 0.00px; without
  it containment drifts document geometry +0.94..+2.44%); `content-visibility: auto` +
  `contain-intrinsic-size: auto var(--lp-section-size, 640px)` on `.paper-article >
  [data-lp-section] > .section-body` ONLY; the `@media print` escape on
  `.section-body`. No `PaperSection` DOM change — arm ii is pure CSS. Expressed in the
  skeleton (latex-paper pick `aff7c73`).
- **VR-46** [SETTLED] Governor frontier-monotonicity unit test + the vitest/Playwright
  gate set; counts per VR-73.

### The R10 additions (VR-47..49)

- **VR-47** [SETTLED — F-7a executed, arm ii ships] **The sticky-header × containment
  trap, resolved: the CV carrier is the inner `.section-body`; the header stays OUTSIDE
  the contained box.** The headline correction to the fear: sticky POSITIONING survives
  CV in BOTH engines (10,560 samples, worst offset Δ 0.00px, every arm including the
  naive one). What breaks is Chromium PAINT — **new sub-law: a sticky header must not
  pin INSIDE a paint-contained box** (arm i rejected on header text-AA repaint ghosting,
  0.66–2.32% of header pixels, 6/6 Chromium lanes; WebKit 0 everywhere). Arm iii
  (headers hoisted) passes but is unneeded (DOM restructure, measured costs). Residual:
  sub-headers inside the root's contained body diff ≤0.38% — recorded, subthreshold,
  Chromium-only. Evidence: `traces/f7/F7-VERDICT.md` + `f7a-results.json`.
- **VR-48** [SETTLED — F-7b executed, arm A ships] **The nesting model: CV + CIS on
  ROOT sections only (depth === 0), seeded by the subtree-inclusive
  `subtreeEstimatedHeight` field** (lands in `flattenPaperSections`, skeleton pick
  `12ed7e4`, with the per-root-sums ≡ per-node-totals honesty identity tested). Rest
  geometry is EXACT (0.00px tops/heights/gaps, 12/12 lanes; the residual pixel is
  Chromium-only text-AA under containment, 0 on WebKit). Arm B (per-level) is
  hard-REJECTED: structural scrollbar dishonesty (0.063–0.131 — a skipped parent's
  own-content seed cannot count its unlaid-out children) + measured WebKit layout
  divergence up to **414.41px** on the real fourier paper (`f7b-results.json`
  results[39]). Landing held: worst errorPx 0.83 across all F-7b lanes.
- **VR-49** [SETTLED — semantic + mechanism + implementation] **ONE activeId authority.**
  Semantic: **deepest-visible-wins** (R10 canon). Mechanism: **offsetTop-table +
  binary-search primary** (F-5: 0% vs 22.7% transient mismatch,
  `traces/p8/p8-f5-results.json`); IntersectionObserver = optional in-DOM enhancement,
  never authoritative. Implemented as ONE `resolveActivePosition` + unit-tested
  (`p6-glass-ui`). **The 6-item regression floor binds verbatim:** (1) deepest-visible
  tie-break; (2) closest-below fallback when nothing is above the active zone; (3)
  `scroll-margin-top: 5rem` anchor landing; (4) `activeId` init non-null to
  `roots[0].id` inside the document, null outside; (5) container-vs-window duality
  math; (6) the `useSidebarFollow` manual-override state machine (deadzone
  0.18·navHeight, damping 0.22, `programmaticScrollDepth` guard, input suspension,
  resume on next content scroll) preserved as a documented black box (VR-54).

---

## 4. THE VR REGISTER, PART II — VR-50..76

### The glass-ui core + sidebar family (VR-50..58)

- **VR-50** [SETTLED + extraction-proven] The document-native viewport core ships at
  `@mkbabb/glass-ui/virtual`, generic-typed (`FlatSection`/`SectionHierarchy`):
  registerSection + measurement registry (VR-17/18), settle-EPOCH gate (VR-19),
  settle-aware scrollTo + hash nav (VR-23), mount governor with the F-1 constants
  (VR-13), activeId authority (VR-49), ViewportAdapter seam (VR-40). Proven at
  `p6-glass-ui @ ea7fac8c` (9 modules, suite green). Consumers of the DOCUMENT core:
  latex-paper (hence fourier + vft). words does NOT consume the document core — per
  the settled F-9 branch (b) it consumes the RETAINED windower family at the same
  `./virtual` subpath (VR-57), the core landing additively beside it (VR-71).
- **VR-51** [SETTLED — expressed in the skeleton; UNLANDED at the real 4.2.0 root
  until W2] `peerDependenciesMeta` marks `@lucide/vue`, `class-variance-authority`,
  `clsx`, `reka-ui`, `tailwindcss` **optional** (bundle content probes:
  `./virtual`/`./sidebar` are Vue-only). Expressed in the promoted skeleton (pick
  `ea7fac8c`, verified in the clone `package.json`); lands at the real root with W2,
  before latex-paper consumes. Making a mandatory peer optional is
  constraint-loosening = additive — it does NOT force major (VR-71).
- **VR-52** [SETTLED + proven] `FlatPaperSection extends FlatSection`
  (vue-tsc-validated across the `file:` boundary); glass-ui becomes a runtime
  **`dependency`** of latex-paper (deps 3 → 4; 5,722 B gzip both subpaths — accepted);
  `vue` stays a peer; `/vue` re-exports the sidebar suite (VR-32).
- **VR-53** [SETTLED — source-unify IMPLEMENTED in the promoted skeleton] The tracker
  consumes the core's position table as its ONE authority. P6 unified the semantic
  (`resolveActivePosition`); the skeleton (pick `2b27d7c7`) closes G-c2: the core
  exposes `getPositions()`, `useScrollTracker` gains `positionSource` with an
  early-return — **`getElementById` is uncalled on the scroll path (spy-asserted)**;
  the DOM walk demotes to the standalone-sidebar fallback; `roots[0].id` init
  preserved (the VR-49.4 regression floor). Consumer wiring of `positionSource` where
  sidebar + core coexist is W4/W5 (`W2-SKELETON.md` residual #5).
- **VR-54** [SETTLED + implemented] `useSidebarFollow` single home = glass-ui
  `composables/sidebar/`, with the VR-28 PRM snap + `scrollSource: 'window'`;
  latex-paper's 234-line copy deleted (`p6-latex-paper`); fourier's import continues
  via the `/vue` re-export; the manual-override machine regression-tested (VR-49.6).
- **VR-55** [SETTLED] glass-ui TOC surface to spec: persistent never-virtualized
  `<nav>` with `aria-current`; PRM gates on all scroll sites; `useScrollTo` inherits
  the VR-23 final-commit + scroll-margin discipline. Consumers: words, bbnf-lang,
  fourier.
- **VR-56** [SETTLED — fence REWORDED] Shared-leaf canon: `useResizeObserver`,
  `createScrollReader`, `useTextHighlight` are the only observer/listener/highlight
  primitives. **The fence is "ONE observer per CONCERN," not "one wrapper"**: the
  measurement pipeline keeps its single ResizeObserver; forcing the per-element house
  leaf onto N sections would mint N observers (G-c3). Hand-rolled twins delete.
- **VR-57** [SETTLED — **survive-at-root**, F-9 executed, `traces/F9-VERDICT.md`] The
  words copy disposition: the §5.2 read ran — **4/4 importers feed/grid-shaped, ZERO
  document-shaped** (DefinitionContentView.vue:162 `useVirtualSectionWindow`, the
  pivotal twin — result-card clusters, no CV/print/find/fragment/reader anywhere in its
  subtree, grep-verified; flattenDefinitions.ts:1 type-only; wordlist.ts:90
  `useWindowedStore` infinite-scroll feed; ThesaurusView.vue:219 `useVirtualGrid`,
  stays words-local per the ≥2-consumer bar). Branch (b): the windower survives at the
  glass-ui root ONLY; words deletes its stale copy (726 LOC), keeps `useVirtualGrid`
  (116 LOC), re-points 3 imports + bumps glass-ui — a W5 net-deletion, not a rewrite.
  The twin gate (VR-72) closes for the windower family when words deletes.
- **VR-58** [SETTLED] `leadingOffsetPx` deleted, named in MIGRATION; glass-ui's
  `contentEl` + `getContentScrollTop()` covers non-virtualized leading content.

### The ONE search surface (VR-60..67) — owner: glass-ui `./search`

- **VR-60** [SETTLED] glass-ui `./search` (FuzzySearch.vue, SearchBar.vue,
  `useFuzzySearch`, index/match/cache utils, `searchVariants`, `useTextHighlight`) is
  the ONE surface; latex-paper ships no search source (grep-verified); fourier DELETES
  its entire `web/src/components/paper/search/` directory — no shims; the deletion
  retires fourier's module-singleton cache bug + v-html `<mark>` injection.
- **VR-61** [SETTLED — implemented + unit-tested] `toPaperSearchItems(sections):
  SearchableItem[]` at latex-paper (number-prefixed title → `label`; plainText/rawTex →
  `text`; paper type union → `type`; `id` → `id`); VR-64 nav contract tested
  (`p6-latex-paper`). Domain coupling stays out of glass-ui.
- **VR-62** [SETTLED — extended by P8] CSS Custom Highlight API is the match-marking
  path (`::highlight(glass-search-mark)`, token-bridged; floor Chromium 105/Safari
  17.2/Firefox 140; feature-detected no-op fallback). **P8 extension:** Ranges over
  CV-skipped content are constructible + registrable in both engines, but rect queries
  on skipped Ranges are engine-divergent (WebKit 0×0) — **never gate logic on skipped-
  Range rects**; painted-after-reveal verified (`traces/p8/` screenshots).
- **VR-63** [SETTLED] Keybinding law: Cmd+K (consumer-wired) opens the custom surface;
  the library NEVER intercepts Cmd+F — find-in-page is native (VR-22/4).
- **VR-64** [SETTLED — P8-corrected ×2] Result-nav contract: (1) ALL result-nav scrolls
  go through the VR-23 settle-aware contract — plain `scrollIntoView` is Chromium-only
  (WebKit lands +257px short permanently otherwise); (2) under governed mount the
  highlight Range is built AFTER `revealTo(id)` resolves (target null pre-reveal);
  under pure CV it may precede; idempotent re-set after `whenSettled()`. PRM applies
  to the scroll, not the modal.
- **VR-65** [SETTLED] Index from the transform layer's `PaperSectionData` (never the
  DOM), after transform, before first keystroke; reactive-index path for updates.
- **VR-66** [SETTLED] WeakMap-per-index result cache; `onScopeDispose` →
  `clearSearchCache(index)`; debounce timers cancelled via `onCleanup` — shipped in
  glass-ui, gate-asserted, not rebuilt.
- **VR-67** [SETTLED — first-consumer composition deferred to W5 by design] The paper
  composition (FuzzySearch + adapter + viewport nav) lands with fourier at the same
  glass-ui cut that declares the surface stable; the adapter seam is stub-free to its
  boundary (P6).

### Cross-repo publish + gates (VR-70..76)

- **VR-70** [SETTLED] **Migration order** (owner + vehicle per §7): (1) glass-ui —
  peer-meta + the core + sidebar/search hardening → semver cut; (2) latex-paper —
  twins deleted, dep + adapters, theme substrate → changesets v0.3.0 (`/vue` re-exports
  hold the VR-32 floor); (3) fourier — windowing + search-dir net-deletion; (4) words —
  F-9 branch; (5) vft — binding doc.
- **VR-71** [SETTLED — mechanical rule + **value pinned: 4.3.0 additive corner
  PRIMARY; 5.0.0 CONTINGENT**] **glass-ui semver.** The rule, stated once (semver
  honesty, the VR-31 discipline): **if the shipped cut removes OR signature-breaks any
  currently-published `./virtual` export relative to 4.2.0, the cut is MAJOR (5.0.0);
  only a purely-additive surface is minor (4.3.0).** The F-9 disk truth
  (`traces/F9-VERDICT.md §2/§4`) CORRECTS the pass-2 "both branches lean 5.0.0"
  premise: 4.2.0 `./virtual` does NOT ship a document core — it ALREADY SHIPS the
  windower family (`useVirtualSectionWindow` + `clearSessionHeightCache` +
  `useWindowedStore` + `virtualSectionLayout`; `src/composables/virtual/index.ts:9-19`,
  `package.json:375-378`, re-minted at tranche BC for the words consumers). Branch (b)
  keeps the windower unchanged and words consumes it unchanged, so **branch (b)'s
  NATURAL landing is the 4.3.0 ADDITIVE corner: add the document core beside the
  retained windower, zero removal/signature-break** (peer-meta loosening is additive;
  the VR-56/`FlatSection` reshaping already shipped in 4.2.0 — not part of the W2
  delta). `W2-SKELETON.md §4.2` confirms the additive corner HOLDS in the actual
  skeleton (windower untouched, core net-new, the constants additive exports).
  **5.0.0 fires ONLY if W2/W5 elects to remove the standalone windower or re-signature
  a published `./virtual` export.** latex-paper is v0.3.0 minor either way (VR-31).
- **VR-72** [SETTLED — pattern AMENDED per G-vgate; **gate PASS for the {glass-ui,
  latex-paper} pair** at the skeleton cut] **The twin-deletion gate.** Each twin
  filename — `useVirtualSectionWindow.ts`, `virtualSectionLayout.ts`,
  `useWindowedStore.ts`, `useSidebarFollow.ts`, `useScrollTracker.ts`, `useScrollTo.ts`,
  `useLazyLoader.ts`, `useClickDelegate.ts`, `useTreeIndex.ts`, sidebar `types.ts` —
  and the fourier search files (VR-60 list) exist in **AT MOST ONE repo** across
  {glass-ui, latex-paper, fourier-analysis, words}: exactly one for surviving files.
  **Run state (`W2-SKELETON.md §3`): all 7 sidebar twins exactly-one (glass-ui
  `composables/sidebar/`); the two pendings are exactly the named W5 migrations — the
  windower family (glass-ui + words, deletes with the F-9 branch (b) execution) and
  fourier `search/` (7 files, deletes when fourier composes `./search`).** The grep
  MUST be path-qualified (`src/composables/{virtual,sidebar}/`,
  `src/vue/{composables,tracking}/`) — a bare `types.ts` filename false-positives.
  **The EXCLUSION SET is part of the gate (G-vgate2), not an implementation nicety:**
  `node_modules`/`dist` PLUS the **vendored subrepos** (words vendors full glass-ui +
  latex-paper copies under `frontend/`) PLUS the **`.claude/worktrees`** checkouts
  (~40) — a naive un-excluded re-run surfaces hundreds of apparent twins. The W5/W6 CI
  grep (`W2-SKELETON.md` residual #9) MUST encode these exclusions or it
  false-positives. The atlas `j0-absorb-leaf-census` gate (asserts
  `@mkbabb/glass-ui/virtual` has zero consumers today) is updated in the same wave the
  first consumer lands.
- **VR-73** [SETTLED — count re-pinned] Acceptance criteria cite ONLY machine-derived
  counts with reporter output committed beside the claim. Current pin (latex-paper @
  `952c897`, §2): `Test Files 14 passed (14)` / `Tests 127 passed (127)`. All prior
  pins struck (§2). glass-ui + clone counts pinned the same way at their gates.
- **VR-74** [SETTLED] Fleet hygiene: isolated `newContext()` per Playwright lane;
  prototype lanes reuse the scratchpad clones; nothing built in a clone travels to a
  real landing uncommitted-reviewed. Real-repo writes this pass are LIMITED to the
  `952c897` fix (landed) + docs under `docs/tranches/VIRT/`.
- **VR-75** [SETTLED — NEW, from F-3] **The timeline-host law.** A `ViewTimeline` used
  to animate content inside a CV subtree MUST be declared ON the CV host element
  (`view-timeline` on the contained box; scoped/named consumption below it). Anonymous
  `view()` on a descendant of a skipped box is BANNED (broken both engines, §1 F-3).
  Detect skip-state events by `addEventListener` — the `on*` prototype property reads
  false in both engines while the event fires. `--scroll-tl` scrub scalars are exact
  over CV. Owner: glass-ui `./virtual` docs + the vft binding; the atlas `Beat.vue`
  model is measured-correct as-is and is the reference implementation.
- **VR-76** [SETTLED — NEW, from the P6 extraction; **+ one named residual (#10)**]
  **The ElementRef structural-ref surface rule.** Nominal `Ref<T>` does not cross the
  `file:`-linked package boundary (glass-ui's own `useResizeObserver` documents the
  trap). EVERY consumer-facing glass-ui option that accepts an element or ref uses the
  structural `ElementRef` contract (landed in `sidebar/types.ts`, `p6-glass-ui @
  ea7fac8c`; made mechanically true across the surface at pick `2133597d` —
  `ScrollToOptions.visibleCount` `Ref` → `RefLike`, the last nominal input ref);
  vue-tsc across the boundary is the gate. A standing surface rule for all future
  options, not a one-off. **Residual #10 (`W2-SKELETON.md`, a W2 DECISION):**
  `SectionViewportOptions.items` carries vue's `MaybeRefOrGetter`, whose `Ref` arm is
  nominal and does not cross the boundary — consuming `toValue(options.items)` inside
  latex-paper needs a re-brand cast (documented at the cast site,
  `usePaperViewport.ts`). W2 either accepts the documented cast or adds a structural
  `ItemsSource<T> = readonly T[] | { value: readonly T[] } | (() => readonly T[])` to
  the surface; passing getters/arrays (every known consumer) is unaffected.

---

## 5. THE TWO SHAPE FORKS — settle protocols, EXECUTED (both verdicts committed; kept verbatim as the decision record)

The protocols below are the pre-committed decision rules the verdicts fired
mechanically; closure state per fork is appended, and the ONE disk-truth correction
(the §5.2 branch-(b) semver consequence, reversed by the F-9 verdict) is folded in
where it stood — the stale "leans 5.0.0" wording does not survive this re-cut.

### 5.1 F-7 — containment × real-paper CSS (gated VR-45/47/48) — **EXECUTED, SETTLED**

**Question.** Which box carries `content-visibility` so that the sticky-header design
survives and nested papers neither double-contain nor under-seed.

**Fixtures.** (a) fourier's real paper (98 flat sections, library-parsed); (b) a real
nested-section paper (≥3 depth levels). **Environment.** Prod builds; Chromium AND real
WebKit; 3 viewports (390×844, 1280×800, 1920×1080); isolated `newContext()` per lane
(VR-74); the **CV-off render is the reference** in every diff.

**F-7a — sticky-header survival.** Arms: (i) CV on `.paper-section` (the naive control —
expected FAIL by containment law); (ii) **CV on an inner `.section-body`** wrapper that
excludes the header; (iii) headers hoisted out of the contained box.
Measures, per arm × engine × viewport: sticky-offset assertion at ≥5 sampled scrollTops
per section (chapter header `rect.top === 0`, sub-header `=== 3.5rem`, while the section
spans the viewport) + screenshot pixel-diff of the header band vs the CV-off reference.
**PASS = sticky offsets exact within ±1px at every sample AND ≤0.5% differing pixels in
the header band AND no z-order/paint-clipping artifact, on BOTH engines at ALL 3
viewports.**
**Decision rule:** ship the first PASSING arm in the order (ii) → (iii). If (i)
unexpectedly passes both engines, ship (i) (simplest wins). If (ii) and (iii) BOTH fail
on both engines, sticky demotes to static — recorded as a VR-47 amendment, last resort.

**F-7b — root-only vs per-level CV.** Arms: (A) CV + CIS on depth===0 roots only,
seeded by `subtreeEstimatedHeight`; (B) per-level CV with own-content seeds.
Measures: (1) rest-state pixel-diff vs the CV-off reference ≤0.5% per viewport;
(2) scrollbar honesty — pre-settle total document height within 15% of settled;
(3) landing `errorPx ≤ 1` via the VR-23 contract on 5 far targets including
deep-nested ids; (4) zero trailing-margin un-collapse deltas at section boundaries
(the G-g check).
**Decision rule:** (A) root-only ships if it passes all four clauses (the
pre-registered preferred shape, VR-48). (B) ships ONLY if (A) fails a clause that (B)
passes. If both fail, the fork escalates: CV moves to the inner `.section-body`
per-root (composing with the F-7a outcome) and the failing clause re-probes; W4 is
blocked until a passing shape exists.

**G-f — `.deferred-section` strip geometry** (rides the same lanes): on fourier under
governed mount, placeholder strip height = the seeded estimate ±10%; cumulative layout
shift during `idleRevealAll` ≤ 0.02; no visible strip flash at any viewport.

**Closure — EXECUTED.** Verdict table + pixel-diff artifacts committed under
`docs/tranches/VIRT/traces/f7/` (`F7-VERDICT.md` + per-arm JSON/screenshots, 120
lanes). **Outcome: the strict clause-1/2 readings failed BOTH F-7b arms → the
escalation above fired mechanically → the escalation shape IS A-ii (already in the
matrix) and the two re-probes closed it** (geometry-split: A exact at 0.00px 12/12;
attribution: A's honesty misses are the VR-15/16 estimator's calibration error, B's
0.063–0.131 is structural). Arm ii won F-7a in the pre-committed order (arm i rejected
on Chromium repaint ghosting). G-f PASS (strip height = seed exactly, 94 strips × 12
lanes, 0% error; CLS 0.0002–0.0052; zero in-viewport reveals under idle AND a
0.6vh/120ms scroll chase). VR-45/47/48 flipped in §3; the estimator recalibration
lifted into VR-15/16.

### 5.2 F-9 — the words virtual-copy disposition (the branch selector; set VR-57 + VR-71) — **EXECUTED, SETTLED: branch (b)**

**Question.** Is `words/frontend/src/composables/virtual/` consumed document-shaped or
feed/grid-shaped?

**The read (ONE clean pass, no code changes):** enumerate every importer of the copy's
modules (grep + module graph over `words/frontend/src`); classify each consumer:
**document-shaped** = renders one contiguous prose document where the VR-4..9 semantics
(find-in-page, print, reader mode, fragment nav) apply; **feed/grid-shaped** = windows a
collection (list/grid/results) where those semantics do not apply.

**Decision rule (pre-committed):**
- **(a) ALL consumers document-shaped** → words migrates to the native core (VR-50);
  the windower family (`useVirtualSectionWindow`, `virtualSectionLayout`,
  `useWindowedStore`) **DELETES EVERYWHERE** — zero repos under the VR-72 gate; VR-57
  closes as *migrate*; VR-71 mechanical rule → the cut removes a published `./virtual`
  export → **glass-ui 5.0.0**.
- **(b) ANY consumer feed/grid-shaped** → the windower **SURVIVES at the glass-ui root
  ONLY** (`./virtual`); words' copy deletes and words becomes the windower's first
  external consumer; VR-57 closes as *survive-at-root*. **VR-71 consequence, CORRECTED
  by the executed verdict's disk truth (the pass-2 wording assumed a signature
  generalization → 5.0.0; the windower is ALREADY published in 4.2.0 unchanged and
  words consumes it unchanged):** branch (b)'s natural landing is the **4.3.0 additive
  corner** — the document core lands beside the retained windower with zero
  removal/signature-break; **5.0.0 is contingent** on W2/W5 electing a removal or
  re-signature (`traces/F9-VERDICT.md §4`).
- Either branch: words' `@tanstack/vue-virtual` grid (`useVirtualGrid.ts`) stays
  words-local (the ≥2-consumer bar); either branch closes the VR-72 gate.

**Verdict, EXECUTED (supersedes the pass-2 leaning):** the one-clean-read protocol ran
over `words/frontend/src` — **4/4 importers feed/grid-shaped, zero document-shaped**
(the full consumer table with `file:line` quotes in `traces/F9-VERDICT.md §1`); the
"ANY consumer feed/grid" rule fired unambiguously → **branch (b), 4.3.0 primary.** A
branch selector, not an architecture risk (dismissal D-1 stands) — and branch (b) was
found ALREADY HALF-EXECUTED on disk (the tranche-BC windower re-mint, verdict §2).

**Closure — EXECUTED.** The consumer table (file → import → classification → branch)
committed as `docs/tranches/VIRT/traces/F9-VERDICT.md` (latex-paper `f246a11`,
docs-only); VR-57 + VR-71's value flipped in this re-cut (§4).

---

## 6. THE W3 RUNTIME-GATE EVIDENCE SET — EXECUTED (pass-3 lanes E3 + E4, dual-engine)

The set ran on real engines this pass (Chromium 149.0.7827.55, WebKit 26.5, Firefox
151.0 for reader mode; isolated `newContext()` per lane, VR-74), in the `virt-p4`
clone at `761ba9d` (W3a, §6.1–§6.2) and `4391629` (W3b, §6.3–§6.4). **The text
artifacts (verdicts + machine reports) are PROMOTED into
`docs/tranches/VIRT/traces/w3/`** (`W3-INDEX.md` names what each file is and which
binaries — screenshots, the two 18-page PDFs, the harness bundles — remain clone-side
at those exact SHAs, cited the same way §0 cites `p6-glass-ui @ ea7fac8c`).

1. **`compensateScrollTop` on REAL WebKit — GREEN** (VR-2/21; the load-bearing
   stable-Safari path, previously unit-tested only). Both growth arms (rendered band
   +400px, CV-skipped box +600px): jumps ≤0.25px; absorbed/applied 1000/1000 —
   exactly-one-corrector; native branch verified both engines. Honesty: the
   stable-Safari arm is EMULATION on WebKit 26.5 trunk (shimmed `CSS.supports` +
   `overflow-anchor:none` forces the manual path as the only corrector by
   construction, same real engine); a manual old-Safari spot-check on the landed W2
   substrate is the last inch (W3-wave residual). → `traces/w3/W3A-VERDICT.md` §6.1.
2. **Print render-all — GREEN to the limit of automation** (VR-24): `beforeprint`
   ALONE reveals 8 → 60 both engines; governed `page.pdf()` ≡ eager reference (18 = 18
   pages, byte-identical extracted text); the OS dialog is unautomatable headlessly —
   manual Cmd+P is the last inch. New W4 hardening: `break-inside: avoid` (VR-24).
   → `traces/w3/W3A-VERDICT.md` §6.2.
3. **SSR-hydration damage guard — GREEN + proven LOAD-BEARING** (VR-25): zero-mismatch
   hydrate dual-engine, 25ms sampler never below 60/60; the blinded-guard NEGATIVE
   control truncates 60 → 8, sentinel lost. → `traces/w3/W3B-VERDICT.md` §A.
4. **axe + keyboard walk (VR-29) + reader-mode extraction (VR-30) — GREEN, 13/13
   clauses** (machine-recomputed by the clone's `verdict.mjs`, exit 0): axe zero
   critical/serious both engines; focus never lost across reveal; PRM
   0-interpolation vs 91-position smooth control; Readability governed ≡ eager at
   1.0000 on real Firefox (+ the `.section-header` W4 markup finding, lifted into
   VR-30). → `traces/w3/W3B-VERDICT.md` §B + `w3b-axe-report.json`.
5. **find-over-CV WebKit re-confirm** (VR-22) — rides the W3 implementing wave's
   re-run against the LANDED W2 substrate (low-risk: P2 already proved it green on
   real WebKit 26.5; the substrate CSS shape it re-confirms is now F-7-settled).
6. **F-5 spot-checks** (VR-49 residual, low-risk by the spec's own §1 assessment):
   the vft window-scroller arm + a WebKit run of the offsetTop-primary tracker —
   ride the same W3-wave re-run.

Items 1–4 — including the single most load-bearing item, the real-WebKit
`compensateScrollTop` run — are DONE with committed artifacts; items 5–6 are the two
pre-declared low-risk spot-checks that always belonged to the W3 wave's re-run against
the landed substrate (§7 W3 row), plus the two manual last-inch checks named above.

---

## 7. WAVES + LANDING-READINESS (owner + vehicle per wave; migration order per VR-70)

**The W2 skeleton promotion — DONE (`W2-SKELETON.md`, the cherry-pick manifest):** the
proven clone commits are PROMOTED into ONE consolidated skeleton branch per clone —
clone-side only (VR-74; the real glass-ui repo was not written this pass):
**`p6-glass-ui @ w2-skeleton (2133597d)`** — 5 picks IN ORDER (`ea7fac8c` the P6
extraction + peer-meta, `b74875c3` the P7-proven navigation/settle shapes incl. the
`MIN_TRAVEL_MS=3000` wall-clock fix, `fd516982` the F-1 constants + hydration guard,
`2b27d7c7` the VR-53 source-unify, `2133597d` the VR-56/75/76 rules expressed); gates
green at tip (123 files / 1176 tests, build + vue-tsc + `proof:virtual-window` exit 0)
— and **`p6-latex-paper @ w2-skeleton (aff7c73)`** — 3 picks (`62a18e7` the P6
consumption/twin deletion, `12ed7e4` the paper substrate consumption, `aff7c73` the
F-7 SETTLED CSS shape) **+ the skip-marked `1616c6b`** (the clone-side twin of
`952c897`, already on real master — SKIP when landing); gates green at tip (15 files /
136 tests with the golden block running, build exit 0). Full manifest, diffstats,
residuals #1–#10, and the VR-72 gate run: `W2-SKELETON.md`.

| Wave | Name | Owner repo → vehicle | Goal criterion | Closes on (evidence) |
|------|------|----------------------|----------------|----------------------|
| W1 | Pass-3 Evidence Fleet | scratchpad clones → verdict files in this folder | **DONE** — F-7 settled per §5.1; F-9 settled per §5.2; the §6 W3 lanes E3/E4 executed | `traces/f7/F7-VERDICT.md` + artifacts; `traces/F9-VERDICT.md`; `traces/w3/` (verdicts + reports; binaries at `virt-p4 @ 761ba9d`/`4391629`) |
| W2 | The glass-ui Substrate | glass-ui → **4.3.0** (additive corner per VR-71 × F-9; 5.0.0 ONLY if this cut or W5 removes/signature-breaks a published `./virtual` export) | The promoted skeleton (`p6-glass-ui @ w2-skeleton 2133597d`, 5 picks) lands at the root: peer-meta (VR-51), core at `./virtual` (VR-50: VR-13 constants, VR-19 epoch gate, VR-23 wall-clock scrollTo, VR-2/21 two-path anchoring, VR-17/18 width epoch), sidebar rebase + source-unify (VR-53/54/56), ElementRef rule + the residual-#10 decision (VR-76), search asserts (VR-62/65/66), VR-75 docs | glass-ui build + vitest + `proof:virtual-window` green (counts per VR-73; skeleton pin 123f/1176t); consumer-evidence docs updated |
| W3 | Semantics Runtime Proofs | both repos' clones → committed artifacts | The §6 set re-run against the landed W2 substrate (the `traces/w3/` lanes are the template) + the §6.5/§6.6 spot-checks + the manual Cmd+P and old-Safari last-inch checks | dual-engine Playwright artifacts; PDF; axe report |
| W4 | latex-paper Consumption | latex-paper → changesets v0.3.0 (from `p6-latex-paper @ w2-skeleton aff7c73`, 3 picks + skip `1616c6b`) | Twins deleted; glass-ui dep + adapters (VR-52/61); theme substrate per the F-7 verdict (VR-45/47/48 SETTLED CSS: flow-root norm, root-only `.section-body` CV, print escape + `break-inside: avoid`) + tokens (VR-37) + headingLevel (VR-26) + aria-labelledby (VR-27); **the Reader-View `.section-header` rename (VR-30 W4 finding)**; **the VR-15/16 estimator recalibration (gate ≥0.85 @ 390×844)**; MIGRATION complete (VR-31/32/58 + G-c5-narrowed/G-c6 notes) | Build + test exit 0 (VR-73 pin; skeleton 15f/136t); deletion diff; oklch smoke screenshot |
| W5 | Consumer Migrations | fourier, words, vft (sci-report docs) | fourier: windowing + search-dir net-deletion (7 files), first `./search` paper consumer (VR-60/67); words: the F-9 branch (b) executed per `F9-VERDICT.md §3` (delete the 3 windower files −726 LOC, re-point 3 imports, bump glass-ui, keep `useVirtualGrid`); vft: binding doc (`.paper-article` scoping + `scroll-margin-top` + window model + VR-75) | fourier build + runtime probe on its real paper; consumer LOC diffs; VR-72 grep dry-run green |
| W6 | Release + Gate Close | both roots → npm cuts | Both versions cut; VR-72 gate (path-qualified, **with the G-vgate2 exclusion set encoded**) + atlas census-gate update; π visual lane (≥3 viewports, PRM on/off, sticky-header pixel check per the F-7a verdict) | Tags + CI publish green; VR-72 grep output committed; π artifacts; before/after frame-budget ledger incl. WebKit |

≤3-concurrent dispatch (concurrency law); zero-deferral; gates are
build/test/runtime/benchmark artifacts only. This table is the wave SKETCH at
landing-readiness: each row already names owner, publish vehicle, goal criterion, and
closing evidence; at convergence 100 the full `VIRT.md` is cut, expanding every row to
the house WAVE_SPEC 11-section bar (per-wave prompts, gate scripts, rollback, and
coordination — `docs/tranches/VIRT/coordination/` opens if vft V4 runs concurrently in
sci-report). The π lane is binding (search modal, sticky headers, TOC states ride the
glass surface/controlSize registers — no bespoke chrome).

---

## 8. THE REGISTER INDEX

| VR | Subject | Status | Evidence pointer |
|----|---------|--------|------------------|
| 1 | dynamic height stability | SETTLED | P3 registry + P2 CV memory |
| 2 | no above-viewport jump (two-path) | SETTLED (runtime-proven) | P1/P2/P3 0.0px; R11 floors; §6.1 executed — `traces/w3/W3A-VERDICT.md` |
| 3 | programmatic nav | SETTLED | P3 LandingReport |
| 4 | find-in-page | SETTLED | P2 both engines; floor 26.0 |
| 5 | fragment/text-fragment nav | SETTLED | P2/P4; 26.2 |
| 6 | a11y heading/landmark tree | SETTLED | via VR-26/27 |
| 7 | complete print | SETTLED | P4 PDF; §6.2 executed (`traces/w3/W3A-VERDICT.md`) |
| 8 | reader mode (`textContent`) | SETTLED | P7 sentinel probe, both engines |
| 9 | view-timeline reveals over CV | SETTLED | `traces/p8/p8-f3{,b}-results.json` |
| 10 | off-screen deferral | SETTLED | `traces/f1-verdict.json` R4 PASS |
| 11 | the SOTA target statement | SETTLED | pass-1 §1.1 |
| 12 | document-native architecture | SETTLED | A.0; §2.2 strikes hold |
| 13 | mount governor default (64/4/true) | SETTLED (expressed in code) | `traces/F1-VERDICT.md` + JSONs; fourier 891ms eager; skeleton `fd516982` — 64/65 boundary tested |
| 14 | ranked contingency pair (dormant) | SETTLED | R5 not triggered |
| 15 | CIS seeding | SETTLED + W4 recalibration gate | F-7b executed (`traces/f7/F7-VERDICT.md`); mobile 0.759 < the 0.85@390px W4 gate |
| 16 | simplest width-aware estimator | SETTLED | A.6; G-b closed; narrow-width recalibration rides VR-15's W4 gate |
| 17/18 | RO registry + width epoch | SETTLED | P3 unit tests; `p6-glass-ui` |
| 19 | fonts.load + settle-EPOCH gate | SETTLED (`deriveFontFaceProbes` wired as default) | P3 falsification; C-3 green unit test (P6/P7); skeleton `12ed7e4` removes `KATEX_FONT_PROBES` |
| 20 | image dimensions | SETTLED | VR-2 two-path |
| 21 | anchor stability | SETTLED (REAL-WebKit runtime-proven) | P3 0.00px; §6.1 executed — absorbed==applied==1000, `traces/w3/W3A-VERDICT.md` |
| 22 | WebKit find floor 26.0 | SETTLED | R11 + P2 26.5 probe; §6.5 re-confirm rides the W3 wave |
| 23 | settle-aware scrollTo + hash nav | SETTLED (wall-clock ≥3s bound at the root) | P7 shell; P8 epoch-based arrival; skeleton `b74875c3` `MIN_TRAVEL_MS=3000` (frame-count regression caught + fixed) |
| 24 | Promise-print | SETTLED (§6.2 executed) | P4; `traces/w3/W3A-VERDICT.md` — 18=18pp byte-identical; manual Cmd+P last inch; W4 `break-inside` hardening |
| 25 | SSR archived + guards | SETTLED (guard runtime-proven load-bearing) | P4; G-c6 note; §6.3 executed — blinded-guard control 60→8, `traces/w3/W3B-VERDICT.md` |
| 26 | min-normalized headingLevel | SETTLED | P4; W4 plumbing; skeleton `12ed7e4` |
| 27 | aria-labelledby regions | SETTLED | P4 falsification; skeleton `12ed7e4` |
| 28 | PRM incl. damping loop | SETTLED (implemented) | P4 + R10; `p6-glass-ui` PRM snap; W3b PRM arm green |
| 29 | focus | SETTLED (§6.4 executed) | axe zero critical/serious + focus-retained keyboard walk, `traces/w3/W3B-VERDICT.md` |
| 30 | reader extraction gate | SETTLED (governor) + W4 markup finding | §6.4 executed — governed ≡ eager 1.0000; `.section-header` Readability strip → W4 rename |
| 31 | semver honesty / removals / MIGRATION | SETTLED | semver re-run; G-c5 note |
| 32 | byte-compat floor (re-exports) | SETTLED (proven) | `p6-latex-paper @ 62a18e7` |
| 33 | breaks list | SETTLED | pass-1 + VR-48 |
| 34 | usePaperViewport adapter | SETTLED (implemented) | `p6-latex-paper`; vue-tsc boundary |
| 35 | scrollToId promotion | SETTLED | 2-consumer test |
| 36 | ONE reader authority | SETTLED | R10 race ended; VR-49 |
| 37 | full-color two-tier tokens | SETTLED | P4 live probe |
| 38 | latex-paper release vehicle | SETTLED | changesets CI |
| 39–42 | Vue core hygiene | SETTLED | R8/P1/P4 |
| 43 | v-memo (v-once shipped) | SETTLED | R10 correction |
| 44 | O(1) lookups / binary search | SETTLED | glass-ui root |
| 45 | CSS substrate + spacing | **SETTLED** | F-7 executed — `traces/f7/F7-VERDICT.md` final CSS block (flow-root norm + print escape); skeleton `aff7c73` |
| 46 | test gates | SETTLED | counts per VR-73 |
| 47 | sticky × containment | **SETTLED** | F-7a executed — carrier = inner `.section-body` (arm ii); sticky offsets 0.00px both engines; arm i rejected on Chromium repaint ghost |
| 48 | root-only CV + subtree seed | **SETTLED** | F-7b executed — root-only (depth===0) + `subtreeEstimatedHeight`; per-level rejected on WebKit 414px divergence |
| 49 | ONE activeId authority + floor | SETTLED | `traces/p8/p8-f5-results.json`; `p6-glass-ui` |
| 50 | glass-ui owns the core | SETTLED (extraction-proven) | `p6-glass-ui @ ea7fac8c` |
| 51 | peer-meta optional | SETTLED (expressed in skeleton; lands at the root with W2) | skeleton `ea7fac8c` clone `package.json`; 4.2.0 root still mandatory |
| 52 | latex-paper adapter + dep | SETTLED (proven) | `p6-latex-paper @ 62a18e7` |
| 53 | tracker rebase | SETTLED (source-unify IMPLEMENTED) | skeleton `2b27d7c7` — `getPositions()` + `positionSource`; `getElementById` spy-asserted uncalled |
| 54 | sidebarFollow single home | SETTLED (implemented) | P6 dedupe + PRM snap |
| 55 | glass-ui TOC to spec | SETTLED | consumer map |
| 56 | shared-leaf canon (per-concern fence) | SETTLED (reworded + expressed) | P6; G-c3; skeleton `2133597d` |
| 57 | words copy disposition | **SETTLED — survive-at-root** | F-9 executed — `traces/F9-VERDICT.md`: 4/4 importers feed/grid, 0 document-shaped |
| 58 | leadingOffsetPx subsumed | SETTLED | P4 + contentEl |
| 60 | ONE search surface | SETTLED | R13; fourier dir deletion |
| 61 | toPaperSearchItems adapter | SETTLED (implemented + tested) | `p6-latex-paper` |
| 62 | CSS Custom Highlight path | SETTLED (P8-extended) | `traces/p8/p8-search-results.json` |
| 63 | Cmd+K law | SETTLED | R13 |
| 64 | result-nav contract (corrected ×2) | SETTLED | P8 |
| 65 | index from transform | SETTLED | R13 |
| 66 | cache/lifecycle discipline | SETTLED | shipped; gate-asserted |
| 67 | first search consumer (W5) | SETTLED | precept; seam stub-free (P6) |
| 70 | migration order + owner/vehicle | SETTLED | §7 |
| 71 | glass-ui semver (mechanical rule) | **SETTLED — value = 4.3.0 additive corner (5.0.0 contingent)** | `traces/F9-VERDICT.md §4` + `W2-SKELETON.md §4.2`; corrects the pass-2 "leans 5.0.0" |
| 72 | twin-deletion gate (path-qualified + exclusion set) | SETTLED — gate PASS for {glass-ui, latex-paper} | G-vgate + G-vgate2; `W2-SKELETON.md §3`: 7/7 sidebar twins exactly-one; windower + fourier `search/` = the two named W5 pendings |
| 73 | test-count discipline | SETTLED (re-pinned) | §2: 14f/127t @ `952c897`; skeleton pins 123f/1176t (glass-ui) + 15f/136t (latex-paper), reconciled |
| 74 | fleet hygiene | SETTLED | VR-74 |
| **75** | **timeline-host law** | **SETTLED (new + expressed)** | P8 F-3, both engines; skeleton `2133597d` barrel docs |
| **76** | **ElementRef surface rule** | **SETTLED (new) + residual #10 named** | P6 `sidebar/types.ts`; vue-tsc boundary; the `MaybeRefOrGetter` cast-vs-`ItemsSource<T>` W2 decision |

Open set: **EMPTY** — every VR SETTLED. The named residuals are all wave-assigned work
items (W2 residual #10 decision, W4 recalibration + rename, W5 migrations, W3 re-run
spot-checks), not open forks. No VR references work outside the executed §5 protocols,
the §6 evidence set, and the §7 waves.

---

## 9. CONVERGENCE SELF-ASSESSMENT (RE-CUT, PASS 4)

The pass-3 spec-cut self-assessed 93 with four execution items outstanding. All four
EXECUTED on disk this pass (F-7 → `traces/f7/`, F-9 → `traces/F9-VERDICT.md`, the W3
set → `traces/w3/` + the clone SHAs, the W2 skeleton → `W2-SKELETON.md`), but the spec
was cut at 04:07 — BEFORE its own verdicts landed (F9 04:11, F7 05:07, W2-SKELETON
05:52) — and was never re-cut: a material recurrence of the exact B-1 staleness class
§2 declares closed, plus one INVERTED headline value (VR-71 "leans 5.0.0" vs the
verdict's 4.3.0-primary). `PASS-3-SYNTHESIS.md` booked these as **B-3 + B-4** and
capped the pass at **96** — five of six termination criteria holding, spec
self-coherence the one failure.

**This re-cut IS the closing edit (the §D checklist 1–5, all mechanical, zero
research):** VR-45/47/48/57 flipped SETTLED with evidence pointers; the open fork set
set to EMPTY in §1/§8; VR-71 corrected everywhere to 4.3.0-additive-corner-PRIMARY /
5.0.0-contingent (§0 targets, §4, §5.2 branch (b), the §7 W2 vehicle cell, §8); the
two W4 findings lifted into the register (VR-30 Reader-View rename; VR-15/16
0.85@390px recalibration gate); W2-SKELETON residual #10 named under VR-76 and the
G-vgate2 exclusion set encoded in VR-72; the §6 W3 pointers resolved (text artifacts
promoted into `traces/w3/`, binaries clone-cited at `virt-p4 @ 761ba9d`/`4391629` per
`W3-INDEX.md`). Every count herein is machine-derived and reconciled (§2, VR-73).

With the register self-coherent against its committed artifacts, all six termination
criteria hold: F-7 + F-9 settled with committed evidence; green suites + true machine
counts; the W3 artifact set present; the W2 skeleton promoted; the spec self-coherent.
**The loop terminates; the remaining act is the terminal plan cut** — `VIRT.md` +
`waves/` at the house WAVE_SPEC 11-section bar, expanded from the §7 table (per-wave
prompts, gate scripts, rollback, coordination), with the migration order per VR-70:
glass-ui 4.3.0 → latex-paper 0.3.0 → fourier → words → vft. Per the loop's
settle-then-land discipline, implementation is the tranche, not the loop.
