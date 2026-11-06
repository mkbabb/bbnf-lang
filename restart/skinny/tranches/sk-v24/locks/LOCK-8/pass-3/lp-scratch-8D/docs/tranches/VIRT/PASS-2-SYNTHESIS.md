# VIRT — SOTA Virtualization + Cross-Repo Abstraction — PASS-2 SYNTHESIS (AGGLOMERATED)

Status: **PASS 2 of the convergence loop, agglomerated.** Supersedes the self-assessment
in `SPEC-PASS-2.md §8`. This file reconciles the pass-2 corpus into ONE authoritative
state: `SPEC-PASS-2.md` (the pass-2 spec), the five concrete/spec-augmented lanes
(P5 F-1 benchmark, P6 extraction, P7 unified shell, P8 behavior probes), and five critique
lanes — each re-verified against on-disk artifacts and two independent suite runs.

Two roots (ROOT-REPO LAW): `@mkbabb/glass-ui` 4.2.0 → next cut (the owning root for the
headless core, decision D-A) and `@mkbabb/latex-paper` v0.2.1 → v0.3.0 (consumes the core).

**Overall convergence: 90%.** Fork-and-blocker-capped, not blind-averaged. Justification +
the checklist to 100 in §D. The headline of pass 2 — the abstraction fork D-A — is SETTLED
on multi-repo on-disk evidence AND survived a real two-clone extraction (P6/P7). The one
convergence-capping fork of pass 1 (F-1, the mount default) is now CLOSED on committed
throttled traces (P5). F-3 and F-5 also closed (P8). What remains below 100 is a small,
bounded set: two spec-coherence defects the loop's own laws forbid (a stale spec and a RED
test on master), three engineering forks that gate *shape not direction* (F-7 CSS, F-9
semver branch, the two F-1 constants), and the un-run W3 real-engine gates.

---

## A. STATE OF EVERY FORK AFTER PASS 2

### A-D-A — the abstraction fork → **DECIDED (a): glass-ui owns the headless core**

The first architecture decision of the tranche. **SETTLED, and now falsifier-survived.**
The pass-1 premise "glass-ui fork is zero-consumer / possibly unmapped" is FALSIFIED and
struck: verified on disk this pass — the engine exists in **THREE** repos
(`glass-ui/src/composables/virtual/`, `latex-paper/src/vue/composables/` + `tracking/`,
and `words/frontend/src/composables/virtual/`), and the glass-ui sidebar/search surfaces
have live external consumers (words `ProgressiveSidebar`, bbnf-lang `DocsSidebar`, fourier
`PaperSidebar`; value.js/bbnf-lang search). glass-ui is BOTH origin and current home
(re-homed at tranche BC `c56586c0`).

**The evidence is not just documentary — P6 executed the decision for real in two clones**
(`p6-glass-ui @ ea7fac8c`, `p6-latex-paper @ 62a18e7`, both present on disk):
- The document-native core landed ONCE at glass-ui `./virtual` (9 modules); latex-paper's
  `usePaperViewport` became a ~15-line typed adapter (`FlatPaperSection extends FlatSection`,
  vue-tsc-validated across the `file:` package boundary — the falsifier: feeding
  `flattenPaperSections` output straight into the generic layout engine).
- **All 10 twin filenames now resolve to exactly ONE path** under `p6-glass-ui/src`
  (VR-72 grep gate green in the clone); latex-paper's twins physically deleted, no shims,
  no dangling imports.
- Both clones BUILD and both suites are GREEN (glass-ui vitest green; latex-paper green).
- Options (b) new-package and (c) latex-paper-owns are refuted on evidence (overfitting;
  inverted platform relationship + latex-paper has no search source).

**One binding amendment W2 must codify (P6's headline friction):** nominal `Ref<T>` does
NOT cross the `file:`-linked package boundary — glass-ui's own `useResizeObserver`
documents this trap. Consumer-facing glass-ui options MUST use the structural `ElementRef`
contract P6 landed in `sidebar/types.ts`, or every cross-repo consumer breaks at typecheck.
This is a surface RULE, not a one-off fix.

### F-1 — the mount default (the pass-1 capping fork) → **CLOSED (P5)**

The three-way throttled trace exists and is committed (`traces/F1-VERDICT.md` +
`f1-results.json` + `f1-smalln.json` + `f1-verdict.json`, all on disk, machine-computed
verdict). The pre-committed §4 rule fired mechanically:
- **R2: shipped default = `mount: 'governed'`.** **R4 PASS on all three clauses** on the
  220-uniquemath fixture at 4× throttle: worst governed longtask 133ms (< 200 budget);
  scroll avg +0.2% vs windowed reference with 0 frames > 33ms; far-target LandingReport
  errorPx 0.09–0.31 (≤ 1). **R5 not triggered** (neither VR-14 fallback fires).
- **The real-consumer datum** is the strongest single input: fourier's ACTUAL paper (parsed
  by the library's own vite plugin — **98 flat sections**, not the ~60 estimate) eager-mounts
  at **891ms** worst longtask (4.5× budget) vs governed 123ms with FCP 260ms. Pass-1's
  "~150–190ms at the real consumer" extrapolation is falsified in both inputs.
- **VR-10 and VR-13 flip PROVISIONAL → SETTLED.**

**Two constants remain un-picked (the residual — a wording fork, not a direction fork):**
(1) the eager auto-select threshold: literal `S_cross=32 → floor(0.8×32)=25` vs the
mount-attributable decomposed `S_cross=80 → 64` (the demo's `main.ts` fuses bundle-eval +
Vue-mount into one longtask; the decomposed number is transferable to code-split consumers);
(2) the batch default: the pre-committed "smallest under one frame budget" picks batch 2
(reveal 2.0s), but batch 4 also passes p95 (14.2 < 16.7ms), halves reveal to 1.11s, and
equals the VR-13 default. **Synthesis picks (binding for W2):** adopt the **decomposed
threshold 64** (the literal 25 is a demo-fixture artifact; every real consumer code-splits)
and **batchSize default 4** ("largest batch under one frame budget" — the engineering-correct
direction, matching VR-13). Both constants ship in the doc comment with the trace citation.
The consumer-facing outcome is identical either way (the 98-section real paper is governed).

### F-3 — view-timeline × CV skipped-state → **CLOSED (P8), new law VR-75**

Executed on real Chromium + real WebKit 26.5. Reveals fire ON SCHEDULE from the skipped
state in both engines **when the ViewTimeline is declared ON the CV host** (`wrapper` and
`named-inner` variants: cv=on ≡ cv=off; named-inner 0.000 deviation control). Anonymous
`view()` on a descendant of a skipped box is BROKEN (Chromium: timeline inactive while
skipped, `fill` void; WebKit: late-firing, deviation to 0.945). → **VR-75 timeline-host law**
(new, SETTLED). The atlas `Beat.vue` model is measured-correct as-is; vft adopts it verbatim.
`--scroll-tl` scrub scalars are exact over CV. **VR-9 flips OPEN → SETTLED.**

### F-5 — activeId mechanism A/B → **CLOSED (P8): offsetTop-primary wins**

offsetTop-table + binary-search: 0% transient TOC-highlight mismatch vs IntersectionObserver-
primary's 22.7% (lag exactly 1 section) under fast scroll + remeasure storms at 4× throttle,
at statistically indistinguishable jank (8.84 vs 9.01ms avg). IO demotes to optional in-DOM
enhancement, never authoritative (tightens VR-53). **VR-49 mechanism arm flips → SETTLED.**
Residual (low-risk, W1 one-day): the A/B is Chromium + element-scroller only; the vft
window-scroller arm and a WebKit spot-check are unrun.

### F-7 — containment × real-paper CSS → **OPEN (shape, not direction)**

Still genuinely open — P6/P7 did NOT touch the theme/CSS substrate (correctly out of scope).
Two named sub-probes gate the final shape of VR-45/47/48: **F-7a** whether the shipped
two-tier sticky header stack (`theme.css:443-465`, chapter `top:0 z:10` / sub `top:3.5rem
z:9`, verified by the R10 forensic) survives inside a CV-contained box — CV implies
`contain: layout paint`, so sticky cannot escape its containing block; the probe decides
CV-on-inner-`.section-body` vs headers-out-of-box. **F-7b** root-only (depth===0) vs
per-level CV on the real nested papers + the `subtreeEstimatedHeight` seed. Plus the G-f
`.deferred-section` strip geometry check on fourier. Direction is known (inner-wrapper /
root-only are the preferred candidates); pixel-diff evidence is not yet on disk.

### F-9 — the words virtual-copy disposition → **OPEN, but strongly leaning (new evidence)**

The ONE deciding read. New on-disk evidence this pass sharpens it toward decided: the words
copy directory carries `useVirtualGrid.ts` (a `@tanstack/vue-virtual` grid) ALONGSIDE the
section windower + `useWindowedStore.ts`. The grid consumers (wordlist search-mode,
definition/thesaurus views) are unambiguously **feed/grid-shaped**, not document-shaped.
Leaning: `useVirtualSectionWindow` survives at the glass-ui root as a windower with words as
its first external consumer → glass-ui's cut is **MAJOR (5.0.0)** if the published surface
keeps the windower, or minor (4.3.0) if the windower is fully deletable. One clean read
(what actually imports `words/.../virtual/`) closes it; the semver branch is nearly decided
toward major. This is a *branch selector*, not an architecture risk.

### Forks CLOSED this pass (confirmation)

F-1 (P5), F-3 (P8), F-5 (P8), plus the pass-1 closures carried forward: F-2 (WebKit floors
pinned; residual = W3 runtime gate, not a fork), F-4 (A.6), F-6 (A.4), F-8 (executed as R10).
The abstraction fork is D-A, decided AND extraction-proven. **Open set: F-7, F-9, and the
two F-1 constants above** (this synthesis picks them; they land in code at W2).

---

## B. THE RECONCILED VR REGISTER (status + evidence pointers)

Stable IDs. Corrections from P5/P6/P7/P8 folded. Only VRs whose status or evidence CHANGED
this pass are called out; the remainder hold their `SPEC-PASS-2.md §7` status.

### Flips and corrections landed this pass

| VR | Subject | Status (pass-2 final) | Evidence pointer |
|----|---------|-----------------------|------------------|
| 9  | view-timeline reveals over CV | **SETTLED** (was OPEN→F-3) | P8 `traces/p8/p8-f3{,b}-results.json`; both engines |
| 10 | off-screen render deferral | **SETTLED** (was PROVISIONAL) | P5 `traces/f1-verdict.json` R4 PASS |
| 13 | mount governor default | **SETTLED** (was PROVISIONAL); `mount:'governed'`, eager ≤ **64** (decomposed), batchSize **4**, idleRevealAll | P5 `F1-VERDICT.md` + traces; fourier 891ms eager quoted as the why |
| 8  | reader mode | SETTLED — **evidence wording corrected** | P7: sentinel absent from `innerText` under CV-skip, PRESENT in `textContent`, BOTH engines; cite `textContent` not `innerText` |
| 19 | fonts.load gate + settle epoch | SETTLED — **implemented** | P6/P7: epoch settle gate rewritten (monotone epoch, shared per-epoch promise, no mutable dirty flag); C-3 clobber scenario is a green unit test; `deriveFontFaceProbes` derives faces from transform output |
| 23 | settle-aware scrollTo + hash nav | SETTLED — **implemented + P8-hardened** | P7: scrollend-primary, ≥3s bound (90-frame cap gone), long-jump-instant, computed scroll-margin-top, leadingOffsetPx removed, unconditional final commit. P8: WebKit +257px arrives at frame 1 AFTER a correct frame-0 read → arrival detection MUST be whenSettled-epoch-based |
| 45 | CSS substrate + spacing | PROVISIONAL → **F-7** (unchanged; NOT touched by P6/P7) | theme substrate is W4; F-7a/F-7b pending |
| 47 | sticky × containment | PROVISIONAL → **F-7a** | R10 forensic (`theme.css:443-465`) verified line-accurate; probe unrun |
| 48 | root-only CV + subtree seed | PROVISIONAL → **F-7b** | R10; probe unrun |
| 49 | activeId semantic + mechanism | **SETTLED** (semantic + mechanism) | Semantic: R10 deepest-visible-wins. Mechanism: P8 offsetTop-primary (0% vs 22.7%). P6/P7: ONE `resolveActivePosition` authority implemented + unit-tested |
| 50 | glass-ui owns the core | **SETTLED + extraction-proven** | P6 `p6-glass-ui @ ea7fac8c` — 9 modules at `./virtual`, suites green |
| 51 | peer-meta optional | SETTLED — **verified unlanded** | glass-ui 4.2.0 `package.json`: `@lucide/vue`, `class-variance-authority`, `clsx`, `reka-ui`, `tailwindcss` still mandatory peers (NOT in peerDependenciesMeta); one-line fix, real work |
| 52 | latex-paper adapter + dep | **SETTLED + proven** | P6: `FlatPaperSection extends FlatSection` vue-tsc-validated across `file:` boundary; usePaperViewport ~15-line adapter |
| 53 | tracker rebase | SETTLED (semantic) — **source-unify residual named** | P6: `useScrollTracker` still reads per-tick getElementById rects + inits activeId to `roots[0].id` while the core owns the position table; W2 moves it onto the core table (two-authorities residual) |
| 54 | sidebarFollow single home | **SETTLED + implemented** | P6: deduped to glass-ui, VR-28 PRM snap + `scrollSource:'window'`; latex-paper 234-line copy deleted |
| 56 | shared-leaf canon | SETTLED — **fence reworded** | P6: the house `useResizeObserver` leaf watches ONE element; forcing N sections mints N observers. Fence = "ONE observer per concern" (the pipeline keeps its single RO), not "one wrapper" |
| 61 | toPaperSearchItems adapter | SETTLED — **implemented + unit-tested** | P6: maps transform-layer sections → `SearchableItem`, VR-64 nav contract tested |
| 62 | CSS Custom Highlight path | SETTLED — **extended** | P8: skipped-content Ranges constructible + registrable both engines; rect queries on skipped Ranges engine-divergent (WK 0×0) — never gate on them; painted-after-reveal verified (screenshot) |
| 64 | result-nav contract | SETTLED — **corrected ×2** | P8: (1) ALL result-nav scrolls go through VR-23 settle-aware contract (plain scrollIntoView is Chromium-only; WK +257px permanent otherwise); (2) governed: highlight-Range built AFTER `revealTo` (target null pre-reveal); CV: may precede; idempotent re-set after whenSettled |
| 67 | first search consumer | SETTLED — **deferred to W5 by design** | P6: FuzzySearch composition awaits fourier; adapter seam stub-free to its boundary |
| **75** | **timeline-host law (NEW)** | **SETTLED** | P8: ViewTimeline declared ON the CV host; anonymous `view()` on a skipped descendant BANNED; detect skip-state event by listening (the `on*` prototype property is false in both engines while the event fires). Owner: glass-ui `./virtual` docs + vft binding; Beat.vue is the reference, needs no change |
| 73 | test-count discipline | **SETTLED as a rule, but the pinned NUMBER is wrong** | See C-2 below — the "145+9=154" pin does not reproduce; the rule (machine-count only) stands, the number is struck |

All other VRs (1–7, 11–12, 14–18, 20–22, 24–46, 55, 57–58, 60, 63, 65–66, 70–72, 74) hold
their `SPEC-PASS-2.md §7` status. VR-57 stays OPEN → F-9 (leaning per §A). VR-71 semver stays
gated on F-9.

---

## C. THE CRITIQUE LEDGER (blockers/gaps deduped, assigned or dismissed)

### Blockers (must close before 100)

- **B-1 — the spec is STALE vs its own committed artifacts.** [ASSIGNED → this synthesis
  supersedes; W1 folds into the spec] `SPEC-PASS-2.md §4/§5/§8` say F-1 is OPEN and "P5
  executes" (future) and its PASS traces "do not exist yet" — but `traces/F1-VERDICT.md`
  closed F-1 (VR-10/13 SETTLED) and `P8-BEHAVIOR-PROBES.md` closed F-3/F-5 and added VR-75,
  all on disk (verified: file mtimes 03:16–03:42, spec mtime 02:56). The spec's §3/§5/§7/§8
  never reference P8 or VR-75 or the F-1 verdict. **This synthesis IS the reconciliation**
  (§A/§B/§D carry the closures); a spec editorial pass folds them into `SPEC-PASS-2.md`
  before W2 planning. Sibling artifacts even disagreed (P8 line 233 says "F-1, F-7, F-9
  remain the open set" while F1-VERDICT closes F-1) — this file is now the single fork-status
  source of truth.

- **B-2 — VR-73 pinned test count is unverifiable AND a RED test hides under it.**
  [ASSIGNED → W2 (glass-ui), W4 (latex-paper)] Two-fold, both re-verified by two independent
  runs this pass: (a) the spec pins "145 pass + 9 skip = 154 (viewport 27)"; the actual
  latex-paper suite at HEAD `4775845` is **126 passed + 1 FAILED + 0 skipped = 127** across
  14 files, with NO viewport test file (only `virtualSectionLayout.test.ts`). The C-2 closure
  number cannot be reproduced — the exact class of error C-2 was created to fix, recurring.
  (b) The 1 failure is **pre-existing RED on clean master**: `tests/paper.test.ts` golden
  test, "Unprocessed unbraced accent — `\ o` in 'Poles inside vs.\ outside'" in
  `fourier_paper.tex` via `validate.ts`. Working tree is clean (only `docs/tranches/`
  untracked), so this is a genuine red UNRELATED to VIRT (a tex-fixture/validator escape).
  The W2/W4 gates say "npm test exit 0" — they cannot pass until the fixture or validator is
  fixed. **Action:** (1) strike "145+9=154/viewport 27" everywhere; the VR-73 RULE
  (machine-derived counts, reporter output committed beside the claim) stands. (2) Fix the
  `fourier_paper.tex` `\ o` accent OR the `validate.ts` unbraced-accent matcher as a W0/W4
  pre-req, independent of VIRT, so the gate is green.

  *Note on the P6/P7 baseline counts:* two critiques independently found the P6/P7 docs'
  stated CLONE baseline (145+9 / 125+9) is also wrong — the true baseline at commit `4775845`
  is 118 passed + 9 skipped = 127. The final counts in both clones ARE reproducible
  (P6 latex-paper 121+9; P7 174-total), only the *deltas* were mis-narrated. This is a ledger
  defect, not a broken build — but it is the same false-count class and is corrected here:
  **latex-paper baseline = 118+9; P6 delta = +3 (−2 twin, +5 adapter); P7 delta = +46.**

### Gaps (assigned to W-work)

- **G-c1 (F-1 constants)** [ASSIGNED → W2] The literal-25 vs decomposed-64 threshold and the
  batch-2 vs batch-4 default were left as "flags for synthesis" in `F1-VERDICT.md`. **Picked
  here (§A F-1): threshold 64, batchSize 4.** Lands in code with the trace citation.
- **G-c2 (VR-53 source-unify)** [ASSIGNED → W2] P6 unified the SEMANTIC (one
  `resolveActivePosition`) but `useScrollTracker` still reads per-tick getElementById rects +
  inits `activeId` to `roots[0].id` — two position authorities until W2 moves the tracker
  onto the core position table.
- **G-c3 (VR-56 fence wording)** [ASSIGNED → W2] "one observer per concern," not "one
  wrapper" — the pipeline keeps its single RO; forcing the house per-element leaf onto N
  sections mints N observers.
- **G-c4 (ElementRef surface rule)** [ASSIGNED → W2] Structural refs on ALL consumer-facing
  glass-ui options, stated as a rule (P6's `file:`-boundary friction), else every new
  consumer re-breaks at typecheck.
- **G-c5 (headless scroll-margin footgun)** [ASSIGNED → W4 MIGRATION note] Headless
  `scrollToSection` ignores `scroll-margin-top` unless the caller forwards `scrollMarginTopPx`
  (the pure adapter has no DOM to read computed style); a direct-substrate consumer can
  mis-land 5rem. Document in the option + MIGRATION.
- **G-c6 (hydration-guard heuristic)** [ASSIGNED → W4 MIGRATION note] The VR-25 guard is
  "first-item-id already present at setup" — a pre-existing duplicate id false-positives into
  eager (safe direction, never truncates). Name it in the v0.3.0 MIGRATION notes.
- **G-w3 (W3 real-engine gates)** [ASSIGNED → W3] Still un-run with no artifacts: the real
  print DIALOG (only `page.pdf()` exercised; unautomatable headlessly), the WebKit
  `compensateScrollTop` growth-compensation arm (unit-tested only; the two-path anchoring
  no-anchor branch ran only in unit tests — R11 confirmed stable Safari through 26.x has NO
  `overflow-anchor`, so this IS the default stable-Safari path), find-over-CV WebKit
  re-confirm, real hydration, axe + keyboard walk, reader-mode extraction.
- **G-f7 (F-7 pixel-diffs)** [ASSIGNED → W1/W4] sticky-header survival (F-7a) + root-only vs
  per-level CV (F-7b) + `.deferred-section` strip geometry — no on-disk pixel evidence yet.
- **G-f9 (F-9 read)** [ASSIGNED → W1] one clean read of `words/.../virtual/` consumers;
  leaning feed/grid-shaped (the `useVirtualGrid.ts` sibling) → windower survives, glass-ui
  cut major.
- **G-vgate (VR-72 pattern)** [ASSIGNED → W6] the twin-deletion grep gate lists sidebar
  `types.ts`, which exists generically in many places — the gate needs a path-qualified
  pattern (`src/composables/{virtual,sidebar}/…`) or it false-positives/negatives.
- **G-prov (evidence provenance)** [ASSIGNED → spec edit] the R9–R13 dossiers cited as the
  spec's evidence base do not exist as files in `docs/tranches/VIRT/` — findings live inline.
  D-A + R10 are independently corroborated on disk (this synthesis re-verified), so D-A
  survives, but restate the pointers as direct disk citations rather than phantom dossiers.
- **G-sha (P5 bookkeeping)** [ASSIGNED → trivial] the P5 summary cites clone commit
  `31b39e3`; the actual `virt-p2` HEAD is `67cb0ca` (verified). Correct the SHA; non-substantive.

### Dismissed (with reasoning)

- **D-1 (F-9 as a coin-flip)** — DISMISSED as fully-open: the on-disk `useVirtualGrid.ts` +
  wordlist/definition/thesaurus consumers make it feed/grid-shaped with high confidence.
  Keep it as an OPEN *branch selector* for semver only, not an architecture risk.
- **D-2 (fresh-clone smoke failure)** — DISMISSED: fresh glass-ui clones with
  `--ignore-scripts` fail one pre-existing `FourierField.smoke` self-import of `dist/`
  until first build — a clone-recipe trap, not a VIRT defect.
- **D-3 (P8 `msToStable` comma-operator dead code)** — DISMISSED: `p8-search.mjs:140`'s
  comma-operator smell is not cited in any conclusion; clean before reuse, not load-bearing.
- **D-4 (F-3 unthrottled)** — DISMISSED as correctly-scoped: F-3 is a correctness probe, not
  a budget probe; the reveal-cost-under-throttle characterization is a W5 nicety, not a gate.

---

## D. OVERALL CONVERGENCE: 90% — JUSTIFICATION + CHECKLIST TO 100

Per-target critique numbers this pass: SPEC-PASS-2 74, P5 90, P6 86, P7 89, P8 90. These are
NOT averaged. The number is set by fork state + blocker severity, per the loop's law
(convergence 100 = implementable as-is, zero blockers, zero open forks, all load-bearing
claims verified).

**What moved the number up from pass-1's 82:**
- The abstraction fork D-A — the pass's first-class question — is DECIDED AND
  extraction-proven in two real clones (P6/P7), twins deleted to exactly one repo, both
  suites green. This is the strongest possible evidence short of landing.
- **F-1, the pass-1 capping fork, is CLOSED** on committed throttled three-way traces (P5),
  with the real-consumer 891ms-eager datum as the decisive input. VR-10/13 SETTLED.
- F-3 and F-5 CLOSED on real dual-engine probes (P8); VR-9 + VR-49-mechanism SETTLED; VR-75
  discovered and settled. The open-fork set shrank from {F-1,F-3,F-5,F-7,F-9} to {F-7,F-9}.
- All three pass-1 blockers (C-1/C-2/C-3) are closed by design/evidence, and C-3's epoch gate
  is implemented + unit-tested against the byte-verbatim legacy race (P6/P7).

**What separates the number from 100 — the checklist (each item is mechanical/bounded):**

1. **[BLOCKER B-1] Fold the closures into `SPEC-PASS-2.md`.** F-1/F-3/F-5 SETTLED, VR-75
   added, §5 open-set → {F-7, F-9}, §8 recomputed. This synthesis is the source of truth;
   the spec editorial pass makes it self-coherent. *(spec edit)*
2. **[BLOCKER B-2] Test-count + RED test.** Strike "145+9=154/viewport 27"; fix the
   `fourier_paper.tex` `\ o` unbraced-accent (or `validate.ts`) so `tests/paper.test.ts` is
   green and W2/W4 "npm test exit 0" can pass; commit reporter output beside every count
   claim (correct latex-paper baseline = 118+9). *(W0/W2/W4)*
3. **[FORK F-7] CSS substrate shape.** F-7a sticky-header survival at the chosen CV level +
   F-7b root-only vs per-level CV + `.deferred-section` geometry — pixel-diff evidence on
   real papers. Gates VR-45/47/48's final shape. *(W1 probe → W4 land)*
4. **[FORK F-9] words disposition + glass-ui semver branch.** One clean consumer read →
   major (5.0.0, windower survives) vs minor (4.3.0). Leaning major. *(W1 read)*
5. **[F-1 constants] threshold 64 + batchSize 4** land in code with trace citations
   (picked here; mechanical). *(W2)*
6. **[G-w3] W3 real-engine gates with committed artifacts:** WebKit find-over-CV re-confirm,
   the FIRST real-engine `compensateScrollTop` run (the load-bearing stable-Safari path),
   real print dialog, real hydration, axe + reader-mode. No artifacts on disk yet. *(W3)*
7. **[G-c2..c6 + G-vgate + G-prov + G-sha] the named residuals:** VR-53 source-unify,
   VR-56 fence wording, ElementRef surface rule, scroll-margin + hydration-guard MIGRATION
   notes, path-qualified VR-72 grep, provenance restatement, the P5 SHA. *(W2/W4/W6, trivial-to-small)*
8. **Then implement per the §6 wave plan** (W2 glass-ui substrate → W3 runtime proofs →
   W4 latex-paper consumption → W5 consumer migrations → W6 release + π close) and land the
   substrate + a consumer together.

**90%** reflects: the abstraction decision settled AND extraction-proven (+, the pass's
headline, worth the jump from 82), the capping fork F-1 closed on evidence (+), two more
forks closed (+), and the C-1/C-2/C-3 blockers closed — against a small residual of two
spec-coherence defects (B-1 stale spec, B-2 count + RED test) that the loop's own laws
forbid and that are purely mechanical, two engineering forks that gate *shape not direction*
(F-7, F-9), and the un-run W3 runtime gate set. An open fork touching implementation shape
(F-7 CSS) plus two un-landed blockers cap it below the mid-90s; but nothing architectural
or directional remains open, and both libraries' surfaces have been re-expressed over the
decided core and run green. Honesty over optimism: this pass is 90, not 100.

---

## E. RESEED BRIEF FOR PASS 3

The loop does NOT terminate at pass 2 — but it is close, and pass 3 is a *hardening +
landing-readiness* pass, not a research pass. The architecture and all directional forks are
decided; what remains is spec-coherence, two shape-forks with known-leaning answers, and
the runtime-gate evidence set.

**Pass-3 must, in ≤3-concurrent fleet dispatch (concurrency law), close:**

1. **The spec-coherence blockers (B-1, B-2) FIRST, as a synchronous pre-step** — fold F-1/
   F-3/F-5/VR-75 into `SPEC-PASS-2.md` (or cut `SPEC-PASS-3.md`), strike the bad count, and
   fix the RED `fourier_paper.tex` accent so the "npm test exit 0" gate is real. Nothing else
   in pass 3 is trustworthy while the spec disagrees with its artifacts and master is red.
2. **F-7 (the last shape fork)** — the sticky-header × containment pixel-diff (F-7a: CV on
   `.section-body` inner vs headers-out-of-box) + root-only vs per-level CV (F-7b) +
   `.deferred-section` geometry, on the real fourier + a nested paper, 3 viewports, real
   engines. Output: the final VR-45/47/48 CSS shape. This is the highest-value pass-3 probe.
3. **F-9 (the semver branch selector)** — one read of `words/.../virtual/` consumers
   (leaning feed/grid → windower survives → glass-ui major 5.0.0); sets VR-71.
4. **The W3 runtime-gate evidence set** — real WebKit `compensateScrollTop` (first real-engine
   run of the load-bearing stable-Safari anchoring path), real print dialog, real hydration,
   axe + keyboard walk, reader-mode extraction; committed Playwright/PDF/axe artifacts.
5. **Promote the P6/P7 clone commits to the W2 substrate skeleton** rather than re-deriving:
   `p6-glass-ui @ ea7fac8c` (epoch gate + `resolveActivePosition` + `useSectionViewport`,
   unit-tested at the owning root) is the substrate; the P7 unified shell (virt-p4) carries
   the runtime-proven print/hydration shape. Land the two F-1 constants (threshold 64,
   batchSize 4) and the named residuals (VR-53 source-unify, VR-56 fence, ElementRef surface
   rule) at the glass-ui root.

**Pass-3 termination criterion:** F-7 and F-9 settled with committed evidence; the spec
self-coherent with a green suite; the W3 artifact set present; the two constants + named
residuals landed at their owning roots. At that point convergence is 100 = implementable
as-is, zero blockers, zero open forks, and `VIRT.md` (the full wave plan) can be cut for
implementation. Estimated pass-3 convergence target: ≥97 (residual only the un-landed
implementation itself, per the loop's "settle then land" discipline).

Honesty over optimism: pass 2 is **90**. The distance to 100 is a bounded, mostly-mechanical
checklist — no architectural or directional question survives — but it is not zero, and the
two spec-coherence defects (a stale spec, a red master) are exactly the evidence-coherence
failures the loop forbids and must clear before pass 3 can trust its own ground.
