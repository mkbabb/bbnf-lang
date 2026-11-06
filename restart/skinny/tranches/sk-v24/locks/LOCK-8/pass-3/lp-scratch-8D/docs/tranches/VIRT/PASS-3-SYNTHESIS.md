# VIRT — SOTA Virtualization + Cross-Repo Abstraction — PASS-3 SYNTHESIS (AGGLOMERATED)

Status: **PASS 3 of the convergence loop, agglomerated — the hardening + landing-readiness
pass.** This file reconciles the pass-3 corpus into ONE authoritative state: `SPEC-PASS-3.md`
(the pass-3 spec, cut at 04:07), the RED-fix (`952c897`), the four evidence lanes (E1 F-7,
E2 F-9, E3 W3a, E4 W3b), the W2 skeleton-promotion lane, and three critique lanes — each
re-verified against on-disk artifacts and independent suite/mtime runs by the agglomerator.

Two roots (ROOT-REPO LAW): `@mkbabb/glass-ui` 4.2.0 → next cut (the owning root for the
headless core, decision D-A) and `@mkbabb/latex-paper` v0.2.1 → v0.3.0 (consumes the core).

**Overall convergence: 96%.** Fork-and-blocker-capped, not blind-averaged. The whole pass-3
research question is ANSWERED and the whole engineering work is DONE ON DISK — both shape
forks settled with committed evidence, the W3 runtime set executed on real engines, the W2
skeleton promoted, the RED test fixed, all counts machine-true. **What holds the loop open is
a single, mechanical spec-coherence act:** `SPEC-PASS-3.md` was cut BEFORE its own verdicts
landed and was never re-cut, so the authoritative spec-of-record disagrees with the artifacts
sitting beside it — a material recurrence of the B-1 class it declares closed, plus one
INVERTED semver value (VR-71). The termination criterion ("spec self-coherent with its
committed artifacts") is therefore not yet met. The loop does NOT terminate — but it is one
edit away, and the reseed is that one edit plus two un-lifted findings. Justification + the
residual checklist in §D.

---

## A. STATE OF EVERY FORK AFTER PASS 3

The pass-3 open set was exactly **{F-7, F-9}** (plus the §6 W3 evidence items, which are
artifacts not forks). Both forks are now SETTLED with committed, reproducible, real-engine
evidence, settled by the pre-committed §5.1/§5.2 decision rules (mechanically, not by fiat).

### F-7 — containment × real-paper CSS → **SETTLED (E1), by the §5.1 escalation**

Executed end-to-end on prod builds over real Chromium 149.0.7827.55 + real WebKit 26.5 at
three viewports, 120 isolated-context lanes, fourier's REAL paper (98 flat sections, library-
parsed) + a synthetic 3-depth nested fixture, CV-off render as the reference in every diff.
Verdict at `traces/f7/F7-VERDICT.md` (+ per-arm JSON, worst-diff screenshot triples).

- **F-7a → VR-47: carrier = inner `.section-body` (arm ii).** Sticky POSITIONING survives CV
  in BOTH engines (10,560 samples, worst offset delta **0.00px**, all arms — the corrected
  VR-47 fear). Arm i (naive CV on `.paper-section`) is REJECTED on Chromium header-repaint
  ghosting (0.66–2.32% of header pixels, 6/6 Chromium lanes; WebKit 0). Arm iii passes but is
  unneeded. **A sticky header must not pin INSIDE a paint-contained box** — the header stays
  outside the CV carrier (new VR-47 sub-law).
- **F-7b → VR-48: root-only CV (depth===0) + `subtreeEstimatedHeight` seed (arm A).** Rest
  GEOMETRY is exact (0.00px per-section tops/heights/gaps, 0.00% height delta, 12/12 lanes);
  the residual pixel is Chromium-only text-AA under containment (0 on WebKit everywhere). Arm
  B (per-level) is hard-REJECTED: structural scrollbar dishonesty (0.063–0.131) + real WebKit
  layout divergence up to **414px** on the real fourier paper (raw datum `f7b-results.json`
  results[39], agglomerator-confirmed). The strict clauses 1/2 failed both arms → the §5.1
  escalation fired → the re-probes closed on the escalation shape (A composed with the F-7a
  carrier = A-ii).
- **VR-45 amendment (the G-g normalization, part of the shipped substrate):**
  `display:flow-root` on `[data-lp-section]` and `.section-body` in ALL states — this makes
  CV-on ≡ CV-off geometry EXACTLY (0.00px/0.00% measured); without it containment drifts
  document geometry ±0.94–2.44%. Plus the `.section-body` print escape.
- **G-f PASS:** governed placeholder-strip height = seed exactly (94 strips × 12 lanes, 0%
  error), CLS 0.0002–0.0052 (≤0.02, Chromium; WebKit lacks layout-shift, recorded), zero
  in-viewport reveals, flash-free under idle AND a 0.6vh/120ms scroll chase.
- **New W4 work item (VR-15/16):** the narrow-width/dense-math estimator honesty measured
  0.759 at 390×844 on the real paper (< the 0.85 acceptance; ref's own settle-growth floor is
  0.918 at ≥1280px) — a real calibration deficit, correctly booked as W4, does NOT change the
  A-vs-B direction.

**Verdict:** VR-45/47/48 are READY to flip PROVISIONAL → SETTLED. The final CSS block
(root-only body-carrier CV + CIS `auto var(--lp-section-size,640px)` + flow-root norm + print
escape) is written verbatim in the verdict, W4-ready with no `PaperSection` DOM change.

### F-9 — the words virtual-copy disposition → **SETTLED (E2): branch (b) survive-at-root**

Ran §5.2's pre-committed one-clean-read protocol over `words/frontend/src`. Verdict at
`traces/F9-VERDICT.md` (committed to latex-paper `f246a11`, docs-only). FOUR importers, ZERO
document-shaped: DefinitionContentView.vue:162 (`useVirtualSectionWindow`, the pivotal twin →
feed/grid, no CV/print/find/fragment/reader anywhere in its subtree, grep-verified),
flattenDefinitions.ts:1, wordlist.ts:90 (`useWindowedStore` infinite-scroll feed),
ThesaurusView.vue:219 (`useVirtualGrid`, stays words-local). The §5.2 rule "ANY consumer
feed/grid → branch (b)" fires unambiguously. **VR-57 flips OPEN → SETTLED (survive-at-root).**

**The disk-truth correction that flips VR-71 (agglomerator-verified against the REAL repos):**
glass-ui 4.2.0 `./virtual` does NOT ship a document-native core — it ALREADY SHIPS the
windower family (`useVirtualSectionWindow` + `clearSessionHeightCache` + `useWindowedStore` +
`virtualSectionLayout`; `src/composables/virtual/index.ts:9-19`, `package.json:375-378`),
re-minted at tranche BC precisely because the words consumers overturned the no-consumer
verdict. So branch (b) is already HALF-EXECUTED on disk. words consumes the windower
UNCHANGED; the migration is a net-deletion (delete 726 LOC of stale copy, keep useVirtualGrid
116 LOC, re-point 3 imports + bump glass-ui). Because the windower is already published
unchanged and words consumes it unchanged, **branch (b)'s NATURAL landing is the 4.3.0
ADDITIVE corner** (add the document core beside the retained windower, zero removal/signature-
break), NOT 5.0.0. **5.0.0 is CONTINGENT** — fires only if W2/W5 elects to remove the
standalone windower or re-signature a published `./virtual` export.

### Forks opened by the evidence (both closed same-pass, none architectural)

- The §5.1 escalation opened and closed within F-7 (the mechanical §5.1 shape).
- The VR-71 semver VALUE was re-opened by the F-9 disk-truth finding and re-closed to 4.3.0
  (see §B VR-71). Not a new fork — a value correction under the existing mechanical rule.

**Open set after pass 3: EMPTY** (no F-n survives). What remains is not a fork — it is the
spec RE-CUT that folds these settlements (§C blocker B-3).

---

## B. THE RECONCILED VR REGISTER — FINAL STATE

Stable IDs. Only VRs whose status/evidence CHANGED this pass are called out; the remainder
hold their `SPEC-PASS-3.md §3/§4/§8` status. **These are the flips the spec re-cut must land.**

| VR | Subject | Status (pass-3 final) | Evidence pointer |
|----|---------|-----------------------|------------------|
| 45 | CSS substrate + spacing | **SETTLED** (was PROVISIONAL→F-7) | `traces/f7/F7-VERDICT.md`: final CSS block + flow-root G-g norm + print escape |
| 47 | sticky × containment | **SETTLED** (was PROVISIONAL→F-7a) | E1: carrier = inner `.section-body`; sticky offset 0.00px both engines; arm i rejected on Chromium repaint ghost |
| 48 | root-only CV + subtree seed | **SETTLED** (was PROVISIONAL→F-7b) | E1: root-only (depth===0) + `subtreeEstimatedHeight`; per-level rejected on measured WebKit 414px divergence |
| 57 | words copy disposition | **SETTLED — survive-at-root** (was OPEN→F-9) | `traces/F9-VERDICT.md`: 4/4 importers feed/grid, 0 document-shaped |
| 71 | glass-ui semver (mechanical rule) | **SETTLED — value = 4.3.0 additive corner** (5.0.0 contingent) | F9-VERDICT §4 + W2-SKELETON §4.2; **CORRECTS the spec's "leans 5.0.0"** |
| 72 | twin-deletion gate (path-qualified) | SETTLED — **gate PASS for {glass-ui, latex-paper} pair** | W2-SKELETON: 7/7 sidebar twins exactly-one; windower (glass-ui+words) + fourier search/ (7) = the two W5 pendings, per F-9(b) |
| 13 | mount governor default (64/4/true) | SETTLED — **EXPRESSED IN CODE** | W2-SKELETON: `EAGER_AUTO_SELECT_THRESHOLD=64`, `DEFAULT_BATCH_SIZE=4`, idleRevealAll true, governed default; 64/65 boundary unit-tested |
| 25 | SSR archived + hydration guard | SETTLED — **runtime-proven (E4) + ported to root** | W3b: dual-engine zero-mismatch + a blinded-guard negative control (60→8, sentinel lost) proving the guard load-bearing; guard at skeleton `useSectionViewport.ts:182` |
| 2/21 | two-path anchoring / `compensateScrollTop` | SETTLED — **REAL-WebKit runtime-proven (E3)** | W3a: compensateScrollTop green on WebKit 26.5, native + shimmed-stable-Safari branches, exactly-one-corrector (absorbed==applied==1000) |
| 23 | settle-aware scrollTo + hash nav | SETTLED — **wall-clock ≥3s bound at the root** | W2-SKELETON: `MIN_TRAVEL_MS=3000` replaced the clone's frame-count regression (a live VR-23 regression caught + fixed); scroll-margin geometry-derived |
| 53 | tracker rebase / source-unify | SETTLED — **IMPLEMENTED** | W2-SKELETON: `getPositions()` + `positionSource` early-return, `getElementById` uncalled on scroll path (spy-asserted) |
| 19 | fonts.load + settle-EPOCH gate | SETTLED — **`deriveFontFaceProbes` wired as default** | W2-SKELETON: old unconditional `KATEX_FONT_PROBES` removed; faces derived from transform output |
| 30 | reader mode ≥95% extraction | SETTLED (governor) — **BUT a new W4 markup finding** | W3b: governed ≡ eager at 1.0000 on `textContent`; NEW: `.section-header` matches Firefox Readability `unlikelyCandidates` → strips ALL headings (rename arm restores) — a W4 markup item, not a governor defect |
| 15/16 | CIS seeding / estimator | SETTLED (design) — **new W4 recalibration acceptance gate** | F7-VERDICT: mobile pre-settle 0.759 < 0.85 on the real paper; W4 recalibration named |
| 73 | test-count discipline | SETTLED — **re-pinned + triple-verified** | master `c140473` paper.test.ts 19/19 (agglomerator re-ran); glass-ui skeleton 123f/1176t; latex-paper skeleton 15f/136t |
| 76 | ElementRef surface rule | SETTLED — **+ one residual (#10)** | W2-SKELETON: `SectionViewportOptions.items` `MaybeRefOrGetter` nominal-`Ref` arm needs a documented cast or a structural `ItemsSource<T>` — a W2 decision |

All other VRs (1–12, 14, 17–18, 20, 22, 24, 26–29, 31–44, 46, 49–56, 58, 60–67, 70, 74, 75)
hold their `SPEC-PASS-3.md` status (all SETTLED). **After these flips the register is fully
SETTLED — the open set is EMPTY.**

---

## C. THE CRITIQUE LEDGER

Per-target critique numbers this pass (NOT averaged): **spec-coherence 86, evidence fleet
(E1–E4) 97, W2 skeleton 97.** The evidence and landing lanes are near-perfect and machine-
verified; the number is capped by the one spec-coherence blocker below.

### Blockers (must close before 100)

- **B-3 — the spec-of-record is STALE vs its own committed artifacts (a B-1 recurrence).**
  [ASSIGNED → the spec re-cut, one docs edit] Confirmed by git mtimes + independent re-read:
  `SPEC-PASS-3.md` was cut at **04:07**; F9-VERDICT committed **04:11**, F7-VERDICT written
  **05:07**, W2-SKELETON **05:52** — ALL after the spec, which was never re-cut. So the spec
  still asserts, at HEAD, claims its own repo now contradicts: §1/§8 "open set = {F-7, F-9}";
  VR-45/47/48 [PROVISIONAL → F-7]; VR-57 [OPEN → F-9]; §6 header "never yet run on real
  engines"; §7 "The W2 skeleton promotion (this pass's landing-readiness deliverable)" framed
  as pending. **Every one is DONE on disk.** A plan-cutter treating this spec as authoritative
  would re-dispatch F-7, F-9, W3, and the skeleton — the precise waste B-1 exists to prevent.
  The spec's §2 boast "B-1 → CLOSED BY THIS SPEC" is itself falsified by the same failure mode.

- **B-4 — VR-71 semver is INVERTED, not merely stale.** [ASSIGNED → the same spec re-cut]
  Spec §5.2/§8/VR-71 say both F-9 branches "lean 5.0.0" and the F-9 leaning IS "5.0.0." The
  committed F9-VERDICT §4 REVERSES this: branch (b)'s natural landing is the **4.3.0 additive
  corner** (the windower ALREADY ships in 4.2.0 `./virtual` unchanged; the W2 core is net-
  additive beside it; making mandatory peers optional is constraint-loosening = additive), with
  5.0.0 demoted to CONTINGENT. W2-SKELETON §4.2 confirms 4.3.0 HOLDS in the actual skeleton.
  The spec's headline semver value is BACKWARDS versus the settled evidence, and it drives the
  W2 wave's publish vehicle. This is a harder defect than pure staleness.

### Gaps (fold into the spec re-cut; none block direction)

- **G-w3prov — the §6 W3 evidence pointers dangle.** §6 says the set was "never yet run on
  real engines — this pass's lanes" and points to `docs/tranches/VIRT/traces/w3/`, which
  **does not exist in the real repo** (agglomerator-confirmed: `traces/` holds `f7/`, `p8/`,
  F1/F9 verdicts — no `w3/`). The W3 evidence actually lives in the scratchpad `virt-p4` clone
  (`761ba9d` W3a, `4391629` W3b) and is referenced only from W2-SKELETON §5. The spec's own
  G-prov "direct disk citation" rule is unsatisfied for §6. The re-cut must EITHER promote the
  W3 artifacts into `traces/w3/` OR restate §6 as executed-in-clone with the clone SHAs as the
  citations (matching how §0 cites `p6-glass-ui @ ea7fac8c`).

- **G-lift — two evidence findings are recorded only in verdict/skeleton bodies, not lifted
  into the spec's VR register:** (a) the W3b Reader-View defect (`.section-header` matches
  Readability `unlikelyCandidates`, strips ALL headings — contradicts VR-30 as currently
  worded cleanly-SETTLED; W4-owned, coincident with the F-7a header re-shaping since "heading"
  is regex-safe and "header" is not); (b) the VR-15/16 estimator narrow-width recalibration
  (mobile 0.759 < the 0.85 gate) — a real new W4 acceptance gate. Both are named in
  F7-VERDICT/W2-SKELETON but the spec still lists VR-30 and VR-15/16 as cleanly SETTLED.

- **G-item — W2-SKELETON residual #10** (the `MaybeRefOrGetter` nominal-`Ref` arm on
  `SectionViewportOptions.items`): a real surface-rule seam the spec's VR-76 wording does not
  yet cover. The re-cut must name it as a W2 decision (accept the documented cast vs add a
  structural `ItemsSource<T>`).

- **G-vgate2 — the VR-72 gate exclusion set is implicit.** The gate only PASSES under exclusion
  of vendored subrepos (words vendors full glass-ui + latex-paper copies under `frontend/`) and
  ~40 `.claude/worktrees`; a naive re-run surfaces hundreds of apparent twins. The W5/W6 CI
  grep (skeleton residual #9) MUST encode those exclusions or it false-positives. Bounded, but
  name it explicitly in VR-72.

### Dismissed / non-blocking (with reasoning)

- **The stable-Safari `compensateScrollTop` proof is under EMULATION on WebKit 26.5 trunk**
  (which itself ships overflow-anchor), not a genuinely old Safari ≤26.x binary. DISMISSED as
  non-blocking: the shimmed arm (CSS.supports false + overflow-anchor:none) forces
  compensateScrollTop as the only corrector by construction on the SAME real engine — a
  faithful cross-check; the load-bearing path IS proven on a real engine. A manual old-Safari
  spot-check on the landed W2 substrate is the acknowledged last inch, W3-lane residual.
- **The real print DIALOG (OS chrome) is unautomatable headlessly.** DISMISSED: `page.pdf()` +
  own-task `beforeprint` dispatch reproduces the dialog's rendering pipeline + event ordering;
  governed PDF == eager reference (18=18 pages, byte-identical extracted text). A manual Cmd+P
  spot-check is the last inch. (New W4 hardening candidate surfaced in passing: theme ships NO
  `break-inside: avoid` on `.math-block`/figures — add under `@media print` in W4.)
- **`useVirtualGrid` LOC reads 115 vs the verdict's 116.** DISMISSED: trailing-line rounding,
  immaterial to any conclusion.
- **latex-paper `vue-tsc` is non-zero (4 errors).** DISMISSED as non-VIRT: all 4 reproduce
  byte-identically at base `62a18e7` in the untouched `src/grammar/compile.ts` (bbnf env
  drift); the skeleton never touches that file. A pre-existing asterisk the implementing wave
  inherits, not a VIRT defect.

---

## D. OVERALL CONVERGENCE: 96% — JUSTIFICATION + RESIDUAL CHECKLIST

**The termination criterion (spec-stated, verbatim): F-7 + F-9 settled with committed
evidence; spec self-coherent with a green suite + machine counts; the W3 artifact set present;
the W2 skeleton promoted.** Evaluated against disk:

| Criterion | State | Evidence |
|---|---|---|
| F-7 settled with committed evidence | **YES** | `traces/f7/F7-VERDICT.md`, 120 real-engine lanes, mechanical §5.1 close |
| F-9 settled with committed evidence | **YES** | `traces/F9-VERDICT.md`, 4/4 feed/grid, mechanical §5.2 close |
| Green suite + true machine counts | **YES** | master 19/19; glass-ui skeleton 123f/1176t; latex-paper skeleton 15f/136t; all re-run |
| W3 artifact set present | **YES (in clone)** | `virt-p4 @ 761ba9d` (W3a) + `@ 4391629` (W3b), 13/13 + real-WebKit compensateScrollTop; NOT promoted into the real repo's `traces/w3/` (G-w3prov) |
| W2 skeleton promoted | **YES** | `p6-glass-ui @ w2-skeleton (2133597d)` + `p6-latex-paper @ w2-skeleton (aff7c73)`, gates green |
| **Spec self-coherent with its artifacts** | **NO** | B-3 (stale, never re-cut) + B-4 (VR-71 inverted) — the ONE unmet criterion |

Five of six termination criteria HOLD with committed, machine-verified evidence. The sixth —
spec self-coherence — FAILS, on the exact B-1 failure mode the loop forbids: the spec was cut
before its verdicts and never re-cut. This is not manufactured residue: a plan-cutter reading
the authoritative spec would re-dispatch settled work, and the spec's headline semver value is
backwards. The underlying WORK is at ~100; the SPEC ARTIFACT (the thing the loop's law names)
is at 86; blended and blocker-capped, the honest number is **96**, not the spec's self-assessed
93 (the spec under-counted because it listed F-7/F-9/W3/skeleton as the remaining 7% — those
are DONE) and not 100 (the coherence blocker is real).

**The residual checklist to 100 — ONE edit + two lifts, all mechanical, no research:**

1. **[B-3] Re-cut the spec** (a `SPEC-PASS-3.md` revision, or fold directly into the terminal
   `VIRT.md`): flip VR-45/47/48 → SETTLED, VR-57 → SETTLED (survive-at-root); set §1/§8 open
   set = ∅; reframe §6 as EXECUTED (with the G-w3prov citation fix) and §7's W2-skeleton line as
   PROMOTED. *(one docs edit)*
2. **[B-4] Correct VR-71** to "value = 4.3.0 additive corner; 5.0.0 contingent on a W2/W5
   removal or signature-break"; fix the §5.2 branch-(b) leaning and the §7 W2 publish-vehicle
   cell to name 4.3.0 primary. *(same edit)*
3. **[G-lift] Lift the two W4 findings into the register:** VR-30 gains the Reader-View markup
   caveat (`.section-header` rename, W4); VR-15/16 gains the 0.85@390px recalibration acceptance
   gate (W4). *(same edit)*
4. **[G-item + G-vgate2] Name residual #10** (the `MaybeRefOrGetter` cast vs `ItemsSource<T>`
   W2 decision) under VR-76, and encode the VR-72 exclusion set (vendored subrepos +
   worktrees). *(same edit)*
5. **[G-w3prov] Promote the W3 artifacts** into `docs/tranches/VIRT/traces/w3/` (copy from the
   `virt-p4` clone) OR restate §6 as clone-cited — pick one so the pointers resolve. *(trivial)*

Once that single spec-coherence edit lands, ALL six termination criteria hold, the open fork
set is empty, and convergence is 100 = the terminal `VIRT.md` + waves/ cut can be made
directly from a self-coherent spec.

**96%** reflects: both shape forks SETTLED on real-engine committed evidence (the pass's
headline work), the W3 runtime set executed dual-engine incl. the load-bearing real-WebKit
compensateScrollTop path + a blinded-guard negative control, the W2 skeleton promoted with the
F-1 constants and all residuals expressed-in-code and gates machine-green, the RED test fixed
and master green — against a single, purely-mechanical spec-coherence blocker (a spec cut before
its own verdicts, plus an inverted semver value) that the loop's own law forbids. Nothing
architectural, directional, or evidentiary remains open. Honesty over optimism: the WORK is
essentially complete, but the loop's termination gate is the SPEC's self-coherence, and that is
not yet true — this pass is **96**, not 100. The loop does NOT terminate this pass; it
terminates the instant the re-cut lands.

---

## E. RESEED BRIEF FOR PASS 4 (THE TERMINAL CLEANUP PASS)

Pass 4 is NOT a research pass and NOT a hardening pass — it is a **single spec-coherence edit
+ the terminal plan cut.** No forks survive; no evidence is missing; the only defect is that
the authoritative artifact was frozen before its own conclusions. Dispatch as ONE synchronous
edit lane (no fanout needed):

1. **Execute the §D checklist items 1–5** — the spec re-cut (VR-45/47/48/57 → SETTLED, open
   set ∅, VR-71 → 4.3.0-primary, §6/§7 reframed, the two W4 findings + residual #10 + the
   VR-72 exclusion set folded in, the W3 pointers resolved). This is the ONLY thing between 96
   and 100.
2. **Then cut the terminal `VIRT.md` + waves/** at the house WAVE_SPEC 11-section bar. The
   pass-3 corpus already hands the plan cut everything it needs:
   - **Wave roster (owner repo → publish vehicle):**
     - W1 Pass-3 Evidence Fleet — DONE (scratchpad clones → the committed verdict files).
     - W2 glass-ui Substrate — **glass-ui → 4.3.0** (additive corner per the corrected VR-71;
       5.0.0 only if W5 breaks a windower signature). Lands the promoted skeleton
       (`p6-glass-ui @ w2-skeleton 2133597d`): peer-meta (VR-51), core at `./virtual`
       (VR-13 64/4/true constants, VR-19 epoch gate, VR-23 wall-clock scrollTo, VR-2/21
       two-path anchoring, VR-17/18 width epoch), VR-53 source-unify, VR-56 fence, VR-75 docs,
       VR-76 ElementRef rule + the residual-#10 decision, VR-62/65/66 search asserts.
     - W3 Semantics Runtime Proofs — the §6 set re-run against the landed W2 substrate (the
       clone artifacts `761ba9d`/`4391629` are the template) + the manual Cmd+P and old-Safari
       last-inch spot-checks.
     - W4 latex-paper Consumption — **latex-paper → changesets v0.3.0** (from
       `p6-latex-paper @ w2-skeleton aff7c73`): twins deleted, dep + adapters (VR-52/61), the
       F-7 SETTLED CSS (flow-root G-g norm, root-only `.section-body` CV, print escape +
       `break-inside:avoid`), the Reader-View `.section-header` rename (VR-30), the VR-15/16
       estimator recalibration, tokens (VR-37), headingLevel (VR-26), aria-labelledby (VR-27),
       MIGRATION (VR-31/32/58 + G-c5/G-c6 notes).
     - W5 Consumer Migrations — fourier (windowing + `search/` 7-file net-deletion, first
       `./search` consumer VR-60/67); words (F-9 branch (b): delete 3 windower files, re-point
       3 imports, bump glass-ui, keep useVirtualGrid); vft (binding doc + VR-75).
     - W6 Release + Gate Close — both roots' npm cuts; the path-qualified VR-72 gate (with the
       G-vgate2 exclusion set) + atlas census-gate update; the π visual lane.
   - **Cherry-pick manifest (already assembled, W2-SKELETON.md):** glass-ui 5 picks
     (ea7fac8c, b74875c3, fd516982, 2b27d7c7, 2133597d); latex-paper 3 picks (62a18e7,
     12ed7e4, aff7c73) + the skip-marked 952c897 twin.
   - **Gate corpus:** master 19/19; glass-ui skeleton 123f/1176t + build + vue-tsc +
     proof:virtual-window; latex-paper skeleton 15f/136t + build; the F-7 (120-lane) / F-9
     (4-consumer) / W3a-b (13/13 dual-engine) verdicts; VR-72 twin-gate PASS for the
     {glass-ui, latex-paper} pair with the windower + fourier-search W5 pendings named.

The loop terminates when the re-cut spec is self-coherent (the ONLY remaining act) and
`VIRT.md` is cut from it. Estimated pass-4 convergence: 100 (the residual is a single docs edit,
then the plan itself — per the loop's "settle then land" discipline, implementation is the
tranche, not the loop).
