# VIRT — SOTA Virtualization + Cross-Repo Abstraction — TRANCHE PLAN

Status: **CUT (terminal deliverable of the convergence loop, pass 4).** Cut from the
self-coherent spec-of-record `SPEC-PASS-3.md` (RE-CUT, PASS 4; open fork set EMPTY; all
six termination criteria hold — `PASS-3-SYNTHESIS.md §D` + `SPEC-PASS-3.md §9`). Every
directional and shape decision herein is SETTLED with committed evidence; this plan
consumes those settlements, it does not re-open them. Implementation is the tranche.

Tranche letter: **VIRT** (house form `VIRT.W<N>.<x>`). Docs root:
`docs/tranches/VIRT/`. `PROGRESS.md` opens at the first W2 dispatch; `FINAL.md` closes
at W6.

## Thesis

ONE headless document-virtualization + scroll-tracking core, brought to SOTA
(TanStack-Virtual-class, the ten VR-1..11 properties simultaneously), lands ONCE at its
owning root — `@mkbabb/glass-ui` `./virtual` (decision D-A, extraction-proven) — as a
**document-native substrate**: all sections mounted, `content-visibility: auto` +
`contain-intrinsic-size` defer layout/paint, a slim viewport core supplies activeId /
deep links / settle hygiene / print / PRM (VR-12). `@mkbabb/latex-paper` consumes it
and keeps only paper semantics; fourier, words, and vft migrate; every losing twin is
DELETED, never shimmed. The loop settled the shape on real-engine evidence: F-1
governed/64/4/true, F-3 → the VR-75 timeline-host law, F-5 offsetTop-primary, F-7 arm
A-ii (root-only CV on the inner `.section-body`), F-9 survive-at-root → VR-71 = 4.3.0
additive-corner PRIMARY. Nothing remains to research; everything remains to land.

## Goal criterion

The tranche succeeds if, at close, the portfolio runs ONE virtualization core: glass-ui
4.3.0 published with the document-native core beside the retained windower; latex-paper
0.3.0 published consuming it (twins deleted, F-7 CSS shipped, `/vue` floor held);
fourier and words running on the published cuts with their copies deleted; the vft V4
fence drained; and the VR-72 twin gate PASS across all four repos — with every gate
count machine-derived and every runtime claim carried by a committed artifact.

## Completion criterion (the artefact checklist)

All of the following exist on disk / in CI at close (each is a wave hard-gate item;
IDs cite `SPEC-PASS-3.md`):

1. glass-ui `4.3.0` tag + npm publish green; additivity proof vs 4.2.0 committed
   (VR-71: zero `./virtual` export removed or signature-broken).
2. glass-ui suite at the cut: `Test Files 123 passed` / `Tests 1176 passed` + build +
   `vue-tsc --noEmit` + `proof:virtual-window` exit 0 (VR-73; skeleton pin held).
3. The W3 runtime-gate suite re-run GREEN against the LANDED substrate, dual-engine,
   promoted to CI; artifacts committed under `docs/tranches/VIRT/traces/w3-landed/`.
4. latex-paper `v0.3.0` via changesets CI; suite `15 files / 136 tests` with the
   sibling fixture (`127 | 9 skipped` without — both honesty forms recorded); master
   golden `paper.test.ts` 19/19 (the `952c897`-fixed block) still green.
5. The twin-deletion diffs committed: latex-paper −1,582 LOC (windowing seam +
   `tracking/` + `useSidebarFollow`), words −726 LOC (3 windower files), fourier
   `search/` −7 files.
6. VR-72 final gate output committed: every twin filename path-qualified exactly-one
   across {glass-ui, latex-paper, fourier-analysis, words}, with the G-vgate2
   exclusion set encoded; the CI grep landed.
7. MIGRATION notes complete at both roots (VR-31/32/58 + G-c5-narrowed/G-c6); release
   notes cut.
8. The π visual lane artifacts (≥3 viewports, PRM on/off, sticky-header pixel check
   per the F-7a verdict) + the before/after frame-budget ledger incl. WebKit.
9. The vft binding doc landed beside `LATEX-PAPER-TOC.md`; the `latex-paper-virt`
   fence drain notice appended to the V-arc `V-EXEC-PROGRESS.md`.
10. `FINAL.md` + `PROGRESS.md` reconciled; zero deferrals outside this plan's ledger.

## Invariants and laws (binding on every wave)

- **Root-repo law.** Every fix lands at the owning repo's root and ships via that
  repo's publish vehicle — glass-ui by semver cut, latex-paper by changesets CI,
  consumers by their own commits. Nothing is consumer-patched. Every wave State block
  names owner + vehicle.
- **Twin-deletion gate (VR-72).** Losing twins are DELETED, never shimmed or
  deprecated-in-place. The gate grep is path-qualified and carries the G-vgate2
  exclusion set (`node_modules`/`dist` + words' vendored glass-ui/latex-paper copies
  under `frontend/` + the ~40 `.claude/worktrees` checkouts) — a naive run
  false-positives hundreds of twins.
- **No-legacy.** No compatibility shims, no `@deprecated` corpses, no dual code paths.
  `^0.2.1` does not admit 0.3.0 (VR-31) — consumers opt in explicitly; the `/vue`
  re-exports are CONSUMPTION of glass-ui, not a shim (VR-32).
- **Machine-count discipline (VR-73).** Acceptance criteria cite ONLY machine-derived
  counts, reporter lines committed verbatim beside the claim. Current pins: latex-paper
  master `952c897` 14f/127t (golden 19/19 at `c140473`); glass-ui skeleton 123f/1176t;
  latex-paper skeleton 15f/136t (reconciliation in `W2-SKELETON.md §2`).
- **Semver honesty (VR-71, mechanical).** glass-ui cut = 4.3.0 while the surface stays
  purely additive vs 4.2.0; 5.0.0 fires ONLY if a wave removes or signature-breaks a
  published `./virtual` export. No wave in this plan does; any scope reveal that would
  is a triumvirate trigger, not a quiet major.
- **Fleet hygiene (VR-74).** Prototype lanes reuse the scratchpad clones; isolated
  `newContext()` per Playwright lane; nothing travels clone → real repo except by the
  reviewed cherry-pick manifest.
- **Concurrency law.** ≤3 concurrent agent lanes at any moment, tranche-wide.
- **Standing surface rules.** VR-75 (timeline-host law) and VR-76 (structural
  `ElementRef`; nominal `Ref` never crosses the package boundary) bind all new code.

## Wave roster

| Wave | Name | Owner repo → vehicle | Goal (one line) | Status |
|------|------|----------------------|-----------------|--------|
| VIRT.W1 | EVIDENCE | scratchpad clones → committed verdicts (docs-only) | The loop's evidence corpus: every fork settled, every artifact on disk | **complete** |
| VIRT.W2 | CORE | glass-ui → semver minor **4.3.0** | The promoted skeleton lands at the root; peer-meta optional; 4.3.0 published | planned |
| VIRT.W3 | RUNTIME GATES | glass-ui CI + `traces/w3-landed/` docs → CI job (no npm cut) | The §6 runtime set re-proven on the LANDED core; promoted to CI | planned |
| VIRT.W4 | PAPER | latex-paper → changesets **v0.3.0** | Twins deleted; glass-ui runtime dep; F-7 CSS + W4 findings shipped; 0.3.0 cut | planned |
| VIRT.W5 | CONSUMERS | fourier-analysis, words, sci-report (vft docs + census gate) → per-repo commits | All three consumers on the published cuts; copies deleted | planned |
| VIRT.W6 | CLOSE | both roots + tranche docs → release notes, gate outputs, FINAL.md, fence drain | Portfolio-wide VR-72 PASS; π lane; the V-arc fence drained | planned |

Dependency spine (migration order per VR-70): W2 → {W3 ∥ W4} → W5 → W6. W3 and W4 may
run concurrently (disjoint repos) under the ≤3-lane law; W5's fourier/vft units need
W4's published 0.3.0, its words unit needs only W2's 4.3.0, and all of W5 waits for W3
green (runtime confidence before consumer rollout).

## Fences and coordination

- **The vft V4 fence (`latex-paper-virt`).** The sci-report V-arc holds vft V4 HALTED
  on this tranche: fence `latex-paper-virt` in
  `/Users/mkbabb/Programming/sci-report/atlas/docs/tranches/V/exec/V-EXEC-PROGRESS.md`
  §1 — "`@mkbabb/latex-paper` publishes the SOTA window-virtualization cut from its own
  root." **W4's npm cut drains it; W6 posts the drain notice** (the V-arc's revival
  tick re-tests the fence — the notice makes the drain discoverable, it does not run
  V4). If vft V4 begins while W4/W5 are in flight, `docs/tranches/VIRT/coordination/`
  opens per the spec §7 note; until then no coordination doc is needed.
- **Consumer repos.** fourier-analysis and words are written ONLY in W5, each at its
  own root under its own commit discipline. words' vendored `frontend/` copies of
  glass-ui/latex-paper are NOT touched (they are the G-vgate2 exclusion, not twins).
- **The atlas census gate.** `j0-absorb-leaf-census` (asserts `@mkbabb/glass-ui/virtual`
  has zero consumers) is updated in W5, the same wave the first consumer lands (VR-72).

## The cherry-pick manifest

`W2-SKELETON.md` is the binding manifest — the proven clone commits promoted onto ONE
branch per side, gates green at tip:

- **glass-ui:** `p6-glass-ui @ w2-skeleton (2133597d)` — 5 picks IN ORDER: `ea7fac8c`
  (P6 extraction + peer-meta), `b74875c3` (P7 navigation/settle, `MIN_TRAVEL_MS=3000`),
  `fd516982` (F-1 constants + hydration guard), `2b27d7c7` (VR-53 source-unify),
  `2133597d` (VR-56/75/76 rules). Diffstat 17 files, +2623/−72.
- **latex-paper:** `p6-latex-paper @ w2-skeleton (aff7c73)` — 3 picks: `62a18e7` (P6
  consumption, twins deleted), `12ed7e4` (paper substrate consumption), `aff7c73` (the
  F-7 SETTLED CSS) — **plus the skip-marked `1616c6b`** (clone twin of `952c897`,
  already on real master; SKIP when landing). Diffstat 27 files, +916/−1393 (net −477).

Clone roots, per-pick content, diffstats, and residuals #1–#10:`W2-SKELETON.md §1–§4`.

## Gate corpus (the evidence this plan stands on — all committed)

- **Suites (VR-73 pins):** latex-paper master golden `paper.test.ts` **19/19** at
  `c140473` incl. the `952c897`-fixed golden; master full suite 14f/127t; glass-ui
  skeleton **123f/1176t** + build + vue-tsc + `proof:virtual-window` exit 0;
  latex-paper skeleton **15f/136t** (`127 | 9 skipped` without the sibling symlink).
- **F-7 verdict** (`traces/f7/F7-VERDICT.md`, 120 real-engine lanes): arm A-ii ships —
  sticky offsets Δ 0.00px both engines; arm i rejected (Chromium repaint ghosting
  0.66–2.32%); arm B hard-rejected (scrollbar dishonesty 0.063–0.131 + WebKit 414px
  divergence); G-f PASS (CLS ≤0.0052, zero in-viewport reveals).
- **F-9 verdict** (`traces/F9-VERDICT.md`): 4/4 words importers feed/grid-shaped, zero
  document-shaped → survive-at-root; the disk-truth correction pinning VR-71 = 4.3.0.
- **W3 evidence** (`traces/w3/`, byte-exact from `virt-p4 @ 761ba9d`/`4391629`):
  `compensateScrollTop` green on REAL WebKit 26.5 (jumps ≤0.25px, absorbed/applied
  1000/1000); governed PDF ≡ eager (18=18 pp, byte-identical text); hydration guard
  proven load-bearing by the blinded-guard NEG control (60→8); axe zero
  critical/serious; Readability governed ≡ eager 1.0000.
- **VR-72 pair-PASS** (`W2-SKELETON.md §3`): 7/7 sidebar twins exactly-one at glass-ui;
  the two pendings are exactly the named W5 migrations (windower: glass-ui+words;
  fourier `search/`).

## Zero-deferral ledger

Every known residual is wave-assigned here; nothing defers past W6. (Sources:
`W2-SKELETON.md §4` residuals #1–#10; the spec's §6 last-inch items.)

| # | Item | Wave |
|---|------|------|
| 1 | Cherry-pick landing, order + rebase-verify (residual #1) | W2 |
| 2 | glass-ui 4.3.0 cut per VR-71 (residual #2) | W2 |
| 3 | `MaybeRefOrGetter` cast vs structural `ItemsSource<T>` — the VR-76 residual-#10 decision | W2 |
| 4 | VR-62/65/66 search gate-asserts (residual #8) | W2 |
| 5 | W3 re-run vs the landed substrate + §6.5 find-over-CV WebKit re-confirm + §6.6 F-5 spot-checks | W3 |
| 6 | Manual Cmd+P dialog + old-Safari `compensateScrollTop` spot-checks (the two last-inch items) | W3 |
| 7 | latex-paper v0.3.0 changeset + MIGRATION (residual #3) | W4 |
| 8 | Reader-View `.section-header` rename (residual #7, VR-30 W4 finding) | W4 |
| 9 | VR-15/16 estimator recalibration, gate ≥0.85 @ 390×844 (residual #6; current 0.759) | W4 |
| 10 | Consumer wiring: `sizeHint`/`headingLevel` + VR-53 `positionSource` (residuals #4/#5) | W4 (latex-paper-side) + W5 (fourier) |
| 11 | words windower deletion + re-point + bump (residual #9a; F9-VERDICT §3) | W5 |
| 12 | fourier `search/` deletion + first `./search` consumer (residual #9b; VR-60/67) | W5 |
| 13 | atlas `j0-absorb-leaf-census` gate update (VR-72) | W5 |
| 14 | Path-qualified VR-72 grep in CI with G-vgate2 exclusions (residual #9c) | W6 |
| 15 | π visual lane + frame-budget ledger; release notes; FINAL.md; fence drain notice | W6 |

Pre-existing asterisk, NOT VIRT's (recorded, no wave): the 4 `src/grammar/compile.ts`
vue-tsc errors reproduce byte-identically at base `62a18e7` (bbnf env drift) — the
implementing waves inherit them and must add zero NEW errors.

## Convergence provenance

Four passes, one loop, terminated at 100 when the pass-4 re-cut landed:

- **Pass 1** — the SOTA definition + architecture space; `SPEC-PASS-1.md` +
  `PASS-1-SYNTHESIS.md`. Scope expanded mid-pass by the owner directive recorded in
  `SCOPE-GLASS-UI.md` (the glass-ui twin; the abstraction fork became first-class).
- **Pass 2** — D-A settled (glass-ui owns), the P6 two-clone extraction proof, VR-75/76
  minted; `SPEC-PASS-2.md` + `PASS-2-SYNTHESIS.md`.
- **Pass 3** — the hardening + landing-readiness pass: F-7/F-9 settled on real-engine
  committed evidence, the W3 runtime set executed dual-engine, the W2 skeleton
  promoted, the RED test fixed (`952c897`); `SPEC-PASS-3.md` + `PASS-3-SYNTHESIS.md`
  (96%, capped by the B-3/B-4 spec-coherence blocker).
- **Pass 4** — the terminal coherence edit + this plan: `SPEC-PASS-3.md` re-cut in
  place (§9 is the pass-4 assessment; all six termination criteria hold), `traces/w3/`
  promoted, and `VIRT.md` + `waves/` cut from the self-coherent spec.

Wave specs: `waves/W1.md` … `waves/W6.md` (each at the WAVE_SPEC 11-section bar).
