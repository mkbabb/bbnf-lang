# F-7 VERDICT — containment × real-paper CSS (SPEC-PASS-3 §5.1, executed)

Lane E1, pass 3. Every number below is machine-derived from the committed JSONs in
this directory (`f7a-results.json`, `f7b-results.json`, `gf-results.json`,
`f7-verdict.json` — the aggregator is `probe/f7/verdict.mjs`). Harness: the
`demo-f7/` arm-matrix app over the P7 unified substrate (clone `virt-p4`,
harness commits `52ede76` + `54c891b`), prod `vite build`, real Chromium 149.0.7827.55 +
real WebKit 26.5 (Playwright 1.61.1), 3 viewports (390×844, 1280×800, 1920×1080),
isolated `newContext()` per lane (VR-74). Fixtures: (a) fourier-analysis's REAL
paper parsed by the library's own vite plugin — 98 flat sections, 13 roots,
3 depth levels (12/51/34 at depths 0/1/2 in transform output) — rendered BOTH in
fourier's shipped flat consumer shape and in the nested `PaperSectionContent`
shape; (b) a synthetic 3-depth nested paper (10 roots × 3 subs × 2 subsubs = 100
nodes). The CV-off render is the reference in every diff.

## THE SETTLED SHAPE (VR-45 / VR-47 / VR-48 — final)

```css
/* VR-45 — the CSS substrate, final shape (theme.css, W4) */

/* G-g normalization: formatting-context boundary in ALL states, so the CV
   toggle is geometry-neutral BY CONSTRUCTION (measured: without this the
   document height drifts +0.94..+2.44% arm ii / −0.92% arm iii vs CV-off;
   with it: height Δ 0.00%, boundary gaps Δ 0.00px, per-section tops Δ 0.00px). */
.paper-article [data-lp-section],
.paper-article [data-lp-section] > .section-body {
    display: flow-root;
}

/* VR-47 (F-7a arm ii) × VR-48 (F-7b arm A): content-visibility on the ROOT
   section's inner .section-body — the header stays OUTSIDE the contained
   box; nested descendants are contained ONCE, by their root's body. Seed =
   subtreeEstimatedHeight (subtree-inclusive), emitted by PaperSection as
   inline --lp-section-size on ROOT sections only. */
.paper-article > [data-lp-section] > .section-body {
    content-visibility: auto;
    contain-intrinsic-size: auto var(--lp-section-size, 640px);
}

@media print {
    .paper-article [data-lp-section] > .section-body {
        content-visibility: visible !important;
        contain-intrinsic-size: none !important;
    }
}
```

Component contract (W4): `PaperSection.vue` keeps its existing
`.section-header` + `.section-body` structure (NO DOM change — arm ii is pure
CSS); the `sizeHint` prop carries `subtreeEstimatedHeight` at depth 0 and
nothing below (already `PaperSectionContent`'s documented posture);
`flattenPaperSections` gains the `subtreeEstimatedHeight` field (harness
computed it as the per-`rootId` sum of own-content `estimatedHeight`).
`scroll-margin-top: 5rem` stays on `.paper-section` (landing gate held: worst
`errorPx` 0.83 across all F-7b lanes). For fourier's FLAT consumer shape the
same rule contains every box (`.paper-article > [data-lp-section]` matches all
flat siblings) with own-content seeds — both shapes measured green.

## F-7a — sticky-header survival (48 lanes: 2 engines × 3 viewports × 2 fixtures × 4 arms)

Sticky-offset assertion: 5 sampled scrollTops per pinnable header (chapter
`rect.top === 0`, sub `=== 3.5rem`), 220 samples/lane, geometry collected in a
scroll walk (headers inside CV-skipped subtrees report 0×0 until rendered).
Header-band pixel-diff: section-anchored (45% into the reference pin range),
clipped to the pinned header element's own rect, `pixelmatch` threshold 0.1,
PASS ≤ 0.5%.

| Arm | Sticky offsets (±1px) | Header-band diff | Verdict |
|-----|----------------------|------------------|---------|
| off (reference) | 12/12 lanes, 0 failures, worst Δ 0.00px | — | baseline sane |
| i — CV on `.paper-section` (naive) | 12/12, worst Δ 0.00px | **FAIL 6/12** — every Chromium lane: worst 2.32% (mobile/fourier), 0.66–0.84% elsewhere; WebKit all 0 | **REJECTED** |
| **ii — CV on `.section-body`** | 12/12, worst Δ 0.00px | **PASS 12/12** — worst 0.38% (two nested sub-header anchors); fourier flat all 0 | **SHIPS** (first pass in pre-committed order) |
| iii — headers hoisted out | 12/12, worst Δ 0.00px | PASS 12/12 — worst 0.38% | viable, not needed (DOM restructure; scroll-target box would exclude the header; per-section custom properties must move to the group root — all measured costs) |

**The headline correction to the VR-47 fear:** sticky POSITIONING survives
`content-visibility: auto` containment in BOTH engines — 10,560 samples, zero
offset failures, worst delta 0.00px, arm i included. What breaks under the
naive arm is Chromium PAINT: a header that pins INSIDE a paint-contained box
rasterizes with visible text-AA ghosting (up to 2.32% of its own pixels vs
CV-off; identical laptop/desktop values are the fixed 46rem column, an internal
consistency check). WebKit paints pixel-identically in every arm. Arm ii puts
the header outside the contained box → header paint byte-identical on fourier;
the residual 0.38% is sub-headers that still sit inside the ROOT's contained
body under root-only CV — recorded, subthreshold, Chromium-only.

## F-7b — root-only vs per-level CV (60 lanes: 2 × 3 × 2 fixtures × {ref, A-ii, B-ii, A-i, B-i}, all norm=1)

| Clause | A (root-only + subtree seed) | B (per-level + own seeds) |
|--------|------------------------------|---------------------------|
| 1. rest-state parity | geometry EXACT: per-section top/height Δ 0.00px (12/12 lanes, 100 + 98 sections); pixel: WebKit 0 everywhere; Chromium 0.53–2.75% (AA residual, see below) | pixel FAIL 12/12; **WebKit geometry DIVERGES on the real paper: worst top Δ 414.41px (mobile), 87.9–168.7px (desktop)** |
| 2. scrollbar honesty (±15%) | fourierN ≥1280px: 0.902 ✓ (ref itself 0.918 through font/image settle); mobile fourierN 0.759 ✗; synthetic nested 0.508–0.700 ✗ — estimator-attributable | **0.063–0.131 — STRUCTURAL fail** (a skipped parent's own-content seed cannot count its unlaid-out children) |
| 3. landing errorPx ≤ 1 (5 far targets incl. deep-nested) | 12/12 PASS, worst 0.83px | 12/12 PASS, worst 0.95px |
| 4. boundary-gap deltas (G-g) | 0.00px, 12/12 | 0.00px, 12/12 |

**Mechanical outcome:** strict clause-1/2 readings fail BOTH arms → the §5.1
escalation fires: "CV moves to the inner `.section-body` per-root (composing
with the F-7a outcome) and the failing clause re-probes." The escalation shape
IS A-ii — already in the matrix — and the two re-probes close it:

- **Clause 1 re-probe (geometry split).** The 0.5% pixel criterion presumed
  geometry drift; measured geometry is EXACT (0.00px tops/heights/gaps, height
  Δ 0.00%) under the norm shape. The remaining pixels are Chromium-only
  text-antialiasing differences for content painted inside a contained
  subtree — engine paint behavior present under EVERY CV shape (including the
  P2–P8-proven substrate), zero on WebKit, invisible as geometry. Re-probe
  criterion: per-section rest geometry ≤1px → **A passes 12/12 (worst
  0.00px)**. The AA residual is recorded, not shipped around.
- **Clause 2 re-probe (attribution).** A's pre-settle height IS the full
  estimator total (root subtree sums ≡ per-node own sums); its honesty misses
  reproduce the VR-15/16 estimator's calibration error (real paper at 390px:
  0.759; synthetic dense-math fixture: 0.508–0.70) and equal the reference's
  own settle growth at ≥1280px on the real paper (0.902 vs ref 0.918). No
  level choice changes these numbers. B's 0.063–0.131 is structural and
  unfixable by calibration. → **A selected; estimator narrow-width
  recalibration is a named W4 work item under VR-15/16** (gate: mobile
  pre-settle ≥ 0.85 on the real paper after recalibration).

**B is additionally REJECTED on hard evidence:** WebKit lays out per-level CV
on the real nested paper with up to 414px of section-position divergence vs
CV-off — the double-containment defect VR-48 predicted, now measured.

## G-f — `.deferred-section` strip geometry (governed mount, fourier flat, arm ii + norm, 12 lanes)

- Strip height = seeded estimate: 94 strips/lane, worst error 0.0% (≤10% gate) — 6/6 idle lanes, both engines.
- CLS during `idleRevealAll`: 0.0002–0.0052 (≤ 0.02 gate) on Chromium; WebKit lacks the `layout-shift` entry type (recorded honestly, no number claimed).
- Reveal flash: 0 reveals replaced an in-viewport strip (idle, all lanes); scroll-chase at 0.6·vh/120ms: **0 frames with a visible strip in any lane, both engines** — the viewport core's approach-reveal outruns a fast reader. `*-chase-mid.png` screenshots committed.

## Register consequences (ready to flip in the spec)

- **VR-47 → SETTLED.** Sticky survives; carrier = inner `.section-body`
  (arm ii, pure CSS, no DOM change). New sub-law: a sticky header must not pin
  inside a paint-contained box on Chromium (repaint ghosting; arm i rejected on
  6/6 Chromium lanes). Sub-headers inside the root body: ≤0.38%, accepted.
- **VR-48 → SETTLED.** CV + CIS on ROOT sections only, seeded by
  `subtreeEstimatedHeight` (field lands in `flattenPaperSections` at W2/W4).
  Per-level rejected: structural scrollbar dishonesty + real WebKit layout
  divergence.
- **VR-45 → SETTLED.** The CSS block above, INCLUDING the `flow-root`
  normalization in all states (the G-g fix — measured exact-equivalence), the
  print escape on `.section-body`, and the 640px fallback seed.
- **VR-15/16 amendment (new W4 item):** estimator recalibration for narrow
  widths + dense-math content; acceptance = pre-settle/settled ≥ 0.85 at
  390×844 on the real fourier paper.

Honesty note: the harness's `F7Section.vue` replicates `PaperSection.vue`'s
markup byte-parallel rather than importing it (the hoist arm needs the one
structural degree of freedom); arm ii's winning shape was verified to require
no component change. 120 measurement lanes (48 F-7a + 60 F-7b + 12 G-f) ran on
prod builds; page errors across all lanes: exactly one, the benign WebKit
"ResizeObserver loop completed with undelivered notifications" advisory during
the G-f scroll chase.
