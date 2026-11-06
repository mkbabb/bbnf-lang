# W3B — HYDRATION + A11Y runtime-gate evidence (§6.3 + §6.4, pass-3 lane E4)

Verdict: **ALL GREEN — 13/13 clauses PASS** (machine-recomputed by
`verdict.mjs` from `W3B-REPORT.json`; exit 0). Engines: **Chromium
149.0.7827.55, WebKit 26.5, Firefox 151.0** — every scenario in its own
isolated `newContext()` (VR-74). Fixture: the W3A 60-section paper (numbered
display math every third section) wrapped in the real consumer shell — a
`<nav>` TOC of in-document links beside `.paper-article`; ONE app definition
shared by the node SSR pass and the browser bundle (`app.mjs`).

## A. Hydration — the VR-25 damage guard on real SSR (§6.3)

Real `renderToString` under a governed mount emits the FULL document
(`ssrHtmlSections: 60` — the isSSR posture). A 25ms section-count sampler is
armed via `addInitScript` BEFORE any page script, so transient truncation
during hydration cannot hide.

| Arm | Engine | Result |
|---|---|---|
| hydrate-governed | Chromium + WebKit | Guard fires (ONE `client-render-only` warn, forces eager); **zero hydration mismatches; sampler minimum 60/60 — the DOM never dipped below the SSR count**; sentinel present in `textContent`. |
| hydrate-damage (NEGATIVE control) | Chromium | Server mutilates only `id="sec-0"` so the guard's `getElementById` probe misses → governed hydration proceeds → **60 SSR sections truncate to 8** (sampler floor 4 = batchSize), sentinel GONE, Vue emits "Hydration children mismatch … The DOM will not be rectified in production". The pass-1 P4 silent-reconcile hazard is REAL and the guard is load-bearing. |
| governed-idle | Chromium + WebKit | Client-only shipped default: 8 mounted at ready (governed) → **60/60 + sentinel after `idleRevealAll`** — full content post-idle. |

The guard heuristic's known false-positive direction (G-c6) is unchanged:
safe, never truncates.

## B. A11y — axe, keyboard, PRM, reader (§6.4)

- **axe-core 4.12.1** on the settled governed view: **zero critical, zero
  serious** on BOTH Chromium and WebKit (`w3b-axe-report.json`). Remaining:
  one moderate `page-has-heading-one` (fixture-level — the probe shell has no
  h1 title block; real papers do) and `color-contrast` *incomplete* on 170
  KaTeX nodes (unstyled math — the probe page ships no katex.css; not a
  substrate defect).
- **Keyboard walk** (governed-hold, frontier stays partial): Tab order walks
  the TOC in document order (sec-0..sec-5); Enter on the **sec-50 link with
  only 8 sections mounted** → reveal + settle-aware landing (errorPx 0.53,
  behavior `instant` — long-jump demotion) + **focus arrives on the section
  root** (`tabindex="-1"` skip-nav pattern; visible focus ring in
  `w3b-keyboard-after-enter.png`); focus RETAINED across `revealTo(sec-57)`
  and `revealAll()` (60/60 mounted, activeElement still sec-50); next Tab
  continues the ring — **focus never lands on body at any step** (VR-29:
  monotone reveal never destroys focus).
- **PRM** (`reducedMotion:'reduce'` contexts, near targets below the
  long-jump threshold): explicit `behavior:'smooth'` AND the no-request
  adapter default BOTH issue `instant` with **0 distinct interpolation
  positions** (Chromium; WebKit 0–1). The no-preference control issues
  `smooth` with **91/82-position interpolation trains** — the sampler
  detects real smooth scrolls, so the zeros are load-bearing. Control runs
  post-`revealAll` on a settled document: with the frontier still advancing,
  reveal-driven remeasure lets the settle loop's final commit jump-cut the
  animation within ~6 frames (observed) and starve the sampler — recorded as
  method, not defect (the keyboard scenario covers reveal+scroll).
- **Reader mode, REAL Firefox 151**: `isProbablyReaderable` true; Readability
  (the Reader View parser) parses the governed post-idle document at
  **extraction ratio 1.0000 vs the eager reference** (14071/14071 chars),
  sentinel present — governed ≡ eager, VR-30's ≥95% gate met with margin.

## FINDING (owner: latex-paper, W4 — markup, NOT a governor defect)

**Reader View strips every section heading from the shipped markup.** The
`.section-header` class matches Readability's `unlikelyCandidates`
(`/…|header|…/i`, `Readability.js:141`) and contains no
`okMaybeItsACandidate` token → all 60 headings are removed from the article
output in BOTH eager and governed arms (identical — no governor involvement).
Mechanism pinned by the rename arm: re-classing the same nodes to
`lp-heading-row` on a clone restores **60 missing → 0 missing**
(`headingStripFinding.mechanismConfirmed: true`). W4 should rename the class
(anything avoiding the regex tokens; "heading" is safe, "header" is not) —
naturally coincident with the F-7a re-shaping of the same box.

Benign, known: one WebKit `ResizeObserver loop completed with undelivered
notifications` pageerror in the PRM-reduce arm — the same advisory already
named in the F7 verdict; no assertion rides on it.

## Artifacts

`probe/w3b/`: `W3B-REPORT.json` (full machine report), `w3b-axe-report.json`,
`w3b-keyboard-after-enter.png`, `verdict.mjs` (pass-table recomputation,
exit-code gated), `run.mjs` + `app.mjs` + `client.mjs` + `client.bundle.js`
(the harness). Repro: `npx esbuild probe/w3b/client.mjs --bundle --format=esm
--define:process.env.NODE_ENV='"development"' --outfile=probe/w3b/client.bundle.js
&& node probe/w3b/run.mjs && node probe/w3b/verdict.mjs`.
