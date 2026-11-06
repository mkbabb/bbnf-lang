# F-1 VERDICT — the mount default, closed (P5, 2026-07-02)

Protocol: SPEC-PASS-2 §4, executed verbatim. Environment: prod build, Chromium
(Playwright 1.61.1), CDP 4x CPU throttle on EVERY cell, isolated `newContext()` per run
(VR-74), 3 runs/cell (median [min–max]). Method: `METHOD-P5.md`. Raw traces:
`f1-results.json` (incl. per-run longtask lists), `f1-smalln.json`, `f1-verdict.json`.

## The decision rule, applied (R1..R5 — no re-litigation)

- **R1 — windowed baseline RETIRED.** Holds regardless of numbers (fails VR-4..9
  structurally). It appears below as the scroll reference only.
- **R2 — the shipped default is `mount: 'governed'`.** Confirmed by every cell below.
- **R3 — S_cross and the eager auto-selection constant.**
  - LITERAL reading (worst longtask incl. the demo's fused eval+mount task):
    **S_cross = 32** → `items.length ≤ 25 → 'eager'`. The protocol family {40..220}
    contains NO passing N (N=40 median 219ms > 200); a supplementary {8,16,24,32} sweep
    located the crossing.
  - MOUNT-ATTRIBUTABLE reading (worst − the 147ms windowed-arm eval baseline; same
    bundle, ~3-section mount): **S_cross = 80** → threshold **64**. This is the
    transferable number for consumers whose bundle eval and mount run in separate tasks
    (fourier: router + code-split `vendor-paper` chunk).
  - **The shipped rule per the pre-committed text is the literal one: `S_cross = 32`,
    constant `floor(0.8×32) = 25`**, cited to `f1-results.json` + `f1-smalln.json`.
    Synthesis may swap in the decomposed 64 with a one-line rule amendment; both
    constants land here with their traces. Either way the real consumer (98 sections)
    is governed — the fork's consumer-facing outcome is identical.
- **R4 — governor PASS/FAIL on the 220 fixture at 4x: PASS on all three clauses.**
  - (i) EVERY longtask < 200ms including idleRevealAll batches: worst across all
    governed runs/batches = **133ms** (the eval+initial-4-sections task); every idle
    batch patch ≤ 30ms. PASS.
  - (ii) scroll frame stats vs the windowed reference: avg **+0.2%** (8.42 vs 8.40ms
    median over 20s full-document sweeps, ~2,380 frames/run), **0 frames > 33ms** in
    all 9 sweep runs (windowed/eager/governed). PASS.
  - (iii) far-target `revealTo`+`scrollTo`: errorPx **0.09** (stress-220, sec-210) /
    **0.31** (fourier-real), settleFrames 2, worst longtask during chunked reveal
    155ms. LandingReport `errorPx ≤ 1`: PASS.
- **R5 — NOT TRIGGERED.** Neither VR-14 fallback fires. VR-10 and VR-13 flip
  PROVISIONAL → SETTLED.

## Batch sweep (220 fixture, uniquemath, 4x; per-step patch = frontier bump → flush end)

| batch | patch p95 (ms) | patch max | worst longtask | full reveal | pick |
|---|---|---|---|---|---|
| 2 | 8.1 | 13.4 | 125 [122–133] | 1999ms [1991–2007] | **← literal rule (smallest under 16.7ms)** |
| 4 | 14.2 | 18.9 | 131 [128–133] | 1110ms [1104–1110] | also under budget; VR-13 default; 2x faster reveal |
| 8 | 25.2 | 30.0 | 125 [125–128] | 744ms [740–765] | over the 16.7ms budget |

The pre-committed text picks the **smallest** batch under one frame budget → **2**.
Flag for synthesis (not re-litigated here): batch **4** also satisfies the budget at
p95 and halves time-to-full-reveal; "largest batch under budget" would pick 4. All
downstream cells (scroll/landing/fourier) ran at the mechanical pick, batch 2.

## The longtask/FCP table (eager arm = full mount + CV/CIS, uniquemath, 4x)

| N | worst longtask ms (median [min–max]) | FCP ms | mount-attributable (−147ms baseline) |
|---|---|---|---|
| 8 | 125 [117–129] | 252 | ~0 |
| 16 | 148 [148–154] | 272 | ~1 |
| 24 | 171 [170–177] | 300 | ~24 |
| 32 | **189 [185–198]** | 308 | ~42 |
| 40 | 219 [199–252] | 340 | ~72 |
| 60 | 245 [237–252] | 364 | ~98 |
| 80 | 284 [271–292] | 412 | **~137** |
| 120 | 365 [353–378] | 488 | ~218 |
| 160 | 427 [416–435] | 560 | ~280 |
| 220 | 550 [535–564] | 688 | ~403 |

(The fixture's ~1.9ms/section slope reproduces P2's 555ms finding at 220.)

Reference arms on 220: windowed worst longtask 147 [146–149] / FCP 244;
governed (batch 2) worst 125 [122–133] / FCP 248 / full reveal 2.0s.

## The real consumer (fourier_paper.tex via the library's own vite plugin — 98 flat sections)

| arm | worst longtask (4x) | FCP | full reveal |
|---|---|---|---|
| eager (full mount + CV) | **891ms [847–893]** | 1036ms | — |
| governed (batch 2) | **123ms [121–124]** | 260ms | 1285ms [1267–1287] |
| windowed 0.2.1 (reference) | 134ms [134–141] | 240ms | — |

The real paper is HEAVIER per section than the stress fixture (dense display-math
matrices, 26 figures): eager's 891ms is 1.6x the 220-section stress fixture's 564ms and
4.5x the 200ms budget. Pass-1's "~150–190ms at the real ~60-section consumer" scaling
estimate is falsified in both inputs (the consumer is 98 sections, and per-section cost
is ~3x the fixture's). Governed matches the windowed baseline's FCP within 20ms while
mounting everything within 1.3s.

## Caveats (honest)

- The demo's `main.ts` mounts synchronously inside the module-eval task → the fused
  longtask; both R3 readings reported above with the decomposition method stated.
- Scroll sweep is a programmatic scrollTop ramp (main-thread layout/paint cost), not a
  compositor fling; display rAF ~120Hz (8.33ms floor).
- fourier-real landing target `subject-isolation` (10th-from-last; the LAST section is
  bottom-clamped by definition and cannot align to top — the probe design accounts for
  the VR-23 bottom-clamp exemption).
- Landing errorPx measured against `scroll-margin-top`-adjusted geometry after a
  10-stable-frame settle; stress 0.09px is sub-pixel rounding, fourier 0.31px includes
  late figure-image RO writebacks.
- 4 figure `.pdf` sources are excluded from the copied assets (web formats only); no
  `<img>` in the rendered output references them.

## Closure

F-1 CLOSES: verdict table + S_cross constant + committed traces land together (§4
"Closure"). VR-10 and VR-13 flip PROVISIONAL → SETTLED in the same edit; the spec's
`mount` union gains the settled default:
`mount: 'governed'` default, `items.length ≤ 25 → 'eager'` auto-selection (literal
constant; 64 if synthesis adopts the decomposed reading), batchSize default per the
batch-sweep flag above (mechanical pick 2; engineering-preferred 4), `idleRevealAll: true`.
