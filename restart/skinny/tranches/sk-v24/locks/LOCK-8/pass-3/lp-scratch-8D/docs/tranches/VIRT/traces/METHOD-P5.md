# VIRT P5 bench method (lane P5 — the F-1 three-way trace, SPEC-PASS-2 §4)

Machine: Apple Silicon macOS 25.4.0, Chromium (Playwright 1.61.1). CPU throttle via CDP
`Emulation.setCPUThrottlingRate` rate 4 for EVERY cell (the decision rule is defined at
4x). Viewport 1280x900. Display runs rAF at ~120Hz (8.33ms baseline frame). Prod build
(`vite build` demo → `vite preview` :5299). Isolated `newContext()` per RUN (VR-74).
3 runs per cell; median reported with min/max spread.

## Fixtures

- **stress-N** — the pass-1 P2 deterministic fixture, now `?n=`-parametrized
  (`demo/fixture.ts`): N flat sections = N/4 roots x (1 root + 3 subs), always with
  `?uniquemath=1` (defeats the useKatex module cache — every display equation is a
  distinct katex.renderToString call). Family: N in {40, 60, 80, 120, 160, 220}, plus a
  supplementary {8, 16, 24, 32} sweep to locate S_cross under the literal R3 reading.
- **fourier-real** — fourier-analysis's REAL paper (`paper/fourier_paper.tex` + compiled
  artifacts, copied into `demo/fourier/`), parsed at build time by the library's own
  `src/vite.ts` plugin (identical wiring to fourier's `vite.config.ts`, incl.
  extractedMacros); figure assets copied to `demo/public/assets/`. Flattens to **98**
  sections (the spec's "~60" was an estimate; 98 is the measured truth).

## Arms (`?mode=`)

- **eager** (protocol arm 1) = demo `mode=native`: ALL sections mounted, wrapper per
  section with `content-visibility:auto; contain-intrinsic-size:auto var(--lp-section-size)`.
- **governed** (protocol arm 2) = demo `mode=governed` (NEW this lane): VR-13 monotone
  frontier — initial frontier = estimated 1.5x-viewport fill (min 4); `idleRevealAll`
  advances by `?batch=` per idle callback (one Vue patch per step); a single trailing
  spacer at estimate keeps the scrollbar honest below the frontier; chunked
  `revealTo(id)`/`revealAll()` (4x batch per frame) exposed at `window.__lpGoverned`.
  Nothing ever unmounts. `?idle=off` isolates far-target probes.
- **windowed** (protocol arm 3) = the shipped 0.2.1 `useVirtualSectionWindow`
  spacer/slice template. REFERENCE TRACE ONLY (rule R1).

## Metrics

- Worst main-thread longtask (PerformanceObserver `longtask`, buffered, nav → settle /
  full reveal). CAVEAT: the demo's `main.ts` calls `createApp().mount()` synchronously at
  module top level, so bundle-eval and Vue mount FUSE into one task. The windowed arm
  (same bundle, ~3-section mount) gives the shared eval baseline (~147ms at 4x);
  the verdict reports S_cross under BOTH the literal reading and the mount-attributable
  decomposition (worst − baseline). Real consumers (fourier: router + code-split
  vendor-paper chunk) mount in a separate task, so the decomposed reading transfers.
- FCP (paint timing).
- 20s scripted scroll: `scrollTop = f(t)·max` per rAF (constant time-velocity, full
  document sweep), frame deltas → avg / p95 / max / count>33ms. Programmatic sweep
  measures main-thread layout+paint per frame, not compositor fling.
- Governed: time-to-full-reveal (`__lpFullRevealMs`, app start → frontier == N), per-step
  patch duration (frontier bump → Vue flush end) and step duration (→ 2nd rAF).
- LandingReport: `revealTo(target)` + `scrollIntoView` + settle (scrollTop stable 10
  frames, cap 500) → `errorPx = |rect.top − scrollerTop − scroll-margin-top|`,
  settleFrames, revealMs, worst longtask during. Target = 10th-from-last section
  (the LAST section is bottom-clamped by definition and cannot align to top).

## Scripts / artifacts

- `bench/f1.mjs` → `bench/f1-results.json` (all raw runs incl. per-run longtask lists)
- `bench/f1-smalln.mjs` → `bench/f1-smalln.json` (supplementary small-N eager sweep)
- `bench/f1-verdict.mjs` → `bench/f1-verdict.json` (applies R1..R5 mechanically)
- Known cosmetic defect: `f1.mjs` spreads `...m` after the cell's `fixture` label, so
  stress rows read `fixture:"stress"`; key on `totalFlat` (the verdict script does).

Reproduce: `npx vite build --config demo/vite.config.ts && npx vite preview --config
demo/vite.config.ts` then `node bench/f1.mjs && node bench/f1-smalln.mjs && node
bench/f1-verdict.mjs`.
