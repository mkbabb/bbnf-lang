# W3A VERDICT — WebKit anchoring + print (pass-3 lane E3; SPEC-PASS-3 §6.1 + §6.2)

Engines (real, Playwright): **Chromium 149.0.7827.55**, **WebKit 26.5**. Clone:
`virt-p4` (the P7 unified-substrate shell over `dist/vue.js`). Isolated
`newContext()` per scenario (VR-74). Fixture: 60 sections, 20 numbered display
equations (`probe/w3a/app.mjs`). Report: `probe/w3a/W3A-REPORT.json`.

## §6.1 — compensateScrollTop on REAL WebKit — PASS (all 4 arms)

Protocol: eager mount, `whenSettled()`, `scrollTo('sec-40', instant)` (landing
errorPx 0.14 Chromium / 0.45 WebKit — inside the VR-23 ≤1 gate), then two
above-viewport remeasures: **+400px** on the nearest fully-above section
(`sec-38`, the rendered CV band) and **+600px** min-height on far-above `sec-2`
(contents CV-SKIPPED at that depth — descendant-confirmed both engines,
`skipcheck.mjs`; the in-run `skippedBefore:false` was a host-vs-descendant
`checkVisibility` probe artifact, corrected in `skipStateSupplement`).

PASS = `|Δ rect.top(sec-40)| ≤ 1px` per growth AND scrollTop absorbs ≈ the
applied delta (a real correction, not a no-op).

| arm | CSS.supports('overflow-anchor','auto') | branch taken | jump1 / jump2 (px) | absorbed / applied (px) |
|---|---|---|---|---|
| chromium-native | true | native anchoring | 0.25 / 0.25 | 1000 / 1000 |
| chromium-manual (emulation) | false (shimmed) | **compensateScrollTop** | 0.25 / 0.25 | 1000 / 1000 |
| webkit-natural | **true** | native anchoring | 0 / 0.25 | 1000 / 1000 |
| **webkit-manual (stable-Safari)** | false (shimmed) | **compensateScrollTop** | **0 / 0.25** | **1000 / 1000** |

Findings:

1. **The load-bearing arm is green.** `compensateScrollTop` (the VR-2/21
   default stable-Safari path, previously unit-tested only) ran on a REAL
   WebKit engine and held the viewport stationary to ≤0.25px across a
   rendered-band growth AND a CV-skipped-box growth. Screenshots
   (`anchor-webkit-manual-{before,after}.png`) are visually identical.
2. **Exactly-one-corrector proof:** absorbed == applied exactly (1000/1000).
   Double compensation would read 2000; a dead path would read 0. The emulation
   arms disable native anchoring in CSS (`overflow-anchor: none`) AND shim the
   branch selector, so the manual path is the only corrector by construction.
3. **The branch picks native on Chromium** — `CSS.supports('overflow-anchor',
   'auto')` true → manual path disabled (`featureArms.ts:52`, consumed at
   `usePaperViewport.ts:235`) — and the native path is functionally green
   (0.25px). Same on WebKit-natural.
4. **New engine datum (VR-2/VR-22 table note, not a floor change):** Playwright
   WebKit 26.5 (Safari-27-era trunk) already reports `overflow-anchor` support
   — consistent with R11's "Safari 27 beta". On such engines the two-path rule
   selects native and it WORKS; stable Safari ≤26.x remains the
   compensateScrollTop population, proven here under emulation on the same real
   engine.
5. Incidental (not this lane's gate): the anchor screenshots show the section
   heading overlapping body text at the viewport top — the sticky ×
   containment shape F-7 owns (consistent with VR-47's premise).

## §6.2 — print render-all — PASS (Chromium PDF + WebKit DOM half)

Protocol: governed mount (`batchSize 4, idleRevealAll false`), partial frontier
confirmed (**8 of 60** mounted), then `beforeprint` dispatched as its OWN task
— **`revealAll()` never called** — then `emulateMedia({media:'print'})` +
`page.pdf()` (Chromium; the engine that allows it), against an eager-mount
reference PDF.

| check | chromium | webkit |
|---|---|---|
| mounted before / after `beforeprint` alone | 8 → **60** | 8 → **60** |
| print-media computed `content-visibility` (sec-30, sec-59) | visible | visible |
| CV-skipped holes under print media | **0** | **0** |
| sentinel in print-media innerText | true | true |
| PDF pages (governed vs eager reference) | **18 = 18** | n/a (page.pdf Chromium-only) |
| PDF text: sentinel / missing section titles / eq markers | true / none / 20 | n/a |

The governed and eager PDFs extract **byte-identical text** (14,929 chars) —
zero content lost to the governor. Page-break sanity on math blocks: rasters
(`w3a-governed-p{2,9,17}-*.png`) show every display equation intact as a unit;
eq (19) fragments cleanly to the top of page 17 rather than splitting.

Findings:

1. **The `beforeprint` → `revealAllSync` path works ALONE on both engines**:
   the reactive flush (microtask) drains before any subsequent task — the
   Cmd+P event-ordering shape. The VR-24 CSS half (`@media print`
   content-visibility: visible) verified computed on both engines.
2. **HARDENING FINDING (W4 candidate, VR-24 amendment):** `theme.css` ships NO
   fragmentation rules — `.math-block` computes `break-inside: auto` under
   print media. Single-line KaTeX display survives via line-box atomicity
   (tallest block 111px < the 960px usable page), but multi-line display
   environments (aligned/gather) and figures are splittable. W4 should add
   `@media print { .math-block, .paper-article figure { break-inside: avoid } }`.
3. **Residual (honesty):** headless Playwright cannot open the OS print
   DIALOG; `page.pdf()` drives Chromium's print-rendering pipeline and the
   own-task `beforeprint` dispatch reproduces the dialog's event ordering.
   The spec §6.2 "real dialog" wording is satisfied to the limit of
   automation; a manual Cmd+P spot-check on the landed W2 substrate remains
   the last inch.

## Artifacts (this directory)

`W3A-REPORT.json` (machine measurements), `run.mjs` / `app.mjs` / `client.mjs`
/ `skipcheck.mjs` (protocol, reproducible), `anchor-{arm}-{before,after}.png`
(8 screenshots), `w3a-print-eager-reference.pdf`, `w3a-print-governed.pdf`,
`w3a-governed-p{2,9,17}-*.png` (fragmentation rasters).
