# traces/w3 — the pass-3 W3 runtime-gate evidence (lanes E3 + E4), promoted

Text artifacts in this directory are byte-exact copies out of the `virt-p4`
prototype clone (fleet hygiene VR-74; scratchpad
`/private/tmp/claude-504/-Users-mkbabb-Programming-sci-report/61ef4d2b-22d2-4c2d-a271-4c36ee42c6b5/scratchpad/virt-p4`):

| Lane | Source commit | Promoted here | Stays clone-side (binaries + harness) |
|------|---------------|---------------|----------------------------------------|
| E3 / W3a — WebKit anchoring + print (§6.1 + §6.2) | `virt-p4 @ 761ba9d` (`probe/w3a/`) | `W3A-VERDICT.md`, `W3A-REPORT.json` | 8 anchor screenshots, `w3a-print-{eager-reference,governed}.pdf` (18 pp each), 3 fragmentation rasters, `run.mjs`/`app.mjs`/`client.mjs`/`skipcheck.mjs` + `client.bundle.js` |
| E4 / W3b — hydration + a11y (§6.3 + §6.4) | `virt-p4 @ 4391629` (`probe/w3b/`) | `W3B-VERDICT.md`, `W3B-REPORT.json`, `w3b-axe-report.json` | `w3b-keyboard-after-enter.png`, `verdict.mjs` (exit-code-gated recomputation), `run.mjs`/`app.mjs`/`client.mjs` + `client.bundle.js` |

Headline results: W3a — `compensateScrollTop` green on REAL WebKit 26.5
(stable-Safari shimmed arm, jumps ≤0.25px, absorbed/applied 1000/1000 —
exactly-one-corrector), governed PDF ≡ eager reference (18 = 18 pages,
byte-identical extracted text, 14,929 chars). W3b — 13/13 clauses green on
Chromium 149 / WebKit 26.5 / Firefox 151; the VR-25 hydration guard proven
load-bearing by a blinded-guard negative control (60 → 8 truncation, sentinel
lost); Readability extraction governed ≡ eager at 1.0000.

Repro: each verdict's closing section names the exact esbuild + node commands;
the harnesses run from the clone commits above.
