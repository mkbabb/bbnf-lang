# SK-V18 S-P1 — A0 Re-Baseline Ledger

Source: `restart/skinny/tranches/sk-v18/research/p1/raw/css_canon_n200.txt` (N=200 cold per-parse medians)
Capture: `restart/skinny/tranches/sk-v18/research/p1/raw/capture.log`. The `capture.log`
header stamps `0fbee121f`, but that is the **S-P0 audit-convergence SHA only**: at
`0fbee121f` the `css_canon_bench.rs` harness with the `track1_rich` workload **did not yet
exist** (it was created by the bit-rot fix `784ceb418`, the current HEAD; verified
`git cat-file -e 0fbee121f:…/css_canon_bench.rs` → absent). The `track1_rich` rows below
therefore came from the **`784ceb418` binary** (the bit-rot-fixed harness), not from
`0fbee121f`. The header SHA is corrected here to avoid claiming a row a SHA could not produce.
Host: arm64 / Apple M5 Max. x86 is a prune target (not measured).

## LOAD CAVEAT (honesty)

This capture ran under **concurrent-session machine load**. `capture.log` line 2 records
`host_loadavg: 4.35 6.03 5.70` (1m/5m/15m). On a loaded host **absolute Mbps is
DIRECTIONAL / depressed** versus the W0 quiet baseline and **must NOT be re-locked as a
new baseline**.

W0-LOCKED lightningcss absolute bars (quiet, reference only):
bootstrap 1112.393 | tailwindcss 841.332 | material-components-web 1292.260 | animate 1218.685 Mbps.
Today's lcss medians (402 / 299 / 475 / 480) are far below those W0 bars — confirming the
load depression. Hence the load-robust load-bearing outputs are the **same-run ratios**
(track1_rich/lcss and track1_full_parse/lcss) and the **relative hot-leaf rank**, both of
which are invariant to a uniform host slowdown that hits our parser and lightningcss alike.

## CANONICAL-BENCH BIT-ROT FIX (recorded)

The old `track1_fact_stream` workload referenced the **W1-pruned fact-stream String** and no
longer compiled, breaking the canonical CSS harness. THIS PASS replaced it with
**`track1_rich`** (`parser::rich_summary`, summing **9 materialized fields**:
rules/at_rules/qualified_rules/declarations/selectors/dimensions/numbers/colors/functions).
Per the source-of-truth doc comment (`css_l4_declaration_values/generated.rs:297-304`),
`rich_summary` reconstructs the rich typed CSSOM **LAZILY** from the tape — re-deriving every
field from `(source, offset)` via `ValueRef` spans, **writing nothing to the payload arena**
("preserve-rich-ast: rich, lazy, not eager, not flattened"). So the honest framing (matching
A2 + SYNTHESIS) is **full-value-materialization, lazy-rich** — it reads and categorizes every
node's typed value-head (the same work lightningcss does to populate its CSSOM), NOT an eager
arena build. Change already committed (`784ceb418`). This closes residual **R14/H1**: where
the old `track1_fact_stream` compared a thin fact String against the lightningcss full CSSOM,
`track1_rich` now realizes all 9 typed value-plane fields per node — equal-depth value work
against lightningcss's full CSSOM, the fair comparator, not a count-only structural probe.

## Per-Corpus Table (median Mbps, all 4 workloads)

| corpus                  | bytes  | track1_full_parse | track1_rich | lightningcss | cssparser |
|-------------------------|-------:|------------------:|------------:|-------------:|----------:|
| bootstrap               | 232803 |           940.282 |     880.961 |      402.246 |  1123.098 |
| tailwindcss             | 179631 |          1013.597 |    1010.050 |      299.264 |   542.602 |
| material-components-web | 495454 |           900.446 |     788.282 |      475.464 |  1266.775 |
| animate                 |  71750 |           956.866 |    1009.342 |      480.368 |  1011.863 |

Notes on dispersion (load artifacts): tailwindcss shows pathological stddev
(track1_full_parse min 311.254 vs max 1085.042, stddev 159.820; lcss min 76.806) — the
heavy-load tail. Medians remain robust. `track1_rich` on bootstrap is the tightest line
(stddev 12.543), the most trustworthy single reading.

## Ratios (LOAD-ROBUST — the load-bearing output)

| corpus                  | track1_rich / lcss | track1_full_parse / lcss | rich verdict | recognizer verdict |
|-------------------------|-------------------:|-------------------------:|:------------:|:------------------:|
| bootstrap               |              2.190 |                    2.338 | PASS         | PASS (ahead)       |
| tailwindcss             |              3.375 |                    3.387 | PASS         | PASS (ahead)       |
| material-components-web |              1.658 |                    1.894 | PASS         | PASS (ahead)       |
| animate                 |              2.101 |                    1.992 | PASS         | PASS (ahead)       |

cssparser (token-scan flaw probe, not a fair CSSOM bar): it out-tops us on bootstrap (1123)
and material (1267) but is BELOW us on tailwindcss (543) and roughly even on animate (1012) —
consistent with cssparser doing token-scan only, not the rich CSSOM projection. Directional,
not a parity claim.

## VERDICT (load-robust finding)

**PASS on all four corpora.** On the two regular corpora called out (animate, bootstrap):

- `track1_rich` (the >SOTA lazy-rich full-value-materialization CSSOM product) BEATS
  lightningcss: **animate 2.101×**, **bootstrap 2.190×** — both ratios > 1.0.
- The recognizer `track1_full_parse` is still AHEAD of lightningcss: **animate 1.992×**,
  **bootstrap 2.338×**.

The smallest margin (material-components-web, rich 1.658×) is still a comfortable beat. The
>SOTA position (CSS beats lightningcss) holds same-run on every corpus for both the rich
product and the recognizer. Absolute Mbps this pass is depressed by loadavg 4.35/6.03/5.70
and is NOT a re-locked baseline; a quiet re-capture is required before any absolute claim.
