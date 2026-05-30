# SK-V17 W0 — Baseline / Telemetry Ledger

Pass: SK-V17 wave triumvirate. Wave: W0 (baseline/telemetry). Date: 2026-05-30.
Status: `SK-V17-W0-open`. Behaviour LOC: **0** (W0 ran the existing canonical
harness; no parser/runtime behaviour was changed).
Master HEAD at capture: `33b51d8f4`.

## §1 — Canonical harness (the W6_SAMPLE_COUNT=1 fix)

Canonical harness: `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`
(N defaults to 200, asserts `N >= 50` — the telemetry-honesty gate; replaces the
statistically-inadequate single-sample W6 harness). Cold per-parse, no warm cache,
no amortised allocation, no cross-sample state; reports median/min/max/stddev in
Mbps (`bytes * 8 / s`), aarch64 `-C target-cpu=native`. Four workloads:

- `track1_full_parse` — `parser::parse_full -> emit_full_parse` (recognition; the
  fast path; `css_canon_bench.rs:103`).
- `track1_fact_stream` — `parser::parse -> emit_fact_stream` (the current benched
  typed materialization = a `Result<String,_>` fact-stream String; `:108`).
- `lightningcss` — `StyleSheet::parse(input, ParserOptions::default())` +
  `black_box(sheet.rules.0.len())` — **full-CSSOM materialization, the FAIR >SOTA
  bar** (SYNTHESIS §0.6).
- `cssparser` — `StyleSheetParser` token-scan; materializes nothing; flaw probe
  (not a fair materializing comparator).

4-corpus load-bearing byte sum = 979638 (animate 71750 + bootstrap 232803 +
material 495454 + tailwind 179631).

## §2 — W0 denominators (fresh N=200 cold medians, Mbps)

| corpus | track1_full_parse (recognizer) | track1_fact_stream (current typed) | **lightningcss@W0 (>SOTA bar)** | cssparser | recognizer/lcss | fact_stream/lcss |
|---|---:|---:|---:|---:|---:|---:|
| bootstrap | 2175.411 (±199.8) | 843.728 (±15.7) | **1112.393** (±25.2) | 2901.632 | 1.96× | 0.76× |
| tailwindcss | 2826.862 (±181.2) | 554.479 (±11.2) | **841.332** (±13.9) | 1740.865 | 3.36× | 0.66× |
| material-components-web | 2669.153 (±146.7) | 868.365 (±26.2) | **1292.260** (±25.5) | 3278.438 | 2.07× | 0.67× |
| animate | 2430.273 (±134.6) | 731.735 (±27.9) | **1218.685** (±16.8) | 2633.529 | 1.99× | 0.60× |

**LOCKED per-corpus lightningcss@W0 >SOTA bar:** bootstrap 1112.393 · tailwindcss
841.332 · material 1292.260 · animate 1218.685 Mbps. These are the W3 denominators
(`delta_vs_lightningcss > 1.0×` at N≥50 median); no fixed/inferred figure is used.

## §3 — Load-bearing finding

- The CSS **recognizer beats lightningcss on ALL 4 corpora (1.96×–3.36×)** — large,
  reproducible headroom (stddev 5–9% of median; tailwind is the recognizer's best
  at 3.36×).
- The **current materialization (fact-stream String) LOSES on ALL 4 (0.60×–0.76×)**
  — the String emission is the tax, exactly the S-P1 diagnosis.
- Therefore >SOTA is a **materialization** problem with real headroom, not a wall:
  W1 prunes the fact-stream into the tape, W2 rebuilds the lazy `ValueRef<G>`
  projection, W3 adds the shared NEON classifier. If the tape-materialized typed
  path stays meaningfully cheaper than the String path, it inherits the recognizer
  lead and clears the bar.

## §4 — W0 exit gate

| check | status |
|---|---|
| Canonical N≥50 harness established (css_canon_bench, N=200, asserts N≥50) | MET |
| Per-corpus N≥50 cold medians captured for all 4 workloads | MET (§2) |
| lightningcss@W0 denominator LOCKED per corpus (the >SOTA bar) | MET (§2) |
| JSON 51/51 ±1.0% guard (no behaviour drift) | MET — W0 is 0 behaviour LOC; the JSON parser/runtime is untouched (ran an existing bench only) |
| `SK-V17-W0-open` baseline doc | MET (this file) |

**W0 EXIT GATE: MET.** Ready for W1 (PRUNE: delete fact-stream + W5C array, route
the typed product into the existing tape; equality-before-speed, no speed admission).

## §5 — W3 >SOTA target (made measurable here)

Tranche success criterion (SYNTHESIS §0.1): `max(track1_typed / lcss@W0) > 1.0` on
≥1 **regular** corpus (animate OR bootstrap) at N≥50 cold median, typed full-CSSOM
plane, preserve-rich-ast + EXACT 8-field structural equality. Easiest cross by
recognizer headroom: **tailwind (3.36×)**; the regular-corpus targets are **animate
> 1218.685** and **bootstrap > 1112.393** Mbps. JSON 51/51 ±1.0% guarded every wave.
