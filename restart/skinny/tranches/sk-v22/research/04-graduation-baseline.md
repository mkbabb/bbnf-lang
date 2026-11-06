# SK-V22 — the graduation baseline, load-accounted

Date 2026-07-09. Solo profiling session, the empirical leg of the graduation
(PASS-ALPHA §Standing-corrections 5). Skinny at HEAD is the candidate tree of
record; this is its certified cold performance floor. Method is the SK-V21
pass-2 protocol validated at PROTO-A1 (`sk-v21/prototypes/PROTO-A1-bench-adjudication.md`)
and report 33 (`sk-v21/research/33-cold-truth-profiling.md`): PMU cycles/byte via
`proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles`, paired per-sample interleaving,
N=200 cold-per-parse, min AND median, per-block loadavg, THREE separated capture
windows with cross-window agreement reported, 95% CIs on paired deltas.

## The instrument (rebuilt from the report-33 / A1 recipe)

`crates/bbnf-bench/src/bin/json_canon_bench.rs`, rebuilt this session as a
byte-faithful mirror of `css_canon_bench.rs` cold discipline (one out-of-band
touch, then N timed single parses, fresh state, output dropped). Release
fat-LTO, `debug=true`, `split-debuginfo="packed"` (`skinny/Cargo.toml:78-85`),
**system allocator** (no `#[global_allocator]` in the bin — faithful to A1, NOT
P2's private mimalloc harness). Three modes: `default N` (cold ns/byte + Mbps),
`pmu ITERS` (per-lane cyc/byte + ins/byte), `paired N` (interleaved paired
ratios, 95% CI). Lanes: `parse_only` (structural validation, no tape),
`parse_tape` (the live lazy-view product; the only path that builds the offset
tape), `fused` (`direct_struct::track1_digest` = `parse_direct` sink, eager
digest), `sonic` (sonic-rs 0.5.8), `serde` (serde_json). CSS: the pre-existing
`css_canon_bench` unchanged. The JSON parse path is frozen — `generated.rs` last
touched 2026-06-02, `parser.rs` 2026-05-27, `scan.rs` 2026-05-20 — i.e. the
identical code A1 (2026-07-05) and report 33 profiled.

**Harness self-validation (cross-session).** My PMU cyc/byte reproduces the
quiet-baseline references within ~2-5%: twitter `parse_tape` **1.607** vs A1
1.5654 / report-33 1.559; twitter `parse_only` **1.358** vs report-33 1.343
(near-exact); canada `parse_tape` **2.081** vs A1 2.0376; citm `sonic` **2.347**
vs A1 2.2234. The instrument is the same truth-floor, re-confirmed at HEAD.

## Load accounting — read this before trusting any absolute

- Host arm64 Apple-M, 18 logical cores. Session loadavg band **8.6 → 14.0**
  (1-min), never below 2.0 (the standing environmental gate). Per-block loadavg
  recorded beside every table below.
- **Two metric classes.** (1) **PMU cyc/byte and ins/byte are load-robust /
  near-absolute** — a per-process counter, not wall time; it reproduces the
  loadavg-8-10 quiet references within 5% (above). These are the **certified**
  numbers. ins/byte is fully load-INVARIANT (a deterministic instruction count).
  (2) **Wall ns/byte and Mbps are DIRECTIONAL absolutes** — frequency- and
  scheduling-sensitive. Their **same-run inter-lane ratios are robust**, and the
  **paired-interleaved ratios (§2) are load-CERTIFIED** because each ratio's two
  sides are timed back-to-back inside one sample, so load hits both equally.
- **Cross-window drift = the load-noise floor.** Across three separated windows
  the certified cyc/byte drifts ≤3% on compute-bound lanes (twitter/citm all
  lanes, canada structural). The one exception: **canada `fused` cyc/byte** ran
  3.291 / 3.333 / 3.970 — window 3 (captured at loadavg 12.2) is +19% over
  windows 1-2 (loadavg ~10). That lane is the memory-bandwidth-bound float
  materialiser; under heavier contention even its cycle count inflates. So:
  cyc/byte is load-robust for compute-bound work, ~±20% for the one
  memory-bound lane at loadavg 12 vs 10.
- **Estimated absolute-inflation factor.** My quietest windows (loadavg ~8.6 CSS
  post-cold, ~9.5 JSON) land the wall throughput at or slightly ABOVE report-33's
  loadavg-8-10 absolutes — twitter `parse_tape` cold ~21,000 Mbps here vs
  report-33's 16,923 (+24%, the favourable direction). So at this load band the
  wall absolutes are **not materially degraded** (~1.0×, within ±25%); the
  M-series holds clocks and median-filtering removes stalls. The catastrophic
  ~20× wall inflation CRIT-P2 diagnosed was at loadavg 16+, a regime this session
  never entered. The min-sample (fastest) is the unloaded wall estimator; the
  median/min gap is only 4-8% on JSON, bounding residual load jitter on the parse
  itself to <10% at this band.

Reproduce: `cargo build --release -p bbnf-bench --bin json_canon_bench --bin
css_canon_bench`; `./target/release/json_canon_bench 200` (cold), `... pmu 800`,
`... paired 200`; `./target/release/css_canon_bench 200`, `CSS_CANON_PMU=1 ... 800`.
Raw captures: `/private/tmp/skv22_w1.txt`, `_w2.txt`, `_w3.txt`, `_css.txt`.

---

## Deliverable 1 — the graduation baseline table (JSON, cold)

**PMU cyc/byte — the certified column** (mean of 3 windows; iters=800; loadavg
per window 10.2 / 10.2 / 12.2). ins/byte is load-invariant.

| corpus | lane | cyc/B | ins/B | CPI | vs sonic (cyc) |
|---|---|--:|--:|--:|--:|
| twitter | parse_only | 1.358 | 6.25 | 0.216 | — |
| twitter | **parse_tape** (product) | **1.607** | 9.98 | 0.161 | **0.48× (2.10× faster)** |
| twitter | fused (sink) | 1.981 | 12.39 | 0.160 | 0.59× (1.70× faster) |
| twitter | sonic | 3.371 | 21.37 | 0.158 | 1.00 |
| twitter | serde | 4.643 | 25.86 | 0.180 | 1.38× |
| canada | parse_only | 1.831 | 14.38 | 0.127 | — |
| canada | **parse_tape** | **2.081** | 16.32 | 0.127 | **0.71× (1.41× faster)** |
| canada | fused | 3.531 | 27.67 | 0.128 | **1.21× (SLOWER)** |
| canada | sonic | 2.930 | 19.74 | 0.147 | 1.00 |
| citm | parse_only | 1.005 | 5.58 | 0.180 | — |
| citm | **parse_tape** | **1.138** | 7.39 | 0.153 | **0.48× (2.06× faster)** |
| citm | fused | 1.311 | 9.90 | 0.132 | 0.56× (1.79× faster) |
| citm | sonic | 2.347 | 16.34 | 0.148 | 1.00 |

**Cold wall — DIRECTIONAL** (median ns/byte across 3 windows; min = fastest
sample; Mbps = 8000 / ns_per_byte; window loadavg pre-cold ~10.2/10.2/12.2).

| corpus | lane | med ns/B | min ns/B | med Mbps | cross-window med ns/B |
|---|---|--:|--:|--:|---|
| twitter | parse_tape | 0.374 | 0.360 | 21,388 | 0.374 / 0.380 / 0.384 |
| twitter | fused | 0.464 | 0.449 | 17,244 | 0.464 / 0.481 / 0.504 |
| twitter | sonic | 0.771 | 0.740 | 10,373 | 0.771 / 0.801 / 0.842 |
| twitter | serde | 1.080 | 0.999 | 7,404 | 1.080 / 1.129 |
| canada | parse_tape | 0.487 | 0.454 | 16,416 | 0.487 / 0.490 / 0.496 |
| canada | fused | 0.828 | 0.761 | 9,657 | 0.828 / 0.820 / 0.911 |
| canada | sonic | 0.730 | 0.670 | 10,959 | 0.730 / 0.720 / 0.753 |
| citm | parse_tape | 0.267 | 0.256 | 29,952 | 0.267 / 0.261 / 0.274 |
| citm | fused | 0.308 | 0.292 | 25,965 | (W1; ≈ cyc-scaled) |
| citm | sonic | 0.545 | 0.507 | 14,687 | 0.545 / 0.549 / 0.601 |

**The headline.** The live lazy-view product `parse_tape` beats both SOTA
baselines cold on every corpus — **2.10× / 1.41× / 2.06× vs sonic-rs** and
2.75× / 2.51× / 2.75× vs serde_json (cyc/byte). The generalized generated parser
outruns hand-written SOTA under honest cold discipline. The JSON >SOTA claim
holds at the graduation baseline.

## Deliverable 2 — paired-interleaved ratios (load-CERTIFIED)

Each sample times `parse_tape`, `fused`, `sonic`, `serde` back-to-back; ratio =
A_ns/B / B_ns/B per sample; mean + 95% CI over N=200, three windows. Ratio < 1 =
skinny faster.

| corpus | ratio | W1 | W2 | W3 | mean | speedup | verdict |
|---|---|--:|--:|--:|--:|--:|---|
| twitter | tape/sonic | 0.497 | 0.527 | 0.522 | 0.515 | **1.94×** | skinny wins |
| twitter | fused/sonic | 0.676 | 0.699 | 0.655 | 0.677 | 1.48× | skinny wins |
| canada | tape/sonic | 0.732 | 0.733 | 0.713 | 0.726 | **1.38×** | skinny wins |
| canada | **fused/sonic** | 1.213 | 1.163 | 1.223 | **1.199** | **0.83×** | **fused LOSES** |
| citm | tape/sonic | 0.541 | 0.502 | 0.544 | 0.529 | **1.89×** | skinny wins |
| citm | fused/sonic | 0.597 | 0.601 | 0.619 | 0.606 | 1.65× | skinny wins |

Every window's 95% CI excludes 1.0 in the same direction. The **canada
fused/sonic loss is confirmed across all three windows** (CI [1.17,1.25] W1,
[1.12,1.21] W2, [1.17,1.28] W3) — this reproduces report 33's sign and A1's
Deliverable-2 settlement (eager `materialize_f64` float tax loses to sonic's SIMD
number parse). The lazy `parse_tape` product beats sonic on canada (1.38×); only
the EAGER fused sink loses there. Per-corpus, never a 4/4 sweep — the SPEC's
per-corpus framing is upheld.

## Deliverable 3 — ATTRIBUTION: where skinny's time goes at HEAD

**Stage decomposition (fresh, load-robust — PMU cross-mode deltas).** The lane
ladder `parse_only → parse_tape → fused` isolates the three stages; ins/byte
deltas are fully load-invariant.

| corpus | structural floor (parse_only) | + tape emit+alloc | + eager materialize+digest |
|---|---|---|---|
| twitter | 1.358 cyc/B · 6.25 ins/B (**68% of product**) | +0.253 cyc/B (+19%) · +3.73 ins/B | +0.611 cyc/B · +6.14 ins/B (over floor) |
| canada | 1.831 cyc/B · 14.38 ins/B | +0.257 cyc/B (+14%) · +1.93 ins/B | **+1.481 cyc/B · +13.26 ins/B** (the float tax) |
| citm | 1.005 cyc/B · 5.58 ins/B | +0.133 cyc/B (+13%) · +1.81 ins/B | +0.306 cyc/B · +4.32 ins/B |

Reading: the **scalar structural walk is the floor and the bulk** — 55-68% of the
product's cycles. **Tape emit+alloc adds a modest +13-19% cycles** (but +30-60%
instructions at very low CPI 0.16 — cheap, high-ILP work). **Eager number
materialise+digest is the largest incremental cost and scales with numeric
density**: negligible on citm, +0.61 on twitter, **+1.48 cyc/B on canada** — the
one lever that localises to `materialize_f64` (the Eisel-Lemire/SWAR target). On
the lazy `parse_tape` PRODUCT number materialisation is absent (lazy in the view),
so the product never pays this — it is the fused/digest sink's tax.

**Leaf-level within-stage split (report-33 xctrace Time Profiler, transferred).**
Live xctrace/samply symbolisation was not attempted this session: the load band
(9.5-14) makes headless symbolisation unreliable, and per the charter fallback
("if neither symbolizes, use the stage-timer decomposition from report 33") the
PMU decomposition above is the primary attribution. The finer leaf ranking is
carried from report 33's xctrace capture on the **identical frozen path** (the 92
`parse-attribution` deinline sites still present in `generated.rs`), re-anchored
by the fresh floor cyc/B above (twitter parse_only 1.358 vs report-33 1.343):

| stage | ~leaf self-time | dominant leaves |
|---|--:|---|
| String span scan | **~23%** | match_tiny_plain_string, consume_quote_at_cursor, first_control_byte |
| Structural navigation | ~21% | parse_key_colon, consume_structural, dispatch_value |
| Whitespace skip | ~14% | skip_ascii_whitespace, skip_ascii_spaces |
| Number span scan | ~5% | scan_digit_run, match_number_span_from_first |
| Alloc (offset-Vec grow + copies) | ~5-6% | raw_vec grow, copy_nonoverlapping |
| Tape emit | ~4% | emit_plain_offset, patch_flags |

Ranking: **scanner dominates (string+structural+ws+number ≈ 63%; string span
scanning is the single largest stage) > alloc ~5-6% > number decode ~5% (lazy on
the product) > tape emit ~4%.** No NEON/structural-scan symbol appears — the
one-shot SIMD scan is dead on the live lane (report 33's DELETE verdict stands).

## Deliverable 4 — the CSS side (cold, track1 vs lightningcss)

`css_canon_bench`, N=200 cold + PMU iters=800; loadavg pre-cold 8.9, post-cold
8.6. lightningcss is wired (full-CSSOM `StyleSheet::parse`); cssparser is the
bare token-scan probe.

**PMU cyc/byte — certified.**

| corpus | track1_full | track1_rich | lightningcss | cssparser | track1 vs lcss |
|---|--:|--:|--:|--:|--:|
| bootstrap | 15.02 | 15.13 | 35.32 | 12.47 | **2.35× faster** |
| tailwindcss | 13.29 | 13.10 | 46.16 | 21.86 | **3.47× faster** |
| material | 13.48 | 14.42 | 30.39 | 11.02 | **2.25× faster** |
| animate | 14.56 | 13.24 | 30.08 | 13.76 | **2.07× faster** |

**Cold wall Mbps (median) — directional.**

| corpus | track1_full | track1_rich | lightningcss | cssparser |
|---|--:|--:|--:|--:|
| bootstrap | 2039 | 2060 | 929 | 2680 |
| tailwindcss | 2437 | 2435 | 689 | 1586 |
| material | 2241 | 2049 | 1008 | 2872 |
| animate | 2052 | 2259 | 1117 | 2426 |

Track1 full structural parse beats lightningcss's full-CSSOM bar by **2.07-3.47×
in certified cyc/byte** and **1.8-3.5× in cold Mbps**, holding at/above that when
materialising the rich typed CSSOM (`track1_rich`). It trails cssparser's bare
token-scan on bootstrap/material but beats it on tailwind. Reproduces report 33's
CSS table (2.0-3.4× vs lightningcss) at HEAD. Note the very low CSS cold min_mbps
(e.g. animate 183) are load-stall outliers — the median is the robust statistic,
which is why it is reported.

## What the graduation inherits

1. **The certified floor is intact and >SOTA.** `parse_tape` beats sonic-rs
   2.10×/1.41×/2.06× and serde 2.75×/2.51×/2.75× in cyc/byte cold; CSS track1
   beats lightningcss 2.07-3.47×. These are the load-robust numbers the graduated
   tree's PERFORMANCE-THESIS carries forward.
2. **The one honest gap is canada eager float** — fused/sonic 1.20× SLOWER,
   confirmed across three windows; +1.48 cyc/B materialise tax localised to
   `materialize_f64`. Eisel-Lemire/SWAR number decode is the single measured win,
   and only on eager/numeric workloads; the lazy product never pays it.
3. **The scanner is the hot path** (string span scan first, ~63% combined); tape
   emit ~4%, alloc ~5-6%, dead-NEON scan absent (DELETE stands).
4. **The instrument is re-established at HEAD** — `json_canon_bench` rebuilt,
   self-validated against A1/report-33 within 5%. No JSON figure need be quoted
   warm again.

## Evidence index

- Harness: `crates/bbnf-bench/src/bin/json_canon_bench.rs` (rebuilt this session),
  `css_canon_bench.rs` (pre-existing).
- Raw windows: `/private/tmp/skv22_w1.txt`, `_w2.txt`, `_w3.txt`, `_css.txt`.
- Frozen JSON parse path: `crates/runtime/src/grammars/json/{generated.rs,parser.rs,scan.rs}`,
  `tape/assembler.rs`; lazy view number `view.rs:231`.
- Method of record: `sk-v21/prototypes/PROTO-A1-bench-adjudication.md`,
  `sk-v21/research/33-cold-truth-profiling.md`.
