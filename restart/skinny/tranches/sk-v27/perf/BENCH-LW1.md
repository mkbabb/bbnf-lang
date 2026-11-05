# LW1 PERF bench records: NOISY

**Label: NOISY. Nothing here is a certified claim.**

- The 1-minute load was 15–22 for every run, never below 8.
- No quiet window opened during the seat; per X-7 the runs went ahead and are labelled NOISY.
- Each figure is one divan process of 100 samples (one iteration each), run in two windows with the order reversed. That makes two process reps, not the ≥ 10 that certification requires (CON-1's harness, fresh process per rep, lands in LW1.CON-1).
- The runner was `guarded --cpu-cores 0`.
- Raw logs: `~/.dev-logs/bbnf-lockstep/rust-perf-lw1/bench-run.log` and `bench-rp13.log`.
- Profile: `[profile.bench]` (fat LTO, cgu 1). Host: Apple M5 Max, NEON tier.

## LW1-PERF-1: `simd-scan` stage-1 throughput, base vs cure (NEON)

The base is `perf-base`, detached at `b4e635d3e`; the cure is `sk-v27-rust-perf`. Bench: `crates/simd-scan/benches/stage1_throughput.rs`, i.e. `scan_structural` on the five corpora.

| corpus | W1 base median (fastest–slowest) | W1 cure median (fastest–slowest) | W2 cure | W2 base | cure/base W1 | cure/base W2 |
|---|---|---|---|---|---|---|
| bootstrap.css | 427.3 µs (398.6 µs–2.597 ms) | 239.1 µs (211.5–329.1 µs) | 243.3 µs (218.7–428.3 µs) | 468.0 µs (424.9 µs–4.1 ms) | 0.560 | 0.520 |
| tailwind.css | 5.547 ms (4.853–25.57 ms) | 2.879 ms (2.433–5.111 ms) | 2.899 ms (2.52–4.42 ms) | 5.776 ms (5.028–13.48 ms) | 0.519 | 0.502 |
| canada.json | 695.1 µs (680.7 µs–1.609 ms) | 601.0 µs (595.6 µs–2.475 ms) | 660.2 µs (611.7–883.7 µs) | 703.2 µs (698.2 µs–1.039 ms) | 0.865 | 0.939 |
| citm_catalog.json | 441.1 µs (431.3 µs–17.02 ms) | 376.6 µs (375.9–601.6 µs) | 382.0 µs (356.8–599 µs) | 456.5 µs (446.4 µs–10.39 ms) | 0.854 | 0.837 |
| twitter.json | 158.2 µs (157.3 µs–3.034 ms) | 147.3 µs (118.9–271.2 µs) | 158.1 µs (118.2–284.5 µs) | 166.2 µs (162.6–397.1 µs) | 0.931 | 0.951 |

**Reading:**

- **No regression from the shared-stripe refactor**, on any corpus, in either window.
- The CSS rows move by about 2x in both windows, both orders. The cause is in the code: CSS has two quote classes. Before the cure, the multi-class walk visited 64 bits × n classes per stripe. `stripe::multi_class_body` skips every bit that holds no quote byte (`any_quote & mask == 0`).
- The JSON rows (0.84–0.95) lie inside this run's dispersion. They are **not claimed**.
- Certifying the CSS row needs LW2's frozen floor in a quiet window.

## RP-13 re-measure at the sk-v27 base (`b4e635d3e`, with the numeric-leaf probe NOT applied)

Emitted `JsonParser::parse` (`json_monolithic`) vs incumbent `json_parser()` (`json_parse_that --features competitor`). The incumbent is parse-that `b4f63f4`, reached through the `../parse-that` patch. Medians, with fastest–slowest in brackets.

| corpus | W1 emitted | W1 incumbent | W2 incumbent | W2 emitted | ratio W1 | ratio W2 |
|---|---|---|---|---|---|---|
| canada | 5.630 ms (4.777–22.73) | 3.490 ms (3.111–4.544) | 3.285 ms (2.866–4.204) | 5.735 ms (4.868–6.544) | **1.61** | **1.75** |
| citm | 1.936 ms (1.434–12.4) | 1.297 ms (1.244–3.373) | 1.436 ms (1.267–3.668) | 1.560 ms (1.404–2.114) | 1.49 | 1.09 |
| twitter | 520.2 µs (469.2 µs–1.07 ms) | 674.1 µs (522.2–906.5 µs) | 572.3 µs (525.4–926.3 µs) | 570.8 µs (488.2–977.6 µs) | 0.77 | 1.00 |
| data (35 KB) | 24.04 µs | 32.64 µs | 36.66 µs | 24.31 µs | 0.74 | 0.66 |

**Reading (NOISY):**

- At the sk-v27 base, the emitted parser is still about **1.6–1.75x slower than parse-that on canada**.
- This agrees with the record's "1.50–1.60x before the probe" and with the probe leaving canada unchanged.
- The citm and twitter ratios swing by up to 0.4 between windows, so they are noise at this load.
- The data row (0.66–0.74) favours the emitted parser in both windows, but it is not certified.
- See `RP-13.md` for the trace and the disposition.
