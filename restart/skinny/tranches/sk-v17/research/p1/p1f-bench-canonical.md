# SK-V17 P1-F: Canonical N>=50 Cold CSS Bench Harness + RESULTS Delta

Pass: S-P1 Profile. Cycle: V4.
Date: 2026-05-29.
Scope: P1-F bench/measurement row (dispatch-specialised). Formalize the canonical
N>=50 cold-per-parse CSS harness; report median/min/max/stddev per benched corpus
for track1 (typed full-parse + fact-stream) / lightningcss full-CSSOM / cssparser
token-scan; resolve every hot leaf to a symbol; compute the delta vs the prior
SK iteration's CSS measurement (the W8R 24-row broadcast tuple + W6 retime). As the
bench/measurement row, this artefact OWNS the cross-pass harness-convergence verdict
(X2): it names THE single canonical harness S-P2/S-P3 binds the gate consumer onto.
Output: this file.
Baseline: SK-V17-open (`6496fecae706c5ffb1b80b82ea5dcfa6f7ff0e33`, master HEAD;
the W0 baseline state — no CSS tape activation, CSS rides the fact-stream String).
Host triple: aarch64-apple-darwin; arch=aarch64; cpu=Apple M5 Max.
Build flags: `cargo build --release -p bbnf-bench --bin css_canon_bench`
(`[profile.release]` opt-level=3 + `debug = true` + `split-debuginfo = "packed"`,
`Cargo.toml:78-85`); no `-C target-cpu=native` (host-portable release; the prior
W11 JSON rows used target-cpu=native, the CSS W8 path did not — kept release-default
so the CSS number is not a native-CPU overfit). simd=Scalar (no SIMD on any CSS path,
verified §4).
Profile tool: samply 0.13.1 (`samply record --save-only -o <f>`); symbolication via
`atos -o <bin> -arch arm64 -l 0x100000000` against the packed dSYM
(`target/release/css_canon_bench.dSYM`). PMU via `proc_pid_rusage` V5
(`ri_cycles`/`ri_instructions`), the same path as `profile_direct.rs:55-72`.
Corpus coverage: 4/4 of the benched CSS set (`css_l4_corpus.rs:21-54`:
bootstrap, tailwindcss, material-components-web, animate). The 17 JSON corpora in
§2.1 are NOT in scope for the CSS-tape subject row; CSS is benched against its own
fixed 4-corpus set per SYNTHESIS §0.5. JSON 51/51 is the guard tripwire (§4), not a
P1-F measurement target.

**V4 fold log (CHALLENGE V3 dispositions resolved in this revision):**
- CHALLENGE V3 returned **42/42 = 100% ACCEPT, 0 REVISE, 0 REJECT** for the whole
  pass (CH3 `V3/CH3.md:324-326`), and **P1-F §3.2 + §4 = 9/9 ACCEPT**
  (`V3/CH3.md:231-256`). There is NO open REVISE or REJECT against this artefact to
  fold. V4 carries every V3 posture verbatim and re-grounds it on a FRESH measurement
  run (the profile-first discipline: re-verify, do not inherit blindly):
  - the X1' single c/B posture (instr/byte primary + load-bearing; cyc/byte co-reported
    with IPC explicit + NON-load-bearing because `ri_cycles` is non-disambiguable from
    the rusage interface — NOT because sub-1.0 CPI is impossible) is RE-CONFIRMED on a
    fresh V4 PMU run (§2.2.1): the 16 rows span CPI [0.158, 0.277] ⇒ IPC 3.6-6.3, and
    instr/byte reproduces V3 to <0.5% (bootstrap full 53.70=53.70; tailwind fact
    364.51 vs V3 363.76 = 0.2%; material fact 214.56 vs V3 214.54).
  - the row-2 wrapper cite (`css_canon_bench.rs:103-105 fn track1_full_parse`, with
    `:43` being the `RusageInfoV5` PMU struct) is grep-RE-VERIFIED this cycle
    (`43:struct RusageInfoV5`, `103:fn track1_full_parse`, `146:fn sample`).
  - the X2 single-canonical-harness verdict (`css_canon_bench.rs`, asserts N>=50 at
    `:250`, grep-RE-VERIFIED this cycle: `250: assert!(n >= 50, …)`) is unchanged.
  - the de-fact-stream-String honesty (`emit_fact_stream -> Result<String,…>`
    `generated.rs:5`, routed to the EXISTING `assembler.rs:71 push_plain_offset`,
    never a new builder type) RE-VERIFIED fresh: `5:pub fn emit_fact_stream`,
    `71: pub fn push_plain_offset`, `42:pub struct TapeBuilder`.
- One source-line refresh (no claim change): `push_ascii_lower_hex` opens at
  `generated.rs:628` this cycle (V3 cited `:625-634`); the fn body is `:628-634`,
  the `push_hex` call at `:633`. Symbol + class (FNV/hex diagnostic) unchanged.
- Within-harness >SOTA ratio stability: now demonstrated across THREE independent
  `css_canon_bench` runs (V1, V2/V3, V4) — full_parse BEATS lightningcss on all four
  corpora every run; fact_stream below on all four every run (§2.1.1).

**V3 fold log (CHALLENGE V2 dispositions resolved in the prior revision):**
- CH4-4 / CH6 REJECT / X1' (root of the V2 contradiction; p1f:299) — §2.2.1's
  "A retired-instruction CPI below 1.0 is physically impossible on M5" was WRONG PHYSICS
  and is the authority four sibling artefacts cited. STRUCK: §2.2.1 now states CPI
  0.16-0.28 ⇒ IPC 3.5-6.4 is PHYSICAL on the ~8-wide M5 P-core; cyc/byte is a valid
  counter that stays non-load-bearing because rusage `ri_cycles` is non-disambiguable
  (real-cycles vs wall-proportional tick), NOT because it is impossible. §2.2 posture
  and §4.5 anomaly re-worded to match. P1-D §3.1 reading adopted. instr/byte unaffected.
- CH5-V2-R1 / CH6 REVISE (p1f:317) — §2.3 row-2 wrapper line cite `css_canon_bench.rs:43`
  (the `RusageInfoV5` PMU struct) corrected to `:103-105` (the `track1_full_parse`
  wrapper fn, grep-verified `103: fn track1_full_parse`); the I2 non-conflation anchor
  now cites the right line. Symbol + %self were correct and are unchanged.
- X1' cross-artefact posture INVERSION resolved — the pass now carries ONE c/B reading
  (P1-D §3.1): instr/byte primary, cyc/byte co-reported with IPC explicit + non-load-bearing.

**V2 fold log (CHALLENGE V1 dispositions resolved in the prior revision):**
- CH6 / prompt REVISE (p1f:185) — `find_component_delim` hot-line attribution was
  transposed. CORRECTED in §2.3: `:295` is `delimiters.contains(&byte)` (the byte-
  membership scan leaf), `:293` is the `while pos < len` loop test, `:298` is
  `pos = match byte` (byte dispatch), `:307` is `_ => pos + 1`, `:294` is the byte
  load. Verified against source (`grep -n`, §1.3). The 59% self-time and the symbol
  (`CssFullParser::find_component_delim`) were correct and are unchanged.
- CROSS X2 (harness convergence) — this row now FORMALIZES the single canonical
  harness verdict (§1.1.1): `css_canon_bench.rs` is THE harness; the other four are
  declared superseded; a comparability caveat (absolute Mbps differ by
  harness/alloc/CPU-flag/host-noise, only within-harness ratios are load-bearing)
  is stated and demonstrated with a fresh re-run (§2.1.1).
- CROSS X1' (c/B posture, V3 — the ONE pass-wide posture) — instr/byte is adopted as
  the PRIMARY cost-density figure (§2.2); `ri_cycles`-derived cyc/byte is co-reported
  as a VALID counter whose sub-1.0 CPI is high IPC (3.6-6.4 on the ~8-wide M5 P-core),
  NOT a defect (§2.2.1). The V2 "physically impossible / falsified" characterization
  is WITHDRAWN (it was wrong physics; >3.5 IPC is exactly what a wide superscalar
  retires on a well-predicted scan loop). cyc/byte stays NON-LOAD-BEARING — not because
  it is impossible, but because `proc_pid_rusage.ri_cycles` cannot be disambiguated as
  dynamic core-cycles vs a wall-proportional scaled tick from this interface alone
  (P1-D §3.1 reading adopted; CH4-4). instr/byte is the sole grounded cost density.
- P1-B §4.3 consistency fold — `push_ascii_lower_hex` is annotated as an FNV/hex
  diagnostic emission with NO CSS-semantic value (§2.3, fact-stream plane note),
  matching P1-A §4.3 / P1-C A3 / P1-B §4.3.
- Aggregate-byte reconciliation (P1-C fold) — the 4-corpus raw sum is **979638**
  (`wc -c` verified §1.3); this row uses 979638 throughout (no aggregate Mbps is
  computed from a wrong denominator; the per-corpus medians are the load-bearing
  figures and are unaffected).

---

## §1 — Method (commands run; verbatim, reproducible)

### 1.1 The canonical harness (the formalized N>=50 replacement)

The W6 single-sample harness (`W6_SAMPLE_COUNT=1`) and the W8 broadcast harness
(`css_l4_w8.rs:217` — one timed loop over `total_bytes × 7 grammars × 8 iters`,
yielding ONE aggregate Mbps tuple) are statistically inadequate per SYNTHESIS §0.1
"Telemetry honesty (N>=50 fix)". The canonical harness landed for this profile is:

```
skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs   (403 lines; assert :250)
```

It takes **N cold per-parse samples** per corpus per workload, where each sample
times exactly one `parse(...)` call (`Instant::now()` … `elapsed()`, `sample()`
`css_canon_bench.rs:146-176`), black-boxes the result, and drops it — no warmed
cache, no amortised allocation, no cross-sample state. It reports **median / min /
max / stddev** in Mbps. N defaults to 200 and asserts `N >= 50` (the telemetry-
honesty gate, `css_canon_bench.rs:250`: `assert!(n >= 50, "N must be >= 50 (SK-V17
telemetry-honesty gate)")`, grep-re-verified this cycle). Mbps = `bytes * 8 /
(secs * 1e6)`.

Four workloads, each a real same-run comparator on the same input:
- `track1_full_parse` — `runtime::generated_css_l4_declaration_values::parser::parse_full`
  → `emit_full_parse` (`css_l4_declaration_values/generated.rs:61`); the
  `CssFullParser` structural full-parse the benched W8 real-corpus path times.
  Emits a 4-field summary (rules / at_rules / qualified_rules / declarations).
  Wrapper fn `track1_full_parse` at `css_canon_bench.rs:103` (grep-verified).
- `track1_fact_stream` — `…::parser::parse` → `emit_fact_stream` (`generated.rs:5`);
  the W2-W10 benched typed fact-stream String path (`track1_facts`,
  `nonjson_css_l4.rs:596-597`). Emits the per-declaration / per-token fact rows.
  Wrapper fn `track1_fact_stream` at `css_canon_bench.rs:108`.
- `lightningcss` — `StyleSheet::parse(input, ParserOptions::default())` +
  `black_box(sheet.rules.0.len())`; **full-CSSOM materialization**, the fair
  >SOTA bar (SYNTHESIS §0.6). NOT the criterion harness's `lightningcss_facts`
  (`nonjson_css_l4.rs:636`), which builds the CSSOM AND then walks it into a
  projection String — a conflated plane. The canonical harness times the CSSOM
  build only.
- `cssparser` — `StyleSheetParser` token-scan probe (mirrors
  `css_l4_w8.rs:275 cssparser_full_parse`); materializes nothing; the flaw probe.

Commands (verbatim, V4 run):
```
cd skinny
cargo build --release -p bbnf-bench --bin css_canon_bench
./target/release/css_canon_bench 200 > /tmp/skv17-p1/css_canon_n200_v4.txt
CSS_CANON_PMU=1 ./target/release/css_canon_bench 2000 > /tmp/skv17-p1/css_canon_pmu_v4.txt
```

The corpus loader (`css_l4_corpus.rs:62 load_all`) reads the four sha256-pinned
files from `skinny/corpora/css-l4-sk-v14/` (the gitignored `data/css` set is copied
into the build tree; the pinned corpora are committed under `corpora/`). `wc -c`
this cycle: animate 71750, bootstrap 232803, material-components-web 495454,
tailwindcss 179631; the `total` line includes the 1985-byte `manifest.md`, so the
load-bearing 4-corpus byte sum is **979638** (excludes manifest; §1.3).

#### 1.1.1 X2 — THE single canonical harness (cross-pass convergence verdict)

CHALLENGE V1 / CH6 §2 + CROSS §2 found **five** distinct "canonical N>=50"
harnesses authored across the six agents this pass:

| Harness source | Author(s) | asserts N>=50 | PMU mode | lines |
|---|---|:--:|:--:|---:|
| `css_canon_bench.rs` | P1-E, **P1-F** | **YES (:250)** | **YES** | 403 |
| `css_cold_harness.rs` | P1-A, P1-B | YES (:316) | YES | (12742 B) |
| `css_cold_bench.rs` | P1-C | **NO** (comment-only) | no | (11287 B) |
| `css_cold_canonical.rs` | P1-D | (not verified) | no | (8151 B) |
| `css_track1_profile.rs` | (profile feeder) | NO | no | (1292 B) |

Five harnesses cannot all be canonical; they yield ~20-29% same-plane Mbps
dispersion (e.g. tailwind `track1_fact_stream` reported 458-555 across the five),
which confounds host noise + allocator + N + harness shape. PASS-1-PROFILE §2.2 and
the orchestrator dispatch mandate ONE harness that S-P3 binds the
`--skv17-css-sota-report` gate consumer onto.

**Verdict (this row, as the bench/measurement authority): the single canonical
harness is `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`.** It is the only
harness that simultaneously (a) asserts the N>=50 gate (`:250`, grep-verified),
(b) carries the PMU instr/byte mode (`CSS_CANON_PMU`, the only reliable cost-density
source per X1), (c) carries the samply driver (`CSS_CANON_PROFILE`), and (d) is
cited with CORRECT line numbers by the two agents that name it (P1-F's `:250` is
correct; P1-E's `303`/`:150`/`:84-116` were fabricated-precision and are the
separate P1-E REJECT). The other four are **superseded** — S-P2/S-P3 cite
`css_canon_bench` numbers exclusively; the orchestrator may delete
`css_cold_harness.rs`, `css_cold_bench.rs`, `css_cold_canonical.rs`,
`css_track1_profile.rs` (kept this cycle only as the V1 evidentiary record).

**Comparability caveat (load-bearing, per X2):** absolute Mbps differ across
harnesses, allocator state, target-cpu flags, and run-to-run host scheduling noise
on the M5 Max. **Only within-harness, same-run ratios are load-bearing.** Every
>SOTA claim in this artefact is a same-run ratio (`track1 ÷ lightningcss` measured
in ONE `css_canon_bench` invocation on the SAME input bytes); the absolute median is
reported for completeness but is NOT the gate signal. §2.1.1 demonstrates the
within-harness ratio stability across THREE independent `css_canon_bench` runs.

### 1.2 samply hot-leaf attribution (cold-loop driver)

The harness exposes a `CSS_CANON_PROFILE=<workload>` driver: a tight loop of one
workload over all four corpora, for samply attribution (no statistics; the
flame-profile feeder). Each parse is independent (output dropped); the loop is the
cold-per-parse shape repeated for sample density.

```
CSS_CANON_PROFILE=track1_fact_stream samply record --save-only -o /tmp/skv17-p1/fact_stream.json.gz -- ./target/release/css_canon_bench 800
CSS_CANON_PROFILE=track1_full_parse  samply record --save-only -o /tmp/skv17-p1/full_parse.json.gz  -- ./target/release/css_canon_bench 1500
CSS_CANON_PROFILE=lightningcss       samply record --save-only -o /tmp/skv17-p1/lightningcss.json.gz -- ./target/release/css_canon_bench 1500
python3 /tmp/skv17-p1/symbolicate.py /tmp/skv17-p1/<plane>.json.gz <symbol-table.json>
```

samply `--save-only` emits the Firefox-profiler JSON; symbol *names* were emitted
as raw RVAs (the `samply-symbol-resolution` feedback — `--save-only` defers
symbolication), so leaf frames were resolved post-hoc by `atos` against the packed
dSYM with load address `0x100000000` (the arm64 `__TEXT` vmaddr; `frameTable.address`
is the RVA). Resolution verified: `0x215848 → emit_fact_stream (generated.rs:5)`,
`0x215dc0 → push_ascii_lower_hex (generated.rs:628)`. Self-time = leaf-frame count
per sample (the `selftime`/`symbolicate.py` rollup attributes the leaf of each
sample stack and the per-lib resource mapping). The flame artefacts on disk this
cycle: `/tmp/skv17-p1/{fact_stream,full_parse,lightningcss}.json.gz` (+ the
per-corpus `fact-bootstrap`, `full-tailwind`, etc. splits; §5).

### 1.3 source verification (the line cites + the byte aggregate; re-run this cycle)

`grep -n` against `crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
(verbatim V4 cycle):
```
5:pub fn emit_fact_stream(input: &str) -> Result<String, CssFactError>   ← de-fact-stream target
61:pub fn emit_full_parse(input: &str) -> Result<String, CssFactError>   ← recognition plane
288:    fn find_component_delim(
293:        while pos < self.bytes.len() {        ← loop test
294:            let byte = self.bytes[pos];        ← byte load
295:            if delimiters.contains(&byte) {    ← byte-membership SCAN leaf
298:            pos = match byte {                 ← byte dispatch
307:                _ => pos + 1,                  ← advance
320:    fn consume_balanced_at(&self, start: usize, close: u8) -> Result<…
628:fn push_ascii_lower_hex(out: &mut String, text: &str)  ← FNV/hex diagnostic (push_hex at :633)
```
`css_canon_bench.rs` (verbatim V4 cycle): `43:struct RusageInfoV5`,
`103:fn track1_full_parse`, `108:fn track1_fact_stream`, `146:fn sample`,
`250: assert!(n >= 50, …)`. `assembler.rs`: `42:pub struct TapeBuilder`,
`71: pub fn push_plain_offset`.
`wc -c` of the four pinned corpora: bootstrap 232803, tailwindcss 179631,
material-components-web 495454, animate 71750; **raw sum = 979638** (the aggregate
figure all six artefacts must share; the `wc -c total` line reads 981623 because it
folds the 1985-byte `manifest.md` — the load-bearing 4-corpus byte sum is 979638).

---

## §2 — Findings (per-corpus table; file:line on every hot-leaf claim)

### 2.1 Canonical N=200 cold per-parse medians (Mbps) — fresh V4 run

The load-bearing artefact. Source: `/tmp/skv17-p1/css_canon_n200_v4.txt`,
`css_canon_bench 200`, host Apple M5 Max, N=200 cold per-parse, statistic = median.
The V4 figures are shown (within host-noise of the V1/V2/V3 runs; see §2.1.1).

| Corpus | bytes | workload | median | min | max | stddev |
|---|---:|---|---:|---:|---:|---:|
| bootstrap | 232803 | track1_full_parse | **2272.923** | 1841.175 | 2308.314 | 96.413 |
| bootstrap | 232803 | track1_fact_stream | 851.021 | 742.976 | 890.313 | 19.302 |
| bootstrap | 232803 | **lightningcss (full-CSSOM)** | 1110.169 | 884.867 | 1131.886 | 35.372 |
| bootstrap | 232803 | cssparser (token-scan) | 2900.407 | 2020.897 | 2930.256 | 80.586 |
| tailwindcss | 179631 | track1_full_parse | **2576.509** | 2117.978 | 2793.552 | 128.086 |
| tailwindcss | 179631 | track1_fact_stream | 559.480 | 368.183 | 602.757 | 29.357 |
| tailwindcss | 179631 | **lightningcss (full-CSSOM)** | 833.786 | 444.230 | 863.416 | 74.675 |
| tailwindcss | 179631 | cssparser (token-scan) | 1731.253 | 1161.603 | 1786.911 | 100.744 |
| material-components-web | 495454 | track1_full_parse | **2590.116** | 2142.118 | 2861.138 | 142.758 |
| material-components-web | 495454 | track1_fact_stream | 874.902 | 576.983 | 939.537 | 46.606 |
| material-components-web | 495454 | **lightningcss (full-CSSOM)** | 1261.148 | 160.300 | 1320.019 | 193.598 |
| material-components-web | 495454 | cssparser (token-scan) | 3248.159 | 2853.928 | 3298.560 | 72.540 |
| animate | 71750 | track1_full_parse | **2493.164** | 1707.063 | 2542.163 | 146.039 |
| animate | 71750 | track1_fact_stream | 741.702 | 485.772 | 804.908 | 68.456 |
| animate | 71750 | **lightningcss (full-CSSOM)** | 1237.346 | 885.916 | 1258.888 | 29.406 |
| animate | 71750 | cssparser (token-scan) | 2643.127 | 1835.332 | 2669.246 | 67.191 |

(The material lightningcss min=160.300 is a single cold first-touch outlier — the
median 1261.148 and stddev 193.598 absorb it; the load-bearing figure is the median,
and the `no-warm-benches` discipline retains the cold outlier rather than discarding
it. §4.)

**Per-corpus delta-vs-lightningcss (median ratio), the >SOTA gate signal (same-run,
within-harness — the only load-bearing comparison per §1.1.1):**

| Corpus | track1_full_parse ÷ lightningcss | track1_fact_stream ÷ lightningcss |
|---|---:|---:|
| bootstrap | **2.05× (BEATS)** | 0.77× (below) |
| tailwindcss | **3.09× (BEATS)** | 0.67× (below) |
| material-components-web | **2.05× (BEATS)** | 0.69× (below) |
| animate | **2.01× (BEATS)** | 0.60× (below) |

#### 2.1.1 Within-harness ratio stability (X2 comparability demonstration — 3 runs)

The same `css_canon_bench` binary, run three times independently (V1 run
`css_canon_n200.txt`, V2/V3 run `css_canon_n200_v2.txt`, V4 run
`css_canon_n200_v4.txt`), shows absolute medians drift with host scheduling but the
load-bearing ratio holds:

| Corpus | full÷lcss (V1) | full÷lcss (V2/V3) | full÷lcss (V4) |
|---|---:|---:|---:|
| bootstrap | 2.12× | 2.25× | 2.05× |
| tailwindcss | 3.50× | 3.00× | 3.09× |
| material-components-web | 2.37× | 2.11× | 2.05× |
| animate | 2.06× | 1.97× | 2.01× |

Every corpus stays decisively > 1.0× across all three runs (full_parse BEATS
lightningcss on all four, every run); the fact_stream ratio stays decisively < 1.0×
on all four, every run. The absolute-Mbps drift (within the host-noise band the
§1.1.1 caveat names) does not flip any verdict. This is why the ratio, not the
absolute median, is the gate signal.

### 2.2 PMU instructions-per-byte (the SOLE reliable cost-density figure — X1)

Source: `/tmp/skv17-p1/css_canon_pmu_v4.txt`, `CSS_CANON_PMU=1 css_canon_bench 2000`,
`proc_pid_rusage` V5 (16 rows). instr/byte = `ri_instructions / (bytes × iters)`.

**X1' c/B posture (the single posture the whole pass adopts):** the
**instructions/byte** figure is the PRIMARY cost-density proxy — instruction
retirement is counted accurately and is reproducible to <0.5% across runs. The
`ri_cycles`-derived **cycles/byte is a VALID counter** (the V2 "physically impossible
/ falsified" framing is withdrawn — sub-1.0 CPI is high IPC, not a defect; §2.2.1),
but it stays **NON-LOAD-BEARING** because `proc_pid_rusage.ri_cycles` cannot be
disambiguated as dynamic core-cycles vs a wall-proportional scaled tick from this
interface alone (P1-D §3.1). This row, P1-D §3.1, and the consolidated pass all stand
on ONE posture: **cost density = instr/byte (primary); cyc/byte co-reported with IPC
explicit, non-load-bearing.** No artefact presents `ri_cycles` c/B as the authoritative
cost figure; instr/byte is the sole S-P2 input.

| Corpus | workload | instr/byte (primary) | raw cyc/byte (non-load-bearing) | Mbps (loop) |
|---|---|---:|---:|---:|
| bootstrap | track1_full_parse | 53.70 | 14.85 | 2300.1 |
| bootstrap | track1_fact_stream | 234.89 | 40.30 | 840.8 |
| bootstrap | lightningcss | 160.14 | 31.23 | 1096.9 |
| bootstrap | cssparser | 68.25 | 12.03 | 2843.0 |
| tailwindcss | track1_full_parse | 51.50 | 13.38 | 2564.8 |
| tailwindcss | track1_fact_stream | 364.51 | 61.64 | 556.5 |
| tailwindcss | lightningcss | 236.61 | 41.40 | 828.6 |
| tailwindcss | cssparser | 126.12 | 19.98 | 1717.9 |
| material-components-web | track1_full_parse | 46.46 | 12.85 | 2669.8 |
| material-components-web | track1_fact_stream | 214.56 | 39.50 | 863.0 |
| material-components-web | lightningcss | 137.63 | 27.73 | 1197.8 |
| material-components-web | cssparser | 60.86 | 10.76 | 3111.5 |
| animate | track1_full_parse | 57.72 | 14.40 | 2320.7 |
| animate | track1_fact_stream | 279.28 | 46.31 | 731.6 |
| animate | lightningcss | 155.33 | 28.18 | 1207.4 |
| animate | cssparser | 79.58 | 13.28 | 2558.8 |

The instr/byte ranks the planes cleanly: `track1_full_parse` is the **cheapest**
(46-58 i/B, below even cssparser's 61-126), `track1_fact_stream` is the
**most expensive** (215-365 i/B, ~4.4× the full-parse — the String-building tax),
lightningcss sits between (138-237 i/B). The fact-stream tax is the cost SK-V17's
tape activation removes. The V4 re-run reproduces every V3 instr/byte to <0.5%
(bootstrap full 53.70=53.70; tailwind fact 364.51 vs V3 363.76 = 0.2%; material fact
214.56 vs V3 214.54 = <0.01%; bootstrap fact 234.89 vs V3 234.04 = 0.36%),
confirming instruction retirement is the stable counter.

#### 2.2.1 ri_cycles: sub-1.0 CPI is high IPC (PHYSICAL), not impossible

The `ri_cycles`/`ri_instructions` ratio (CPI) on the rusage path, fresh V4 run:

| range across all 16 rows | V4 run | as IPC (1/CPI) |
|---|---|---|
| min CPI | 0.1584 (tailwind cssparser) | IPC 6.3 |
| max CPI | 0.2766 (material full_parse) | IPC 3.6 |

**Every one of the 16 rows reports CPI in [0.158, 0.277] — uniformly sub-1.0.**
The V2 characterization of this as "physically impossible" was WRONG PHYSICS and is
withdrawn (this row originated it; the correction is load-bearing). A sub-1.0
CPI is simply **IPC > 1**: CPI 0.158 ⇔ IPC 6.3, CPI 0.277 ⇔ IPC 3.6. IPC of 3.6-6.3
is entirely physical on the Apple M5 Max's ~8-wide out-of-order P-core for tight,
well-predicted load+compare+branch scan loops (`find_component_delim`) and for the
String-dense fact-stream emit; >3.5 instructions retired per cycle is exactly what a
wide superscalar delivers on branch-friendly code, not evidence of a broken counter.
The `ri_cycles / wall_seconds` derivation (P1-D §3.1) is a steady ~4.27 GHz across
every workload, consistent with a real fixed-frequency core-cycle counter.

**Why cyc/byte stays NON-LOAD-BEARING despite being a valid counter:** the steady-GHz
derivation is observationally identical for a true core-cycle counter and a
wall-proportional scaled tick (`wall_s` is itself derived from the loop Mbps, so
`ri_cycles/wall ≈ const` disambiguates neither model — e.g. on bootstrap the cyc/B
ratio fact/full = 40.30/14.85 = 2.714 tracks the 1/Mbps wall ratio 2300.1/840.8 =
2.736 to <0.8%). `proc_pid_rusage.ri_cycles` therefore cannot be disambiguated as
dynamic core-cycles vs a wall-proportional tick from this interface alone, so it is
non-load-bearing — NOT because it is impossible, but because it is non-disambiguable.
A true c/B (if S-P2 needs one) must come from `xctrace record --template
'CPU Counters'` or kperf, which read the architected PMU directly. The Mbps medians
(§2.1) are unaffected — they are wall-clock `Instant`, independent of the PMU counter.
The instr/byte figure (§2.2) is the sole grounded cost density.

### 2.3 Hot-leaf attribution — symbols resolved (the load-bearing CH1 obligation)

The flame artefacts on disk (`/tmp/skv17-p1/{full_parse,fact_stream,lightningcss}.json.gz`,
§5) are unchanged from V3 and the binary is byte-rebuilt from identical source; the
attribution below is artefact-backed and the symbols + %self are unchanged (the V3
CHALLENGE accepted them; the row-2 line cite fix to `:103-105` is retained).

**`track1_full_parse` plane** (`/tmp/skv17-p1/full_parse.json.gz`, 5684 leaf samples,
function rollup):

| % self | symbol | file:line | class |
|---:|---|---|---|
| **59.24** | `CssFullParser::find_component_delim` | `css_l4_declaration_values/generated.rs:288-311`; hot at **:295 `delimiters.contains(&byte)`** (the byte-membership scan leaf), `:293` loop test, `:294` byte load, `:298` `pos = match byte` dispatch, `:307` `_ => pos + 1` | **scan** (byte-at-a-time delimiter scan over `b";{}"` / `b"{};"` / `b":{};"`) |
| 26.74 | `css_canon_bench::track1_full_parse` wrapper (`String::len()` of the 4-field summary + `black_box` + the LTO-inlined outer driver loop — PURE measurement scaffold; maps to `emit_full_parse` `generated.rs:61` in prod) | `css_canon_bench.rs:103-105` | tape/structural (the summary emit + harness wrapper; NOT a retained/second pass — see §4.6) |
| 10.31 | `CssFullParser::consume_balanced_at` | `generated.rs:320-340` (hot at :327 `pos = match byte` dispatch; recursion OVER the same byte-membership inner loop as `find_component_delim`) | **scan** (recursive `()[]{}` balance) |
| 2.29 | `CssFullParser::parse_block` | `generated.rs:189-207` | structural |
| <1 | `parse_declaration` / `parse_at_rule` | `generated.rs:242,137` | structural |
| 0.12 | `[libsystem_malloc.dylib]` | — | tape/alloc (near-zero — full-parse is alloc-free) |

*(Row-2 line per CH5-V2-R1 fold: `css_canon_bench.rs:43` is the `RusageInfoV5` PMU
struct; the `track1_full_parse` wrapper fn is `:103-105` — `parse_full(input)` +
`black_box(out.len())`, grep-verified `103:fn track1_full_parse` this cycle.)*

The combined **scan leaves `find_component_delim` (59%) + `consume_balanced_at`
(10%) = ~69%** of in-binary self time. `consume_balanced_at` is structural recursion
OVER the same byte-class-membership scan primitive (`delimiters.contains` /
`byte == close` inner loop), so the two share ONE NEON target. This RE-CONFIRMS, on
the benched skinny tree, the architecture doc's core-tree figure
(`find_component_delim ~56%` / `consume_balanced_at ~10%`, SYNTHESIS NEON gate /
§0.3) — the NEON gate's "RE-PROFILE on the benched tape path first" obligation is
discharged: the figure holds (56→59%, 10→10%). The structural delimiter scan is the
dominant cost and is the grammar-neutral primitive a NEON structural pre-scan
targets. It is the SAME byte-class-membership primitive JSON runs through
`select_classifier` / `PrimitiveKernels` (P1-E verified `json/scan.rs:219`).

**`track1_fact_stream` plane** (`/tmp/skv17-p1/fact_stream.json.gz`, 9711 leaf
samples, function rollup):

| % self | symbol | file:line | class |
|---:|---|---|---|
| 32.60 | `[libsystem_kernel.dylib]` | — | tape/alloc (madvise/page-zero of the per-parse `String` grow+free) |
| 25.13 | `[libsystem_malloc.dylib]` | — | tape/alloc (`String` realloc/free per parse) |
| 25.01 | `emit_fact_stream` (inlines `emit_declarations` `generated.rs:411`; hot at :5 entry, `String::new()`/`push_str`) | `generated.rs:5,45,411` | **string** (per-decl/-token `push_str` + `to_string`) |
| 8.98 | `push_ascii_lower_hex` | `generated.rs:628-634` (hot :633 `push_hex`) | **string** (FNV/hex diagnostic encode — see note) |
| 6.71 | `[libsystem_platform.dylib]` | — | tape/alloc (`memcpy`/`memmove` for String growth) |
| 0.70 / 0.58 / 0.29 | `free` / `memcpy` / `malloc` (binary stubs) | — | tape/alloc |

Note on `push_ascii_lower_hex` (consistent with P1-A §4.3 / P1-B §4.3 / P1-C A3):
this leaf hex-encodes each ident/property lexeme into the fact-stream String for the
FNV/hex *diagnostic* row format. It carries **NO CSS-semantic value** — it is an
artefact of the fact-stream's hex serialization, not a parsing operation. Tape
activation removes it entirely (the tape stores a `ValueRef` byte-offset span, never
a hex re-encoding of the lexeme). It is a String-tax leaf, not a primitive to port.

The fact-stream plane is **~64% in system alloc/copy** (kernel 32.6 + malloc 25.1
+ platform 6.7) and **~34% in String emission** (`emit_fact_stream` 25.0 +
`push_ascii_lower_hex` 9.0). The reliable cost-density figure (instr/byte, §2.2)
puts this on a firmer footing than the wall-derived %self: fact_stream runs at
215-365 i/B vs full_parse's 46-58 i/B = **~4.4× the instructions per byte**, and
that 4.4× is the String-building + allocation tax. This is the
`emit_fact_stream`/`CSS_GENERATED_RS` String serialization cost SYNTHESIS §0.4
pre-blocks as a live admission plane — the tape append replaces it.

**`lightningcss` full-CSSOM plane** (`/tmp/skv17-p1/lightningcss.json.gz`, 13583 leaf
samples, function rollup — the fair-bar attribution, proving it genuinely
materializes):

| % self | symbol | class |
|---:|---|---|
| 15.06 | `[libsystem_malloc.dylib]` | alloc (CSSOM node allocation) |
| 14.50 | `cssparser::Parser::next_including_whitespace_and_comments` | scan/token |
| 8.92 | `cssparser::tokenizer::consume_name` | scan |
| 5.88 | `cssparser::Tokenizer::skip_whitespace` | scan |
| 5.36 | `cssparser::tokenizer::next_token` | scan |
| 5.04 | `parcel_selectors::parser::parse_selector` | structural (typed selector build) |
| 4.16 | `lightningcss::declaration::parse_declaration` | structural |
| 3.95 | `core::ptr::drop_in_place::<cssparser::Token>` | tape/alloc (drop) |
| 2.39 | `lightningcss::PropertyId::from_name_and_prefix` | structural (typed property) |
| 1.86 | `cssparser::tokenizer::consume_numeric` | number |
| … | `Property::parse`, `TokenList::parse_into`, `drop_in_place::<Property>` | structural/alloc |

lightningcss spends ~38% in the cssparser tokenizer + ~30% building+dropping typed
Property/Selector/CssRule nodes — confirming the comparator is a true full-CSSOM
build (not a token-scan), the correct >SOTA bar.

---

## §3 — Delta vs SK-V{N-1} (per row; Mbps + classification)

There are **zero ADMITTED per-corpus typed-CSS rows** in the prior iteration to
delta against (SYNTHESIS §0.2: the only CSS rows in `skinny/RESULTS.md` are the 24
W8R `direct_to_struct/main` broadcast diagnostics, lines 112-135, all
`not_admitted` / `AUDIT-FALSIFIED`, carrying ONE aggregate tuple). The delta is
therefore against the prior **aggregate** measurements, re-baselined per-corpus
here for the first time.

### 3.1 Delta vs the W8R broadcast tuple (the 24-row source)

Prior (SK-V16 / W8R, ONE aggregate tuple broadcast across all corpora ×
all 7 grammars × 8 iters, `css_l4_w8.rs:217`; `skinny/RESULTS.md` lines 112-135):
`track1_mbps=2319.041; cssparser_mbps=2362.037; lightningcss_mbps=929.281`.

| Plane | Prior (W8R aggregate) | This pass (N=200 per-corpus median range, V4) | Classification |
|---|---:|---|---|
| track1 (full-parse) | 2319.041 | 2273-2590 (matches; aggregate ≈ low end) | **A** (consistent; broadcast aggregate ≈ per-corpus median of the full-parse plane — the W8R "track1" WAS `parse_full`) |
| lightningcss (full-CSSOM) | 929.281 | 834-1261 | **A** (consistent; the aggregate 929 sits inside the per-corpus split) |
| cssparser (token-scan) | 2362.037 | 1731-3248 | **A** (consistent) |

The W8R aggregate tuple is faithfully reproduced per-corpus; the only defect of W8R
was the broadcast (one tuple, no per-corpus split, N effectively 1 timed loop). The
canonical harness retires the broadcast: 16 distinct per-corpus per-workload rows
with N=200 median/min/max/stddev.

### 3.2 Delta vs the W6 typed-retime + the "~70 Mbps / ~14×" narrative

The SK-V17 contract ground-truth (SYNTHESIS §0.2 / alphaB:112,166) carries a CSS
"~70 Mbps full corpus (51-164 per corpus), ~14× slower than lightningcss". **This
pass FALSIFIES that figure as the benched-track1 number** — and it must be flagged
(profile-first non-negotiable, ORCHESTRATOR §8: no hypothesis carries without a
fresh hot-leaf antecedent):

- alphaB itself tags every "~70" cell `[INF]` / `[AGG cited]` (inferred from the
  contract-supplied canonical N=100, NOT a fresh measurement; alphaB:139,166,169).
- The benched full-parse plane is **2273-2590 Mbps** (BEATS lightningcss 2.0-3.1×).
- The benched fact-stream plane is **559-875 Mbps** (below lightningcss, 0.60-0.77×).
- **Neither benched plane is ~70 Mbps.** The only ~3 Mbps figure in the evidence is
  the `css_l4_w6_typed_retime` EAGER typed-CSSOM plane (3.093 Mbps,
  `sk-v16-w6-speed-report.md:164`) — the AZ-IV eager-value-tree regression
  SYNTHESIS §0.4 PRE-BLOCKS. The "~70 / ~14×" appears to be a contract canonical
  that "No single committed measurement equals" (SYNTHESIS §0.2 verbatim re 974).

Classification of the "~70 Mbps / ~14× slower" hypothesis: **N-direct (unsupported
by a fresh benched-plane measurement)**. The honest benched truth is two-planed:
the recognition-only full-parse plane already BEATS lightningcss; the typed
fact-stream plane is below it by 0.60-0.77×; the eager-typed plane (3 Mbps) is
pre-blocked. SK-V17's task is to land a TYPED plane (preserve-rich-ast) at
full-parse-like speed via the tape — i.e. close the 4.4× instr/byte gap between
fact_stream (215-365 i/B) and full_parse (46-58 i/B) WITHOUT the eager-tree
regression.

| Outcome enum | Verdict |
|---|---|
| CSS full-parse plane vs lightningcss | **A (admit-shaped)** — already > bar; but it is recognition-only (4-field summary), NOT preserve-rich-ast, so it does not by itself discharge the SK-V17 typed gate |
| CSS fact-stream plane vs lightningcss | **L (loss)** — 0.60-0.77×, String-tax bound |
| CSS eager-typed plane | **K (pre-blocked)** — AZ-IV, SYNTHESIS §0.4 |
| "~70 Mbps / ~14×" narrative | **N-direct** — no fresh benched antecedent |

---

## §4 — Anomalies + masking signals (flagged for S-P2)

1. **The headline gap is plane-dependent, not a flat 14×.** The "CSS ~14× slower"
   framing collapses three distinct planes into one number. S-P2 must design against
   the SPECIFIC plane: the typed gate needs full-parse-class cost-density
   (46-58 i/B) at preserve-rich-ast fidelity. The full-parse plane proves the
   *scanner* is not the bottleneck for >SOTA; the fact-stream plane proves the
   *String emission* (4.4× the instr/byte) is. This is the empirical floor for the
   tape-activation lever.

2. **Fact-stream is 64% system alloc + 4.4× full-parse instr/byte.** 32.6% kernel +
   25.1% malloc + 6.7% platform on `track1_fact_stream` is `String` grow/realloc/free
   + page-zeroing per cold parse, and the reliable counter (instr/byte) puts the
   total tax at ~4.4× the full-parse plane. The tape append
   (`TapeBuilder::push_plain_offset`, `assembler.rs:71`, one branchless u32 write)
   removes the per-token `push_str`/`to_string`/hex-encode and the String allocation
   entirely. This is the single largest addressable cost and the direct antecedent
   for the SYNTHESIS §0.3 "Tape activation + builder seam flip" receiver. (NOT a
   re-proposal — it is the contract's own lever, grounded here.)

3. **`find_component_delim` 59% RE-CONFIRMS the NEON antecedent on the benched tree.**
   The SYNTHESIS NEON gate forbids inheriting the core-tree ~56% figure; this pass
   re-profiles it on the benched skinny path and it holds (59% / 10% balance). The
   leaf is a grammar-neutral byte-class delimiter scan (`delimiters.contains(&byte)`
   over `b";{}"`, `generated.rs:295`) — a `byte_class_index_64` / movemask structural
   pre-scan target (SYNTHESIS §0.3 NEON receiver). `consume_balanced_at` (10%) shares
   the same byte-membership inner loop (ONE NEON target). NEON is gated behind tape
   activation (no structural index to pre-scan into until the tape decodes CSS), so
   this is a flagged antecedent, not a route re-opening.

4. **`tape_activated = false` for CSS (verified fresh this cycle).** `grep -rln
   "TapeBuilder|ValueRef|PayloadArena|crate::tape" skinny/crates/runtime/src/grammars/
   css_l4_*/` returns ZERO; the JSON grammar (`grammars/json/parser.rs`) DOES reference
   the tape. The W6 "substrate landed but unwired for CSS" finding is confirmed
   empirically — the benched CSS planes touch no tape symbol. This is the close-gate
   baseline for the `tape_activated` telemetry column.

5. **PMU `ri_cycles` is a valid counter but NON-LOAD-BEARING on the rusage interface
   (CPI 0.158-0.277 = IPC 3.6-6.3), re-confirmed on the fresh V4 run.** The V2
   "physically impossible" framing this row originated is WITHDRAWN (wrong physics):
   sub-1.0 CPI is high IPC, entirely physical on the ~8-wide M5 P-core for
   well-predicted scan + String-emit loops; `ri_cycles/wall` is a steady ~4.27 GHz
   (P1-D §3.1), consistent with a real core-cycle counter. cyc/byte stays
   non-load-bearing only because `proc_pid_rusage.ri_cycles` cannot be disambiguated as
   dynamic core-cycles vs a wall-proportional scaled tick from this interface alone
   (the steady-GHz derivation is observationally identical for both models). The
   **instr/byte figure is the SOLE grounded cost-density figure** (the X1' posture this
   whole pass adopts); a directly-architected c/B (if `gate-json` needs one) must come
   from `xctrace`/perfmon, NOT this rusage path. This does not affect the Mbps medians
   (wall-clock `Instant`).

6. **No second substrate / no sidecar introduced (Lock 1 / CH5).** The canonical
   harness times the EXISTING benched parse fns (`parser::parse`, `parser::parse_full`)
   and the comparator crates; it introduces no retained cursor, no event vector, no
   parallel source pass. The 26.74% `track1_full_parse` wrapper bucket (§2.3) is the
   `String::len()` + `black_box` + LTO-inlined outer driver loop — PURE measurement
   scaffold, NOT a retained or second pass. The samply driver loop is a transient
   measurement scaffold, not a parser-owned structure. Track 1
   (`generated_css_l4_*`) and the comparators keep separate symbol paths (the §2.3
   tables are per-plane, never conflated).

7. **JSON 51/51 guard untouched.** This pass is read-only against `skinny/` source
   except the `css_canon_bench.rs` bin (a bench-only addition, no runtime path edit);
   the JSON parse path and the 51 JSON rows are not touched. The tripwire holds by
   construction (no source-path edit).

8. **X2 harness convergence (cross-pass).** Five "canonical" harnesses existed in
   V1; this row designates `css_canon_bench.rs` as THE single canonical harness
   (§1.1.1) and states the comparability caveat (only within-harness same-run ratios
   are load-bearing; absolute Mbps differ by harness/alloc/CPU-flag/host-noise,
   demonstrated §2.1.1 across THREE runs). S-P2/S-P3 cite this harness exclusively.

---

## §5 — Sources (every artefact path + run id)

Profile + data artefacts (under `/tmp/skv17-p1/`, not committed per §5 of the pass
contract; the harness binary is reproducible from source):

- `/tmp/skv17-p1/css_canon_n200_v4.txt` — V4 N=200 cold per-parse table (run id:
  `css_canon_bench 200`, host Apple M5 Max, 2026-05-29). §2.1 / §2.1.1 source.
- `/tmp/skv17-p1/css_canon_n200.txt`, `…_n200_v2.txt` — V1 / V2-V3 prior runs
  (§2.1.1 three-run ratio-stability demonstration).
- `/tmp/skv17-p1/css_canon_pmu_v4.txt` — V4 PMU instr/byte + raw cyc/byte + CPI
  (run id: `CSS_CANON_PMU=1 css_canon_bench 2000`, 16 rows). §2.2 / §2.2.1 source.
- `/tmp/skv17-p1/css_canon_pmu.txt`, `…_pmu_v2.txt` — prior PMU runs (instr/byte
  reproduced <0.5% in V4; §2.2).
- `/tmp/skv17-p1/fact_stream.json.gz` — samply track1_fact_stream flame profile
  (9711 samples; run id `CSS_CANON_PROFILE=track1_fact_stream … 800`). §2.3 source.
- `/tmp/skv17-p1/full_parse.json.gz` — samply track1_full_parse flame profile
  (5684 samples; run id `… track1_full_parse … 1500`). §2.3 source.
- `/tmp/skv17-p1/lightningcss.json.gz` — samply lightningcss full-CSSOM flame profile
  (13583 samples; run id `… lightningcss … 1500`). §2.3 source.
- `/tmp/skv17-p1/{fact,full}-{bootstrap,tailwind}.json.gz` — per-corpus flame splits
  on disk (corroborating the aggregate planes).
- `/tmp/skv17-p1/symbolicate.py` — the atos-symbolicating self-time extractor
  (Firefox-profiler format; load addr `0x100000000`; takes profile + symbol-table).

Source artefacts (committed by the orchestrator):
- `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` — THE canonical N>=50 cold
  harness + PMU mode + samply driver (403 lines; assert N>=50 at :250 grep-verified;
  `RusageInfoV5` :43; `track1_full_parse` :103; `track1_fact_stream` :108;
  `sample()` :146-176; the formalized W6/W8 replacement; the X2 single-harness designate).

Benched-surface citations (grep-verified this cycle):
- `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:21-54` — the fixed 4-corpus set;
  raw 4-corpus byte sum **979638** (`wc -c` verified §1.3; the `total` line reads
  981623 incl. the 1985-byte `manifest.md`).
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs:217` — the prior W8 broadcast loop.
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:596-597` — `track1_facts` →
  `track1::parser::parse`; `:636` — the conflated `lightningcss_facts` fact-stream.
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`:5
  (`emit_fact_stream`), :61 (`emit_full_parse`), :288-311 (`find_component_delim`;
  scan leaf at :295 `delimiters.contains`, loop test :293, dispatch :298, advance
  :307), :320-340 (`consume_balanced_at`), :411 (`emit_declarations`), :628-634
  (`push_ascii_lower_hex`, FNV/hex diagnostic; `push_hex` call :633).
- `skinny/crates/runtime/src/tape/assembler.rs:42,71` — `TapeBuilder` /
  `push_plain_offset` (the seam-flip target; branchless u32 push into `offsets`).
- `skinny/crates/runtime/src/grammars/json/scan.rs:219` — JSON byte-class-membership
  primitive (the same primitive class as `find_component_delim`; P1-E verified).

Prior-iteration references (delta baseline):
- `skinny/RESULTS.md` lines 112-135 — the 24 W8R broadcast diagnostic rows
  (track1=2319.041 / cssparser=2362.037 / lightningcss=929.281, one tuple).
- `restart/audit/skinny-impl-overfit/sk-v16-w6-speed-report.md:58,59,164` —
  cssparser 2476.472 / lightningcss 833.199 / track1 eager-typed retime 3.093.
- `restart/skinny/tranches/sk-v17/research/alpha/alphaB-competitor-deltas.md`:112,139,
  166,169 — the `[INF]`-tagged "~70 Mbps / ~14×" canonical (falsified §3.2).

Run environment: aarch64-apple-darwin, Apple M5 Max, rustc 1.96.0-nightly
(02c7f9bec 2026-04-10), samply 0.13.1, release profile (opt-level=3, debug=true,
no target-cpu=native), simd=Scalar, 2026-05-29.
