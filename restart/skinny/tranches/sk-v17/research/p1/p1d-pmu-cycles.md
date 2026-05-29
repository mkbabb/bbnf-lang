# SK-V17 P1-D: PMU + Cycles-Per-Byte + Canonical Cold CSS Bench Harness

Pass: S-P1 Profile. Cycle: V4.
Date: 2026-05-29.
Scope: Bench/measurement + PMU row for the SK-V17 CSS-tape subject. (1) Anchor on
the orchestrator-designated ONE canonical N>=50 cold per-parse harness
(`css_canon_bench.rs`, the only bin with `assert!(n>=50)` + PMU mode + samply
profile mode), retiring the W6 single-sample (`measure_mbps` 2000-iter amortised)
and W8 broadcast (`W8_PROFILE_ITERS=8`, one aggregate tuple) surfaces; report
MEDIAN/MIN/MAX/STDDEV Mbps per corpus for track1_full_parse / track1_fact_stream /
lightningcss / cssparser on the SAME run. (2) Deliver the PMU counters
(cycles, instructions, derived cycles/byte + instr/byte + IPC) that establish
the c/B baseline `gate-json` consumes — the row P1-F flagged P1-D to re-derive.
(3) samply-profile the BENCHED skinny CSS Track 1 scan and resolve every hot
leaf to a symbol + % self-time + file:line via atos line-level symbolication.
Output: this file.
Baseline: SK-V17-open (master HEAD `6496fecae706c5ffb1b80b82ea5dcfa6f7ff0e33`).
Host triple: aarch64-apple-darwin (Apple M5 Max, macOS 26.4.1 (25E253), arm64,
~8-wide P-core; `machdep.cpu.brand_string` "Apple M5 Max" — see §3.1 for the
cyc/byte counter posture).
Build flags: `cargo build --release` (profile.release: opt-level=3, lto="fat",
codegen-units=1, panic="abort", `debug=true`, strip=false) +
`RUSTFLAGS="-C target-cpu=native"`; mimalloc global allocator (matches the gate
bench plane).
Profile tool: PMU via `proc_pid_rusage(RUSAGE_INFO_V5)` `ri_cycles`/`ri_instructions`
(`css_canon_bench.rs:74-99`, `CSS_CANON_PMU=1`); cross-validated against
`xctrace record --template 'CPU Counters'` (xctrace 26.0) wall-time. samply
0.13.1 (`record --save-only`); symbol + file:line resolution via
`xcrun atos -o <release binary> -arch arm64 -l 0x100000000` against the binary's
`__TEXT` vmaddr `0x100000000` (the `--save-only` profile carries
`nativeSymbols.length=0` per the `samply-symbol-resolution` feedback, so atos
symbolicates post-hoc from the `debug=true` binary — the
`[optimized + debuginfo]` link confirms DWARF presence).
rustc 1.96.0-nightly (02c7f9bec 2026-04-10).
Comparator versions: lightningcss `=1.0.0-alpha.71` (default-features=false),
cssparser `0.34` (workspace pin).
Corpus coverage: 4/4 of the benched CSS L4 SK-V14 corpus set
(`skinny/crates/bbnf-bench/src/css_l4_corpus.rs:22-60`:
`{bootstrap-5.3.3, tailwindcss-0.2.0, material-components-web-14.0.0,
animate-4.1.1}`, per-corpus 232,803 / 179,631 / 495,454 / 71,750 B,
aggregate **979,638 B** — raw byte sum of the four `.min.css` files, verified by
`wc -c`). The 17-corpus JSON matrix of §2.1 is the JSON-guard agents' scope
(P1-A/B/C/E/F); P1-D's contract row is the CSS-tape bench/PMU row, so its corpus
universe is the four benched CSS corpora — `normalize` is NOT in the benched set
and is not measured (per SYNTHESIS §0.5).

---

## §1 — Method (commands run; verbatim, reproducible)

### §1.1 The retired harnesses (what "N>=50 fix" replaces)

Two inadequate measurement surfaces exist at baseline, both
statistically/structurally unfit per SYNTHESIS §0.1 "Telemetry honesty (N>=50)"
and the §0.4 broadcast pre-block:

1. **W6 amortised quick-measurement** —
   `nonjson_css_l4.rs:3093 measure_mbps`: 16 warmup iterations + a single
   timing window around **2000 amortised iterations** of the parse, dividing
   total elapsed by `input.len() * 2000`. This is ONE observation of a warm,
   amortised batch: no per-parse sample distribution, no median, no cold cache.
   Each `write_*_report_with_quick_measurement` (`:1048,:1169,:1319,…`) calls
   it once. CH6 rejects warm benches; this is warm.
2. **W8 corpus broadcast** — `css_l4_w8.rs:206 measure_full_parse_profiles` /
   `:262 time_loop`: a SINGLE `Instant` around one loop of
   `W8_PROFILE_ITERS=8` (`css_l4_w8.rs:18`) over **all 4 corpora × all 7
   `TRACK1_PROFILES`** (`css_l4_w8.rs:60-89`), then ONE aggregate Mbps tuple
   (`track1/lightningcss/cssparser`) divided across `total_bytes * 7 * 8`. This
   is the W8R broadcast (`W8_SELECTED_CSS_ROWS=24`, `css_l4_w8.rs:17`) that
   projects one timing tuple into 24 conceptual rows — the source of the 24
   falsified `css_l4/*/direct_to_struct/main` RESULTS rows (lines 112-135) and
   the §0.4 pre-block. It measures a single sample.

### §1.2 The ONE canonical N>=50 cold harness (orchestrator-designated)

Per CROSS X2 / CH6-CROSS the pass converges on a SINGLE canonical harness:
`skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` (registered
`crates/bbnf-bench/Cargo.toml:21`). It is the only CSS bench bin that carries
all three of: (a) the `assert!(n >= 50, "N must be >= 50")` telemetry-honesty
gate (`css_canon_bench.rs:250`); (b) a PMU mode (`CSS_CANON_PMU`,
`:211-247`); (c) a samply profile-driver mode (`CSS_CANON_PROFILE`, `:183-208`).
The other four CSS-bench bins authored across this pass — `css_cold_harness`,
`css_cold_bench`, `css_cold_canonical`, `css_track1_profile` — are SUPERSEDED for
measurement; their absolute Mbps differs from `css_canon_bench` by harness/
allocator/CPU-flag/N (the same-plane dispersion CROSS X2 names), so only
`css_canon_bench` numbers are load-bearing here. `css_track1_profile` is retained
ONLY as the samply attribution driver (§1.4), not as a measurement source.

Discipline of `css_canon_bench` cold mode (`sample`, `:146-177`):

- **N >= 50 cold samples per corpus per workload**; this run uses N=64
  (CLI arg; `assert!(n>=50)` at `:250`).
- **One sample == one parse**, timed with an individual `Instant::now()` /
  `.elapsed()` around a single `parse(black_box(input))` call (`:154-156`). NO
  amortising inner loop, NO warm cache. The only warmup is a one-shot
  `black_box(parse(input))` touch outside the timed window (`:152`) so first-touch
  page faults do not dominate; each parse still allocates cold and reuses no
  parser state (output dropped via `black_box`).
- **Mbps = bytes * 8 / (secs * 1e6)** (Mbit/s; `mbps`, `:138-143`),
  corpus-byte-accurate per corpus (NOT a broadcast aggregate).
- Per corpus per workload: `median / min / max / stddev` (`:160-176`). Median
  (not mean, not single-sample) per the §Section-2 `css_sample_statistic` binding.
- **Four workloads on the SAME run** (`WORKLOADS`, `:123-128`):
  `track1_full_parse` = `runtime::generated_css_l4_declaration_values::parser::
  parse_full` (= `emit_full_parse`, `generated.rs:61`); `track1_fact_stream` =
  `…::parser::parse` (= `emit_fact_stream`, `generated.rs:5`, the entry the
  contract cites at `nonjson_css_l4.rs:596`); `lightningcss` =
  `StyleSheet::parse(input, ParserOptions::default())` (eager typed CSSOM);
  `cssparser` = full token scan (`cssparser_full_parse`, materialises nothing).

Build + run (verbatim, single invocation, sequential, quiescent host):

```
cd skinny
RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench \
    --bin css_canon_bench --bin css_track1_profile
./target/release/css_canon_bench 64 > /tmp/skv17-p1d-v2-cold64.txt        # run 1
./target/release/css_canon_bench 64 > /tmp/skv17-p1d-v2-cold64-run2.txt   # run 2 (stability)
```

(Data fixtures gitignored; `corpora/css-l4-sk-v14/*.min.css` resolve from the
in-tree `corpus_dir()` = `skinny/corpora/css-l4-sk-v14/`, `css_l4_corpus.rs:56-60`.)

### §1.3 PMU mode (the c/B baseline `gate-json` consumes)

```
cd skinny
CSS_CANON_PMU=1 ./target/release/css_canon_bench 2000 > /tmp/skv17-p1d-v2-pmu.txt
```

`css_canon_bench.rs:211-247`: per corpus × workload, read `proc_pid_rusage`
`RUSAGE_INFO_V5` (`read_rusage_v5`, `:84-98`) before/after a 2000-iter loop,
report `cycles` (`ri_cycles` delta), `instructions` (`ri_instructions` delta),
`cycles_per_byte = cyc / (bytes*iters)`, `cpi = cyc/ins`, and loop `mbps`. This
is the same rusage path as `profile_direct.rs:55-72` (the JSON `gate-json` c/B
source), so the CSS c/B and the JSON c/B are produced by ONE counter interface.

Wall-time cross-check (xctrace; records the same wall duration the loop reports,
NOT a disambiguation of `ri_cycles` as core-cycles vs scaled tick — see §3.1):

```
cd skinny
xcrun xctrace record --template 'CPU Counters' \
    --output /tmp/skv17-p1d/xc-test.trace --launch -- ./target/release/css_canon_bench 60
```

### §1.4 samply hot-leaf profile of the benched track1 scan (atos line-resolved)

```
cd skinny
samply record --save-only -o /tmp/skv17-p1d/track1-v2.json.gz -- \
    ./target/release/css_track1_profile 6000
# leaf-frame address histogram -> absolute (offset + 0x100000000) -> atos:
xcrun atos -o target/release/css_track1_profile -arch arm64 -l 0x100000000 \
    <abs-leaf-addrs...>  > /tmp/skv17-p1d/atos_v2.txt
```

`css_track1_profile.rs:29-34` loops `parse_full` over all 4 corpora (iters=6000).
The leaf-frame `address` per sample is read from the Firefox-profiler
`frameTable.address` keyed by `samples.stack -> stackTable.frame`; each relative
address is offset by the `__TEXT` vmaddr `0x100000000` (`otool -l` confirms) and
passed to atos. **Resolution artefact `/tmp/skv17-p1d/atos_v2.txt` is 199 lines
of `<symbol> (file:line)` (NON-empty, resolving the V1 0-byte
`atos_out.txt` paper-close).** Flame profile: `/tmp/skv17-p1d/track1-v2.json.gz`
(160,737 B, **20,377 leaf samples**). Run id `skv17-p1d-v2-track1` 2026-05-29.

---

## §2 — Findings (per-corpus table; file:line on every hot-leaf claim)

### §2.1 Canonical cold per-corpus median/min/max/stddev (`css_canon_bench` N=64, run 1)

N=64 cold samples/parse, mimalloc, aarch64-apple-darwin, quiescent host.
Mbps = Mbit/s. Source: `/tmp/skv17-p1d-v2-cold64.txt`.

| corpus | bytes | workload | plane | median | min | max | stddev |
|---|---:|---|---|---:|---:|---:|---:|
| bootstrap | 232,803 | track1_full_parse | recognition+count | **2069.51** | 1199.57 | 2387.98 | 287.02 |
| bootstrap | | track1_fact_stream | fact-stream String | 833.13 | 727.98 | 867.89 | 26.24 |
| bootstrap | | lightningcss | full-CSSOM | **1093.69** | 953.30 | 1166.63 | 39.77 |
| bootstrap | | cssparser | token-scan | 2866.46 | 2103.84 | 2970.37 | 123.29 |
| tailwindcss | 179,631 | track1_full_parse | recognition+count | **2736.71** | 2253.75 | 3002.19 | 168.00 |
| tailwindcss | | track1_fact_stream | fact-stream String | 554.35 | 486.63 | 583.73 | 16.94 |
| tailwindcss | | lightningcss | full-CSSOM | **833.53** | 627.50 | 874.43 | 35.21 |
| tailwindcss | | cssparser | token-scan | 1717.46 | 1380.95 | 1769.76 | 78.68 |
| material-components-web | 495,454 | track1_full_parse | recognition+count | **2667.30** | 2193.03 | 2960.51 | 161.06 |
| material-components-web | | track1_fact_stream | fact-stream String | 859.83 | 787.91 | 895.10 | 20.07 |
| material-components-web | | lightningcss | full-CSSOM | **1258.95** | 1080.11 | 1326.35 | 51.29 |
| material-components-web | | cssparser | token-scan | 3260.52 | 2910.87 | 3343.43 | 87.41 |
| animate | 71,750 | track1_full_parse | recognition+count | **2499.73** | 1396.31 | 2718.23 | 266.07 |
| animate | | track1_fact_stream | fact-stream String | 743.73 | 603.10 | 785.27 | 32.64 |
| animate | | lightningcss | full-CSSOM | **1224.15** | 803.22 | 1280.65 | 61.66 |
| animate | | cssparser | token-scan | 2613.05 | 1603.91 | 2818.33 | 148.16 |

Per-corpus delta (track1_full_parse median / lightningcss median): bootstrap
1.892x, tailwindcss 3.283x, material 2.119x, animate 2.042x.

**HARNESS-COMPARABILITY CAVEAT (CROSS X2).** These are the load-bearing numbers
because they come from the orchestrator-designated canonical harness
`css_canon_bench`. Absolute Mbps differs across the five harness bins authored
this pass (by allocator, `target-cpu=native`, N, and CPU thermal/contention
state) by ~20-29% on the same plane; ONLY the **within-harness ratio**
(track1/lightningcss) is load-bearing for the >SOTA narrative, and that ratio is
stable to ~0.1x across harnesses and runs. Do not compare an absolute Mbps from
this table to an absolute Mbps from `css_cold_harness`/`css_cold_bench`/etc.

### §2.2 Stability (run 2, same harness, quiescent)

Source `/tmp/skv17-p1d-v2-cold64-run2.txt` (track1_full_parse / lightningcss):

| corpus | track1 median | lightningcss median | delta |
|---|---:|---:|---:|
| bootstrap | 2059.31 | 1082.99 | 1.901x |
| tailwindcss | 2711.31 | 823.85 | 3.291x |
| material-components-web | 2731.54 | 1266.05 | 2.158x |
| animate | 2518.23 | 1219.65 | 2.065x |

Medians stable run-to-run (<3.5% on track1 median, <1.5% on lightningcss); the
delta ratios are stable to within 0.04x. The elevated min / stddev on bootstrap
+ animate track1 (single-digit outlier samples) is per-sample cold-cache spread
the canonical harness exposes through the distribution — NOT a harness defect;
the W6/W8 single-sample harnesses hid exactly this.

### §2.3 CRITICAL plane caveat — track1 "beats" lightningcss is WRONG-PLANE today

track1 `emit_full_parse` measures **1.9-3.3x faster than lightningcss
full-CSSOM** on every corpus. This is NOT a >SOTA result; it is the
plane-mismatch the contract names (SYNTHESIS §0.6, §0.4 broadcast pre-block):

- **track1 `emit_full_parse` is recognition+count only.** It runs
  `CssFullParser::parse_stylesheet` (`generated.rs:118`) — a structural scan that
  counts `rules/at_rules/qualified_rules/declarations` — then emits a fixed
  ~5-line summary String (`generated.rs:91-99`:
  `full_parse\tstatus=accepted\trules=…\tdeclarations=…`). It builds NO typed
  CSSOM: no `CssColor`, no `CssDimension`, no per-declaration value tree. It is
  structurally the A-series "recognition-only" plane.
- **`track1_fact_stream` (= `parser::parse` = `emit_fact_stream`,
  `generated.rs:5`) is NOT metadata-only and IS a full per-declaration byte
  scan.** [CORRECTED from V1 §2.3, which wrongly stated it "does not even run the
  scan" / "is metadata-only".] At `generated.rs:45` `emit_fact_stream` calls
  `emit_declarations(input, &mut out)` (`generated.rs:411`), which walks the
  WHOLE source emitting per-declaration facts as a `String`. Its PMU instr/byte
  is **234-370 i/B** (§3.2) — the **most expensive of the four planes**, ~4.4-7x
  `track1_full_parse`'s 46-58 i/B and ~1.5-2.6x lightningcss. The extra work over
  full_parse is the per-declaration `push_str`/hex/`fnv64` String building. It is
  the fact-stream String tax SK-V17's tape activation removes — a genuine forward
  cost concern, not a free metadata emit.
- **lightningcss `StyleSheet::parse` builds the full typed CSSOM** (eager typed
  properties, the materializing comparator, the fair >SOTA bar).

Therefore the measured 1.9-3.3x is "recognition+count vs full-CSSOM" — a wrong
plane. The honest typed-CSSOM plane figure for track1 is the prior
`css_l4_w6_typed_retime` ~3.09 Mbps (`sk-v16-w6-speed-report.md:164`), which
materializes the typed tree eagerly and is ~270-350x SLOWER than lightningcss.
**The SK-V17 >SOTA bar is: track1 building a typed CSSOM (via lazy `ValueRef`
projection over the tape) must beat lightningcss full-CSSOM — and today there is
no benched typed track1 path.** The canonical harness is the apparatus; Wave 0
must wire a typed track1 entry (tape-activated, lazy-view-projected) into a new
workload slot before its median is an admissible >SOTA number. Until then the
`css_comparator_plane` for the track1 row is `recognition+count`, NOT
`typed-direct`, and the row is NOT admissible (SYNTHESIS Section-2 gate:
`css_typed_summary_equal` + `css_rich_ast_preserved` +
`css_comparator_plane==full-cssom`).

### §2.4 Hot-leaf attribution — benched track1 scan (samply + atos, `/tmp/skv17-p1d/track1-v2.json.gz`)

20,377 leaf samples over `parse_full` × 4 corpora × 6000 iters. Self-time by
atos-resolved symbol (`/tmp/skv17-p1d/atos_v2.txt`, 199 resolved addresses):

| self% (all) | self% (parse-only) | symbol | file:line | class |
|---:|---:|---|---|---|
| 57.40 | **79.64** | `CssFullParser::find_component_delim` | `css_l4_declaration_values/generated.rs:288-311` | scan |
| 11.33 | **15.72** | `CssFullParser::consume_balanced_at` | `generated.rs:320-340` | scan |
| 2.28 | 3.17 | `CssFullParser::parse_block` | `generated.rs:189-207` | structural |
| 0.91 | 1.27 | `CssFullParser::parse_declaration` | `generated.rs:242-261` | structural |
| 0.09 | 0.13 | `CssFullParser::parse_at_rule` | `generated.rs:137` | structural |
| 27.92 | — | `main` (harness corpus-iter + `parse_full().len()` + black_box) | `css_track1_profile.rs:31` | harness (excluded from parse-only) |
| ~0.05 | ~0.07 | `core::str::count::do_count_chars` + `alloc::raw_vec` + `mi_free` + fmt | — | tape/alloc |

**parse-only (excl. the `main` harness loop, 14,687 samples):
`find_component_delim` (79.64%) + `consume_balanced_at` (15.72%) = 95.36% of
parse self-time.**

Per-line within `find_component_delim` (% of ALL 20,377 samples; atos-resolved):

| self% | file:line | source | what it does |
|---:|---|---|---|
| 27.88 | `generated.rs:298` | `pos = match byte { … }` | per-byte dispatch on the current byte (string/comment/bracket/advance) |
| 17.24 | `generated.rs:295` | `if delimiters.contains(&byte)` | **linear membership test of the byte against the 2-4 element delimiter slice, every byte** |
| 4.74 | `generated.rs:294` | `let byte = self.bytes[pos]` | bounds-checked load |
| 2.99 | `generated.rs:307` | `_ => pos + 1` | the common-case single-byte advance |
| 2.42 | `generated.rs:296` | `return Ok(Some((byte, pos)))` | delimiter-hit return |
| 1.13 | `generated.rs:311` | `Ok(None)` / loop tail | |
| 0.75 | `generated.rs:288` | fn entry/frame | |

Per-line within `consume_balanced_at` (% of ALL samples):

| self% | file:line | source | what it does |
|---:|---|---|---|
| 7.84 | `generated.rs:327` | `pos = match byte { … }` | per-byte dispatch in the balanced-block scan |
| 1.30 | `generated.rs:336` | `_ => pos + 1` | single-byte advance |
| 0.88 | `generated.rs:323` | `let byte = self.bytes[pos]` | bounds-checked load |
| 0.42 | `generated.rs:340` | unclosed-block error tail | |
| 0.35 | `generated.rs:325` | `return Ok(pos + 1)` | close-delim hit |

The hot leaf is a **scalar, byte-at-a-time delimiter scan** with a per-byte
`slice::contains` linear membership probe (`:295`) and a per-byte match dispatch
(`:298`). This is precisely the structural pre-scan a NEON `byte_class_index_64` +
movemask cascade (`bbnf-simd/src/dispatch.rs:42 select_classifier`) replaces:
build a `u64` class bitmap per 64-byte block, then `bitmap_next_set_bit` to the
next delimiter, eliminating the per-byte branch and the `contains` probe.

### §2.5 Structural inefficiency — redundant overlapping re-scan

`parse_block_item` (`generated.rs:209`) scans a declaration body to its
terminator with `find_component_delim(self.pos, b"{};")` (`:211`), then
`find_colon_before(start, end)` (`:219`) **re-scans the same span from `start`**
via another `find_component_delim(start, b":{};")` (`:314`), then
`parse_declaration` (`:242`) scans the value span a **third** time with
`find_component_delim(colon+1, b";}")` (`:247`). Each declaration body is walked
2-3× by `find_component_delim`. This multiplies the dominant hot leaf and is a
named structural target for S-P2: tokenize-once over a NEON-produced structural
index rather than three overlapping scalar walks.

**REDRESS 51/53 boundary (citation discipline; CH3 §3).** The single-pass route
named above is admissible ONLY in the **REDRESS-53-admissible** shape
(`REDRESS.md:807-813`): "the scanner writes the tape/event stream and generated
lowering consumes that stream directly" — structural projection as the parser's
SINGLE substrate. It is **REDRESS-51/53-REJECTED** if implemented as a
parser-local SECOND scanner / retained `ParserState`-owned cursor over source
bytes that runs alongside the recursive-descent reader (`REDRESS.md:784-805,
807-813`: "A `ParserState`-owned structural cursor over source bytes is
non-canonical"; item 53 measured that shape as a regression). S-P1 proposes
nothing; this is flagged as the admissible-vs-rejected boundary S-P2 must respect
when it designs the tokenize-once primitive — the tape/event stream IS the
substrate, not a bolted-on second cursor.

---

## §3 — Delta vs SK-V{N-1} (per row; Mbps + c/B + classification)

### §3.1 PMU posture (CROSS X1' — the ONE c/B posture for the pass)

The c/B baseline `gate-json` consumes is established here from real
`proc_pid_rusage` V5 counters (`/tmp/skv17-p1d-v2-pmu.txt`, re-verified on disk
this cycle: the bootstrap track1_full row reads `instructions=25010386205`,
`cycles=6737349921`, `cpi=0.2694` → instr/byte 53.72, matching §3.2 exactly).
The X1' posture below is carried forward UNCHANGED from V3: the ROOT split (the
c/B interpretation camps) is closed, every sibling adopts ONE verbatim posture
(instr/byte primary + load-bearing; cyc/byte RAW + non-load-bearing because
`ri_cycles` is non-disambiguable from the rusage interface, NOT because sub-1.0
CPI is impossible), and the prior "proven 4.27 GHz / supersedes A/B/F" over-claim
stays WITHDRAWN as circular. Two counters, two reliability tiers — and one
posture the whole pass adopts:

- **`ri_instructions` (instr/byte) — PRIMARY, the SOLE load-bearing cost
  density.** Instruction retirement is counted exactly; it is allocator-, clock-,
  and contention-independent, reliable to <0.5% run-to-run, so it is the
  load-bearing plane-ranking cost figure. Every conclusion in §3.2/§3.3/§4 rests
  on instr/byte and on it alone.
- **`ri_cycles` (cyc/byte) — reported RAW, NON-LOAD-BEARING.** The harness `cpi`
  column (`css_canon_bench.rs:241`) is literally `cyc/ins`, so the sub-1.0 values
  are **CPI = 1/IPC**; CPI 0.16 ⇔ **IPC 6.4**, CPI 0.27 ⇔ **IPC 3.7**. The V1
  consensus that some siblings still carry — that this is "falsified /
  physically-impossible" — is itself **wrong physics**: an IPC of 3.6-6.4 is
  entirely PHYSICAL on the Apple M5 Max's ~8-wide out-of-order core for a tight,
  well-predicted load+compare+branch scan loop (`find_component_delim`). CPI < 1.0
  is the EXPECTED signature of a wide superscalar on branch-friendly scan code,
  not evidence of a broken counter. So the sub-1.0 CPI is NOT impossible.

  **HOWEVER, this does not promote cyc/byte to a load-bearing figure.** The
  rusage `ri_cycles` value cannot be DISAMBIGUATED, from the
  `proc_pid_rusage(RUSAGE_INFO_V5)` interface alone, as dynamic core cycles
  versus a wall-proportional scaled tick. The "steady ~4.27 GHz across workloads"
  derivation is non-probative because it is circular: `ri_cycles / wall_s` is
  observationally IDENTICAL for a fixed-frequency real-cycle counter and for a
  wall-proportional tick, since both yield a constant ratio against wall time.
  Tested directly: bootstrap cyc/byte ratio fact/full = 2.836, while the wall-time
  (1/Mbps) ratio = 2.847 — cyc/byte tracks wall to <0.4%, which is consistent with
  BOTH models and disambiguates NEITHER (and `hw.tbfrequency` 24 MHz confirms a
  scaled reference clock exists on this platform). Therefore cyc/byte is reported
  RAW in §3.2 and no conclusion rests on it.

**Posture adopted (X1', verbatim — the ONE c/B posture for all six artefacts):**
> instr/byte (`ri_instructions`) is the sole load-bearing cost density and is
> reliable to <0.5%. The sub-1.0 CPI from `ri_cycles` is PHYSICAL (IPC 3.7-6.4 on
> the M5's ~8-wide core), NOT impossible; however `proc_pid_rusage.ri_cycles`
> cannot be disambiguated as dynamic core-cycles vs a wall-proportional scaled
> tick from the rusage interface alone, so cyc/byte is reported RAW and
> non-load-bearing. No conclusion rests on it.

This corrects two defective framings at once. It rebuts the "physically
impossible / CPI < 1.0 cannot happen" claim (A/B/C/F's false justification: it IS
physical, high IPC on a wide core). And it withdraws this artefact's prior V2
over-claim that `ri_cycles` is a "proven 4.27 GHz counter" superseding A/B/F — the
GHz derivation was circular, so "proven" was not earned and the unilateral
"supersedes" was a per-artefact assertion that the pass cannot make from one
section. The shared, correct conclusion all six already ground on — instr/byte
only — is unchanged; the V1 "~3× cycles" inference is correctly carried as the
4.4-7.1× instr/byte gap (§3.2). The V1 wall-derived nominal-4.0-GHz proxy (CH7) is
likewise withdrawn: cyc/byte is a raw rusage counter, not a clock estimate.

### §3.2 PMU table — instr/byte (load-bearing) + cyc/byte (RAW, non-load-bearing) (`css_canon_bench` CSS_CANON_PMU, iters=2000)

Source `/tmp/skv17-p1d-v2-pmu.txt`. instr/byte and cyc/byte over
`bytes*iters = bytes*2000`. The `cpi` column is the harness `cyc/ins`
(`css_canon_bench.rs:241`), i.e. `CPI = 1/IPC`; IPC is shown alongside so no
reader mis-reads sub-1.0 CPI as a defect. Per the §3.1 posture, **instr/byte is
the sole load-bearing column**; cyc/byte (and therefore IPC/CPI, both derived
from cyc) are reported RAW and non-load-bearing.

| corpus | workload | **instr/byte** (load-bearing) | cyc/byte (raw) | IPC=ins/cyc | CPI=cyc/ins | loop Mbps |
|---|---|---:|---:|---:|---:|---:|
| bootstrap | track1_full_parse | **53.72** | 14.47 | 3.71 | 0.269 | 2367.6 |
| bootstrap | track1_fact_stream | **237.48** | 41.03 | 5.79 | 0.173 | 831.7 |
| bootstrap | lightningcss | **160.28** | 31.69 | 5.06 | 0.198 | 1079.7 |
| bootstrap | cssparser | **68.26** | 12.02 | 5.68 | 0.176 | 2843.8 |
| tailwindcss | track1_full_parse | **51.60** | 13.41 | 3.85 | 0.260 | 2497.3 |
| tailwindcss | track1_fact_stream | **369.55** | 63.30 | 5.84 | 0.171 | 539.5 |
| tailwindcss | lightningcss | **235.61** | 41.48 | 5.68 | 0.176 | 825.1 |
| tailwindcss | cssparser | **126.71** | 19.89 | 6.37 | 0.157 | 1718.8 |
| material-components-web | track1_full_parse | **46.47** | 12.81 | 3.63 | 0.276 | 2676.2 |
| material-components-web | track1_fact_stream | **217.15** | 40.15 | 5.41 | 0.185 | 850.3 |
| material-components-web | lightningcss | **137.76** | 27.10 | 5.08 | 0.197 | 1261.6 |
| material-components-web | cssparser | **60.88** | 10.72 | 5.68 | 0.176 | 3169.2 |
| animate | track1_full_parse | **57.73** | 13.83 | 4.17 | 0.240 | 2474.9 |
| animate | track1_fact_stream | **282.45** | 46.45 | 6.08 | 0.164 | 732.0 |
| animate | lightningcss | **155.58** | 28.54 | 5.45 | 0.183 | 1187.8 |
| animate | cssparser | **79.60** | 13.20 | 6.03 | 0.166 | 2567.9 |

instr/byte ranks the planes cleanly and unambiguously (allocator/clock-
independent), and it is the figure every conclusion rests on:
`track1_full_parse` is the **cheapest** (46-58 i/B, below even cssparser's
61-127), `track1_fact_stream` is the **most expensive** (217-370 i/B, ~4.4-7x the
full-parse — the String-building tax), lightningcss sits between (138-236 i/B).
The fact-stream tax is the cost SK-V17's tape activation removes. The cyc/byte
column (raw, non-load-bearing per §3.1) is internally consistent — track1_full's
derived IPC 3.6-4.2 is lower than the ~5.4-6.4 of the other planes, i.e. the
scalar scan retires fewer instructions per cycle — but no plane-ranking
conclusion is drawn from it; the instr/byte ranking already carries the cost
story (the NEON lever both cuts instr/byte and would raise the per-cycle density).

### §3.3 Delta vs prior CSS references

There is **no prior per-corpus admitted typed-CSS row** to delta against: the
only CSS rows in `skinny/RESULTS.md` are the 24 falsified W8R broadcast
diagnostics (lines 112-135, `not_admitted:SK-V15-W0-broadcast-diagnostic`),
carrying one aggregate tuple `track1=2319.041 / cssparser=2362.037 /
lightningcss=929.281` — not per-corpus, not cold-N>=50, not median.

| Source | track1 Mbps | lightningcss Mbps | cssparser Mbps | plane | sample discipline |
|---|---:|---:|---:|---|---|
| W6 typed-retime (`w6-speed-report.md:164`) | 3.09 | 833.20 | 2476.47 | typed CSSOM (eager) | single amortised |
| W8R broadcast (`RESULTS.md:112-135`) | 2319.04 | 929.28 | 2362.04 | recognition broadcast | 1 tuple → 24 rows |
| W6 fact-stream profile (`w6tape-report.md:42-47`) | 13-70 | 61-793 | 151-2529 | fact-stream | single, run-dependent |
| **P1-D canonical (this pass, `css_canon_bench` N=64)** | **2069-2737 / corpus** | **834-1259 / corpus** | **1717-3261 / corpus** | **recognition+count (track1), full-CSSOM (lcss), token-scan (cssp)** | **N=64 cold, per-corpus median** |

P1-D is the FIRST per-corpus cold median split on the canonical harness.
Classification per corpus:
- track1 `emit_full_parse` (recognition+count plane): all 4 corpora **N-direct /
  NoGo for the >SOTA gate** — wrong plane (no typed CSSOM); the apparent
  >lightningcss margin is the plane-mismatch artefact, not an admit.
- track1 `emit_fact_stream` (fact-stream String plane): 554-860 Mbps median — the
  benched CSS entry the contract cites; **slower than lightningcss on 3/4 corpora**
  (834 vs 1094 bootstrap, 554 vs 834 tailwind, 860 vs 1259 material; faster only on
  animate 744 vs ... no — 744 < 1224, slower on all 4). The String-building tax
  (217-370 i/B) is what makes it the slow plane; tape activation is its remedy.
- lightningcss full-CSSOM re-baselined same-run: **bootstrap 1094 / tailwind 834
  / material 1259 / animate 1224 Mbps median** — these supersede the inferred
  alphaB endpoints (animate↔164, tailwind↔51, material↔60, marked `[INF]`),
  which were inferred from corpus character, not measured. This is the per-corpus
  lightningcss bar the §0.5 wave gates key on.

---

## §4 — Anomalies + masking signals (flagged for S-P2)

1. **MASKING — recognition-plane track1 beats full-CSSOM lightningcss by
   ~2-3.3x.** The dominant masking signal: the benched `track1_full_parse` path
   is fast *because it does almost nothing* (counts structure, emits a 5-field
   summary; 46-58 i/B). It masks the real >SOTA gap, which lives on the
   typed-CSSOM plane (track1 ~3 Mbps eager vs ~1100 Mbps lightningcss =
   ~270-350x). S-P2 must design the typed lazy `ValueRef` projection to land
   between these planes — recovering most of the recognition-plane speed while
   producing the typed CSSOM. The honest feasibility band (SYNTHESIS Section 3) is
   300-600 Mbps; the recognition ceiling here (~2070-2740) is the headroom, the
   eager floor (~3) is what lazy projection must NOT regress to.

2. **`find_component_delim` 79.6% / `consume_balanced_at` 15.7% = 95.4% of parse
   self-time** — RE-CONFIRMS the architecture-doc core-tree figures
   (`find_component_delim ~56%` / `consume_balanced_at ~10%`) ON THE BENCHED
   SKINNY PATH (`generated.rs:288,320`), discharging the SYNTHESIS NEON-gate
   "RE-PROFILE on the benched tape path first" obligation and the HANDOFF
   `S-P1-re-confirm-on-benched-path` tag. The NEON structural-index lever (W4) is
   profile-justified: a `byte_class_index_64` movemask over the
   `select_classifier` kernel (`bbnf-simd/src/dispatch.rs:42`) directly replaces
   the per-byte `:295 slice::contains` + `:298 match`. CSS is the non-JSON
   exercise grammar for the kernel (SYNTHESIS `simd_non_json_exercise`).

3. **CORRECTION to architecture-doc `emit_* ~34%` — on the recognition path the
   String emit is negligible (<0.1%).** On `emit_full_parse` the String emit is
   `do_count_chars` + `alloc::raw_vec` + `mi_free` + fmt combined < 0.1% of
   self-time; the cost is ~95% scan. The ~34% emit figure does NOT hold on the
   benched RECOGNITION path. **HOWEVER** the String-emit pre-block remains a
   correct forward concern, and §2.3's correction sharpens why: the
   `track1_fact_stream` plane (`emit_fact_stream`, the contract's cited entry)
   DOES pay the String tax in full — 217-370 i/B, the most expensive plane,
   ~4.4-7x `track1_full_parse`. So S-P2 should NOT over-index the String lever
   *on the recognition plane* (it is not today's recognition hot leaf), but the
   per-declaration `push_str`/hex/`fnv64` String building IS the dominant tax on
   the fact-stream plane and IS what tape activation removes. The forward concern
   is real; it just does not appear in the `parse_full` flame.

4. **Redundant 2-3× overlapping scan** (`parse_block_item:211` →
   `find_colon_before:219/:314` → `parse_declaration:247`) multiplies the
   dominant hot leaf. Tokenize-once over the structural index is a named S-P2
   candidate — **admissible ONLY as REDRESS-53's "scanner writes the tape/event
   stream, generated lowering consumes it directly" single-substrate shape
   (`REDRESS.md:807-813`); REJECTED as the REDRESS-51/53 parser-local second
   scanner / retained `ParserState` cursor over source bytes (`REDRESS.md:784-805`),
   which item 53 measured as a regression.** Flagged with the boundary, not
   re-opened.

5. **PMU instr/byte is now MEASURED (V1 gap closed); cyc/byte is RAW and
   non-load-bearing (X1' posture).** V1 had no PMU counters and a nominal-4.0 GHz
   wall estimate (CH6 REJECT, CH7 REVISE). This pass reads
   `ri_cycles`+`ri_instructions` via `proc_pid_rusage` V5 (§3.2). The ONE pass-wide
   c/B posture (§3.1, adopted verbatim by all six artefacts): instr/byte is the
   sole load-bearing cost density (reliable <0.5%); the sub-1.0 CPI is PHYSICAL
   (IPC 3.7-6.4 on the M5's ~8-wide core), NOT impossible — which corrects the
   "physically impossible / falsified" framing — but `proc_pid_rusage.ri_cycles`
   cannot be disambiguated as dynamic core-cycles vs a wall-proportional scaled
   tick from the rusage interface alone, so cyc/byte is reported RAW and
   non-load-bearing and no conclusion rests on it. The V2 "proven 4.27 GHz counter
   / supersedes A/B/F" over-claim is withdrawn (the GHz derivation was circular).
   No nominal-clock proxy remains.

6. **Per-line attribution is now ARTEFACT-BACKED (V1 gap closed).** V1's per-line
   %s had no resolving artefact (0-byte `atos_out.txt`, CH6 REJECT). This pass
   produces `/tmp/skv17-p1d/atos_v2.txt` (199 resolved `<symbol> (file:line)`
   lines), joined to the leaf-address histogram from the samply
   `frameTable.address` — every per-line % in §2.4 traces to a resolved address.

7. **No SIMD on the CSS path (confirmed, not anomaly).** The benched track1 scan
   (`find_component_delim`/`consume_balanced_at`) calls NO `select_classifier` /
   `PrimitiveKernels` / NEON symbol; the `bbnf-simd/src/dispatch.rs:42` kernels
   are JSON-wired only, and `digit_mac.rs:27 parse_4_digits_dotprod` (udot orphan)
   is never reached. Confirmed empirically — no `bbnf_simd` frame in 20,377
   samples. This is the SYNTHESIS "zero SIMD on the CSS path" starting state, the
   gated-behind-tape W4 lever's empirical antecedent; NOT a REDRESS-blocked
   re-proposal.

8. **Concurrency / harness-comparability (CROSS X2).** Five canonical-named bins
   on disk give ~20-29% same-plane Mbps dispersion; the pass converges on the ONE
   harness `css_canon_bench` (`assert!(n>=50)` + PMU + profile mode). Only the
   within-harness track1/lightningcss RATIO is load-bearing; absolute Mbps differ
   by allocator/CPU-flag/N. The gate run must be quiescent
   (`single-cargo-per-target`).

---

## §5 — Sources (every artefact path + run id)

- **Canonical harness (orchestrator-designated ONE):**
  `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` (registered
  `crates/bbnf-bench/Cargo.toml:21`): cold-N mode (`:179,249-277`, `assert!(n>=50)`
  `:250`), PMU mode (`:211-247`, `read_rusage_v5` `:84-98`), samply profile mode
  (`:183-208`). Cold run ids `skv17-p1d-v2-cold64`(run1)/`-run2` →
  `/tmp/skv17-p1d-v2-cold64.txt`, `/tmp/skv17-p1d-v2-cold64-run2.txt`. PMU run
  `/tmp/skv17-p1d-v2-pmu.txt`.
- **Superseded bins** (NOT measurement sources): `css_cold_harness`,
  `css_cold_bench`, `css_cold_canonical`, `css_track1_profile` (latter retained
  ONLY as samply driver).
- **samply profile driver:** `skinny/crates/bbnf-bench/src/bin/css_track1_profile.rs`.
  Flame profile `/tmp/skv17-p1d/track1-v2.json.gz` (160,737 B, 20,377 leaf
  samples), run id `skv17-p1d-v2-track1` 2026-05-29. Symbolication artefacts:
  `/tmp/skv17-p1d/leaf_addr_v2.txt` (relative leaf histogram),
  `/tmp/skv17-p1d/abs_counts_v2.txt` (absolute addr + count),
  `/tmp/skv17-p1d/atos_v2.txt` (199 resolved `<symbol> (file:line)` — the
  per-line resolution artefact).
- **xctrace wall-time cross-check:** `/tmp/skv17-p1d/xc-test.trace` (CPU Counters
  template, 2.298 s duration; records the same wall time the loop reports — a wall
  cross-check only, NOT a disambiguation of `ri_cycles` as core-cycles vs a
  wall-proportional scaled tick; cyc/byte stays non-load-bearing per §3.1).
- **Retired harnesses cited:** `nonjson_css_l4.rs:3093` (`measure_mbps`, W6
  amortised); `css_l4_w8.rs:18` (`W8_PROFILE_ITERS=8`), `:60-89`
  (`TRACK1_PROFILES`), `:206` (`measure_full_parse_profiles`), `:262`
  (`time_loop`).
- **Benched track1 entries + hot leaves:** `nonjson_css_l4.rs:596`
  (`track1_facts` → `track1::parser::parse` → `emit_fact_stream`);
  `css_l4_declaration_values/generated.rs:5` (`emit_fact_stream`), `:45`
  (`emit_declarations` call), `:61` (`emit_full_parse`), `:118`
  (`parse_stylesheet`), `:189` (`parse_block`), `:209` (`parse_block_item`,
  `:211` first scan), `:242` (`parse_declaration`, `:247` value scan), `:288`
  (`find_component_delim`, hot `:294,295,296,298,307`), `:313`
  (`find_colon_before`, `:314` re-scan), `:320` (`consume_balanced_at`, hot
  `:327,336`), `:411` (`emit_declarations`).
- **NEON entry confirmed unused on CSS path:** `bbnf-simd/src/dispatch.rs:42`
  (`select_classifier`), `:101` (`lo6_table_admissible`);
  `bbnf-simd/src/aarch64/digit_mac.rs:27` (`parse_4_digits_dotprod`, orphan).
- **REDRESS boundary:** `skinny/REDRESS.md:784-805` (item 53 parser-local cursor
  rejected), `:807-813` (admissible single-substrate route).
- **Corpus set:** `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:22-60`
  (`CSS_L4_SK_V14_CORPORA`), `:56-60` (`corpus_dir` =
  `skinny/corpora/css-l4-sk-v14/`); files `{bootstrap-5.3.3.min,
  tailwindcss-0.2.0.min, material-components-web-14.0.0.min,
  animate-4.1.1.min}.css` (232,803 / 179,631 / 495,454 / 71,750 B; aggregate
  **979,638 B** raw byte sum, `wc -c` verified).
- **Prior-number references:** `restart/audit/skinny-impl-overfit/
  sk-v16-w6-speed-report.md:58,164`; `sk-v16-w6tape-report.md:42-47`;
  `skinny/RESULTS.md:112-135` (W8R broadcast).
- **Host:** Apple M5 Max (`machdep.cpu.brand_string`), macOS 26.4.1 (25E253),
  aarch64, ~8-wide OoO P-core (the IPC 3.7-6.4 the sub-1.0 CPI implies is within
  issue width; see §3.1 posture). `hw.tbfrequency` 24 MHz (scaled reference clock
  exists, so `ri_cycles` is non-disambiguable, non-load-bearing). Toolchain: rustc
  1.96.0-nightly (02c7f9bec 2026-04-10), samply 0.13.1, xctrace 26.0, atos
  (Apple). Baseline HEAD `6496fecae`.
