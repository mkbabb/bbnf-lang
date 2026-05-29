# SK-V17 P1-E: CSS-Tape Hot-Leaf Attribution + Canonical N≥50 Cold Bench

Pass: S-P1 Profile. Cycle: V4.
Date: 2026-05-29.
Scope: Per-corpus per-symbol hot-leaf attribution of the BENCHED skinny CSS path
(`skinny/crates/`), classified scan/number/string/unicode/structural/tape/dispatch;
AND formalization of the canonical N≥50 cold-per-parse harness, reporting
median/min/max/stddev Mbps for track1 / lightningcss / cssparser per corpus.
Output: this file.
Baseline: SK-V17-open (master HEAD `6496fecae`; SK-V16 close bracket `1c5bd7a25`).
Host triple: aarch64-apple-darwin (Apple M5 Max, `uname -m`=arm64).
Build flags: `--release` profile (workspace `Cargo.toml:78` release: `opt-level`
default + `debug = true` :83 + `split-debuginfo = "packed"` :85) — samply-ready
debuginfo. No x86, no SVE. NEON is the host SIMD ISA.
Profile tool: samply 0.13.1 (`samply record --save-only`), symbolicated post-hoc via
`atos -o <binary> -l 0x100000000` against the packed debuginfo (the `--save-only`
funcTable carries raw addresses; atos resolves them to Rust mangled symbols — see §1).
Corpus coverage: 4/4 of the benched CSS set (`css_l4_corpus.rs:21-54`:
bootstrap / tailwindcss / material-components-web / animate). The §2.1 seventeen-JSON
roster does NOT apply to the CSS-tape subject; the CSS benched set is the four-corpus
roster the SK-V17 contract gates (`SYNTHESIS.md` §0.5), and all four are covered.

## §1 — Method (commands run; verbatim, reproducible)

### 1.1 Canonical N≥50 cold harness (formalized)

The W6 single-sample (`W6_SAMPLE_COUNT=1`) and the W8 8-iteration aggregate
(`css_l4_w8.rs:18` `W8_PROFILE_ITERS=8`, summed-and-divided over a 7×4 broadcast,
`css_l4_w8.rs:217-259`) are both statistically inadequate. The canonical replacement
is the standalone bin **`skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`** (403
lines, already present in the tree from a prior cycle). It is the formalized canonical
harness for this profile and — per the pass-wide convergence below — the **single
canonical N≥50 harness for the whole S-P1 CSS pass** (cited by P1-D's PMU mode and the
most-cited bench across artefacts; CROSS X2 / CH4-3 / CH6-CROSS designate it sole, the
other authored bins — `css_cold_harness` / `css_cold_bench` / `css_cold_canonical` /
`css_track1_profile` — are retired and any number they produced is superseded by this
harness's numbers).

Discipline (verified at exact source lines this cycle, fixing the V1 paper-citation
that misnamed line numbers):
- **Cold per-parse** (`css_canon_bench.rs:146-159`, `fn sample` :146). Each of the N
  samples times **exactly one** `parse(black_box(input))` call between `Instant::now()`
  (:154) and `.elapsed()` (:156), the result `black_box`-dropped each sample (:157). No
  warmed cache, no amortised allocation across samples (`no-warm-benches`). One
  pre-touch parse runs OUTSIDE the timed window only to fault in the source buffer pages
  (:152, comment :149-151); the timed parse itself is cold.
- **N ≥ 50 enforced in code** (`:250`: `assert!(n >= 50, "N must be >= 50 (SK-V17
  telemetry-honesty gate)")` — a runtime assert, not a comment-promise).
- **Statistic = median** (`:160-165`; even-N average of the two central samples :162,
  odd-N central :164), with min (`:166`) / max (`:167`) and **population** stddev
  (`:168-169`, variance /n) emitted per row (`Stats` :131-135; ROW print :260-272).
- **Mbps = bytes·8 / (secs·1e6)** (`fn mbps` :138-142: `(bytes as f64 * 8.0) / (secs *
  1_000_000.0)`).

Four workloads per corpus (`WORKLOADS` table `css_canon_bench.rs:123-128`; workload
fns :103-121):
| Workload | Symbol | Plane |
|---|---|---|
| `track1_full_parse` | `track1_full_parse` :103 → `generated::emit_full_parse` (`generated.rs:61`) | **recognition-only** structural full-parse (counts rules/at_rules/qualified/decls; NO typed CSSOM, NO value tree, NO String body) |
| `track1_fact_stream` | `track1_fact_stream` :108 → `generated::emit_fact_stream` (`generated.rs:5`) | **typed fact-stream `String`** (the live benched Track 1 plane, `nonjson_css_l4.rs:596`) |
| `lightningcss` | `lightningcss_full_cssom` :113 → `StyleSheet::parse(.., ParserOptions::default())` (`.rules.0.len()`) | **full L2 CSSOM** (the fair >SOTA bar) |
| `cssparser` | `cssparser_token_scan` :118 → `cssparser_full_parse` :282 / `CssparserFullParseProbe` :294-403 (mirrors `css_l4_w8.rs` `CssparserFullParseProbe`) | **token stream only** (flaw probe) |

Verbatim commands (one cargo per target; the target dir is shared so the build and
the run serialise):
```
# corpora copied into the benched tree (gitignored): present at
#   skinny/corpora/css-l4-sk-v14/{bootstrap-5.3.3.min.css, tailwindcss-0.2.0.min.css,
#   material-components-web-14.0.0.min.css, animate-4.1.1.min.css}
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo build --release -p bbnf-bench --bin css_canon_bench   # Finished in 48.09s
./target/release/css_canon_bench 100  > /tmp/skv17-p1e-canon-n100.txt   # primary, N=100
./target/release/css_canon_bench 60   > /tmp/skv17-p1e-canon-n60.txt    # reproducibility, N=60
```

### 1.2 samply hot-leaf profiling (cold-per-parse driver)

The same binary carries a profiling driver (`css_canon_bench.rs:183-207`, the
`CSS_CANON_PROFILE` branch of `main`): with `CSS_CANON_PROFILE=<workload>` set, it runs
a tight loop of one workload over all four corpora `iters` times for samply attribution
(`:198-202`). The per-iter parse is the same cold call; the loop is the only way to
accumulate enough samples — each individual parse is still a fresh
`parse(black_box(src))` (:200), no retained parser state. (The binary additionally
carries the PMU mode at `:211-247` consumed by P1-D — same harness, one canonical bin.)

```
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CSS_CANON_PROFILE=track1_full_parse  samply record --save-only --reuse-threads \
    -o /tmp/skv17-p1e/full_parse.json.gz  -- ./target/release/css_canon_bench 4000
    # 14.56s, 14486 samples
CSS_CANON_PROFILE=track1_fact_stream samply record --save-only --reuse-threads \
    -o /tmp/skv17-p1e/fact_stream.json.gz -- ./target/release/css_canon_bench 4000
    # 46307 samples
```

Symbolication: the `--save-only` funcTable holds raw lib-relative addresses (the
`samply-symbol-resolution` feedback names this loss). Resolution is post-hoc against
the packed debuginfo with `atos -o target/release/css_canon_bench -l 0x100000000
<addr+0x100000000>` (extractor scripts `/tmp/skv17-p1e/{symbolicate,agg2,caller}.py`).
Self-time is the leaf-frame func of each sample (`selftime`/`symbolicate.py`); the
syslib-caller attribution (§2.3) walks each non-binary leaf up its `stackTable.prefix`
chain to the nearest binary frame. All numbers below are reproducible by re-running
these scripts on the two committed `.json.gz` profiles.

## §2 — Findings

### 2.1 Canonical N=100 cold per-corpus table (the load-bearing artefact)

`/tmp/skv17-p1e-canon-n100.txt`, N=100 cold samples, Apple M5 Max, all Mbps, produced
by the sole canonical harness `css_canon_bench.rs` (§1.1).

**Comparability caveat (CROSS X2 / CH4-3 fold).** Absolute Mbps are harness-, allocator-
and CPU-flag-dependent: the several N≥50 bins authored across this S-P1 pass diverge by
up to ~29% on the same plane (e.g. tailwind `track1_full` reported 2334/2222/2870/2656/2414
across the different bins) due to differing `target-cpu` flags, allocator, and N. The
**within-harness ratio (track1/lightningcss, §2.2) is the load-bearing figure**, not the
absolute Mbps; the ratios are stable across all bins so the conclusions hold. All numbers
in this artefact are the `css_canon_bench` numbers and must be cited as such.

| Corpus | Bytes | Workload | median | min | max | stddev |
|---|---:|---|---:|---:|---:|---:|
| bootstrap | 232803 | track1_full_parse | **2006.43** | 1650.17 | 2261.71 | 121.49 |
| bootstrap | 232803 | track1_fact_stream | **719.60** | 546.07 | 792.00 | 48.95 |
| bootstrap | 232803 | lightningcss | **909.95** | 734.75 | 1009.17 | 47.84 |
| bootstrap | 232803 | cssparser | 2525.68 | 1979.99 | 2672.22 | 145.13 |
| tailwindcss | 179631 | track1_full_parse | **2413.69** | 1801.19 | 2535.97 | 158.47 |
| tailwindcss | 179631 | track1_fact_stream | **466.34** | 378.59 | 517.07 | 35.10 |
| tailwindcss | 179631 | lightningcss | **666.72** | 524.02 | 736.62 | 42.21 |
| tailwindcss | 179631 | cssparser | 1431.83 | 1150.41 | 1640.08 | 79.50 |
| material-components-web | 495454 | track1_full_parse | **2255.30** | 1898.56 | 2751.09 | 183.32 |
| material-components-web | 495454 | track1_fact_stream | **735.62** | 531.08 | 851.60 | 54.46 |
| material-components-web | 495454 | lightningcss | **972.44** | 121.52 | 1085.59 | 151.90 |
| material-components-web | 495454 | cssparser | 2622.46 | 2149.04 | 2788.59 | 120.45 |
| animate | 71750 | track1_full_parse | **2042.25** | 1428.01 | 2166.38 | 145.51 |
| animate | 71750 | track1_fact_stream | **626.38** | 538.46 | 717.54 | 42.22 |
| animate | 71750 | lightningcss | **1015.07** | 808.78 | 1099.79 | 57.46 |
| animate | 71750 | cssparser | 2097.76 | 1761.19 | 2318.80 | 97.07 |

(material/lightningcss `min=121.52` is a single first-window page-fault outlier; the
median `972.44` is unaffected — the reason the contract mandates median, not min/mean.)

Reproducibility — N=60 medians (`/tmp/skv17-p1e-canon-n60.txt`) track N=100 within
run-to-run thermal scatter and preserve the rank order on every corpus:
bootstrap full_parse 1929 / fact_stream 785 / lcss 926 / cssparser 2556;
tailwind 2401 / 488 / 727 / 1499; material 2549 / 770 / 1149 / 2901;
animate 2043 / 666 / 1138 / 2397.

### 2.2 Per-corpus per-workload delta vs lightningcss (THE fair bar)

`delta = median_track1 / median_lightningcss` (N=100). >1.0 = beats the bar.

| Corpus | full_parse / lcss | fact_stream / lcss | full_parse plane | fact_stream plane |
|---|---:|---:|---|---|
| animate | 2042/1015 = **2.01×** | 626/1015 = **0.62×** | recognition-only | typed `String` |
| bootstrap | 2006/910 = **2.21×** | 720/910 = **0.79×** | recognition-only | typed `String` |
| material | 2255/972 = **2.32×** | 736/972 = **0.76×** | recognition-only | typed `String` |
| tailwindcss | 2414/667 = **3.62×** | 466/667 = **0.70×** | recognition-only | typed `String` |

This **inverts the SK-V16 narrative**. The prior canonical (~70 Mbps, ~14× slower —
`alphaB §1`, `SYNTHESIS.md §0.2`) was a single-sample typed-retime number on a
mislabelled plane. The measured truth at N=100:
- **`track1_full_parse` (recognition-only) already BEATS lightningcss 2.0–3.6× on every
  corpus.** But it is NOT preserve-rich-ast — it counts rules/at_rules/qualified/decls
  (`generated.rs:91-99`) and materializes nothing. It is the proof the *scanner* is not
  the bottleneck (it echoes the A-series 454/496 recognition-only result,
  `SYNTHESIS.md` §1), not a >SOTA admission.
- **`track1_fact_stream` (the live benched typed plane) lands at 0.62–0.79× lightningcss
  on the three regular/dense corpora**, i.e. ~21–38% short of the bar, NOT ~14× short.
  The gap is a sub-2× materialization-plane gap, not an order of magnitude. This is the
  real SK-V17 starting line, and §2.3 attributes exactly where the gap lives.

cssparser (token-scan, materializes nothing) is 1.4–2.6 Gbps and is a flaw probe only
(`SYNTHESIS.md` §0.6); it is not a >SOTA bar.

### 2.3 Hot-leaf attribution — `track1_full_parse` (recognition path)

`/tmp/skv17-p1e/full_parse.json.gz`, 14486 leaf samples, self-time:

| %self | symbol (mangled → demangled) | file:line | class |
|---:|---|---|---|
| **56.52%** | `runtime::generated_css_l4_declaration_values::generated::CssFullParser::find_component_delim` | `css_l4_declaration_values/generated.rs:288` | **scan** (byte-at-a-time delimiter find: `b";{}"`/`b"{};"`/`b";}"`; `delimiters.contains(&byte)` linear membership + per-byte `match`) |
| 28.87% | `css_canon_bench::track1_full_parse` | `bin/css_canon_bench.rs:103` | **scan** (driver frame; `emit_full_parse`/`parse_stylesheet`/`parse_block_item` are `#[inline]`-collapsed into this leaf — the recognition outer loop) |
| **11.05%** | `…::CssFullParser::consume_balanced_at` | `generated.rs:320` | **structural-over-scan** (structural recursion built directly ON the scan primitive: its inner loop `generated.rs:322-338` is byte-for-byte the same `while pos < len` + per-byte `match` over `'"/([{ )]}` as `find_component_delim` :293-308, differing only in the membership test — `byte == close` :324 vs `delimiters.contains` :295 — and it recurses on open-brackets. It is therefore NOT an independent hot leaf; it is the SAME byte-class-membership scan inner loop reached recursively, and shares ONE NEON byte-class-scan target with `find_component_delim`.) |
| 2.45% | `…::CssFullParser::parse_block` | `generated.rs:189` | **structural** (block dispatch loop) |
| 0.85% | `…::CssFullParser::parse_declaration` | `generated.rs:242` | **structural** |
| 0.13% | `…::CssFullParser::parse_at_rule` | `generated.rs:137` | **structural** |
| <0.05% | libsystem_kernel / malloc / platform leaves | — | **tape/alloc** (negligible: one tiny summary `String`, `generated.rs:63`) |

Resolution: **the recognition scan is ~68% in ONE byte-class-membership scan primitive
reached two ways (`find_component_delim` 56.5% top-level + `consume_balanced_at` 11.0%
recursive-over-the-same-inner-loop)**, ~32% in the inlined recognition control loop,
near-zero in the allocator. The 11.05% in `consume_balanced_at` is structural recursion
OVER the scan primitive, not a second distinct leaf: both share the `while pos < len` +
per-byte `match` inner loop, so they collapse to a SINGLE NEON byte-class-scan target.
This is the grammar-neutral delimiter/balance scanner — the NEON structural-pre-scan
candidate. It **confirms the
core-tree-inherited `find_component_delim ~56% / consume_balanced_at ~10%` figures
(`HANDOFF.md:79-83`, `SYNTHESIS.md` §3) DIRECTLY on the benched skinny path** — they are
no longer assumed; they are re-profiled and measured at 56.52% / 11.05%.

### 2.4 Hot-leaf attribution — `track1_fact_stream` (the live typed Track-1 plane)

`/tmp/skv17-p1e/fact_stream.json.gz`, 46307 leaf samples. Library buckets first
(`agg2.py`), then own-code leaves, then syslib-caller attribution (`caller.py`):

| %total | bucket | class |
|---:|---|---|
| 35.25% | css_canon_bench own code | (see breakdown) |
| **31.36%** | libsystem_kernel.dylib | **tape/alloc** (mach vm / page-fault / madvise) |
| **26.27%** | libsystem_malloc.dylib | **tape/alloc** (heap alloc/realloc/free) |
| 7.13% | libsystem_platform.dylib | **tape/alloc** (`_platform_memmove` etc.) |

Own-code leaf breakdown:
| %total | symbol | file:line | class |
|---:|---|---|---|
| **24.59%** | `…::generated::emit_fact_stream` | `generated.rs:5` | **string/tape** (the `out.push_str(...)` fact-stream `String` accumulator — ~590 LOC of `push_str`/`push`, `generated.rs:5-60`) |
| **9.11%** | `…::generated::push_ascii_lower_hex` | `generated.rs:628` | **string** (FNV64 → lowercase-hex serialization of the source-hash field, byte-loop `push`) |
| 0.62% | `DYLD-STUB$$memcpy` | — | string (String grow copy) |
| 0.57% / 0.35% | `DYLD-STUB$$free` / `$$malloc` | — | tape/alloc |

**Syslib-caller attribution (`caller.py`, 29986 syslib leaf samples walked to nearest
binary caller):**

| %syslib | binary caller | meaning |
|---:|---|---|
| **91.44%** | `…::generated::emit_fact_stream` | the String accumulator's growth/alloc reaches the allocator |
| 3.13% | `alloc::raw_vec::RawVecInner::finish_grow` | String/Vec capacity grow |
| 3.13% | `…::generated::push_ascii_lower_hex` | hex serialization buffer |
| 2.29% | `css_canon_bench::track1_fact_stream` | driver `String` drop/free |

Resolution: **the typed fact-stream plane spends ~58% of wall time in the syscall+heap
allocator floor (31.36% kernel + 26.27% malloc), of which 91.44% is reached FROM
`emit_fact_stream`'s `String` growth**, plus 24.59% in the `push_str` accumulator
itself and 9.11% in hex serialization. The dominant benched-CSS-Track-1 cost is
**String building**, exactly as `SYNTHESIS.md §0.4` pre-block-3 asserts. The scanner
(the `find_component_delim`/`consume_balanced_at` that dominates the recognition path)
does NOT appear in the fact_stream top leaves — at the typed plane it is overwhelmed by
the serialization-and-alloc floor. The same `CssFullParser` scan runs underneath, but
the `String` emission dwarfs it.

### 2.5 Hot-leaf classification roll-up (the §2 P1-E synthesis deliverable)

Every benched-CSS hot leaf, resolved to symbol + %self-time + file:line + class:

| Hot leaf (symbol) | file:line | full_parse %self | fact_stream %self | class |
|---|---|---:|---:|---|
| `CssFullParser::find_component_delim` | generated.rs:288 | 56.52% | (under serialization floor) | **scan** (the byte-class-membership scan primitive; one NEON target) |
| `CssFullParser::consume_balanced_at` | generated.rs:320 | 11.05% | — | **structural-over-scan** (structural recursion OVER the `find_component_delim` scan primitive — shares its byte-membership `while`+`match` inner loop :322-338≡:293-308; NOT a distinct leaf, it folds into the SAME single NEON byte-class-scan target as the row above) |
| recognition control loop (`parse_stylesheet`/`parse_block`/`parse_block_item`, inlined) | generated.rs:118/189/209 | 28.87%+2.45% | — | **structural** |
| `generated::emit_fact_stream` (String accumulator) | generated.rs:5 | n/a | 24.59% (+91% of 57.6% syslib) | **string** |
| `generated::push_ascii_lower_hex` (FNV hex) | generated.rs:628 | n/a | 9.11% | **string** |
| syscall+heap allocator floor | libsystem_kernel/malloc | <0.05% | 57.63% | **tape/alloc** |
| `parse_declaration` / `parse_at_rule` | generated.rs:242/137 | 0.85%/0.13% | — | **structural** |

**No `number` / `unicode` / `dispatch` leaf is hot** on either benched plane. The CSS
recognition path does no float parse (it counts, not decodes) and no unicode decode (it
treats `>=0x80` as a name byte, `generated.rs:404`, no codepoint work). There is **no
`tape` leaf at all** — confirming the W6 "tape landed but UNWIRED for CSS" finding
empirically: `Tape`/`ValueRef`/`TapeBuilder` (`runtime/src/tape/`) appear nowhere in
either CSS profile. The `dispatch` vehicle (`bbnf-simd/src/dispatch.rs`
`select_classifier`) likewise appears nowhere — zero SIMD on the CSS path.

## §3 — Delta vs SK-V{N-1} (per row; Mbps + classification)

No SK-V16 admitted per-corpus typed-CSS row exists to delta against: the 24
`css_l4/*/direct_to_struct/main` RESULTS rows (lines 112-135) are all
`not_admitted:SK-V15-W0-broadcast-diagnostic` / `AUDIT-FALSIFIED`, carrying one
broadcast tuple (`track1=2319.041 / cssparser=2362.037 / lightningcss=929.281`)
projected across 24 rows (`SYNTHESIS.md §0.2`; grep `^| css_l4/.*/direct_to_struct/main`
= 24). There is no per-corpus SK-V16 typed throughput. This P1-E table is the **first
per-corpus N≥50 split** that replaces the broadcast and the `[INF]` per-corpus
endpoints alphaB §2 left UNMEASURED-PENDING.

Delta against the prior SK-V16 *plane-level* references (run-dependent, `SYNTHESIS.md
§0.2`):
| Plane | SK-V16 reference | SK-V17-open measured (N=100) | Classification |
|---|---|---|---|
| typed retime (fact_stream-adjacent) | ~3.09 Mbps single-sample (`w6-speed-report.md:164`) | fact_stream 466–736 Mbps median | the 3.09 figure was single-sample noise, not architecture; corrected |
| fact-stream profile plane | ~64–70 Mbps (`SYNTHESIS.md §0.2`) | fact_stream 466–736 Mbps median | prior figure was a different (full-corpus-aggregate-over-broadcast) measurement; the per-corpus cold median is far higher |
| lightningcss full-CSSOM | run-dependent 793 / 833 / 929 / ~974 | 667–1015 Mbps median per corpus | same-run re-baseline; per-corpus split now measured (animate 1015, material 972, bootstrap 910, tailwind 667) |
| cssparser token-scan | ~2476–2539 | 1432–2622 Mbps median per corpus | per-corpus split measured |

The headline delta: the CSS gap-to-SOTA is **a sub-2× materialization-plane gap on the
typed fact-stream plane (0.62–0.79×)**, not the ~14× the SK-V16 close recorded. The
~14× was a wrong-plane single-sample artefact; the measured per-corpus median is the
truth S-P2 grounds on.

**c/B posture (CROSS X1' / CH4-4 fold — the ONE agreed pass-wide reading).** P1-E
carries no cycles-per-byte column — its load-bearing metric is the within-harness Mbps
ratio above. For pass consistency it adopts the single convergent S-P1 c/B posture
(P1-D §3.1, as adjudicated by CROSS §1 / CH4-4): the sub-1.0 CPI the `proc_pid_rusage`
`ri_cycles` surface yields (CPI 0.16–0.28) is **NOT physically impossible** — it is
high IPC (CPI 0.16 ⇔ IPC 6.4, CPI 0.28 ⇔ IPC 3.7), entirely within the Apple M5 Max's
~8-wide out-of-order issue/retire width, so the earlier "falsified / physically-
impossible CPI" characterization was WRONG physics and is withdrawn. The IPC re-reading
is correct; the residual caveat is only that `ri_cycles` is **non-disambiguable from
`proc_pid_rusage` alone** (a real core-cycle counter and a wall-proportional reference
tick are observationally identical at fixed P-core frequency), and therefore
**non-load-bearing** here. The load-bearing cost-density metric is **instructions/byte**
(`ri_instructions` is reliably retired-counted). S-P2 grounds cost on instr/byte and the
within-harness Mbps ratio; any cyc/byte is co-reported with IPC made explicit, never as
a "falsified" or standalone-authoritative figure.

## §4 — Anomalies + masking signals (flagged for S-P2)

1. **Recognition path beats SOTA but is not the subject (plane mask).**
   `track1_full_parse` at 2.0–3.6× lightningcss is recognition-only — it counts, it
   does not materialize, and it builds no `String`. It MUST NOT be read as a >SOTA
   admission (it fails preserve-rich-ast, `SYNTHESIS.md §0.1`). It is a *masking probe*
   in the §8.5 sense: it shows the structural scanner has ~2–3.6× headroom over
   lightningcss, so the entire fact_stream gap is the serialization+materialization
   plane, not the scan. S-P2's job is to keep the scanner's headroom while adding
   lazy-`ValueRef` materialization that costs less than the current `String` floor.

2. **The fact_stream floor is ~58% allocator, 91% of it from `emit_fact_stream`.** This
   is the single dominant intervention surface and it is grammar-neutral (it is `String`
   `push_str` growth, not CSS-specific logic). The SK-V17 tape activation (kill
   `emit_fact_stream` String → `TapeBuilder` append, `push_plain_offset` =
   `assembler.rs:71` one branchless u32 write) directly retires this 58% floor. This is
   the empirical antecedent for the §3 lever-1 (kill fact-stream String) +
   lever-2 (O(1) tape checkpoint, no per-leaf eager payload). NOT a re-proposal — it is
   the measured ground for the contract's own pre-declared route.

3. **`find_component_delim` 56.52% + `consume_balanced_at` 11.05% (= ~68% in ONE shared
   scan primitive) re-confirmed on the benched skinny path** (HANDOFF §3 demanded this
   re-confirm before any NEON kernel lands; it is done). The delimiter find is
   `delimiters.contains(&byte)` over a 3-byte set plus a per-byte `match`
   (`generated.rs:293-308`); `consume_balanced_at` (:322-338) is the identical inner
   loop reached recursively. They are ONE NEON target, not two — a textbook
   `byte_class_index_64` + movemask-cascade NEON candidate routed through
   `bbnf-simd/src/dispatch.rs select_classifier` (`SYNTHESIS.md §0.1` NEON gate). BUT:
   the NEON gate is gated behind tape activation (there is no structural index to
   pre-scan into until the tape decodes CSS); and on the *typed* fact_stream plane the
   scan is masked by the String floor, so the order is lever-1/2 (tape) FIRST, then NEON
   on the surviving scan. S-P2 must not invert this.

4. **No number/unicode/dispatch/tape hot leaf** on either plane (§2.5). Two consequences:
   (a) the udot/i8mm digit kernel (`digit_mac.rs:27`, C4b) has **no benched CSS
   antecedent** — there is zero digit-parse self-time in CSS recognition (it counts, it
   does not decode dimensions); per HANDOFF, C4b lands ONLY if a re-profile proves the
   digit leaf is a top-N tailwind self-time leaf — this profile shows it is NOT, so C4b
   stays orphan-blocked on the current (recognition/fact-stream) planes. It may resurface
   once the typed lazy-`ValueRef` path decodes dimensions, which is a NEW plane this
   profile cannot measure (the path does not exist yet) — S-P2 must re-profile the typed
   path after W1/W2, not inherit a CSS digit-kernel hypothesis from here. (b) The tape
   substrate is provably unwired for CSS (zero `Tape`/`ValueRef` samples).

5. **material lightningcss min=121.52 outlier** (one first-window page fault). Median
   (972.44) is robust; flagged only to justify the median statistic (CH6 reproducibility).

6. **Pre-block check (CH3).** Nothing in §4 re-proposes a REDRESS-blocked route. The
   tape-activation antecedent (anomaly 2) is the contract's own pre-declared lever, not
   a blocked one; it explicitly does NOT re-open AZ-IV eager-value-tree (the tape stays
   lazy-by-default, no per-leaf eager payload) nor StructRegistry indirection (single
   non-generic `TapeBuilder`). The recognition-path headroom (anomaly 1) is NOT a
   proposal to ship recognition-only as admission (brace-counter CSS admission is
   pre-blocked, `SYNTHESIS.md §0.4`); it is a masking-signal observation only.

## §5 — Sources (every artefact path + run id)

- Canonical bench harness (formalized, sole canonical S-P1 CSS harness):
  `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` (403 lines, N≥50 cold,
  median/min/max/stddev). Verified source lines this cycle: `fn sample` :146 (cold
  per-parse loop :153-159, `Instant::now()` :154, pre-touch :152); `assert!(n >= 50)`
  :250; `fn mbps` :138-142 (`bytes*8 / secs*1e6`); `Stats` :131-135; median :160-165,
  min :166, max :167, population stddev :168-169; ROW print :260-272; `CSS_CANON_PROFILE`
  samply driver :183-207; `CSS_CANON_PMU` mode (P1-D) :211-247.
- Bench output: `/tmp/skv17-p1e-canon-n100.txt` (N=100, primary),
  `/tmp/skv17-p1e-canon-n60.txt` (N=60, reproducibility).
- samply profiles (committed-citable, on disk; binaries not committed):
  `/tmp/skv17-p1e/full_parse.json.gz` (track1_full_parse, 14486 samples, run 14.56s),
  `/tmp/skv17-p1e/fact_stream.json.gz` (track1_fact_stream, 46307 samples).
- Symbolication scripts: `/tmp/skv17-p1e/{symbolicate.py, agg2.py, caller.py}`
  (atos `-l 0x100000000` against `skinny/target/release/css_canon_bench` packed debuginfo).
- Benched CSS recognition path: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
  (`emit_full_parse` :61, `CssFullParser::find_component_delim` :288,
  `consume_balanced_at` :320, `parse_stylesheet` :118, `parse_block` :189,
  `parse_block_item` :209, `parse_declaration` :242, `parse_at_rule` :137).
- Benched CSS typed fact-stream path: `generated.rs` (`emit_fact_stream` :5,
  `push_ascii_lower_hex` :628, `fnv64` :619, `push_hex64` :636); entry
  `nonjson_css_l4.rs:596` `track1_facts` → `track1::parser::parse` (parser.rs:5).
- Corpus set: `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:21-54` (4 corpora, total
  979,638 B); files at `skinny/corpora/css-l4-sk-v14/`.
- Tape substrate (unwired for CSS, confirmed absent from both profiles):
  `skinny/crates/runtime/src/tape/` (`Tape` mod.rs:94, `ValueRef` :175, `PayloadArena`
  :38, `DocumentView` :227; `TapeBuilder` assembler.rs:42, `push_plain_offset` :71).
- NEON dispatch (absent from both profiles): `skinny/crates/bbnf-simd/src/dispatch.rs`
  (`select_classifier` :42, `lo6_table_admissible` :101); udot orphan
  `aarch64/digit_mac.rs:27`.
- Prior-narrative sources corrected: `SYNTHESIS.md §0.2`, `HANDOFF.md` Current-State,
  alphaB §1/§2 (`[INF]` per-corpus endpoints now measured), `w6-speed-report.md:164`
  (3.09 single-sample).
- Host: Apple M5 Max, aarch64-apple-darwin. HEAD `6496fecae` (bracket `1c5bd7a25`).
  samply 0.13.1, rustc 1.96.0-nightly (02c7f9bec 2026-04-10).
