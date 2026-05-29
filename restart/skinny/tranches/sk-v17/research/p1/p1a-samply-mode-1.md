# SK-V17 P1-A: samply mode-I profile of the benched CSS-tape path + canonical N>=50 cold harness

Pass: S-P1 Profile. Cycle: V4.
Date: 2026-05-29.
Scope: samply profiling of the SK-V17 BENCHED skinny CSS path (mode I = cold per-parse, release+`debug=true`), redirected per the SK-V17 CSS-tape subject. P1-A additionally carries the bench/measurement mandate: formalize the N>=50 canonical cold harness and report median/min/max/stddev for track1 / lightningcss / cssparser per corpus. Every hot leaf resolved to a symbol + file:line.
Output: this file.
Baseline: SK-V17-open. Master HEAD `6496fecae` (working tree; `1c5bd7a25` is the SK-V16 close referenced by the contract — the benched CSS source on this tree is byte-identical for the profiled path).
Host triple: aarch64-apple-darwin (Apple M5 Max). No x86.
Build flags: `RUSTFLAGS="-C target-cpu=native" cargo build --profile bench` (`[profile.bench]` inherits release: `opt-level=3, lto="fat", codegen-units=1, debug=true, strip=false, split-debuginfo="packed"`).
Profile tool: samply 0.13.1 (`samply record --save-only --unstable-presymbolicate -r 9999`); self-time symbolicated via the emitted `.syms.json` sidecar against the packed dSYM. **Cold-per-parse Mbps + dispersion AND instruction-per-byte both via the single designated canonical harness `bin/css_canon_bench`** (`std::time::Instant` for the N>=50 dispersion table, `CSS_CANON_PMU` mode for `proc_pid_rusage(RUSAGE_INFO_V5).ri_instructions`). V3 folds CH4-5: the authoritative §2.1 Mbps table is now sourced from `css_canon_bench` (N=200) — the one designated binary — not from `css_cold_harness`.
Corpus coverage: 4/4 of the BENCHED CSS set + the 979638 B aggregate. The §2.1 17 JSON corpora are NOT in scope for the SK-V17 CSS-tape subject; the benched CSS corpus set is fixed at `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:21-58` = `{animate, bootstrap, tailwindcss, material-components-web}` (per SYNTHESIS §0.5; `normalize` is not benched). JSON guard rows are P1-F's delta scope, not re-profiled here.

> **CANONICAL-HARNESS NOTE (CROSS X2 / CH4-3 + CH4-5 fold, V3).** Five distinct cold-bench binaries were authored across the pass (`css_cold_harness`, `css_canon_bench`, `css_cold_bench`, `css_cold_canonical`, `css_track1_profile`). The orchestrator designates **`skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` as the SINGLE canonical harness** (403 lines; N>=50 `assert!` at `:250`; the only binary carrying both the cold N>=50 dispersion mode `:249-277`, the `CSS_CANON_PMU` PMU mode `:211`, and the samply driver mode `:183`). **V3 folds CH4-5:** both the §2.1 Mbps dispersion table AND the §2.1b instr/byte cost column are now sourced from this ONE designated binary — §2.1 from its N=200 cold mode (`css_canon_n200_v2.txt`), §2.1b from its `CSS_CANON_PMU` mode (`css_canon_pmu_v2.txt`). The V2 residual (§2.1 Mbps coming from `css_cold_harness` while instr/byte came from `css_canon_bench`) is closed. The historical observation that absolute Mbps differed ~20-29% across the five harnesses (per-harness allocator pattern, target-cpu flag, N) still holds and is why a single canonical binary is now used end-to-end; within-harness plane ratios were always the load-bearing quantity and are unchanged.

> **V4-FOLD NOTE (CH3 V3 disposition; convergence carry).** V3 CHALLENGE returned **42/42 = 100% ACCEPT, 0 REVISE, 0 REJECT** against the six P1 artefacts; combined with V2 (also ≥95%) CH3 has returned ≥95% for two consecutive cycles with zero open REVISE — the per-lens convergence condition (`ORCHESTRATOR.md` §3Z) is met. The V2 ROOT REJECT (the c/B "falsified/physically impossible" mis-physics) is **RESOLVED** in the single posture below and carried verbatim. No P1-A item was REVISE/REJECT at V3, so V4 re-emits the V3 content with every load-bearing citation re-verified fresh against source + on-disk data on master HEAD `6496fecae`: hot-leaf lines `generated.rs:{61,103,118,189,242,288,295,320,628}` confirmed; `css_canon_n200_v2.txt` and `css_canon_pmu_v2.txt` byte-identical to §2.1/§2.1b; IPC range 3.51–6.23 recomputed from on-disk `cpi`; `emit_fact_stream` still `Result<String, CssFactError>` (`generated.rs:5`). No new re-open; zero orphan REVISE.
>
> **COST-SURFACE POSTURE (CROSS X1' / CH4-4 fold — the ONE pass-wide posture, adopted verbatim, V3; re-affirmed V4).** **instr/byte (`ri_instructions`) is the sole load-bearing cost density and is reliable to <0.5%.** The sub-1.0 CPI from `ri_cycles` (measured 0.16-0.28, `css_canon_pmu_v2.txt`) is **PHYSICAL, NOT impossible**: it corresponds to IPC 3.5-6.2 (per-row IPC table in §2.1b), well within the Apple M5 Max P-core's ~8-wide issue/retire width — high IPC is the normal signature of a tight, branch-friendly scan loop on a wide superscalar. The earlier V1/V2 characterization of this counter as "falsified / physically impossible" was **wrong physics** (it confused CPI with IPC) and is retracted here. However, `proc_pid_rusage.ri_cycles` **cannot be disambiguated as dynamic core-cycles vs a wall-proportional scaled tick from the rusage interface alone** (the `ri_cycles/wall ≈ 4.2-4.3 GHz` steadiness is observationally identical under both models because `wall` is itself loop-derived; `hw.tbfrequency` confirms a 24 MHz scaled reference clock exists on this platform). Cyc/byte is therefore reported RAW and **non-load-bearing**; **no conclusion in this artefact rests on it.** This is the single posture adopted across all six P1 artefacts (it replaces P1-D §3.1's earlier "proven 4.27 GHz / supersedes A/B/F" over-claim and A/B/C/F's "falsified/impossible" under-claim with the one agreed reading). The §2.1 cyc/B column below is retained struck-through, labelled RAW-non-load-bearing, for traceability only.

---

## §1 — Method (commands run; verbatim, reproducible)

### §1.1 The canonical N>=50 cold harness (formalized; replaces W6_SAMPLE_COUNT=1)

The prior CSS speed authority is `skinny/crates/bbnf-bench/src/css_l4_w8.rs`. It is statistically inadequate AND structurally a broadcast (the W8R regression SYNTHESIS §0.2 names):

- `W8_PROFILE_ITERS = 8` (`css_l4_w8.rs:18`) — far below the N>=50 telemetry-honesty gate.
- `measure_full_parse_profiles` (`css_l4_w8.rs:206-260`) times ONE aggregate `Instant::elapsed()` across **all 4 corpora x all 7 grammar `parse_full` profiles x 8 iters** and divides by the summed byte count → exactly ONE `track1_mbps` / ONE `lightningcss_mbps` / ONE `cssparser_mbps` tuple. That single tuple is the source of the 24 broadcast rows (SYNTHESIS §0.2, pre-blocked §0.4). There is NO per-corpus split and NO dispersion.
- It also runs the 7 generated grammar parsers (`parse_declaration_values` … `parse_nested_layout`, `css_l4_w8.rs:398-429`) over each corpus — a profile-fan, not the real per-corpus typed parse.

The N>=50 cold replacement times **every corpus individually**, **cold** (one parse per sample, no warmup, no shared cache, no amortised allocation across samples — the timed region is a single black-boxed `parse(...)` with the result dropped each sample, `sample()` `css_canon_bench.rs:146`), over N>=50 samples (`assert!(n>=50)` `:250`), and reports **median / min / max / mean / stddev (Mbps)** for FOUR planes. **V3: both the Mbps dispersion (§2.1) and the authoritative instructions-per-byte cost (§2.1b) are now from the SINGLE canonical harness `css_canon_bench`** — §2.1 from its N=200 cold mode, §2.1b from its `CSS_CANON_PMU` mode. The `ri_cycles`-derived cycles-per-byte is reported RAW and non-load-bearing per the COST-SURFACE POSTURE above (physical sub-1.0 CPI, but non-disambiguable from rusage). FOUR planes:

| Plane | Symbol | Role |
|---|---|---|
| `track1_fact` | `css_l4_declaration_values::parser::parse` → `emit_fact_stream` (`generated.rs:5`) | the ACTUAL benched CSS Track 1 — the fact-stream String emitter reached via `nonjson_css_l4.rs:596`; pre-blocked as an admission plane (SYNTHESIS §0.4) but it is what is benched today |
| `track1_full` | `css_l4_declaration_values::parser::parse_full` → `emit_full_parse` (`generated.rs:61`) | the `CssFullParser` recognition-only structural-summary scanner — the W8 `track1_full_parse` plane |
| `lightningcss` | `StyleSheet::parse(src, ParserOptions::default())` (lightningcss 1.0.0-alpha.71) | full CSSOM materialise — the fair >SOTA bar, re-baselined same-run |
| `cssparser` | `StyleSheetParser` full token-walk probe (matches `css_l4_w8.rs CssparserFullParseProbe`) | token-scan flaw probe; beating it is NOT a SOTA claim |

Commands (single invocation, sequential — one cargo per target):

```
cp -r /Users/mkbabb/Programming/bbnf-lang/data <build-worktree>/   # fixtures gitignored; corpora already at skinny/corpora/css-l4-sk-v14/ (4 files, 979638 B)
cd skinny
RUSTFLAGS="-C target-cpu=native" cargo build --profile bench -p bbnf-bench --bin css_canon_bench
# §2.1 authoritative Mbps dispersion (N=200 cold per-parse, median/min/max/stddev), canon binary:
./target/release/css_canon_bench 200          > /tmp/skv17-p1/css_canon_n200_v2.txt
# §2.1b authoritative instr/byte + RAW cyc/B (CPI/IPC), same canon binary, PMU mode:
CSS_CANON_PMU=1 ./target/release/css_canon_bench 2000 > /tmp/skv17-p1/css_canon_pmu_v2.txt
```

(The `[profile.bench]` artefact lands in `target/release/` because bench inherits release and shares the dir name. The legacy `css_cold_harness` N=64/N=80 cold runs — `cold-N64.txt` / `cold-N80-run2.txt` — are retained on disk only as a cross-harness stability check; they are NOT the authoritative source any longer per CH4-5.)

### §1.2 samply mode-I hot-leaf capture (cold per-parse, both track1 planes)

`css_cold_harness samply <corpus> <iters> <plane>` runs a pure parse loop (no timing calls inside the loop) for the named plane, so samply attributes only parse self-time:

```
BIN=./target/release/css_cold_harness
samply record --save-only --unstable-presymbolicate -r 9999 -o /tmp/skv17-p1/fact-bootstrap.json.gz -- $BIN samply bootstrap   3000 fact
samply record --save-only --unstable-presymbolicate -r 9999 -o /tmp/skv17-p1/full-bootstrap.json.gz -- $BIN samply bootstrap   3000 full
samply record --save-only --unstable-presymbolicate -r 9999 -o /tmp/skv17-p1/fact-tailwind.json.gz  -- $BIN samply tailwindcss 3000 fact
samply record --save-only --unstable-presymbolicate -r 9999 -o /tmp/skv17-p1/full-tailwind.json.gz  -- $BIN samply tailwindcss 3000 full
python3 /tmp/skv17-p1/symbolicate.py <profile>.json.gz <profile>.json.syms.json   # leaf self-time, sidecar-symbolicated
```

samply-discipline note (`samply-symbol-resolution` memory): the memory warns `--save-only` loses symbols. On samply 0.13.1 `--unstable-presymbolicate` emits a `.syms.json` sidecar that, combined with `debug=true` packed dSYM, resolves every Rust symbol (the `symbolicate.py` walker maps each leaf frame's RVA+module to the sidecar `symbol_table`, including inlined frames). Symbol resolution is verified complete: all binary frames resolve to named Rust symbols (see §2); only libsystem-internal frames remain as hex offsets, which is expected and they are attributed to their family by caller (§2.3).

---

## §2 — Findings

### §2.1 Per-corpus cold throughput (N=200, canonical harness; host=arm64)

Median / min / max / stddev in Mbps from the **single canonical harness `css_canon_bench` N=200 cold mode** (V3 CH4-5 fold — authoritative Mbps now from the designated binary, not `css_cold_harness`). Source: `/tmp/skv17-p1/css_canon_n200_v2.txt`. The trailing `(RAW cyc/B)` column is the `ri_cycles`-derived figure from the PMU mode of the SAME harness, **retained only for traceability and reported RAW / non-load-bearing** per the COST-SURFACE POSTURE above — its sub-1.0 CPI is physical (high IPC), but `ri_cycles` is non-disambiguable from rusage, so no conclusion rests on it. The authoritative per-byte cost surface is the instructions-per-byte table in §2.1b.

| corpus | bytes | plane | median | min | max | stddev | RAW cyc/B (non-load-bearing) |
|---|---:|---|---:|---:|---:|---:|---:|
| animate | 71750 | track1_fact | 750.45 | 401.11 | 851.84 | 65.97 | ~~47.93~~ |
| animate | 71750 | track1_full | **2431.77** | 1417.28 | 2608.61 | 166.96 | ~~14.40~~ |
| animate | 71750 | lightningcss | 1235.35 | 819.37 | 1331.53 | 63.21 | ~~28.23~~ |
| animate | 71750 | cssparser | 2625.25 | 2039.37 | 2663.05 | 54.79 | ~~13.54~~ |
| bootstrap | 232803 | track1_fact | 835.71 | 448.95 | 880.48 | 77.10 | ~~41.65~~ |
| bootstrap | 232803 | track1_full | **2327.30** | 1869.04 | 2586.55 | 140.44 | ~~15.17~~ |
| bootstrap | 232803 | lightningcss | 1034.09 | 655.05 | 1118.49 | 82.92 | ~~32.75~~ |
| bootstrap | 232803 | cssparser | 2835.28 | 2019.80 | 2936.23 | 161.99 | ~~12.01~~ |
| tailwindcss | 179631 | track1_fact | 549.15 | 397.14 | 631.66 | 26.34 | ~~63.15~~ |
| tailwindcss | 179631 | track1_full | **2530.01** | 1919.58 | 2997.49 | 202.57 | ~~12.98~~ |
| tailwindcss | 179631 | lightningcss | 842.52 | 400.14 | 899.51 | 56.39 | ~~42.30~~ |
| tailwindcss | 179631 | cssparser | 1708.95 | 1422.76 | 1867.31 | 62.79 | ~~20.24~~ |
| material-components-web | 495454 | track1_fact | 881.64 | 783.71 | 946.27 | 29.14 | ~~40.47~~ |
| material-components-web | 495454 | track1_full | **2727.31** | 2230.26 | 2993.59 | 135.93 | ~~13.25~~ |
| material-components-web | 495454 | lightningcss | 1290.47 | 1117.04 | 1378.26 | 43.57 | ~~26.84~~ |
| material-components-web | 495454 | cssparser | 3250.43 | 2137.45 | 3510.36 | 192.34 | ~~10.56~~ |

Aggregate byte count is the raw sum of the four corpus files = 71750 + 232803 + 179631 + 495454 = **979638 B** (the 3 inter-file newline separators are NOT counted in the throughput denominator; all six P1 artefacts use 979638). Throughput is `bytes_per_parse / sample_seconds`, median over N=200 cold per-parse samples. The byte-weighted aggregate medians across the four corpora are: track1_fact **~800**, track1_full **~2574**, lightningcss **~1143**, cssparser **~2823** Mbps (computed from the per-corpus medians weighted by corpus byte count).

### §2.1b Per-corpus instruction-per-byte (authoritative cost surface; reliable counter)

Instructions-per-byte = `ri_instructions / (bytes * iters)` from the canonical harness `css_canon_bench` `CSS_CANON_PMU` mode (iters=2000). Source: `/tmp/skv17-p1/css_canon_pmu_v2.txt`. The `ri_instructions` retirement counter is reliably counted on this host (P1-F §2.2) and is the per-byte cost figure S-P2 grounds on. The trailing `1/IPC` (=CPI) column is from the SAME PMU run, reported to substantiate the COST-SURFACE POSTURE: every plane's CPI is sub-1.0, i.e. IPC 3.5-6.2 — physical on the M5's ~8-wide core, NOT impossible. The RAW cyc/B these CPIs imply is reported in §2.1 struck-through / non-load-bearing.

| corpus | track1_fact i/B | track1_full i/B | lightningcss i/B | cssparser i/B | fact/full ratio | IPC range (all 4 planes) |
|---|---:|---:|---:|---:|---:|---:|
| animate | 279.83 | 57.75 | 155.38 | 79.64 | 4.85x | 4.01-5.88 |
| bootstrap | 234.18 | 53.72 | 160.27 | 68.26 | 4.36x | 3.54-5.69 |
| tailwindcss | 364.18 | 51.51 | 236.62 | 126.21 | 7.07x | 3.97-6.23 |
| material-components-web | 215.32 | 46.52 | 137.56 | 60.86 | 4.63x | 3.51-5.76 |
| aggregate (byte-weighted) | 251.82 | 49.97 | 162.43 | 75.98 | 5.04x | 3.51-6.23 |

**Cost-surface reading.** On the reliable instruction counter the fact-stream plane retires **4.4-7.1x more instructions per byte than its own recognition scan** (bootstrap 234 i/B vs 53.7 i/B = 4.36x; tailwind 364 vs 51.5 = 7.07x). That instruction-count gap is the empirical magnitude of the String-building + allocation tax the tape activation retires. The recognition plane (track1_full) is also strictly fewer instructions per byte than lightningcss on every corpus (aggregate 49.97 vs 162.4 i/B = 3.25x leaner), corroborating its Mbps lead on a counter that cannot be gamed by clock estimation. The IPC column confirms the COST-SURFACE POSTURE: across all 16 plane×corpus cells CPI ranges 0.160-0.285 (IPC 3.51-6.23), every value physical on the ~8-wide M5 P-core — the sub-1.0 CPI is high IPC, not a falsified counter; it is simply non-disambiguable from the rusage interface and therefore not load-bearing.

The N=200 cold dispersion (§2.1) and the iters=2000 PMU run reproduce every plane ordering and magnitude within cold-run noise across both v1/v2 PMU passes (instr/byte stable to <0.3%, e.g. animate fact 279.27→279.83); the medians are stable, stddev is 3-8% of median on the recognition + comparator planes.

**Load-bearing facts (N=200 canon medians):**
1. **The benched CSS `track1_full` recognition scan already BEATS lightningcss full-CSSOM on EVERY corpus** — animate 2432 vs 1235 (1.97x), bootstrap 2327 vs 1034 (2.25x), tailwind 2530 vs 843 (3.00x), material 2727 vs 1290 (2.11x), aggregate ~2574 vs ~1143 (2.25x). It also beats cssparser on tailwind (2530 vs 1709). BUT it materializes NO AST — it only increments a 4-field `CssFullParseSummary` (rules/at_rules/qualified_rules/declarations, `generated.rs:91-99`). It is the recognition skeleton, not the rich typed CSSOM SK-V17 must ship.
2. **The benched `track1_fact` fact-stream is BELOW lightningcss on every corpus** — animate 750 vs 1235 (0.61x), bootstrap 836 vs 1034 (0.81x), tailwind 549 vs 843 (0.65x), material 882 vs 1290 (0.68x), aggregate ~800 vs ~1143 (0.70x). The fact-stream plane is **2.8-4.5x slower than its own recognition scan** (fact/full = 0.22-0.36x) — the entire delta is String building + allocation (§2.3), NOT scanning. (On the canon N=200 harness the fact-stream sits closer to lightningcss than the earlier `css_cold_harness` run reported — 0.70x aggregate vs 0.63x — but the qualitative conclusion is identical: below lightningcss, dominated by the emission tax, not the scan.)
3. **lightningcss re-baselines at 843-1290 Mbps median** on the canon harness — materially HIGHER than the contract's run-dependent prior references (61/793/833/929/974, SYNTHESIS §0.2). This is the same-run re-baseline the gate requires; the prior "~70 Mbps track1, ~14x slower" narrative (SYNTHESIS §0.2 / HANDOFF:41-48) is an artefact of the eager `OpenFrame` value-tree path in the TOTALITY tree, which is NOT on the benched skinny CSS path. On the benched skinny path no plane is ~14x off lightningcss.

### §2.2 Hot leaves — track1_full recognition plane (samply mode I, sidecar-symbolicated)

Source: `/tmp/skv17-p1/full-bootstrap.json.gz` (27180 samples) + `full-tailwind.json.gz` (18955 samples), `selftime-consolidated.txt`. All symbols are in `runtime::generated_css_l4_declaration_values::generated` (`skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`).

| Symbol | file:line | bootstrap self-time | tailwind self-time | class |
|---|---|---:|---:|---|
| `CssFullParser::find_component_delim` | `generated.rs:288` (hot inner: `delimiters.contains(&byte)` `:295`) | **58.41%** | **65.05%** | scan (delimiter membership) |
| `CssFullParser::parse_stylesheet` | `generated.rs:118` (incl. inlined `skip_ws_comments` `:263`) | 26.85% | 29.74% | scan (driver + ws skip) |
| `CssFullParser::consume_balanced_at` | `generated.rs:320` | 10.79% | 0.15% | scan (nesting balance) |
| `CssFullParser::parse_block` | `generated.rs:189` | 2.51% | 3.71% | structural |
| `CssFullParser::parse_declaration` | `generated.rs:242` | 1.19% | 1.16% | structural |
| `CssFullParser::parse_at_rule` | `generated.rs:137` | 0.07% | — | structural |
| allocation/syslib total | — | < 0.05% | < 0.1% | tape/alloc |

The recognition plane is **scan-bound and allocation-free** (malloc+memmove < 0.1% on both corpora). `find_component_delim` (58-65%) is the single dominant leaf, and the body is a per-byte loop testing `delimiters.contains(&byte)` against a 3-4 byte delimiter slice (`b"{};"` / `b";}"` / `b":{};"`), branching into `consume_balanced_at` / `consume_string_at` / `consume_comment_at` on bracket/quote/comment.

### §2.3 Hot leaves — track1_fact fact-stream plane (samply mode I)

Source: `/tmp/skv17-p1/fact-bootstrap.json.gz` (92057 samples) + `fact-tailwind.json.gz` (93534 samples).

| Symbol | file:line / module | bootstrap | tailwind | class |
|---|---|---:|---:|---|
| `mach_absolute_time` (caller `0x2b483` ∈ libsystem_malloc) | libsystem_malloc deferred-reclaim path (caller verified §below) | 27.85% | 28.81% | alloc |
| `<alloc::raw_vec::RawVecInner>::reserve` | std (String/`out.push_str` growth in `emit_fact_stream`/`emit_full_parse`, `generated.rs:5,61`) | 23.82% | 18.64% | alloc/tape |
| `generated::push_ascii_lower_hex` | `generated.rs:628` (fnv64 source-hash hex emission) | 8.71% | 7.78% | string |
| `_platform_memmove` | libsystem_platform (String append) | 6.22% | 8.22% | string |
| `<u32 as core::fmt>::_fmt` / `LowerHex` | std (count `.to_string()` in `emit_full_parse` `:92-98`) | 1.42% | 1.39% | string |
| libsystem_malloc internals (sum of `0x2a*`/`0x2b*`/`0x3a*` leaves) | libsystem_malloc | ~12% | ~14% | alloc |
| `DYLD-STUB$$free` / `DYLD-STUB$$memcpy` (the `0x2176f4`/`0x217760` harness leaves, atos-resolved) | css_cold_harness PLT stubs | ~1.4% | ~1.4% | alloc |

`mach_absolute_time` caller attribution: a stack-prefix walk over all 92057 fact-bootstrap samples finds 25591/25640 `mach_absolute_time` leaves are called from `0x2b483` (a libsystem_malloc frame) — modern macOS libmalloc calls `mach_absolute_time` in its deferred-`madvise`/reclaim bookkeeping. It is therefore an **allocation-family** leaf, not a timer artefact (the samply-mode loop makes no timing call). Aggregating the alloc family (mach_absolute_time + RawVecInner::reserve + malloc internals + dyld free/memcpy stubs + memmove) the fact-stream plane is **~75-80% allocation + String growth + memmove**, ~8-9% hex formatting, and the actual recognition scan (the `find_component_delim` family that dominates track1_full) is pushed below the noise floor — i.e. in the fact-stream plane the structural scan is NOT the wall; the String/alloc emission is.

### §2.4 Equality / plane-shape note (gate-before-speed context)

`track1_full` emits a recognition summary (rule counts), not the 8-field typed CSSOM equality the SK-V17 gate requires; the EXACT 8-field cssparser equality (`rules=10136/style=9561/sel=9561/decls=20043`) lives on the W6 typed-retime path (TOTALITY tree), not on this benched skinny structural path. P1-A measures throughput planes only; the typed-equality re-proof is a wave deliverable (S-P3), not an S-P1 measurement. Flagged for S-P2: there is **no rich-typed real-corpus CSS parser in the benched skinny tree today** — `generated_real_typed.rs` is JSON-only; the benched CSS typed surface is exactly these two String-emitting planes over the `decl_values` grammar (entry rule `stylesheet`, `config.rs:11`).

---

## §3 — Delta vs SK-V16 (per row; Mbps + classification)

There is **no prior per-corpus admitted typed-CSS row** to delta against: SK-V16's only CSS rows are the 24 falsified broadcast diagnostics carrying one tuple `track1=2319.041 / cssparser=2362.037 / lightningcss=929.281` (SYNTHESIS §0.2, lines 112-135 of `skinny/RESULTS.md`). The honest deltas are vs that broadcast tuple and vs the run-dependent W6 figures:

| Plane | SK-V16 reference | SK-V17 P1-A (this run, median) | Δ / classification |
|---|---|---|---|
| track1 (broadcast tuple) | 2319.041 (single aggregate, falsified) | track1_full aggregate ~2574 (per-corpus 2327-2727) | the broadcast 2319 ≈ the per-corpus track1_full recognition plane; the broadcast was the FULL-PARSE recognition number broadcast across 24 rows, NOT a typed number. P1-A de-broadcasts it into 4 per-corpus medians + dispersion. |
| track1 typed-retime (W6) | ~3.09 Mbps (`w6-speed-report.md:164`, TOTALITY eager tree) | n/a on benched skinny | the ~3 Mbps figure is the core-tree eager `OpenFrame` path; it is NOT a benched skinny CSS plane. No skinny plane is anywhere near 3 Mbps. |
| lightningcss | 929.281 (broadcast) / 793 / 833 / ~974 (run-dependent) | per-corpus 843-1290, aggregate ~1143 | re-baselined same-run on the canon harness; now per-corpus, not one broadcast tuple. |
| cssparser | 2362.037 (broadcast) / ~2476-2539 | per-corpus 1709-3250, aggregate ~2823 | per-corpus split reveals tailwind cssparser is only 1709 (the corpus character matters; the broadcast hid it). |

Outcome classification (schema-v3): the CSS subject remains **N-direct** (no typed CSS row admitted; the rich typed parser does not exist on the benched tree). The measured facts REVISE the contract's framing: the "~14x slower than lightningcss" premise (SYNTHESIS §0.2) does not hold on the benched skinny path — the recognition scan is 2-3x FASTER than lightningcss, and the fact-stream is 0.61-0.81x (aggregate 0.70x). The SK-V17 gap is the **typed-projection cost between those two planes**, not a 14x scan deficit.

---

## §4 — Anomalies + masking signals (flagged for S-P2)

1. **MASKING — recognition plane masks the typed cost.** `track1_full` (recognition-only) at 2327-2727 Mbps beats lightningcss everywhere, but it builds no AST. If S-P2 (or any wave) reports the recognition number as the typed result, that is the exact W6 "summary lane retains nothing" error (`w6-speed-report.md:83-87`) and the W8R broadcast pattern. The honest SK-V17 target lives BETWEEN track1_full (~2574 aggregate, no AST) and track1_fact (~800 aggregate, String fact-stream) — the lazy `ValueRef` projection must reconstruct the typed CSSOM at a cost that keeps the aggregate above the lightningcss ~1143 bar. The ~2.25x recognition headroom over lightningcss is the budget the projection may spend. NOT a re-propose of any route; an observation.

2. **find_component_delim is the re-confirmed NEON leaf (58-65%, benched skinny path).** The architecture-doc `find_component_delim ~56%` / `consume_balanced_at ~10%` figures (SYNTHESIS NEON gate; `sk-v16-css-sota-tape-architecture.md:255-256`) were inherited from the CORE-TREE profile and tagged `S-P1-re-confirm-on-benched-path` (HANDOFF Next-Move §2). **RE-CONFIRMED on the benched skinny path:** `find_component_delim` 58.41% (bootstrap) / 65.05% (tailwind), `consume_balanced_at` 10.79% (bootstrap) / 0.15% (tailwind). The leaf is `runtime::generated_css_l4_declaration_values::generated::CssFullParser::find_component_delim` (`generated.rs:288`), hot body `delimiters.contains(&byte)` (`:295`). This is a grammar-neutral byte-set-membership scan over a runtime delimiter slice (CH2: it is the generic `find_ascii_set_member64` primitive shape, NOT a CSS-named path) — the candidate for `byte_class_index_64` / `to_bitmask64` via `bbnf-simd/src/dispatch.rs select_classifier` (`:42`). The NEON route is pre-blocked behind tape activation (SYNTHESIS NEON gate: "no structural index to pre-scan into until the tape decodes CSS") — flagged, NOT re-opened.
   - **consume_balanced_at is corpus-character-dependent**, not a fixed ~10%: 10.79% on bootstrap (deep nested parens/brackets in shorthand values), 0.15% on tailwind (short flat utility declarations). S-P2 must not treat ~10% as a constant; it is a bootstrap/material trait. tailwind's cost is almost entirely the top-level `find_component_delim` + `parse_stylesheet` driver.

3. **The fact-stream plane is allocation-bound, not scan-bound (rich-AST profile shape).** track1_fact ~75-80% allocation/String-growth/memmove (`RawVecInner::reserve` 19-24%, libmalloc family incl. `mach_absolute_time` ~28%, memmove 6-8%) + ~8% hex formatting. This matches the architecture doc's Investigation-5 "68.7% in system libraries" rich-AST shape (`sk-v16-css-sota-tape-architecture.md:320`). The String-emission lever (lever 1: kill fact-stream String → `TapeBuilder` append; SYNTHESIS Trajectory) is RE-CONFIRMED real and dominant on the fact-stream plane — its reliable magnitude is the instr/byte gap (§2.1b: fact-stream retires 4.4-7.1x more instructions per byte than the recognition scan), NOT the inherited "emit_* ~34%" recognition-plane cycle figure (`:256`), which P1-D §4-3 corrects and which S-P2 must NOT over-index on the recognition plane (the ~34% emit was a recognition-plane self-time share, not the fact-stream allocation tax measured here). `push_ascii_lower_hex` (`generated.rs:628`) emits the **fnv64 source-hash hex** into the fact-stream String — it is a **FNV-diagnostic primitive with NO CSS-semantic value** (it hashes the matched source span for the fact-stream's self-describing diagnostic record; it parses nothing and carries no CSSOM information). Consistent with P1-B §4.3 and P1-C A3, it is pure diagnostic-emission overhead that disappears entirely when the fact-stream String is retired — it must not be carried into S-P2 as a primitive to optimise. The `RawVecInner::reserve` cost is the un-presized `String::new()` growth in `emit_fact_stream`/`emit_full_parse`; the O(1) tape-checkpoint lever (lever 2) and the `push_plain_offset` branchless-u32-write tape (assembler.rs:71) replace it.

4. **The `emit_fact_stream` / fact-stream String is a pre-blocked admission plane (SYNTHESIS §0.4) — profiled as a measurement, not proposed.** P1-A measures it because it IS the benched Track 1 today; the profile is the evidence that its dominant cost (String alloc) is exactly what the tape activation retires. This is not a re-open of REDRESS (the fact-stream-as-admission pre-block); it is the empirical case for retiring it.

5. **No second-substrate / sidecar observed (CH5).** The recognition scan retains nothing but `pos` + a 4-field summary; the fact-stream retains only the output String. No retained cursor, no sidecar event vector, no parallel source pass on the benched path. Lock 1 substrate-union is intact on the profiled path (there is no tape on the CSS path at all yet — the substrate is UNWIRED, as W6 found).

---

## §5 — Sources (every artefact path + run id)

Profile artefacts (uncommitted, on disk; binaries not committed per §5 of the pass):
- `/tmp/skv17-p1/css_canon_n200_v2.txt` — **canonical harness `css_canon_bench`, N=200 cold mode (the §2.1 authoritative Mbps dispersion table; V3 CH4-5 fold).**
- `/tmp/skv17-p1/css_canon_pmu_v2.txt` — **canonical harness `css_canon_bench CSS_CANON_PMU` mode, iters=2000 (the §2.1b instr/byte table + the IPC/CPI column substantiating the COST-SURFACE POSTURE).**
- `/tmp/skv17-p1/cold-N64.txt` / `cold-N80-run2.txt` — legacy `css_cold_harness` N=64/N=80 runs, retained ONLY as a cross-harness stability check (no longer authoritative per CH4-5).
- `/tmp/skv17-p1/fact-bootstrap.json.gz` (+ `.syms.json`, 92057 samples) — track1_fact bootstrap samply mode I.
- `/tmp/skv17-p1/full-bootstrap.json.gz` (+ `.syms.json`, 27180 samples) — track1_full bootstrap.
- `/tmp/skv17-p1/fact-tailwind.json.gz` (+ `.syms.json`, 93534 samples) — track1_fact tailwind.
- `/tmp/skv17-p1/full-tailwind.json.gz` (+ `.syms.json`, 18955 samples) — track1_full tailwind.
- `/tmp/skv17-p1/selftime-consolidated.txt` — symbolicated self-time dump (all 4 profiles).
- `/tmp/skv17-p1/symbolicate.py`, `/tmp/skv17-p1/extract.py` — the sidecar symbolicators.

Source under profile (benched skinny tree, master HEAD `6496fecae`):
- **Canonical harness (the single designated binary; V2 designation, V3 sole Mbps + cost source): `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`** (403 lines; N>=50 `assert!` `:250`; cold dispersion mode `:249-277` via `sample()` `:146`; `CSS_CANON_PMU` mode `:211`; samply driver `:183`) — the source for BOTH the §2.1 Mbps dispersion table AND the §2.1b instr/byte cost surface (CH4-5 fold: one designated binary end-to-end).
- Legacy Mbps-dispersion harness (NO LONGER authoritative, retained as cross-harness check only): `skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs` (366 lines). One of five cold-bench binaries authored this pass; superseded by `css_canon_bench` per CROSS X2 / CH4-5 — absolute Mbps differs ~20-29% across the five harnesses, only within-harness plane ratios are load-bearing.
- Corpus set: `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:21-58` (4 corpora) + `skinny/corpora/css-l4-sk-v14/` (979638 B).
- Track 1 fact-stream: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/parser.rs:5` → `generated.rs:5` (`emit_fact_stream`).
- Track 1 full-parse: `…/parser.rs:17` → `generated.rs:61` (`emit_full_parse`) → `CssFullParser` (`generated.rs:103`).
- Hot leaves: `find_component_delim` `generated.rs:288`, `consume_balanced_at` `:320`, `parse_stylesheet` `:118`, `parse_block` `:189`, `parse_declaration` `:242`, `push_ascii_lower_hex` `:628`.
- Prior broadcast harness (retired by this pass): `skinny/crates/bbnf-bench/src/css_l4_w8.rs` (`W8_PROFILE_ITERS=8` `:18`; `measure_full_parse_profiles` `:206`).
- NEON entry (route for the re-confirmed leaf, gated behind tape): `skinny/crates/bbnf-simd/src/dispatch.rs select_classifier:42`, `lo6_table_admissible:101`.
