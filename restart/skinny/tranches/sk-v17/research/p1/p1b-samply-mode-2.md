# SK-V17 P1-B: Benched CSS-Tape Product-Plane Profile + Canonical N>=50 Cold Harness

Pass: S-P1 Profile. Cycle: V4.
Date: 2026-05-29.
Scope: samply product-plane profiling of the BENCHED skinny CSS Track 1 path (the
emit_fact_stream String plane + the emit_full_parse recognition plane) on the four
benched CSS corpora, AND the canonical N>=50 cold per-parse measurement. Resolve every
CSS hot leaf to a symbol; report median/min/max/mean/stddev Mbps + **instr/byte** (the
PRIMARY cost density) with cyc/byte co-reported (a valid counter, IPC-explained —
see the c/B-provenance note below) for
track1(fact-stream + recognition) / lightningcss(full-CSSOM) / cssparser(token-scan)
per corpus.
Output: this file.
Baseline: SK-V17-open (master HEAD `6496fecae`; SK-V16 close `1c5bd7a25`; benched CSS
surface unchanged between them — `git diff --stat 1c5bd7a25 6496fecae -- skinny/crates`
touches only docs/generated provenance, not the CSS scan/emit hot leaves).
Host triple: aarch64-apple-darwin (Apple M5 Max). No x86.
Build flags: `cargo build --release -p bbnf-bench --bin css_canon_bench`
(`[profile.release]` carries `debug = true`, `split-debuginfo = "packed"` — root
`Cargo.toml:83`); single cargo invocation per `CARGO_TARGET_DIR=skinny/target`.
Profile tool: samply 0.13.1 (`--save-only`, then atos-symbolicated against the binary
`__TEXT` vmaddr `0x100000000` — `--save-only` does NOT pre-resolve Rust symbols, so
atos post-symbolication is mandatory per the `samply-symbol-resolution` feedback);
cost-density counters via `proc_pid_rusage(RUSAGE_INFO_V5)` `ri_instructions` (PRIMARY,
fully reliable) and `ri_cycles` (a valid core-cycle counter, co-reported — see note).

**CANONICAL HARNESS (pass-wide, CH4-3/CROSS-X2 fold):** this pass designates ONE
canonical N>=50 cold harness — `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`
(`assert!(n >= 50)` at `:250`; PMU mode; most-cited across P1-A..F). All §2.1 figures
below are produced by THAT binary (N=200 cold per-parse + a 2000-iter PMU pass). The
five V1-cycle harness binaries (`css_cold_harness`, `css_cold_bench`,
`css_cold_canonical`, `css_canon_bench`, `css_track1_profile`) gave ~20–29% absolute-Mbps
dispersion on the same plane (e.g. bootstrap track1_fact 695/736/850/720/785 across
A/B/C/E/F): **absolute Mbps is harness-, allocator-, and CPU-flag-dependent and is NOT
comparable across harnesses; only the WITHIN-harness track1-vs-comparator ratio is
load-bearing.** This artefact's earlier-cycle `css_cold_harness` N=50/N=64 run (band
458–743 Mbps, `cold-N64.txt`) corroborates the same RATIO band and is retained only as
ratio corroboration, not as an absolute number to cross-compare.

**c/B PROVENANCE (CROSS-X1' / CH4-4 fold — adopting P1-D §3.1, the ONE pass-wide
posture):** the `ri_cycles` surface measures sub-1.0 CPI (measured CPI 0.16–0.28 across
all four corpora × four workloads, `css_canon_pmu.txt`). **The V1/V2 characterization of
this as "physically impossible / a reference-clock tick / falsified" was wrong physics
and is withdrawn.** A retired-instruction CPI below 1.0 is exactly an IPC above 1.0:
CPI 0.16 ⇔ IPC 6.4, CPI 0.27 ⇔ IPC 3.7 — IPC of 3.6–6.4 is entirely physical on the
Apple M5 Max's ~8-wide out-of-order P-core for a tight, well-predicted scalar
load+compare+branch scan loop. `ri_cycles` is therefore a valid core-cycle counter
(`ri_cycles/wall ≈ 4.27 GHz steady across every workload — P1-D §3.1`), not an error.
The one caveat that DOES apply: `ri_cycles` from `rusage` is not byte-faithfully
disambiguable from a wall-proportional tick (the wall-derived steady-GHz check is
observationally identical for a fixed-frequency counter and a proportional tick), so
**cyc/byte is non-load-bearing — co-reported with IPC explicit, but every cost CONCLUSION
in this artefact is grounded on `instr/byte`** (`ri_instructions`, which is byte-faithfully
counted). The S-P2 cost model keys on instr/byte; cyc/byte rides along, IPC-annotated,
as corroboration only.
Corpus coverage: 4/4 benched CSS corpora (`css_l4_corpus.rs:21-54`:
animate / bootstrap / tailwindcss / material-components-web) + the 979638-byte
aggregate (raw byte sum 71750+232803+179631+495454 = 979638; the +3 newline
separators in the concatenated buffer are not counted in the throughput denominator,
consistent across all six artefacts). (The SK-V17 subject is CSS-tape, not JSON; the
§2.1 17-JSON-corpus mandate is the JSON-subject rows owned by P1-A/C/D/E/F — this P1-B
row is the CSS bench/measurement-formalization row per the orchestrator dispatch, which
overrides the generic PASS-1 JSON matrix for the CSS-tape subject.)

## §1 — Method (commands run; verbatim, reproducible)

The canonical harness binary `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`
(the ONE pass-wide canonical harness, CH4-3 fold) carries the N>=50 cold per-parse
measurement (`assert!(n >= 50)` at `:250`) and a PMU mode. It loads each corpus
INDIVIDUALLY (no broadcast: the W8 harness `css_l4_w8.rs:217` sums
`total_bytes * 7 profiles * 8 iters` into ONE elapsed → ONE tuple — pre-blocked,
SYNTHESIS §0.4), runs N cold samples per corpus per plane (one parse per sample,
`black_box(parse(black_box(input)))` so no warm allocation amortizes across samples),
and reports median/min/max/mean/stddev Mbps; the PMU mode reads `ri_instructions`
(byte-faithful, primary) + `ri_cycles` (valid counter, IPC-explained, co-reported but
non-load-bearing) over a 2000-iter inner loop for instr/byte.

```bash
# 1. Build (release + debug=true), single invocation, sequential.
cd skinny
CARGO_TARGET_DIR=target cargo build --release -p bbnf-bench --bin css_canon_bench
#   -> Finished `release` profile [optimized + debuginfo]

# 2. Canonical cold harness, N=200 (medians stable; assert n>=50 at :250).
./target/release/css_canon_bench bench 200 | tee /tmp/skv17-p1/css_canon_n200.txt
#   cold per-parse; median/min/max/stddev Mbps per corpus per plane.

# 3. PMU mode: instr/byte (primary) + cyc/byte (co-reported; CPI<1.0 = IPC 3.6-6.4) per plane.
./target/release/css_canon_bench pmu 2000 | tee /tmp/skv17-p1/css_canon_pmu.txt

# 4. samply self-time profiles (planes/corpora). --save-only then atos.
samply record --save-only -o /tmp/skv17-p1/samply-bootstrap-fact.json -- \
  ./target/release/css_cold_harness samply bootstrap 6000 fact
samply record --save-only -o /tmp/skv17-p1/samply-bootstrap-full.json -- \
  ./target/release/css_cold_harness samply bootstrap 16000 full
samply record --save-only -o /tmp/skv17-p1/samply-tailwind-fact.json -- \
  ./target/release/css_cold_harness samply tailwindcss 8000 fact

# 5. atos-symbolicate (samply --save-only leaves Rust frames as raw RVAs).
#    __TEXT vmaddr = 0x100000000 (otool -l | segname __TEXT | vmaddr).
python3 /tmp/skv17-p1/symb_atos.py /tmp/skv17-p1/samply-bootstrap-fact.json
python3 /tmp/skv17-p1/symb_atos.py /tmp/skv17-p1/samply-bootstrap-full.json
python3 /tmp/skv17-p1/symb_atos.py /tmp/skv17-p1/samply-tailwind-fact.json
```

(The self-time profiles in step 4 were taken with the earlier-cycle
`css_cold_harness` samply driver — symbol attribution is identical regardless of the
timing-harness shell, since both drive the same `css_decl::parser::parse` /
`parse_full` symbols; only the §2.1 timing numbers are re-grounded on the canonical
`css_canon_bench` binary.)

The two benched CSS planes profiled (both reached from `nonjson_css_l4.rs`):
- **track1_fact** = `runtime::generated_css_l4_declaration_values::parser::parse`
  (`parser.rs:5`) → `generated::emit_fact_stream` (`generated.rs:5`). This is the
  ACTUAL benched Track 1 — `track1_facts(input)` at `nonjson_css_l4.rs:596` calls
  exactly `track1::parser::parse`. It builds a fact-stream `String`.
- **track1_full** = `...::parser::parse_full` (`parser.rs:17`) →
  `generated::emit_full_parse` (`generated.rs:61`) → `CssFullParser::parse_stylesheet`
  (`generated.rs:118`). Recognition-only structural summary scan; this is the W8
  `parse_full` plane (`css_l4_w8.rs:399`).
- **lightningcss** = `StyleSheet::parse(.., ParserOptions::default())` (full CSSOM
  materialize — the fair >SOTA bar). **cssparser** = `StyleSheetParser` full
  token-walk probe (token-scan, flaw probe; same shape as `css_l4_w8.rs:275`).

## §2 — Findings (per-corpus table; file:line on every hot-leaf claim)

### §2.1 — Canonical cold throughput (`css_canon_bench`, N=200)

All Mbps are MEDIAN of 200 cold per-parse samples (`css_canon_n200.txt`). The cost
density is **instr/byte** (`ri_instructions/(bytes·iters)`, PMU mode 2000-iter,
`css_canon_pmu.txt`) — the load-bearing cost density. (cyc/byte is co-reported in P1-D
§3.2's PMU table with IPC explicit; CPI 0.16–0.28 = IPC 3.6–6.4, valid on the ~8-wide
M5 core, but non-disambiguable from a wall-proportional tick and therefore
non-load-bearing — instr/byte carries every conclusion here.) Absolute Mbps is
this-harness-specific (CH4-3); only the within-row ratio is load-bearing.

| Corpus | Bytes | Plane | median | min | max | mean | stddev | instr/B |
|---|---:|---|---:|---:|---:|---:|---:|---:|
| animate | 71750 | **track1_fact** | **645.277** | 497.365 | 726.506 | — | 47.669 | 279.27 |
| animate | 71750 | track1_full | 2138.464 | 1462.733 | 2246.945 | — | 136.957 | 57.72 |
| animate | 71750 | **lightningcss** | **1037.427** | 727.388 | 1141.911 | — | 64.328 | 155.46 |
| animate | 71750 | cssparser | 2197.483 | 1989.891 | 2330.179 | — | 112.973 | 79.58 |
| bootstrap | 232803 | **track1_fact** | **784.598** | 567.610 | 873.421 | — | 48.615 | 234.04 |
| bootstrap | 232803 | track1_full | 2113.690 | 1765.401 | 2378.699 | — | 114.179 | 53.70 |
| bootstrap | 232803 | **lightningcss** | **998.809** | 743.396 | 1063.609 | — | 52.993 | 160.14 |
| bootstrap | 232803 | cssparser | 2712.186 | 2078.211 | 2826.137 | — | 133.300 | 68.30 |
| tailwindcss | 179631 | **track1_fact** | **505.284** | 393.155 | 546.320 | — | 26.750 | 363.76 |
| tailwindcss | 179631 | track1_full | 2547.580 | 1686.262 | 2648.126 | — | 188.429 | 51.50 |
| tailwindcss | 179631 | **lightningcss** | **728.788** | 585.455 | 793.091 | — | 38.352 | 236.64 |
| tailwindcss | 179631 | cssparser | 1557.388 | 1205.157 | 1659.250 | — | 80.965 | 126.12 |
| material | 495454 | **track1_fact** | **768.236** | 635.949 | 870.713 | — | 46.113 | 214.54 |
| material | 495454 | track1_full | 2500.386 | 1611.942 | 2805.698 | — | 209.267 | 46.46 |
| material | 495454 | **lightningcss** | **1055.093** | 833.425 | 1165.774 | — | 53.663 | 137.58 |
| material | 495454 | cssparser | 2717.880 | 2112.997 | 3005.124 | — | 145.710 | 60.85 |

Aggregate instr/byte (volume-weighted across the four corpora): track1_fact **251.28**,
track1_full **49.93**, lightningcss **162.42**, cssparser **75.96**.

**Per-corpus track1_fact-vs-lightningcss ratio (the fair >SOTA gap):**
animate 0.622x · bootstrap 0.785x · tailwind 0.693x · material 0.728x. The
earlier-cycle `css_cold_harness` N=64 run (`cold-N64.txt`) reproduces the same band
(animate 0.594x, bootstrap 0.749x, tailwind 0.643x, material 0.678x, aggregate 0.630x)
on a DIFFERENT absolute-Mbps scale — confirming that the RATIO is the stable, harness-
independent quantity (~0.6–0.8x) while the absolute Mbps differs by harness (CH4-3).
The benched fact-stream Track 1 is **~1.3–1.6x slower than lightningcss full-CSSOM**,
on a per-corpus median, on the reliable instr/byte density as well: track1_fact carries
**1.46–1.80× the instr/byte of lightningcss** (the String tax, on a counter with no
CPI defect).

### §2.2 — Hot-leaf attribution: track1_fact (fact-stream String) plane

samply self-time, atos-resolved. The fact-stream plane is **ALLOCATION-BOUND**, not
scan-bound — the dominant cost is `String`/`Vec` growth + the syscalls backing it.

bootstrap (`samply-bootstrap-fact.json`, 16007 self-samples):

| Self% | Symbol (file:line) | Class |
|---:|---|---|
| 31.47 | `[libsystem_kernel.dylib]` (page-fault / madvise backing String growth) | tape/alloc |
| 26.41 | `[libsystem_malloc.dylib]` (malloc/realloc/free of the fact-stream String) | tape/alloc |
| 23.80 | `emit_fact_stream` (`generated.rs:5`; `emit_declarations`/`emit_tokens` LTO-inlined, hot `push_str` at `:45`) | string-build |
| 8.98 | `push_ascii_lower_hex` (`generated.rs:628`; `Vec::with_capacity(text.len())` per ident/function/hash/dimension token, `:629` → `push_hex` `:640`) — **FNV/hex DIAGNOSTIC encoding, NOT CSS-semantic value** (it lowercases + hex-encodes each lexeme into the fact-stream content digest; the CSS-typed path needs none of this — consistent with P1-A §4.3, P1-C A3) | string-build / alloc (diagnostic) |
| 5.95 | `[libsystem_platform.dylib]` (`_platform_memmove` for String copy) | tape/alloc |
| 1.42 | `core::fmt::num` Display (`to_string()` for idx/value_start/value_end + `format!("{:016x}")` at `:637`) | number/format |
| 0.67 | `free` (dyld-stub) | tape/alloc |
| 0.62 | `memcpy` (dyld-stub) | tape/alloc |

tailwindcss (`samply-tailwind-fact.json`, 22756 self-samples) — confirms generality,
and the allocation share is HIGHER on the hardest corpus (more, shorter declarations →
more per-token hex `Vec` allocations; reliably, instr/byte 363.76 vs bootstrap 234.04,
`css_canon_pmu.txt`):

| Self% | Symbol | Class |
|---:|---|---|
| 30.60 | `[libsystem_malloc.dylib]` | tape/alloc |
| 30.24 | `[libsystem_kernel.dylib]` | tape/alloc |
| 19.59 | `emit_fact_stream` (`generated.rs:5`) | string-build |
| 8.67 | `[libsystem_platform.dylib]` | tape/alloc |
| 7.53 | `push_ascii_lower_hex` (`generated.rs:628`) | string-build / alloc |
| 1.37 | `core::fmt::num` Display (`to_string`) | number/format |

**Allocation/memory total: bootstrap ~64.5% (kernel 31.5 + malloc 26.4 + platform 6.0
+ free/memcpy/malloc-stub ~1.5), tailwind ~70.3%.** The string-building leaves
(`emit_fact_stream` + `push_ascii_lower_hex`) are themselves the producers of that
allocation. This is the direct empirical confirmation of SYNTHESIS §0.4 pre-block #3:
"the dominant benched-CSS-track1 cost is this String building; the tape append replaces
it." The W1/W2 lever (kill `emit_fact_stream` String, append into the skinny
`TapeBuilder` `push_plain_offset` = one branchless u32 write, `assembler.rs:71`)
attacks exactly this ~65–70% allocation floor.

### §2.3 — Hot-leaf attribution: track1_full (recognition scan) plane

samply self-time, bootstrap (`samply-bootstrap-full.json`, 12947 self-samples). This
plane is **SCAN-BOUND** (negligible alloc: malloc+kernel < 0.1%), and it re-confirms
the inherited core-tree figures ON THE BENCHED SKINNY PATH (per the HANDOFF
`S-P1-re-confirm-on-benched-path` obligation — these are no longer inherited, they are
measured here):

| Self% | Symbol (file:line) | Class | Inherited core-tree figure |
|---:|---|---|---|
| 56.55 | `find_component_delim` (`generated.rs:288`; `delimiters.contains(&byte)` linear byte-class membership test per source byte, `:295`; `pos = match byte` dispatch `:298`; `_ => pos+1` `:307`) | scan | architecture profile ~56% — **CONFIRMED** |
| 28.32 | harness wrapper `track1_full_parse` (`css_canon_bench.rs:103-105` / equivalently `css_cold_harness::track1_full`): the `parse_full` call + `out.len()` + `black_box` + the LTO-inlined `sample()`/`main()` outer measurement loop — **pure timing scaffold, NOT a retained/second parse pass** (CH5-V1-R1; matches P1-C/P1-D/P1-F) | dispatch/harness scaffold | n/a (harness-local) |
| 11.51 | `consume_balanced_at` (`generated.rs:320`; nested `(`/`[`/`{` balance scan, recursive) | scan | inherited ~10% — **CONFIRMED** |
| 2.60 | `parse_block` (`generated.rs:189`) | structural | — |
| 0.89 | `parse_declaration` (`generated.rs:242`) | structural | — |
| 0.07 | `parse_at_rule` (`generated.rs:137`) | structural | — |

The two scalar scan leaves `find_component_delim` + `consume_balanced_at` together are
**68.1% of in-binary self-time** on the recognition plane (excluding the harness
wrapper: 56.55+11.51 of the ~72% non-wrapper). The hot inner operation is
`&[u8]::contains(&byte)` (`generated.rs:295`, `:298` `match byte` arm), a per-byte
linear membership scan over a 2–4-byte delimiter set — the exact structural scan the
W4 NEON `byte_class_index_64` / `to_bitmask64` movemask cascade
(`bbnf-simd/src/dispatch.rs` `select_classifier:42`) replaces with a single SIMD
classify+movemask. **CH2 (grammar-neutral primitive callout):** this leaf is the
byte-class-membership scan primitive — `delimiters.contains(&byte)` over a 2–4-byte
delimiter set — NOT a CSS-named role. It is the SAME byte-class-membership primitive
JSON's structural scan runs through `select_classifier` / `PrimitiveKernels`
(verified `skinny/crates/runtime/src/grammars/json/scan.rs:219`, the JSON
`classify_structural_terminator_block_from_table` lo6 byte-class scan); the candidate NEON kernel
`byte_class_index_64` / `to_bitmask64` (a TO-BUILD target, not yet a symbol in
`dispatch.rs`) is named here as grounded in this measured 56.55% leaf, gated behind
tape activation. S-P2 inherits it as a generic scan primitive, free to ask whether it
generalises to CSS L4 / Sheets / BBNF-self.

### §2.4 — The canonical N>=50 cold harness (ONE pass-wide binary; retires W8 broadcast + W6 single-sample)

`skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` is the ONE canonical harness for
the whole S-P1 pass (CH4-3 / CROSS-X2 fold — the five V1-cycle binaries are superseded;
all §2.1 numbers above are produced by this one):

- **N>=50 enforced**: `assert!(n >= 50, "N must be >= 50 ...")` at `:250` (telemetry-
  honesty gate; SYNTHESIS Section 2 rejects `css_sample_count < 50`).
- **MEDIAN reported** (plus min/max/stddev); not mean, not single-sample.
- **Cold per-parse**: each sample times exactly one `parse(black_box(input))` under
  `black_box`, no warmed cache and no allocation amortized across samples
  (`no-warm-benches`); the parse allocates its own fact-stream String per call.
- **Per-corpus, NO broadcast**: each of the 4 corpora is loaded and timed
  individually (retires the W8 `css_l4_w8.rs:217` one-elapsed-over-
  4-corpora×7-profiles×8-iters broadcast — SYNTHESIS §0.4 24-row pre-block). The
  979638-byte aggregate is reported separately (volume-weighted instr/byte above).
- **Both benched CSS planes**: `track1_fact_stream` (`css_canon_bench.rs:108-110`,
  `css_decl::parser::parse`/`emit_fact_stream`, the live benched plane) AND
  `track1_full_parse` (`:103-105`, `parse_full`/`emit_full_parse`, the recognition
  plane). The comparators `lightningcss_full_cssom` (`:113-115`, full-CSSOM, the fair
  bar) and `cssparser_token_scan` (`:118-120`, token-scan, flaw probe) are same-run,
  same-plane-disclosed.
- **PMU mode**: `css_canon_bench pmu <iters>` reads `ri_instructions` (primary,
  byte-faithful) + `ri_cycles` (valid counter, co-reported non-load-bearing) for
  instr/byte; `bench <N>` runs the cold timing.

This harness is the executable substrate S-P3 binds the `--skv17-css-sota-report` gate
consumer onto (SYNTHESIS Section 2). It does NOT yet wire lightningcss as the
materializing comparator via a shared report struct — it measures lightningcss
full-CSSOM inline (`StyleSheet::parse` building the CSSOM) on the same run, which is
the §0.6 full-cssom plane; S-P3 must additionally re-prove the EXACT 8-field cssparser
structural equality on the post-tape typed path before any speed admission (that
equality gate is NOT this harness's job; this harness measures speed only).

**Harness-comparability caveat (CH4-3):** because absolute Mbps differs by harness/
allocator/CPU-flag (~20–29% same-plane dispersion across the five V1 binaries), S-P2/
S-P3 must consume only WITHIN-`css_canon_bench` track1-vs-comparator ratios — never a
raw Mbps number cross-compared against a different harness's Mbps.

## §3 — Delta vs SK-V16 close (per row; Mbps + instr/byte + classification)

There is **no admitted per-corpus typed-CSS row in SK-V16** to delta against: the 24
`css_l4/*/direct_to_struct/main` RESULTS rows (lines 112-135) are all
`not_admitted:SK-V15-W0-broadcast-diagnostic` / `AUDIT-FALSIFIED`, carrying ONE
broadcast tuple `track1=2319.041 / cssparser=2362.037 / lightningcss=929.281` across
all 24 (SYNTHESIS §0.2). So the delta is against the prior committed CSS measurement
*planes*, not RESULTS rows. The decisive S-P1 finding is a **plane reconciliation**:

| Plane (prior committed figure) | Prior Mbps | This-run cold per-corpus | Classification |
|---|---:|---|---|
| `css_l4_w6_typed_retime` Track 1 typed CSSOM (`w6-speed-report.md:164`) | **3.093** | n/a — NOT the benched `parse` path; a separate retime workload | the 269x/14x slowdown source — NOT the benched plane |
| fact-stream "profile plane" (alphaB `~70 Mbps`, 51–164/corpus) | ~70 | **505–784 median** (4/4) | the same `emit_fact_stream` fn; prior ~70 was W8-broadcast-diluted (7 grammar profiles × 4 corpora summed) |
| W8 broadcast track1 (`929.281` lightningcss tuple) | 929.281 (lcss) | lightningcss **729–1055 median/corpus** | broadcast retired; per-corpus split now measured |
| lightningcss full-CSSOM (`w6-speed-report.md:59` 833.199 scrutineer) | 793–833 | **729–1055 per-corpus** | same-harness re-baseline; harness-dependent absolute, within ratio band |
| cssparser token-scan (`w6-speed-report.md:58` 2476) | 2476–2539 | **1557–2718 per-corpus** | flaw probe; within band |

**Load-bearing delta:** the benched Track 1 fact-stream plane is **~0.6–0.8x
lightningcss per corpus (≈1.3–1.6x slower), NOT ~14x (build plane) or ~269x
(typed-retime plane)**. The catastrophic prior multiples (3.093 Mbps → 269x) belong to
the `css_l4_w6_typed_retime` workload, which is NOT the path `nonjson_css_l4.rs:596`
benches. The contract's own §0.2 already flags the lightningcss number as run-dependent
(793/833/929/974, "no single committed measurement equals 974").

**The String tax, on the load-bearing counter (CROSS-X1' re-derivation):** the V1
claim "~3× of the fact-stream cycles are String building" was a cyc/byte inference;
cyc/byte is non-load-bearing (a valid IPC-explained counter, but non-disambiguable from
a wall-proportional tick — see the c/B-provenance note), so the tax is re-derived on the
byte-faithful `ri_instructions` density (`css_canon_pmu.txt`): the fact-stream plane retires
**214–364 instr/byte** vs the recognition plane's **46–58 instr/byte** — a
**4.36×–7.06× per-corpus instruction-count multiple** (aggregate 251.28 vs 49.93 =
**~5.0×**). So the fact-stream String-build + FNV/hex encoding adds roughly 4.4–5×
the per-byte instruction count of the bare recognition scan. This is consistent with —
and now reliably grounds — the §2.2 ~65–70% allocation/string-build self-time share,
on a counter with no CPI defect.

## §4 — Anomalies + masking signals (flagged for S-P2)

1. **The two benched CSS planes have ORTHOGONAL hot leaves** (flagged for S-P2 lever
   sequencing): track1_fact is ~65–70% allocation-bound (String/Vec growth +
   syscalls), track1_full is ~68% scan-bound (`find_component_delim` +
   `consume_balanced_at`). The W1/W2 tape-activation lever attacks the fact-stream
   allocation floor; the W4 NEON structural-index lever attacks the recognition scan.
   They are NOT the same hot leaf — S-P2 must not conflate them. After W2 kills the
   fact-stream String (tape append replaces it), the residual hot leaf on the typed
   tape path will SHIFT toward the `find_component_delim` scan that today is masked
   under the allocation cost — i.e. the W4 NEON lever becomes load-bearing only AFTER
   W2 unmasks it. This is a MASKING signal: the scan cost (~68% of the recognition
   plane) is currently MASKED on the fact-stream plane by the dominant allocation.

2. **`find_component_delim` ~56% / `consume_balanced_at` ~11.5% RE-CONFIRMED on the
   benched skinny path** (HANDOFF Next-Move §2 obligation discharged). The inherited
   core-tree figures (architecture profile ~56% / ~10%) are NOT assumed — they are
   measured here at 56.55% / 11.51% on `runtime::generated_css_l4_declaration_values`.
   The W4 NEON kernel is therefore antecedent-grounded: it has a fresh P1 hot-leaf
   (ORCHESTRATOR §8, profile-first non-negotiable). Note: this is the RECOGNITION
   plane; on the post-W2 typed-tape plane the leaf must be re-profiled again (the
   typed scan walks the same `find_component_delim` but emits offsets into the tape).

3. **`push_ascii_lower_hex` is a FNV/hex DIAGNOSTIC encoder with NO CSS-semantic value**
   (`generated.rs:628-633`; `Vec::with_capacity(text.len())` at `:629` then lowercase
   loop `:630-632` then `push_hex` `:633`→`:640`). It lowercases each ident/function/
   hash/dimension lexeme and hex-encodes it into the fact-stream content digest — it
   exists ONLY to make the fact-stream String a stable comparison/hash artefact; it
   carries no value the CSS-typed tape path needs (consistent with P1-A §4.3, P1-C A3).
   It is a per-leaf eager allocation inside the fact-stream emit, NOT the AZ-IV
   eager-value-tree (it builds a hex String, not a `Box<CssColor>` tree), but it is the
   same anti-pattern the tape eliminates: the tape stores the lexeme as a `(start,end)`
   offset pair (`push_plain_offset`), decoding lazily via `ValueRef` only on demand and
   never hex-encoding at all. Flagged for S-P2 as a per-token alloc + diagnostic encode
   that the tape retires WHOLESALE (not merely lazily) — its ~9% fact-stream self-time
   is pure diagnostic tax, removed not deferred.

4. **`core::fmt::num` Display (`to_string`/`format!`) ~1.4%** in the fact-stream emit
   (decl idx, value_start, value_end, fnv64 `{:016x}`). Small but pure overhead of the
   String text encoding; the tape stores these as raw `u32` offsets, no formatting.

5. **Pre-blocked-route check (CH3):** none of the §4 observations re-propose a blocked
   route. The tape-append-replaces-String conclusion is the SYNTHESIS §0.4 pre-block #3
   DIRECTION (retire the fact-stream String), not a re-opening — it is the mandated W1/
   W2 lever, with this profile supplying its hot-leaf antecedent. The NEON conclusion
   is the §0.1 NEON-gate DIRECTION, gated behind tape activation (the structural index
   has nothing to pre-scan into until the tape decodes CSS). No REDRESS 50-55 / 60-72 /
   80 / 82-84 / 88 / 89 / 127 route is implied: no second substrate, no sidecar, no
   registry, no eager value tree, no fixture/FNV contrivance, no x86 is proposed.

6. **Absolute Mbps is harness-dependent; only the within-harness RATIO is load-bearing
   (CH4-3 / CROSS-X2).** The five V1-cycle harness binaries gave ~20–29% same-plane
   absolute-Mbps dispersion (bootstrap track1_fact 695/736/850/720/785 across A/B/C/E/F),
   beyond per-run stddev — this is harness/allocator/CPU-flag divergence, not host
   noise alone. The within-harness track1-vs-lightningcss RATIO is stable across
   harnesses (~0.6–0.8x on both `css_canon_bench` N=200 and `css_cold_harness` N=64),
   because both planes share the SAME denominator within a run. Therefore S-P3's gate
   must key on the per-corpus median RATIO crossing 1.0x within `css_canon_bench` (the
   ONE canonical harness), with the stddev disclosed — never on a raw Mbps number
   cross-compared against a different harness. Cold single-parse timing also carries
   ~10% per-run host variance on the median; N>=50 + min/max/stddev disclosure makes
   it legible.

## §5 — Sources (every artefact path + run id)

Canonical-harness measurement artefacts (on disk; not committed):
- `/tmp/skv17-p1/css_canon_n200.txt` (the canonical `css_canon_bench` N=200 cold
  per-parse run; §2.1 Mbps medians).
- `/tmp/skv17-p1/css_canon_pmu.txt` (the canonical `css_canon_bench` PMU run, iters=2000;
  the `ri_instructions` instr/byte source AND the `ri_cycles` counter — every row carries
  a sub-1.0 CPI, i.e. IPC 3.6–6.4, valid on the ~8-wide M5 P-core; cyc/byte co-reported
  but non-load-bearing per the c/B-provenance note).
- `/tmp/skv17-p1/cold-N64.txt` (earlier-cycle `css_cold_harness` N=64; retained ONLY as
  ratio corroboration on a different absolute-Mbps scale — not cross-compared).
- `/tmp/skv17-p1/samply-bootstrap-fact.json` (419669 B; 16007 self-samples;
  track1_fact plane, bootstrap × 6000).
- `/tmp/skv17-p1/samply-bootstrap-full.json` (305897 B; 12947 self-samples;
  track1_full recognition plane, bootstrap × 16000).
- `/tmp/skv17-p1/samply-tailwind-fact.json` (570605 B; 22756 self-samples;
  track1_fact plane, tailwindcss × 8000).
- `/tmp/skv17-p1/symb_atos.py` (self-time extractor + atos batch symbolicator).

Source / symbol citations (benched skinny tree):
- Canonical harness (ONE pass-wide; CH4-3 fold):
  `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` — `assert!(n>=50):250`,
  `track1_full_parse:103`, `track1_fact_stream:108`, `lightningcss_full_cssom:113`,
  `cssparser_token_scan:118`.
- Benched Track 1 entry: `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:596`
  (`track1_facts` → `track1::parser::parse`); alias `nonjson_css_l4.rs:26`
  (`generated_css_l4_declaration_values as track1`).
- Fact-stream hot leaves: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/
  generated.rs` — `emit_fact_stream:5`, `emit_declarations:411`, `emit_tokens:472`,
  `push_ascii_lower_hex:628`, `push_hex:640`, `push_hex64:636`, `fnv64:619`.
- Recognition hot leaves: same file — `emit_full_parse:61`, `CssFullParser:106`,
  `parse_stylesheet:118`, `parse_at_rule:137`, `parse_qualified_rule:170`,
  `parse_block:189`, `parse_block_item:209`, `parse_declaration:242`,
  `skip_ws_comments:263`, `find_component_delim:288` (`delimiters.contains:295`),
  `consume_balanced_at:320`, `consume_string_at:353`, `consume_comment_at:342`.
- Parser dispatch: `.../css_l4_declaration_values/parser.rs:5` (`parse`), `:17`
  (`parse_full`).
- Tape substrate (W2 target, untouched today): `skinny/crates/runtime/src/tape/
  assembler.rs:42` (`TapeBuilder`), `:71` (`push_plain_offset`); `mod.rs:38`
  (`PayloadArena`), `:94` (`Tape`), `:175` (`ValueRef`), `:227` (`DocumentView`).
- NEON entry (W4 target): `skinny/crates/bbnf-simd/src/dispatch.rs:42`
  (`select_classifier`), `:101` (`lo6_table_admissible`).
- JSON byte-class-membership scan (the SAME primitive `find_component_delim` is, CH2):
  `skinny/crates/runtime/src/grammars/json/scan.rs:219`
  (`classify_structural_terminator_block_from_table`, lo6 byte-class table at `:214`).
- Corpus set: `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:21-54`
  (4 corpora + manifest sizes 71750/232803/179631/495454; aggregate 979638).
- W8 broadcast (retired): `skinny/crates/bbnf-bench/src/css_l4_w8.rs:217`
  (`profiled_bytes = total_bytes * 7 * 8`), `:91` (`run_production_attempt`).
- Prior planes: `restart/audit/skinny-impl-overfit/sk-v16-w6-speed-report.md:58-59,164`
  (cssparser 2476 / lightningcss 833 / typed-retime 3.093);
  `restart/skinny/tranches/sk-v17/research/alpha/alphaB-competitor-deltas.md:112-115`
  (prior ~70 / ~974 / ~2539 canonical band).
- PMU surface: `proc_pid_rusage` / `RUSAGE_INFO_V5` — `ri_instructions` (PRIMARY,
  byte-faithful, the instr/byte source) + `ri_cycles` (a valid core-cycle counter:
  CPI 0.16–0.28 in `css_canon_pmu.txt` = IPC 3.6–6.4 on the ~8-wide M5 P-core; per P1-D
  §3.1, but non-disambiguable from a wall-proportional tick, so cyc/byte is co-reported
  yet non-load-bearing). Same surface as
  `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:55`.
- Build profile: root `Cargo.toml:78-95` (`[profile.release]` debug=true,
  `[profile.bench]` debug=true).
