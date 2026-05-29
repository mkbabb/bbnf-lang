# SK-V17 S-P1 PROFILE — HARDENING CONSOLIDATED (V4, converged)

Pass: S-P1 Profile. Cycles: V1 → V2 → V3 → V4. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/research/p1/{p1a-samply-mode-1, p1b-samply-mode-2,
p1c-samply-mode-3, p1d-pmu-cycles, p1e-hot-leaf-attribution, p1f-bench-canonical}.md`.
Baseline: SK-V17-open, master HEAD `6496fecae` (SK-V16 close `1c5bd7a25`).
Host: aarch64-apple-darwin, Apple M5 Max. Build: release + `debug=true` + packed split-debuginfo.
Authority: `restart/prompts/skinny/PASS-1-PROFILE.md` §3 (CH1–CH6) + §3Z; `restart/prompts/ORCHESTRATOR.md`
§3W (lens registry, monotonically extended to CH7 OVERFIT-PRUNE) / §3Z (convergence: ≥95% ACCEPT × 2
consecutive cycles, zero open critical defect, zero orphan REVISE, V ≤ 5).

## §1 — Convergence ledger

| Cycle | ACCEPT rate | Open REVISE | Open REJECT | Note |
|---|---:|---:|---:|---|
| V1 | **86.8%** | several (CH3-R1, CH4-1×3/2/3, CH5-V1-R1) | 1 (P1-E fabricated line cites) | below bar; gross paper-closes (0-byte atos, missing PMU) |
| V2 | **90.5%** | 4 (single-rooted CH4-4 `ri_cycles`/CPI posture; CH5-V2-R1 line cite) | 1 ROOT (the c/B "physically impossible" interpretation split) | below bar; X1 posture inversion surfaced |
| V3 | **100.0%** | 0 | 0 | first cycle ≥95%; X1′/X2/X3 all folded |
| V4 | **100.0%** | 0 | 0 | second consecutive cycle ≥95%; fresh re-grounded run (`css_canon_n200_v4.txt`, `css_canon_pmu_v4.txt`) |

**Converged = TRUE.** V3 and V4 both clear ≥95% across every lens with zero open
critical defect and zero orphan REVISE. V4 is not a paper-carry of V3: the six artefacts
were re-emitted at Cycle-V4 frontmatter on a fresh measurement run, and each lens
re-resolved every load-bearing symbol/line against source at HEAD `6496fecae` and
re-checked the fresh PMU log rather than inheriting the V3 verdict (profile-first
discipline, ORCHESTRATOR §8). CH7 independently re-fired the N<50 gate (panics at
`css_canon_bench.rs:250`) and re-ran the harness at N=60, reproducing every BEATS ratio.

Per-lens V4 ACCEPT (all at or above the §3Z two-consecutive-cycle bar):

| Lens | V1 | V2 | V3 | V4 | V4 count |
|---|---:|---:|---:|---:|---|
| CH1 CORRECTNESS | — | — | 100% | **100%** | 6/6 artefact-level (10 dispositions incl. X1′/X2/X3/X4) |
| CH2 GENERALITY | 90.5% | 100% | 100% | **100%** | 61/61 sections |
| CH3 REGRESSION | 97.3% | 100% | 100% | **100%** | 42/42 |
| CH4 COST | 87.8% | 90.5% | 100% | **100%** | 42/42 |
| CH5 HIDDEN-COUPLING | 96.2% | 96.3% | 100% | **100%** | 32/32 |
| CH6 ANTI-PAPER-CLOSE | — | — | 100% | **100%** | 25/25 |
| CH7 OVERFIT-PRUNE | 77.8% | 100% | 100% | **100%** | 11/11 |

## §2 — §3Z VERDICT

**S-P1 SK-V17 CONVERGES at V4.** The aggregate CHALLENGE returns ≥95% ACCEPT for two
consecutive cycles (V3 100%, V4 100%), zero open REJECT, zero open REVISE, zero orphan
disposition, and V (=4) is within the V≤5 ceiling. Every per-lens axis independently
satisfies the two-consecutive-≥95% condition. All prior cycle defects are FOLDED and
re-verified closed in V4:

- **X1′ (the V2 ROOT) — RESOLVED.** The single pass-wide cost-surface posture is carried
  verbatim by all six artefacts: **instr/byte is the sole load-bearing cost density**
  (`ri_instructions` reliably retired-counted, reproducible to <0.5% across runs);
  cyc/byte is co-reported RAW with IPC made explicit but **non-load-bearing** — because
  `proc_pid_rusage.ri_cycles` cannot be disambiguated as dynamic core-cycles vs a
  wall-proportional scaled tick from that interface alone, NOT because sub-1.0 CPI is
  impossible. The V2 "physically impossible / falsified" framing is withdrawn (CH1 §2,
  CH4 §3, CH6 §1.3, CH7 §3 all confirm the retraction with no live broken cross-cite). The
  16 fresh-V4 PMU rows span CPI [0.158, 0.277] ⇔ IPC 3.6–6.3, physical on the ~8-wide M5
  Max P-core.
- **X2 (single canonical harness) — RESOLVED.** `css_canon_bench.rs` is THE harness (sole
  binary with the `assert!(n >= 50)` gate `:250` + PMU mode + samply driver); the five
  competing W6/W8 harnesses are retired. Within-harness same-run ratio is the only
  load-bearing comparison; demonstrated stable across THREE independent runs.
- **X3 (aggregate byte count 979638) — RESOLVED.** wc-verified four-corpus sum; the 981623
  `wc -c total` divergence is the 1985-byte `manifest.md` fold, stated by P1-F §1.3.
- **V1 P1-E fabricated-line REJECT — CLEARED** (cites now `:103/:146-159/:123-128/:183-207`,
  all source-exact). **CH5-V1-R1 / CH5-V2-R1 line cites — FOLDED** (`:43`→`:103-105`).
  **CH3 R-CH3-1 (REDRESS-51/53 tokenize-once boundary) — RESOLVED** and re-verified present.

The profile is reproducible by construction, every hot leaf resolves to a real
samply/atos symbol at the cited `file:line`, every Mbps is an N≥50 cold-per-parse median
with min/max/stddev, both comparator planes are correctly classed (lightningcss =
materializing full-CSSOM, PROVEN by ~30% typed node build+drop in its own flame;
cssparser = token-scan, all `()` associated types), and the recognition-plane
"BEATS lightningcss" margin is uniformly disclosed as recognition-only (does NOT
discharge the preserve-rich-ast typed gate). No JSON-corpus skip is a defect — the
17-corpus PASS-1 §2.1 mandate binds the JSON subject; SK-V17's subject is the CSS-tape
plane, scoped to the 4 sha256-pinned production sheets per SYNTHESIS §0.5.

## §3 — The LOCKED profile

### 3.1 Canonical bench — N≥50 cold per-parse, median (Mbps), fresh V4 run

Harness: `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` (N≥50 code-asserted `:250`;
cold per-parse `fn sample :146`, one timed `parse(black_box(input))`, result dropped, only
an untimed source-page pre-touch outside the window). Source: `/tmp/skv17-p1/css_canon_n200_v4.txt`
(N=200). track1 = the benched CSS planes; lightningcss = `StyleSheet::parse` full-CSSOM;
cssparser = `CssparserFullParseProbe` token-scan. Aggregate corpus bytes = **979638**
(animate 71750 + bootstrap 232803 + tailwindcss 179631 + material-components-web 495454).

| Corpus | bytes | track1_full_parse | track1_fact_stream | lightningcss (full-CSSOM) | cssparser (token-scan) |
|---|---:|---:|---:|---:|---:|
| bootstrap | 232803 | **2272.923** | 851.021 | 1110.169 | 2900.407 |
| tailwindcss | 179631 | **2576.509** | 559.480 | 833.786 | 1731.253 |
| material-components-web | 495454 | **2590.116** | 874.902 | 1261.148 (min 160.300 cold outlier) | 3248.159 |
| animate | 71750 | **2493.164** | 741.702 | 1237.346 | 2643.127 |

Within-harness ratio vs lightningcss (the only load-bearing comparison, same-run):

| Corpus | full_parse ÷ lcss | fact_stream ÷ lcss |
|---|---:|---:|
| bootstrap | **2.05× BEATS** | 0.77× L |
| tailwindcss | **3.09× BEATS** | 0.67× L |
| material | **2.05× BEATS** | 0.69× L |
| animate | **2.01× BEATS** | 0.60× L |

Ratio stability across 3 runs (full÷lcss): bootstrap 2.12/2.25/2.05; tailwind 3.50/3.00/3.09;
material 2.37/2.11/2.05; animate 2.06/1.97/2.01 — every corpus stays decisively >1.0× every
run; fact_stream stays decisively <1.0× every run.

**PMU instr/byte (the sole reliable cost-density figure)** — `/tmp/skv17-p1/css_canon_pmu_v4.txt`,
`CSS_CANON_PMU=1`, V4 re-run reproduces V3 to <0.5%:

| Plane | instr/byte range | rank |
|---|---|---|
| track1_full_parse | **46.46–57.72** | cheapest (below even cssparser) |
| cssparser | 60.86–126.12 | — |
| lightningcss | 137.63–236.61 | — |
| track1_fact_stream | **214.56–364.51** | most expensive — the ~4.4× String-building + allocation tax over full_parse |

### 3.2 Outcome classification (P1-F §3.2)

| Plane | Outcome | Verdict |
|---|---|---|
| CSS full-parse vs lightningcss | **A (admit-shaped)** | already > bar, but recognition-only (4-field `CssFullParseSummary`), NOT preserve-rich-ast → does NOT by itself discharge the SK-V17 typed gate |
| CSS fact-stream vs lightningcss | **L (loss)** | 0.60–0.77×, String-tax bound |
| CSS eager-typed plane | **K (pre-blocked)** | AZ-IV eager-value-tree, SYNTHESIS §0.4 |
| "~70 Mbps / ~14×" prior narrative | **N-direct** | no fresh benched antecedent; FALSIFIED — neither benched plane is ~70 Mbps (the only ~3 Mbps figure is the pre-blocked eager retime) |

### 3.3 Resolved CSS hot leaves (%self-time + symbol + candidate primitive)

Two benched Track-1 planes over ONE grammar module
(`skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`); the tape
substrate (`runtime/src/tape/`) and the SIMD dispatch (`bbnf-simd/src/dispatch.rs
select_classifier`) are **dormant-on-CSS / live-on-JSON** — grep of
`TapeBuilder|ValueRef|PayloadArena|crate::tape` over `grammars/css_l4_*/` returns ZERO
(`tape_activated = false`), over `grammars/json/` returns 6 files. No `number` / `unicode`
/ `dispatch` / `tape` leaf is hot on either CSS plane.

**Recognition plane (`track1_full_parse`, `full_parse.json.gz`, 5684 leaf samples):**

| %self | symbol | file:line | class | candidate primitive it grounds |
|---:|---|---|---|---|
| **59.24** (P1-E N=100: 56.52) | `CssFullParser::find_component_delim` | `generated.rs:288-311`; hot at **:295 `delimiters.contains(&byte)`** byte-membership scan leaf, `:293` loop test, `:294` byte load, `:298` `pos = match byte` dispatch, `:307` advance | **scan** (byte-class-membership over a runtime `&[u8]` delimiter slice `b";{}"`/`b"{};"`/`b":{};"`) | NEON `byte_class_index_64` / `to_bitmask64` movemask-cascade routed through `select_classifier` (`dispatch.rs:42`) / `PrimitiveKernels` — the SAME byte-class primitive JSON's structural scan runs through at `json/scan.rs:219`; **named to-build (verified ABSENT as extant symbols), gated behind tape activation** |
| **10.31** (P1-E: 11.05) | `CssFullParser::consume_balanced_at` | `generated.rs:320-340`; hot at :327 `pos = match byte` | **structural-over-scan** | folds into the SAME single NEON byte-class-scan target — its inner loop `:322-338` is byte-for-byte the same `while pos<len` + per-byte `match` as `find_component_delim :293-308`, differing only in the membership test (`byte == close :324` vs `delimiters.contains :295`) → ONE NEON target, not two |
| 28.87 + 2.45 | `parse_stylesheet`/`parse_block`/`parse_block_item` (inlined) | `generated.rs:118/189/209` | structural (recognition control loop) | — |
| 26.74 (full_parse.json.gz function rollup) | `track1_full_parse` harness wrapper | `css_canon_bench.rs:103-105` | tape/structural — **PURE measurement scaffold**, maps to `emit_full_parse generated.rs:61` in prod; NOT a retained/second pass | — |

Combined scan leaves = **~69%** of in-binary self-time, all ONE byte-class-membership
primitive. P1-D §2.5 notes each declaration body is walked 2–3× by the SAME primitive
(`parse_block_item:211 b"{};"` → `find_colon_before:314 b":{};"` → `parse_declaration:247
b";}"`) — a tokenize-once target bounded to the REDRESS-53 single-substrate shape, not a
parser-local second cursor.

**Typed (fact-stream) plane (`track1_fact_stream`, `fact_stream.json.gz`, 9711 leaf samples):**

| %self | symbol | file:line | class | candidate primitive it grounds |
|---:|---|---|---|---|
| 32.60 + 25.13 + 6.71 = **~64%** | `libsystem_kernel` / `libsystem_malloc` / `libsystem_platform` (page-zero / `String` realloc+free / memcpy) | — | **tape/alloc** | the fact-stream `String` allocation floor; 91% reached FROM `emit_fact_stream` String growth (P1-E caller-walk) — grammar-neutral `String` growth, not CSS logic |
| 25.01 (P1-E: 24.59) | `emit_fact_stream` (inlines `emit_declarations`) | `generated.rs:5,45,411` | **string** (per-decl/-token `push_str` + `to_string`) | **lever-1 (kill the String) / lever-2 (`TapeBuilder::push_plain_offset`, `assembler.rs:71`, one branchless u32 push into the EXISTING `self.offsets`)** — retires the ~64% alloc floor; NOT a second substrate (HANDOFF :171-174) |
| 8.98 (P1-E: 9.11) | `push_ascii_lower_hex` | `generated.rs:628-634` (hot :633 `push_hex`) | **string** | **NONE — FNV/hex DIAGNOSTIC encode with NO CSS-semantic value**; vanishes wholesale with tape activation; explicitly must NOT be carried into S-P2 as a primitive (FNV bench-only, HANDOFF :165) |

**Comparator (lightningcss full-CSSOM, `lightningcss.json.gz`, 13583 leaf samples)** —
profiled to PROVE the fair bar materializes: ~38% cssparser tokenizer (`consume_name`
8.92%, `skip_whitespace` 5.88%, `next_token` 5.36%) + ~30% typed node build+drop
(`parcel_selectors::parser::parse_selector` 5.04%, `lightningcss::declaration::parse_declaration`
4.16%, `drop_in_place::<cssparser::Token>` 3.95%, `PropertyId::from_name_and_prefix` 2.39%).
Symbol path stays disjoint from Track-1 `generated_css_l4_*` (CH5 I3).

**Orphan-blocked (no benched CSS antecedent):** the udot/i8mm digit kernel
`parse_4_digits_dotprod` (`bbnf-simd/src/aarch64/digit_mac.rs:27`, C4b) — zero digit-parse
self-time on either CSS plane (recognition counts, it does not decode dimensions); never
reached (no `bbnf_simd` frame in any CSS profile). Re-admission condition: re-profile the
typed lazy-`ValueRef` path AFTER W1/W2; S-P2 must NOT inherit a CSS digit-kernel hypothesis
from this profile (profile-first non-negotiable, ORCHESTRATOR §8).

### 3.4 What S-P2 inherits as primitive antecedents

1. **ONE byte-class-membership scan primitive** (`find_component_delim` + `consume_balanced_at`,
   ~69% of recognition self-time) → NEON `byte_class_index_64`/`to_bitmask64` via
   `select_classifier`, grammar-neutral, shared with JSON's `json/scan.rs:219`; **gated
   behind tape activation** (no structural index to pre-scan into until the tape decodes CSS).
2. **The fact-stream String allocation floor** (~64% alloc + 25% `emit_fact_stream`) →
   tape append (`push_plain_offset`, `assembler.rs:71`) over the EXISTING `Tape`/`ValueRef`;
   the cost SK-V17's tape activation removes. The lever order is **tape FIRST, then NEON on
   the surviving scan** — S-P2 must not invert it (the scan is masked by the String floor on
   the typed plane).
3. The 4.4× instr/byte gap between fact_stream (215–365 i/B) and full_parse (46–58 i/B) is
   the quantified target: land a TYPED (preserve-rich-ast) plane at full-parse-like cost via
   the tape, WITHOUT the eager-tree (K) regression.

## §4 — Residual REVISE

**NONE.** Zero open REVISE, zero open REJECT, zero orphan disposition across all seven
lenses at V4. Two non-blocking ACCEPT-with-note items recorded for an optional V5 tidy
(neither gates convergence; both are disclosed-and-stable, no claim is wrong):

- **X4 / CH1 + CH6 cosmetic provenance nit.** P1-A frontmatter line 10 retains a "V3"
  meta-label on a Cycle-V4 artefact, and P1-A/P1-B/P1-E cite the V2/V3 run files
  (`css_canon_n200_v2.txt`, `css_canon_pmu_v2.txt`, `css_canon_n100.txt`) while P1-F cites
  the fresh `_v4` run. All cited run files exist on disk and reproduce their numbers to the
  decimal; the load-bearing instr/byte is stable across snapshots to ≤0.2%; no verdict
  flips. CH4-6 advisory: P1-B/C/D/E lack an explicit per-artefact V4 fold-carry note (pass-
  cohesion uniformity only; V3 had zero REVISE so nothing to orphan).
- **CH2 sub-cosmetic prose noun.** P1-C §1.1 describes the `emit_full_parse` plane as the
  "delimiter/balance structural scanner" (plane-descriptive); its load-bearing class column
  at §2.4 correctly reads **scan**. Carried unchanged from V2/V3, below the cosmetic
  threshold, NOT a REVISE.

## §5 — HANDOFF

- **next-move = `ready-for-S-P2`.** S-P1 SK-V17 has converged (V3+V4 ≥95%, zero orphan
  REVISE, V≤5). Per PASS-1-PROFILE §6 the orchestrator reads the six P1 artefacts + this
  consolidation end-to-end, sets the `restart/skinny/tranches/sk-v17/HANDOFF.md` next-move
  line to `ready-for-S-P2`, and dispatches S-P2 Research per `skinny/PASS-2-RESEARCH.md`.
- **What S-P2 grounds on:** §3.3 hot leaves + §3.4 primitive antecedents. Every primitive
  S-P2 designs must answer to a P1 hot leaf named here; a primitive with no P1 antecedent is
  rejected by S-P2 CH1. The empirical floor is two-planed and honest: the recognition
  scanner already BEATS lightningcss (≥2× every corpus), the typed fact-stream plane is the
  String-tax loss (0.60–0.77×), and the SK-V17 task is to close the 4.4× instr/byte gap with
  a tape-activated typed plane — scan primitive gated behind tape, FNV/hex diagnostic
  retired, eager-tree pre-blocked.
