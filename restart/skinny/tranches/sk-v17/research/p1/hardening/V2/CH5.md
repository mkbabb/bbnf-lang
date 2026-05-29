# SK-V17 S-P1 CHALLENGE — CH5 HIDDEN-COUPLING (V2)

Lens: CH5 HIDDEN-COUPLING. Cycle: V2. Date: 2026-05-29.
Pass: S-P1 Profile (`restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH5).
Contract: ORCHESTRATOR §3W/§3Z. Reviewer scope: does each P1 artefact honour the
substrate union — no parallel-substrate / sidecar implied; the benched tape
(`skinny/crates/runtime/src/tape/`) is the one substrate (Lock 1); Track 1 ≡
generated runtime and is NOT conflated with comparator symbol paths; the profile
does NOT assume `crates/core` (the SK-V18 totality fold target, explicitly out of
S-P1 scope).
Artefacts reviewed: `p1/{p1a-samply-mode-1, p1b-samply-mode-2, p1c-samply-mode-3,
p1d-pmu-cycles, p1e-hot-leaf-attribution, p1f-bench-canonical}.md` (all V2).
Prior cycle: CH5 V1 = 25/26 ACCEPT (96.2%), one REVISE (CH5-V1-R1, p1b §2.3).

## §1 — CH5 rubric (what this lens dispositions)

Per PASS-1-PROFILE §3 CH5: "A hot-leaf attribution that implies a parallel substrate
(a sidecar event vector, a second source scan, a retained cursor) must name it as a
Lock 1 observation, not normalise it. Track 1 ≡ generated runtime; Track 2 is
structurally independent — the profile must not conflate their symbol paths."
Specialised to the SK-V17 CSS-tape subject, four invariants per artefact section:

- **I1 substrate-union (Lock 1).** The benched tape (`tape/ mod.rs:38/94/175/227`,
  `assembler.rs:42/71`) is the ONE substrate. A profile attributes tape symbols as
  substrate, not as a separable producer, and names (not normalises) any retained
  cursor / sidecar vector / second source pass it observes.
- **I2 no parallel substrate observed.** On the benched CSS path the parser retains
  nothing beyond `pos` + the output String (fact-stream) or `pos` + the 4-field
  summary (recognition) — no sidecar event vector. Measurement scaffold (the harness
  wrapper / samply driver loop) is named as scaffold, never normalised into a
  parser-owned substrate.
- **I3 Track 1 vs comparator non-conflation.** `runtime::generated_css_l4_*` symbol
  paths stay disjoint from `lightningcss` / `cssparser` symbol paths; per-plane tables
  never merge a Track-1 leaf with a comparator leaf.
- **I4 no crates/core assumption.** Every symbol path resolves to `skinny/crates/`;
  any `crates/core` / "core-tree" reference is a NAMED contrast (un-benched core figure
  vs benched skinny figure), never a normalised substitution for a benched measurement.

## §2 — Source verifications performed (this lens, this cycle)

| Check | Command / inspection | Result |
|---|---|---|
| **Tape UNWIRED for CSS (I1/I2)** | `grep -rln "TapeBuilder\|ValueRef\|PayloadArena\|crate::tape" runtime/src/grammars/css_l4_*/` | **ZERO matches** — the substrate-union claim (no second substrate; tape unbuilt for CSS) is empirically TRUE, re-verified on master HEAD |
| JSON grammar DOES reference tape | `grep` `grammars/json/` | 6 files (`config.rs`, `event_grammar_witness.rs`, `value.rs`, `scan.rs`, `parser.rs`, `view.rs`) — confirms the asymmetry every artefact cites (P1-F §4.4, P1-E §2.5) |
| Tape symbols at cited lines | `grep` `tape/mod.rs`, `assembler.rs` | `PayloadArena:38`, `Tape:94`, `ValueRef:175`, `DocumentView:227` (a **trait** — accurate line; the artefacts' type-noun "DocumentView" is harmless), `TapeBuilder:42`, `push_plain_offset:71` — all confirmed |
| Hot-leaf scan lines | `grep` `generated.rs` | `find_component_delim:288`, `delimiters.contains(&byte):295`, `consume_balanced_at:320`, `parse_block_item:209`, `find_colon_before:313`, `parse_declaration:242` — all confirmed |
| Benched entries ride disjoint fns (I3) | `grep` `nonjson_css_l4.rs` | `track1_facts:596 -> Result<String,String>`, `lightningcss_facts:636 -> Result<String,CssOracleError>` — confirmed; Track-1 and the comparator ride structurally disjoint fns |
| **Harness wrapper is pure scaffold (I2)** | read `css_canon_bench.rs:103-110` | `track1_full_parse` = `parse_full(input)` + `black_box(out.len())`; `track1_fact_stream` = `parse(input)` + `black_box(out.len())`; `lightningcss_full_cssom` = `StyleSheet::parse` + `black_box(.rules.0.len())`. NO retained cursor, NO sidecar, NO second source pass — verified at the ACTUAL lines `:103-105`/`:108-110` |
| REDRESS-53 single-substrate boundary (P1-D §2.5/§4-4) | read `REDRESS.md:809-812` | "the scanner writes the tape/event stream and generated lowering consumes that stream … A `ParserState`-owned structural cursor over source bytes is [non-canonical]" — P1-D's citation is accurate |
| No `crates/core` symbol-path leakage (I4) | `grep -niE "crates/core\|core-tree\|/core/"` over all 6 artefacts, filtered for unqualified hits | every hit is a NAMED contrast (`inherited` / `n/a on benched` / `un-benched` / `core-tree figure`); ZERO unqualified `crates/core` symbol-path substitutions |
| **CH5-V1-R1 fold check** | read `p1b §2.3` row-2 cell (`:209`) + narrative | **FOLDED** — `:209` now reads "pure timing scaffold, NOT a retained/second parse pass (CH5-V1-R1; matches P1-C/P1-D/P1-F)"; the orphan REVISE is closed |

## §3 — Per-artefact, per-section dispositions

### P1-A (`p1a-samply-mode-1.md`)

- §2.2 / §2.3 hot-leaf tables — **ACCEPT.** Symbols are
  `runtime::generated_css_l4_declaration_values::generated::*` exclusively; no
  comparator leaf merged into a symbol table (lightningcss/cssparser are throughput-only
  rows in §2.1, never in §2.2/§2.3 symbol tables). I3 honoured.
- §2.3 `mach_absolute_time` caller attribution — **ACCEPT.** The stack-prefix walk
  (25591/25640 leaves called from libsystem_malloc frame `0x2b483`) attributes the leaf
  to the **allocation family** of the ONE output-String substrate, not to a hidden
  timer/second pass. Most CH5-load-bearing call in the artefact; it resolves the
  ambiguity rather than normalising it. I1 honoured.
- §3 row "track1 typed-retime (W6) ~3.09 Mbps … core-tree eager `OpenFrame` path …
  n/a on benched skinny" — **ACCEPT.** I4 exemplary: the core-tree figure is named as
  NOT a benched plane and marked `n/a on benched skinny`, never substituted.
- §4.5 "No second-substrate / sidecar observed (CH5)" — **ACCEPT.** Explicit Lock-1
  statement: "retains nothing but `pos` + a 4-field summary … the substrate is
  UNWIRED." Matches the source grep (§2).

### P1-B (`p1b-samply-mode-2.md`)

- §2.2 track1_fact table — **ACCEPT.** Allocation buckets (kernel/malloc/platform)
  are named as the cost of the ONE fact-stream `String` substrate ("the string-building
  leaves … are themselves the producers of that allocation"). No parallel substrate
  normalised. I1 honoured.
- §2.3 track1_full table, row 2 `css_cold_harness::track1_full` 28.32% wrapper —
  **ACCEPT (CH5-V1-R1 FOLDED).** The V1 REVISE is resolved: `:209` now reads "the
  `parse_full` call + `out.len()` + `black_box` + the LTO-inlined `sample()`/`main()`
  outer measurement loop — **pure timing scaffold, NOT a retained/second parse pass**
  (CH5-V1-R1; matches P1-C/P1-D/P1-F)", and the §2.3 narrative ("68.1% … excluding the
  harness wrapper") is now grounded as scaffold. I2 honoured; the differentiation
  P1-C/P1-D/P1-F carried is now matched. No reader can mis-read the 28% as a retained
  second pass.
- §3 W6 typed-retime row — **ACCEPT** (I4, same exemplary contrast as P1-A §3).
- §4.5 (pre-block) "no second substrate, no sidecar, no parallel source pass is
  proposed" — **ACCEPT.** I1/I2 honoured.

### P1-C (`p1c-samply-mode-3.md`)

- §2.3 by-resource table (`css_canon_bench` 35.56% own-code vs 64.45% syslib) —
  **ACCEPT.** Resource bucketing is the ONE substrate's own-code vs syslib split; no
  parallel substrate. I1 honoured.
- §2.4 row 2 `track1_full_parse` 27.93% "harness loop frame … pure measurement
  scaffold, no second pass" — **ACCEPT.** Names the 27.93% as scaffold AND re-states the
  parse-only fraction with the scaffold excluded ("Discounting the 27.93% harness-loop
  scaffold"). I2 honoured.
- §4 A1 "plane bifurcation … two distinct code paths over one grammar module … disjoint
  hot-leaf sets" — **ACCEPT.** Cleanest I3 statement across the six: proves the two
  Track-1 planes do NOT share a hidden substrate (each retains only its own output) and
  does not conflate them with the comparators.
- §4 A5 / pre-block — **ACCEPT.** "Neither benched plane touches
  `bbnf-simd/src/dispatch.rs`" + "no structural index until the tape decodes CSS"
  honours I1 (the tape is the only substrate, and it is unbuilt; no NEON sidecar).
- §3 "core-tree, inherited" rows — **ACCEPT** (I4, named contrast +2.6pp/−0.0pp).

### P1-D (`p1d-pmu-cycles.md`)

- §2.4 hot-leaf table, row `main` 27.92% "harness (excluded from parse-only)" —
  **ACCEPT.** Named as harness scaffold AND arithmetically excluded; I2 honoured.
- §2.5 + §4-4 "redundant overlapping re-scan … each declaration body is walked 2-3× by
  `find_component_delim`" with the **REDRESS-51/53 boundary citation** — **ACCEPT
  (CH5-strengthened over V1).** In V1 this was ACCEPTed with a lens note flagging the
  one place a reader might infer a "second source scan." V2 now explicitly names the
  admissible-vs-rejected boundary (`REDRESS.md:807-813` single-substrate vs `:784-805`
  parser-local second cursor), source-verified at `REDRESS.md:809-812`. This is exactly
  the CH5 discipline: the overlapping re-scan is the SAME scalar scanner re-invoked over
  overlapping spans within the ONE recognition pass (all writing into the same
  `pos`/summary), NOT a parallel substrate, AND the artefact now bounds the S-P2
  primitive to the single-substrate shape. I1 honoured and reinforced.
- §2.3 "`track1_fact_stream` … IS a full per-declaration byte scan [CORRECTED from V1]"
  — **ACCEPT.** Distinguishes the `emit_fact_stream` entry from the recognition scan
  without conflating their symbol paths (I3); the V1 mis-statement ("does not even run
  the scan") is corrected, sharpening rather than blurring the plane distinction.
- §4.7 "No SIMD on the CSS path … no `bbnf_simd` frame in 20,377 samples" — **ACCEPT.**
  Confirms I1 by symbol-table absence: the only substrate is the scalar generated
  runtime; no NEON sidecar; the udot orphan `digit_mac.rs:27` is never reached.
- §3 W6 typed-retime / W8R rows — **ACCEPT** (I4, named contrasts).
- §3.1 ri_cycles rehabilitation (P1-D now argues `ri_cycles` IS a valid 4.27 GHz
  counter, diverging from the pass-wide "falsified" posture P1-A/B/C/E/F carry) —
  **NOT A CH5 MATTER (referred to CH1/CH4).** This is a cost-density-posture conflict
  (instr/byte vs cyc/byte; counter reliability), not a substrate-union / sidecar /
  Track-1-vs-comparator question. CH5 takes no disposition; the cross-artefact c/B
  posture divergence is the consolidation's CH1/CH4/CROSS-X1 matter. (Recorded so the
  aggregator does not expect CH5 to adjudicate it.)

### P1-E (`p1e-hot-leaf-attribution.md`)

- §2.3 / §2.4 hot-leaf tables — **ACCEPT.** Track-1 symbols only; comparators absent
  from the symbol tables (I3).
- §2.4 syslib-caller attribution "91.44% … reached FROM `emit_fact_stream`'s String
  growth" — **ACCEPT.** Strongest I1 attribution: the syscall+heap floor is bound to the
  ONE output-String substrate by an explicit caller-chain walk, not normalised as
  ambient cost.
- §2.5 roll-up "**no `tape` leaf at all** … `Tape`/`ValueRef`/`TapeBuilder` appear
  nowhere … the `dispatch` vehicle appears nowhere" — **ACCEPT (I1 exemplary).** The
  empirical proof of substrate-union: the one substrate is unbuilt for CSS, verified by
  symbol absence. Matches the source grep (§2).
- §4.4 "the udot/i8mm digit kernel … has no benched CSS antecedent … S-P2 must
  re-profile the typed path after W1/W2, not inherit a … hypothesis from here" —
  **ACCEPT.** Honours the profile-first non-negotiable AND I4 (no carried hypothesis, no
  assumed un-benched path).
- §4.6 pre-block — **ACCEPT** (no second substrate; explicitly "no per-leaf eager
  payload … single non-generic `TapeBuilder`").

### P1-F (`p1f-bench-canonical.md`)

- §2.3 three per-plane tables (track1_full_parse / track1_fact_stream / lightningcss)
  — **ACCEPT (I3 exemplary).** The lightningcss table is a SEPARATE plane attribution
  (`cssparser::Parser::next_…`, `parcel_selectors::…`, `lightningcss::…` symbols) never
  merged with the `generated_css_l4_*` Track-1 symbols. The three tables are explicitly
  per-plane.
- §2.3 track1_full_parse row 2, the 26.74% wrapper cell — **REVISE (CH5-adjacent I2,
  line-precision).** The CH5 SUBSTANCE is correct and well-stated: "`String::len()` of
  the 4-field summary + `black_box` + the LTO-inlined outer driver loop — PURE
  measurement scaffold, NOT a retained/second pass — see §4.6", and §4.6 re-states it.
  But the cited file:line is **wrong**: the cell reads `css_canon_bench.rs:43`, and
  `:43` is inside the unrelated `RusageInfoV5` PMU struct — the `track1_full_parse`
  wrapper fn is at `css_canon_bench.rs:103-105` (verified `parse_full(input)` +
  `black_box(out.len())`). The CH5 non-conflation anchor (the scaffold-not-substrate
  framing) is the one cell CH5 leans on for I2, so a wrong line on it weakens the
  reader's ability to verify the scaffold is scaffold. **Fix:** change `css_canon_bench.rs:43`
  → `css_canon_bench.rs:103-105` in the §2.3 row-2 cell (and the matching §4.6 reference
  to "26.74% `track1_full_parse` wrapper (§2.3)" inherits the corrected line). The
  defect is primarily CH1/CH4 traceability; it is dispositioned here only because the
  cell IS the CH5 I2 anchor.
- §4.4 "`tape_activated = false` for CSS (verified) … grep … returns ZERO; the JSON
  grammar DOES reference the tape" — **ACCEPT (I1 exemplary).** Independently reproduces
  the source grep this lens performed; the substrate-union baseline is a telemetry column.
- §4.6 "No second substrate / no sidecar introduced (Lock 1 / CH5) … no retained cursor,
  no event vector, no parallel source pass … the samply driver loop is a transient
  measurement scaffold, not a parser-owned structure. Track 1 … and the comparators keep
  separate symbol paths" — **ACCEPT.** Direct, correct CH5 self-statement; matches every
  source check. (The line-precision defect is confined to the §2.3 cite; the §4.6 prose
  is sound.)
- §3.2 / outcome enum "~70 / ~14× narrative … N-direct … the only ~3 Mbps figure is the
  EAGER typed-CSSOM plane … SYNTHESIS §0.4 PRE-BLOCKS" — **ACCEPT** (I4: the un-benched
  eager plane is named + pre-blocked, never substituted).

## §4 — Cross-artefact CH5 finding (coordination, not substrate)

The five-harness divergence flagged in V1 §4 is now **resolved at the substrate level**:
every V2 artefact designates `css_canon_bench.rs` as the single canonical harness
(P1-F §1.1.1 owns the verdict). This was never a CH5 substrate-union violation — all
five were transient measurement scaffolds over the SAME benched `parser::parse` /
`parser::parse_full` fns, introducing no parser-owned state, no second substrate, no
sidecar — and the convergence further reduces the surface. **No CH5 fix arises from the
harness convergence**; it is the consolidation's CROSS-X2 matter (the reproducibility /
absolute-Mbps dispersion is CH4's; the line-number fabrication-precision in some harness
cites is CH1's). CH5 confirms only that Track 1 ≡ generated runtime is preserved across
all named harness shells and that none introduces a parallel substrate.

The P1-D §3.1 ri_cycles rehabilitation creates a genuine **pass-wide c/B posture
conflict** (P1-D says `ri_cycles` is valid; P1-A/B/C/E/F say it is falsified). CH5
explicitly declines this — it is not a substrate/sidecar/conflation question — and refers
it to CH1/CH4 and the consolidation so it is not orphaned.

## §5 — Disposition summary

| Artefact | Sections dispositioned | ACCEPT | REVISE | REJECT |
|---|---|---:|---:|---:|
| P1-A | §2.2, §2.3, §3, §4.5 | 4 | 0 | 0 |
| P1-B | §2.2, §2.3 (R1 folded), §3, §4.5 | 4 | 0 | 0 |
| P1-C | §2.3, §2.4, §4-A1, §4-A5/pre-block, §3 | 5 | 0 | 0 |
| P1-D | §2.4, §2.5+§4-4, §2.3, §4.7, §3 (§3.1 referred out) | 5 | 0 | 0 |
| P1-E | §2.3/§2.4, §2.5, §4.4, §4.6 | 4 | 0 | 0 |
| P1-F | §2.3 tables, §2.3 row-2 cite, §4.4, §4.6, §3.2 | 4 | 1 | 0 |
| **Total** | **27 sections** | **26** | **1** | **0** |

CH5 ACCEPT rate: **26/27 = 96.3%**. One REVISE (P1-F §2.3, CH5-adjacent I2): the
26.74% scaffold-wrapper cell cites `css_canon_bench.rs:43` (the unrelated `RusageInfoV5`
struct) where the wrapper fn is `:103-105`; the CH5 framing is correct but the cite —
the I2 non-conflation anchor — is wrong. Zero REJECT.

The V1 REVISE (CH5-V1-R1, p1b §2.3) is **FOLDED and closed** — verified at `p1b:209`.

The substrate union is **honoured by construction** across all six V2 artefacts: the
benched tape is the one substrate and is empirically UNWIRED for CSS (zero tape symbols
in any CSS profile — source-grep re-confirmed this cycle); no parallel substrate, sidecar,
or retained cursor is observed or normalised; the redundant 2-3× re-scan is correctly
bounded to the REDRESS-53 single-substrate shape (P1-D); Track 1 (`generated_css_l4_*`)
symbol paths stay disjoint from the lightningcss/cssparser comparators; and every
`crates/core` / "core-tree" reference is a NAMED un-benched contrast, never a substitute
for a benched skinny measurement (I4 satisfied — the profile does not assume `crates/core`).

## §6 — REVISE ledger (for the V3 fold)

| ID | Artefact:section | Defect | Concrete fix |
|---|---|---|---|
| CH5-V2-R1 | `p1f-bench-canonical.md` §2.3 (track1_full_parse row 2) | The 26.74% scaffold-wrapper cell cites `css_canon_bench.rs:43`, which is the unrelated `RusageInfoV5` PMU struct; the actual `track1_full_parse` wrapper fn is at `:103-105` (verified `parse_full(input)` + `black_box(out.len())`). The cell is the CH5 I2 non-conflation anchor ("PURE measurement scaffold, NOT a retained/second pass"), so the wrong line weakens its verifiability. | In the §2.3 row-2 cell, change `css_canon_bench.rs:43` → `css_canon_bench.rs:103-105`. The CH5 framing prose ("`String::len()` … PURE measurement scaffold, NOT a retained/second pass — see §4.6") is correct and stays. (Cross-check: P1-E §2.3 carries the same wrong-line shape, `:45`, for its `track1_full_parse` wrapper cell — primarily CH1/CH4's to disposition, but the same `:103-105` correction applies; noted here for the aggregator to route.) |

## §7 — Referrals (non-CH5, recorded so no defect is orphaned)

- **C/B posture conflict (P1-D §3.1 vs P1-A/B/C/E/F).** P1-D rehabilitates `ri_cycles`
  as a valid 4.27 GHz counter (sub-1.0 CPI = high IPC on a wide core); the other five
  strike `ri_cycles` as falsified. Not a CH5 substrate matter — referred to CH1/CH4 /
  CROSS-X1 for the consolidation. CH5 takes no disposition.
- **Harness-cite line precision (P1-E §2.3 `:45`, P1-F §2.3 `:43`).** Wrong line numbers
  for the scaffold wrapper; CH1/CH4 traceability. CH5 dispositions only P1-F's instance
  (CH5-V2-R1) because it is P1-F's I2 anchor; P1-E's is noted for routing.
