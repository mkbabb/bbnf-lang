# SK-V17 S-P1 CHALLENGE — CH5 HIDDEN-COUPLING (V3)

Lens: CH5 HIDDEN-COUPLING. Cycle: V3. Date: 2026-05-29.
Pass: S-P1 Profile (`restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH5).
Contract: ORCHESTRATOR §3W/§3Z. Reviewer scope: does each P1 artefact honour the
substrate union — no parallel-substrate / sidecar implied; the benched tape
(`skinny/crates/runtime/src/tape/`) is the one substrate (Lock 1); the benched
Track 1 (`skinny/crates/.../nonjson_css_l4.rs:596 track1_facts`, generated runtime)
is NOT conflated with comparator (lightningcss / cssparser) symbol paths; the
profile does NOT assume `crates/core` (the SK-V18 totality fold target, explicitly
out of S-P1 scope).
Artefacts reviewed: `p1/{p1a-samply-mode-1, p1b-samply-mode-2, p1c-samply-mode-3,
p1d-pmu-cycles, p1e-hot-leaf-attribution, p1f-bench-canonical}.md` (all V3).
Prior cycles: CH5 V1 = 25/26 ACCEPT (96.2%, one REVISE CH5-V1-R1). CH5 V2 = 26/27
ACCEPT (96.3%, one REVISE CH5-V2-R1).

## §1 — CH5 rubric (what this lens dispositions)

Per PASS-1-PROFILE §3 CH5: "A hot-leaf attribution that implies a parallel substrate
(a sidecar event vector, a second source scan, a retained cursor) must name it as a
Lock 1 observation, not normalise it. Track 1 ≡ generated runtime; Track 2 is
structurally independent — the profile must not conflate their symbol paths."
Specialised to the SK-V17 CSS-tape subject (the benched String fact-stream is the one
substrate today; the tape is UNWIRED for CSS), four invariants per artefact section:

- **I1 substrate-union (Lock 1).** The benched tape (`tape/ mod.rs:38/94/175/227`,
  `assembler.rs:42/71`) is the ONE substrate. The benched CSS path retains only `pos`
  + its output (the fact-stream `String`, or the 4-field recognition summary); a
  profile attributes those as the substrate, and names (not normalises) any retained
  cursor / sidecar vector / second source pass it observes.
- **I2 no parallel substrate observed.** Measurement scaffold (the harness wrapper /
  samply driver loop) is named as scaffold, never normalised into a parser-owned
  substrate.
- **I3 Track 1 vs comparator non-conflation.** `runtime::generated_css_l4_*` symbol
  paths stay disjoint from `lightningcss` / `cssparser` symbol paths; per-plane tables
  never merge a Track-1 leaf with a comparator leaf.
- **I4 no crates/core assumption.** Every benched symbol path resolves to
  `skinny/crates/`; any `crates/core` / "core-tree" reference is a NAMED contrast
  (un-benched core figure vs benched skinny figure), never a normalised substitution
  for a benched measurement.

## §2 — Source verifications performed (this lens, this cycle, master HEAD 6496fecae)

| Check | Command / inspection | Result |
|---|---|---|
| **Tape UNWIRED for CSS (I1/I2)** | `grep -rln "TapeBuilder\|ValueRef\|PayloadArena\|crate::tape" crates/runtime/src/grammars/css_l4_*/` | **ZERO matches** — substrate-union claim (no second substrate; tape unbuilt for CSS) empirically TRUE, re-verified this cycle on master HEAD |
| JSON grammar DOES reference tape (the cited asymmetry) | `grep` `grammars/json/` | **6 files** (`event_grammar_witness.rs`, `scan.rs`, `config.rs`, `value.rs`, `parser.rs`, `view.rs`) — confirms the asymmetry P1-E §2.5 / P1-F §4.4 cite |
| Benched entries ride disjoint fns (I3) | `grep` `nonjson_css_l4.rs` | `track1_facts:596 -> Result<String,String>`, `lightningcss_facts:636 -> Result<String,CssOracleError>` — Track-1 and the comparator ride structurally disjoint fns, confirmed |
| **Harness wrappers are pure scaffold (I2)** | read `css_canon_bench.rs:103-121` | `track1_full_parse:103` = `parse_full(input)` + `black_box(out.len())`; `track1_fact_stream:108` = `parse(input)` + `black_box(out.len())`; `lightningcss_full_cssom:113` = `StyleSheet::parse` + `black_box(.rules.0.len())`; `cssparser_token_scan:117` = `cssparser_full_parse` + `black_box(input.len())`. NO retained cursor, NO sidecar, NO second source pass — verified at the ACTUAL lines |
| **CH5-V2-R1 wrong-line cite (the V2 REVISE)** | read `css_canon_bench.rs:43` vs `:103` | `:43` is `struct RusageInfoV5` (PMU struct); `:103 fn track1_full_parse` is the wrapper — confirms the V2 defect AND the V3 fix target |
| Assert N>=50 (substrate context: the ONE harness over the ONE benched fn) | `grep` `assert!(n >= 50` | `:250 assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)")` — confirmed |
| REDRESS-53 single-substrate boundary (P1-D §2.5/§4) | read `REDRESS.md:807-813` | "structural projection must be the parser's single substrate, not a second scanner bolted onto source-byte recursive descent … A `ParserState`-owned structural cursor over source bytes is non-canonical" — P1-D's citation accurate |
| **Overlapping re-scan is ONE pass, not a parallel substrate (I1)** | read `generated.rs:209-261,313-318` | `parse_block_item:211 find_component_delim(self.pos,b"{};")` → `find_colon_before:314 find_component_delim(start,b":{};")` → `parse_declaration:247 find_component_delim(colon+1,b";}")` — the SAME scanner re-invoked over overlapping spans within ONE recognition pass, all advancing the single `self.pos`; NOT a parallel substrate. P1-D §2.5 bounds it to REDRESS-53 single-substrate shape correctly |
| **CH5-V2-R1 fold check** | read `p1f §2.3` row-2 cell + `p1f` V3 fold log :40-43 + §4.6 | **FOLDED** — `:43` → `:103-105` corrected in the §2.3 cell; the parenthetical at :360-363 documents the correction; §4.6 prose ("PURE measurement scaffold, NOT a retained or second pass") intact; the I2 non-conflation anchor now cites the right line |
| No `crates/core` symbol-path leakage (I4) | scan all 6 V3 artefacts for unqualified `crates/core`/"core-tree" hits | every hit is a NAMED contrast (`inherited`, `n/a on benched skinny`, `un-benched`, `core-tree figure`); ZERO unqualified `crates/core` symbol-path substitutions |
| Un-negated sidecar/parallel-substrate language scan | `grep -niE "sidecar\|parallel substrate\|second source\|retained cursor\|second pass\|event vector\|second scanner"` filtered for non-negated | only p1d:330 (the REDRESS-53 boundary, correctly naming the REJECTED shape) + p1a `.syms.json` *samply-symbolication sidecar* (a profiler output file, not a parser substrate) — neither is a normalised parser-owned second substrate |

## §3 — Per-artefact, per-section dispositions

### P1-A (`p1a-samply-mode-1.md`)

- §2.2 / §2.3 hot-leaf tables — **ACCEPT.** Symbols are
  `runtime::generated_css_l4_declaration_values::generated::*` exclusively; comparators
  (lightningcss/cssparser) appear only as throughput rows in §2.1/§2.1b, never merged
  into a §2.2/§2.3 symbol table. I3 honoured.
- §2.3 `mach_absolute_time` caller attribution (25591/25640 leaves called from
  libsystem_malloc frame `0x2b483`) — **ACCEPT.** The stack-prefix walk attributes the
  leaf to the **allocation family** of the ONE output-String substrate, not to a hidden
  timer / second pass. It resolves the ambiguity rather than normalising it. I1 honoured.
- §4.5 "No second-substrate / sidecar observed (CH5) … retains nothing but `pos` + a
  4-field summary … the substrate is UNWIRED" — **ACCEPT.** Explicit Lock-1 statement,
  matches the source grep. I1/I2 exemplary.
- §3 W6 typed-retime "~3.09 Mbps … core-tree eager `OpenFrame` path … n/a on benched
  skinny" — **ACCEPT.** I4 exemplary: the core-tree figure is named NOT-a-benched-plane,
  never substituted.
- §1.2/§1 `.syms.json` "sidecar" diction (samply symbolication file) — **ACCEPT (with
  lens note CH5-V3-N1, no fix).** The word "sidecar" here denotes the samply
  `--unstable-presymbolicate` symbol-table **output file**, a profiler artefact on disk —
  NOT a "sidecar event vector" parser substrate (the CH5 rubric sense). The two meanings
  do not collide in context (every use is qualified `.syms.json`/`symbolicate`), so no
  reader can mis-read it as a parser-owned second substrate. Noted only so the aggregator
  sees CH5 considered and cleared the terminology overlap; no REVISE.

### P1-B (`p1b-samply-mode-2.md`)

- §2.2 track1_fact table — **ACCEPT.** Allocation buckets (kernel/malloc/platform) are
  named as the cost of the ONE fact-stream `String` substrate ("the string-building
  leaves … are themselves the producers of that allocation"). No parallel substrate
  normalised. I1 honoured.
- §2.3 track1_full table, row 2 `track1_full_parse` 28.32% wrapper — **ACCEPT (CH5-V1-R1
  stays folded).** The cell reads "pure timing scaffold, NOT a retained/second parse
  pass (CH5-V1-R1; matches P1-C/P1-D/P1-F)"; the §2.3 narrative grounds the 68.1% as the
  scan fraction excluding the scaffold. I2 honoured; the V1 REVISE remains closed across
  the V3 revision.
- §2.3 CH2 callout "the SAME byte-class-membership primitive JSON's structural scan runs
  through `select_classifier` / `PrimitiveKernels` (verified `json/scan.rs:219`)" —
  **ACCEPT (I3 strengthened).** This names a SHARED generic *primitive shape* across two
  grammars without conflating the two grammars' symbol paths: `find_component_delim`
  (`generated_css_l4_*`) and `classify_structural_terminator_block_from_table`
  (`json/scan.rs`) are kept as distinct symbols that happen to be the same primitive
  CLASS — exactly the grammar-neutral framing CH2 wants, and it does not introduce a
  hidden shared substrate (each grammar retains its own state). I3 honoured.
- §3 W6 typed-retime row — **ACCEPT** (I4, same exemplary contrast as P1-A §3).
- §4.5 pre-block "no second substrate, no sidecar, no parallel source pass, no registry,
  no eager value tree, no x86 is proposed" — **ACCEPT.** I1/I2 honoured.

### P1-C (`p1c-samply-mode-3.md`)

- §2.3 by-resource table (`css_canon_bench` 35.56% own-code vs 64.45% syslib) —
  **ACCEPT.** Resource bucketing is the ONE substrate's own-code-vs-syslib split; no
  parallel substrate. I1 honoured.
- §2.4 row 2 `track1_full_parse` 27.93% "harness loop frame … pure measurement scaffold,
  no second pass" — **ACCEPT.** Names the 27.93% as scaffold AND re-states the parse-only
  fraction with it excluded ("Discounting the 27.93% harness-loop scaffold"). I2 honoured.
- §2.4 re-classification "`consume_balanced_at` is structural recursion OVER the same scan
  primitive … shares `find_component_delim`'s NEON target rather than being a separate
  kernel" — **ACCEPT (I1 exemplary).** The clearest statement across the six that the
  10% `consume_balanced_at` leaf is NOT a distinct/parallel scan substrate but the SAME
  inner loop reached recursively (`:322-337 ≡ :293-308`) — source-verified by this lens
  at the overlapping-scan check (§2). It collapses two apparent leaves into ONE substrate
  target, the opposite of normalising a hidden second pass.
- §4 A1 "plane bifurcation … two distinct code paths over one grammar module … disjoint
  hot-leaf sets … S-P2 must not conflate them" — **ACCEPT.** Proves the two Track-1 planes
  do NOT share a hidden substrate (each retains only its own output) and does not conflate
  them with the comparators. I3 honoured.
- §4 A5 / pre-block "Neither benched plane touches `bbnf-simd/src/dispatch.rs` … no
  structural index until the tape decodes CSS" — **ACCEPT.** Honours I1 (the tape is the
  only substrate, and it is unbuilt; no NEON sidecar).
- §3 "core-tree, inherited" rows (+2.6pp/−0.0pp) — **ACCEPT** (I4, named contrast).

### P1-D (`p1d-pmu-cycles.md`)

- §2.4 hot-leaf table, row `main` 27.92% "harness (excluded from parse-only)" —
  **ACCEPT.** Named as harness scaffold AND arithmetically excluded; I2 honoured.
- §2.5 + §4-4 "redundant overlapping re-scan … each declaration body is walked 2-3× by
  `find_component_delim`" with the **REDRESS-51/53 boundary citation** — **ACCEPT (the
  load-bearing CH5 call, source-re-verified this cycle).** This is the one place a reader
  could infer a "second source scan." The artefact correctly bounds it: the
  `parse_block_item:211 → find_colon_before:314 → parse_declaration:247` triple is the
  SAME scalar scanner re-invoked over overlapping spans within the ONE recognition pass,
  all advancing the single `self.pos` (this lens read `generated.rs:209-261,313-318` and
  confirms it), AND it cites `REDRESS.md:807-813` to bound the S-P2 tokenize-once
  primitive to the single-substrate shape (the tape/event stream IS the substrate, NOT a
  `ParserState`-owned second cursor). I1 honoured and reinforced; the boundary citation is
  accurate (this lens read `REDRESS.md:807-813`).
- §2.3 "`track1_fact_stream` … IS a full per-declaration byte scan [CORRECTED from V1]" —
  **ACCEPT.** Distinguishes the `emit_fact_stream` entry from the recognition scan without
  conflating their symbol paths (I3); the V1 mis-statement is corrected, sharpening the
  plane distinction.
- §4.7 "No SIMD on the CSS path … no `bbnf_simd` frame in 20,377 samples … the udot orphan
  `digit_mac.rs:27` is never reached" — **ACCEPT.** Confirms I1 by symbol-table absence:
  the only substrate is the scalar generated runtime; no NEON sidecar.
- §3.1 c/B posture (instr/byte primary; `ri_cycles` non-load-bearing) — **NOT A CH5
  MATTER (referred to CH1/CH4/CROSS-X1).** A cost-density-posture / counter-reliability
  question, not a substrate-union / sidecar / Track-1-vs-comparator question. In V3 the
  posture has CONVERGED across all six artefacts (the V2 P1-D-vs-rest divergence CH5 V2
  referred out is resolved — every artefact now carries the same "instr/byte primary,
  cyc/byte non-disambiguable/non-load-bearing" reading). CH5 takes no disposition; recorded
  so the aggregator does not expect CH5 to adjudicate it and notes the V2 referral closed.
- §3 W6 typed-retime / W8R rows — **ACCEPT** (I4, named contrasts).

### P1-E (`p1e-hot-leaf-attribution.md`)

- §2.3 / §2.4 hot-leaf tables — **ACCEPT.** Track-1 symbols only; comparators absent from
  the symbol tables (I3).
- §2.4 syslib-caller attribution "91.44% … reached FROM `emit_fact_stream`'s String
  growth" — **ACCEPT.** Strongest I1 attribution: the syscall+heap floor is bound to the
  ONE output-String substrate by an explicit caller-chain walk (`caller.py`, 29986 syslib
  leaves walked to nearest binary caller), not normalised as ambient cost.
- §2.5 roll-up "**no `tape` leaf at all** … `Tape`/`ValueRef`/`TapeBuilder` appear nowhere
  … the `dispatch` vehicle appears nowhere — zero SIMD on the CSS path" — **ACCEPT (I1
  exemplary).** The empirical proof of substrate-union: the one substrate is unbuilt for
  CSS, verified by symbol absence. Matches this lens's source grep (§2).
- §4.4 "the udot/i8mm digit kernel … has no benched CSS antecedent … S-P2 must re-profile
  the typed path after W1/W2, not inherit a … hypothesis from here" — **ACCEPT.** Honours
  the profile-first non-negotiable AND I4 (no carried hypothesis, no assumed un-benched
  path).
- §4.6 pre-block "explicitly does NOT re-open AZ-IV eager-value-tree … nor StructRegistry
  indirection (single non-generic `TapeBuilder`)" — **ACCEPT.** I1 honoured (no second
  substrate, no per-leaf eager payload).

### P1-F (`p1f-bench-canonical.md`)

- §2.3 three per-plane tables (track1_full_parse / track1_fact_stream / lightningcss) —
  **ACCEPT (I3 exemplary).** The lightningcss table (§2.3, 13583 leaf samples) is a
  SEPARATE plane attribution with comparator symbols only (`cssparser::Parser::next_…`,
  `cssparser::tokenizer::consume_name`, `parcel_selectors::parser::parse_selector`,
  `lightningcss::declaration::parse_declaration`, `lightningcss::PropertyId::*`) — never
  merged with the `generated_css_l4_*` Track-1 symbols. The three tables are explicitly
  per-plane.
- §2.3 track1_full_parse row-2 wrapper cell — **ACCEPT (CH5-V2-R1 FOLDED + CLOSED).** The
  V2 REVISE is resolved: the cell now cites `css_canon_bench.rs:103-105` (verified by this
  lens: `:103 fn track1_full_parse` = `parse_full(input)` + `black_box(out.len())`); the
  wrong V2 cite `:43` (the `RusageInfoV5` PMU struct) is gone; the parenthetical at the
  cell foot documents the correction; the CH5 I2 non-conflation framing ("PURE measurement
  scaffold; NOT a retained/second pass — see §4.6") is intact and now verifiable at the
  right line. The V3 fold log (:40-43) records the fold. I2 anchor restored.
- §4.4 "`tape_activated = false` for CSS (verified) … grep … returns ZERO; the JSON
  grammar DOES reference the tape" — **ACCEPT (I1 exemplary).** Independently reproduces
  the source grep this lens performed; the substrate-union baseline is a telemetry column.
- §4.6 "No second substrate / no sidecar introduced (Lock 1 / CH5) … no retained cursor,
  no event vector, no parallel source pass … the samply driver loop is a transient
  measurement scaffold, not a parser-owned structure. Track 1 (`generated_css_l4_*`) and
  the comparators keep separate symbol paths" — **ACCEPT.** Direct, correct CH5
  self-statement; matches every source check. The line-precision defect that confined this
  in V2 is now fixed.
- §3.2 / outcome enum "~70 / ~14× narrative … N-direct … the only ~3 Mbps figure is the
  EAGER typed-CSSOM plane … SYNTHESIS §0.4 PRE-BLOCKS" — **ACCEPT** (I4: the un-benched
  eager plane is named + pre-blocked, never substituted).

## §4 — Cross-artefact CH5 finding (coordination, not substrate)

The five-harness divergence is **resolved**: every V3 artefact designates
`css_canon_bench.rs` as the single canonical harness (P1-F §1.1.1 owns the verdict).
This was never a CH5 substrate-union violation — all five were transient measurement
scaffolds over the SAME benched `parser::parse` / `parser::parse_full` fns, introducing
no parser-owned state, no second substrate, no sidecar. CH5 confirms only that the
Track 1 ≡ generated-runtime identity is preserved across every named harness shell and
that none introduces a parallel substrate. The reproducibility / absolute-Mbps dispersion
is CH4's matter (CROSS-X2); CH5 takes no disposition.

The c/B posture conflict CH5 V2 referred out (P1-D §3.1 vs the other five) has
**CONVERGED in V3** — all six artefacts now carry the one reading (instr/byte primary;
`ri_cycles` non-disambiguable, non-load-bearing). This was never a CH5 substrate matter
and remains CH1/CH4/CROSS-X1's; CH5 records that the V2 referral is closed so no defect
is orphaned.

CH5-V3-N1 (terminology overlap, NO fix): P1-A uses "sidecar" for the samply
`.syms.json` symbolication output file. This is a profiler artefact on disk, not a
"sidecar event vector" parser substrate (the CH5-rubric sense). Every use is qualified
(`.syms.json`/`symbolicate`), so the two meanings do not collide in context. Recorded for
completeness; no REVISE — it does not normalise a parallel parser substrate.

## §5 — Disposition summary

| Artefact | Sections dispositioned | ACCEPT | REVISE | REJECT |
|---|---|---:|---:|---:|
| P1-A | §2.2/§2.3, §2.3 mach_absolute_time, §4.5, §3, §1 .syms.json (N1) | 5 | 0 | 0 |
| P1-B | §2.2, §2.3 (R1 folded), §2.3 CH2, §3, §4.5 | 5 | 0 | 0 |
| P1-C | §2.3, §2.4 scaffold, §2.4 re-class, §4-A1, §4-A5, §3 | 6 | 0 | 0 |
| P1-D | §2.4, §2.5+§4-4, §2.3, §4.7, §3 (§3.1 referred out) | 5 | 0 | 0 |
| P1-E | §2.3/§2.4, §2.4 caller, §2.5, §4.4, §4.6 | 5 | 0 | 0 |
| P1-F | §2.3 tables, §2.3 row-2 (R1 folded), §4.4, §4.6, §3.2 | 5 | 0 | 0 |
| **Total** | **31 sections** | **31** | **0** | **0** |

CH5 ACCEPT rate: **31/31 = 100%**. Zero REVISE, zero REJECT.

The V2 REVISE (CH5-V2-R1, p1f §2.3 `:43`→`:103-105`) is **FOLDED and closed** — verified
at `p1f §2.3` row-2 (now `:103-105`, grep-verified `:103 fn track1_full_parse`) and the
p1f V3 fold log. The V1 REVISE (CH5-V1-R1, p1b §2.3) remains folded across the V3 revision.

The substrate union is **honoured by construction** across all six V3 artefacts: the
benched tape is the one substrate and is empirically UNWIRED for CSS (zero tape symbols in
any CSS profile — source-grep re-confirmed this cycle on master HEAD `6496fecae`); no
parallel substrate, sidecar, or retained cursor is observed or normalised; the redundant
2-3× overlapping re-scan is the SAME scalar scanner re-invoked within ONE pass (advancing
the single `self.pos`) and is correctly bounded to the REDRESS-53 single-substrate shape
(P1-D §2.5/§4-4); Track 1 (`generated_css_l4_*`) symbol paths stay disjoint from the
lightningcss/cssparser comparators (P1-F §2.3 three per-plane tables exemplary); and every
`crates/core` / "core-tree" reference is a NAMED un-benched contrast, never a substitute
for a benched skinny measurement (I4 satisfied — the profile does not assume `crates/core`).

## §6 — REVISE ledger (for the V4 fold)

None. CH5 V3 carries zero open REVISE and zero orphan dispositions. The single V2 REVISE
(CH5-V2-R1) folded cleanly; CH5 converges at 100% ACCEPT this cycle.

## §7 — Referrals (non-CH5, recorded so no defect is orphaned)

- **C/B posture (P1-D §3.1).** Now CONVERGED across all six artefacts; the V2 cross-artefact
  conflict CH5 referred out is closed. Remains CH1/CH4/CROSS-X1's domain for any residual
  reliability adjudication; CH5 takes no disposition.
- **CH5-V3-N1 terminology (P1-A `.syms.json` "sidecar").** Profiler-output diction, not a
  parser substrate; no fix. Recorded only so the aggregator sees the overlap was checked.
