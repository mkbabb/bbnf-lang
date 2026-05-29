# SK-V17 S-P1 CHALLENGE — CH5 HIDDEN-COUPLING (V1)

Lens: CH5 HIDDEN-COUPLING. Cycle: V1. Date: 2026-05-29.
Pass: S-P1 Profile (`restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH5).
Contract: ORCHESTRATOR §3W/§3Z. Reviewer scope: does each P1 artefact honour the
substrate union — no parallel-substrate / sidecar implied; the benched tape
(`skinny/crates/runtime/src/tape/`) is the one substrate (Lock 1); Track 1 ≡
generated runtime and is NOT conflated with comparator symbol paths; the profile
does NOT assume `crates/core` (the SK-V18 totality fold target, explicitly out of
S-P1 scope).
Artefacts reviewed: `p1/{p1a-samply-mode-1, p1b-samply-mode-2, p1c-samply-mode-3,
p1d-pmu-cycles, p1e-hot-leaf-attribution, p1f-bench-canonical}.md`.

## §1 — CH5 rubric (what this lens dispositions)

Per PASS-1-PROFILE §3 CH5: "A hot-leaf attribution that implies a parallel
substrate (a sidecar event vector, a second source scan, a retained cursor) must
name it as a Lock 1 observation, not normalise it. Track 1 ≡ generated runtime;
Track 2 is structurally independent — the profile must not conflate their symbol
paths." Specialised to the SK-V17 CSS-tape subject this lens checks four invariants
per artefact section:

- **I1 substrate-union (Lock 1).** The benched tape (`tape/ mod.rs:38/94/175/227`,
  `assembler.rs:42/71`) is the ONE substrate. A profile must attribute tape symbols
  as substrate, not as a separable producer, and must name (not normalise) any
  retained cursor / sidecar vector / second source pass it observes.
- **I2 no parallel substrate observed.** On the benched CSS path the profile must
  confirm the parser retains nothing beyond `pos` + the output String (fact-stream)
  or `pos` + 4-field summary (recognition) — no sidecar event vector.
- **I3 Track 1 vs comparator non-conflation.** `runtime::generated_css_l4_*` symbol
  paths must stay disjoint from `lightningcss` / `cssparser` symbol paths; per-plane
  tables must never merge a Track-1 leaf with a comparator leaf.
- **I4 no crates/core assumption.** Every symbol path resolves to `skinny/crates/`;
  any `crates/core` / "core-tree" reference must be a NAMED contrast (this is the
  un-benched core figure; here is the benched skinny figure), never a normalised
  substitution for a benched measurement.

## §2 — Source verifications performed (this lens, this cycle)

| Check | Command / inspection | Result |
|---|---|---|
| Tape symbols exist at cited lines | `grep` `tape/mod.rs`, `assembler.rs` | `PayloadArena:38`, `Tape:94`, `ValueRef:175`, `DocumentView:227` (a **trait**, artefacts say "DocumentView:227" — accurate line, harmless type-noun), `TapeBuilder:42`, `push_plain_offset:71` — all confirmed |
| **Tape UNWIRED for CSS (I1/I2)** | `grep -rln "TapeBuilder\|ValueRef\|PayloadArena\|crate::tape" grammars/css_l4_*/` | **ZERO matches** — the substrate-union claim (no second substrate; tape unbuilt for CSS) is empirically TRUE |
| JSON grammar DOES reference tape | `grep` `grammars/json/` | 6 files reference tape — confirms P1-F §4.4 "JSON references tape, CSS does not" |
| Hot-leaf lines | `grep` `generated.rs` | `emit_fact_stream:5`, `emit_full_parse:61`, `find_component_delim:288`, `consume_balanced_at:320`, `push_ascii_lower_hex:628` — all confirmed |
| Benched entry | `grep` `nonjson_css_l4.rs` | `track1_facts:596`, `lightningcss_facts:636` — confirmed; the two ride disjoint fns |
| **Harness wrapper is pure scaffold (I2)** | read `css_canon_bench.rs:103-120` | `track1_full_parse` = `parse_full(input)` + `black_box(out.len())`; `track1_fact_stream` = `parse(input)` + `black_box(out.len())`; `run_lightningcss` = `StyleSheet::parse` + `black_box(.len())`. NO retained cursor, NO sidecar, NO second source pass |
| No crates/core in symbol paths (I4) | `grep -niE "crates/core\|core-tree\|/core/"` over all 6 artefacts | every hit is a NAMED contrast against the benched figure; no symbol path resolves outside `skinny/crates/runtime` |

## §3 — Per-artefact, per-section dispositions

### P1-A (`p1a-samply-mode-1.md`)

- §2.2 / §2.3 hot-leaf tables — **ACCEPT.** Symbols are
  `runtime::generated_css_l4_declaration_values::generated::*` exclusively; no
  comparator leaf merged (lightningcss/cssparser are throughput-only rows in §2.1,
  never in the symbol tables). I3 honoured.
- §2.3 `mach_absolute_time` caller attribution — **ACCEPT.** The stack-prefix walk
  (25591/25640 leaves called from a libsystem_malloc frame `0x2b483`) correctly
  attributes the leaf to the **allocation family** of the ONE output-String
  substrate, not to a hidden timer/second-pass. This is the most CH5-load-bearing
  call in the artefact and it resolves the ambiguity rather than normalising it.
- §3 row "track1 typed-retime (W6) ~3.09 Mbps … core-tree eager `OpenFrame` path …
  n/a on benched skinny" — **ACCEPT.** I4 exemplary: the core-tree figure is named
  as NOT a benched plane and marked `n/a on benched skinny`, never substituted for a
  measurement.
- §4.5 "No second-substrate / sidecar observed (CH5)" — **ACCEPT.** Explicit Lock-1
  statement: "retains nothing but `pos` + a 4-field summary … the substrate is
  UNWIRED." Matches the source grep (§2 above).

### P1-B (`p1b-samply-mode-2.md`)

- §2.2 track1_fact table — **ACCEPT.** Allocation buckets
  (`libsystem_kernel`/`malloc`/`platform`) are correctly named as the cost of the
  ONE fact-stream `String` substrate ("the string-building leaves … are themselves
  the producers of that allocation"). No parallel substrate normalised.
- §2.3 track1_full table, row 2 `css_cold_harness::track1_full` 28.32% "harness
  wrapper" — **REVISE (CH5-adjacent, I2).** The 28% wrapper bucket is labelled
  "the `parse_full` + `.len()` call frame inlined" — correct, but undifferentiated.
  A reader could mis-read a ~28% self-time frame outside the named parse leaves as a
  retained/second pass. Source check (`css_canon_bench.rs:104-105` shape; P1-B's own
  `css_cold_harness.rs` is the same `parse_full` + `black_box(.len())`) confirms it
  is `String::len()` + `black_box` + the LTO-collapsed recognition outer loop — pure
  scaffold, NOT a substrate. **Fix:** `p1b §2.3` — append to the row-2 cell: "(=
  `String::len()` + `black_box` + LTO-inlined `parse_stylesheet` outer loop; pure
  measurement scaffold, no retained state — verified `css_cold_harness.rs` body)".
  Same one-line fix applies to the §2.3 narrative "68.1% of in-binary self-time …
  excluding the harness wrapper" so the exclusion is grounded as scaffold not as a
  second path.
- §4.5 (pre-block check) "no second substrate, no sidecar … is proposed" —
  **ACCEPT.** I1/I2 honoured.
- §3 W6 typed-retime row — **ACCEPT** (I4, same exemplary contrast as P1-A §3).

### P1-C (`p1c-samply-mode-3.md`)

- §2.3 by-resource table (`css_cold_bench` 35.52% own-code) — **ACCEPT.** Resource
  bucketing is the ONE substrate's own-code vs syslib split; no parallel substrate.
- §2.4 row 2 `run_track1_full` 27.65% "harness loop frame … scaffold (inlined `&self`
  scan-loop return / black_box)" — **ACCEPT.** Unlike P1-B, P1-C explicitly names the
  27.65% as scaffold AND re-states the parse-only fraction with the scaffold excluded
  ("Discounting the 27.65% harness-loop scaffold"). I2 honoured — the wrapper is named
  as scaffold, not normalised into a substrate.
- §4 A1 "plane bifurcation … two distinct code paths over one grammar module …
  disjoint hot-leaf sets" — **ACCEPT.** This is the cleanest I3 statement across the
  six: it proves the two Track-1 planes do NOT share a hidden substrate (each retains
  only its own output), and it does not conflate them with the comparators.
- §4 A5 / pre-block — **ACCEPT.** "Neither benched plane touches
  `bbnf-simd/src/dispatch.rs`" + "no structural index until the tape decodes CSS"
  honours I1 (the tape is the only substrate, and it is unbuilt).
- §3 "core-tree, inherited" rows — **ACCEPT** (I4, named contrast +2.1pp/+0.5pp).

### P1-D (`p1d-pmu-cycles.md`)

- §2.4 hot-leaf table, row `main` 27.76% "harness (excluded from parse-only)" —
  **ACCEPT.** Named as harness scaffold AND arithmetically excluded ("parse-only
  (excl. the `main` harness loop)"); I2 honoured.
- §2.5 "redundant overlapping re-scan … each declaration body is walked 2-3× by
  `find_component_delim`" — **ACCEPT, with a CH5 note (no disposition change).** This
  is the one place a reader might infer a "second source scan." It is NOT a parallel
  substrate: it is the SAME scalar scanner re-invoked over overlapping spans within
  the ONE recognition pass (`parse_block_item:211` → `find_colon_before:219` →
  `parse_declaration:247`), all writing into the same `pos`/summary. The artefact
  correctly frames it as a structural inefficiency of the single pass, not a sidecar.
  I1 honoured. (Recorded so CH3/CH2 do not need to re-adjudicate it as coupling.)
- §2.3 "track1 ... is metadata-only and even cheaper ... does not even run the scan"
  — **ACCEPT.** Correctly distinguishes the `emit_fact_stream` entry from the
  recognition scan without conflating their symbol paths (I3).
- §4.7 "No SIMD on the CSS path (confirmed … no `bbnf_simd` frame in 20,900 samples)"
  — **ACCEPT.** Confirms I1 by symbol-table absence: the only substrate is the
  scalar generated runtime; no NEON sidecar.
- §3 W6 typed-retime / W8R rows — **ACCEPT** (I4).

### P1-E (`p1e-hot-leaf-attribution.md`)

- §2.3 / §2.4 hot-leaf tables — **ACCEPT.** Track-1 symbols only; comparators absent
  from the symbol tables (I3).
- §2.4 syslib-caller attribution "91.44% … reached FROM `emit_fact_stream`'s String
  growth" — **ACCEPT.** Strongest I1 attribution: the syscall+heap floor is bound to
  the ONE output-String substrate by an explicit caller-chain walk, not normalised as
  ambient cost.
- §2.5 roll-up "**no `tape` leaf at all** … `Tape`/`ValueRef`/`TapeBuilder` appear
  nowhere … the `dispatch` vehicle … appears nowhere" — **ACCEPT (I1 exemplary).**
  This is the empirical proof of substrate-union: the one substrate is unbuilt for
  CSS, verified by symbol absence. Matches the source grep (§2 above).
- §4.4 "the udot/i8mm digit kernel … has no benched CSS antecedent … S-P2 must
  re-profile the typed path after W1/W2, not inherit a … hypothesis from here" —
  **ACCEPT.** Honours the profile-first non-negotiable AND I4 (no carried hypothesis,
  no assumed un-benched path).
- §4.6 pre-block — **ACCEPT** (no second substrate; explicitly "no per-leaf eager
  payload … single non-generic `TapeBuilder`").

### P1-F (`p1f-bench-canonical.md`)

- §2.3 three per-plane tables (track1_full_parse / track1_fact_stream / lightningcss)
  — **ACCEPT (I3 exemplary).** The lightningcss table is a SEPARATE plane attribution
  (`cssparser::Parser::next_…`, `parcel_selectors::…`, `lightningcss::…` symbols) and
  is never merged with the `generated_css_l4_*` Track-1 symbols. The three tables are
  explicitly per-plane.
- §2.3 track1_full_parse row 2 `css_canon_bench::track1_full_parse` 26.74% "wrapper;
  maps to `emit_full_parse` … the `parse_full().len()` + black_box" — **ACCEPT.**
  Named as wrapper + `.len()` + black_box; I2 honoured (this is the differentiation
  P1-B §2.3 lacks).
- §4.4 "`tape_activated = false` for CSS (verified) … grep … returns ZERO; the JSON
  grammar DOES reference the tape" — **ACCEPT (I1 exemplary).** Independently
  reproduces the source grep this lens performed; the substrate-union baseline is
  established as a telemetry column.
- §4.6 "No second substrate / no sidecar introduced (Lock 1 / CH5) … introduces no
  retained cursor, no event vector, no parallel source pass. The samply driver loop
  is a transient measurement scaffold, not a parser-owned structure. Track 1 … and
  the comparators keep separate symbol paths" — **ACCEPT.** Direct, correct CH5
  self-statement; matches every source check.
- §3.2 / outcome enum "~70 Mbps / ~14× narrative … N-direct … the only ~3 Mbps figure
  is the … EAGER typed-CSSOM plane … SYNTHESIS §0.4 PRE-BLOCKS" — **ACCEPT** (I4:
  the un-benched eager plane is named + pre-blocked, never substituted).

## §4 — Cross-artefact CH5 finding (coordination, not substrate)

The six agents authored **five distinct harness binaries** (`css_cold_harness.rs`,
`css_cold_bench.rs`, `css_cold_canonical.rs`, `css_canon_bench.rs`,
`css_track1_profile.rs`; all uncommitted `??` per `git status`). This is NOT a CH5
substrate-union violation — every harness is a transient measurement scaffold over
the SAME benched `parser::parse` / `parser::parse_full` fns (verified
`css_canon_bench.rs:103-115`), introducing no parser-owned state, no second
substrate, no sidecar. Track 1 ≡ generated runtime is preserved in all five. It IS a
convergence concern for the orchestrator (S-P3 binds ONE canonical harness; the
duplicate scaffolds must be reconciled to a single committed bin before the gate
consumer binds), and it is flagged to CH4 (reproducibility: five harnesses yield five
slightly different medians) and to the consolidation — not dispositioned by CH5.
**No fix to any P1 artefact is required from this finding** under the CH5 lens; it is
recorded for the aggregator.

## §5 — Disposition summary

| Artefact | Sections dispositioned | ACCEPT | REVISE | REJECT |
|---|---|---:|---:|---:|
| P1-A | §2.2, §2.3, §3, §4.5 | 4 | 0 | 0 |
| P1-B | §2.2, §2.3, §3, §4.5 | 3 | 1 | 0 |
| P1-C | §2.3, §2.4, §4-A1, §4-A5/pre-block, §3 | 5 | 0 | 0 |
| P1-D | §2.3, §2.4, §2.5, §4.7, §3 | 5 | 0 | 0 |
| P1-E | §2.3/§2.4, §2.5, §4.4, §4.6 | 4 | 0 | 0 |
| P1-F | §2.3, §3.2, §4.4, §4.6 | 4 | 0 | 0 |
| **Total** | **26 sections** | **25** | **1** | **0** |

CH5 ACCEPT rate: **25/26 = 96.2%**. One REVISE (P1-B §2.3, CH5-adjacent I2): the
28.32% harness-wrapper frame must be explicitly named as `String::len()` +
`black_box` + LTO-inlined outer loop (pure scaffold) so no reader infers a retained
second pass — the same differentiation P1-C/P1-D/P1-F already carry. Zero REJECT.

The substrate union is **honoured by construction** across all six artefacts: the
benched tape is the one substrate and is empirically UNWIRED for CSS (zero tape
symbols in any CSS profile — source-grep confirmed); no parallel substrate, sidecar,
or retained cursor is observed or normalised; Track 1 (`generated_css_l4_*`) symbol
paths stay disjoint from the lightningcss/cssparser comparators; and every
`crates/core` / "core-tree" reference is a NAMED un-benched contrast, never a
substitute for a benched skinny measurement (I4 satisfied — the profile does not
assume `crates/core`).

## §6 — REVISE ledger (for the V2 fold)

| ID | Artefact:section | Defect | Concrete fix |
|---|---|---|---|
| CH5-V1-R1 | `p1b-samply-mode-2.md` §2.3 | 28.32% `css_cold_harness::track1_full` wrapper bucket undifferentiated — risk of inferring a retained/second pass (I2) | Append to the row-2 cell + the "excluding the harness wrapper" narrative: "(= `String::len()` + `black_box` + LTO-inlined `parse_stylesheet` outer loop; pure measurement scaffold, no retained state — verified `css_cold_harness.rs` body has no parser-owned structure)". Matches P1-C §2.4 / P1-F §2.3 framing. |
