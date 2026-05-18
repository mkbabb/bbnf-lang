# SK-V9 S-P2 CHALLENGE V1 — CH4 COST

Pass: S-P2 Research. Cycle: V1.
Lens: CH4 COST.
Date: 2026-05-18.
Scope: cost-audit of the six P2 reports at `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-{A..F}-*.md`, keyed to `restart/prompts/ORCHESTRATOR.md` §8 non-negotiables and `restart/prompts/skinny/PASS-2-RESEARCH.md` §3 CH4 (scalar-reference status + checkasm-parity expectation + same-wave-consumer note) and `PASS-3-SYNTHESIS-PLAN.md` §2 S-P3 scope.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

S-P1 V4 CH4 returned 14% ACCEPT on the first cycle because S-P1-D pre-empted S-P3 with uncosted wave proposals. S-P2 must not repeat: S-P2 proposes intervention shapes WITH preliminary cost envelopes; S-P3 owns final wave sequence + manifest. The lens scans each proposed intervention for the six CH4 surfaces:

1. LOC envelope (hand + regen).
2. Risk class (LOW / MEDIUM / HIGH; per-intervention, not one-row blanket).
3. Hard cap (preliminary minute budget per intervention; ORCHESTRATOR.md §9).
4. Same-wave consumer (no orphan kernel; ORCHESTRATOR.md §8).
5. Revert protocol (how to roll back if the falsifiability gate fires).
6. Pre-block reference (the specifically-named REDRESS entries each intervention must not re-open).

Plus two structural checks:

7. No S-P3 overreach (the proposal stops at intervention shape + preliminary envelope; it does not author the wave manifest, the §0 close-condition, or the wave sequence).
8. P2-F's §7 >SOTA path — each of the three coordinated interventions carries its own cost set OR explicitly defers cost-authorship to S-P3.

## §1 — Method (cost-audit protocol)

Per report, the lens enumerates each candidate intervention (one row per primitive / kernel / wave-shape), grades each across the eight checks above, and disposes ACCEPT / REVISE / REJECT. A row is ACCEPT if all eight checks pass. A row is REVISE if a check is missing but recoverable inside the artefact (e.g. add an explicit minute cap line; restate revert protocol in one sentence). A row is REJECT if a check is structurally violated (e.g. the intervention authors the wave manifest; the same-wave consumer is absent; the LOC envelope contradicts the cited code surface).

The lens does not relitigate CH1 correctness, CH2 generality, CH3 regression, CH5 hidden coupling, or CH6 paper-close — those are the other five lenses' surfaces. CH4 stays within cost discipline.

The S-P1 V4 failure mode binds the lens: a P2 report that authors a wave-sequence manifest under the guise of "cost envelope" is REJECTed for S-P3 overreach even if the rest of the envelope is sound. The HANDOFF candidate-boundary table (`restart/skinny/tranches/sk-v9/HANDOFF.md` §3) is the orchestrator's intervention shape pool; S-P2 elaborates against that table, S-P3 sequences across it.

## §2 — Disposition table per report

### §2.1 — P2-A (Union event-model — W3 fit-gate diagnosis + alternate design)

P2-A enumerates ONE intervention: the cursor/class split (class column co-emitted into the tape; SIMD structural index move-consumed). The artefact's §5 LOC table is the cost surface; §4 is the falsifiability gate.

| # | Intervention slice | LOC envelope | Risk class | Hard cap | Same-wave consumer | Revert protocol | Pre-block reference | S-P3 overreach? | Disposition |
|---:|---|---|---|---|---|---|---|---|---|
| A.1 | `runtime/src/tape/{mod,assembler}.rs` — add `classes: Vec<u8>`, `class_at`, `push_offset_with_class`; drop `push_plain_offset` once codegen migrated | +60 / -20 (§5 row 1) | LOW (stated) | NOT STATED at slice level; W3 plan-cited 90 min ceiling carried forward implicitly | `JsonNodeKind::at_cursor` (§2.4.2) | NOT STATED explicitly; deletable by reverting commit but no "if the falsifiability gate fires, do X" sentence | REDRESS 50, 51, 53, 60-72, 82-84, 88, 89, 92 (§6) | No — stays at primitive shape | **REVISE** (add explicit per-slice minute cap + revert sentence; otherwise CH4-clean) |
| A.2 | `runtime/src/grammars/json/parser.rs` — `emit_plain_offset` → `emit_event_offset(offset, class)` | +15 templates (§5 row 2) | LOW (stated) | absent | A.1 substrate consumer | absent | as above | No | **REVISE** (same as A.1) |
| A.3 | `runtime/src/grammars/json/generated.rs` — class-ordinal emit at each callsite; `consume_structural` deleted | +80/-50 regen | MEDIUM (stated) | absent | A.1+A.2 | absent | as above | No | **REVISE** |
| A.4 | `runtime/src/grammars/json/value.rs::JsonNodeKind::at_cursor` — byte-rediscovery → class-column read | +5/-15 regen | LOW (stated) | absent | self-consumer (this IS the same-wave consumer for A.1) | absent | as above | No | **REVISE** |
| A.5 | `codegen/src/json_templates/{generated,parser,view,value}.rs` — emit class column write, structural-walk lowering, `class_at` read | +120 templates | MEDIUM (stated; "checkasm-style parity tests required") | absent | A.3+A.4 (consumer is the regenerated output) | absent | as above | No — codegen is the template surface, not a wave manifest | **REVISE** (named "checkasm-style parity tests required" but no separate per-template checkasm gate) |
| A.6 | `bbnf-simd/src/lib.rs` — already produces `StructuralAlphabet::class_table`; surface as move-consume API | +20 | LOW (stated) | absent | A.3 (parser consumes the index) | absent | as above | No | **REVISE** |
| A.7 | `runtime/src/grammars/json/scan.rs` — stop discarding the index; expose move-consume API | +10/-5 regen | LOW (stated) | absent | A.6+A.3 | absent | as above | No | **REVISE** |
| A.8 | `bbnf-bench/src/parity.rs` — class-column parity assert; structural-index move-consumed assert | +30 (§5 row 8) | LOW (stated) | absent | gate-only telemetry | absent | as above | No | **REVISE** |

**P2-A aggregate verdict.** Cost surface present in §5; risk class stated per slice; LOC envelope ~265 hand + ~120 regen, well inside the W3 plan-cited 450 default / 650 exceptional budget. Same-wave consumer A.4 (`at_cursor` byte-rediscovery → class-column read) is the load-bearing production consumer, correctly named at §2.4.2 — this is the artefact's strongest CH4 surface. Pre-block ledger at §6 is exhaustively cited. The single recoverable defect is the absence of an explicit minute cap per slice (only an implicit 90-min ceiling) and the absence of one-sentence revert protocols per slice. P2-A does **not** author the wave manifest — it elaborates intervention shape against HANDOFF §3 row 2. **REVISE 8/8** rows. CH4 verdict for the report: **REVISE** (cost surface present; eight per-slice revisions add minute cap + revert sentence).

### §2.2 — P2-B (Retained class/event grammar + `ValueRef` cursor proof)

P2-B enumerates ONE intervention with five owner-file slices. The artefact's §6 LOC + risk envelope is the cost surface; §5.1 formally disposes the same-wave-consumer rule.

| # | Intervention slice | LOC envelope | Risk class | Hard cap | Same-wave consumer | Revert protocol | Pre-block reference | S-P3 overreach? | Disposition |
|---:|---|---|---|---|---|---|---|---|---|
| B.1 | `runtime/src/tape/event_grammar.rs` (NEW) — `EventGrammar` trait + `ValueRef` `PhantomData` rename | ~110 (§6.1) | R1 (LOW with mitigation: `#[cfg]` gate + zero non-proof callers `rg` audit) | 15 min (§6.3) | proof-only, no production consumer — §5.1 formally disposes the rule | "negative compile test rejection" (§2.3) + drop files on REJECT | REDRESS 60-72, 71 (admitted), 92 (direct antecedent) (§4) | No — proof-only, no wave plan | **ACCEPT** |
| B.2 | `runtime/src/tape/event_grammar_tests.rs` (NEW) — type-level `_proof_compiles` lines | ~80 | LOW (R5: differential vs REDRESS 71) | 15 min (§6.3) | self-consumer (the proof IS the consumer) | absent except in §2.3 prose; the `const _: fn() = …` lines failing to compile is the verification surface | as above | No | **ACCEPT** |
| B.3 | `runtime/src/grammars/json/event_grammar_witness.rs` (NEW) — JSON `EventGrammar` instance | ~120 | R2 (LOW: `_witness` suffix + `find -mindepth 1 -maxdepth 1` audit) | 10 min (§6.3) | B.2 (test) | drop file on REJECT | as above | No | **ACCEPT** |
| B.4 | `runtime/src/grammars/sheets_witness/event_grammar_witness.rs` (NEW) — Sheets Lock-14 witness | ~80 | R6 (LOW: absence of Sheets grammar source strengthens the demonstration) | 10 min (§6.3) | B.2 (test) | drop file on REJECT | as above | No | **ACCEPT** |
| B.5 | `runtime/src/lib.rs` `cfg`-gated re-exports | ~5 | R4 (LOW: `rg 'event_grammar' skinny/crates/bbnf-bench/` = 0) | 5 min (§6.3) | B.1-B.4 | revert the 5 lines on REJECT | as above | No | **ACCEPT** |

**P2-B aggregate verdict.** Cost surface present in §6.1; risk envelope at §6.2 (R1-R6 with named mitigations); time envelope at §6.3 sums to ~90 min ceiling matching HANDOFF; total ~395 LOC inside the 450 LOC envelope. The same-wave-consumer rule disposition at §5.1 is the artefact's strongest CH4 surface: the rule "binds substrates, not contracts; the proof is a contract; therefore the rule is silent" is a rigorous formal disposition, not a deferral. P2-B does **not** author the wave manifest — it specifies a proof-only candidate boundary HANDOFF §3 row 2 already names. No S-P3 overreach. **ACCEPT 5/5** rows. CH4 verdict for the report: **ACCEPT**.

### §2.3 — P2-C (Apache + CITM measured-row typed admission methodology)

P2-C enumerates ONE wave-shape ("W{n}-A: Apache + CITM Typed Row-Table Admission") with seven owner-file slices at §4.1. The artefact's §2.5 / §4.3 set the falsifiability gates; §4.2 the dispatch sequence.

| # | Intervention slice | LOC envelope | Risk class | Hard cap | Same-wave consumer | Revert protocol | Pre-block reference | S-P3 overreach? | Disposition |
|---:|---|---|---|---|---|---|---|---|---|
| C.1 | `report.rs:709` (`SK_V8_OPEN_BASELINE`) — add two new rows; optional rename to `SK_V9_OPEN_BASELINE` | NOT STATED at slice level; HANDOFF-cited total 300 LOC | NOT STATED at slice level; report-wide row-table-only framing implies LOW | ≤90 min total (HANDOFF, cited §4 prose); not per slice | gate-only (telemetry; not a parse-loop consumer) | "A wave that misses any gate halts at the redress phase, records the falsified gate in REDRESS, and routes back into S-P2/S-P3 without promoting the row" (§4.3 close) | REDRESS 60-72, 71 (admitted), 91 binding for canada, 92, 93 (§6) | No — row-table admission, not a parser intervention | **REVISE** (add per-slice LOC) |
| C.2 | `gate.rs:1820-1831` — flip `w0_real_typed_metadata_expected` assertions Apache/CITM `!expected` → `expected` | absent | absent | as above | C.1 | as above | as above | No | **REVISE** |
| C.3 | `skinny/RESULTS.md` — promoted row block + schema-v3 telemetry rows; refreshed run-id across file | absent | absent | as above | self (the row IS the artefact) | as above | as above | No | **REVISE** |
| C.4 | `bbnf-bench/target/skv9-w{n}/criterion/` — fresh same-run Criterion capture across 21+8 ids | not a LOC item (artefact-only) | LOW (capture envelope cited from W0) | as above | self | as above | as above | No | **ACCEPT** (LOC-irrelevant; capture is the artefact) |
| C.5 | `skinny/REDRESS.md` — new entry recording the promotion | absent (typically ~50-100 lines of prose) | absent | as above | docs (not a runtime consumer) | as above | as above | No | **REVISE** |
| C.6 | `restart/skinny/tranches/sk-v9/HANDOFF.md` §3 row 1 — move candidate from "may admit" to "admitted" | trivial (~5 lines) | LOW | as above | self | as above | as above | No — handoff state-update, not wave-manifest authoring | **ACCEPT** |
| C.7 | `restart/locks/LOCKS.md` — add `sk-v9-real-typed-w{n}` parent-diff allowance | trivial (~5-10 lines) | LOW | as above | scoped to seven owner paths | as above | as above | No — Lock 14 allowance, scoped | **ACCEPT** |

**P2-C aggregate verdict.** Cost surface mostly inherited from HANDOFF §3 row 1 (300 LOC, ≤90 min); per-slice LOC and risk class are not broken out, which weakens the artefact's CH4 surface relative to P2-A and P2-B even though the overall envelope is sound. The same-wave consumer is `gate_only` (per §2.4 telemetry binding) — this is the correct framing because the wave produces no parser intervention and no kernel; the consumer is the gate's row-table admission. Revert protocol is **stated** at §4.3 close ("halts at redress, records the falsified gate, routes back into S-P2/S-P3") — this is the artefact's strongest CH4 surface and demonstrates the discipline the other five reports should match. Pre-block citations at §6 are exhaustive. P2-C does **not** author the wave manifest — it specifies an admission methodology against an explicit HANDOFF candidate. **REVISE 4/7, ACCEPT 3/7** rows. CH4 verdict for the report: **REVISE** (per-slice LOC + risk class break-out; otherwise CH4-clean and exemplary on revert protocol).

### §2.4 — P2-D (Host-targeted aarch64 ASM/SIMD opportunities for the four uncloseable rows)

P2-D is the survey artefact across §3 unicode codec, §4 string-block widening, §5 structural-bitmap chain (with §5.3.1 EOR3 prefix-XOR + §4.4 CSSC CTZ at the string-mask consumer + §3.6 deferred TBL-fold floor). There is NO single LOC envelope (this is a survey, not a single intervention plan); each §-numbered opportunity carries its own cost surface inline.

| # | Intervention | LOC envelope | Risk class | Hard cap | Same-wave consumer | Revert protocol | Pre-block reference | S-P3 overreach? | Disposition |
|---:|---|---|---|---|---|---|---|---|---|
| D.1 | §3 vectorised `\uXXXX` codec body (x4 batched, NEON kernel already in-tree at `unescape_uxxxx.rs:125`) | NOT STATED in P2-D (P2-E §7.1 carries the full LOC envelope for the codec; P2-D defers to P2-E) | NOT STATED at slice; "MEDIUM" implied by REDRESS 82 differential at §3.5 | absent | "union-substrate string-content materialiser at the tape-cell projection layer" (§6.4 / §3.5.2) | absent | REDRESS 82 (§3.5 three-axis differential) | No — defers cost set to P2-E and consumer set to P2-A; this is correct deferral | **REVISE** (state the deferral explicitly: "LOC + risk authored by P2-E §7; consumer authored by P2-A §2") |
| D.2 | §4 32-byte string-block scanner widening (`scan_string_special_block_32`) at `match_string_at_quote_trusted_utf8` | NOT STATED | NOT STATED ("µop-neutral per byte unless… interesting fold collapses first-only" — qualitative) | absent | "existing `match_string_at_quote_trusted_utf8`" (§4.3) — same call site, wider blocks | absent | REDRESS 83 (§4.3 three-axis differential) | No — primitive shape only | **REVISE** (state preliminary LOC + risk; defer final to S-P3) |
| D.3 | §4.4 CSSC CTZ at string-mask consumer first-set extract (NOT the bulk-emit consumer that REDRESS 89 rejected) | NOT STATED | NOT STATED ("different call site" — qualitative differential vs REDRESS 89) | absent | "union-substrate string-mask consumer" (§4.4.3) | absent | REDRESS 89 (§4.4 three-axis differential) | No | **REVISE** (preliminary LOC + risk) |
| D.4 | §5 structural-bitmap chain with SHA3 EOR3 prefix-XOR alternative (NOT the PMULL body REDRESS 88 rejected) | NOT STATED | NOT STATED ("6 cheap µops for 3 1-cycle µops, monotonically faster" — qualitative) | absent | "union-substrate's typed event cursor (P2-A scope)" (§5.4) | absent | REDRESS 88 (§5.3 differential) | No — defers wiring to P2-A | **REVISE** (preliminary LOC + risk; defer final to P2-A's S-P3 fold) |
| D.5 | §5.3.3 VEXT-based cross-chunk carry for quote/escape state | NOT STATED | LOW (no extension needed; "admits under FEAT_NEON baseline") | absent | D.4 consumer | absent | not a REDRESS pre-block reference (this is a primitive shape new to the cohort) | No | **REVISE** (preliminary LOC + risk) |
| D.6 | §3.6 TBL-fold floor (a Wave 2+ optimisation explicitly deferred) | "Wave 2+ optimisation" (deferred) | "deferred" | n/a | n/a | n/a | n/a | No — explicit deferral is correct | **ACCEPT** (correct deferral) |
| D.7 | §6.2 missing checkasm gates: `checkasm_unescape_uxxxx.rs`, `checkasm_string_block.rs`, `checkasm_match_tiny_plain_string.rs`, `checkasm_digit_mac.rs`, `checkasm_movemask.rs` | "before wiring any new primitive into a hot path, the primitive ships a `checkasm_<name>.rs` differential test" (§6.2 closing sentence) | LOW (parity tests; the discipline gate is binary) | absent | gate-only (the test IS the consumer) | absent | dav1d process invariant 1 (§6.3) | No | **REVISE** (preliminary LOC per missing test; defer prioritisation to S-P3) |
| D.8 | §6.3 invariants 2-5 (forced feature masks, ABI-checked-call shim, recoverable fault, cycle-counter source) | "SK-V10+ work per the SK-V7 A3 §2 menu and skv6-B2" (deferred) | "deferred" | n/a | n/a | n/a | n/a | No — correctly deferred | **ACCEPT** (correct deferral with rationale) |

**P2-D aggregate verdict.** P2-D is the survey artefact across the dav1d-discipline opportunity space. The artefact intentionally does not author a single LOC envelope — its job per the PASS-2-RESEARCH §2 P2-C scope is *host-architecture instruction inventory* — and the qualitative cost arguments (µop counts, latency comparisons, retire-port pressure) are the correct currency for a survey. However, CH4 binds every named opportunity to *some* preliminary LOC + risk envelope so S-P3 can sequence; the artefact's reliance on P2-A / P2-E for downstream cost-set-authoring is correct but must be stated explicitly per opportunity. The §6.2 missing-checkasm-gate enumeration is the artefact's strongest CH4 contribution: it converts five primitives into a discipline-gated admission queue. Pre-block differentials at §7 synthesis table are exhaustive across REDRESS 28, 33, 50-55, 60-62, 64, 66-69, 82, 83, 84, 88, 89, 90. P2-D does **not** author the wave manifest — it surveys the host surface against HANDOFF §3 rows 3-4. **REVISE 6/8, ACCEPT 2/8** rows. CH4 verdict for the report: **REVISE** (add preliminary LOC + risk per opportunity, with explicit "final cost-set authored by P2-{X} / S-P3" deferral lines; the survey shape is correct).

### §2.5 — P2-E (Unicode-escape codec primitive `escape_codec_hex_unit`)

P2-E enumerates ONE intervention with twelve owner-file slices at §7.1; §6 sets the falsifiability gates; §7.2 the risk envelope.

| # | Intervention slice | LOC envelope | Risk class | Hard cap | Same-wave consumer | Revert protocol | Pre-block reference | S-P3 overreach? | Disposition |
|---:|---|---|---|---|---|---|---|---|---|
| E.1 | `bbnf-simd/src/aarch64/escape_codec/mod.rs` (NEW) — const-generic kernel surface | ~80 (§7.1) | NONE (§7.2 Lock surface) | NOT STATED at slice level; wave envelope §7.4 has no minute cap | E.10+E.11 | NOT STATED | REDRESS 82 (§5 five-axis differential) | No | **REVISE** (add minute cap + revert sentence) |
| E.2 | `escape_codec/scalar.rs` (NEW) — scalar reference, parameter-bound | ~120 | LOW (correctness; §7.2) | absent | E.12 (checkasm) | absent | REDRESS 82 | No | **REVISE** |
| E.3 | `escape_codec/hex_x4_neon.rs` (NEW) — fixed-4 NEON body | ~150 | LOW (§7.2 "correctness — single quartet" LOW) | absent | E.10 | absent | REDRESS 82 | No | **REVISE** |
| E.4 | `escape_codec/hex_x8_neon.rs` (NEW) — fixed-8 NEON body (TOML `\U`) | ~140 | LOW (parity with E.3 shape) | absent | (TOML consumer NOT shipped same-wave; only codegen template lands) | absent | REDRESS 82 | No — TOML is out-of-band; the binding is shipped but not wired | **REVISE** (state TOML consumer status: shipped-without-consumer at codegen-template depth, justified by const-generic specialisation argument; otherwise borderline orphan kernel) |
| E.5 | `escape_codec/hex_variable_neon.rs` (NEW) — variable-width NEON body | ~180 | MEDIUM (§7.2 "Correctness — variable digit (CSS L4 / JS)" MEDIUM) | absent | E.11 (CSS L4 sketch) | absent | REDRESS 82 | No | **REVISE** |
| E.6 | `escape_codec/surrogate_join.rs` (NEW) — scalar pair-join | ~50 | LOW | absent | E.10 | absent | REDRESS 82 | No | **REVISE** |
| E.7 | `bbnf-simd/tests/checkasm_escape_codec.rs` (NEW) — ~6000 cases × 4 bindings | ~250 | LOW (parity gate) | absent | E.1-E.6 (the test IS the consumer for the kernel bodies) | absent | dav1d invariant 1 | No | **REVISE** |
| E.8 | `codegen/src/escape_codec_template.rs` (NEW) — const-generic emission for the four bindings | ~120 | MEDIUM (§7.2 "Maintenance — const-generic explosion" MEDIUM; 5 specialisations × ~250 LOC body ≈ 1.3 KB hot path) | absent | E.10+E.11 | absent | REDRESS 82 | No | **REVISE** |
| E.9 | Existing kernel removal at `unescape_uxxxx.rs` | -215 (deletion) | LOW (superseded by E.3) | absent | self (the removal IS the consumer migration) | absent | REDRESS 82 (W4 attempt artefacts go out with the removal) | No | **REVISE** |
| E.10 | `parse-that-regex/src/lib.rs:775-786` consumer edit — `Some(b'u')` arm | ~30 | LOW (call-site swap) | absent | self (this IS the same-wave production consumer) | absent | REDRESS 82 | No | **REVISE** |
| E.11 | `bbnf-css/src/tokenizer/escape.rs` — CSS L4 consumer sketch + unit test | ~40 | LOW (sketch + unit test) | absent | self (Lock-14 same-wave generality demonstration; the second grammar that calls the kernel) | absent | REDRESS 82 (§4.2 CSS L4 sketch refutes JSON-overfit) | No | **REVISE** |
| E.12 | `runtime/src/grammars/json/sink.rs` consumer edit — trivial call-site swap | ~10 | LOW | absent | E.10 | absent | REDRESS 82 | No | **REVISE** |

**P2-E aggregate verdict.** Cost surface present at §7.1 (LOC table) and §7.2 (risk envelope per axis); total ~780 net / ~1025 with tests. The risk-class table at §7.2 is the artefact's strongest CH4 surface — it explicitly classes each axis (correctness × digit-width, performance × per-row, locks 1/14/16, REDRESS 82) at LOW / MEDIUM / MEDIUM-HIGH / HIGH. The same-wave consumer cardinality at §4 is **two grammars** (JSON `unescape_string` load-bearing + CSS L4 sketch), correctly answering CH2 GENERALITY simultaneously and providing the explicit material differential to REDRESS 82's single-grammar consumer. The honest §6.4 verdict ("Closes / Approaches / Does not close / Does not affect" per row) is the artefact's strongest CH6 anti-paper-close discipline. The two recoverable defects are (i) absence of per-slice minute caps + revert protocols; (ii) E.4 (TOML `\U` binding) is shipped at codegen-template depth without a same-wave TOML consumer — the artefact justifies this as const-generic specialisation that emits no code unless a TOML grammar is loaded, which is structurally correct but should be stated explicitly to pre-empt a CH4 "orphan kernel" flag at S-P3 fold. P2-E does **not** author the wave manifest — §7.4 is a "wave envelope" summarising the cost set, not a wave sequence. **REVISE 12/12** rows. CH4 verdict for the report: **REVISE** (cost surface present and the strongest of the six reports on risk-class breakout; twelve per-slice revisions add minute cap + revert sentence + explicit TOML-no-consumer disposition).

### §2.6 — P2-F (SOTA teardown for parse + node speed on Apple M5 Max)

P2-F's §1-§6 are competitor teardown (CH2 generality + CH1 correctness lens surfaces). §7 is the load-bearing CH4 surface: three coordinated interventions in a prescribed sequence (I → II → III). §7.4 is the cumulative impact projection.

| # | Intervention | LOC envelope | Risk class | Hard cap | Same-wave consumer | Revert protocol | Pre-block reference | S-P3 overreach? | Disposition |
|---:|---|---|---|---|---|---|---|---|---|
| F.1 | §7.1 Intervention I — Consume stage-1 index (P2-A union substrate Tier A) | "+150 source net" + per-crate breakdown (§7.1 "Cost." line) | NOT STATED | NOT STATED | "The JSON retained parser at the `consume_structural` call sites" (§7.1) — defers to P2-A §2.4 | absent | "SC-2 §3.3 carries the owner paths, LOC budget, and verification harness" — defers | No — explicit deferral to P2-A | **REVISE** (defer all cost authorship to P2-A explicitly; the inline `+150 source net` is consistent with P2-A's `~265 hand + ~120 regen` so no contradiction, but the framing must read as deferral not as authorship) |
| F.2 | §7.2 Intervention II — Fused `\uXXXX` codec (P2-E unicode codec) | NOT STATED in P2-F (defers to P2-E) | NOT STATED | NOT STATED | "The retained-parse string match path *and* the DirectBuild field-fact emit site for unicode-bearing typed structs" (§7.2) — extends P2-E §4 with a second consumer at the DirectBuild step | absent | defers to P2-E §5 REDRESS 82 differential | No — explicit deferral | **REVISE** (defer cost set to P2-E §7) |
| F.3 | §7.3 Intervention III — Cost-fact-gated NEON tiny-string equality (`match_tiny_plain_string` re-wiring) + `BITMAP_NEXT_SET_BIT` consumer wiring | NOT STATED | NOT STATED | NOT STATED | "The DirectBuild dispatch and the retained-parse next-structural seek. Both consumers ship with the kernel." (§7.3) | absent | "per SK-V7-A2 §8 admission #4/#5 + P1-V3-B Layer-1 macro vocabulary" — defers | No | **REVISE** (defer cost set; specify which of P2-A / P2-D / P2-E owns the cost authorship per sub-kernel; the tiny-string re-wiring touches REDRESS 28+33 which neither P2-A nor P2-D explicitly own — the absence of a clear owner is the load-bearing CH4 defect of P2-F) |
| F.4 | §7.4 sequencing table + cumulative impact projection ("After I / After I+II / After I+II+III") | n/a (projection table) | n/a | n/a | n/a | n/a | n/a | **YES — this is the S-P3 overreach surface.** §7.4 sequences the waves (I → II → III) AND projects cumulative throughput per row class AND names the >SOTA close criterion. PASS-3-SYNTHESIS-PLAN §2 P3-B (wave sequencing) and P3-C (falsifiability gates) own this material. | **REJECT** (§7.4 sequencing table is S-P3 P3-B/P3-C territory; P2-F must either reframe §7.4 as "preliminary intervention dependency graph" with explicit "S-P3 P3-B authors the final wave sequence; this table is the input to P3-B, not its output" OR delete §7.4 and let S-P3 derive it from the three intervention shapes) |

**P2-F aggregate verdict.** The §1-§6 competitor teardown is CH1/CH2 surface and CH4-irrelevant. The §7 three-intervention coordinated path is the load-bearing CH4 surface: each intervention is correctly framed as **the shape**, with cost authorship explicitly deferred to P2-A / P2-D / P2-E. F.1 and F.2 defer cleanly; F.3 has a load-bearing ambiguity (the `match_tiny_plain_string` re-wiring touches REDRESS 28+33 which sit in a no-owner gap between P2-D's host-cap survey and P2-A's substrate elaboration — S-P3 must resolve which P2-{X} authors this cost set or whether it admits as a P2-D §3 cost set extension). The load-bearing defect is **§7.4**: the sequencing table + cumulative impact projection + >SOTA close-criterion sentence reach into PASS-3-SYNTHESIS-PLAN §2 (P3-B wave sequencing + P3-C falsifiability gates + P3-F SPEC drafting). This is the S-P1 V4 CH4 failure mode recurring at the S-P2 layer: a research artefact authoring the wave sequence under the guise of "intervention cost set." **REVISE 3/4, REJECT 1/4** rows. CH4 verdict for the report: **REVISE** with one **REJECT** row at §7.4 (reframe as preliminary dependency graph with explicit S-P3 deferral, OR delete the table; the three intervention shapes I/II/III at §7.1-§7.3 stand as CH4-clean once their cost-authorship deferrals are made explicit).

## §3 — Aggregate verdict

Per-report disposition summary across 44 enumerated intervention slices (the count is the sum of rows in the six tables above, exceeding the CH4 specification's "≥30 total" floor):

| Report | ACCEPT | REVISE | REJECT | Total | Report verdict |
|---|---:|---:|---:|---:|---|
| P2-A | 0 | 8 | 0 | 8 | REVISE |
| P2-B | 5 | 0 | 0 | 5 | ACCEPT |
| P2-C | 3 | 4 | 0 | 7 | REVISE |
| P2-D | 2 | 6 | 0 | 8 | REVISE |
| P2-E | 0 | 12 | 0 | 12 | REVISE |
| P2-F | 0 | 3 | 1 | 4 | REVISE (with one REJECT row) |
| **TOTAL** | **10** | **33** | **1** | **44** | **REVISE (cohort-level)** |

ACCEPT rate: 10/44 = 22.7%. REVISE rate: 33/44 = 75.0%. REJECT rate: 1/44 = 2.3%.

**Convergence verdict.** The cohort is **below the 95% ACCEPT threshold** that PASS-2-RESEARCH §4 requires for advancement. CH4 disposes: **REVISE** at the cohort level; V2 dispatch must fold the per-slice revisions and the §7.4 P2-F reframe.

**The S-P1 V4 failure mode is recurring at one site only** — P2-F §7.4 — and is structurally fixable in V2 by either reframing the table as a preliminary dependency graph or deleting it. The other five reports stay correctly on the intervention-shape-plus-preliminary-envelope side of the S-P2/S-P3 boundary; the 33 REVISE rows are recoverable inside each artefact via per-slice minute caps + one-sentence revert protocols (and, for P2-D, per-opportunity preliminary LOC + risk lines with explicit "final cost-set authored by P2-{X}" deferrals).

**Strongest CH4 surfaces across the cohort** (lessons for V2 fold):

1. **P2-B §5.1 same-wave-consumer rule disposition** — the formal "the rule binds substrates, not contracts" argument is the cohort's exemplar for handling rule-applicability questions inside the artefact rather than punting to S-P3.
2. **P2-C §4.3 revert protocol** — the sentence "A wave that misses any gate halts at the redress phase, records the falsified gate in REDRESS, and routes back into S-P2/S-P3 without promoting the row" is the cohort's exemplar revert protocol; every report should adopt this language pattern.
3. **P2-E §7.2 risk envelope per axis** — the LOW/MEDIUM/MEDIUM-HIGH/HIGH × correctness/performance/maintenance/locks grid is the cohort's exemplar risk-class breakout; P2-A's `LOW / LOW / MEDIUM / LOW / MEDIUM / LOW / LOW / LOW` per-slice table is a leaner variant and both are CH4-clean.
4. **P2-D §6.2 missing checkasm gate enumeration** — converting "what checkasm tests are missing" into an explicit five-row queue is the cohort's exemplar dav1d-discipline binding.

**Weakest CH4 surfaces across the cohort** (must fold in V2):

1. **Hard cap absence** — none of the six reports carries explicit per-slice / per-intervention minute caps. Only P2-B §6.3 sums per-slice minutes to a per-report total (~90 min); the others inherit minute caps from HANDOFF §3 or leave them implicit. V2 must add per-intervention minute caps in every report; ORCHESTRATOR.md §9 "Every dispatch carries an explicit minute cap" binds.
2. **Revert protocol absence** — only P2-C states the revert protocol explicitly. V2 must add a one-sentence revert protocol per intervention in every report.
3. **P2-D LOC absence** — survey artefacts must carry preliminary LOC + risk per opportunity (with explicit deferral to the cost-authoring P2-{X}) so S-P3's P3-A shortlist can rank candidates by cost; current qualitative-only framing forces P3-A to re-derive per-opportunity cost.

## §4 — Specific cost gaps requiring V2 fold

Per the §3 weakest-surfaces analysis, V2 dispatch should carry these targeted folds. The lens does not author the V2 dispatch (that is the orchestrator's role under ORCHESTRATOR.md §5.4) — it enumerates the cost gaps so the orchestrator's fold is complete.

### §4.1 — Per-slice minute caps (all six reports)

Every intervention slice carries an explicit minute cap. The cap is preliminary (S-P3's P3-B finalises wave-level caps); the discipline is that no slice is uncapped. Reference: ORCHESTRATOR.md §9 "Every dispatch carries an explicit minute cap. At 0.9× the cap the agent commits what it has; at the cap it halts."

Suggested cap discipline (the lens proposes; V2 author owns the value):

- A slice with hand-LOC ≤ 30: ~15 min.
- A slice with hand-LOC 30-100: ~30 min.
- A slice with hand-LOC > 100: ~45-60 min, with checkasm parity counted separately if applicable.
- A regen slice (codegen-emitted): ~10 min of regen + verification, plus the codegen-template hand-LOC.

### §4.2 — Per-intervention revert protocol (P2-A, P2-B, P2-D, P2-E, P2-F)

Every intervention carries a one-sentence revert protocol modelled on P2-C §4.3: "If the falsifiability gate at §{X} fires, the wave halts at the redress phase, records the falsified gate in REDRESS, and routes back to S-P2/S-P3 without admitting the intervention." For proof-only candidates (P2-B), the revert is "drop the new files and the `lib.rs` re-exports."

### §4.3 — P2-D preliminary LOC + risk per opportunity

P2-D's §3, §4, §4.4, §5, §5.3.1, §5.3.3 each carry a preliminary LOC envelope (even if range-valued, e.g. "100-200 hand LOC depending on whether the EOR3 fold subsumes the scalar prefix-XOR or merely augments it") + a risk class + an explicit "final cost-set authored by P2-{X} / S-P3 P3-A" deferral line. The §6.2 missing-checkasm enumeration carries an LOC-per-test estimate (the existing `checkasm_unescape_uxxxx` baseline is ~250 LOC; missing tests scale similarly).

### §4.4 — P2-E E.4 TOML `\U` consumer status

P2-E's §4 same-wave consumer plan ships JSON (load-bearing) + CSS L4 (sketch); TOML `\U` lands at codegen-template depth (E.4) without a TOML grammar consumer. The const-generic specialisation argument is correct: a binding that is never instantiated emits no code. V2 must state this explicitly with a sentence like "E.4 (`hex_x8_neon.rs`) is shipped as a codegen-template binding; absent a TOML grammar consumer, the specialisation is never instantiated and emits zero hot-path code — this is structurally distinct from an orphan kernel because the kernel body is the deferred output of a deferred specialisation, not an admitted runtime path."

### §4.5 — P2-F §7.4 reframe or delete

P2-F's §7.4 sequencing table + cumulative impact projection + >SOTA close criterion is the S-P3 P3-B/P3-C/P3-F surface. V2 must either:

- **Reframe** §7.4 as "preliminary intervention dependency graph: I unblocks II's `DirectBuild + structural-tape substrate` claim; II precedes III's tiny-string re-wiring because III's same-wave consumer at DirectBuild expects the codec-fused emit. S-P3 P3-B authors the final wave sequence; this graph is P3-B's input, not its output. S-P3 P3-C authors the falsifiability gates; §7.4's cumulative impact column is non-binding projection, not the close criterion."
- **Delete** §7.4 and let S-P3 derive the dependency graph from the three intervention shapes at §7.1-§7.3 plus the candidate-pool reads from P2-A / P2-D / P2-E.

The reframe is preferred because the dependency graph is a useful S-P3 input; the projection column must be either dropped or marked non-binding. The >SOTA close-criterion sentence at §7.4 paragraph 2 must move to a clearly-marked "S-P3 P3-C input" framing.

### §4.6 — P2-F §7.3 owner-resolution for `match_tiny_plain_string` re-wiring

P2-F's Intervention III names cost-fact-gated NEON tiny-string equality as a sub-kernel; this touches REDRESS 28+33 which sit in a no-owner gap between P2-D's host-cap survey (P2-D §5.5 cites the differential vs REDRESS 28/33 but does not author the cost set) and P2-A's substrate elaboration (P2-A §6 cites REDRESS 28/33 only as cardinality/Lock-1 pre-blocks, not as a positive intervention surface). V2 must resolve which P2-{X} authors this cost set. Two routes:

- **Route a**: P2-D §3 extends to a §3a "tiny-string equality cost set" with the LOC + risk + same-wave consumer (DirectBuild dispatch + retained-parse next-structural seek).
- **Route b**: A new P2-G agent dispatches with the tiny-string-equality-as-cost-fact scope; or P3-A admits the gap as a Cohort B candidate with deferred-author status.

Route a is the minimum-perturbation route and is the lens's preference.

---

End of CH4 disposition. Cycle V2 of S-P2 will fold the per-slice revisions and the §7.4 P2-F reframe; cohort ACCEPT rate is projected to cross the 95% threshold after V2 if the §4.1-§4.6 gaps are folded uniformly.
