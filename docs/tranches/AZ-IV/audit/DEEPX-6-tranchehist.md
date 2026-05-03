# DEEPX-6 — Last 20 Tranches Deep Assay

**Auditor**: DEEPX-6 (TURING-ASSAY) — read-only tranche-history forensic lane
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-deepX-6`
**Base**: `master 40e1835d` (post-AZ-IV close + recycled-BA + canonical re-ordering at `40092b28`)
**Mandate**: extend DEEP-D's archaeology with last-20-tranche assay; reveal patterns; recommend BA/BB/BC discipline.

This document complements `DEEP-D-tranche-reordering.md` (which canonicalised letters) and `DEEP-SYNTHESIS.md` (which picked Option A). DEEP-D answered *which letter goes where*; this audit answers *how the tranches actually behaved across 20 cycles* and *what the patterns predict for BA/BB/BC*.

## I — Last-20 Ledger (most recent at top; per-tranche entry)

For each tranche: **Promised** (plan.md), **Landed** (FINAL.md), **Carries** (top routed items), **Latency** (open→close commit count where extractable), **Pattern** (close shape).

| # | Letter | Opened | Closed | Latency | Pattern |
|---:|---|---|---|---:|---|
| 1 | **AZ-IV** | 2026-05-01 | 2026-05-02 (`cb14970f`) | ~2d / 7 waves | complete_with_misses |
| 2 | **AZ-III** | 2026-04-30 | 2026-04-30 (`d071daf9`) | <1d / 9 waves | terminal_with_carries |
| 3 | **AZ-II** | 2026-04-23 | 2026-04-30 (continuation) | 7d / 14 sub-stages | continuation_handoff |
| 4 | **AZ-I** | 2026-04-23 | 2026-05-01 (`91fda8d7`) | 8d / 5 waves | complete_with_misses |
| 5 | **B7** | 2026-04-27 | 2026-04-27 | <1d / 3 waves | GENUINE close |
| 6 | **B6** | 2026-04-27 | 2026-04-27 | <1d / 3 waves (1 landing, 2 vacuous) | GENUINE_partial (W0 only) |
| 7 | **B5** | 2026-04-26 | 2026-04-27 | 1d / 8 waves | GENUINE close |
| 8 | **B4** | 2026-04-25 | 2026-04-25 | <1d / 2 waves | GENUINE close |
| 9 | **B3** | 2026-04-25 | 2026-04-25 | <1d / 1 wave | GENUINE close |
| 10 | **B2** | 2026-04-25 | 2026-04-25 | <1d / 4 waves | GENUINE close |
| 11 | **B1** | 2026-04-22 | 2026-04-24 | 2d / 4 waves | GENUINE close |
| 12 | **B0** | 2026-04-20 | 2026-04-20 | <1d / 3 waves | GENUINE close |
| 13 | **AY-III** | 2026-04-27 | 2026-04-27 | 0d (never executed) | SUPERSEDED-DEFERRED |
| 14 | **AY-II-I** | 2026-04-27 | 2026-04-27 | 0d (never executed) | SUPERSEDED-DEFERRED |
| 15 | **AY-I** | 2026-04-21 | 2026-04-21 | <1d / 7 waves (5 missed) | terminal_with_carries (Pass-I) |
| 16 | **AX** | ~2026-04-19 | ~2026-04-22 | ~3d / 5 sub-waves | complete_with_misses |
| 17 | **AW** (-I…-V) | ~2026-04 | ~2026-04 | multi-pass | complete_with_misses (5 sub-tranches) |
| 18 | **AV** | ~2026-04-13 | ~2026-04-15 | ~2d / 11 phases | terminal_with_carries (V6-V9 → AW) |
| 19 | **AU** | ~2026-04-14 | ~2026-04-15 | ~1d / 7 waves | complete_with_misses (5 partial) |
| 20 | **AT** | (pre-2026-04-13) | 2026-04-13 | <1d | GENUINE close |

### Per-tranche entries

**AZ-IV** — *promised*: 7-wave union absorbing AZ-III carry burn-down + recycled-BA (typed `path!` + lazy parse) + recycled-BB (perf items) + TS binding + 100% test redress + 23 hard gates + 33 non-routable carries. *Landed*: every Hard Gate has resolving artefact (10 MET, 9 MET_WITH_MISSES, 4 ROUTED); permanent substrate-audit test landed; 1505 LOC bootstrap_parser DELETED via canonical generated path (W2.4). *Carries*: F2 sonic ≤ 5× (4196× actual), F5 TS Node-execute (RED gate; W1 backend-ts gap), F8 32 zero-caller substrates, F10 3 watchdog rows, plus 18/19 AU floor BELOW. *Latency*: 7 waves over 2 days, 91 commits W0→W2 mid-tranche alone. *Pattern*: **complete_with_misses with named-routed-forward**. Landed substantial; the routed carries route to a fictional "AZ-V" in 4 places of the close-state docs (per DEEP-D §III), which violates the AZ-IV thesis non-routable-carry contract.

**AZ-III** — *promised*: O5 reclose, semantic parity, fact/type/CSP/projection authority, 17-row close matrix. *Landed*: 9 waves; bootstrap_parser DELETED 1505 LOC; 95/95 BBNF parity GREEN via canonical generated path; durable FactAuthority + 2 named obligations replacing silent BoxedEnum; CSP installer trio with no-op deletion; 5 dead-code deletions for 301 LOC; 17-entry post-AZ-III.json refreshed under bench-iter (15 MEASURED + 2 WATCHDOG_HALT). *Carries*: 6 named carries to BA + BB (Sheets path-API, TS backend, regen drift, egraph cost extractor, tailwind perf, cross-profile bench refresh). *Latency*: 9 waves single session-block, ~1 day wall. *Pattern*: **terminal_with_carries** — every miss has a routed destination; no unnamed deferral.

**AZ-II** — *promised*: BBNF self-host activation + tape deletion + 4-grammar StructDirect cutover. *Landed*: cutover.A through cutover.M Phase 3 LANDED; cutover.N halted at usage limit; cutover.O.0–O4 LANDED; O5/O6/O7 routed to AZ-III. The 14 sub-stage decomposition reveals plan-time scope under-estimation of ~3×. *Carries*: O5 reclose, semantic parity, 17-entry matrix → AZ-III. *Latency*: 7 days, 14 sub-stages, multiple dispatch sessions. *Pattern*: **continuation_handoff** — closed AS continuation.

**AZ-I** — *promised*: direct-to-struct activation for JSON/CSS L4/Sheets via StructRegistry; 17-entry close matrix at AU floor; samply attribution. *Landed*: substrate landed for 3 grammars; W2-act activation flipped resolver; 17-entry matrix RAN with regressions (-55% canada, -77% BBNF self-parse). 5 audit + 2 plan agents collapsed remaining 17 declared waves → 5. *Carries*: 17-entry matrix MISSED (routed to BB.close); samply WAIVED; SIGABRT on CSS bootstrap + Sheets (pre-existing). *Latency*: 8 days, 5 waves. *Pattern*: **complete_with_misses with substrate landed but performance route-deferred**.

**B7** — *promised*: cross-repo modernization (parse-that + pprint divan + nextest + cross-repo bbnf-ser patch unification). *Landed*: 10 agents, 20 commits, 3 repo-isolated waves; bbnf-lang internal (W0), parse-that catchup (W1), pprint catchup (W2). All three repos use divan as the sole bench harness; nextest as the sole CI runner; bbnf-ser patch lives in exactly one place. *Carries*: parse-that bbnf_derive 0.2 deprecation routes to AZ-I.W0; pprint rustflags to future cross-repo refinement. *Latency*: ~15 min real wall, longest single-agent ~12 min. *Pattern*: **GENUINE close** — clean, scoped, bounded.

**B6** — *promised*: cold xtask wall (W0) + cold iter-check-full (W1) + warm iter-test (W2). *Landed*: W0 mtime-cycle fix → 192× speedup (88.26s → 0.46s); W1 + W2 closed on plan-time miscalibration (asserted baselines 38× and ~12% stale; mechanisms structurally incapable of moving prescribed metrics). *Carries*: slow-test surface partition (bench-class + fuzz tests on routine surface) → AY-III's domain. *Latency*: <1 day, 1 substantive landing + 2 vacuous closes. *Pattern*: **GENUINE_partial** — W0 substantive; W1+W2 honest plan-time-miscalibration close per SPEC. The honesty is laudable; the underlying issue is plan-time baseline staleness, not execution.

**B5** — *promised*: substrate restoration over 6 architectural smells (FusedBuilder weld, rollback triplication, columns_mut leak, extern crate self alias, 4 god modules, depth-stamp cascade). *Landed*: 8 waves (W0→W6b); FusedBuilder DELETED (~1258 LOC); single rollback primitive; god-module decomposition across 6+7 files (no file >800 LOC); single-writer invariant on frame_depth; bench non-regression vs B4 baseline. *Carries*: NONE. *Latency*: 1d / 8 waves. *Pattern*: **GENUINE close** — terminal substrate cleanup.

**B4** — *promised*: codegen syn::parse2 emit-correctness + builder.rollback_to atomic-tape unification. *Landed*: W0 SIMD-bitmap kernel labelled-break wrap fix; W1 unified ValueCheckpoint rollback path; alias retirement (TapeBuilder = FusedBuilder, ValueBuilderOutput, Parsed::new_fused 4-arg bridge — all DELETED). Workspace nextest +317 passes (1163 → 1480). *Carries*: 10 pre-existing failures (5 cursor-shape stub trip + 1 to_value + 2 pretty-directives + 1 projection_totality + 1 parse_count_invariant) → AY-II.W1. *Latency*: <1d / 2 waves. *Pattern*: **GENUINE close** with documented orthogonal pre-existing failures.

**B3** — *promised*: parser-baseline restoration. *Landed*: 5 architectural fixes: tape finaliser cycle (reverse-walk retired; in-builder bookkeeping); atomic depth rollback; end-compound bump scope widened; Pratt operand seeding corrected; lowering cousin-leak guard. Tape suite 100/100 pass; bbnf_parses_its_own_grammar 0.20s. *Carries*: B4 inherits codegen `syn::parse2` emit-correctness defect surfaced during regen. *Latency*: <1d / 1 wave. *Pattern*: **GENUINE close** — surgical restoration.

**B2** — *promised*: retire bbnf_derive proc-macro IR-pipeline; xtask regen canonical entry; per-grammar source on disk. *Landed*: 4 waves (W0–W3) + W4 close ceremony; crates/derive/ DELETED (3 files / 457 lines); 62 consumer sites across 43 files migrated; cargo iter-check warm 0.21s (under 0.5s gate); pre-B2 80-min wall on cargo expand RETIRED. *Carries*: FusedBuilder open-frames assertion class (327 failures) → B4.W1. *Latency*: <1d / 4 waves. *Pattern*: **GENUINE close** — substrate transposition lands cleanly.

**B1** — *promised*: toolchain migration (pin nightly-2026-04-11) + alias surface + bench harness divan port + 16 invariants. *Landed*: 4 waves; rust-toolchain.toml pinned; .cargo/config.toml rewritten with 4-exclude iter-check + per-exclude fast-paths; .config/nextest.toml 4 profiles; Makefile reduced 470 → 210 lines; 19 bench files + cross-crate json_value ported bencher → divan; bencher dep removed; iai-callgrind feature gate; ci.yml rewired to nextest; sibling-repo triad pinned (bbnf-lang + parse-that + pprint). *Carries*: derive cache relocation + Watt → AZ-I.W0; per-bench divan JSON → AZ-I.W0 close; samply baseline refresh → AY-II.W0' close. *Latency*: 2d / 4 waves. *Pattern*: **GENUINE close** with green-routed numeric continuation (substrate landed; numeric targets routed where the bbnf-bootstrap >600s wall blocks).

**B0** — *promised*: bounded prelude annex over AY runway; profile tiers; Makefile cleanup; named ay-* commands. *Landed*: 3 waves (W0/W1/W2); 14 commits; 3 profile tiers (ax-iter / profiling-prep / bench); idempotent profiling-prep scripts; 10 ay-* Makefile targets; cargo iter-check warm 0.16s vs baseline 7.16s = 45×. *Carries*: NONE (parse-that SaturationCache stash absorbed environmentally). *Latency*: <1d / 3 waves. *Pattern*: **GENUINE close** — bounded annex.

**AY-III** — *promised*: gestalt continuation on post-B5 substrate; admission-totality + competitor-keyed close gates + fused-pipeline wire contracts. *Landed*: NEVER EXECUTED. Per fifth /plan synthesis cycle, the substrate AY-III verifies is deprecated (direct-to-struct in AZ-I supersedes the tape lane). Durable AY-III gates absorb forward as grammar-general infrastructure: AY-III.W0+W1 → AZ-I.W4 close ceremony; AY-III.W2 BBNF self-host identity → AZ-II.W2 post-cutover regen-check. *Carries*: scope absorbed into AZ-I.W4 + AZ-II.W2. *Latency*: 0 (deferred at open). *Pattern*: **SUPERSEDED-DEFERRED** — historical record only.

**AY-II-I** — *promised*: AY Pass II — FusedBuilder substrate refinement. *Landed*: NEVER EXECUTED. Absorbed forward into B4.W1 + B5 + AZ-I.W4 + AZ-II.W2. *Pattern*: **SUPERSEDED-DEFERRED**.

**AY-I** — *promised*: visitor-lane shape; direct-to-struct admission broadening; near-parity close gates; W7 + W8 (twitter ≤ 1.15× sonic, 5-fixture geomean ≤ 1.20). *Landed*: 7 waves (W0–W6 + superseded W7); direct-to-struct admission broadened 2 → 71 grammar-derived projections; B0 annex closed; navigate_tape DEAD (0 consumers); twitter regressed 746 → 548 MB/s cumulative -27%. **Pass I close gates NOT MET** — the FINAL.md is honest about this. *Carries*: rollback-invariant violation in TapeBuilder::note_push surfaced; 4-agent audit triumvirate produced AUDIT-A/B/C/D in AY-II/audit/; AY-II owns the architectural transposition. *Latency*: <1d / 7 waves with 5 missing close gates. *Pattern*: **terminal_with_carries (Pass-I; gates not met; honest)**.

**AX** — *promised*: gate repair (W0a) + interpreter deletion (W0b) + AW-V doc rewrite (W0c) + grammar-derived view layer with byte-equality parity (W1). *Landed*: 5 sub-waves; gate repair cascade; W0b deleted ~85K LOC interpreter machinery; W1 closed as substrate-and-API closure rather than full Block B (W2-W14 parity CI gating + lever portfolio + e-graph rewriting + document-parallel). *Carries*: Block B routes wholesale into AY (the BEAT-sonic tranche). *Latency*: ~3d / 5 sub-waves. *Pattern*: **complete_with_misses with scope-reveal absorption** — explicit "AX cannot absorb without silent deferral."

**AW** (multi-pass: -I, -II, -III, -IV, -V) — *promised*: lowering pipeline migration off fn-per-rule; 19-entry parse-bench matrix. *Landed*: 28 commits across 5 waves + sub-waves; 14 parse-passing bench entries measured; 3 architectural surfaces remained un-migrated; AW-V close 0/17 (every parse entry below post-AU). *Carries*: V6 (parallel parse), V7 (SIMD keyword + PHF), V8 (bloom+GADT dedup), V9 (walker + reader migration) routed forward. *Latency*: multi-pass — at least 5 sub-tranches. *Pattern*: **complete_with_misses across 5 sub-tranches** — the AW.W4-V0 to AW-V chain is itself a sign of plan churn.

**AV** — *promised*: V0–V10 (10 waves; The Flattening); typed-materialisation completion; GrammarProfile codegen; columnar substrate; DTA synthesis. *Landed*: V0–V5 shipped substrate; 83 commits on bbnf-lang master + 13 on parse-that; V10 closes. V6 (document-parallel parse) + V7 (SIMD keyword + PHF) + V8 (bloom + GADT dedup) + V9 (walker + reader migration) routed forward to AW. *Carries*: V6-V9 → AW. *Latency*: ~2d / 11 phases (V0-V10) with 4 phases routed forward. *Pattern*: **terminal_with_carries** — explicit tranche-boundary scope cut, not silent deferral.

**AU** — *promised*: 7 waves; projection activation + CSS L4 typed-AST parity + string decode + honest JSON bench + structural bitmap + samply truth. *Landed*: 10 hard gates fully MET; 2 with documented qualifier; 5 partial; 5 missed; 1 deferred; 1 N/A. AU floor matrix established (post-AU.json 19-row defensible floor). 967/1000 pass / 33 fail / 30 ignored. *Carries*: typed-parity audit Bug 1 + Bug 2 → AV; canada 1800 + bootstrap 600 + parse_simple 200 perf gates → AV. *Latency*: ~1d / 7 waves. *Pattern*: **complete_with_misses with documented partial gates**. AU floor referenced every tranche since.

**AT** — *promised*: projection truth + regression redress + bench parity over AS audit findings. *Landed*: comprehensive plan; the AS audit critical-failure inventory (f64 payload, bool payload, KvPair, Span, StructRegistry, JSON regression AQ→AR -14%/-39%, bench non-equivalence with sonic-rs). Dispatch to next tranches. *Carries*: most failures route forward to AU for activation. *Latency*: <1d. *Pattern*: **GENUINE close** — terminal AT, AU opens against the AT findings.

## II — Chronic-Carry Tracker

The carry chain DEEP-D first surfaced; this audit extends it to the 20-tranche window with first-introduced + tranche-count + current-routing-honesty assessment.

| Carry | First introduced | Tranche count | Current routing | Real or fictional? |
|---|---|---:|---|---|
| **F2 sonic-rs ≤ 5× gap** (`bbnf_get_twitter` 4196× actual) | AY-II-I (`twitter ≥ 1967 MB/s`) | **6** (AY-II-I, AY-III, AZ-I, AZ-II, AZ-III, AZ-IV) | Recycled-BA = current BA (W4); also routed to "post-AZ-IV optimization" | **Real** at canonical-BA-W4 (post DEEP-D); **fictional** in AZ-IV/FINAL.md `audit/W6-fat-lto.txt` "AZ-V" mention |
| **AU floor regression** (18/19 BELOW post-AZ-IV) | AU close (the floor itself, 2026-04-14) | **8+** (AU, AV, AW-I-V, AX, AY-I, AZ-I, AZ-II, AZ-III, AZ-IV) | "post-AZ-IV optimization tranche" / now BA.W2+W3 per DEEP-A thesis | **Was fictional**; now real (BA-W2/W3 explicit close criterion at canonical-BA Hard Gate 5) |
| **TS Node-execute gap** | AZ-I (W2-act.close: "string-checked, not executable") | **5** (AZ-I, AZ-II, AZ-III, AZ-IV, BA) | AZ-IV's W5 RED gate routed to "post-AZ-IV TS triumvirate"; canonical BA punts to BD (TS/WASM re-engineering) | **Honest punt** at canonical-BA per user directive; fictional in AZ-IV/FINAL.md F5 routing |
| **Tailwind perf** (regex_scan timeout) | AZ-I (W2-act CSS perf cluster) | **5** (AZ-I, AZ-II, AZ-III, AZ-IV, BA→BB) | "BB rule-discovery cross-tranche" + canonical BA.W4 alternative path | **Real** at canonical BB.W2 (CSS-wide alphabet enumeration); also a real BA option |
| **32 zero-caller substrates** | AY-I + AZ-III (Babbage 3rd-pass surfaced 5+3, AZ-IV W5 enumerated 32) | **5** (AY-I, AZ-II, AZ-III, AZ-IV, BA) | Canonical BA.W0 ("18 zero-caller substrates DELETED; 3 module clusters retired"); permanent CI-gated test | **Real** — owned by canonical BA.W0 |
| **3 WATCHDOG_HALT bench rows** | AZ-III.W4 (when bench-iter profile added) | **3** (AZ-III, AZ-IV, BA) | Canonical BA.W6 ("zero watchdog rows in fat-LTO + bench-iter matrices") | **Real** at canonical BA.W6 |
| **Sheets Flat-shape lazy `#[ignore]`** (2 tests) | AZ-IV.W3 close (`715747db`) | 1 | "post-W3 follow-on" / no named owner | **Naming defect** — needs owner letter |
| **4 outlier-grammar arena/builder dedup** | AZ-IV.W5 close | 1 | "post-AZ-IV follow-on" / now BA.W2 (arena_template + builder_template DELETED entirely under direct-projection) | **Real** at canonical BA.W2 |
| **AUDIT-B routed splits** (`dta.rs`/1565, `csp_strategy/mod.rs`/1316, `css_l4/builder.rs`/1014, `passes/types/mod.rs`/786) | AZ-IV.W4+W5 | 1 | "post-AZ-IV follow-on" / canonical BC.W1 | **Real** at canonical BC.W1 |
| **Samply 7-artefact contract per claim** | AZ-IV.W6 | 1 | "post-AZ-IV measurement cohort" / canonical BC.W3 | **Real** at canonical BC.W3 |

**Pattern across the chronics** (from DEEP-D §III, refined): three carries (F2 sonic, F5 TS, AF AU floor) dominate the chronic-deferral landscape. All three were routed to a **fictional successor letter** ("AZ-V") in 4 places of the AZ-IV close-state docs (per DEEP-D synthesis). DEEP-D's Option-A re-ordering corrected this — F2 → canonical BA.W4, AF → canonical BA.W2/W3, F5 honestly punted to BD per user directive. The naming-defect carries (Sheets Flat-shape, 4 outliers, AUDIT-B splits) all gained real owners after the canonical re-ordering.

**Latency observation**: chronic-carry items have first-introduction-to-current-tranche distances of 5–8 tranches. This is the structural signature of the AZ-IV thesis violation — the union tranche absorbed BA and BB explicitly to **end** the chronic-deferral pattern, but its own residual produced 6 routed F-items that re-instantiate the pattern (F1–F12 in AZ-IV/FINAL.md). DEEP-D's Option A is the corrective; this deep assay confirms its necessity.

## III — Pattern Synthesis

Five systemic patterns the tranche history reveals:

### Pattern 1 — Letter-discontinuity-by-design

The bbnf-lang code-tranche letter sequence is **not contiguous** across the 20-tranche window:

- AT → AU → AV → AW (5 sub-tranches) → AX → AY-I (Pass I; superseded AY-II-I + AY-III deferred) → AZ-I → AZ-II → AZ-III → AZ-IV → (canonical) BA → BB → BC → BD+
- B0 → B1 → B2 → B3 → B4 → B5 → B6 → B7 ran in **parallel** with the AY/AZ series as bounded prelude annexes.

The B-letter family is genuinely "infrastructure prelude before AY/AZ work" — B0 opened 2026-04-20 (before AY-I 2026-04-21); B1–B7 opened across 2026-04-22 to 2026-04-27 in service of AY-II / AZ-I. The B-letters are **not** post-AZ tranches; they are a separate axis. Per DEEP-D §I, the user's "AZ → BA → BB → BC" canonical-ordering rule applies only to the post-AZ code-tranche letters; B0–B7 are not retroactively renamed.

The AW sub-tranche fan-out (-I, -II, -III, -IV, -V) is the only remaining letter-inflation pattern in the recent window. Each of the 5 AW sub-passes "completed_with_misses" routing carries forward; this is plan churn, not structural complexity.

### Pattern 2 — The "union tranche" overload + post-close-cohort

AZ-IV explicitly absorbed BA + BB scope (typed `path!`, lazy bail-out parse, per-grammar value-API consolidation, perf items) plus AZ-III carry burn-down + TS binding + 100% test redress + 23 hard gates + 33 non-routable carries, all in one tranche.

**Was that the right call?** The evidence is split:

**For**: AZ-IV did land an extraordinary amount of substrate. 7 waves over 2 days; 91 commits W0→W2 mid-tranche; 1505 LOC bootstrap_parser DELETED; permanent substrate-audit test landed; -1032 LOC ruler::* + RuleSet DELETE; 9-grammar regen 9/9 green. The thesis (one parse path; one IR substrate; grammar-derived semantics) holds across the entire tranche.

**Against**: AZ-IV produced the largest post-close audit cohort in project history — 6 hardening agents (Pauli/Meitner/Wegener/Mencius/Locke/Socrates) + 4 post-close (POST-CLOSE-A/B/C/D) + 6 deep audits (DEEP-A/B/C/D + DEEP-SYNTHESIS + DEEP-D-tranche-reordering) + this 7th deep audit. **Total ≥ 16 audit agents on AZ-IV alone.** This is not "more audit, more execution"; this is "the union tranche scope-revealed during execution and triggered cascading post-close audits to honestly reconcile what landed."

The chronic carries (F2 sonic 4196×, AF 18/19 AU floor BELOW, F5 TS RED) all have a **single mechanism** that closes them: **direct-projection codegen** (per DEEP-A/B/C). That mechanism did not land inside AZ-IV; it could not have, because AZ-IV's scope was already 7 waves. So AZ-IV correctly **landed substrate for the next tranche to consume**, but the AZ-IV thesis's "non-routable carry" framing was over-promised: F2/F5/AF could not close inside AZ-IV without changing the thesis, which is exactly the trigger AZ-IV §Hard Gates 23 names for triumvirate review.

**The right call would have been**: split AZ-IV into AZ-IV (carry burn-down + grammar generality + path!/lazy parse) + AZ-V (direct-projection codegen + value-API hot path + sonic floor) at plan time. The cost was **not** in execution (AZ-IV executed cleanly within its scope); the cost was in the post-close audit cohort that had to discover what AZ-IV could not absorb. This audit cost is real; it is the structural overhead of "union" framing.

### Pattern 3 — The B-prelude annex pattern is the project's most reliable shape

B0 → B1 → B2 → B3 → B4 → B5 → B6 → B7 all closed cleanly (GENUINE or GENUINE_partial), every annex bounded, every carry routed, no chronic-deferral pattern. Latency mean: ~1 day per tranche. Promise→carry ratio: ~95% of promised gates closed in-tranche.

**Why?** Three structural features:

1. **Bounded charter** — every B-tranche names exactly what it touches and what it does not (B0: "no parser-runtime work"; B1: "no parity-critical runtime architecture"; B7: "no parity-critical runtime architecture; benches/CI/test-routing only").
2. **Pre-condition explicit** — every B-tranche names its successor pre-condition (B0 → AY.W5; B1 → AY-II.W0' close ceremony; B2 → AY-II.W0' compressed; B3 → B4 codegen-emission fix; B6 → AY-III; B7 → AZ-I.W0).
3. **Hard cap on scope** — when scope-reveal happens (B6.W1+W2 plan-time miscalibration; B7 audit "16" was actually 18 bench-targets), the wave closes on rationale-satisfied rather than expanding.

The B-pattern is the project's **reliability shape**. The AY/AZ pattern is the project's **innovation shape**. The bbnf-lang fleet alternates between these two shapes; the B-pattern delivers infrastructure runway; the AY/AZ pattern delivers parity / performance / architectural transposition.

### Pattern 4 — The "scope-reveal absorption" terminal close

Six tranches in the 20-tranche window closed as `terminal_with_carries` or `complete_with_misses`: AT (genuine), AU (5 partial gates), AV (V6-V9 routed to AW), AW (5 sub-passes), AX (Block B routed to AY), AY-I (Pass-I gates not met), AZ-I (perf misses routed to BB.close), AZ-II (continuation handoff to AZ-III), AZ-III (terminal_with_carries with 6 named carries to BA+BB), AZ-IV (complete_with_misses).

**Three of these were "explicit tranche-boundary scope cuts"** — AV, AX, AY-I. Each ran into a scope-reveal at execution time and the orchestrator chose to ship the substrate while routing the activation forward. AV V0–V5 substrate landed cleanly; V6–V9 needed AW. AX gate repair + interpreter deletion + API closure landed; the optimisation arc routed wholesale to AY. AY-I direct-to-struct admission broadened to 71 surfaces; the parity gates routed to AY-II.

**The lesson**: terminal_with_carries with explicit named destination is not a failure mode. It is the project's honest scope-reveal protocol. The failure mode is **terminal_with_carries routing to a fictional letter** — which is what AZ-IV's "AZ-V" mentions did before DEEP-D corrected the plan-doc state.

### Pattern 5 — Audit-cohort discipline trends toward "more audit per tranche"

Audit-agent counts per tranche (where tracked):

- AT: 0 internal audits (the AS audit was the predecessor)
- AU: 2 (typed-parity-audit, profiling)
- AV: 0 (the V0 close-out was orchestrator-led)
- AW: 0–1 per sub-pass
- AX: 6 audits (W0a.2h SYNTHESIS-2026; pre-W2 audits W0c)
- AY-I: 4 (AUDIT-A/B/C/D triumvirate)
- AZ-I: 6 audit + 2 plan agents (W2-CLOSE-AUDIT)
- AZ-II: REAUDIT-2026-04-30 packet (R1–R8)
- AZ-III: 3 (REAUDIT R1–R8 absorbed before any wave dispatched)
- AZ-IV: **16+ audit agents** (6 hardening Pauli/Meitner/Wegener/Mencius/Locke/Socrates + 6 loss-prevention Aquinas/Lagrange/Ohm/Averroes/Banach/James + 3 mid-tranche hardening Cantor/Heisenberg/Babbage/Fermat/Diophantus/Boole + 4 POST-CLOSE A/B/C/D + 6+ DEEP A/B/C/D/SYNTHESIS/DEEP-D + this DEEPX-6/DEEPX-1–5)

**Trend**: audit count grew from 0–2 per tranche (AT–AV) to 4–6 (AX–AZ-III) to 16+ (AZ-IV).

**Is this "more audit, less execution"?** Two readings:

- **Surface reading**: yes. AZ-IV produced more close-state docs (3,800+ lines across PROGRESS, FINAL, audit/) than executable code in a single late commit window.
- **Structural reading**: AZ-IV's audit count is a **lagging indicator of scope inflation** — the post-close audit cohort exists because the union-tranche thesis under-specified what could close inside the authoring window. If AZ-IV had been split (AZ-IV + AZ-V) at plan time, each tranche would have had 4–6 audits and the cumulative would be ~12, not 16+.

**The right ratio**: 4–6 audit agents per substantive tranche (AX/AY/AZ-I/AZ-III shape), with hardening passes (3 agents max) at mid-tranche and post-close synthesis (1 agent) at terminal close. The B-pattern (1–3 agents per tranche) is sufficient for bounded annexes. AZ-IV's 16+ is a project-level outlier driven by union-tranche overload; the corrective is plan-time scope discipline, not audit-cap-reduction.

## IV — Discipline Recommendations for BA / BB / BC

Per the user's "deep plan" mandate, recommendations rooted in the 20-tranche pattern analysis:

### Recommendation 1 — Single-thesis tranche scope; reject "union" framing

**Pattern violated**: AZ-IV (union tranche absorbed BA + BB + AZ-III + TS + test redress).
**Cost paid**: 16+ post-close audit agents to reconcile what landed vs what was promised.

**Discipline for BA**: BA's thesis is **direct-projection codegen** (per DEEP-A/B/C). One thesis. The 22-hard-gate count in canonical BA.md is justified because every gate is a direct consequence of the thesis. Resist the temptation to absorb BB's rule-discovery scope (it is mechanically separate per DEEP-A/B). Resist the temptation to absorb BC's cleanup pass (it is genuine residue, not rule-discovery, not direct-projection).

**Concrete rule**: a tranche letter has exactly one thesis. If a wave is needed that does not fit the thesis, it is a sign of plan-time scope error and triggers a triumvirate review (per AZ-IV §Hard Gates 23 — but applied at plan time, not at close).

### Recommendation 2 — Non-routable carries close inside the tranche or trigger triumvirate at plan time

**Pattern violated**: AZ-IV named 33 non-routable carries; 3 routed to a fictional "AZ-V" in close-state docs; 9 routed forward to "post-AZ-IV follow-on" without owner.

**Discipline for BA**: BA's "Non-Routable Carries" table (BA.md §Non-Routable Carries) has 6 rows. Every row has a named close criterion AND a wave assignment AND a Hard Gate that closes on it. **No routing forward**. If F4 Tailwind cannot close inside BA, it triggers triumvirate review of the BA thesis at the wave that owns it (W4), not after BA's FINAL.md lands.

**Concrete rule**: any carry that cannot close inside the current tranche routes to the **next** tranche letter only via explicit, named-owner row in the next tranche's plan. No "post-LETTER follow-on" without owner.

### Recommendation 3 — Hard cap on audit-agent count per tranche; cap rises only on triumvirate trigger

**Pattern observed**: audit-agent count grew from 0–2 to 16+ across the 20-tranche window.

**Discipline for BA**: hard cap of **6 audit agents** at plan time, **+ 3 hardening agents** mid-tranche (only if scope-reveal fires), **+ 1 post-close synthesis** at terminal close. Maximum 10 audit agents per tranche. If more are needed, the tranche is structurally over-scoped and triggers triumvirate review of the thesis.

The 4 deep audits (DEEP-A/B/C/D + DEEP-SYNTHESIS) plus DEEP-D-tranche-reordering plus this DEEPX-6/DEEPX-1–5 cohort exist precisely because AZ-IV's post-close synthesis was insufficient and required iterative refinement. BA should aim to land its FINAL.md with one synthesis, not seven layers of post-close audit.

### Recommendation 4 — Generated-size budget per wave; overflow blocks until O(N) regression traced

**Pattern observed**: AZ-IV.W0 generated-LOC budget had to be MET as a hard gate at W0 close (`audit/W0-generated-size.txt` showed -2.10% vs pre-W0 from canonical-tree scaffolding contraction). This was a Babbage-pass surfaced item.

**Discipline for BA**: per DEEP-D §VIII recommendation, BA's per-wave generated-size budget is enforced as a CI gate at every wave close. Direct-projection codegen will produce significantly larger generated files (typed structs + typed enums + typed Document + typed accessor surface per grammar); the budget must be set wave-by-wave with explicit acceptable growth (e.g., W2 expects +30% generated LOC; W3 expects -10% as arena/builder templates retire).

### Recommendation 5 — Worktree provenance + cherry-pick discipline; no destructive ops

**Pattern observed**: every B-tranche (B0–B7) and every clean-close AZ tranche (B5, B7, B2) used cherry-pick of named commits from sibling worktrees, not git merge. AZ-II's 14-substage cutover decomposed into multiple sessions because of worktree isolation discipline.

**Discipline for BA**: continue the practice. BA.W0 dispatches 5 parallel sub-agents in sibling worktrees with disjoint modify-paths. Cherry-pick is the integration step. Linear history per wave. Rejected before-dispatch: any wave plan whose units overlap on modify paths (per AZ-IV.md §Orchestration Rules 2-3).

### Recommendation 6 — Triumvirate auto-trigger on JSONL quiet >15min OR first-pass no-commit

**Pattern observed**: AZ-IV fired multiple triumvirates (W0.3 lowering quartet REGEN triumvirate at `a975844b/2246a87b/redress`; W3 lazy-lane triumvirate at `c6ba1719/4d270142/2-parallel-redress`; W1 cross-cutting triumvirate at `b68d0e4d/2d270daf` after 8 residual cross-cutting failures). Each triumvirate fired auto-correctly per the SPEC §Triumvirate auto-trigger rule.

**Discipline for BA**: keep the auto-trigger. BA waves have inherent scope-reveal risk because direct-projection codegen will surface substrate gaps the AZ-IV.W5 substrate-audit test enumerates (32 zero-caller substrates carry forward; some will resist deletion because direct-projection re-purposes them). Triumvirate is the project's correct response to this risk.

### Recommendation 7 — FINAL.md cites resolving artefact for every Hard Gate, every Non-Routable Carry, every routed carry — at close, not after

**Pattern observed**: AZ-IV/FINAL.md does cite resolving artefacts for every gate. The post-close audit cohort exists because: (a) some artefacts cited generic destinations ("post-AZ-IV optimization") that were not real letters; (b) the 18/19 AU floor BELOW had no named-mechanism-that-closes-it inside AZ-IV's substrate; (c) the F5 TS Node-execute RED gate had no plan-doc owner for the post-AZ-IV TS triumvirate.

**Discipline for BA**: BA/FINAL.md's `complete_with_misses` row (if any) cites a resolving artefact AND a named owner letter (BB or BC) AND a Hard Gate in that owner that closes on the carry. No "post-BA follow-on" without specific letter assignment. **At close, not after.**

### Recommendation 8 — Promise→carry ratio > 80% closes; below triggers thesis review

**Pattern observed across the 20 tranches**:

- B-tranches: ~95% promise-close ratio (best case, B5/B7).
- AZ-III: 9/9 hard gates MET or NAMED-BLOCKER with named destination = ~100% (best AZ-series case).
- AZ-IV: 23 hard gates with 10 MET / 9 MET_WITH_MISSES / 4 ROUTED = ~43% pure-MET; ~83% if MET_WITH_MISSES counts. The 9 MET_WITH_MISSES plus 4 ROUTED is the union-tranche overload signature.
- AY-I Pass-I: 6/11 gates MISSED (parity close gates) = ~45% close ratio. This is below threshold and correctly triggered AY-II as successor pass.

**Discipline for BA**: target ≥ 80% pure-MET (no _with_misses) on Hard Gates at close. If actual close ratio drops below 80%, trigger triumvirate review of the BA thesis BEFORE closing — do not close as `complete_with_misses` and route forward. The thesis itself is wrong.

## V — Concrete BA / BB / BC Path-Forward

Per the user mandate "actual path forward":

### BA (current canonical) — Direct-Projection Codegen

7 waves; 22+ Hard Gates; 6 Non-Routable Carries; thesis: **every grammar rule's TypeDesc reaches the emitter and produces a typed Rust struct/enum at codegen time. The runtime arena/builder template registry indirection retires. The lazy parse path becomes canonical; eager is its degenerate case.**

**Discipline checks for BA at plan time**:

1. ☑ **Single thesis** (per Recommendation 1).
2. ☑ **6 named non-routable carries with wave + Hard Gate assignments** (per Recommendation 2). F2 → W4. AF → W2/W3. F8 → W0. F4 → W4 (BA close criterion) OR routes to BB (BB.W2 close criterion). F10 → W6. F5 → BD (honest punt per user directive).
3. ☑ **6 audit agents at plan time (the DEEP A/B/C/D + SYNTHESIS + DEEP-D + this DEEPX-6 are pre-BA-open; pre-BA-open audit cohort terminates here)** + 3 hardening reserved + 1 post-close synthesis = max 10 audit agents (per Recommendation 3).
4. ☑ **Generated-size budget per wave** with explicit acceptable growth; CI-enforced (per Recommendation 4).
5. ☑ **Worktree provenance + cherry-pick** per AZ-IV.md §Orchestration (per Recommendation 5).
6. ☑ **Triumvirate auto-trigger** preserved (per Recommendation 6).
7. ☑ **FINAL.md citation discipline** (per Recommendation 7).
8. **Promise→carry ratio target**: 22 Hard Gates × 80% = ≥ 18 pure-MET at close; ≤ 4 _with_misses; 0 ROUTED-without-named-owner-letter (per Recommendation 8).

### BB — Egraph Rule Inference + Ruler + VM Oracle + Ranker

Opens after BA close. 7 waves; thesis: **discover grammar-level rewrite rules rather than apply a fixed set; CVC enumeration over IrNode + e-graph fast-path equivalence + VM residue oracle + ranker tiering**.

**Discipline checks for BB at plan time**:

1. **Single thesis** preserved (per Recommendation 1).
2. **Hard opening gate**: BA close conditions (Direct-projection GREEN; rewrites/ tree clean; nextest 100% pass; regen --check 9/9; TypedPath + path! + lazy parse on 4 grammars; substrate-audit GREEN) per BB.md §BA Dependency. If any gate misses at BA close, BB does not open; carry routes back to BA per non-routable-carries discipline.
3. **Audit cap**: 4 agents at plan time (3 BB-internal + 1 cross-tranche BA→BB synthesis); 3 hardening reserved.
4. **Promise→carry ratio target**: ≥ 80% pure-MET.

### BC — Cleanup Pass + Discipline Codification

Opens after BB close. 7 waves; thesis: **absorb the residual carries from BA + BB without invalidating either's thesis: AUDIT-B routed splits, worktree fixture symlink contract, samply 7-artefact contract canonicalization, post-BA-and-BB substrate-audit residual**.

**Discipline checks for BC at plan time**:

1. **Single thesis** preserved.
2. **No new substrate** — BC is cleanup. If a new substrate is needed, the carry was misclassified; route back to BA or BB per non-routable-carries discipline.
3. **Audit cap**: 3 agents at plan time (2 BC-internal + 1 cross-tranche BB→BC synthesis); 2 hardening reserved.
4. **Promise→carry ratio target**: ≥ 90% pure-MET (cleanup tranche; high reliability expected).

### BD+ (TS / WASM re-engineering)

Opens after BC close. Per user punt directive in BA.md §TS / WASM Position. Thesis selection deferred between Option 1 (wasm-bindgen-shared), Option 2 (abi_stable), Option 3 (Custom IR-based ABI). The deep audits explicitly do not select; user requested explicit punt; honored.

## VI — Hard Gate Self-Check (this audit)

| Gate | Status |
|---|---|
| Doc exists at `docs/tranches/AZ-IV/audit/DEEPX-6-tranchehist.md` | MET |
| Doc ≤ 700 lines | MET (≈ 540 lines) |
| Last-20-tranche table covers ≥ 20 letters | MET (covers exactly 20: AT, AU, AV, AW [-I to -V], AX, AY-I, AY-II-I, AY-III, AZ-I, AZ-II, AZ-III, AZ-IV, B0–B7) |
| Per-tranche entry covers Promised + Landed + Carries + Latency + Pattern | MET |
| Chronic-carry tracker with first-introduced + tranche-count + routing-honesty | MET |
| Pattern synthesis identifies ≥ 5 systemic patterns | MET (5: letter-discontinuity-by-design, union-tranche overload, B-prelude reliability, terminal_with_carries scope-cut, audit-cohort growth trend) |
| Discipline recommendations for BA/BB/BC are ≥ 5 with concrete rules | MET (8 recommendations) |
| Concrete BA/BB/BC path-forward with discipline checks | MET |

## VII — Cross-References

- DEEP-D archaeology: `docs/tranches/AZ-IV/audit/DEEP-D-tranche-reordering.md`
- Post-close synthesis: `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md`
- AZ-IV close: `docs/tranches/AZ-IV/FINAL.md`
- AZ-IV plan: `docs/tranches/AZ-IV/AZ-IV.md`
- Project synthesis: `docs/GESTALT.md`
- Canonical BA: `docs/tranches/BA/BA.md`
- Canonical BB: `docs/tranches/BB/BB.md`
- Canonical BC: `docs/tranches/BC/BC.md`
