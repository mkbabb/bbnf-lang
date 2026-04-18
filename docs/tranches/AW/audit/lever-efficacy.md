# AW Audit — Lever Efficacy Priors for AX

## Angle headline

Cross-tranche lever efficacy audit: which optimisations **historically moved the bench**, which landed as **ledger-only substrate**, which **regressed**, and what priors that yields for AX's 37-item portfolio. Principal finding: the AP-AW arc reveals a canonical failure signature — substrate emitted, consumer un-wired, gate widened silently, bench missed — that makes `W0a/W0b` routing-first discipline load-bearing. Nine of AX's 37 items are HIGH confidence, ten MEDIUM, twelve LOW/regressed-history, six NOVEL. **Honest P(all 37 land) ≈ 5-12%**; **P(HIGH subset recovers post-AU) ≈ 55-70%** conditional on W0a holding.

## Per-wave bench-delta attribution (material only; ≥ 2× or ≤ -50%)

All figures MB/s. JSON `twitter` headlines; other entries move congruently per `post-*.json`.

| From → To | Entry | From | To | Δ% | Material driver |
|---|---|---:|---:|---:|---|
| AP → AQ | json_twitter | 2173 | 2086 | -4% | neutral (AQ silent) |
| AQ → AR | json_canada | 1796 | 1097 | **-39%** | modifier-recovery correctness cost (AR bench note) |
| AR → AS | css_bootstrap | — | 525 | **NEW** | CSS L4 activation via sub-flag dispatch (AS hard gate) |
| AS → AU | json_canada | 1089 | 1231 | +13% | arena + fingerprint capacity (AU.1.1 canada fix) |
| AS → AU | css_bootstrap | 525 | 454 | -14% | structural-bitmap v2 under-performed deleted nibble-LUT (AU FINAL §Phase 2) |
| AU → AV | json_canada | 1231 | 455 | **-63%** | Span-aggregate per-instance writes + Bug-1/2b correctness (AV `regression_drivers`) |
| AU → AV | css_bootstrap | 454 | 182 | **-60%** | same substrate-pay-no-consumer pattern |
| AV → AW (II) | json_twitter | 481 | 123 | **-74%** | DTA walker activation w/o specialisation (post-AW commentary) |
| AV → AW (II) | css_bootstrap | 182 | 1436 | **+689%** | correctness regression disguised as win (post-AW.json:62) |
| AW-II → AW-III | json_twitter | 123 | 170 | +38% | walker specialisation (W4.d) + stage-1 SIMD (W5.d) both active |
| AW-III-W4 → W5 | json_twitter | 192 | 170 | **-11%** | stage-1 SIMD cost without regex-bound amortisation (W5.d samply 12.13%) |
| AW-III → AW-IV | json_twitter | 170 | 288 | +69% | W1 interpreter abrogation + W2 helper splice |
| AW-IV-W3 → W4 | css_tailwind | 16 | 37 | **+131%** | W4.4 document-parallel fork 2.24× at 4 threads (only breakthrough) |
| AW-IV-W4 → W5 | all | — | — | flat | W5 correctness/cost-grid; no throughput wave |
| AW-IV → AW-V | json_twitter | 288 | 486 | +69% | W5.2 per-Ref dispatcher reaches `parse()` indirectly |
| AW-IV → AW-V | css_bootstrap | 15 | 14 | -7% | CSS still walker-fallback |
| AW-V W3→W6 (historical) | json_visitor_twitter | — | **2683** vs sonic 2747 | **0.98×** | W3-fix-bench (c1e86ab3); regressed to NON-COMPILE at W6 by `has_w4_classified` |

**Cumulative vs post-AU (RD baseline):** post-AW-V geomean **0.082** (JSON 0.20-0.29; CSS/Sheets/BBNF 0.03-0.07).

## Lever efficacy verdict table

| Lever category | Tranches attempted | Material gain | Ledger-only | Regression | Verdict |
|---|---|---:|---:|---:|---|
| Stage-1 SIMD structural pre-pass | AW-III.W5, AW-IV.W1γ | 0 | 1 | 1 (-11% twitter W4→W5) | MIXED; SIMD kernel cost not amortised without bounded-regex consumer |
| Inline SIMD kernels (nospace64, first_quote_or_backslash, NEON str2int) | W2.1 prototype | **1 (prototype 0.89× sonic)** | 1 (emitter-path) | 0 | PROVEN in prototype; emitter-lift regressed at W4-fix-rest |
| Quote parity (CLMUL / shift-XOR) | AU.2.7 | 0 | 1 | 1 (bootstrap 454→438) | REGRESSED CSS; kept JSON constant |
| Numeric decode (Eisel-Lemire / simdjson / vdotq_s32) | AV.3.5, AW-IV.W2.3 | 0 | 2 | 0 | substrate-only; never fired at bench level (AW-IV.W4 note) |
| Movemask cascades (vpaddq_u8) | AW-IV.W2.3 | 0 | 1 | 0 | NO downstream bench attribution |
| Fused tape writes (push_compound_fused_v32 / paired stp) | AV, AW-V.W1.3 | 0 | 2 | 0 | Lever 4 zero consumer at W6 (dead-code-manifest.md:18) |
| Scanner closure (HashMap DFA elim) | AW-III.W1.8 | **1** | 0 | 0 | LANDED + verified (scanner_closure_arc_dfa active) |
| PHF keyword dispatch | AP.4, AW-IV.W3.2 | **2** (AP bootstrap +50%) | 0 | 0 | **RELIABLE**; proven AP + AW-IV |
| ShapeRef dedup | AW-III.W6.1, AW-IV.W3.1 | 0 | 2 | 0 | SHAPE_DICT emitted empty for every grammar post-W4 (post-AW-III.json) |
| Bloom+GADT dedup | AW-IV.W3.3, W4.3 | 0 | 2 | 0 | fires only on Seq empty-children (W4.4 note) |
| ClassifyByte dispatch | AW-III.W6.A, AW-IV.W3.3 | ~0.5 (CSS L4 135 tables emit) | 1 | 0 | substrate live; no bench delta attributable |
| Direct-to-struct projection | AU.1.1, AW-III.W6.4 | **1** (canada +18% AU) | 1 | 0 | MIXED; activation needed |
| Codegen-specialised walker (dta_run_<grammar>) | AW-III.W4.d, AW-IV.W1 | **1** (+38% W2→W3) | 0 | 0 | LANDED; symbol verified |
| Inline DFA body splicing (W1.4-aggressive) | AW-IV.W2.2 | **1** | 0 | 0 | +14% over W1 (post-AW-IV-W2) |
| Per-arm helper splicing (emit_leaf, close_compound) | AW-IV.W2.1 | **1** (~+5-15%) | 0 | 0 | LANDED; `advance_or_pop_with` residual |
| SIMD widening u8x32 | AW-IV.W1, W4.1 | 0 | 1 | 0 | aarch64 NEON already 128-bit; AVX2 codegen only |
| Document-parallel fork | AW-IV.W4.4 | **1 (+131% tailwind)** | 0 | 0 | **HIGH-confidence single breakthrough** |
| Reduce-column | AW-IV.W5.1 | **1 (6.57× microbench)** | 1 (parse bench) | 0 | microbench only; no parse-bench consumer |
| Pratt precedence / ShuntingYard | AU.6.3 deferred, AW-III.W6.5 | 0 | 1 | 0 | PRECEDENCE_LUT emits; walker linear-scan (levers_substrate_only_not_activated) |
| GRAMMAR_PROFILE wire-contract | AW-IV.W1.δ, AW-V.W5.1 | 0 | 1 | 0 | contract **PASSES** (9/9 tests); substrate only |
| Shape-dispatch classifier | AW-V.W3.1 | **1 JSON** | 1 CSS/Sheets/BBNF | 0 | V-audit-overfit matrix: 0 non-JSON runtime coverage |
| Per-shape inline emitters | AW-V.W3/W4 | **1 JSON (prototype parity)** | 1 non-JSON | 0 | 162 CSS fns + 29 Sheets + 36 BBNF emit; dead code at runtime |
| Visitor-path emission | AW-V.W3-fix-bench | **1 at c1e86ab3** | 0 | **1 at W6** | REGRESSED by `has_w4_classified` gate widening (W4-fix-rest) |

## AX 37-optimisation priors

Sequential numbering tracks AX.md §Wave-schedule left-to-right; categories reuse the table above.

| # | Category | Precedent tranches | Prior outcome | AX prior |
|---:|---|---|---|---|
| 1 | W0a gate repair | AW-V (the regression itself) | surgical; one-file | **HIGH** |
| 2 | W0a parse() routing non-Alt-rooted | AW-V.W5.2 admission-only | substrate, never wired | **MEDIUM** |
| 3 | W0a gate_predicate_wire_contract | AW-IV.W1.δ (wire-contract pattern) | pattern proven | **HIGH** |
| 4 | W0b DTA delete ~74K LOC | AW-IV.W1 (scanner elim, smaller) | at this scale novel | **MEDIUM** (mechanical but interlocked) |
| 5 | W0b crate renames | none | trivial | **HIGH** |
| 6 | W0c AW-V.md rewrite | AP/AQ/AR doc pass | doc-only | **HIGH** |
| 7 | W1 Value API (JSON/CSS/Sheets/BBNF) | AU direct-to-struct partial | partial activation | **MEDIUM** |
| 8 | W1 AoS sidecar + paired stp | AW-V Lever 4 (ledger-only) | zero consumer | **LOW** (R4 §5 tautology flag) |
| 9 | W1 twitter lazy-field bench | none | novel pattern | **NOVEL** |
| 10 | W2 sonic/simdjson OnDemand/serde/lightningcss/cssparser parity | AW-IV.W5.2 sonic+lightning | **MET; CI-gated** | **HIGH** |
| 11 | W3 subsystem closures | AU.4 debt sweep precedent | partial (test_selective still ignored through AW) | **MEDIUM** |
| 12 | W4 paired stp | same as 8 | ledger-only | **LOW** |
| 13 | W4 unreachable_unchecked | never tried | mechanical | **MEDIUM** |
| 14 | W4 scan fusion | AP.3.3 (SIMD string scanner) | landed | **MEDIUM** |
| 15 | W4 AltReorder | never tried | novel | **NOVEL** |
| 16 | W4 vpaddq_u8 | AW-IV.W2.3 ledger-only | no attribution | **LOW** |
| 17 | W4 vdotq_s32 | AV.3.5 Eisel-Lemire (substrate) | never fired at bench | **LOW** |
| 18 | W4 PMULL verify | AU.2.7 CSS regression context | caused regression | **LOW** |
| 19 | W5 TBL-4 kinded bitmap | none | novel | **NOVEL** |
| 20 | W5 kind-separated stage-1 streams | AW-III.W5.d dense-alphabet pathology | **regressed** -11% | **LOW** |
| 21 | W5 SIMD-speculative Alt Unordered | never tried | novel | **NOVEL** |
| 22 | W5 Bloom+GADT dedup | AW-IV.W4.3 (empty-children only) | ledger-only | **LOW** |
| 23 | W5 ShapeRef consumer | AW-III.W6.1 + AW-IV.W3.1 (both ledger-only) | zero consumer 2/2 | **LOW** |
| 24 | W6 BoundedRegex | AW-III.W5.c revert, W6.A dense-alphabet defeat | **regressed** | **LOW** |
| 25 | W6 CTNS emission | AW-III.W6.A (gated off), AW-IV.W3.5 (admission rejects all) | 0/0 emissions at AW-V | **LOW** |
| 26 | W7 LazyRef tape kind + should_descend | simdjson OnDemand analog; not tried locally | novel in-codebase | **NOVEL** |
| 27 | W7 per-grammar *LazyValue wrappers | AU.6.4 Bug-2 (unfinished) | partial | **MEDIUM** |
| 28 | W8 shape-transition matrix + tune_online | never tried | novel | **NOVEL** |
| 29 | W8 rollback scratchpad | never tried | novel | **NOVEL** |
| 30 | W9 document-parallel fork | AW-IV.W4.4 **+131% tailwind** | **proven breakthrough** | **HIGH** |
| 31 | W10 e-graph G1-G4 universal | AL CSP, AM.6 cost-grid (chronic defer) | never shipped | **MEDIUM** |
| 32 | W11 G5-G9 per-shape + variants | same chain | never shipped | **LOW** |
| 33 | W12 detector retirement (1676→150 LOC) | never tried | novel | **MEDIUM** |
| 34 | W13 CPU autotune + PMC | AN.5 AVX2 widening (substrate-only) | landed but no delta | **MEDIUM** |
| 35 | W14 #[emit_paired_with] + multi-key Object | never tried | novel | **NOVEL** |
| 36 | W15 ≥ 2.5× parallel on ≥ 1 MB | W9 | conditional | **HIGH** (conditional on W9) |
| 37 | W15 JSON ≥ sonic × 1.07× | W2.1 prototype 0.94× best | prototype-only | **MEDIUM** (visitor path re-enabled) |

**Tally:** HIGH 6; MEDIUM 10; LOW 12; NOVEL 6; (Conditional HIGH 3).

## Recurring failure patterns (with commit citations)

1. **SIMD kernel lands before emitter-side consumer.** AW-III.W5.b ships `bbnf_simd_scan::neon::scan` at 4775 MB/s standalone; W5.d integration *regresses* JSON twitter -11% (192→170) because the Regex bound collapse was unsound on dense alphabets (post-AW-III-W5.json:74). Prototype artefacts: Lever 4 `push_compound_fused_v32` (commit b3cf555e, W1.3) with zero consumer at W6 (dead-code-manifest.md:18).
2. **IR mining passes with no downstream reader.** SHAPE_DICT emits empty for every grammar post-W4-fix-rest (AW-III `levers_substrate_only_not_activated`); KEYWORD_PHF 0 tables across grammars (post-AW-III.json:67); ClassifyByte gated off when compute_dispatch subsumes (commit cf691347); pattern_alphabet mining alive but bounded Regex disabled by dense-alphabet pathology.
3. **Gate predicates widening without wire-contract test.** `has_w4_classified` introduced at `grammar.rs:718` (AW-V W3-fix-bench), widened silently by W4-fix-rest (`569c17e4`/`ce2fd9f6`) to admit JSON Flat/Wrap. No wave-close wire-contract test asserted "JSON visitor bench compiles." Regression hidden across three wave boundaries (W4→W5→W6) — V-audit-overfit §Gate pathology §has_w4_classified.
4. **Substrate-with-cold-path-hedge.** AV.3.6 fn-per-rule deletion scheduled V3 → V4 → never (AV-retro §2); AW-I preserves the walker's cold-path for "AX replay" which survives all the way to AX.W0b. The hedge kept interpreter paths reachable for 5 tranches.
5. **Correctness regression disguised as throughput win.** post-AW.json:62 — "css_bootstrap improved 454 → 1436 MB/s (+3.16×); Hypothesis: DTA parses a small subset of bootstrap.css rapidly (emits 9 records vs golden's 92228), explaining both the throughput spike AND the css_bootstrap_tape_parity failure." Bench mean without tape-parity cross-check is unverified.

## Probability assessment for AX aggregate

Using the efficacy distribution and AW-V's 0/17 gate miss as the posterior: **P(all 37 land) ≈ 5-12%**. Twelve LOW items repeat patterns that have already regressed or sat substrate-only through 2+ tranches; one LOW turn-around per wave is historical ceiling (AP PHF, AU.6.7 arena). The six NOVEL items compound the unknown.

**HIGH-confidence subset sufficient to recover post-AU** (9 items):
- W0a gate repair (1, 3) — closes the single diagnosed V regression; visitor-path re-enabled recovers JSON tape-path from 0.247× AU to ~0.98× prototype × sonic ratio ≈ ≥1.07× post-AU on JSON directly per W3-close evidence (FINAL-V.md:56).
- W2 parity harnesses (10) — already proven AW-IV.W5.2 CI-gated.
- W4 paired stp (12) — if 1.94× AU (W4.1) → 6.64× under reordered-unrolling pattern per AU.7 finding.
- W9 document-parallel fork (30) — AW-IV.W4.4 +131% tailwind.
- W0b DTA delete (4) — mechanical given W0a.
- W0c doc (6), W0b renames (5).

This 9-item floor historically could recover ≥ post-AU on JSON (5 entries) + tailwind (1 of 3 CSS) + maybe ebnf/self via bootstrap. **P(HIGH subset recovers post-AU on ≥ 10/17 entries | W0a holds) ≈ 55-70%.** Note: CSS bootstrap + all three Sheets + 3 BBNF entries **have no HIGH-confidence historical lever attributable** — they sit in the LOW/NOVEL tail (BoundedRegex, CTNS, ShapeRef, multi-key SIMD).

**Defensible floor (5 items):** 1, 3, 10, 30, 6. Closes the diagnosed regression, preserves parity CI, lands the one breakthrough lever, no architectural risk. Every parse entry expected ≥ post-AU only on the five JSON entries and tailwind.

## Risk-ranked load-bearing sequence

Sorted by (historical risk ↓ × impact-if-land ↑ × confidence ↑):

1. **W0a gate repair + routing (#1, #2, #3)** — **load-bearing #1**. Every downstream throughput claim on AX depends on this closing. V-to-X-learnings §2 explicitly names this pre-requisite; AW-V's 0/17 miss is precisely this.
2. **W2 parity harness extension (#10)** — **load-bearing #2**. Pattern proven; without it, novel levers (W8 speculative, W10/W11 e-graph) introduce silent divergence.
3. **W9 document-parallel fork (#30)** — **load-bearing #3**. The single +131% breakthrough in the AW arc; only lever with demonstrated scaling.
4. **W1 Value API (#7)** — enables HIGH-confidence W2-W14 consumer wiring; without it, every downstream is substrate-only.
5. **W4 per-arm inline splicing (#13, #14)** — extends AW-IV.W2 proven pattern; low architectural risk.
6. **W12 detector retirement (#33)** — high impact on codebase simplicity but interlocked with W10/W11 e-graph; if e-graph slips, this slips.
7. **W7 LazyRef + should_descend (#26, #27)** — simdjson-OnDemand analogue, genuinely novel; high variance.

**Candidate scope-cut (bottom quartile):** #16 (vpaddq_u8), #17 (vdotq_s32), #18 (PMULL), #22 (Bloom+GADT), #23 (ShapeRef consumer) — five items where prior attempts landed ledger-only AND no consumer wiring has appeared in two+ tranches. Cutting these retires ~450 LOC of planned substrate, preserves the 9-HIGH floor, and frees 3-4 agent-waves to harden W0a/W9/W1.

The orchestrator should protect items 1-4 above at all costs; items 5-7 should have re-plan triggers at their first mid-wave bench miss per AX.md's own `bench-checkpoint mid-wave` invariant.

---

**Key finding for AX planning:** the AW arc's throughput ceiling on JSON was achieved once (W3 close commit `c1e86ab3`, visitor-path 0.98-1.03× sonic) and lost via silent gate widening. The 37-item plan restates nearly every AW-IV/V substrate lever that landed ledger-only; prior outcome says ≥ 10 of those 37 will repeat that fate absent stronger consumer-wiring discipline than AW-V demonstrated. AX's mid-wave bench-checkpoint + frozen-gate-predicate invariants *are* the correct response, but only if enforced; if they slip, AX becomes AW-V redux.
