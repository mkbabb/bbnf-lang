# Pre-AX Deep Tranche Analysis — Last 10 Slowdown Census

## Angle headline

Ten tranches (AP through AW-V) closed between 2026-04-12 and 2026-04-18. AP-AU constitute the pre-DTA RD era; AV seeded the DTA substrate; AW-I/II/III/IV/V executed the DTA activation + reckoning. Throughput traced: post-AP 2173 MB/s (twitter) → post-AU 1967 MB/s → post-AW-V 486 MB/s. The census confirms two dominant failure modes: **substrate-without-consumer landings** and **gate-off / predicate-widening correctness regressions that hide behind architectural narrative**. Pratt lowering is NOT the Sheets bottleneck — walker fallback + small-input amortisation is.

## Per-tranche bench-delta table (MB/s; BLOCKED means parse failure)

| Tranche | json twitter | css bootstrap | sheets parse_stress | bbnf_self | Headline |
|---|---:|---:|---:|---:|---|
| post-AP | 2173 | 505 | n/a (pre-sheets) | n/a | **Materially IMPROVED** — +41% twitter, +50% bootstrap, tailwind fixed from FAIL |
| post-AQ | 2086 | 500 | n/a | n/a | **FLAT** — within noise (−4% twitter); Wave-4 CSS dispatch hash reverted |
| post-AR | 2069 | BROKEN | n/a | n/a | **CSS REGRESSED**; canada 1797→1097 from modifier-recovery fix hiding real cost of f64 payload writes |
| post-AS | 2003 | 525 | n/a | n/a | CSS reactivated (scan_ident sub-flag); mostly flat on JSON |
| post-AU | 1967 | 454 | 121 | 394 | **HIGH-WATER MARK** (pre-DTA). canada 1796→1231 from f64 payload activation; gates 21-24 all MISSED (1800/600/200/550 MB/s all missed) |
| post-AV | 481 | 182 | 28 | 106 | **CATASTROPHIC REGRESSION** — 2.5-4.5× across every entry. Substrate-only V5 close; V6-V9 route to AW |
| post-AW (I+II) | 123 | 1436* | 3 | 14 | **Universal −91% regression**; *bootstrap spike is PARSE-FAILURE artefact (9 records vs 92228 golden) |
| post-AW-III | 170 | 8 | 3 | 12 | W4 dispatch_one removal (0% self-time); W5 SIMD-scan active at 12.13%; 0/17 exceed post-AU |
| post-AW-IV | 288 | 15 | 6 | 20 | +83% vs AW-III; **tailwind +311% (9→37) via W4.4 parallel fork**. Still 0/17 exceed post-AU |
| post-AW-V | 486 | 14 | 6 | 22 | JSON tape +65% vs AW-IV. **Prototype beats sonic 0.89-0.94× — but emitter bench DOES NOT COMPILE** due to has_w4_classified gate |

## Named slowdown causes per tranche

- **AP.1 structural dispatch gated off** — `structural_mode=false` (generate/mod.rs:61); pre-scan cost dominated. AQ.5 deleted ~400 LOC. `AP-retro.md:12-17`.
- **AQ Wave 4 reverted** — CSS regression on `var(--x)`; key dispatch hash backed out (commit 64a2cf9). `post-AQ.json:42`.
- **AR canada regression not AR's fault** — post-AQ parser silently skipped modifiers; "those numbers were from a broken parser" (post-AR.json:30).
- **AT Phase-1 grep-gate passed, runtime dead** — `branch_pushes_children` mis-classified; "every typed payload capture was a dead store until AU.1.1". `AT-retro.md:22-25`.
- **AU canada 1796→1231** — real cost of storing 111K f64 payloads on previously-discarded path (`post-AU.json:71` gate_21_missed).
- **AV substrate-only regression** — "AV closed below the AU baseline on every bench. This is expected and documented" (post-AV.json:47). Five named drivers: Span per-instance aggregate, Bug-1 hoisted payload-write, Bug-2b span-helpers, scalar-Alt admission, empty-compound sentinel. V6-V9 routed to AW.
- **AW-I/II correctness regressions** — 13 Category-A parse failures; bootstrap 454→1436 MB/s called out as "correctness regression disguised as a throughput win — not a genuine optimisation landing" (post-AW.json:62).
- **AW-III universal gate miss** — "0 of 17 parse entries strictly better than post-AU… consumer activation… is AW-IV W1 scope" (post-AW-III.json:12). SHAPE_DICT empty; PHF 0 tables emitted; ClassifyByte 0 tables; CTNS lifter gated off.
- **AW-IV W3 substrate-without-consumer** — Pratt LUT emitted but `lookup_precedence` still linked; ShapeRef record-count drop requires `SeqPromote::ShapeRef` out-of-bounds (FINAL-IV.md:110-113).
- **AW-V has_w4_classified gate miscalibration** — W4 detector widening classified JSON's `pair/value` rules as Flat/Wrap; gate at `emitter/grammar.rs:718` now blocks JSON's visitor-path that matched the prototype at W3 close (post-AW-V.json:51-54).

## Lever efficacy — three-bucket sort

**(a) Materially moved throughput (≥10% verified):**
- **AP.0.2 @ws kernel universality** — unblocked CSS L4 tailwind parse (FAIL→534 MB/s). Commits in tranche range.
- **AP.3.1/3.3 SIMD WS bitmap + string scanner** — JSON citm +44.7%, twitter +41.1%.
- **AP.4 CSS L4 key dispatch for __declaration** — bootstrap +50%, normalize +57%.
- **AS.1 CSS L4 scan_ident sub-flag dispatch** (post-AS-phase1.json) — bootstrap 0→513 MB/s.
- **AU.1.1 (83357e4) branch_pushes_children fix** — activated typed payload captures that AT had emitted as dead stores.
- **AW-IV.W4.4 parallel fork** (tailwind 16→37, +131%; 2.24× 4-core scaling) — ONLY throughput-moving lever in AW-IV; W4.4-fix restored bootstrap parity with depth-0 partitioning.
- **AW-V.W2.1 JSON prototype** — hand-tuned, beats sonic 0.89-0.94× (5/5). Proven viable.

**(b) Ledger-only (substrate landed, consumer never activated OR flat):**
- **AP.1/1b structural dispatch** — gated off, AQ.5 deleted (~400 LOC).
- **AQ.6 typed-payload end-to-end source** — "zero payload writes in six production grammars" (SYNTHESIS.md:44).
- **AS.2.3 StructRegistry** — scaffold with no populator; deleted AU.4.2.
- **AT Phase 1** — grep-gate passed, runtime dead until AU.
- **AV.0.5 Color admission** — layout pass never admits TypeDesc::Named("Color"); inert through V5.
- **AV V0-V5** — DTA_TABLE + GRAMMAR_PROFILE + SHAPE_DICT + PayloadStream + ShapeRef cursor ALL emitted; runtime driver deferred to AW. Entire 83-commit tranche substrate-only.
- **AW-III.W6** — SHAPE_DICT emits empty across grammars; 0 PHF tables; 0 ClassifyByte tables; CTNS gated off; bounded-Regex defeated by dense alphabets; Pratt LUT emitted but walker's ShuntingYard arm still uses linear scan (post-AW-III.json:65-73).
- **AW-IV.W3** — 4 of 5 sub-waves landed substrate without consumer (ShapeRef, Pratt LUT, CTNS, bounded Regex; only PHF + ClassifyByte in walker fired).
- **AW-IV.W1 advance_or_pop_with** — kept as cold call; binding-rule revision arrived after W2.1 commit; "load-bearing residual" (FINAL-IV.md:108).
- **AV.2.5 reordered-unrolling kernels** — `Tape::reduce_column` API never written in AV; landed AW-IV.W5.1 as 6.57× microbench-only.
- **AW-V.W4/W5 per-Ref dispatcher + per-shape emitters for CSS/Sheets/BBNF** — substrate emits for every grammar; parse() still routes through walker.

**(c) Regressed / reverted / gated off:**
- **AQ Wave 4 key-dispatch hash + CSS L4 routing** — reverted (64a2cf9) on var(--x) regression.
- **AW.0.10 fuse/inline activation guard-drop** — reverted; pipeline consumer fix deferred to W1.
- **AW-V.W2.3 novel-exceed levers** — "retired" (FINAL-V.md:43) because W2.1 met gate without them.
- **AV.3.6 fn-per-rule deletion** — V3→V4-close→never; routed to AW.1.3.

## Recurring patterns distinguishing moved-vs-ledger

1. **Substrate-without-consumer (#1 chronic; SYNTHESIS.md:27)** — every ledger-only lever above. Consumer activation must land same-wave with runtime-firing evidence (samply, `nm`, wire-contract). AK/AM/AO/AP.1/AQ6/AS.2.3/AT.1/AV.0.5/AV.2.5/AW-III.W6/AW-IV.W3 all same shape.
2. **Gate-off commits** — AP.1 structural_mode=false; AW-V has_w4_classified. Both hide dead emitted code behind a predicate flipped false by default.
3. **Hard-gate-via-grep** — AT Phase-1 passed source-pattern match while `driver/alt.rs::branch_pushes_children` mis-classified at runtime. Critical-files tables missed the file.
4. **Predicate widening without wire-contract** — AW-V W4 detector widening (Flat/Wrap) classified JSON rules, tripping the has_w4_classified gate; the W3-working bench broke. No wire-contract test asserted "for every grammar, gate output X".
5. **Bench omission masking regression** — AV V10 first bench revealed 2.5-4.5× universal regression. AN/AR also shipped functional-gate-passing -39% canada / -20% data_xl regressions without intra-tranche bench.
6. **Interpreter dispatch moved, not removed** — AW-III.W4.d inlined `dispatch_one` to 0% self-time but redistributed 33% into `__dta_walker_inline::run` + 37% scanner. Eliminating a dispatcher at source level doesn't eliminate the dispatch cost when the state machine still interprets.
7. **Dense-alphabet pathology** — CSS L4 structural alphabet is [0..127]; SIMD index produces ~one entry per byte; amortisation defeated. Bounded-Regex disabled because pattern alphabets themselves dense.
8. **Small-input fixed-cost amortisation** — Sheets inputs 505 B–1.8 KB; stage-1 SIMD + walker specialisation fixed-cost never break even. NOT a Pratt problem (see §6).
9. **Silent deferrals disguised as completion** — AP ~10 sub-phases; AN/AO/AP/AQ/AR/AS all silent-deferred items; SYNTHESIS.md:60 calls this the #2 anti-pattern.
10. **Architectural-narrative throughput-promise gap** — AW-III/IV/V FINALs repeatedly invoke "architectural transposition complete; throughput translation requires next wave." 400 commits, five FINAL-N.md docs, geomean 0.082× post-AU.

## Pratt / Sheets trajectory — the user's thesis verified-and-refuted

Pratt detection AND emission landed, but are NOT the Sheets bottleneck. Trajectory:

- **AU.6.3** — Pratt precedence-tower flattening planned, NOT shipped; identified as "AV scope" (post-AU.json:74). Sheets parse_simple 95 MB/s pre-AV.
- **AV.3** — DTA shunting-yard precedence table emitted (8 tests pass); runtime driver deferred to AW (post-AV.json:34).
- **AW-III.W6.5** — PRECEDENCE_LUT + PRECEDENCE_ENTRIES emitted; walker's ShuntingYard arm still uses `lookup_precedence` linear scan (post-AW-III.json:73). test_let_parses_as_let_call un-ignored.
- **AW-IV.W3.4** — inlined byte-load LUT in walker SY arm; residual `lookup_precedence` in `advance_or_pop_with` out of file bounds (FINAL-IV.md:44, 112).
- **AW-IV.W4-fix-pratt** — detector widened for Seq/Next/Skip/Map/OW wrappers; 7 Sheets Pratt classifications.

Sheets trajectory (parse_simple MB/s): AU 95 → AV 21 → AW 4 → AW-III 4 → AW-IV 6 → AW-V 6. The 95→21 regression is DTA walker activation. 6 MB/s is the walker fallback plus **small-input amortisation** (505 B input; stage-1 SIMD fixed-cost never breaks even — post-AW-III.json:220, documented as plan-time small-input tradeoff). Pratt-precedence flattening did land. **The Sheets bottleneck is walker fallback + per-byte fixed-cost overhead on sub-KB inputs, NOT missing precedence flattening.** AX W0a narrowing `has_w4_classified` and routing `parse()` through shape dispatcher targets this directly.

## Never-tried items in AX (no historical precedent in AP-AW)

Cross-checking the 37 optimizations in AX against AP-AW:

**Novel (no prior attempt):**
- Cranelift JIT substrate (`crates/jit/`) — deferred to AY per AX scope.
- `bbnf::{json,css,sheets,bbnf}::Value` Value API with structural parity vs sonic-rs/lightningcss — AW-V prototype did JSON only; CSS/Sheets/BBNF Value APIs new.
- `LazyRef` tape kind + `should_descend` visitor hook + `*LazyValue` wrappers — simdjson OnDemand-style gradient decode; no tranche attempted.
- Multi-visitor `#[emit_paired_with]` macro — new.
- CPU autotune + PMC counters (W13) — new.
- Ice Lake / Graviton 3 per-CPU variants — new (aarch64-apple-darwin only in all prior tranches).
- Multi-key SIMD Object-shape ≥ 16-keys — new.
- `has_w4_classified` narrowing to `Pratt | Unordered` — W0a only.
- `gate_predicate_wire_contract.rs` (per-gate per-grammar wire-contract) — new mechanism.

**Has prior-attempt track record:**
- PHF keyword dispatch — AW-III.W6.2 emitted 0 tables; AW-IV.W3.2 dropped threshold to 3, got 30 CSS/4 BBNF/2 Sheets/1 JSON.
- Bloom+GADT dedup — AW-IV.W4.3 landed Seq empty-children only; non-empty path deferred.
- ShapeRef runtime dispatch — AW-III/IV landed substrate; AW-IV.W3.1 emitted 1044 BBNF sites but record-count drop deferred.
- E-graph universal + per-shape rewrites — AM.6 cost-model sweep chronic; AW-IV.W5.3 null-result.
- Document-parallel fork — AW-IV.W4.4 landed tailwind +131%; AX.W9 extends.
- CTNS lifter — AW-III.W6.A substrate; admission rejected all production patterns under strict admission.
- BoundedRegex — AW-III defeated by dense alphabets.
- AltReorder, scan-fusion, `unreachable_unchecked` — primitives present but not systematically deployed.

## Recommended AX guardrail additions (evidence-backed)

AX.md has 12 invariants + 4 operational items. Given the census, add:

13. **Ledger-only wave = re-plan trigger.** If wave-close ledger cannot cite runtime evidence (samply self-time ≥ 1%, `nm` symbol absence, wire-contract end-to-end) for every substrate landing, the wave reopens with additional agents. AW-III and AW-IV each had multiple substrate-only waves that shipped on scaffold-exists gates.

14. **Gate-predicate symmetry invariant.** Every predicate that disables emission (has_w4_classified, has_shape_dispatcher_entrypoint, has_full_shape_coverage, structural_mode, branch_pushes_children) carries a per-grammar wire-contract test that asserts the predicate's output BEFORE and AFTER every downstream wave. AW-V.W4 broke the W3-working state by widening a detector that flipped a gate. AX.W0a lands `gate_predicate_wire_contract.rs` — extend to all predicates, not just has_w4_classified.

15. **Small-input amortisation documented at plan time.** Sheets parse entries (505 B, 1.5 KB, 1.8 KB) cannot meet post-AU on SIMD+walker-specialised paths — fixed-cost doesn't break even. AX plan should declare per-entry break-even bytes and which levers apply below threshold. AW-III documented this post-hoc; AX must declare it at plan time.

16. **Predicate-widening requires re-bench of all downstream gates.** AW-V W4 widened Flat/Wrap detectors; consequence not surfaced until W6. Any wave that widens a classification predicate runs the bench matrix mid-wave against every gate it affects. The mid-wave-checkpoint item 1 already addresses this indirectly; make it explicit.

17. **"Architectural transposition complete; throughput in next wave" is not a closeable wave.** The AW-III, AW-IV, AW-V FINALs each invoke this formula. The substrate-without-consumer invariant in README.md §Code-discipline already forbids this; elevate to AX invariant with concrete wave-close refusal.
