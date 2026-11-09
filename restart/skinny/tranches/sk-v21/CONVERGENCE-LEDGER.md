# SK-V21 CONVERGENCE-LEDGER

Per-pass convergence accounting for the convergence-loop re-founding (PASS-ALPHA.md protocol,
step 5). 100 means the tranche plan could be frozen and executed tomorrow with no open question;
scores are conservative by charter.

---

## Pass 1 — 2026-07-05

Inputs: SPEC-DRAFT-P1.md (amended in place this pass), prototypes PROTO-P1..P7 (all seven ran;
several recovered from agent worktrees after the original critique round), critiques CRIT-P1..P7
(CRIT-P2-v2 supersedes CRIT-P2; CRIT-P7-adjudication supersedes both PROTO-P7's claims and
CRIT-P7's location of the wall), the recovered cold-truth capture (research/33), and gap-fills
(research/34-37).

### Item table

Scores are the final critique-round scores (superseding where re-adjudicated). "Moved by" names
the evidence that changed the item THIS pass; "Blocks 100" names what specifically remains. The
in-place spec amendments folded this pass address the editorial fraction of each blocker; they are
pass-2 critique input, not yet re-scored.

| Item | Conv. | Moved by (this pass) | Blocks 100 |
|---|---|---|---|
| P1 — NEON presize | 77 | Prototype ran with triple liveness proof (runtime/expand/nm); OneShotSimd shown live-and-losing 1.6-2.4x directional; Sampled ties GrowOnly; DELETE decided and folded into SPEC §2/§5.6/TW1c | Quiet (loadavg<2) interleaved re-run with stddev/CI so the Sampled tie is demonstrated, not asserted; json_canon_bench methodology review as the truth-floor instrument (CRIT-P1 residual a/b) |
| P2 — fused default lane | 70 (was 43) | PROTO-P2 recovered; cold instrument now exists twice (33's json_canon_bench + p2_fused_cold); reconstruction tax ~3.2x cold-confirmed, doubly sourced; twitter/citm cold wins corroborated; canada sign-contradiction adjudicated for report 33 (fused LOSES 9936 vs 11794, 33:55,63-69) and folded into SPEC §2 per-corpus | Fold-over-lazy-cursor (the spec's actual default construct, §4) unbenched vs sonic dual-path direct-to-struct; no AU-relative cold capture; quiet re-run on json_canon_bench with the digest-parity assert folded in; capture was loadavg 16-17, DIRECTIONAL |
| P3 — edition-2024 + stable | 58 | Skinny-on-stable settled clean (four unsafe-extern wraps, corrected count); smallbox blocker localized to the legacy `parse_that` leaf; the §3/§7.3 coupling claim refuted on Cargo.toml evidence (regex crate is smallbox-free); scope decision TAKEN in SPEC §3 (surviving-crates-only) | Cold-bench parity leg unmeasured (withheld at loadavg 15-18) — TW-S is unpriced; §3.4 dependency modernization entirely unprototyped; `cargo tree` transitive smallbox check undone (NO-cargo bound) |
| P4 — KernelCostTable | 64 | Null refuted (thresholds do not match measurement: LUT wins from N≥3, N*≈2); FLIP 3 movemask inconsistency found (movemask.rs:4-5 vshrn vs byte_class_from_eq_set_64.rs:79-84 vaddv); consumer re-bound in SPEC §5.1 to the live simd-scan KernelShape::select ladder; flip-consumer bound to TW6's third language | Flip demonstrated only against a tests-only selector with proxy kernels; no current-fleet grammar exercises either flip; cited raw logs absent from the worktree; loadavg 17-23 capture — quiet re-run against the live scan_nibble/scan_wide/scan_multi kernels required |
| P5 — hand-asm door | 58 | Instrument sound (200K parity inputs, canary guard, checkasm-shaped liveness); static op-count 40-vs-32 is a load-independent witness; ~10% instruction-selection exception found; SPEC §0/§5.4 rewordings folded (bound widened, gate cause added, doors-first standing order) | Doors (a) vshrn-movemask-algorithm and (b) opaque-barrier both unpriced — either may retire the asm question entirely; binding resource (ALU vs SIMD-GPR crossings) unestablished; PMU quiet re-capture on the pinned toolchain outstanding |
| P6 — HM-core spike | 64 | Spike typed 33/33 real descriptors through the actual frontend, zero Span defaults, three negative injections fire; exposed the upstream width destruction (22/33 land I64 where the grammar wrote U8) and the dropped FnCall args; SPEC §6 amended: step-0 lowering fix scheduled, width close-gate added, @host consumer moved in-wave | Union-find unifier exercised only synthetically (no @host fn in corpus); Record/Sum absent from corpus; integration into generate.rs:371/:399 is a sketch, never swapped in; the lowering fix itself is unimplemented |
| P7 — TOML generalization | 58 (was 40) | Adjudication re-located the true walls: BREAK 1 FALSE (projections ARE carried via general_descent; Sheets emits 31); dispatch un-forked at HEAD (the tested RuntimeEmitterKind fork is phantom); walls are three SHARED generator defects (ws hardcoding emit.rs:126-135, delimiter-scan match_leaf emit.rs:157-228, value name-gate runtime_generator.rs:98); SPEC §9 rewritten; Lock-14 widened to toml-crate round-trip | Walls A and B are unbuilt (generator defects, not measured fixes); P7's run is disqualified as measurement, so NO TOML end-to-end evidence exists against real HEAD; a P7-v2 against base 05302109d with the ?w policy and a real leaf matcher is required |
| spec-architecture | 64 | Whole-draft critique; B1-B5 all folded this pass: TW1 unbundled (TW1a/b/c), TW3-TW4 decode coupling carried (TW4.1 split), AU floor struck for the TW0 cold baseline, D-G reconcile defined executably (SPEC §11 TW0), R1/R2/R4/R7 gate-class and ownership refinements folded | The amendments are unverified by a critique round; TW1a split-first ordering still self-deferred (SPEC §13.3) — the one axis that decides substrate-first recurrence; TW0 remains ~14 workstreams (bundling reduced only at TW1/TW6) |
| spec-honesty | 84 | Highest-scoring item; Defect 1 (0.615 cyc/byte arithmetic impossibility) FIXED to 1.63 with the 09:144 provenance; Defect 2 (dual floor) COLLAPSED to the single TW0 cold baseline, AU demoted to directional context; report-33 accounting note corrected (file now exists) | Defect 3 is structural and honestly dispositioned: no performance ranking is verified cold-quiet until TW0 runs; the generalization proof (TOML) is scheduled last, so TW1-TW5 remain JSON/CSS-shaped exposure |

### Overall convergence — weighted by plan-criticality

Weights sum to 100 and follow how much of the frozen plan each item load-bears, not effort spent:
the two whole-spec items carry the wave graph and the evidence discipline (35 combined);
P2 carries the central performance bet and TW3's headline (15); P7 carries the charter's
no-overfit precept and TW6 (12); P4 and P6 each gate a full wave's core artifact (10 each);
P3 gates the mechanical TW-S (8); P1 and P5 are narrow, already-decided-or-priced doors (5 each).

| Item | Weight | Score | Contribution |
|---|---|---|---|
| spec-architecture | 20 | 64 | 12.80 |
| spec-honesty | 15 | 84 | 12.60 |
| P2 fused lane | 15 | 70 | 10.50 |
| P7 generalization | 12 | 58 | 6.96 |
| P4 cost table | 10 | 64 | 6.40 |
| P6 type plane | 10 | 64 | 6.40 |
| P3 stack | 8 | 58 | 4.64 |
| P1 presize | 5 | 77 | 3.85 |
| P5 asm door | 5 | 58 | 2.90 |
| **OVERALL (pass 1)** | **100** | | **67.05 → 67%** |

The pass-1 amendments close the purely editorial blockers (floor collapse, wave unbundling,
coupling carriage, gate rewordings, D-G definition) — but every remaining blocker of consequence
is a MEASUREMENT or a BUILD, not an edit, and no new measurement exists post-amendment. 67% is
therefore the honest pass-1 number; the amended spec is pass-2's input, to be re-scored by its
critique round.

### Pass-2 work-list (focused; in priority order)

1. **The quiet bench session** (single session, loadavg < 2, one binary where possible): the P2
   canada adjudication + fold-over-lazy-cursor vs sonic dual-path on `json_canon_bench` with the
   digest-parity assert; the P1 four-plan interleaved re-run with stddev/CI; the P5 PMU re-capture
   plus doors (a) vshrn-algorithm and (b) opaque-barrier. One session discharges the top blocker
   of P1, P2, and P5 simultaneously and de-directionalizes the ranking basis.
2. **P7-v2 — TOML against real HEAD**: worktree-prototype the ?w-derived whitespace policy and a
   minimal real leaf matcher in `general_descent`, delete the value name-gate, and round-trip one
   nontrivial TOML corpus against the `toml` crate. This converts the adjudication's "certifiable
   without per-grammar files" from argument to evidence.
3. **P4-v2 — the live-selector flip**: re-run the cost sweep against
   `crates/simd-scan::KernelShape::select` with the live kernels; either name a current-fleet
   decision the table changes or record the TW6 consumer-binding in the table's own header.
4. **TW1a/TW1b seam adjudication**: a file-level disposition table (MIGRATION.md discipline) for
   the core split proving the tape can land second — or reversing the order with cause. Closes
   SPEC §13.3, the last self-deferred ordering.
5. **Critique round over the amended SPEC-DRAFT-P1** (architecture + honesty re-score), including
   verification that the P1-P7 fold-ins above match the critiques' mandates.
6. **P3 residue at TW-S entry** (not pass-2-blocking): `cargo tree` smallbox transitive check;
   the dependency-modernization build spike if pass 2 has capacity.

Projected post-pass-2 position if items 1-5 land clean: the measurement blockers on P1/P2/P5
retire, P7 gains real evidence, and the two spec items re-score against an amended draft —
plausibly ~85%. A pass 3 is expected to be the freeze pass, not a discovery pass.

---

## Pass 2 — 2026-07-05

Inputs: SPEC-DRAFT-P1.md (pass-1-amended; re-amended in place this pass), pass-2 prototypes
PROTO-A1 (bench adjudication), PROTO-A2 (kernel PMU + P4-v2), PROTO-A3 (TW-S pricing), PROTO-B1
(TOML-real), PROTO-B3 (width fix), the seam table research/38, re-critiques
CRIT-spec-architecture-v2 (82), CRIT-spec-honesty-v2 (86), and CRIT-blocker-audit (80,
pre-re-score basis). Environmental adjudication, binding on every row: NO pass-2 session met the
loadavg<2 mandate (all ran 8-25); decisions are certified at the RATIO level (paired interleaving
+ CI, min-of-7 PMU, byte-equality parity, cross-harness reproduction of report 33); ABSOLUTES
remain directional; the quiet re-certification is a TW0 EXECUTION gate, no longer a research
question.

### Item table

| Item | Conv. | Moved by (this pass) | Blocks 100 |
|---|---|---|---|
| P1 — NEON presize | 88 (was 77) | A1 D1 interleaved-paired N=200 with 95% CI: canada +9.3% OVERTURNED to +1.25% TIE; twitter Sampled +3.45% real (CI excludes 0); OneShotSimd/Exact refuted overwhelmingly; json_canon_bench rebuilt with parity assert + validated against report 33 (1.5654 vs 1.559) | TW0 quiet-absolute leg only; the "Sampled buys nothing" spec wording corrected this pass (deleted for KISS, not null) |
| P2 — fused default lane | 85 (was 70) | A1 D2 settles canada (fused/sonic 1.252x wall, 1.137x cyc — confirms 33, refutes P2 0.79x); A1 D3 benches the fold-over-cursor NEGATIVE (loses sonic 2.3-4.9x, own sink 3.9-4.2x); A1 D4 takes the AU-relative capture (parse_tape twitter 1.5654 BEATS 1.63); parity byte-exact all corpora | TW0 quiet re-capture of the absolutes; the negative D3 verdict migrated to spec-architecture as N1 (folded this pass) |
| P3 — edition-2024 + stable | 82 (was 58) | A3: migration APPLIED (5 edits, verified in-worktree); divan port built+ran EXIT 0 both channels; smallbox `cargo tree` check DONE — count=0 through the canonical regex subtree, PROTO-P3's coupling recommendation FALSIFIED; dep modernization priced into TW-S.1-5; criterion deletion re-scoped (60+ ingestion refs → TW-S.2) | Nightly-vs-stable ratio LOAD-CONFOUNDED (nightly window loadavg 19-25) — clean channel ratio rides TW0; cold parity vs TW0 baseline remains the TW-S close deliverable |
| P4 — KernelCostTable | 82 (was 64) | P4-v2 (A2 D2) re-ran against the LIVE kernels, parity byte-exact both runs: live crossover N*=3 (stable), mis-select band \|S\|≤2, NO current-fleet flip — the TW6/CSV consumer binding is now measured fact; FLIP-3 found inapplicable to the live path (already vaddv; spelling is a cost-model cell); raw logs on disk | Consumer-dead until TW6 by structure (JSON/CSS are N≥3) — the CSV flip exercise is the TW6 execution item; TW0 quiet absolutes |
| P5 — hand-asm door | 86 (was 58) | A2 D1 priced both doors: door(a) vshrn intrinsic +34.6/36.5% over baseline, beats hand asm ~20-28%; door(b) +11.5/15.6%; binding resource established (8 SIMD→GPR crossings, ~2.6x the ALU lever); the asm lane is OBVIATED — P5's "exception the intrinsic cannot survive" refuted; §0/§5.4 reconciled this pass | TW0 quiet PMU absolute re-capture on the pinned toolchain (ratios settled); the per-kernel gate stays as law, unexercised |
| P6 — HM-core spike | 78 (was 64) | PROTO-B3 BUILDS the width fix: 12-code-line helper in wrap.rs (+45/−15, one file), all 22 Nu8 tags land U8, checked=33/synth=0; root cause CORRECTED (view.rs:141 span destruction, not literals.rs); §6 step-0 re-written to the proven patch with corrected loci | @host union-find unifier still synthetic-only (no @host fn in corpus); Record/Sum absent; both deferred to TW5/TW6 with their consumers — execution items, not open questions |
| P7 — TOML generalization | 72 (was 58) | PROTO-B1 at the real base efb6c875f (skew corrected): WALL A (one-bit ws-class) and WALL B (bounded regex interpreter) proven dissolvable by grammar-neutral primitives — 7/7 toml-crate round-trip, falsifier fires on cue, all 5 hard leaves match; WART C widened to a two-site JSON fold; §9 re-written with the submodule pin | The in-tree regen wiring (WsPolicy threading, interpreter swap, JSON fold) is specified-to-lines but NOT executed — the mechanical TW6 gate; heavier WART C scope carried |
| spec-architecture | 82 (was 64) | All five pass-1 blockers verified CLOSED (v2); §13.3 seam self-defer DISCHARGED by research/38 (split-first survives; 81.6% of core is regen-owned; counts are regen artifacts); TW4.1/4.2 coupling proven correct by A1 D4; ONE new blocker (N1: §4 one-substrate default refuted) + six refinements (NEW-R1..R7) — ALL folded into the spec this pass | The pass-2 amendments (esp. the §4/TW3 fused-sink rewrite) are unverified by a critique round — the pass-3 re-score; TW0 breadth (~14 workstreams, R3) tolerated, unsequenced |
| spec-honesty | 86 (was 84) | Both v1 must-fix defects verified discharged; pass-2 confirms the hard calls (canada loss double-sourced, smallbox decoupling proven, loadavg discipline honored); Defects A (asm flagship sentence) and B (default labelled unbenched while benched-negative) and C (Sampled) identified — ALL reconciled in the spec this pass | Defect 3 stands by design: no ranking is verified cold-quiet until TW0 runs — an execution gate; the reconciled flagship text awaits the pass-3 re-read |

### Overall convergence — weighted by plan-criticality (weights unchanged from pass 1)

| Item | Weight | Score | Contribution |
|---|---|---|---|
| spec-architecture | 20 | 82 | 16.40 |
| spec-honesty | 15 | 86 | 12.90 |
| P2 fused lane | 15 | 85 | 12.75 |
| P7 generalization | 12 | 72 | 8.64 |
| P4 cost table | 10 | 82 | 8.20 |
| P6 type plane | 10 | 78 | 7.80 |
| P3 stack | 8 | 82 | 6.56 |
| P1 presize | 5 | 88 | 4.40 |
| P5 asm door | 5 | 86 | 4.30 |
| **OVERALL (pass 2)** | **100** | | **81.95 → 82%** |

The bracket is honest: CRIT-blocker-audit's independent pre-re-score computation lands 80 (it
scored spec-architecture 72/honesty 85 with work-item 5 in progress); the superseding v2 critique
scores land 82. Pass 2 therefore closes at **80-82, ledgered 82** under the pass-1 discipline
(final critique-round scores govern). The 3-point shortfall against the ~85 projection is
traceable to two facts: the quiet (<2) session did NOT land — every measurement is
ratio-authoritative, absolute-directional — and the largest pass-2 finding is a NEGATIVE one (N1:
the §4 one-substrate default refuted), which discharged P2's blocker while opening a
spec-architecture correction.

### What blocks 100 (pass-3 list; in priority order)

1. **The pass-3 verification re-score** — the only remaining non-execution blocker. This pass's
   in-place amendments (the §4/TW3 fused-sink rewrite closing N1; the §0 asm reconciliation
   closing Defect A; Defect B/C relabels; the §3/§5/§6/§9/§11/§13 fold-ins) are pass-3 critique
   input, not yet re-scored — same discipline as pass 1. Verify each fold-in against its mandate
   (CRIT-arch-v2 NEW-B1 + NEW-R1..R7; CRIT-honesty-v2 A/B/C; blocker-audit N1-N5).
2. **The freeze decision**: if the re-score confirms the fold-ins, every remaining blocker is an
   execution-tranche deliverable owned by a named wave — TW0 quiet re-certification (the sole
   remaining MEASUREMENT of consequence), TW-S cold parity, TW5 step-0 re-land, TW6 regen wiring
   + CSV flip. Freeze then proceeds: commit the D-G reconcile, tag, open TW0.
3. **TW0 internal sequencing** (R3 residue, non-blocking): order the ~14 workstreams inside TW0's
   letter (D-G → supersession docs → lock audit → Lock-15 → truth-floor quiet session) in the
   execution plan, without re-lettering.

No new prototype or measurement is required for pass 3. A pass-3 freeze pass, not a discovery
pass, is now the expected close.

---

## Pass 3 — 2026-07-05 (the freeze pass)

Inputs: SPEC-DRAFT-P1.md (pass-2-amended; re-amended in place this pass), pass-3 re-scores
CRIT-arch-v3 (94, wave-owned residuals: false — two blocking spec edits), CRIT-honesty-v3 (94,
wave-owned residuals: true — zero blocking), freeze-audit (95, wave-owned residuals: true — zero
blocking). Verification-only, per the pass-2 projection: no new prototype, no new measurement;
every P-item score CARRIES from pass 2 (their residuals were re-certified wave-owned execution
deliverables by the freeze audit, not open questions).

### The two blocking issues, resolved in place this pass

1. **NEW-R8 (primary, spec-edit): the TW4.1 cost-table gate contradiction.** §11's TW4.1 close
   gate was liveness-only ("the table feeds the live selector with a `cargo expand` witness"),
   but §1.1's standing law mandates liveness AND a measured ≥3% win — a clause unmeetable at
   TW4.1 by settled fact, since A2/P4-v2 measured NO current-fleet decision flip (live crossover
   N\*=3, flip band |S|≤2 = TW6's CSV; folded as fact at §5.1). §5.1 reconciled the intent (the
   TW6 consumer binding) but no gate line carried it. RESOLVED: TW4.1 now carries the explicit
   gate-relaxation sentence, mirroring the NEW-R7 clause TW1b already has — the cost-table leg
   closes on liveness alone with the ≥3%-win clause binding at TW6's CSV flip exercise; the
   Eisel-Lemire leg keeps the full liveness-AND-win clause (canada-beats-sonic IS its win).
2. **NEW-R9 (cosmetic): the §4 lane-phrasing self-tension.** "Two lanes over the one tape-side
   substrate" collided with "eager = the tapeless sink" in the same clause. RESOLVED: rephrased
   to "two lanes, only one of them tape-side," matching the cleaner §0 charter phrasing (lazy
   cursor over the ONE tape plus a DISTINCT tapeless fused single-pass sink) with a §0 pointer.

### Item table

| Item | Conv. | Moved by (this pass) | Blocks 100 |
|---|---|---|---|
| spec-architecture | 94 (was 82) | v3 re-score verified all pass-2 fold-ins (NEW-B1 + NEW-R1..R7, the §4/TW3 fused-sink rewrite) against their mandates; found exactly two blockers, both spec edits — NEW-R8 (TW4.1 gate contradiction) and NEW-R9 (§4 phrasing) — RESOLVED in place this pass | The two in-place amendments await the freeze-confirmation read — a micro-verification, not a critique round |
| spec-honesty | 94 (was 86) | v3 re-score: Defects A/B/C reconciliations confirmed; the flagship asm sentence, the per-corpus canada disclosure, and the Sampled relabel all read true; zero blocking issues | Defect 3 stands by design (no ranking verified cold-quiet until TW0 runs) — wave-owned, TW0 |
| freeze-audit (instrument) | 95 | Independent third read over the whole draft: zero blocking issues; certifies every remaining P-item residual as a wave-owned execution deliverable (TW0/TW-S/TW5/TW6), none an open design question | Not a weighted item — the freeze instrument of record |
| P1 — NEON presize | 88 (carried) | No new evidence (verification pass); residual re-certified wave-owned | TW0 quiet-absolute leg |
| P2 — fused default lane | 85 (carried) | Same | TW0 quiet re-capture of absolutes |
| P3 — edition-2024 + stable | 82 (carried) | Same | TW-S cold parity; TW0 un-confounded channel ratio |
| P4 — KernelCostTable | 82 (carried) | Same; the NEW-R8 fix carries its no-flip fact into the gate line | TW6 CSV flip exercise; TW0 quiet absolutes |
| P5 — hand-asm door | 86 (carried) | Same | TW0 quiet PMU absolute re-capture |
| P6 — HM-core spike | 78 (carried) | Same | TW5 step-0 re-land; TW5/TW6 consumer waves |
| P7 — TOML generalization | 72 (carried) | Same | TW6 in-tree regen wiring (WsPolicy, interpreter swap, JSON fold) |

### Overall convergence — weighted by plan-criticality (weights unchanged from pass 1)

| Item | Weight | Score | Contribution |
|---|---|---|---|
| spec-architecture | 20 | 94 | 18.80 |
| spec-honesty | 15 | 94 | 14.10 |
| P2 fused lane | 15 | 85 | 12.75 |
| P7 generalization | 12 | 72 | 8.64 |
| P4 cost table | 10 | 82 | 8.20 |
| P6 type plane | 10 | 78 | 7.80 |
| P3 stack | 8 | 82 | 6.56 |
| P1 presize | 5 | 88 | 4.40 |
| P5 asm door | 5 | 86 | 4.30 |
| **OVERALL (pass 3)** | **100** | | **85.55 → 86%** |

The number is read honestly: 86 is NOT the freeze ceiling failing — the P-item scores (72-88) are
frozen-at-pass-2 by discipline because their remaining blockers are MEASUREMENTS and BUILDS owned
by named waves (TW0 quiet session, TW-S parity, TW5 step 0, TW6 wiring + CSV flip), which no
amount of further spec-passing can retire. The plan-freeze question is carried entirely by the
three verification instruments: 94/94/95, with the only two blocking findings resolved as
one-file spec edits this pass. FREEZE WAS NOT CONFIRMED at the pass-3 re-score (NEW-R8 stood
open at read time); it is expected to confirm on the amended draft.

### What remains (focused next-pass list; in priority order)

1. **Freeze confirmation** — verify the two pass-3 amendments against their mandates: the TW4.1
   gate-relaxation sentence (NEW-R8; §11 TW4.1 close-gate block) and the §4 two-lanes phrasing
   (NEW-R9; §4 value-plane paragraph). A read, not a critique round — the sole remaining
   non-execution check.
2. **On confirmation, the freeze executes**: SPEC.md written from the amended draft; the TW0
   first commit (the D-G reconcile, §11) lands; the `sk-v19-frozen` tag placed; TW0 opens.
3. **Everything else is wave-owned execution, none of it pass-shaped**: TW0 quiet (loadavg<2)
   re-certification of the pass-2 directional absolutes incl. the channel ratio; TW-S cold
   parity; TW5 step-0 re-land; TW6 regen wiring + the CSV flip exercise.

---

## Pass 4 — 2026-07-05 (the confirmation pass; FINAL)

Inputs: SPEC-DRAFT-P1.md (pass-3-amended: the NEW-R8 gate-relaxation sentence and the NEW-R9
two-lanes rephrase, the only two changes since the v3 reads), the confirmation read
CRIT-spec-architecture-v4, the carried CRIT-spec-honesty-v3 (94, zero blocking) and
CRIT-freeze-audit (95, `residuals_wave_owned = true`). Confirmation-only, per the pass-3
projection: no new prototype, no new measurement, no new amendment beyond the freeze-pass
authoring outputs the audit itself named as what declaring convergence TRIGGERS.

### The confirmation score

**CRIT-spec-architecture-v4: 100/100.** Both pass-3 blockers verified folded exactly — NEW-R8
(the TW4.1 cost-table gate now carries the explicit liveness-only relaxation mirroring TW1b's
NEW-R7 clause, the ≥3%-win clause bound to TW6's CSV flip and that binding closed at all four
sites: the TW4.1 gate, the TW6 gate, §5.1, §13; the Eisel-Lemire leg keeps its own measured-win
gate so the wave is not dead substrate) and NEW-R9 (§4 "two lanes, only one of them tape-side"
now matches the §0 charter wording; no affirmative one-substrate residue survives, retractions
only). The sweep of both amended regions surfaced zero new contradiction; honesty-v3's two
editorial hairlines lay outside the amended regions and are corrected in the promoted SPEC.md.

### The freeze declaration

**The criterion** (CRIT-freeze-audit:3-5, standing since pass 3): *the tranche plan may be
declared converged ONLY IF every remaining blocker is a wave-owned execution item AND no open
research or design question remains.* **Met**: arch-v4 100 (zero blockers), honesty-v3 94 (zero
blocking), freeze-audit 95 (`residuals_wave_owned = true`); every residual is owned by a named
wave — the TW0 quiet re-certification (W0.11), TW-S cold parity, TW5 step-0 re-land, TW6 regen
wiring + the CSV flip — and the one ring-fenced frontier uncertainty (CSS SIMD tokenization
severity) is a costed TW4 decision point with a defined mechanism, not a design unknown.
**FREEZE CONFIRMED.** SPEC.md is promoted from the draft as the governing manifest (with §11.1
TW0 internal sequencing, W0.0-W0.12, discharging the last pass-2 residual without re-lettering,
and §14 THE FORWARD PROMPT); GESTALT-SYNTHESIS.md and the final DEFERRED-LEDGER.md capstones
accompany it (commit `3538043c5`). SPEC-DRAFT-P1.md is superseded and retained as the
convergence record.

### Item table and the final percentage

| Item | Weight | Score | Contribution |
|---|---|---|---|
| spec-architecture | 20 | 100 (was 94) | 20.00 |
| spec-honesty | 15 | 94 (carried) | 14.10 |
| P2 fused lane | 15 | 85 (carried) | 12.75 |
| P7 generalization | 12 | 72 (carried) | 8.64 |
| P4 cost table | 10 | 82 (carried) | 8.20 |
| P6 type plane | 10 | 78 (carried) | 7.80 |
| P3 stack | 8 | 82 (carried) | 6.56 |
| P1 presize | 5 | 88 (carried) | 4.40 |
| P5 asm door | 5 | 86 (carried) | 4.30 |
| **OVERALL (pass 4, execution-inclusive index)** | **100** | | **86.75 → 87%** |

Two numbers, both honest, deliberately distinct:

- **The execution-inclusive index closes at 87** — the weighted continuation of the pass-1
  discipline. The 13-point gap is composed ENTIRELY of P-item residuals frozen-at-pass-2 by
  discipline: measurements and builds (the quiet session, TW-S parity, the step-0 re-land, the
  TW6 wiring + flip) that are now SCHEDULED DELIVERABLES inside the frozen plan. No further
  spec-pass can move this number; it is the loop's fixed point, and its remainder transfers to
  the implementation campaign's ledgers.
- **Plan convergence closes at 100** — the loop's terminating quantity per PASS-ALPHA.md:50-51
  ("whereupon 100% convergence: stop, and develop out that exact tranche plan"), under the
  freeze criterion the loop's own instruments defined and applied: 100 is
  frozen-executable-tomorrow with zero surviving blockers and every residual wave-owned. The
  plan-freeze question was carried by the three verification instruments (100/94/95); it is
  answered. The loop STOPS here.

### Closing statement — what the loop settled, passes 0-4

Pass 0 (32 agents, research/01-32) established the substrate: the dig named the master
pathology (substrate-without-consumer, fourteen instances) and its one executed cure (the Y.13
consumer-invariant gate); the now-state audits found the dormant nightly surface, the two-crate
regex collision, and the god-crate topology; the SOTA sweeps bounded the frontier (stage-2
dominates on Apple silicon; no system holds the grammar×emitted-SIMD pair). Pass 1 (P1-P7 +
critiques, 67%) authored the draft and took the first hard decisions: presize DELETE, the
per-corpus fused-lane thesis, skinny-on-stable, the cost-table consumer re-bind, the TOML
adjudication. Pass 2 (A1/A2/A3/B1/B3 + research/38, 82%) was the measurement pass and delivered
every reversal the plan now stands on: the canada sign settled as an honest LOSS with the named
fix, the one-substrate fold-over-cursor REFUTED and retracted, the hand-asm lane OBVIATED by
its own priced doors, TW-S priced to five edits, the TOML walls dissolved by grammar-neutral
primitives at 7/7, the width fix built. Pass 3 (86%) verified every fold-in and reduced the
open surface to two spec edits, resolved in place. Pass 4 confirmed them at 100 and froze.

What is settled, permanently and by evidence: fn-per-rule recursive descent over the ONE AoS
tape (the SoA reversal honored); a dual-lane value plane — lazy cursor over the tape, DISTINCT
tapeless fused sink as default — matched to sonic-rs knowingly after the stronger claim lost on
the bench; intrinsics-first kernels with the asm door priced shut; the greedy extractor
permanent over a measured KernelCostTable, the tautological CSP stage retired; stable 1.96.1 +
edition 2024 everywhere; generalization certified by a third language through three SHARED
generator fixes, never per-grammar emitters; and the whole campaign sequenced
TW0 → TW-S → TW1a/b/c → TW2 → TW4.1 → {TW3 ∥ TW4.2} → TW5 → TW6 under the §1 standing law,
opening with the D-G reconcile and the `sk-v19-frozen` tag. The convergence loop is closed;
what follows is execution, governed by SPEC.md §14.
