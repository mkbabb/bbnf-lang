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
