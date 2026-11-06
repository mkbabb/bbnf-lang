# CRIT-blocker-audit — SK-V21 pass-2 blocker walk

Date 2026-07-05. Method: walk every pass-1 "Blocks 100" clause (CONVERGENCE-LEDGER.md:24-34)
and every pass-2 work-list item (LEDGER:63-84) against the pass-2 evidence
(prototypes PROTO-A1/A2/A3/B1/B3, research/38-tw1-seam-disposition.md, and the digest). Each
clause is scored DISCHARGED (by what) / NARROWED (to what) / STANDING (why). A NEW-blocker sweep
follows. The overall readiness score is computed from the blocker ledger alone, re-weighted by the
pass-1 plan-criticality weights (LEDGER:44-54).

One systematic caveat governs the whole walk: **no pass-2 session met the loadavg<2 quiet mandate**
(A1 ran loadavg 8.6-9.1, PROTO-A1:13-16; A2 8-16, PROTO-A2:12-14; A3 7-25, PROTO-A3:8-9; B1 24,
PROTO-B1:13). Every prototype self-declares "absolutes DIRECTIONAL; same-run ratios authoritative"
and punts quiet certification to TW0. So the measurement blockers are discharged **at the ratio/
decision level** (paired interleaving, min-of-7, same-window cross-harness), never at the absolute-
quiet level. Pass-2 work-item 1 ("the quiet bench session, loadavg<2") did NOT land as written; it
carries to TW0 intact. This caps every measurement discharge below full.

---

## Part 1 — Pass-1 per-item blocker clauses

### P1 — NEON presize (weight 5, pass-1 score 77)

- **"Quiet interleaved re-run with stddev/CI so the Sampled tie is demonstrated, not asserted"** —
  **DISCHARGED (decision-level)** by PROTO-A1 D1 (PROTO-A1:45-78): paired per-sample interleaved
  N=200 with 95% CI. canada +9.3% OVERTURNED → +1.25% CI[−0.43,+2.94] TIE; citm +0.77% TIE; twitter
  +3.45% CI[+1.07,+5.83] real. The +9.3% is confirmed the inter-invocation drift CRIT-P1 §2
  predicted. Interleaving + CI is exactly the instrument the clause demanded. Quiet-absolute leg
  still owed to TW0 (loadavg 8.6).
- **"json_canon_bench methodology review as the truth-floor instrument (CRIT-P1 residual a/b)"** —
  **DISCHARGED**: A1 rebuilt json_canon_bench with the two named gaps closed (P2 digest-parity
  assert folded; fold-over-cursor added as a first-class lane, PROTO-A1:5-9) and self-validated —
  twitter parse_tape 1.5654 cyc/B reproduces report-33's 1.559; canada fused/sonic within ~5%
  (PROTO-A1:20-23). Full byte-equality parity gate incl. fingerprint passes all 3 corpora
  (PROTO-A1:28-41) — strictly stronger than report-33's XOR-under-black_box.
- **NEW residual (see Part 3-N2):** "Sampled buys nothing" (SPEC:102-103) is now falsified as too
  strong — twitter +3.45% is a real small win (PROTO-A1:66-78). DELETE OneShotSimd/Exact/NEON stands
  overwhelmingly; Sampled keep/delete is a small real tradeoff, not the clean null the spec asserts.

**P1 disposition: measurement blockers DISCHARGED; TW0-quiet + one SPEC nuance edit remain. → ~88.**

### P2 — fused default lane (weight 15, pass-1 score 70)

- **"Fold-over-lazy-cursor (the spec's actual default, §4) unbenched vs sonic dual-path"** —
  **DISCHARGED with a NEGATIVE verdict** by PROTO-A1 D3 (PROTO-A1:116-145): the §4 construct is
  measured and LOSES sonic 2.32×/4.85×/2.27× and its own fused sink 3.9-4.2× on the three corpora.
  The unbenched bet is now benched — and refuted as competitive. This closes the sub-gate but
  triggers a SPEC §4 correction (Part 3-N1).
- **"No AU-relative cold capture"** — **DISCHARGED** by PROTO-A1 D4 (PROTO-A1:149-176): cyc/B vs the
  corrected 1.63 floor; structural parse_tape twitter 1.5654 BEATS AU; fused default meets AU on
  citm (1.354), +23% twitter, 2.0× canada (float tax localized).
- **"canada sign-contradiction / P2 0.79× vs report-33"** — **DISCHARGED (SETTLED)** by PROTO-A1 D2
  (PROTO-A1:82-112): fused LOSES canada 1.25× wall / 1.14× cyc; confirms report-33, refutes P2 0.79×;
  divergence attributed to mimalloc+thin-LTO+per-sample-clone+loadavg-16 inflation.
- **"Quiet re-run with digest-parity assert folded in"** — **NARROWED**: digest-parity assert folded
  and PASSING (PROTO-A1:28-41); the quiet-absolute leg carries to TW0.

**P2 disposition: all four measurement blockers DISCHARGED; the central per-corpus bet is now
doubly-sourced cold-confirmed; the negative §4 result migrates to spec-architecture. → ~85.**

### P3 — edition-2024 + stable (weight 8, pass-1 score 58)

- **"Cold-bench parity leg unmeasured — TW-S unpriced"** — **NARROWED**, not discharged, by PROTO-A3
  D1 (PROTO-A3:16-69): both channels now BUILT and RUN (P3 could not finish the nightly build), but
  the raw 2.37× nightly/stable gap is LOAD-CONFOUNDED (nightly window sustained loadavg 19-25 vs
  stable's lighter window; same 1.96 LLVM generation, no channel mechanism). The clean channel ratio
  still cannot be stated; carries to TW0. The same-window divan-vs-criterion cross-check (4-11%
  agreement, PROTO-A3:131-141) is the authoritative parity-relevant signal landed.
- **"§3.4 dependency modernization entirely unprototyped"** — **DISCHARGED** by PROTO-A3 D4
  (PROTO-A3:173-201): priced dep table with TW-S.1-5 wave assignment, each with from→to+risk.
- **"cargo tree transitive smallbox check undone"** — **DISCHARGED, and it FALSIFIES PROTO-P3** by
  PROTO-A3 D2 (PROTO-A3:73-117): smallbox count=0 through the canonical parse-that/regex subtree
  (egraph/csp-solver pull none); §7.3 is decoupled from the abrogation; PROTO-P3's coupling
  recommendation is false on the graph. Sole blocker is the legacy root `crates/` via
  parse_that 0.4.0→smallbox[nightly]; cutover retires the tree (moot) or de-nightlies parse_that.

**P3 disposition: two of three DISCHARGED (one falsifies a prior recommendation); parity ratio
NARROWED to a TW0 quiet re-capture. → ~82.**

### P4 — KernelCostTable (weight 10, pass-1 score 64)

- **"Flip demonstrated only against a tests-only selector with proxy kernels"** — **DISCHARGED** by
  PROTO-A2 D2 (PROTO-A2:92-121): re-run against the LIVE simd-scan kernels
  (classify_stripe_nibble/wide/multi, verbatim source-cited copies sharing the live movemask_u8x16),
  parity byte-exact both runs.
- **"No current-fleet grammar exercises either flip"** — **STANDING, now precisely characterized**
  (PROTO-A2:123-144): live crossover N*=3 (moved from P4's N*=2), flip band |S|≤2; JSON (N≈6-7) and
  CSS are N≥3, so NO current-fleet decision flips; the flip needs CSV (|S|≤2) = the third language,
  TW6. This is structural, not a defect — but the cost table stays consumer-dead until TW6, so its
  live consumer MUST be bound to the CSV wave in the spec.
- **"Cited raw logs absent from the worktree"** — **DISCHARGED**: p4v2-run1.log / p4v2-run2.log
  present beside PROTO-A2.
- **"Quiet re-run"** — **NARROWED**: N*=3 identical across both runs (load-robust); absolutes owed
  to TW0.
- Correction folded: FLIP-3 (movemask vshrn→vaddv) is inapplicable to the live path (already vaddv,
  PROTO-A2:146-156).

**P4 disposition: measurement blockers DISCHARGED; the consumer-dead-until-TW6 fact is nailed and
must be recorded as a TW6 binding in the table header. → ~82.**

### P5 — hand-asm door (weight 5, pass-1 score 58)

- **"Doors (a) vshrn-algorithm and (b) opaque-barrier both unpriced"** — **DISCHARGED** by PROTO-A2
  D1 (PROTO-A2:20-88): door(a) vshrn +36.5%/+34.6% over baseline, door(b) barrier +15.6%/+11.5%,
  hand-asm +6.7%/+10.4%; door(a) beats hand-asm ~20-28%. Both cheaper doors obviate the asm lane;
  the asm emission path is NOT warranted for this kernel (an intrinsic movemask swap suffices).
- **"Binding resource (ALU vs SIMD-GPR crossings) unestablished"** — **DISCHARGED**: the two levers
  are cleanly separated by doors (a)/(b); the crossing lever is worth ~2.6× the classify-ALU lever
  (PROTO-A2:60-68) — the 8 SIMD→GPR crossings bind, not the classify ALU.
- **"PMU quiet re-capture on the pinned toolchain"** — **NARROWED**: both doors' sign + crossing-
  lever ranking are ratio-robust across two windows; absolute cyc directional (loadavg 8-16); pinned-
  toolchain quiet capture owed to TW0.

**P5 disposition: door-pricing and binding-resource DISCHARGED; asm lane RETIRED for this kernel;
TW0-quiet absolutes remain. → ~86.**

### P6 — HM-core spike (weight 10, pass-1 score 64)

- **"The lowering fix itself is unimplemented" / "the upstream width destruction (22/33 land I64)"**
  — **DISCHARGED** by PROTO-B3 (PROTO-B3:8-18,106-131): 12-line helper in one file (wrap.rs, +45/−15)
  lands all 22 Nu8 tags at U8; i64=0, span_default=0, failed=0; checked=33 synth=0 (inverted from
  P6's checked=11/synth=22). Root cause corrected — the destroyed datum is the typed leaf's source
  span (view.rs:141 `_ => None`), NOT literals.rs (PROTO-B3:21-50). Substrate+consumer same-wave
  (consumer generate.rs already reads return_type), TW5-schedulable, with a width-correct close-gate
  (PROTO-B3:155-176).
- **"Union-find unifier exercised only synthetically (no @host fn in corpus)"** — **STANDING**: B3
  is the width leg only; the @host union-find integration is untouched and deferred to TW6 with the
  @host consumer (PROTO-B3:150-152).
- **"Record/Sum absent from corpus"** — **STANDING**: not addressed by pass-2 evidence.
- **"Integration into generate.rs:371/:399 is a sketch, never swapped in"** — **PARTIALLY
  DISCHARGED**: the width fix's consumer (generate.rs `return_type.unwrap_or(Span)`) is a live reader
  fed the correct value; the broader @host FnCall-args integration STANDS.

**P6 disposition: the width leg is DISCHARGED and implemented; the HM-unifier / @host / Record-Sum
legs STAND, deferred to TW6. → ~78.**

### P7 — TOML generalization (weight 12, pass-1 score 58)

- **"Walls A and B are unbuilt (generator defects, not measured fixes)"** — **DISCHARGED
  (demonstrated dissolvable)** by PROTO-B1 (PROTO-B1:59-114): a bounded regex interpreter (WALL B)
  and a one-bit ws-policy (WALL A) drive an emitter-shaped TOML descent that round-trips 7/7 against
  the toml crate at the real un-forked base efb6c875f; the WALL A falsifier breaks on cue (byte 21);
  all 5 hard leaves match full-length. Both are grammar-neutral, Lock-14-clean primitives.
- **"NO TOML end-to-end evidence exists against real HEAD; a P7-v2 is required"** — **NARROWED**: the
  emitted CODE SHAPE round-trips at the real base (worktree skew corrected d2b32f6e3→efb6c875f,
  PROTO-B1:3-9), but the in-tree REGEN pipeline wiring (thread WsPolicy through lower_program, swap
  the interpreter into the RegexProgram arm, fold JSON) is specified-and-scoped-to-lines, NOT
  executed. That wiring is the mechanical TW6 gate, not a wall (PROTO-B1:137-143).
- **WART C** — **NARROWED and made HEAVIER**: confirmed a two-site name-coupling
  (runtime_generator.rs:98 AND json_sink_direct/mod.rs:98, PROTO-B1:40-44), requiring a
  JSON-into-general_descent FOLD, not the cosmetic rename earlier docs implied. Bounded TW6 edit,
  but larger than P7 framed (Part 3-N3).

**P7 disposition: both walls DISCHARGED as dissolvable with runnable generic evidence; the in-tree
regen wiring + JSON fold NARROW to the TW6 execution gate. B1 self-scores 72. → ~72.**

### spec-architecture (weight 20, pass-1 score 64)

- **"TW1a split-first ordering still self-deferred (SPEC §13.3)"** — **DISCHARGED** by
  research/38 (38:126-184): split-first survives contact, keep TW1a→TW1b→TW1c; tape-first is NOT
  forced (no runtime↔codegen cycle — codegen emits text, only generated parsers link the tape,
  38:96-106). core is 81.6% xtask output; the 1,080/960 counts are 95%/88% regen-owned, so the true
  hand-edit surface is ~40-60 template sites (38:37-48). Two binding amendments (A1: TW1a splits only
  the 39.9K hand seam; A2: declare counts regen-owned) close the ordering.
- **"The amendments are unverified by a critique round"** — **PARTIALLY DISCHARGED**: this audit +
  the pass-2 fold-ins (A1-B3) verify the fold-ins match mandates; the full architecture re-score
  (work-item 5) is not complete.
- **"TW0 remains ~14 workstreams"** — **STANDING**: unchanged by pass-2 evidence.
- **NEW (Part 3-N1):** A1 D3 refutes the SPEC §4 "one substrate / fold-over-lazy-cursor" thesis;
  TW3's value-plane architecture needs a correction — a real, well-localized new architecture wound.

**spec-architecture disposition: §13.3 self-defer DISCHARGED; a new §4 architecture correction opens;
TW0 breadth STANDS. → ~72.**

### spec-honesty (weight 15, pass-1 score 84)

- **"No performance ranking is verified cold-quiet until TW0 runs (Defect 3, structural)"** —
  **STANDING, untouched**: every pass-2 measurement ran loadavg 8-25; the quiet mandate was unmet by
  design; all prototypes disclose it and punt to TW0. Defect 3 is exactly as honestly dispositioned
  as pass-1. Pass-2 strengthened the DISCIPLINE (paired interleaving, PMU ri_cycles, min-of-7,
  cross-harness reproduction of report-33) and caught two over-claims (P2 sweep, fold-over-cursor
  default), which is honesty working — but the cold-quiet verification itself is not advanced.

**spec-honesty disposition: Defect 3 STANDS by design (a TW0 gate); discipline reinforced. → ~85.**

---

## Part 2 — Pass-2 work-list items (LEDGER:63-84)

1. **Quiet bench session (loadavg<2, one binary)** — **PARTIALLY LANDED / mandate UNMET.** The P1
   CI, P2 canada+fold-cursor, P5 doors, and P4 live ladder all ran, on shared load-robust harnesses,
   and discharged their decision-level blockers — but at loadavg 8-16, not <2. The quiet-absolute
   certification carries to TW0 unchanged. This is the single largest reason readiness lands below the
   projected 85.
2. **P7-v2 TOML against real HEAD** — **LANDED** (PROTO-B1): ?w-derived ws-policy + real leaf matcher
   in general_descent shape, 7/7 toml-crate round-trip at efb6c875f. Argument→evidence achieved for
   the code shape; in-tree regen is the TW6 gate.
3. **P4-v2 live-selector flip** — **LANDED** (PROTO-A2 D2): live-kernel re-run; no current-fleet
   decision flips; TW6 consumer-binding recorded (|S|≤2 = CSV).
4. **TW1a/TW1b seam adjudication** — **LANDED** (research/38): file-level disposition table; split-
   first survives; §13.3 closed.
5. **Critique round over amended SPEC (architecture + honesty re-score)** — **IN PROGRESS**: this
   audit is a component; a full re-score of the amended draft (esp. the new §4 correction) is not
   yet complete.
6. **P3 residue (non-blocking)** — **LANDED EARLY** (PROTO-A3 D2): cargo tree smallbox check done;
   dep-modernization spike delivered.

---

## Part 3 — NEW blockers introduced by pass-2 evidence

- **N1 (material) — SPEC §4 "one substrate" thesis REFUTED.** PROTO-A1 D3 (PROTO-A1:116-145) shows
  the fold-over-lazy-cursor — the spec's literal default construct (SPEC:191 "Eager is a generic fold
  over the lazy cursor — one substrate," billed SPEC:191-194 as "the stronger claim" over sonic's
  dual-path) — is the SLOWEST lane, losing sonic 2.3-4.9× and its own fused sink 3.9-4.2×. The
  competitive default is the fused single-pass SINK (no tape), which is NOT a fold over a lazy cursor.
  SPEC §4 and the TW3 headline (SPEC:203-213) must be corrected: the eager path is the fused sink,
  distinct from the lazy cursor — conceding the "one substrate / beats sonic's dual-path" framing.
  Well-localized (TW3) and the fused sink is already the §2 named default, but it is a genuine
  architecture edit, not cosmetic. BLOCKS the spec-architecture re-freeze until corrected.
- **N2 (minor) — "Sampled buys nothing" is falsified.** PROTO-A1 D1 (PROTO-A1:66-78) finds twitter
  Sampled +3.45% CI[+1.07,+5.83], a real small win. SPEC:102-103's flat "buys nothing measurable" is
  too strong; DELETE OneShotSimd/Exact/NEON stands overwhelmingly, but Sampled keep/delete is a
  small real (twitter-only, sub-GrowOnly on 2/3) tradeoff. One-line SPEC §2/§5.6 nuance edit.
- **N3 (scoping) — WART C is a two-site fold, not a rename.** PROTO-B1:40-44 finds the JSON family is
  name-coupled at runtime_generator.rs:98 AND json_sink_direct/mod.rs:98; neutralization is a
  JSON-into-general_descent fold (delete both couplings), heavier than the single `has_rule("value")`
  gate SPEC:394 names. Bounded TW6 edit, but the spec's WART-C scope must widen to two sites + a JSON
  template retirement.
- **N4 (priced, non-blocking) — harness unification is not a dep swap.** PROTO-A3 D3 (PROTO-A3:163-169)
  surfaces 60+ criterion-estimates.json ingestion references in gate.rs/report.rs; "delete criterion→
  divan" requires re-pathing that layer first — its own TW-S sub-wave (TW-S.2), not a one-line
  deletion. Newly priced; assigned; not blocking.
- **N5 (reconciled, non-blocker) — movemask choice is kernel-shape-dependent.** PROTO-A2 D1 vs D2:
  vshrn movemask is cheaper for the no-space kernel (fewer crossings), vaddv already correct for the
  multi/nibble classify (PROTO-A2:146-156). Reconciled as a pluggable cost-model cell, not a global
  default — consistent with pluggable-components. No standing blocker; a design note for the cost
  table.

No pass-2 evidence introduced a blocker that reverses a pass-1 discharge or invalidates a landed
substrate. N1 is the only new item of consequence, and it closes an already-flagged OPEN sub-gate
with a negative answer rather than opening virgin territory.

---

## Part 4 — Overall readiness from the blocker ledger

Re-weighted by the pass-1 plan-criticality weights (LEDGER:44-54), using this walk's pass-2 scores:

| Item | Weight | Pass-1 | Pass-2 | Contribution |
|---|---:|---:|---:|---:|
| spec-architecture | 20 | 64 | 72 | 14.40 |
| spec-honesty | 15 | 84 | 85 | 12.75 |
| P2 fused lane | 15 | 70 | 85 | 12.75 |
| P7 generalization | 12 | 58 | 72 | 8.64 |
| P4 cost table | 10 | 64 | 82 | 8.20 |
| P6 type plane | 10 | 64 | 78 | 7.80 |
| P3 stack | 8 | 58 | 82 | 6.56 |
| P1 presize | 5 | 77 | 88 | 4.40 |
| P5 asm door | 5 | 58 | 86 | 4.30 |
| **OVERALL (pass 2)** | **100** | | | **79.80 → 80%** |

**Readiness: 80/100.** Up from pass-1's 67, short of the projected 85. The 5-point shortfall against
projection is honest and traceable: (a) work-item 1 (the quiet <2 session) did NOT land — every
measurement is ratio-authoritative but absolute-directional, so spec-honesty's Defect 3 is untouched
and every P-item's discharge is capped below full; and (b) N1 — the pass-2 evidence's biggest single
finding is a NEGATIVE one (the §4 one-substrate default is refuted), which discharges P2's central
measurement blocker but opens a spec-architecture correction that holds that item below the level a
clean-positive result would have reached.

What pass-3 must clear to freeze: N1 (§4/TW3 architecture correction + the fused-sink-vs-cursor
re-statement), the completion of work-item 5 (full architecture+honesty re-score against the corrected
draft), and — the only remaining MEASUREMENT of consequence — the TW0 quiet (<2) re-capture that
certifies the directional absolutes across P1/P2/P3/P4/P5. N2/N3/N4 are bounded edits. On the blocker
ledger alone, the tranche is one architecture correction and one quiet session away from freeze — a
pass-3 freeze pass, not a discovery pass, is now credible.
