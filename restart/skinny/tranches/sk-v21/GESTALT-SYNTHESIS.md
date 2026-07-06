# SK-V21 GESTALT-SYNTHESIS — the convergence-loop re-founding, closed

Date: 2026-07-05. HEAD at close: `05302109d` (SK-V20 close; zero code commits since — the cutover
commit 0 / D-G reconcile executes as TW0's first act, SPEC §11, §11.1 W0.0). Convergence
trajectory: 67 → 82 → 86 → freeze CONFIRMED at pass 4 — the three pass-3 verification instruments
at 94/94/95 with verdict READY-TO-FREEZE, then the confirmation verify at **100/100**: both
pass-3 amendments folded exactly, zero new contradiction in the amended regions
(CONVERGENCE-LEDGER.md:55,130,214; critiques/CRIT-freeze-audit.md:136-146;
critiques/CRIT-spec-architecture-v4.md:10-15). The governing plan is the frozen **SPEC.md**
(promoted from SPEC-DRAFT-P1.md, which it supersedes), wave order
**TW0 → TW-S → TW1a → TW1b → TW1c → TW2 → TW4.1 → {TW3 ∥ TW4.2} → TW5 → TW6** (§11), TW0
internally sequenced W0.0–W0.12 (§11.1). This document is the campaign's synthesis: what the dig
proved, what the measurements corrected, what generalizes, what the stack becomes, how the
evidence is graded, and what the loop itself was worth.

---

## §1 — The archaeological verdict

The 49-tranche excavation (research/01-15, 668 files, 14 MB of pre-restart history) returns one
master pathology, unanimously: **substrate-without-consumer**. Fourteen instances across eighteen
tranches (research/09:190-206); the #1 chronic anti-pattern of the AK→AV retrospective
(research/07:174-192); tranche V the archetype at tranche granularity — "spent without gain — the
substrate exists but no emission consumer reads `ir.recognizer_decisions`" (`W/W.md:450`, via
research/15:67-73); the AO structural-dispatch chain built across six sub-phases, landed with the
activation flag hardcoded false, zero calls at close, deleted wholesale at AQ.5 (research/04;
research/30 claim 3); AY reproducing the pattern at 5 of 7 closed waves, each wave "complete with
recorded misses" — a reviewer-unfalsifiable escape valve (research/09:200-206). Spec-driven tranches
scored 12/14 dead-substrate instances; profile-driven tranches scored 2/2 with zero dead substrate
(research/09 §C4).

The one executed cure in the entire corpus is the **W→X→Y→Z arc** — git-tagged 2026-04-09 through
2026-04-10, the single most disciplined body of work in the dig (research/15:18-24). Its mechanism
is the **Y.13 consumer-invariant test**: an exhaustive-match gate over every emitted decision
variant that fails to compile until each variant has an observable consumer (`Y/Y.md:326-333`, via
research/15 §1; research/35 N21). SK-V21 promotes that mechanism to standing law (SPEC §1): every
substrate wave closes on a consumer-invariant runtime gate in `crates/` — liveness AND a measured
win, because P1 proved liveness alone insufficient (a scan proven live under `cargo expand` still
lost, SPEC §2); no substrate lands without its consumer in the same wave; profile-driven over
spec-driven; "complete with recorded misses" is not a close state.

The era arc that produced the pathology, and the one invariant that survived it:

- **Era II** set the founding thesis — grammar as standalone input language, the interner-backed IR
  as the single source of truth from which every backend emits (`archaeology/era-II-foundations.md:27-47`,
  via research/15:34-39). That thesis survived every era and was re-anchored at every drift; SK-V21
  is faithful to it.
- **Era IV/V** is the divergence: seven substrate pieces (DTA, PSI, columnar tape, …), none reaching
  parity with the AU baseline; ~600 tranche-tagged commits in five days; per-grammar hand-coded
  backend files — over six thousand lines, reverted (research/15:162-194). The DTA/tape-automaton
  family is settled-negative: non-uniform state bodies cannot compile into fast code
  (research/07 §4.1).
- **The AU peak and the fall**: twitter 1967 MB/s at 1.63 cyc/byte, 0.91× sonic (research/09:144);
  every tranche after AU regressed from it, bottoming at 3.62× worse — a 4.5× fall on the
  most-tested fixture — by eroding exactly three primitives: the flat AoS `Vec<TapeRec>` write path
  (pivoted to a 7-column SoA that LLVM could not fuse, "the single largest regression driver"),
  the unified `push_leaf_with`, and `.map(|_| ())` elimination (research/09:152-164). The SoA
  reversal is the hardest-won negative result of Era VI (research/14; research/35 N19) and directly
  reverses SK-V20's C1 title: TW1b lands **AoS hardening**, never a columnar pivot (SPEC §4).
- **The proof of the winning shape**: the AW-V fn-per-rule prototype — 2,246 LOC of recursive
  descent, inline SIMD kernels, one monomorphic visitor, no interpreter — beat sonic-rs 5/5 with
  91.15% of self-time in one monomorphized symbol (research/07 §3, §5.1). And the gap at HEAD was
  never a parse-speed floor: the visitor lane already ran 0.99× sonic geomean while the default
  path paid the tape-then-walk reconstruction tax (research/09:166-189) — which is why the fused
  single-pass sink is the default materialization lane of this plan (§2 below).
- **The restart itself repeated the disease one level up**: ~600K lines of markdown describing a
  24-crate greenfield never executed, a live tree of 13 crates dominated by a 247,428-LOC `core`
  god-crate, doc-to-code 3.74:1 on a frozen codebase (research/20 §0, §2). TW0 marks the 24-crate
  design SUPERSEDED in writing; TW1a executes the real split along the half-designed seams, scoped
  by research/38 to the 39,931 hand-written LOC (the 176,887 generated LOC relocate as bulk
  `git mv`).

The verdict in one sentence: the campaign never lacked architecture or speed — it lacked the
discipline of landing consumers with substrates, and the W-X-Y-Z gate template plus profile-first
ordering is the cure this plan binds into every wave's close condition.

## §2 — The corrected performance thesis

Every number below carries its environmental grade (§5). The thesis as frozen:

- **The AU floor is met by the structural substrate at HEAD.** AU's recorded peak is 1.63 cyc/byte
  on twitter (research/09:144; the report's prose "0.615" is bytes/cycle mislabeled — the
  arithmetic 3.2e9/1967e6 ≈ 1.63 settles it, SPEC §2). The pass-2 AU-relative capture measures
  structural `parse_tape` at **1.5654 cyc/byte on twitter — under the floor**, reproducing report
  33's independent 1.559 (prototypes/PROTO-A1-bench-adjudication.md:21,156; research/33). AU is
  demoted to directional context; the single governing floor is the TW0 fresh cold baseline.
- **The fused single-pass sink is the default eager lane, per-corpus honest.** Cold, it beats
  sonic-rs on twitter and citm (~0.5-0.6× ratios) and beats the tape-then-walk reconstruction path
  ~3.2× (SPEC §2). **canada LOSES, sign settled**: fused/sonic 1.252× wall, 1.137× cyc/byte
  (PROTO-A1:88), confirming report 33 (9936 vs 11794 Mbps, research/33) and refuting the pass-1
  private 0.79× — the loss is the eager `materialize_f64` float tax, the sharpest optimization
  signal in the corpus, owned by TW4.1's Eisel-Lemire/SWAR decode whose close gate IS
  canada-beats-sonic-cold (SPEC §11 TW4.1). No sweep claims, no geomean: per-corpus, one honest
  loss, one named cure.
- **The one-substrate fold-over-lazy-cursor is REFUTED as the default.** The pass-1 elegance thesis
  (eager as a generic fold over the lazy cursor) was benched exactly as specified and lost on all
  three corpora — 2.32×/4.85×/2.27× slower than sonic, 3.87-4.23× slower than its own fused sink
  (PROTO-A1 D3; SPEC §4). The value plane is therefore two lanes, only one tape-side: the lazy
  cursor over the ONE tape, and the DISTINCT tapeless fused sink — the dual-path shape sonic-rs
  ships, matched knowingly. The elegance argument conceded to the measurement.
- **The hand-asm lane is OBVIATED by an intrinsic spelling.** The doors-first pricing (PROTO-A2 D1)
  found the pure-intrinsic `vshrn` movemask worth +34.6/36.5% over baseline on the classify kernel
  (prototypes/PROTO-A2-kernel-pmu.md:55), beating the hand asm (+6.7/10.4%) by ~20-28% — because
  the binding resource is the 8 SIMD→GPR crossings, not the classify ALU (the crossing lever is
  ~2.6× the ALU lever). P5's candidate "exception the intrinsic cannot survive" is refuted; zero
  measured asm exceptions remain; the per-kernel `asm!` gate stands as law, presently unexercised
  (SPEC §0, §5.4).
- **The structural pre-scan presize is resolved NEGATIVE, seventh loss terminal.** Six historical
  attempts, six losses (research/07 §5.4); P1's `OneShotSimd` — proven live three ways — regressed
  1.6-2.4×; the interleaved-paired CI re-run (N=200, 95% CIs) overturned Sampled's canada +9.3% to
  a +1.25% tie and left only a +3.45% twitter sliver — deleted for KISS (SPEC §2). `GrowOnly` only;
  a CONSUMED stage1→stage2 index may return solely via the cost table with a measured win.
- **The live kernel crossover is N\*=3, and the flip arrives with CSV at TW6.** P4-v2 re-ran the
  cost sweep against the LIVE `scan_nibble/scan_wide/scan_multi` kernels, parity byte-exact: the
  nibble/wide crossover sits at N\*=3, stable across runs (PROTO-A2:119,138); the selector ladder
  mis-selects only at |S|≤2; **no current-fleet decision flips** — JSON (N≈6-7) and CSS sit at N≥3.
  The KernelCostTable's first outcome-changing flip is therefore bound by structure to TW6's third
  language (CSV, |S|≤2), a measured fact recorded in TW4.1's relaxed gate (SPEC §5.1, §11 TW4.1) —
  the table lands with its consumer named, or it is the §1 pathology it exists to cure.
- **Where the remaining time goes**: number materialization + digest ≈ 0.6 cyc/byte, +45% over
  parse_only — matching the external frontier exactly (simdjson stage-2 dominance on Apple
  silicon, canada 1.847 vs 0.547 c/B — research/22 §2; research/21). The win lives in number decode
  and per-value store cost, not structural skipping.

## §3 — The generalization verdict

The charter's no-overfit precept closes on evidence, not assertion:

- **Dispatch is un-forked and structural at HEAD.** P7's claimed per-grammar fork
  (`RuntimeEmitterKind`) is phantom — nonexistent at HEAD; P7 ran against a stale base and is
  disqualified as measurement, retained as a break census (critiques/CRIT-P7-adjudication.md;
  SPEC §9). `->` projections ARE carried for non-JSON grammars via `general_descent` (Sheets emits
  31 projections). The generator is more general than its own prototype believed.
- **The true walls were three SHARED generator defects, and both hard ones are dissolved by
  grammar-neutral primitives** (PROTO-B1, run at the real submodule base `efc6b875f`): WALL A —
  the hardcoded skip-whitespace between Seq elements — falls to the one-bit `?w`-derived
  `WsClass::inline()` policy (falsifier fires on cue: the current behavior hard-misparses at byte
  21, swallowing the newline after `count=42`); WALL B — the delimiter-scan `match_leaf`
  approximation — falls to a bounded regex interpreter that matches all five hard TOML leaves
  full-length (RFC3339 datetimes, escaped strings, non-greedy multiline, `0xDEAD_BEEF`, `1e10`).
  An emitter-shaped TOML descent using ONLY those primitives **round-trips 7/7 against the `toml`
  crate** (prototypes/PROTO-B1-toml-real.md:18,97).
- **WART C is the JSON fold**: the residual name-coupling is a TWO-site gate
  (`runtime_generator.rs:98` AND `json_sink_direct/mod.rs:98`) misrouting any grammar owning a rule
  named `value` onto verbatim JSON templates; its cure is folding JSON into `general_descent` and
  retiring the templates — structural, not a sniffer deletion (SPEC §9).
- **Lock-14 is re-founded as a positive gate**: the emitted TOML parser round-trips the `toml`
  crate field-for-field — never directory-path greps (SPEC §9, §11 TW6). What remains for TW6 is
  mechanical regen wiring (WsPolicy through `lower_program`, the interpreter into the
  `RegexProgram` arm, the JSON fold), not a wall. The novelty seam stays narrow and verified
  still-empty as of July 2026: first GENERATOR to emit grammar-derived block-wide SIMD structural
  classification with a typed value plane (research/26 §0, §7).

## §4 — The stack abrogation

The nightly pin justified itself on three features, all three disabled — the workspace rode nightly
for zero active benefit (research/19; verified research/31 items 9/15). The abrogation, priced and
partially executed in-worktree at pass 2 (PROTO-A3):

- **Edition 2024 everywhere**: five edits total (edition bump, rust-version 1.88, four
  `unsafe extern` wraps), applied and verified — the divan port builds and runs EXIT 0 on both
  stable 1.96.1 and the old nightly (SPEC §3.1). Executed-mechanical, not estimated.
- **Pinned stable post-cutover** (`channel = "1.96.1"`): retires the AttrId-ICE bisect treadmill;
  nightly returns only under a measured cold-build win (SPEC §3.2). The nightly-vs-stable channel
  ratio is the one load-confounded pass-2 number; its clean capture rides TW0.
- **smallbox decoupled via the canonical engine**: the stable blocker dies at one legacy leaf
  (`parse_that` → `smallbox[nightly]`), which the cutover retires; `cargo tree` through the
  canonical parse-that/rust/regex subtree counts ZERO smallbox — the §7.3 regex unification does
  not re-import the nightly surface, falsifying the prototype's coupling recommendation
  (PROTO-A3 D2; SPEC §3 scope decision).
- **criterion → divan**, honestly scoped: divan is the warm/iterated kernel-microbench tier only;
  the cold-single-parse discipline stays in the `harness = false` mains; the 60+
  `criterion-estimates.json` ingestion references re-path first as TW-S.2 (SPEC §3.4).
- **Lock-15 + Apple-M target-cpu** land at TW0 (sole owner) so every later bench is honest — the
  thin-LTO config is documented as halving twitter throughput; the missing target-cpu is "the
  largest untapped zero-risk throughput lever visible in the profile surface" (research/19 §d).

## §5 — The environmental adjudication

No pass-2 measurement session met the charter's loadavg<2 mandate (all ran 8-25). The adjudication,
binding on every number in this campaign (SPEC.md:19-32): decisions are certified at the
**RATIO** level — paired per-sample interleaving with 95% CIs (A1 D1), same-binary min-of-7 PMU
ratios (A2), same-window cross-harness reproduction (A1's 1.5654 vs report 33's 1.559), and full
byte-equality parity including fingerprint on all three corpora — while every **ABSOLUTE** remains
DIRECTIONAL. The quiet (loadavg<2) re-certification is a **TW0 execution gate**, a scheduled
tranche deliverable, no longer an open research question; no pass-2 decision reopens at TW0 — the
quiet session certifies magnitudes (SPEC §11.1 W0.11). One ratio is explicitly not decision-grade:
the A3 channel comparison (load-confounded); its clean capture also rides W0.11, which must run
before TW-S retires the nightly pin. The truth-floor instrument itself was a
campaign deliverable: `json_canon_bench` (mirror of `css_canon_bench`, N=200, PMU) closed the
committed warm/cold ~3× conflation lacuna (research/21:50-77) and graduated at pass 2 with the
digest-parity assert folded and passing — it and the P5 parity harness are the two instruments
that graduate to `crates/` (SPEC §12).

## §6 — The meta-process verdict

The convergence loop earned its cost by catching what single-pass planning demonstrably missed —
each catch is a decision the frozen plan now carries in the corrected direction:

1. **The canada sign** — the pass-1 prototype reported fused BEATING sonic on canada (0.79×);
   report 33 said the opposite; the pass-2 interleaved adjudication settled the LOSS (1.252× wall)
   and converted an embarrassment into the plan's sharpest scheduled win (TW4.1's Eisel-Lemire
   gate). A single-pass plan would have shipped the wrong headline.
2. **The phantom-architecture P7** — the generalization prototype measured breaks against a fork
   that does not exist at HEAD; the critique round disqualified the measurement, re-located the
   true walls to three SHARED defects, and pass 2 then dissolved both hard walls with runnable
   grammar-neutral primitives. The plan's TOML certification went from per-grammar dread to a
   mechanical wiring gate.
3. **The fold-over-cursor refutation** — the pass-1 spec's own preferred construct (the elegant
   one-substrate claim) was benched at pass 2 exactly as the spec demanded and lost 4×; the spec
   retracted the stronger claim in place. The loop falsified its own synthesis, which is the point
   of the loop.
4. The smaller catches compound: Sampled's +9.3% overturned to a tie under paired CIs; the FLIP-3
   movemask "inconsistency" found already-correct on the live path (a cost-model cell, not a
   sweep item); the smallbox coupling recommendation falsified by an actual dependency-graph walk;
   the width-fix root cause corrected from the plausible file to the real one (view.rs:141 span
   destruction, PROTO-B3); the 0.615/1.63 arithmetic inversion; the dual-floor collapse.

The scores tell the honest shape of it: **67 → 82 → 86 → freeze confirmed** — pass 1 amended
editorially and refused to score its own amendments; pass 2 moved every P-item by measurement (its
largest finding NEGATIVE); pass 3 was verification-only, froze the P-items at their measured
scores, and put the plan-freeze question entirely on the three instruments (94/94/95, two blocking
spec edits found and resolved in place); pass 4 was the confirmation read — both amendments
verified folded exactly against their mandates, zero new contradiction in the amended regions,
100/100 (critiques/CRIT-spec-architecture-v4.md:10-15) — whereupon SPEC.md was written from the
amended draft and the freeze executed. The frozen document also closes the freeze audit's one
least-crisp residual in place: TW0's tail, spine-ordered but unsequenced at the audit, is now
fully ordered as §11.1 W0.0–W0.12 with dependency edges and three hard orderings (W0.0 first;
W0.11 after W0.4 + W0.6; W0.11 before TW-S retires the nightly pin — SPEC.md:648-675). 86 is not
a failed ceiling: the residual 14 points are measurements and builds owned by named waves — TW0
quiet re-certification, TW-S cold parity, TW5 step-0 re-land, TW6 regen wiring + CSV flip — which
no further passing could retire (CONVERGENCE-LEDGER.md:216-222). The freeze criterion — every
remaining blocker wave-owned, no open research or design question — is met, with the lone
frontier uncertainty (CSS SIMD tokenization severity) consciously ring-fenced as a costed TW4
decision inside the measurement loop (CRIT-freeze-audit.md:114-124).

## §7 — The frozen shape

Ten waves, two couplings carried openly (SPEC §11): TW0 (gates, hygiene, the D-G reconcile as
commit W0.0, the quiet truth-floor session as W0.11; internally sequenced at §11.1) → TW-S (stack
abrogation, priced TW-S.1-5) → TW1a
(topology split, relocation-only proof) → TW1b (the one AoS tape + eager-builder retirement, the
substrate and its consumer together) → TW1c (mechanical tail + presize deletion) → TW2 (the one
generator, one regex home, size budgets) → TW4.1 (Eisel-Lemire + the cost-table substrate, before
TW3 closes) → {TW3 the value plane ∥ TW4.2 the decision engine + kernel plane} → TW5 (type
resolution, width-fix step 0 first) → TW6 (TOML as fleet member ten, oracle completion, the CSV
flip). Companion of record for every carried item: DEFERRED-LEDGER.md (this directory) — the final
fold, closing with the empty-chronic-ledger assertion.
