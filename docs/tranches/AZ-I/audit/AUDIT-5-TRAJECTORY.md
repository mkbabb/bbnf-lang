# AZ-I Audit 5 — Trajectory Critique + Simplification

Audit Agent 5 of 6, parallel post-AZ-I.W2-substrate close.
Lens: architectural critique of the remaining trajectory + collapse
proposals. The substrate from W2 (nine per-shape struct-direct
emitters, `EmitStrategy` resolver, `<'p>` lifetime threading, fully
qualified trait calls) is grammar-general; the declared trajectory
re-spends that substrate across W2-act, W2.B, W3, AZ-II.W0, AZ-II.W1,
AZ-II.W2, AZ-II.W3, BA.W-1, BA.W0–W3, BB.W0–W4 — twelve waves to
walk a substrate that took one wave to land.

## 1. Inventory of remaining waves (AZ-I.W2-act through BB.W4)

| Wave | Declared scope | Hard gate | Wall | Actual under contact |
|---|---|---|---|---|
| AZ-I.W2-act | JSON view/value API + parity recode + bench | twitter ≥ 1967, parity green | 30 min | call-site recoding; W2 close ceremony delayed |
| AZ-I.W2.B | Sheets struct-direct | parse_simple ≥ 95 | 60 min | `for_grammar` adds one match arm |
| AZ-I.W3 | CSS L4 + typed values (Length, Color, etc.) | normalize ≥ 735, lightningcss parity | 2-4 h | typed enums; resolver flip mirrors W2.B |
| AZ-I.W4 FINAL | Matrix re-run + grep + handoff + FINAL.md | matrix at AU; FINAL.md lands | 30-60 min | ceremonial; no runtime change |
| AZ-II.W0 | BOOTSTRAP-CUTOVER.md + classifier + audit BBNF + baseline bench + preflight | doc lands; preflight captured | 60-90 min | research + audit-pass extension |
| AZ-II.W1 | Stage A/B atomic byte-equal cutover | diff zero; matrix at AZ-I close | 3-6 h | applies W2 substrate to BBNF + reproducibility harness |
| AZ-II.W2 FINAL | tape deletion + view/pprint/@debug rewire + FINAL.md | crates/tape/ gone; FINAL.md lands | 2-3 h | mechanical deletion + parallel rewire |
| BA.W-1 | Opening verification | command packet green | 15-30 min | grep + cargo-test |
| BA.W0 | Path IR + type checker + parent-pointer micro-bench | TypedPath checks; strategy picked | 3-4 h | substrate + 30-min bench addendum |
| BA.W1 | Lazy traversal + `path!` macro | zero alloc; ≥ 20% sonic-rs win | 3-4 h | proc-macro + executor |
| BA.W2 | TS + Python isomorphism | path_isomorphic green | 3-4 h | binding sub-crates |
| BA.W3 FINAL | Matrix + parity + BB handoff | FINAL.md lands | 30-60 min | ceremonial |
| BB.W0 | Enumerator + VM oracle + ranker + scaffold + Tranche H rediscovery | rediscovery ≥ 80% | 4-6 h | new substrate |
| BB.W1 | JSON + Sheets enumeration | 20 candidates validated | 3-4 h | enumeration + curation |
| BB.W2 | CSS L4 + BBNF wide alphabet | 50 candidates validated | 3-4 h | enumeration |
| BB.W3 | Grammar-specific rules + rewrites/*.ron | per-grammar rules persist | 2-3 h | curation |
| BB.W4 FINAL | Cost-model + CI auto-accept | FINAL.md lands | 1-2 h | ceremonial |

**Total declared count: 17 waves.** Two FINAL ceremonial caps within
AZ (W4, W2), one FINAL each in BA + BB. The trajectory walks the
same per-grammar resolver flip three times in AZ-I (W2-act, W2.B,
W3) and once again in AZ-II.W1 (BBNF). The flip is `for_grammar`
adding one match arm + same-commit regen + parity harness recode +
bench gate verification. The substrate for the flip exists at master
HEAD `0321c53a`.

## 2. Architectural fold-in candidates

**(2.1) W2-act + W2.B + W3 share the W2 substrate.** Nine per-shape
emitters (Object, Array, Number, String, Scalar, Keyword, Wrap,
AltDispatch, Flat) gated through `EmitStrategy::for_grammar`; the
resolver match-arm is the only per-grammar variation. CSS L4 typed-
value enums are emit-target detail covered by W1.A's `LayoutKind`
discriminator (`Struct` / `TaggedEnum` / `UntaggedEnum` / `NewtypeWrapper`).
Three waves × per-grammar dispatch + parity harness recode + bench
gate = N matchings, not N waves.

**(2.2) W4 FINAL is ceremonial.** Matrix re-run + samply + parity
+ grep + handoff verification + FINAL.md. AZ-II.W0's baseline-capture
already wants the same matrix; the grep + handoff verification IS
the AZ-II.W0 executable preflight (§AZ-II.W0.5).

**(2.3) AZ-II.W0 + W1 + W2 collapse on substrate reuse.** W1 reapplies
the W2 substrate to BBNF; W0 is research-only (cutover doc + audit
extension); W2 is mechanical deletion + parallelisable downstream
rewire. None of the three waves blocks on a wave-boundary checkpoint
— the byte-equal diff is the one within-wave sequential gate.

**(2.4) BA.W-1 + BA.W0 are one wave.** BA.W-1's grep + cargo-test
packet is a 15-min preflight; BA.W0's parent-pointer micro-bench is
a 30-min addendum to the path IR + type checker substrate. Splitting
verification + bench from substrate across two wave letters is
ceremonial.

**(2.5) BB.W0 opens in parallel with AZ-II.** REMAINING-TRAJECTORY.md
§6 declares BB rules operate over substrate-independent `IrNode` and
BB can run in parallel; the declared trajectory schedules BB
serially anyway. Parallel dispatch saves the entire BB.W0 wall behind
the AZ-II close ceremony.

## 3. Specific simplification proposals

### Proposal A — collapse W2-act + W2.B + W3 into AZ-I.W2-activate

- **Before**: three sequential waves; ~6-8 h; each flips
  `for_grammar` for one grammar, each authors its own parity recode,
  each runs its own bench gate.
- **After**: one wave; three parallel agents on disjoint grammars
  (JSON / Sheets / CSS L4), each owning resolver flip + view/value
  API + parity recode + per-grammar bench; orchestrator close runs
  the full matrix; ~3-4 h via parallelism. The W2 substrate already
  shares per-shape emitters across all three grammars; disjoint file
  bounds guarantee non-collision.
- **Drag eliminated**: two wave-boundary handoffs + two redundant
  matrix captures + two FINAL-dispatch round-trips.

### Proposal B — fold W4 FINAL into W2-activate close + AZ-II.W0 baseline

- **Before**: dedicated W4 wave; ~30-60 min ceremony; matrix +
  samply + parity + grep + handoff + FINAL.md.
- **After**: W2-activate's own close ceremony runs the matrix three
  times (cold, per `feedback_no-warm-benches`) + parity rerun +
  grep. AZ-II.W0's baseline-capture sub-item ingests the same
  matrix into `docs/benchmarks/archive/AZ-II/W0/baseline.json`. AZ-I
  FINAL.md authored as a parallel sub-agent within the W2-activate
  close.
- **Drag eliminated**: one wave dispatch + one redundant matrix
  capture.

### Proposal C — collapse AZ-II.W0 + W1 + W2 into AZ-II.cutover

- **Before**: research wave (W0) + atomic Stage A/B wave (W1) +
  deletion + rewire + FINAL (W2); three waves; ~6-10 h.
- **After**: one wave "AZ-II.cutover" with the following internal
  structure: parallel research sub-agent authors BOOTSTRAP-CUTOVER.md
  while substrate sub-agents run `project_types` BBNF extension +
  `--emit-mode struct` + Stage A capture; sequential Stage B
  self-rebuild + byte-equal diff (the only forced sequential gate);
  parallel deletion + view/pprint/@debug rewire post-byte-equal;
  permanent CI test + AZ-II FINAL.md at the close commit. ~6-8 h
  via parallel substrate dispatch.
- **Drag eliminated**: two wave-boundary handoffs + two bench
  recaptures + two FINAL ceremonies. The "Stage A produced but
  Stage B not verified" unworkability window (per AZ-II.md §Thesis)
  is the wave's interior, exactly where it belongs.

### Proposal D — fold BA.W-1 into BA.W0 opening preflight

- **Before**: BA.W-1 verification wave + BA.W0 substrate wave; two
  waves; ~30 min + 3-4 h.
- **After**: BA.W0 opens with the verification packet as the first
  ~15 min; halt-and-replan if the packet fails. Parent-pointer
  micro-bench lands inside BA.W0's existing parent-pointer scope.
  ~3-4 h wall.
- **Drag eliminated**: one wave dispatch + a redundant ceremony.

### Proposal E — open BB.W0 in parallel with AZ-II.cutover

- **Before**: BB.W0 declared as parallelisable but scheduled
  serially after AZ-II close. ~4-6 h serialised wall.
- **After**: BB.W0 dispatches on the AZ-II.cutover orchestrator
  boundary as a parallel thread; cross-thread interference is
  contained to `crates/egraph/` + `crates/ir/src/rewrites/`, which
  AZ-II does not touch. BB.W1+W2 sequence after BB.W0.
- **Drag eliminated**: 4-6 h serialised behind AZ-II close.

## 4. Defer-removal proposals

The AZ-I corpus carries the following "DEFERRED" markers + their
proposed dispositions:

| Marker | Source | Disposition |
|---|---|---|
| W0 hard-gate 4 — workspace ≥ 1480 → DEFERRED-TO-W1-BOUNDARY | PROGRESS.md 2026-04-27 W0 close | **Retire**: W1's pre-flight already verifies workspace; the W0 deferral is satisfied by the W1.A close ledger (1517/1517 green per PROGRESS.md). Mark as resolved, not deferred. |
| W1 hard-gate 4 — AU-baseline regression on 17-entry matrix → DEFERRED-TO-W4 | PROGRESS.md 2026-04-27 W1 close | **Land in W2-activate close**: per Proposal A+B, the matrix re-run is already inside W2-activate's close ceremony. Drop the W4 deferral. |
| W2 hard-gates 1, 2, 3 (struct-only emission, bench gate, parity harnesses) → DEFERRED-TO-W2-ACT | PROGRESS.md 2026-04-27 W2 close | **Land in W2-activate** (Proposal A). The hard-gate text already specifies the activation work; folding W2-act into W2-activate is implementation, not deferral. |
| AY-III gates → "absorbed into AZ-I.W4 + AZ-II.W2" | REMAINING-TRAJECTORY.md §1 + AZ-I.md §AZ-I.W4 absorbs durable AY-III gates | **Land in W2-activate close + AZ-II.cutover close**: the admission-totality test + grammar-general fused-pipeline contract are gates that ride the existing close ceremonies; no separate wave lift required. |
| Tape-substrate prune candidates (PHF, DTA precedence, value frames, bloom dedup, cursor lookahead, Stage 1 index, visitor re-exports) — "absorbed by AZ-II.W2" per REMAINING-TRAJECTORY.md §9 | REMAINING-TRAJECTORY.md §9.1–9.7 | **Land in AZ-II.cutover deletion** (Proposal C). The prune candidates retire alongside `crates/tape/` whole-crate deletion; the inventory is pre-built and the W2 deletion sub-agent consumes it. |
| BA.W0 parent-pointer decision deferred from AZ-I (per AZ-I.md §Open questions Q1) | AZ-I.md §1 Q1 disposition | **Fold into BA.W0** (Proposal D). The decision is BA-owned; AZ-I propagates the struct tree without committing. Already aligned. |

**Net result of defer-removal**: zero perpetual deferrals.
Every "DEFERRED" marker either retires immediately (W0 gate 4
already satisfied) or lands inside a collapsed wave (W1 gate 4 in
W2-activate; W2 gates in W2-activate; AY-III gates in W2-activate
+ AZ-II.cutover; prune candidates in AZ-II.cutover; BA.W0 question
in BA.W0). Per `feedback_no-deferrals`, no future-tranche carry.

## 5. Critical-path analysis

From master HEAD `0321c53a` to AZ-II close, the actual architectural
transpositions are:

1. Per-grammar `for_grammar` resolver flip × 3 (JSON / Sheets / CSS L4)
   + same-commit regen + call-site recoding + parity-harness recoding.
2. CSS L4 typed-value enums (`Length`, `Color`, `Dimension`, `Time`,
   `Resolution`, `Percentage`, `Angle`) + lightningcss conversion
   impls.
3. BBNF struct-emission mode (`cargo xtask regen --emit-mode struct`)
   + Stage A capture.
4. Stage B self-rebuild + byte-equal diff (only forced-sequential gate).
5. `crates/tape/` whole-crate deletion + view/pprint/@debug rewire.
6. Permanent CI test (`bbnf_bootstrap_reproducibility.rs`).

Six transpositions; four (1, 2, 3, 5) parallelisable per grammar or
per consumer; two sequential checkpoints (Stage A/B diff;
deletion-after-byte-equal-green). The declared trajectory
schedules these across nine waves (W2-act, W2.B, W3, W4, AZ-II.W0,
AZ-II.W1, AZ-II.W2). Strip ceremony and the critical path is **two
waves** for AZ-I + AZ-II combined.

## 6. The new path

| Wave | Scope | Hard gate | Wall | Depends on |
|---|---|---|---|---|
| **AZ-I.W2-activate** | Parallel JSON / Sheets / CSS L4 struct-direct activation: resolver flip + view/value API + parity harness recode + per-grammar bench gate. CSS L4 typed-value enums materialised. Three-grammar matrix at AU floor on struct-only path. AZ-I FINAL.md authored as orchestrator-close artefact. AY-III admission-totality test parameterised across the three grammars lands as same-commit harness. | three-grammar matrix at AU floor; parity harnesses green; tape-scoped-to-BBNF grep zero on JSON / Sheets / CSS runtime paths; FINAL.md committed. | ~4-6 h (three-way parallel) | master `0321c53a` (W2 substrate close) |
| **AZ-II.cutover** | Parallel: BOOTSTRAP-CUTOVER.md + `project_types` extension to BBNF + xtask `--emit-mode struct` + Stage A capture. Sequential: Stage B self-rebuild + byte-equal diff. Parallel: `crates/tape/` deletion + view/pprint/@debug rewire + tape-prune-candidates (per REMAINING-TRAJECTORY.md §9 inventory) retired. Permanent CI test + AZ-II FINAL.md at close commit. | byte-equal diff zero across BBNF corpus; `crates/tape/` does not exist; `cargo build -p bbnf --no-default-features` green; permanent CI test + matrix at AU; FINAL.md committed. | ~6-10 h (sequential byte-equal core, parallel substrate + deletion) | AZ-I.W2-activate |
| **BA.W0-bootstrap** | Path IR + type checker + parent-pointer micro-bench + opening verification packet (folded BA.W-1). | TypedPath compile-time checks; ascent strategy default picked from micro-bench; opening grep + cargo test green. | ~3-4 h | AZ-II.cutover |
| **BA.W1-engine** | Lazy traversal engine + `path!` proc-macro + per-grammar bench. | zero heap allocations (dhat); ≥ 20% sonic-rs win on 3-field citm; parity-or-better on 30-field. | ~3-4 h | BA.W0-bootstrap |
| **BA.W2-isomorphic** | TS template-literal + Python callable bindings + isomorphism test + FINAL.md. | path_isomorphic test green across Rust / TS / Python; FINAL.md committed. | ~3-4 h | BA.W1-engine |
| **BB.W0-scaffold** | Enumerator + e-graph residue split + VM oracle + ranker + `crates/ir/src/rewrites/` scaffold + Tranche H rediscovery + corpus-wide hit-rate measurement. **Opens in parallel with AZ-II.cutover** per Proposal E. | rediscovery ≥ 80%; ≥ 0.1 firings/parse measurement live; scaffold compiles. | ~4-6 h | AZ-I.W2-activate (parallel with AZ-II.cutover) |
| **BB.W1-enumerate** | JSON + Sheets + CSS L4 + BBNF first enumeration runs (folded W1+W2); curated Class-1/2 batch; rejection rate gate. | ≥ 50 candidates validated across the four grammars; rejection ≤ 50%; one grammar's generated.rs shrinks ≥ 10 LOC. | ~4-6 h | BB.W0-scaffold |
| **BB.W2-grammar-rules** | Grammar-specific rule discovery + per-grammar `rewrites/*.ron` authoring + cost-model integration + CI auto-accept + FINAL.md. | ≥ 90% Class-1/2 across accepted rules; ≥ 1 grammar measurable throughput gain; FINAL.md committed. | ~3-4 h | BB.W1-enumerate |

**New schedule wave count: 8 waves.**
**Declared schedule wave count: 17 waves.**
**Wave-count reduction: 53% (17 → 8).**

Critical path through the new schedule:

```
0321c53a → AZ-I.W2-activate ─┬─ AZ-II.cutover ─ BA.W0-bootstrap ─ BA.W1-engine ─ BA.W2-isomorphic
                              │
                              └─ BB.W0-scaffold ─ BB.W1-enumerate ─ BB.W2-grammar-rules
```

AZ-I.W2-activate is the convergence point; AZ-II.cutover and
BB.W0-scaffold open in parallel from there. BA waves sequence
after AZ-II.cutover. BB.W1+W2 sequence after BB.W0-scaffold. Total
declared wall: ~25-40 h critical-path; the trajectory's 17-wave
declared wall sums to ~50-70 h plus ceremonial overhead.

## 7. Hand-off to synthesis

W2-CLOSE-AUDIT.md should lift directly:

- **§3 Proposal A (W2-activate collapse)** — load-bearing
  simplification; the W2 substrate is grammar-general, three-way
  parallel activation is N matchings, not N waves.
- **§3 Proposal C (AZ-II.cutover collapse)** — applies the same
  pattern to BBNF + folds deletion into the same pass.
- **§3 Proposal E (BB.W0 parallel)** — exploits substrate
  independence the trajectory declares but schedules serially.
- **§4 defer-removal table** — every "DEFERRED" marker has an
  immediate landing site.
- **§6 new path** — eight waves; 53% reduction.

The collapse softens no architectural target. Every hard gate in
the declared trajectory survives in the new schedule, attached to a
smaller number of waves. Per `feedback_no-deferrals`,
`feedback_no-workarounds`, `feedback_no-orthogonal-codepaths`, the
simplification is fold-in, not omission.
