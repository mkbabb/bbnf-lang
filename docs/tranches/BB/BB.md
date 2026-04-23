# Tranche BC — E-graph Rewrite Rule Inference

BC closes the loop on `feedback_pluggable-components` and
`feedback_csp-always-optimize` by letting the e-graph *discover*
grammar-level rewrite rules rather than only apply a fixed rule set.
Ruler-style CVC enumeration over `IrNode` produces candidate rewrite
rules; the bbnf IR interpreter (salvaged from the DTA/PSI era!) acts
as the equivalence oracle; CSP schedules candidate rule application
against the existing cost model. Accepted rules persist into
`cost_config` and are consumed by subsequent codegen runs.

BC is the tranche where the long DTA/PSI arc finally earns its keep.
The VM interpreter still compiles at HEAD (`crates/ir/src/vm/`); the
token-dispatch opcode that the VM needed for runtime interpretation
now becomes the equivalence oracle for BC's enumeration, and the
AOT path retains the dispatch-table machinery as a proven substrate.
~572 tranche-tagged commits of Era V do not come back, but the
interpreter's surviving core is what makes BC viable at all.

## Architectural thesis

1. **Rule inference over `IrNode` is an e-graph enumeration
   problem.** Ruler (Nandi et al. 2021) demonstrates the CVC-style
   approach: generate candidate terms over the grammar's alphabet
   up to a bounded size, group them by equivalence under an
   oracle, extract rules as cross-class equivalences. BC applies
   the same pattern to bbnf's `IrNode` shape.
2. **The bbnf IR interpreter is the equivalence oracle.** Two
   `IrNode` candidates are equivalent if running both through the
   interpreter against a corpus of input fixtures produces
   identical tape output. The interpreter exists (salvaged from
   Era V) and is already referenced as "reference semantics" in
   the SOTA synthesis. BC makes this role explicit: the
   interpreter is BC's oracle, not a dead artefact.
3. **CSP schedules rule application.** The existing CSP solver
   (`bbnf-csp-solver`) chooses which candidate rules to apply at
   which cost, reusing the same cost variables that govern regex
   engine choice, emission tier, and wrap mode. No new decision
   surface, per `feedback_no-orthogonal-codepaths`.
4. **Rules are grammar-derived, not hand-coded.** The factor,
   merge_regex_alts, inline_acyclic rewrites that landed at
   Tranche H were hand-coded. BC does not hand-code; BC discovers.
   Every rule that survives to `cost_config` was produced by
   enumeration and validated by the oracle.
5. **Rules are source-level transparent.** Every persisted rule
   has a human-readable `Debug` form and a round-trip to bbnf
   surface syntax. A reviewer can read the rule, understand the
   equivalence, and either accept or reject it manually before
   it lands in `cost_config`. Enumeration is automated; curation
   is not.

## Salvage from Era V — the VM as oracle

The VM / bytecode interpreter arc (2026-03-15 → 2026-04-12) is the
hardest-earned substrate in project history. Its deletion at
AX.W0b (`bc550d2c`, `a206b962`, ~78K LOC reclaim) removed the
DTA walker, the `dta_walker/`, `emitter/dta.rs`, and 8 DTA-coupled
test suites. But the core that remains — `crates/ir/src/vm/` +
token-dispatch opcode machinery — compiles at HEAD and is directly
usable as:

1. **An equivalence oracle.** Given two `IrNode` candidates and a
   corpus, run both through the VM and compare tape output. This
   is exactly what Ruler requires.
2. **A cost-model reference.** The VM's opcode count per rule is
   a proxy for the cost the AOT path would pay if inlined. BC
   uses this for cost-model calibration.
3. **A regression oracle.** After rule inference lands a
   transformation, the VM runs the pre-rule and post-rule forms
   on the same fixtures to verify semantic identity.

This is the role the synthesis doc anticipated ("VM continues to
serve as a reference semantics oracle for regression tests") made
into a first-class BC consumer. BC does not resurrect DTA walker
dispatch or the shape emitter thesis from AW-V; BC uses only the
narrow VM surface that survives at HEAD. `feedback_abrogate-before-patch`
applies: BC does not re-open the walker; BC uses what remains.

## Dependencies

BC depends on BA close + AY-II close. BB is NOT a BC blocker —
rule inference operates over `IrNode`, not over pointer-path
output, so BB can land in parallel with or after BC.

**BA close dependencies:**

- `project_types` IR pass populates `StructRegistry` for every
  grammar. BC's enumeration uses the registry to type-check
  candidate rules.
- Every `->` reaches the tape emitter. BC's oracle relies on
  tape output being the canonical comparison point.

**AY-II close dependencies:**

- Visitor-lane default `to_value()` with unified compound
  emission API. BC's oracle runs against this substrate.
- `Columns::rollback_to` as the rollback primitive. BC's
  enumeration may occasionally need to roll back speculative
  tape state during oracle evaluation.

## Invariants

1. No rule lands in `cost_config` without oracle validation on
   the full fixture corpus.
2. No hand-coded rules added at BC. The existing factor /
   merge_regex_alts / inline_acyclic rules are preserved; BC
   adds only enumerated rules.
3. The enumeration is bounded: candidate term size ≤ N (declared
   per wave), oracle corpus size ≤ M. BC does not ship an
   unbounded search.
4. Inferred rules do not change semantic behaviour for any
   existing fixture. Parity is non-negotiable.
5. Every accepted rule reduces at least one cost metric on at
   least one grammar. Rules that don't buy anything are not
   accepted.

## Operational posture

1. Every wave ships a runtime call site. The enumeration pass
   runs in the codegen pipeline with a bounded budget; the cost
   report is emitted to `docs/benchmarks/BC/<wave>/cost-deltas.json`.
2. The 17-entry AU-baseline matrix runs on every wave boundary.
   Inferred rules must not regress full-parse throughput.
3. Rule curation is a documented step. Each inferred rule
   enters a review queue; a reviewer either accepts or rejects
   before it reaches `cost_config`. Rejected rules are recorded
   under `docs/tranches/BC/rejected-rules/` with a rationale.
4. The VM oracle runs under a time budget per candidate; oracle
   timeouts mark a rule as "inconclusive" rather than
   "equivalent".
5. Samply profiles land under `docs/benchmarks/profiles/BC/<wave>/`
   before and after.

## Hard gates

**Rule-inference gates:**

- Enumeration produces ≥ N_w candidate rules per wave, where
  N_w is declared at wave open (W1 = 20, W2 = 50, W3 = 100).
- Oracle validation rejects ≤ 50% of candidates at each wave;
  higher rejection indicates the enumeration alphabet needs
  narrowing.
- At least 5 rules per grammar accepted into `cost_config`
  across the tranche.

**Cost gates:**

- After applying accepted rules, at least one grammar's
  generated codegen shrinks by ≥ 10 LOC.
- At least one grammar shows a measurable throughput gain on
  the 17-entry bench matrix.
- No grammar regresses on the 17-entry matrix.

**Parity gates:**

- lightningcss / sonic-rs / simdjson parity harnesses green
  after every accepted rule.
- Workspace: pass ≥ BA close pass count, fail ≤ BA close fail
  count.

## Risk register

| Risk | Mitigation |
|---|---|
| **Egraph explosion** — candidate enumeration blows past the bounded budget and consumes GB of memory. | Per-wave size bound N_w; fail-fast if `EGraphSolver::node_count()` exceeds declared ceiling. Revert the wave's enumeration alphabet. |
| **Rule validity drift** — a rule is equivalent on the fixture corpus but semantically wrong on an unseen input. | Accept rules only with oracle coverage ≥ 95% of fixture bytes; mark others as "narrow" and restrict their application to matching grammar positions. |
| **Interaction with AY-II fusion** — a rule rewrites a fused form into an unfused form, regressing throughput. | The CSP cost model is the gate: a rule that increases CSP-assigned cost is rejected regardless of oracle equivalence. |
| **Oracle timeout storms** — enumeration produces many candidates that the VM cannot validate in budget. | Per-candidate timeout produces "inconclusive" marker; the inconclusive queue is reviewed at wave close, not shipped. |
| **Rule accumulation entropy** — hundreds of accepted rules slow compilation. | Hard cap on total accepted rules per grammar (initial = 25 per grammar); a new rule that crosses the cap must displace an existing one based on measured cost delta. |

## Reversal criteria

Inheriting BA's discipline:

1. **Wave-local 20% rule.** A wave whose accepted rules don't
   produce the declared cost delta by > 20% of target reverts
   its rule batch.
2. **No regression on BA / AY-II close.** Any regression of the
   17-entry matrix reverts the responsible rule batch
   immediately.
3. **No hedging forward.** A wave does not route its miss to a
   later wave of BC.
4. **Egraph explosion triggers reversal.** Crossing the per-wave
   node-count ceiling reverts the enumeration alphabet at wave
   close; the alphabet narrows before the next wave opens.

## Wave structure

Four waves. Every wave has a runtime call site at its landing
commit.

| Wave | Spec | Headline | Opens after | Status |
|---|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | Ruler-style enumerator scaffold + VM oracle harness | BA + AY-II close | planned |
| **W1** | [waves/W1.md](waves/W1.md) | First rule batch — JSON (narrow alphabet) | W0 | planned |
| **W2** | [waves/W2.md](waves/W2.md) | Wide alphabet — CSS + Sheets + BBNF | W1 | planned |
| **W3** | [waves/W3.md](waves/W3.md) | FINAL — rule persistence, cost-config integration, review ledger | W2 | planned |

### W0 — Enumerator + oracle harness

The Ruler-style CVC enumerator lives at
`crates/egraph/src/ruler/enumerate.rs`. The VM oracle wrapper at
`crates/egraph/src/ruler/oracle.rs` invokes the VM via
`crates/ir/src/vm/` with a time and memory budget per candidate.
No rule lands in `cost_config` yet; this wave proves the
enumeration and validation pipeline on a small alphabet.

Runtime call site: `cargo run --bin bbnf-egraph-enumerate -- --grammar json --budget N` emits a JSON list of candidate rules with per-candidate oracle status. At least 10 candidates must pass the oracle on JSON for wave close.

### W1 — First rule batch (JSON, narrow)

Narrow alphabet: literal pattern fusion, repeat-unroll, Alt
flattening when bounded. Target N_1 = 20 candidates, at least 5
accepted into `cost_config`. Acceptance triggers `generated.rs`
re-emission; the post-rule JSON generated.rs must differ from the
pre-rule form on at least one rule.

Runtime call site: the bench matrix runs pre-rule and post-rule;
the delta is recorded under
`docs/benchmarks/BC/W1/cost-deltas.json`. At least one accepted
rule must produce a measurable throughput gain on one JSON fixture.

### W2 — Wide alphabet (CSS, Sheets, BBNF)

The enumerator opens to CSS L4 declaration forms, Sheets cell
rules, and BBNF self-hosting rules. N_2 = 50 candidates per
grammar. At least 5 accepted per grammar.

Runtime call site: cross-grammar codegen shrink measurement. At
least one grammar's `generated.rs` shrinks by ≥ 10 LOC after
rule application.

Bench delta gate: no regression on any 17-entry matrix entry;
at least two grammars show measurable throughput gains.

### W3 — FINAL

Rule persistence layer: accepted rules land in a
`docs/tranches/BC/rules/<grammar>/*.rule` form that regenerates
deterministically (`feedback_clean-regen-discipline`). The review
ledger captures every accepted and rejected rule with rationale.
`FINAL.md` records the accepted-rule count per grammar, the
codegen shrink, the bench deltas, and any enumeration-alphabet
reversals taken mid-tranche.

## External SOTA grounding

- **egg — equality saturation with e-graphs.** The substrate bbnf
  uses today (`crates/egraph/`). See
  [egg home](https://egraphs-good.github.io/) and the
  [egg SIGPLAN blog](https://blog.sigplan.org/2021/04/06/equality-saturation-with-egg/).
- **Ruler — rewrite rule inference.** The technique BC applies.
  CVC-style enumeration up to bounded term size, pairwise
  equivalence checks under an oracle, cost-ranked rule extraction.
  See [Rewrite Rule Inference (Nandi et al. 2021)](https://arxiv.org/pdf/2108.10436).
- **Enumo — follow-on tooling for rule inference.** Newer
  infrastructure from the same line of research, with support
  for conditional rules and domain-specific rulers. See
  [Enumo paper](https://dl.acm.org/doi/10.1145/3591283).

## BC handoff contract

BC does not close until all of the following are true:

1. At least 5 rules per production grammar accepted into
   `cost_config` via oracle-validated enumeration.
2. At least one grammar's `generated.rs` shrinks by ≥ 10 LOC.
3. 17-entry AU-baseline matrix at or above BA close.
4. Parity harnesses green after every accepted rule.
5. Review ledger complete: every accepted and rejected rule has
   a human-readable rationale.
6. `FINAL.md` records deltas, reversals, and any follow-on work.

## Defensible floor

1. Working Ruler-style enumerator + VM oracle pipeline.
2. ≥ 5 accepted rules per grammar with oracle validation.
3. Measurable codegen shrink on at least one grammar.
4. No regression on BA close bench matrix.
5. Parity green throughout.

Anything less is rule inference without the "discover, not
declare" payoff that motivates BC.

## Post-tranche review candidates

Decision at W3 close, not mid-wave:

- Whether the enumerator should open further alphabets
  (conditional rules, cross-rule fusion) in a successor tranche.
- Whether the VM oracle should remain ~80 LOC (its current
  narrowed form) or expand for richer semantics.
- Whether BB's pointer-path egraph normalization should absorb
  inferred rules or remain a closed surface.
- Whether the review ledger should graduate into a standing
  human-in-the-loop curation CI job.

## Indefatigability

When BC closes correctly, bbnf's optimiser discovers rules as
first-class output, the DTA/PSI era's VM interpreter serves as
permanent semantic oracle for rule inference, and the cost model
is no longer a hand-curated accumulator but a living record of
what the enumerator has proven equivalent. The loop closes on the
DTA/PSI era without rebuilding any of the walker machinery that
Era V could not make pay.
