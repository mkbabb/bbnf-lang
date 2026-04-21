# Tranche BB — Post-BA Toolchain and Compile-Time Discipline

BB is the post-BA engineering tranche. B0 extracted the narrow pre-AY
runway required to execute `AY.W5-W7`; BB therefore no longer blocks
parity work. BA then takes the direct performance slot after AY. BB's
job is to reduce the repository's remaining compile-time, cache-key,
generated-code, and workflow drag after BA
closes, without mixing parser architecture back into AY, BC, or BA.

## Architectural thesis

1. **BB starts after BA closes.** It is not a precondition for AY or
   BA performance work.
2. **BB shortens iteration without redefining runtime architecture.**
3. **Workflow truth belongs in commands, profiles, and scripts, not
   just prose.**
4. **Bench and profiling surfaces should be tiered.** Everyday
   iteration, profiling preparation, and final proof should not all pay
   the same cost.
5. **Compile-time structure is a first-class concern.** Generated code,
   monomorphization, and workspace defaults are allowed to change here.

## Invariants

1. No parser-architecture work belongs in BB.
2. Repository defaults must encode the intended workflow.
3. Compile-time and bench improvements close on measured timings.
4. Profiling preparation is amortized and repeatable.
5. Heavy test or bench suites are opt-in unless explicitly declared as
   routine defaults.
6. Anything that directly blocked AY cadence belonged in B0, not here.

## Operational posture

1. BB measures cold and warm paths separately at every wave boundary.
2. Every command-surface change ships with docs, script, or profile
   updates that make it the default workflow rather than a suggestion.
3. Bench and profiling harnesses are separated from routine correctness
   loops whenever their costs differ materially.
4. Generated-code and monomorphization changes are evaluated for both
   time and memory impact.
5. BB does not claim success on "fewer steps"; it claims success on
   lower wall-clock and RSS costs.
6. BB does not reopen tranche-boundary hedging. If a runtime change is
   required to hit a parser-performance gate, it does not belong here.

## Wave summary

| Wave | Spec | Headline | Opens after | Status |
|---|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | Generated-code and monomorphization control | BA close | planned |
| **W1** | [waves/W1.md](waves/W1.md) | Cache-key, invalidation, and workspace partition discipline | W0 | planned |
| **W2** | [waves/W2.md](waves/W2.md) | Command, CI, and automation consolidation | W1 | planned |
| **W3** | [waves/W3.md](waves/W3.md) | Profiling fleet and measured compile/workflow proof | W2 | planned |
| **W4** | [waves/W4.md](waves/W4.md) | FINAL and measured iteration close | W3 | planned |

## BB handoff contract

BB does not close until all of the following are true:

1. The default command surface exposes a fast iteration path, a
   profiling-prep path, and a final-proof path distinctly.
2. Routine correctness loops no longer pay heavy bench/profiling costs
   by default.
3. Compile-time and profiling-prep improvements are backed by measured
   cold/warm timing and memory artefacts.
4. Generated-code or monomorphization reductions do not silently hide
   parser regressions.
5. BB does not smuggle any AY parity-critical runtime debt back into
   tooling scope.

## Defensible floor

BB's defensible floor is:

1. Measurable reduction in cold and warm compile/iteration time.
2. Tighter cache-key and workspace-partition discipline.
3. A reusable profiling and proof fleet that does not depend on ad hoc
   command invention.
4. Post-BA command/CI/workflow consolidation that preserves the
   intended heavy-proof separation.

Anything less leaves the repository slower than its own edicts demand.

## Post-tranche review candidates

Decision at W4 close, not mid-wave:

- Whether additional crate partitioning is warranted after generated
  code and monomorphization work land.
- Whether any profiling-prep scripts should be promoted into CI or
  automation.
- Whether any remaining slow paths are truly structural or simply
  underused command defaults.

## Indefatigability

When BB closes correctly, later work in BC and any successor
performance/tooling tranches no
longer waits on avoidable compile, cache, command, or profiling drag;
the command surface itself enforces the intended workflow.
