# Tranche BB — Build, Bench, Profiling, and Compile-Time Discipline

BB is the orthogonal engineering tranche. Its job is to make the
repository's default build, bench, and profiling workflows match the
repo's performance-development edicts without mixing parser-architecture
work back into AY, AZ, or BA. BB shortens iteration time, reduces
profiling friction, and makes heavy benches and generated-code costs
intentional rather than accidental.

## Architectural thesis

1. **BB shortens iteration without redefining runtime architecture.**
2. **Workflow truth belongs in commands, profiles, and scripts, not
   just prose.**
3. **Bench and profiling surfaces should be tiered.** Everyday
   iteration, profiling preparation, and final proof should not all pay
   the same cost.
4. **Compile-time structure is a first-class concern.** Generated code,
   monomorphization, and workspace defaults are allowed to change here.

## Invariants

1. No parser-architecture work belongs in BB.
2. Repository defaults must encode the intended workflow.
3. Compile-time and bench improvements close on measured timings.
4. Profiling preparation is amortized and repeatable.
5. Heavy test or bench suites are opt-in unless explicitly declared as
   routine defaults.

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

## Wave summary

| Wave | Spec | Headline | Opens after |
|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | Public fast-path defaults and command-surface repair | tranche open |
| **W1** | [waves/W1.md](waves/W1.md) | Profile split and prepared-binary discipline | W0 |
| **W2** | [waves/W2.md](waves/W2.md) | Bench and test surface separation | W1 |
| **W3** | [waves/W3.md](waves/W3.md) | Generated-code and monomorphization control | W2 |
| **W4** | [waves/W4.md](waves/W4.md) | FINAL and measured iteration close | W3 |

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

## Defensible floor

BB's defensible floor is:

1. Public fast-path defaults that match the intended development loop.
2. Separated routine, profiling, and final-proof profiles.
3. Bench/test separation that removes accidental heavy costs from
   default correctness runs.
4. A measurable reduction in cold and warm iteration time.

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

When BB closes correctly, performance work in AY, AZ, and BA no longer
waits on avoidable build, bench, or profiling drag; the command surface
itself enforces the intended workflow.
