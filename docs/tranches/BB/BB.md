# Tranche BB — Build, Bench, Profiling, and Compile-Time Discipline

BB is the orthogonal engineering tranche.

It exists because compile-time, bench harness, profiling workflow,
generated-code partitioning, and test/build defaults are real problems,
but they are not parser-architecture work and they should not be mixed
into AY, AZ, or BA.

## Architectural thesis

1. **BB shortens iteration without redefining runtime architecture.**
2. **BB makes the repo defaults match the repo edicts.**
3. **BB is allowed to change code generation structure, crate
   boundaries, profiles, scripts, and workflows, but not the semantic
   parser architecture itself unless needed for iteration cost.**

## Scope

1. **B0 — Public fast path defaults.** Make `ax-iter`, tiered tests,
   scoped benches, and prepared-profile workflows the default command
   surface rather than prose-only guidance.

2. **B1 — Profile split and bench discipline.** Separate routine perf,
   samply, and final-proof profiles; stop paying fat-LTO/single-CGU
   costs for everyday iteration.

3. **B2 — Bench/test surface separation.** Move perf loops out of
   default `tests/`, batch prebuilds, and require prepared binaries for
   profiling waves.

4. **B3 — Generated-code and monomorphization control.** Partition large
   generated outputs where appropriate, reduce pathological expansion
   surfaces, and de-genericize/contain heavy monomorphization clusters.

5. **B4 — FINAL.** Close on measured iteration improvements.

## Invariants

1. **No runtime-architecture leakage.** Runtime substrate work belongs
   in AY/BA/AZ, not BB.
2. **Workflow truth.** The command surface must encode the intended
   workflow; docs alone are insufficient.
3. **Measured close.** BB closes on cold/warm iteration time,
   compile-time memory, and profiling-prep cost, not on script count.

## Closing direction

BB succeeds when the repository can support AY/AZ/BA work with sane
iteration costs:

- warm scoped iteration in tens of seconds,
- cold scoped iteration in minutes rather than tens of minutes,
- profiling waves that truly prepare once and profile many,
- generated-code and bench/test structure that no longer punishes
  routine work by default.
