# Q6 — test-threads × -Zthreads collision

**Status**: resolved
**Owner tranche**: B1 profile configuration
**Decision date**: 2026-04-23
**Affects**: `ax-iter` profile, `close` profile, wave-close sanity script

## Context

The `ax-iter` cargo profile enables `-Zthreads` for the compiler
front-end, parallelizing proc-macro expansion and MIR construction
across multiple rustc threads per crate. The test harness independently
parallelizes across test cases via `--test-threads`. On an 8-core
machine the product of the two is 64 simultaneous threads contending
for the same cores, and the resulting context-switch thrash makes
`ax-iter` slower than a sequential run.

The interaction is not a bug in either mechanism — both do what they
advertise — but their composition is lethal on the iteration loop.
The question was whether to drop one, cap both, or profile per
workload.

## Decision

**Drop test threads on `ax-iter`.** Set `test-threads=2` on `ax-iter`
so the compiler threads dominate. Set `test-threads=8` on `close` so
the test harness dominates when the compiler has nothing left to
parallelize. Wave-close sanity check: iter-test wall-clock stays
under 30s on the 8-core reference machine.

## Reasoning

`ax-iter` is the hot iteration profile — fast rebuilds, small test
subsets, quick signal. The compiler is the bottleneck; cutting test
parallelism to 2 keeps it out of the compiler's way. A test harness
running 8 threads during compile-heavy iteration is wasted capacity
that degrades the compile the tests are waiting on.

`close` is the wave-close profile — full test suite, compiler already
warm from iteration, tests are the bottleneck. Compiler threads have
less to do and test parallelism pays off. test-threads=8 matches the
reference machine's core count.

A single profile with a tuned product was rejected. The two phases
have opposite bottlenecks and no single setting is right for both.
Separate profiles name the regime explicitly.

The 30s wall-clock sanity check is the empirical backstop. Any change
that pushes `ax-iter` past 30s on the reference machine is a
regression and rolls back.

Tradeoff: contributors on non-8-core machines get a less-optimal
setting. The profile knob documents the reference and explains how to
override locally. Correct per-machine tuning is out of scope.

## Resolution mechanism

1. `.cargo/config.toml` (or equivalent profile setting) records
   `test-threads=2` under `ax-iter` and `test-threads=8` under
   `close`.
2. Wave-close runbook adds the 30s iter-test wall-clock check to its
   standard sanity battery.
3. Reference machine spec (8 physical cores, Apple M-series or
   equivalent) documented alongside the profile.

## Follow-up gate

Wave-close sanity battery enforces the 30s bound. A regression triggers
profile re-tuning before the wave closes, not afterward. Per-contributor
machines are documented as falling outside the canonical bound.

## References

- `.cargo/config.toml` (profile `ax-iter`, `close`)
- Feedback: `feedback_iter_profile_always.md`, `feedback_single_cargo_per_target.md`
