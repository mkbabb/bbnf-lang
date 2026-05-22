# SK-V13 W11.3 CHALLENGE - Direct Sink Stack Specialization

Date: 2026-05-21.
Plan: `restart/skinny/tranches/sk-v13/research/w11.3/plan.md`.
Disposition: ACCEPT with constraints.

## CH1 Correctness

PASS. The plan changes only how `JsonDigestSink` reaches the current parent
frame; it does not change digest math or parser semantics. Redress must run
direct parity coverage after the change.

## CH2 Generality / Lock 14

PASS. This is a JSON direct benchmark consumer specialization, scoped to W11.N.
It is not generic-crate behavior and does not weaken Lock 14. Constraint: no
`JsonSink` trait expansion and no runtime/codegen edits.

## CH3 Regression / REDRESS

PASS with hard revert. Existing admits are guards. If no primary row admits,
the source patch must be saved at `/tmp/skv13-waveW11.3-rejected.patch` and
reverted before redress commit.

## CH4 Cost

PASS. The expected diff is local and well under the W11 row-family LOC cap.

## CH5 Hidden Coupling

PASS with constraint. Removing helper closures must not change array element
counting, object member counting, depth, fingerprint, or scalar class counters.
Keep parity against Track 2/serde/sonic.

## CH6 Anti-Paper-Close

PASS. This wave may admit only on a primary row clearing same-run sonic strict
+ 1. Movement without absolute admission is a measured rejection.

## Accepted Contract

- Gate id: `G-W11.3-JSON-DIRECT-SINK-STACK`.
- Primary rows: `instruments`, `mesh`, `random`, `canada` direct.
- Owner boundary: `direct_struct.rs` plus report/gate/status docs only if a
  row admits.
- Forbidden: parser/runtime/codegen/SIMD/generic-crate changes, digest
  shortcut, source hook, row-private branch, comparator weakening.
