# SK-V13 W13.4 CHALLENGE - Instruments Typed Product Surface

Wave: W13.4.
Plan under review: `restart/skinny/tranches/sk-v13/research/w13.4/plan.md`.
Disposition: ACCEPT.

## CH1 Correctness

ACCEPT WITH CONDITION. The selected product must cover the root metadata,
instrument records, pattern records, sample records, and envelope nodes named
in the plan. The checksum must cover every modeled scalar/string field and
every modeled nested record so a partial root or field rename drift is
observable.

## CH2 Generality / Lock 14

ACCEPT. The implementation stays inside the JSON host/API typed product
surface and bench gate. No generic crate, directive, BIR variant,
`BackendShape`, substrate API, or direct-plane policy is planned.
`lock14_baseline` must authorize the W13.4 owner paths because generated typed
output and gate/report parents are touched.

## CH3 Regression / REDRESS

ACCEPT WITH CONDITION. Existing admitted typed rows must maintain their status,
including W13.1 `numbers`, W13.2 `unicode_basic`, and W13.3 `random`.
REDRESS 103 is historical evidence only; W13.4 must record a same-run measured
admit or measured reject under the current sonic+1 gate.

## CH4 Cost

ACCEPT. Expected generated growth is one root and six nested product parsers.
If regeneration expands unrelated roots or exceeds the W13 surface budget,
redress blocks until the growth is traced.

## CH5 Hidden Coupling

ACCEPT WITH CONDITION. Track 2 may be the serde typed oracle, but the gate must
state that explicitly and reject Track 1/Track 2 coupling. Sonic is the strict
SOTA comparator. A direct digest row, hidden typed sink, root-only parser, or
parser that silently drops modeled arrays is REJECT.

## CH6 Anti-Paper-Close

ACCEPT. A synthetic parser test, report-only row, RESULTS-only edit, reused
REDRESS 103 number, direct digest reuse, or partial typed root is REJECT. The
same-wave consumer is the `bbnf-bench` `real_typed_struct` workload for
`instruments`, measured through Criterion and consumed by `gate-json`.

## Accepted Redress Contract

Proceed to redress if and only if:

- `cargo xtask check-real-typed` passes after regeneration.
- Full-fixture typed parity passes for generated Track 1, serde Track 2, serde
  sidecar, and sonic.
- Native Criterion captures the four `instruments/real_typed_struct` lanes.
- The W13.4 companion report validates `Track 1 > sonic strict + 1 Mbps`,
  report artifact hash, strict equality provenance, and RESULTS/rolling
  alignment.
- REDRESS records the measured admit or reject.
