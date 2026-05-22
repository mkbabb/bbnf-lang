# SK-V13 W13.5 CHALLENGE - GSOC Typed Product Surface

Wave: W13.5.
Plan under review: `restart/skinny/tranches/sk-v13/research/w13.5/plan.md`.
Disposition: ACCEPT.

## CH1 Correctness

ACCEPT WITH CONDITION. The selected product must cover every modeled proposal
field plus nested `sponsor` and `author` objects. The checksum must include
the map key, `@context`, `@type`, names, descriptions, URLs, logos, and nested
object fields so a root-only map or skipped nested object is observable.

## CH2 Generality / Lock 14

ACCEPT. The implementation stays inside the JSON host/API typed product
surface and bench gate. No generic crate, directive, BIR variant,
`BackendShape`, substrate API, or direct-plane policy is planned.
`lock14_baseline` must authorize the W13.5 owner paths because generated typed
output and gate/report parents are touched.

## CH3 Regression / REDRESS

ACCEPT WITH CONDITION. Existing admitted typed rows must maintain their status,
including W13.1 `numbers`, W13.2 `unicode_basic`, W13.3 `random`, and W13.4
`instruments`. W13.5 must record a same-run measured admit or measured reject
under the current sonic+1 gate.

## CH4 Cost

ACCEPT. Expected generated growth is one map-entry root and three product
parsers. If regeneration expands unrelated roots or exceeds the W13 surface
budget, redress blocks until the growth is traced.

## CH5 Hidden Coupling

ACCEPT WITH CONDITION. Track 2 may be the serde typed oracle, but the gate must
state that explicitly and reject Track 1/Track 2 coupling. Sonic is the strict
SOTA comparator. A direct digest row, hidden typed sink, root key collector,
or parser that silently drops nested `sponsor` / `author` objects is REJECT.

## CH6 Anti-Paper-Close

ACCEPT. A synthetic parser test, report-only row, RESULTS-only edit, direct
digest reuse, schema-only fixture, or partial typed root is REJECT. The
same-wave consumer is the `bbnf-bench` `real_typed_struct` workload for
`gsoc-2018`, measured through Criterion and consumed by `gate-json`.

## Accepted Redress Contract

Proceed to redress if and only if:

- `cargo xtask check-real-typed` passes after regeneration.
- Full-fixture typed parity passes for generated Track 1, serde Track 2,
  serde sidecar, and sonic.
- Native Criterion captures the four `gsoc-2018/real_typed_struct` lanes.
- The W13.5 companion report validates `Track 1 > sonic strict + 1 Mbps`,
  report artifact hash, strict equality provenance, and RESULTS/rolling
  alignment.
- REDRESS records the measured admit or reject.
