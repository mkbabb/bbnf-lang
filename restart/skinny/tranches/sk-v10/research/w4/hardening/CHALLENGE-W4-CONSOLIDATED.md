# SK-V10 W4 CHALLENGE - `instruments` Typed Product Admission

Pass: CHALLENGE.
Cycle: W4.
Date: 2026-05-19.
Plan under review: `restart/skinny/tranches/sk-v10/research/w4/w4-plan.md`.
Disposition: ACCEPT.

## CH1 Correctness - ACCEPT

The plan names a real typed product surface rather than a digest proxy:
generated Track 1, independent Track 2/oracle, serde_json typed, and sonic-rs
typed all decode the full `instruments` fixture into typed structures and meet
through a checksum. This is sufficient if redress proves `track2_typed` never
calls the generated typed parser and if the new checksum folds product fields,
not just fixture length or object count.

Redress requirement: add a focused `instruments` full-fixture parity test and
keep the checksum path in `real_typed_struct.rs`.

## CH2 Generality / Lock 14 - ACCEPT

The plan stays inside JSON bench typed schema ownership. It does not edit
generic parser policy, generic crate semantics, root-type codegen, CSS L4,
Sheets, or BBNF grammar behavior. Section 2.1 generic-proof obligations are not
triggered because the schema addition is a benchmark typed product row, not a
generic JSON policy change.

Redress requirement: do not touch `skinny/crates/codegen/src/typed_direct.rs`
or generic runtime/parser sources in W4.

## CH3 Regression / REDRESS - ACCEPT

The plan preserves the W3 falsification and does not reopen the union substrate
or W4 cascade-lock. It also preserves W1/W2 direct contracts by adding a
separate typed row contract instead of weakening direct movement validation.

Redress requirement: existing opening rows remain present, existing typed
maintain floors remain true, and direct `instruments` evidence remains
`direct_to_struct` only.

## CH4 Cost - ACCEPT

The plan is within the W4 budget if the generated file changes only by adding
the new typed root and helper bodies. A derived Criterion root is acceptable
because the opening frozen authority lacks instruments typed benches; seeding
from the frozen root and refreshing the `json_instruments` group keeps one
coherent run id for report validation.

Redress requirement: if the generator emits broad unrelated churn, stop and
REVISE before measurement.

## CH5 Hidden Coupling / Lock 1 - ACCEPT

The intervention adds no substrate, sidecar tape, retained class column, parser
cursor, direct semantic facts, or shared bench-private parser. The same-wave
consumer is the existing typed bench and gate/report contract.

Redress requirement: no new telemetry field, no new outcome variant, and no
new public substrate API. The Lock 14 parent-diff authorizer may be extended
only for the W4 typed-schema owner paths already touched by this plan; it may
not authorize generic codegen/runtime diffs.

## CH6 Anti-Paper-Close - ACCEPT

The wave closes only if the report validator consumes the new row contract and
the generated Track 1 and independent Track 2/oracle both meet
`ceil(same-run sonic_typed / 1.10)`. A manual `RESULTS.md` row, a passing
Track 1 with a slow Track 2, missing comparator metadata, or direct digest
evidence must fail closed.

Redress requirement: add negative report tests for the W4 row floor and
contract fields.

## Accepted Redress Conditions

- `json/instruments/real_typed_struct/main` is the only new row accepted.
- `gate-json` renders the W4 row only from real typed Criterion metadata and
  estimates.
- The W4 row has `strictness=strict`, `parse_utf8=measured-row`,
  `measured_validation_path=measured-row`,
  `same_wave_consumer_class=gate_json_typed_contract`,
  `redress_entry=REDRESS-103`, `wave_id=SK-V10-W4`, and
  `sk_v9_open_delta=typed-row-added`.
- Track 1 and Track 2/oracle both meet `ceil(same-run sonic_typed / 1.10)`.
- Same-run native sonic-rs typed and serde_json typed comparator evidence is
  present.
- Existing typed guard rows preserve SPEC Section 0.2 maintain floors.
- Lock 14 accepts the committed W4 typed-schema diff by subject and exact owner
  path, not by weakening the frozen-root set.
- If any accepted condition fails, W4 records a REDRESS reject instead of
  admitting the row.
