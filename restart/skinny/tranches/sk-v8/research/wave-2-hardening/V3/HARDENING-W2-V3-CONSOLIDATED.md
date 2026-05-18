# SK-V8 W2 Hardening V3 Consolidated

Date: 2026-05-18.
Target: `8ce03af4` (`fix(sk-v8-wave2-gate): fold typed hardening disposition`).

## Verdict

REVISE, 5/6 ACCEPT and 1/6 REVISE.

Minimum confidence: 90%.

## Accepted Surface

CH1, CH2, CH4, CH5, and CH6 accepted the V2-folded W2 source/product
disposition. They found the W2 typed source paths, Lock 14 scope, no-RESULTS
posture, Canada route-out, and strict-vs-strict comparator discipline coherent.

## Blocking Finding

CH3 reran the standard checked report path and found a deterministic mismatch
between source-only typed fixture expansion and W0 measured-row metadata
requirements:

- W2 added `apache_builds` and `citm_catalog` to the source/product typed
  fixture map.
- The standard report gate required real typed Criterion metadata for every
  `fixture_for_name(..)` fixture.
- `skinny/RESULTS.md` intentionally still contained only the W0 four measured
  `real_typed_struct` rows.
- Therefore `cargo xtask gate-json --advisory --check-results` failed on
  missing Apache/CITM real typed metadata before reaching the previously known
  W0 run-id strict drift.

## Required Fold

The report gate must derive W0 real typed metadata requirements from the W0
measured baseline row table, not from the broader source/product typed fixture
map. Apache/CITM source/product parity fixtures must not imply unadmitted
Criterion `real_typed_struct` benchmark rows. A regression test must prove that
the W0 measured baseline expects real typed metadata for W0 rows such as
`twitter` and `update_center`, but not for W2 source-only rows such as
`apache_builds` and `citm_catalog`.

After this fold, a standard checked report failure caused by local Criterion
run-id drift remains W0 baseline evidence; it is not a W2 typed metadata
defect and must not be hidden by weakening the W0 run-id validator.
