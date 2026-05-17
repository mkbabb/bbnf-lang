# S-P2 V4 CH4 - Cost

Role: CH4 (Cost) adversarial review of the V4-folded S-P2 substrate-ceiling
cohort and packet docs.

Verdict: ACCEPT

Score: 95/100

## Blocking Findings With Refs

None.

## Notes

- The V3 CH4 blocker is closed. `tape_vs_tape` is now explicitly a routed
  residual, not default W0/W1 scope, and any later adoption must add owner files,
  tests, LOC, and the extra gate refresh before it can consume wave budget
  (`restart/skinny/tranches/sk-v8/SPEC.md:125-131`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:82-85`,
  `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:326-346`).
  The W3 guard is also preserved: telemetry/comparator rows cannot satisfy the
  production same-wave-consumer requirement
  (`restart/skinny/tranches/sk-v8/SPEC.md:447-452`).
- Tier A now has an adequate owner/cost/proof surface. SC-3 names owner files,
  source LOC, generated-output audit, row/plane targets, same-wave consumers,
  named tests/commands, and revert slices for SIMD, tape/runtime, generated JSON,
  retained view, `path!`, direct/SinkOnly, Track 2, and Lock 14 proof
  (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:469-480`).
  SPEC makes that table the mandatory W3 starting point
  (`restart/skinny/tranches/sk-v8/SPEC.md:463-471`).
- Tier B separation is strong enough for CH4. String-boundary closure,
  quote/backslash/parity facts, density policy, CostFacts-template parity,
  non-JSON production migration, and broad lowerer fill are outside Tier A unless
  a later accepted plan prices them inside the cap with same-wave consumers and
  verification (`restart/skinny/tranches/sk-v8/SPEC.md:431-439`,
  `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:318-330`,
  `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:510-526`,
  `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:305-311`).
- Tests, rerun ceilings, and revert boundaries are explicit enough. The packet
  caps W3 at one full gate refresh, with a second rerun requiring a REDRESS cost
  note (`restart/skinny/tranches/sk-v8/SPEC.md:232-245`), while SC-3 gives the
  concrete Tier A command set and reiterates that admission must use the
  enforcing post-W0 gate path (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:482-501`).
  Per-slice revert boundaries are present in the same owner/cost table
  (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:471-480`).
- Non-JSON proof is no longer hidden. SPEC requires CSS L4, Sheets, and
  BBNF-self proof for generic CostFacts/codegen/runtime/SIMD/parser-template
  edits (`restart/skinny/tranches/sk-v8/SPEC.md:247-269`), and SC-3 prices that
  proof as a Tier A audit/test row with named no-op/diff tests and grep/API scans
  (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:480`).
- Residual cost hygiene note: SC-2's summary says Tier A is "net about +150
  source LOC" (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:340`),
  while SC-3's detailed table is the authoritative planning basis and includes
  retained-view plus Lock 14 proof rows. This is not blocking because SPEC
  points W3 at SC-3's table and the detailed Tier A scope remains under the W3
  default cap (`restart/skinny/tranches/sk-v8/SPEC.md:222-239`,
  `restart/skinny/tranches/sk-v8/SPEC.md:463-471`), but S-P3 should copy SC-3,
  not the smaller shorthand.

## Required Folds If REVISE

None. This CH4 cycle accepts the V4 cost/scope fold.
