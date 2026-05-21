# SK-V12 W2 CH4 - Cost / Scope

Disposition: REVISE.

The core redress is small enough if it remains a correctness proof:
direct `escape_mask_64` proof cells, caller scanner parity, report update, and
REDRESS entry.

Blocking issues:

- The plan does not name the <=180 hand/test LOC budget required by SPEC.
- The command surface is duplicated: `primitive-checkasm` already includes
  broad strict checkasm targets, so separate `checkasm_parity` plus
  `primitive-checkasm` is redundant unless the new W2 test is not yet wired
  into the xtask gate.
- Full `bench-json --advisory` is too expensive for proof-only W2 and can
  update `RESULTS.md`, which W2 does not own.

Required revision: name the LOC budget, use focused W2 tests plus
`primitive-checkasm` or a non-duplicative equivalent, and make full JSON
bench/gate/update commands conditional on an actual JSON-producing behavior
move plus explicit `RESULTS.md` ownership.
