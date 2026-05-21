# SK-V12 W2 CHALLENGE V1 - Consolidated

Disposition: REJECT / route back to plan.

Lens dispositions:

- CH1 correctness: REVISE.
- CH2 generality / Lock 14: REVISE.
- CH3 regression / REDRESS: REVISE.
- CH4 cost / scope: REVISE.
- CH5 hidden coupling: REVISE.
- CH6 anti-paper-close: ACCEPT.

The selected proof-only concept is acceptable, but PLAN.md is not redress
authority. V1 places caller-level JSON scanner parity in an illegal or
underspecified location, overloads W2 with native JSON guard commands, omits
the explicit LOC budget, and leaves runtime scanner ownership unresolved.

Required plan revision:

1. Keep `bbnf-simd` W2 work to the primitive `checkasm_escape_mask_64` proof.
2. Move caller-level JSON scanner adversarial parity to an explicitly owned
   runtime or bench test path.
3. Name exact commands for both proof cells.
4. Add the <=180 hand/test LOC budget.
5. Make full JSON guard floors conditional on behavior/source movement, or
   record a no-touch JSON guard proof.
6. Tighten the rejected-patch-before-revert protocol.
