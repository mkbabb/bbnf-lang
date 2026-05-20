# SK-V11 W1b CHALLENGE V2

Date: 2026-05-20.
Wave: W1b - Generated Non-JSON Baseline And Oracle Lane.
Plan under review: committed W1b Phase 2 packet at `HEAD=aaa42034`.

Disposition: ACCEPT. W1b may proceed to redress.

## Prior Rejects

CHALLENGE V1 rejected the plan on two defects:

- CH3: W1b owner lists authorized `skinny/crates/bbnf-bench/src/track2/`, which
  is not a SPEC Section 5 owner path.
- CH5: the report schema did not yet carry gate-consumed Track 1 source,
  generated input/output provenance, strict equality artifact, and explicit
  coupling-negative test classes.

The Phase 2 revisions in `853bf83c` and `aaa42034` close those defects.

## V2/V3 Lens Recheck

| Lens | Disposition | Finding |
|---|---|---|
| CH3 regression / REDRESS | ACCEPT | W1b owner lists now stay on SPEC-compatible paths. `skinny/crates/bbnf-bench/src/track2/` appears only as superseded wording, and `skinny/crates/runtime/src/lib.rs` appears only as read-set/evidence, not future W1b ownership. The plan still blocks `RESULTS.md` movement and baseline-as-behavior claims. |
| CH5 hidden coupling | ACCEPT | The W1b field table now requires `track1_source_kind`, `track1_source_artifact`, generated input/output artifacts, `strict_equality_artifact`, and `fact_bytes_mismatch_artifact`; those fields are gate-consumed. R4/R5 typed/digest conflicts are superseded to `css_l4/declaration_values/direct/main` on `css_l4_declaration_value_fact_bytes`. Oracle independence classes are required implementation tests, and oracle source must live in a reviewable W1b module rather than hidden in the Criterion harness. |

The V1 accepts from CH1, CH2, CH4, and CH6 remain valid after the narrower
revision: the selected row identity is exact, missing generated Track 1 remains
a REDRESS result rather than W1b close, the JSON-provider/root-runtime routes
fail closed, the measured-rejection route fits the budget, and the plan blocks
paper close.
