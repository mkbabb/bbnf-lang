# Alpha-A - Results Extraction - SK-V16 V1

Pass: Pass Alpha. Cycle: SK-V15 -> SK-V16.
Date: 2026-05-28.
Scope: SK-V15 close counts, routed blocks, and invariants.
Output: this file.
Baseline commits: SK-V15 W11 close packet `66232b7c3`; close evidence head
`8bada626a`.

## Findings

SK-V16 brackets from SK-V15's W11 close packet, not from historical CSS
admission text. W11 records `ADMIT-W11`, consumes
`DEP-W11-CLOSE-NO-ORPHANS`, and states that every SK-V15 dependency row is
admitted, routed with REDRESS, or intrinsically blocked by row-level proof
(`restart/skinny/tranches/sk-v15/research/w11/skv15-W11-redress.md:3`,
`:5-7`).

| Family | SK-V15 close state | SK-V16 Alpha state | Evidence |
|---|---:|---|---|
| JSON parse_only | 17 / 17 admitted | guard baseline | `restart/audit/skinny-impl-overfit/V2/CONSOLIDATED-AUDIT.md:15` |
| JSON direct_to_struct | 17 / 17 admitted | guard baseline | `restart/audit/skinny-impl-overfit/V2/CONSOLIDATED-AUDIT.md:15` |
| JSON real_typed_struct | 17 / 17 admitted | guard baseline | `restart/audit/skinny-impl-overfit/V2/CONSOLIDATED-AUDIT.md:15` |
| CSS L4 | 0 admitted | primary rebuild target | `restart/skinny/tranches/sk-v15/research/w11/skv15-W11-redress.md:42` |
| Pattern H | 67 files, line-1 provenance pass | collapse beyond provenance | `restart/skinny/tranches/sk-v15/research/w11/skv15-W11-redress.md:37-41` |
| BackendShape | 5 shapes | preserve canon | `restart/skinny/tranches/sk-v15/research/w11/skv15-W11-redress.md:40-41` |

## Invariant Baseline

Fresh W11 evidence recorded:

- Lock count is `16`
  (`restart/skinny/tranches/sk-v15/research/w11/skv15-W11-redress.md:37`).
- Pattern H runtime count is `67`
  (`restart/skinny/tranches/sk-v15/research/w11/skv15-W11-redress.md:38`).
- Pattern H provenance scan returns no bad rows
  (`restart/skinny/tranches/sk-v15/research/w11/skv15-W11-redress.md:39`).
- CSS L4 admitted rows are `0`
  (`restart/skinny/tranches/sk-v15/research/w11/skv15-W11-redress.md:42`).

## Dirty-State Baseline

W11 routes broad checks blocked by pre-existing dirty files:

- `(cd skinny && cargo test -p codegen)` consumes dirty
  `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`;
- `(cd skinny && cargo xtask check-real-typed)` consumes dirty
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs`.

Evidence:
`restart/skinny/tranches/sk-v15/research/w11/skv15-W11-redress.md:59-65`.

SK-V16 must either retire that dirty generated state or continue to route broad
checks as non-close evidence. It cannot use those dirty files as proof.

## Disposition

JSON is a guard baseline. CSS L4, dirty generated CSS state, full Pattern H
collapse, and grammar-derived generalization are SK-V16's live work. SK-V16 is
not permitted to treat routed SK-V15 blocks as successes.
