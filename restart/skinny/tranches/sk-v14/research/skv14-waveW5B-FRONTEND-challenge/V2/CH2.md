# SK-V14 W5B-FRONTEND CHALLENGE V2 CH2 Generality

Date: 2026-05-26.
Lens: CH2 Generality.
Disposition: REVISE.

## Findings

1. The V2 gate list does not explicitly run the SPEC Section 2.1 public API,
   grammar-branch, and grammar-shape scans required for generic-crate edits.
   The gates at `skv14-W5B-FRONTEND-plan.md:126`-`176` omit the scan set bound
   by `SPEC.md:304`-`313` and `LOCKS.md:377`-`387`. W5B owner paths include
   `skinny/crates/grammar/src/lib.rs` and
   `skinny/crates/codegen/src/grammar_provider.rs` at
   `skv14-W5B-FRONTEND-plan.md:27`-`29`, but the current Lock 14 generic scan
   roots omit both at `lock14_baseline.rs:1772`-`1781`.
2. Public-syntax retirement is exact only for `@ws`. The plan blocks a public
   directive set at `skv14-W5B-FRONTEND-plan.md:85`-`87` and public `@ws` at
   `skv14-W5B-FRONTEND-plan.md:238`, but the exact public-retirement test list
   only names `w5b_frontend_public_ws_remains_retired` at
   `skv14-W5B-FRONTEND-plan.md:131`. SPEC requires the full compatibility set
   to lower as frontend/IR, not new public BBNF syntax, at `SPEC.md:728` and
   `SPEC.md:733`-`735`; ORCHESTRATOR binds "No new BBNF directives" at
   `ORCHESTRATOR.md:197`-`204`.

## Accepted Checks

- Non-JSON proof carry is present: CSS companion gates, JSON unchanged proof,
  and Sheets/BBNF-self fail-closed proof are named at
  `skv14-W5B-FRONTEND-plan.md:135`-`160` and
  `skv14-W5B-FRONTEND-plan.md:226`-`228`.
- W5B sub-slices are internal, not deferrals, per
  `skv14-W5B-FRONTEND-plan.md:17`-`20` and `:47`-`:49`.
- CSS L4 remains a witness path while provider-free generation/deletion remain
  W5C/W5D-owned at `skv14-W5B-FRONTEND-plan.md:74`-`81` and `:234`-`:247`.

## Required Fold

- Add a fail-closed Lock 14 leak census over all W5B generic owner paths, or
  extend `lock14_baseline` so the named Lock 14 test/gate covers
  `grammar/src/lib.rs` and `grammar_provider.rs`.
- Add exact negative public-parse tests for `@pretty`, `?w`, `>>`, `<<`, span
  capture, typed projections, and import-only request closure.
