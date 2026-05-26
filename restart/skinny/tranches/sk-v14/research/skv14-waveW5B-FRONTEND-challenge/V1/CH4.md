# SK-V14 W5B-FRONTEND CHALLENGE V1 CH4 Cost

Date: 2026-05-26.
Lens: CH4 Cost.
Disposition: REVISE.

## Findings

The plan is directionally valid but is not redress-ready under the stated
30-minute implementation cap and <=1.0k source/test LOC budget.

1. The requested frontend is broad. `skv14-W5B-FRONTEND-plan.md:49` requires a
   request-scoped frontend IR, import resolution, missing-import and cycle
   failure, plus canonical lowering for `@ws`, `@pretty`, `?w`, `>>`, `<<`,
   span capture, and typed projections. Current grammar code fact-scans these
   constructs at `skinny/crates/grammar/src/lib.rs:141` and
   `skinny/crates/grammar/src/lib.rs:188`; unsupported mappings still classify
   imports/directives/projections/capture as unsupported at
   `skinny/crates/grammar/src/lib.rs:94`.
2. The public parser is not close to the requested frontend. `parse_grammar`
   parses the current skinny grammar and validates refs at
   `skinny/crates/grammar/src/lib.rs:29`; directives admit only `@import` and
   `@token` at `skinny/crates/grammar/src/lib.rs:320`; `ir::ExprKind` has no
   import/layout/projection/capture/fence representation beyond generic
   `Annotation` at `skinny/crates/ir/src/lib.rs:209`.
3. CSS L4 source uses the broad construct surface in live grammar files:
   `grammar/css/l4/stylesheet.bbnf:1`, `stylesheet.bbnf:12`,
   `stylesheet.bbnf:27`, `stylesheet.bbnf:53`, `grammar/css/l4/values.bbnf:1`,
   `values.bbnf:67`, `grammar/css/l4/color.bbnf:189`, and
   `color.bbnf:220`.
4. The gate burden conflicts with the 30-minute cap. The plan demands 18
   cargo/xtask gates plus topology/no-row gates at
   `skv14-W5B-FRONTEND-plan.md:69` and
   `skv14-W5B-FRONTEND-plan.md:91`, then sets a 30-minute cap at
   `skv14-W5B-FRONTEND-plan.md:107`. The dispatch redress default is 60 minutes
   implementation + 15 minutes measure at `DISPATCH-PROMPT.md:85` and
   `DISPATCH-PROMPT.md:211`.
5. The same-wave consumer list is too expensive for the cap as written:
   Lock 14 tests, grammar lowering tests, codegen consumption proof,
   `regen-css`, seven CSS checks, JSON proof, and Sheets/BBNF fail-closed proof
   all in one redress commit at `skv14-W5B-FRONTEND-plan.md:130`.

## Required Fold

- Do not dispatch redress from V1.
- Either restore the normal 75-minute redress envelope or split/narrow
  W5B-FRONTEND before redress.
- If 30 minutes is non-negotiable, narrow V2 to a single admissible slice:
  Lock 14 W5B routing, request-owned import DAG resolution, missing/cycle
  fail-closed tests, and public `@ws` rejection. Route remaining canonical
  lowering constructs to named W5B-FRONTEND sub-slices before W5C-GEN, without
  borrowing W5C/W5D/W6 budget.
- Add a per-file LOC budget and require the codegen test to prove the frontend
  artefact affects request validation before provider rendering.

## Sources

- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:85`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:211`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:49`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:69`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:107`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:130`
- `skinny/crates/grammar/src/lib.rs:29`
- `skinny/crates/grammar/src/lib.rs:94`
- `skinny/crates/grammar/src/lib.rs:141`
- `skinny/crates/grammar/src/lib.rs:320`
- `skinny/crates/ir/src/lib.rs:209`
