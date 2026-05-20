# SK-V12 W1a CHALLENGE Consolidated

Date: 2026-05-20.
Wave: W1a - GrammarConfig + Lock 14 Legality Gate.
Phase: CHALLENGE.
Disposition: REVISE.

## Lens Results

| Lens | Artifact | Disposition | Binding finding |
|---|---|---|---|
| CH1 correctness | `CH1-correctness.md` | REVISE | Add an executable seven-leak closure matrix, mandatory SK-V12 floor enforcement, and an orphan config/profile-field consumer check. |
| CH2 generality / Lock 14 | `CH2-generality-lock14.md` | ACCEPT | Provider selection is acceptable only as data-driven lookup; generic grammar-policy branches remain rejected. |
| CH3 regression / REDRESS | `CH3-regression-redress.md` | REVISE | REDRESS 121 must require refreshed JSON guard state for this JSON-touching plan and the rejected patch slice must include all generated outputs that can move. |
| CH4 cost / size | `CH4-cost-size.md` | REVISE | The V1 plan is too broad for the cap: baseline `lint-loc` fails, rosters are ambiguous, optional broad plumbing must be removed, and benchmark refresh cost must be handled honestly. |
| CH5 hidden coupling | `CH5-hidden-coupling.md` | REVISE | Resolve scan/sink template ownership and make typed-direct containment mandatory, not optional. |
| CH6 anti-paper-close | `CH6-anti-paper-close.md` | ACCEPT | The route cannot paper-close if redress preserves executable gate consumption, config consumption, refreshed JSON guards, and no CSS/SOTA/fallback claim. |

## Required Plan Revision

Before redress dispatch, the plan must be revised to:

1. Choose one exact owner roster and one exact generated roster.
2. Move or explicitly contain JSON scanner/sink template inputs so runtime
   `scan.rs` and `sink.rs` are not simultaneously ambiguous source and
   generated output.
3. Make typed direct containment mandatory by moving JSON-owned typed rendering
   out of scanned generic roots or making it genuinely grammar-neutral.
4. Add an executable seven-leak closure matrix.
5. Make SK-V12 Section 0.5 JSON direct/typed floor proof a mandatory command,
   not an optional prose check or baseline-failing `lint-loc` substitute.
6. Add an orphan-config/profile-field check for every policy field used to
   satisfy the seven-leak matrix.
7. Remove optional report/xtask/bin-gate/schema/outcome changes unless a later
   CHALLENGE explicitly accepts them.
8. Record that the selected JSON-touching route can only PASS with refreshed
   JSON guard state; no-touch accounting is invalid for this implementation.

W1a source redress remains blocked until the revised plan passes CHALLENGE.
