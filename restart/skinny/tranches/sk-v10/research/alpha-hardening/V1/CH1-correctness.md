# SK-V10 Alpha CH1 Correctness

Date: 2026-05-19.

Scope: adversarial review of Alpha-A through Alpha-F plus
`SYNTHESIS.md`/`HANDOFF.md`.

## Disposition

REVISE -> ACCEPT after fold.

## Findings

1. Typed product rows were overstated as strict-vs-strict. Current
   `RESULTS.md` rows remain `Strictness=deferred` and
   `parse_utf8=view-boundary`; Alpha-A already warned not to overstate this.
   Fold: `SYNTHESIS.md` and Alpha-B now describe the surface as same-run typed
   comparator evidence under the current deferred/view-boundary typed-product
   gate, with no strict-admission claim until `gate-json` consumes a measured
   strictness and validation-path change.
2. Citation ranges were stale. Alpha-A cited `RESULTS.md:46-86`; the manifest
   runs through `RESULTS.md:89`. Alpha-C cited `REDRESS.md:2731-2940`; REDRESS
   98 continues through `REDRESS.md:2950`.
   Fold: Alpha-A and Alpha-C now carry the corrected ranges.

## Result

Correctness blockers are resolved. Remaining Alpha claims cite current
`RESULTS.md`, REDRESS 94-98, or defer measurement to S-P1/S-P2/S-P3 without
authorizing implementation.
