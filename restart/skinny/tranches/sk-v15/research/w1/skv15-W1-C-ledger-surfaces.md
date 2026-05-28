# SK-V15 W1-C: Ledger And Documentation Surfaces

Date: 2026-05-28.
Scope: `skinny/REDRESS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md`, and SK-V15 tranche docs.
Output: this file.

## Findings

- `restart/skinny/ROLLING-SOTA-DELTA.md` still carries the live stale CSS
  admission claim: 24 CSS rows use `css_l4_full_parse`, identical W8R margins,
  and `ADMITTED` from `restart/skinny/ROLLING-SOTA-DELTA.md:70` through
  `restart/skinny/ROLLING-SOTA-DELTA.md:93`.
- `skinny/REDRESS.md` item 215 still says W8R moved all 24 CSS rows in
  `skinny/RESULTS.md` plus rolling delta to `AUDIT-SUSTAINED` / `ADMITTED`
  at `skinny/REDRESS.md:5328` through `skinny/REDRESS.md:5333`.
- Current `skinny/RESULTS.md` is already diagnostic: CSS rows are
  `not_admitted`, `AUDIT-FALSIFIED`, and routed to W1 at
  `skinny/RESULTS.md:112` through `skinny/RESULTS.md:135`; the notes classify
  CSS as diagnostic non-admission at `skinny/RESULTS.md:141` through
  `skinny/RESULTS.md:152`.
- SK-V15 authority already routes this work to W1. SPEC requires no 24-row CSS
  broadcast admit at `restart/skinny/tranches/sk-v15/SPEC.md:54`, says W8R
  CSS rows are diagnostic or NO-GO at `restart/skinny/tranches/sk-v15/SPEC.md:96`,
  and defines W1 demotion/collapse at `restart/skinny/tranches/sk-v15/SPEC.md:268`
  through `restart/skinny/tranches/sk-v15/SPEC.md:279`.

## Recommendations

- W1 should update `restart/skinny/ROLLING-SOTA-DELTA.md` to `OPEN` for all
  24 W8R CSS rows and keep numeric values only as diagnostic margins.
- W1 should add a supersession note under REDRESS-215 explaining that W8R is
  retained as diagnostic/non-admission evidence and cannot support 24/24 live
  admission.
- Treat pre-W0 research documents as historical evidence of the defect, not as
  live authority to edit.

## Risks

- If REDRESS-215 remains unsuperseded, future waves may cite it as retained
  admission authority despite W0's RESULTS demotion.
- If rolling delta remains `ADMITTED`, `gate-json --check-results` can still
  validate an externally visible live admit surface.

## Sources

- `restart/skinny/ROLLING-SOTA-DELTA.md:70`
- `restart/skinny/ROLLING-SOTA-DELTA.md:93`
- `skinny/REDRESS.md:5328`
- `skinny/REDRESS.md:5333`
- `skinny/RESULTS.md:112`
- `skinny/RESULTS.md:135`
- `skinny/RESULTS.md:141`
- `skinny/RESULTS.md:152`
- `restart/skinny/tranches/sk-v15/SPEC.md:54`
- `restart/skinny/tranches/sk-v15/SPEC.md:96`
- `restart/skinny/tranches/sk-v15/SPEC.md:268`
- `restart/skinny/tranches/sk-v15/SPEC.md:279`
