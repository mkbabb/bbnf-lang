# SK-V10 Alpha CHALLENGE V1 Consolidated

Date: 2026-05-19.

## Input

Reviewed:

- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-F-contract-draft.md`

## Initial Disposition

REVISE. Three material defects:

1. Typed product rows were overstated as strict-vs-strict even though bbnf rows
   remain deferred/view-boundary.
2. Lock 14/non-JSON proof was routed but not a hard refusal gate.
3. Alpha-E lacked the cost, target, bounded-owner, and revert details required
   by PASS-ALPHA CH4/CH6.

## Folded Resolutions

1. Rephrased typed wins as same-run typed comparator evidence under the current
   deferred/view-boundary typed-product gate. No strict-admission claim exists
   until `gate-json` consumes a measured-row strictness and validation-path
   change.
2. Added Lock 14 refusal language to the synthesis, handoff, shortlist, and
   contract draft.
3. Corrected stale citation ranges for `RESULTS.md` and `REDRESS.md`.
4. Added per-candidate LOC budgets, hard caps, same-wave consumers, target
   matrices, W10b maintain floors, bounded research artifact namespaces, and
   REDRESS/revert dispositions.

## Final Disposition

ACCEPT after fold. Alpha is eligible for `G-ALPHA-SK-V10` recording, then
S-P1/S-P2/S-P3 dispatch. It does not authorize source implementation directly.
