# SK-V15 Wave W11 Plan: Close Packet

Status: ACCEPT WITH CLOSE-ONLY BOUND.

## Owned Files

W11 owns only the close packet, PASS-IMPL V2 audit packet, tranche handoff, and
REDRESS close note:

- `restart/skinny/tranches/sk-v15/research/w11/*`
- `restart/audit/skinny-impl-overfit/V2/*`
- `restart/skinny/tranches/sk-v15/HANDOFF.md`
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md`
- `skinny/REDRESS.md`

W11 does not touch the pre-existing dirty generated CSS files, prior-tranche
JSON files, or `docs/precepts`.

## Close Rules

1. Treat CSS as `ROUTE-W6-REJECT`, not as admission.
2. Record every `DEP-*` row in a checklist with proof, REDRESS route, or
   intrinsic-block disposition.
3. Record PASS-IMPL V2 as an implementation audit, not as a spec amendment.
4. Keep SK-V16 routing subordinate to SK-V15 evidence. SK-V16 cannot be cited
   as proof that SK-V15 closed.
5. Abort W11 if any dependency row has no owner, no proof, and no
   intrinsic-block classification.

## Artifact Plan

| Artifact | Purpose |
|---|---|
| `skv15-W11-research.md` | Read-only evidence collection and dirty-state classification. |
| `skv15-W11-plan.md` | Close-only owner boundary and abort rules. |
| `skv15-W11-challenge.md` | Seven-lens challenge against docs-only close, CSS admission drift, and dirty-tree masking. |
| `skv15-W11-close-dependency-checklist.json` | Machine-readable dependency-row consumption table. |
| `skv15-W11-pass-impl-v2-handoff.md` | PASS-IMPL V2 summary and SK-V16 route. |
| `skv15-W11-redress.md` | Final W11 admission/routing statement. |
| `restart/audit/skinny-impl-overfit/V2/*` | Six-axis close audit plus consolidated verdict. |

## Verification Plan

Use only current HEAD command output, tracked report artifacts, or explicit
dirty-state/routed-block proof. Do not use historical CSS PASS-ADMIT language as
current SK-V15 authority.

The close commit stages only W11-owned documentation and audit files.
