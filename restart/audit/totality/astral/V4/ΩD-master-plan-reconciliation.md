# Omega-D Master-Plan Reconciliation - Pass Omega V4 W4R

Pass: Pass Omega V4.
Date: 2026-05-26.
Scope: reconcile `restart/MASTER-PLAN.md` §13.3 and SK-V14 dispatch surfaces
with REDRESS-184 and W4R.
Status: proposed patch text only; do not apply before G-Omega.

## Verdict

ACCEPT-WITH-PATCH.

W4's provider/template deletion must be split from the CSS row-ledger prune and
moved into W5. Resequencing W5 before W4 is rejected because it rewrites the
PRUNE chain globally. The local amendment preserves the chain while assigning
deletion to the wave that owns the replacement generator.

## Proposed Wave-Graph Amendment

| Wave | Amended scope | Entry gate | Cap |
|---|---|---|---|
| W4 | PRUNE-2 ledger prune: revert CSS L4 rolling delta to 0/24 and add 24 REDRESS entries; no provider/template deletion. | W2 + W3 close. | <=500 docs/ledger, <=90 min |
| W5 | PRUNE-3 provider collapse: stand up generic provider, migrate `regen_css.rs`, delete seven CSS provider modules plus template dirs, run `regen-css` and companions. | W4 close. | <=1.4k C-1 part-A source/test LOC, <=90 min |
| W6 | Unchanged from V3 W2R: W6.0 CSS L4 root-runtime collapse, W6.1-W6.8 remaining Pattern H dirs. | W5 close. | <=90 min per sub-wave, aggregate <=810 min |

W8/W9/W10 remain globally blocked until W1/W4/W5/W6/W7 PRUNE close.

## Required Patch Payload

The companion `master-plan-diff.md` contains proposed patch text for:

- `restart/MASTER-PLAN.md` §13.3 W4/W5 rows.
- `restart/skinny/tranches/sk-v14/SPEC.md` W4 and W5 sections.
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` R3/C-5/C-1 wording.
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` R3 wording.
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` W4R pre-dispatch guard.

## Dispatch Result Before G-Omega

Until G-Omega V4 authorizes this amendment:

- do not patch SPEC or V1 surfaces;
- do not delete CSS provider/template directories;
- do not dispatch W5;
- keep W8/W9/W10 blocked by the global PRUNE-before-new-admit rule.
