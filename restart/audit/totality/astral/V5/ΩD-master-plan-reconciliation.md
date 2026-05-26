# Omega-D Master-Plan Reconciliation - Pass Omega V5 W5R

Pass: Pass Omega V5.
Date: 2026-05-26.
Scope: reconcile `restart/MASTER-PLAN.md` §13.3 and SK-V14 dispatch surfaces
with REDRESS-209 and W5R.
Status: proposed patch text only; do not apply before G-Omega.

## Verdict

ACCEPT-WITH-PATCH.

W5's provider/template deletion must be split from the generator-capability
work. The current W5 gate asks one wave to create a grammar-neutral
source-consuming generator, make the CSS L4 source surface parseable without
CSS-specific branches, migrate `regen-css`, delete seven provider modules and
seven template directories, and close the Lock 14 baseline. The research/redress
evidence shows that is not credible under the W5 cap and would invite a
static-centralization workaround.

## Proposed Wave-Graph Amendment

| Wave | Amended scope | Entry gate | Cap |
|---|---|---|---|
| W5A | PRUNE-3A generator capability: introduce grammar-neutral source-consuming runtime emission request; make the required V1 grammar-source constructs parseable for runtime generation without grammar-id branches; migrate `regen-css` so source/metadata enter codegen; prove CSS L4 plus JSON/Sheets/BBNF-self non-JSON gates; keep provider/template deletion out of W5A. | W4 ledger close. | <=1.0k C-1 part-A source/test LOC; <=90 min |
| W5B | PRUNE-3B provider/template deletion: delete seven CSS providers and seven template dirs after the new generator path is load-bearing; retire old provider mesh; update Lock 14 baseline. | W5A close. | <=400 C-1 part-A source/test LOC; <=90 min |
| W6 | PRUNE-4 unchanged in substance: W6.0 CSS L4 root-runtime collapse, W6.1-W6.8 remaining Pattern H dirs. | W5B close. | <=90 min per sub-wave; aggregate <=810 min |

W5A + W5B are a split of the existing W5 C-1 part-A envelope, not an expansion:
combined hand-edited source/test LOC must remain <=1.4k. W6 keeps the existing
<=2.0k C-1 part-B aggregate, preserving the total C-1 envelope at <=3.4k.

W7 remains conditional on W6 close. W8/W9/W10 remain globally blocked until
PRUNE-1 through PRUNE-5 close.

## Required Patch Payload

The companion `master-plan-diff.md` contains proposed patch text for:

- `restart/MASTER-PLAN.md` §13.3 W5/W5B/W6 rows.
- `restart/skinny/tranches/sk-v14/SPEC.md` W5, W5B, W6, W9, and W10 entry /
  downstream wording, including all-seven CSS companion coverage and JSON /
  Sheets / BBNF-self non-JSON proof gates.
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` R3/C-1/P-6 wording.
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` R3 wording.
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` W5R pre-dispatch guard
  and NEW-CH3/NEW-CH5 procedural addenda.

## Dispatch Result Before G-Omega

Until G-Omega V5 authorizes this amendment:

- do not patch SPEC or V1 surfaces;
- do not delete CSS provider/template directories;
- do not dispatch a W5 implementation retry;
- keep W6/W7/W8/W9/W10 blocked by the PRUNE chain.
