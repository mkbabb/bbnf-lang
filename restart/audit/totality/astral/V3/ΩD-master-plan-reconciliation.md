# Omega-D Master-Plan Reconciliation - Pass Omega V3 W2R

Pass: Pass Omega V3.
Date: 2026-05-25.
Scope: reconcile `restart/MASTER-PLAN.md` §13.3 and SK-V14 `SPEC.md` /
`SYNTHESIS.md` with REDRESS-183 and W2R.
Status: proposed patch text only; do not apply before G-Omega.

## Verdict

ACCEPT-WITH-PATCH.

The W2 dual-tree requirement must be split locally. Resequencing W5/W6 before
W2 is rejected because it rewrites the PRUNE chain globally. The local
amendment preserves PRUNE-before-rebuild and assigns the root CSS L4 runtime
tree to the wave that can actually generate it.

## Proposed Wave-Graph Amendment

| Wave | Amended scope | Entry gate | Cap |
|---|---|---|---|
| W2 | `regen-css` skinny-side only: emit `skinny/crates/runtime/src/grammars/css_l4_*`, add `check-css-l4-*`, no root runtime output. | W1 close. | unchanged: <=2.0k C-3 part-A, <=90 min |
| W3 | Production CSS corpora. | W2 close, where W2 close means skinny-side regen only. | unchanged |
| W4 | PRUNE-2 deletes CSS templates/providers/runtime twins and recovers skinny runtime via W2. | W2 + W3 close. | unchanged |
| W5 | PRUNE-3 generic provider / generator template. | W4 close. | unchanged |
| W6.0 | CSS L4 root-runtime collapse: emit `crates/core/src/runtime/css_l4/` from grammar source and metadata. | W5 close. | <=90 min |
| W6.1..W6.8 | Remaining Pattern H grammar dirs: `math`, `csv`, `bnf`, `ebnf`, `css_pretty`, `google_sheets`, `bbnf`, `json`. | sequential inside W6. | <=90 min each |

W6 remains exactly nine sub-waves. The aggregate W6 cap remains <=810 minutes.
No lock, architecture, BackendShape, substrate-union, or row-admission change is
implied.

## Required Patch Payload

The companion `master-plan-diff.md` contains proposed patch text for:

- `restart/MASTER-PLAN.md` §13.3 W2/W6 rows and W2R receiver note.
- `restart/skinny/tranches/sk-v14/SPEC.md` W2, W4, W6, rerun ceilings, and downstream notes.
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` stale C-1/C-3 candidate text.

## Dispatch Result Before G-Omega

Until G-Omega authorizes this amendment:

- do not rerun W2;
- do not dispatch W3-W8;
- do not dispatch W9/W10 new-admit waves because the global PRUNE-before-new-admit rule is unsatisfied;
- treat current W2 as rejected by governance, not as an implementation TODO.
