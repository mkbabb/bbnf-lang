# Omega-D Master-Plan Reconciliation - Pass Omega V6 W5BR

Pass: Pass Omega V6.
Date: 2026-05-26.
Scope: reconcile `restart/MASTER-PLAN.md` and SK-V14 dispatch surfaces with
REDRESS-210.
Status: proposed patch text only; do not apply before G-Omega.

## Verdict

ACCEPT-WITH-PATCH.

The current W5B row conflates two steps: building a provider-free generator body
and deleting the old provider/template mesh. W5A built the request boundary, but
the body still delegates to the old mesh. W5B must split.

## Proposed Wave-Graph Amendment

| Wave | Amended scope | Entry gate | Cap |
|---|---|---|---|
| W5A | Already admitted. Source-consuming runtime request boundary, source-fact parser, CSS/JSON/Sheets/BBNF proof. | W4 ledger close. | Closed at 921 augmented source/test LOC. |
| W5B-GEN | Provider-free runtime generator body. Replace provider-backed `render_runtime_profile`, `RuntimeProvider`, and the hard-coded profile registry with one request-driven generator path. CSS L4 and JSON bytes must be emitted from grammar source + workspace metadata, not provider modules, per-grammar templates, or committed generated output. | W5A admitted. | <=1.0k C-1 part-A source/test LOC; explicit cap expansion, cannot borrow W5C or W6. |
| W5C-DELETE | Provider/template deletion and post-W5 Lock 14 baseline close. Delete eight legacy providers, seven CSS template dirs, retired JSON template residue, and update Lock 14 baseline. | W5B-GEN admitted. | <=400 C-1 part-A deletion/baseline LOC. |
| W6 | PRUNE-4 unchanged in substance: W6.0 CSS L4 root-runtime collapse, W6.1-W6.8 remaining Pattern H dirs. | W5C-DELETE admitted. | Existing W6 cap preserved. |

W7 remains conditional on W6 close. W8/W9/W10 remain globally blocked until
PRUNE-1 through PRUNE-5 close.

## Required SPEC Repairs

- Section 8B becomes W5B-GEN, not deletion.
- New Section 8C becomes W5C-DELETE.
- W6 entry gate changes from W5B admitted to W5C-DELETE admitted.
- The generic-crate grep must use ripgrep-correct syntax and scan the intended
  skinny generic-codegen production surface.
- Dispatch prompt must forbid provider/template deletion before W5B-GEN admits.

## Cap Implication

REDRESS-210 proves the V5 cap was not honest for the actual work. W5A consumed
921 augmented source/test LOC; the remaining W5B <=400 deletion cap cannot also
build the missing provider-free generator body. V6 should make the expansion
explicit: W5B-GEN gets its own <=1.0k C-1 part-A generator cap, W5C-DELETE keeps
the <=400 deletion/baseline cap, and W6 remains unchanged at <=2.0k C-1 part-B.

## Proposed Dispatch Result

After G-Omega V6 CRUD, next executable wave is W5B-GEN. W5C, W6, W7, W8, W9,
and W10 remain blocked.
