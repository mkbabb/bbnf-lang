# Omega-F Migration And Handoff - Pass Omega V6 W5BR

Pass: Pass Omega V6.
Date: 2026-05-26.
Scope: migration and next dispatch after REDRESS-210.

## Verdict

ACCEPT-WITH-PATCH.

V6 should record REDRESS-210 and make W5B-GEN the next executable wave after
G-Omega authorization. W5C-DELETE, W6, W7, and W8/W9/W10 remain blocked.

## Migration Impact

No public API migration lands in V6. The implementation migration is future
wave-local:

- W5B-GEN migrates codegen from provider-backed renderers to provider-free
  request-driven generation.
- W5C-DELETE removes the old provider/template files and tightens Lock 14.

## Handoff Directive

After V6 CRUD:

1. Dispatch W5B-GEN research/plan/challenge/redress under amended SPEC.
2. Do not delete provider/template files during W5B-GEN.
3. W5C-DELETE opens only after W5B-GEN admits all same-wave consumers.
4. W6 opens only after W5C-DELETE closes.
5. W8/W9/W10 remain globally blocked until the PRUNE chain closes.

## CRUD Proposal

| CRUD | Operation |
|---|---|
| CRUD-1 ARCHITECTURE | Read/no-op. |
| CRUD-2 MASTER-PLAN | Update wave graph rows. |
| CRUD-3 LOCKS | Read/no-op. |
| CRUD-4 HANDOFF + MIGRATION | Record REDRESS-210 and next dispatch. |
| CRUD-5 SKINNY CORPUS | Limited alignment on INDEX/WORKSPACE/HARDENING/COMPILER. |
| CRUD-6 AUDIT + CLEANUP | Write V6 CRUD-LOG and G-Omega signoff after authorization. |
