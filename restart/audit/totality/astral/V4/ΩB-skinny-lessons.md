# Omega-B Skinny Lessons - Pass Omega V4 W4R

Pass: Pass Omega V4.
Date: 2026-05-26.
Scope: skinny lessons consumed since Pass Omega V3.

## Verdict

ACCEPT.

The new skinny lesson is REDRESS-184: deletion waves cannot be sequenced before
their replacement generator exists when the same-wave consumer compiles through
the deletion target.

## Delta Since V3

| Commit | Lesson | Disposition |
|---|---|---|
| `45568e669` | W2 amended `regen-css` emits skinny runtime profiles only and still uses current `codegen::emit_runtime_profile`. | Valid W2R close; does not make provider deletion safe. |
| `b0a864f0b` | W3 staged production CSS L4 corpora and loader. | R5 closed; no CSS admit. |
| `4a32db45c` | W4 rejected because provider deletion removes W2's emitter before W5 replacement exists. | Triggers Pass Omega V4 W4R. |

## Longitudinal Lesson

REDRESS-183 and REDRESS-184 are the same governance pattern:

- REDRESS-183: W2 required root runtime generation before W6 owned the root
  generator.
- REDRESS-184: W4 requires provider deletion before W5 owns the replacement
  provider path.

Both are correctly handled by wave-graph amendment rather than by local
implementation workarounds.

## V1-Surface Impact

W4R does not alter row outcomes, locks, or architecture. It alters the dispatch
sequence so that:

- W4 prunes CSS L4 admission claims in ledgers and REDRESS.
- W5 deletes provider/template infrastructure only after standing up the
  grammar-neutral replacement path.

The lesson belongs in MASTER-PLAN §13.3, SK-V14 SPEC/SYNTHESIS, HANDOFF, and
MIGRATION. No ARCHITECTURE or LOCKS update is required.
