# Omega-B Skinny Lessons - Pass Omega V5 W5R

Pass: Pass Omega V5.
Date: 2026-05-26.
Scope: skinny lessons consumed since Pass Omega V4.

## Verdict

ACCEPT.

REDRESS-209 extends the W2R/W4R lesson: deletion and generated-output claims
must be sequenced after the rebuild capability is load-bearing and executable.
W5 caught the next recurrence: a provider/template deletion gate cannot be
closed by centralizing static templates when the claimed replacement is a
source-consuming generator.

## Delta Since V4

| Commit | Lesson | Disposition |
|---|---|---|
| `cb16a2ea0` | W4R closed ledger-only CSS PRUNE; no provider/template deletion. | Valid W4 close. |
| `eb8884abc` | W5 research showed current provider dispatch is static and `regen-css` treats CSS source as freshness evidence. | Valid W5 research input. |
| `9471163d4` | W5 plan rejected static centralization. | Correct no-workaround route. |
| `952ec8173` | W5 seven-lens CHALLENGE accepted redress and forward CH3/CH5 lens fixes. | Mandatory W5 challenge satisfied. |
| `bf957ef03` | REDRESS-209 recorded the generator-capability gap and W5R corrective packet. | Triggers Pass Omega V5. |

## Longitudinal Lesson

Three SK-V14 REDRESS gates expose one governance pattern:

- REDRESS-183: W2 required root runtime regeneration before W6 owned the root
  runtime generator/collapse.
- REDRESS-184: W4 required provider deletion before W5 owned the replacement
  provider path.
- REDRESS-209: W5 required provider/template deletion and Lock 14 closure before
  a real source-consuming generator existed.

The correction is not to force a local implementation. The correction is to make
the rebuild-capability wave explicit, then delete only after that capability is
the same-wave consumer.

## Procedural Addenda For Future T-P3

- **NEW-CH3-V4-01**: CH3 REGRESSION must grep wave specs for delete-target /
  rebuild-capability pairs: "delete X", "X exists after wave N", "regen X", and
  "replacement of X". The rebuild-capability-source wave must precede the
  deletion wave unless the same wave first proves the replacement is
  load-bearing.
- **NEW-CH5-V4-01**: CH5 HIDDEN COUPLING must treat provider/template/runtime
  deletion as coupled to the code path compiling the same-wave consumer. If the
  consumer imports, includes, or profile-dispatches through the deletion target,
  the wave is a REJECT until replacement exists.

## V1-Surface Impact

W5R alters dispatch sequence and gate text. It does not alter row outcomes,
locks, architecture, substrate shape, BackendShape, or SIMD admissibility. The
lesson belongs in MASTER-PLAN §13.3, SK-V14 SPEC/SYNTHESIS/DISPATCH surfaces,
HANDOFF, MIGRATION, and limited skinny corpus wording.
