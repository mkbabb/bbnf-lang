# Pass Omega V8 CHALLENGE V2 CH3 Regression

Date: 2026-05-26.
Cycle: V2 after CH1 fold.
Lens: CH3 regression.
Scope: Omega-A through Omega-F, `locks-diff.md`, `master-plan-diff.md`,
prior CH3, REDRESS-209/210/211/212, and the W5B-FRONTEND V2 challenge archive.
Disposition: ACCEPT.

## Verdict

ACCEPT. The CH1 fold strengthens the V8 W5B-FRONTENDR packet with exact
construct ownership, exact W5B.0 Lock 14 tests, per-test/per-log nonzero proof,
and LOC accounting, but it does not change the CH3 ordering result. V8 still
does not reopen static centralization, premature provider/template deletion,
provider-free generation before frontend closure, the one-wave cap violation,
or any new-admit PRUNE bypass.

No CH3-specific revision is required.

## CH1 Fold Check

The prior CH1 rejection was exactness-only: the V8 packet needed owner file/type
per construct, exact positive/fail-closed tests, exact W5B.0 Lock 14 tests,
dedicated nonzero log proof, and redress/REDRESS LOC accounting
(`restart/audit/totality/astral/V8/hardening/CH1.md:21`-`48`).

Those requirements are now present in the V8 source packet:

- `master-plan-diff.md` adds the CH1 construct table, exact W5B.0 Lock 14
  tests, dedicated per-log nonzero proof, LOC accounting, exact no-diff
  maintain wording, and preserves the V7 prohibitions
  (`restart/audit/totality/astral/V8/master-plan-diff.md:68`-`114`).
- Omega-D carries the same exactness fold and keeps W5B.0 through W5B.3 as
  non-close progress only
  (`restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:58`-`112`).
- Omega-F carries the CH1 gate into dispatch/handoff and keeps W5B source edits
  behind admitted W5B.0 authority
  (`restart/audit/totality/astral/V8/ΩF-migration-handoff.md:51`-`85`,
  `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:121`-`140`).

These are stricter proof gates inside W5B. They do not authorize W5C-GEN,
W5D-DELETE, W6, W7, or any new-admit wave early.

## Regression Matrix

| Regression route | Result | Basis |
|---|---:|---|
| Static centralization | ACCEPT | REDRESS-209 remains the rejection of moving the old static provider/template mesh under a new shape (`skinny/REDRESS.md:5173`-`5183`). Omega-B repeats that centralization is not a replacement generator (`restart/audit/totality/astral/V8/ΩB-skinny-lessons.md:25`), and the CH1 fold preserves "no provider-free generator body replacement in W5B" plus no grammar-name branches (`restart/audit/totality/astral/V8/master-plan-diff.md:109`-`114`). |
| Premature deletion | ACCEPT | REDRESS-210 remains the rejection of deleting providers/templates before a provider-free generator body exists (`skinny/REDRESS.md:5197`-`5204`). V8 keeps W5D-DELETE blocked until W5C-GEN and explicitly forbids provider/template deletion in W5B (`restart/audit/totality/astral/V8/master-plan-diff.md:40`-`42`, `restart/audit/totality/astral/V8/master-plan-diff.md:109`-`110`). |
| Provider-free generation before frontend closure | ACCEPT | REDRESS-211 remains the rejection of provider-free generation before generic frontend/import/IR closure (`skinny/REDRESS.md:5221`-`5244`). V8 keeps W5C-GEN after aggregate W5B-FRONTEND close and assigns W5C, not W5B, to the provider-free runtime generator body (`restart/audit/totality/astral/V8/master-plan-diff.md:11`-`26`, `restart/audit/totality/astral/V8/ΩB-skinny-lessons.md:44`-`52`). |
| W5B one-wave cap violation | ACCEPT | REDRESS-212 is still the rejection of one capped W5B-FRONTEND for serial Lock 14, import, lowering, projection, and consumer proof work (`skinny/REDRESS.md:5249`-`5272`). V8 replaces the one slot with formal W5B.0 through W5B.4 sub-waves, <=30 min per sub-wave and <=150 min aggregate (`restart/audit/totality/astral/V8/master-plan-diff.md:30`-`39`, `restart/audit/totality/astral/V8/master-plan-diff.md:47`-`52`). |
| New-admit PRUNE bypass | ACCEPT | REDRESS-209/210/211/212 all keep W8/W9/W10 blocked by the PRUNE chain (`skinny/REDRESS.md:5187`-`5188`, `skinny/REDRESS.md:5210`-`5211`, `skinny/REDRESS.md:5241`-`5245`, `skinny/REDRESS.md:5268`-`5272`). V8 repeats that W7 and W8/W9/W10 remain blocked by the full PRUNE chain (`restart/audit/totality/astral/V8/master-plan-diff.md:43`, `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:16`-`18`, `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:87`-`101`). |

## Locks And Authority

The locks posture remains zero-delta. `locks-diff.md` says no `LOCKS.md`
changes are proposed, REDRESS-212 does not add public syntax, BIR,
BackendShape, substrate surface, or a lock, and the expected lock count remains
16 (`restart/audit/totality/astral/V8/locks-diff.md:3`-`17`). Omega-C reaches
the same result and routes the CH1/CH2/CH5 details to W5B.0/SPEC execution
requirements rather than a lock amendment
(`restart/audit/totality/astral/V8/ΩC-locks-amendments.md:9`-`22`,
`restart/audit/totality/astral/V8/ΩC-locks-amendments.md:51`-`83`).

Omega-A also keeps ARCHITECTURE and LOCKS read/no-op absent unexpected public
syntax, substrate, BackendShape, or Lock 14 evidence
(`restart/audit/totality/astral/V8/ΩA-coherence-audit.md:11`-`21`,
`restart/audit/totality/astral/V8/ΩA-coherence-audit.md:112`-`134`).

## Required CH3 Folds

None. Carry the V2-after-CH1 V8 folds exactly:

1. W5B.0 LOCK14-GATE dispatches first and remains authority-only.
2. W5B.1 through W5B.4 stay serial and cannot bypass the admitted predecessor.
3. W5B-FRONTEND closes only at W5B.4 with same-commit consumer evidence.
4. W5C-GEN remains blocked until aggregate W5B-FRONTEND close.
5. W5D-DELETE remains blocked until W5C-GEN closes; W6 remains blocked until
   W5D-DELETE closes.
6. W7 and W8/W9/W10 remain blocked by the full PRUNE chain.
7. Preserve zero-delta locks and no public syntax, BIR, BackendShape,
   substrate, provider/template deletion, or provider-free generator-body
   replacement inside W5B.

Final CH3 result: ACCEPT.
