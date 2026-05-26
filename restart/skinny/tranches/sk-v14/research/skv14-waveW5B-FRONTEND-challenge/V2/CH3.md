# SK-V14 W5B-FRONTEND CHALLENGE V2 CH3 Regression

Date: 2026-05-26.
Lens: CH3 Regression.
Disposition: ACCEPT.

## Findings

1. CH3 accepts the V2 non-admit maintain fold. V1 required an executable
   maintain proof at `skv14-waveW5B-FRONTEND-challenge/V1/CH3.md:12` and
   `skv14-waveW5B-FRONTEND-challenge/V1/HARDENING-SKV14-W5B-FRONTEND-V1-CONSOLIDATED.md:43`.
   V2 adds the non-admit byte-identical maintain contract at
   `skv14-W5B-FRONTEND-plan.md:116` and no-diff gates at `:151`, `:168`, and
   `:169`.
2. REDRESS-209, REDRESS-210, and REDRESS-211 remain closed. The plan keeps W5B
   to frontend/import/IR only at `skv14-W5B-FRONTEND-plan.md:13`, preserves
   provider rendering at `:77`, assigns provider-free generation to W5C at
   `:78`, and blocks provider/template deletion at `:234`. That matches
   REDRESS history at `skinny/REDRESS.md:5173`, `:5197`, and `:5221`.
3. NEW-CH3-V4/V6/V7 ordering holds. Dispatch requires rebuild before delete,
   request-boundary versus provider-free separation, and frontend/import/IR
   versus provider-free separation at `DISPATCH-PROMPT.md:184`, `:186`, and
   `:188`; V2 preserves this at `skv14-W5B-FRONTEND-plan.md:17`, `:49`, and
   `:236`.
4. No W5C/W5D borrowing is found. SPEC bars borrowing at `SPEC.md:244` and
   `SPEC.md:751`; the plan repeats fail-closed sub-slice routing at
   `skv14-W5B-FRONTEND-plan.md:47`.

## Required Fold

None for CH3. Redress must capture named no-diff/maintain gates, and any diff
or provider/template movement routes to W5B revert/REDRESS rather than W5C/W5D
borrowing.
