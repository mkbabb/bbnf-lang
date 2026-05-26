# SK-V14 W5B.0 CH3: Regression

Date: 2026-05-26.
Scope: W5B.0 regression review.
Disposition: REVISE.

## Findings

W5B.0 has not landed in source. The current roster ends at W5A, the current
owner aggregation ends at W5A, and the parent-diff router has no W5B-FRONTEND
case. No W5C or W5D unblock was observed; the regression risk is omission, not
premature routing.

The W5A topology allowance still permits modified providers/templates, which
would reopen provider/template smuggling before W5C-GEN and W5D-DELETE.

## Required Folds

- Implement W5B.0 in `lock14_baseline.rs` only.
- Add W5B-FRONTEND owner routing and keep W5C/W5D rejected.
- Tighten the protected topology guard to reject `M`, `A`, `D`, `R`, and `??`
  statuses on protected providers/templates.
- Preserve the `grammar_provider.rs` neutral exception.
- Run and record all eight dedicated W5B.0 tests.
