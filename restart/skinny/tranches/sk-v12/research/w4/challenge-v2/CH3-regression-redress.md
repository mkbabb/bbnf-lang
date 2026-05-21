# SK-V12 W4 CHALLENGE V2 - CH3 Regression And REDRESS

Verdict: ACCEPT.

PLAN-V2 closes the CH3 V1 blocker at planning level. It forbids using
REDRESS-125 as the W4 admission gate and requires a W4-specific
`sk-v12-w4-asm-css-v1` report with REDRESS-126, Lock 16, microbench,
post-W4 CSS numbers, JSON guard state, and orphan disposition fields.

Guardrails held:

- Current report/gate code is W1b-2b-only, so W4 cannot honestly reuse it.
- JSON guard escalation is coherent: retained W1a no-write root is allowed
  only for no-touch proof; edits to `report.rs`, `gate.rs`, or production
  `bbnf-simd` require a fresh populated JSON guard root unless CHALLENGE
  accepts a no-touch proof.
- W4 records evidence in REDRESS/artifacts; W5 owns final campaign-close
  `RESULTS.md` movement.
- W5 dependency coherence holds: W3 is not required for PASS-ADMIT on the
  REDRESS-125 candidate path, but remains required for FIXPOINT.
