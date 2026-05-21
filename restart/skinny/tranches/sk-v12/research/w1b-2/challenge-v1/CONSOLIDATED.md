# SK-V12 W1b-2 CHALLENGE V1 Consolidated

Disposition: REJECT; route back to plan.

Six-lens result:

- CH1 correctness: REVISE.
- CH2 generality / Lock 14: REVISE.
- CH3 regression / REDRESS: REVISE.
- CH4 cost: REVISE.
- CH5 hidden coupling: REVISE.
- CH6 anti-paper-close: ACCEPT.

## Blocking Findings

1. The V1 comparator claim overstates lightningcss. Public lightningcss APIs
   can validate parse success and a limited AST declaration/property/importance
   projection, but they do not expose raw token equality, source byte offsets,
   or source declaration ordering for all important/normal cases.
2. Owner paths are incomplete for redress. Adding lightningcss changes
   `skinny/Cargo.lock`; persistent fact artifacts also need an authorized
   directory or must be ephemeral.
3. The documented JSON guard command is not real unless xtask forwards the new
   `--skv12-css-l4-sota-report` flag, or the plan uses `bbnf-bench --bin gate`
   directly.
4. The V1 redress scope is too broad for the stated budget unless dependency,
   comparator, report/gate, and Criterion evidence ingestion are narrowed and
   costed explicitly.
5. The frozen CSS fixture does not prove broad CSS source-order behavior. W1b-2
   must either add adversarial fixture coverage or record the comparator as
   fixture-limited in REDRESS and avoid general CSS SOTA overclaim.

## Required Plan Revision

The next plan must:

- Name the comparator as a frozen-fixture source-scanner fact stream gated by
  lightningcss parse success and best-effort AST checks, not raw lightningcss
  fact emission.
- Add `skinny/Cargo.lock` and the W1b artifact directory or declare artifacts
  ephemeral.
- Use commands runnable from `skinny/` or `--manifest-path skinny/Cargo.toml`.
- Decide between xtask passthrough ownership and direct `bbnf-bench --bin gate`
  invocation.
- Decide whether W1b-2 adds adversarial fixture coverage or explicitly limits
  REDRESS/admission language to the current fixture shape.
- Preserve the strict admission bar:
  `track1_mbps > lightningcss_mbps + 1`.
