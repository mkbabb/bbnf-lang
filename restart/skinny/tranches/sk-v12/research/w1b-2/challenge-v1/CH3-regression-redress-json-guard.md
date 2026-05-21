# SK-V12 W1b-2 CH3 - Regression / REDRESS

Verdict: REVISE.

Blocker:

- The documented no-write JSON guard command uses
  `--skv12-css-l4-sota-report`, but `skinny/xtask/src/main.rs` currently
  forwards only `--w1a-non-json-report` and `--skv12-non-json-report` through
  `gate-json`. The W1b-2 owner list did not authorize xtask, so the command
  would fail before reaching the bench gate.

Non-blocking assessment:

- The distinct `sk-v12-css-l4-sota-v1` schema is the right shape because
  `lightningcss_mbps + 1` is not the W1b-1 baseline-relative threshold.
- RESULTS discipline is acceptable: measurement-only CSS evidence stays in the
  companion report and REDRESS. RESULTS changes only for CSS ADMIT or measured
  JSON guard demotion.

Required revision:

- Authorize and implement `skinny/xtask/src/main.rs` passthrough/test coverage
  for `--skv12-css-l4-sota-report`, or remove xtask from the required command
  and use the `bbnf-bench --bin gate` command directly.
