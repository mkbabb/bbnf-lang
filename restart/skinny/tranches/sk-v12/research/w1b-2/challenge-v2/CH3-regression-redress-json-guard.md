# SK-V12 W1b-2 CH3 V2 - Regression / REDRESS

Verdict: REVISE.

Blockers:

1. SPEC Section 7 does not explicitly bind `PASS-MEASURED-BASELINE` to zero
   `skinny/RESULTS.md` movement, even though PLAN-V2 does. Because RESULTS is
   in the owner table, the SPEC needs the same no-move rule.
2. The JSON guard command points `CRITERION_HOME` at a fresh W1b-2 directory
   but does not populate JSON Criterion data there. The plan must either run a
   JSON guard capture first or point at an accepted existing JSON Criterion
   root.
3. Existing stale-results guidance still mentions the xtask path. If W1b-2
   uses `bbnf-bench --bin gate` directly, the gate message must be updated or
   the plan must require that update.
4. The new `--skv12-css-l4-sota-report` flag must reject `--update-results`,
   `--write-results`, and volatile probe flags the same way other companion
   report flags do.
