# SK-V12 W1b-2 CH4 V2 - Cost

Verdict: REVISE.

Blockers:

1. PLAN-V2 still bundles dependency/lockfile admission, comparator
   implementation, fixture-limit enforcement, three-way artifacts, new schema,
   new gate flag, admission classification, Criterion ingestion or ADMIT
   suppression, and tests under one <=30 min redress cap.
2. Current report/gate code has no W1b-2 lightningcss fields or report flag.
3. Current CSS report values are quick-loop smoke values, not Criterion sample
   estimates. A plan that allows admission must implement Criterion ingestion;
   otherwise it must explicitly forbid CSS ADMIT in this sub-wave.
4. Dependency compile weight is not bounded by a preflight or fallback.

Required revision:

- Split comparator/equality/benchmark-row landing from admission-grade
  report/gate/Criterion ingestion, or otherwise narrow the exit so W1b-2 cannot
  admit until the W1b-2-specific gate exists.
