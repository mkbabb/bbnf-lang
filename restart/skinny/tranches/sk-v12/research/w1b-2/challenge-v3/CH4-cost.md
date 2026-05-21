# SK-V12 W1b-2a CH4 V3 - Cost

Verdict: ACCEPT.

No CH4 blocker remains.

Accepted facts:

- V3 fixes the monolithic cost issue by splitting W1b-2a from W1b-2b.
- W1b-2a is limited to dependency, comparator, equality artifacts, and a
  Criterion row.
- dependency compile risk is fail-closed; it does not authorize owner-path
  broadening.
- report/gate/RESULTS work in this redress would reopen the V2 CH4 blocker and
  is not allowed.
