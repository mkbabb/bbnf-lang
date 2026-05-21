# SK-V12 W1b-2a CH2 V3 - Generality / Lock 14

Verdict: ACCEPT.

No CH2 blocker remains.

Accepted facts:

- owner paths are constrained to comparator scope;
- the dependency is direct to `bbnf-bench` and pinned exactly;
- only nested `skinny/Cargo.lock` is authorized;
- report/gate/RESULTS are excluded from W1b-2a;
- no generic crate, directive, BIR, `BackendShape`, or public substrate API is
  authorized.
