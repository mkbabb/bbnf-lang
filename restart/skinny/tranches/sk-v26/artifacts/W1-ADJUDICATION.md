# W1 public-API adjudication

Task: `/root/skv26_w1_final_adjudicator`.

Subject:

- parse-that commit `9c9714dfdfa6d33b70f5738c311a9abccb4a1bdf`;
- tree `76141a76dee446249233056e94ea9b043968bdd4`.

Final disposition: `ACCEPT`.

The fresh adjudicator found no concrete W1 blocker and ruled that both hostile
review acceptances were justified. It independently replayed:

- 37 kernel laws and one allocation-law process;
- 16 table-plan laws;
- 7 real compiler/runtime bridge laws;
- the all-target/all-feature locked workspace;
- strict workspace and bridge Clippy;
- warning-denying rustdoc;
- clean package verification for both crates;
- clean status, subject identity, and `git diff --check`.

Source inspection confirmed rollback-before-validation, typed localization of
foreign outcomes, allocation-free borrowed nonempty provenance, distinct empty
provenance, a shared exactly-256-class compiler/runtime domain, iterative
validation before recursive HIR lowering, one explicit byte-oriented syntax
policy, and no production `parse_that` unsafe code or allocating container.

Residual non-blockers:

1. Miri is unavailable for the pinned nightly; production `parse_that` has no
   unsafe code.
2. `EventSink` rollback fidelity is necessarily a trait contract for external
   implementations.
3. A caller may deliberately raise construction resource limits beyond the safe
   defaults.
4. Empty-source token exhaustion can panic only after exhausting the process
   `usize` identity space; it cannot silently alias.

Clean package archives observed by the adjudicator:

- `parse_that`: SHA-256
  `4c30d451e42fef441fab29afed4394e142a71b293dc9c4fe301a3b93e8c92293`;
- `bbnf-regex`: SHA-256
  `c9ca32cf4022dbdd238387f9600bc7502d8de3a0f8c6f725a4f289d4e3499d50`.

This adjudication made no edits.
