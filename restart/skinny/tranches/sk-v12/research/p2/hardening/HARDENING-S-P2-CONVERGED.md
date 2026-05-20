# SK-V12 S-P2 Hardening Status

Pass: S-P2 Research.
Date: 2026-05-20.
Status: PRE-PIN CONVERGENCE SUPERSEDED; PIN S-P2 HARDENING IN PROGRESS.

The pre-pin `HARDENING-S-P2-V2-CONSOLIDATED.md` and
`HARDENING-S-P2-V3-CONSOLIDATED.md` accepted a research surface that is no
longer authoritative after `USER-PIN-W1-CSS-L4-SOTA.md`. The pin-aware S-P2
research cohort opened at commit `8017a90b`. PIN-V1 folded to Cycle V2 at
`31859478`; PIN-V2 found one remaining CH4 accounting defect and is now folded
to Cycle V3.

PIN-V1 disposition:

| Lens | Verdict | Score | Action |
|---|---:|---:|---|
| CH1 correctness | REVISE | 86 | Fold inventory/candidate split and fix resolving external anchors. |
| CH2 generality / Lock 14 | ACCEPT | 96 | No required fold. |
| CH3 regression / REDRESS | ACCEPT | 96 | No required fold. |
| CH4 cost / scalar-reference / checkasm | REVISE | 78 | Fold per-row parity, consumer, micro-proof, and orphan disposition. |
| CH5 hidden coupling / substrate | ACCEPT | 96 | No required fold. |
| CH6 anti-paper-close | ACCEPT | 96 | No required fold. |

PIN-V2 disposition:

| Lens | Verdict | Score | Action |
|---|---:|---:|---|
| CH1 correctness | ACCEPT | 96 | No required fold. |
| CH2 generality / Lock 14 | ACCEPT | 97 | No required fold. |
| CH3 regression / REDRESS | ACCEPT | 97 | No required fold. |
| CH4 cost / scalar-reference / checkasm | REVISE | 89 | Fold explicit CH4 accounting into P2-B, P2-D, and P2-F. |
| CH5 hidden coupling / substrate | ACCEPT | 96 | No required fold. |
| CH6 anti-paper-close | ACCEPT | 97 | No required fold. |

Folded V3 surface:

- P2-A C1-C7 now carry per-row class, scalar reference, checkasm/parity,
  micro-proof, same-wave consumer, `escape_mask_64`/Lock 16 prerequisite, and
  orphan disposition columns. C6 is explicitly output-plane/oracle contract;
  C7 is generated-template legality surface, not a standalone parser row mover.
- P2-B keeps the scalar-oracle-first process, uses resolving dav1d `msac.c`
  anchors for cloned-state and benchmark-process claims, and now carries
  per-row micro-proof / explicit N/A plus orphan disposition columns.
- P2-C now distinguishes selectable PIN-V1 candidates from
  inventory/support/nonselectable ARM rows. The selectable set is C1, C3, C4,
  C5, and C6. C2, C9, and C11 are inventory/drop in this cycle; C7, C8, C10,
  and C12 are support-only until a same-wave consumer and required
  `escape_mask_64`/REDRESS material differentials exist.
- P2-D remains a no-shortlist substrate artifact and now carries explicit
  checkasm/parity N/A or parity requirements for every diagnostic/rejected row.
- P2-E now labels parser candidates separately from output-plane/oracle
  accounting rows, with per-row checkasm/parity and orphan/Lock 16 disposition.
- P2-F now states that inventory/drop, support-only, diagnostic-only, and
  parser-candidate-ineligible rows are outside the current S-P3 candidate pool
  unless a later folded pass adds fresh P1 evidence, scalar oracle, micro-proof,
  and same-wave consumer; it also includes a CH4 accounting supplement for
  support/oracle/accounting families.

The next required action is PIN-V3 CHALLENGE over the folded V3 research
packet. S-P2 is not converged under the user pin until §3Z records two
consecutive clean challenge cycles after the last REVISE reset.
