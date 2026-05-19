# Handoff SK-V10

Date: 2026-05-19.

Status: S-P1 Profile closed after V1 hardening under
`HARDENING-S-P1-V1-CONSOLIDATED`. S-P2 Research closed after V1 hardening under
`HARDENING-S-P2-V1-CONSOLIDATED`; `p2g-candidate-ledger.md` is the post-CHALLENGE
candidate-pool authority. S-P3 closed through V3 confirmation challenge. W3
union substrate is retired as falsified. The primary JSON frontier is
`direct_to_struct`; typed product-plane generalization is the bounded second
route; parse-only SOTA is retired from the close target. W0 telemetry freeze is
closed under REDRESS 99, W1 direct contract is closed under REDRESS 100, W2
direct row-table reclamation is closed under REDRESS 101, and W3 parse-only
firewall is closed under REDRESS 102. W4 `instruments` typed product admission
is rejected under REDRESS 103, and W5 root-type typed generalization proof is
closed under REDRESS 104. W6 `github_events` root typed row admission is closed
under REDRESS 105. W7 full-string primitive micro-proof is rejected under
REDRESS 106. W8 hex escape micro-proof is closed under REDRESS 107. W9
existing-call-site kernel production is rejected under REDRESS 108. W10
`instruments` direct residual admission is closed under REDRESS 109. Close is
closed under REDRESS 110; SK-V10 is converged.

## 1. Read First

1. `restart/prompts/pass-contracts/PASS-ALPHA.md`
2. `restart/skinny/tranches/sk-v9/research/alpha/alpha-G-dispatch-sk-v10.md`
3. `restart/skinny/tranches/sk-v9/HANDOFF.md`
4. `restart/skinny/tranches/sk-v9/SPEC.md`
5. `restart/skinny/tranches/sk-v9/DISPATCH-PROMPT.md`
6. `skinny/RESULTS.md`
7. `skinny/REDRESS.md` entries 94-110
8. `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
9. `restart/skinny/tranches/sk-v10/research/alpha/alpha-A-results-extraction.md`
10. `restart/skinny/tranches/sk-v10/research/alpha/alpha-B-competitor-deltas.md`
11. `restart/skinny/tranches/sk-v10/research/alpha/alpha-C-redress-digest.md`
12. `restart/skinny/tranches/sk-v10/research/alpha/alpha-D-validated-invalidated.md`
13. `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md`
14. `restart/skinny/tranches/sk-v10/research/alpha/alpha-F-contract-draft.md`
15. `restart/skinny/tranches/sk-v10/research/alpha-hardening/V1/CONSOLIDATED.md`
16. `restart/skinny/tranches/sk-v10/research/g-alpha/G-ALPHA-PRESENTATION.md`
17. `restart/skinny/tranches/sk-v10/research/p1/p1a-samply-mode-1.md`
18. `restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md`
19. `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md`
20. `restart/skinny/tranches/sk-v10/research/p1/p1d-pmu-cycles.md`
21. `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md`
22. `restart/skinny/tranches/sk-v10/research/p1/p1f-results-delta.md`
23. `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
24. `restart/skinny/tranches/sk-v10/research/p2/p2a-sota-teardown.md`
25. `restart/skinny/tranches/sk-v10/research/p2/p2b-dav1d-process.md`
26. `restart/skinny/tranches/sk-v10/research/p2/p2c-arch-esoterica.md`
27. `restart/skinny/tranches/sk-v10/research/p2/p2d-substrate-tape.md`
28. `restart/skinny/tranches/sk-v10/research/p2/p2e-parse-that-gaps.md`
29. `restart/skinny/tranches/sk-v10/research/p2/p2f-grammar-neutral.md`
30. `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`
31. `restart/skinny/tranches/sk-v10/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
32. `restart/skinny/tranches/sk-v10/SPEC.md`
33. `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
34. `restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md`
35. `restart/skinny/tranches/sk-v10/research/w0/w0-research.md`
36. `restart/skinny/tranches/sk-v10/research/w0/w0-plan.md`
37. `restart/skinny/tranches/sk-v10/research/w0/w0-redress.md`
38. `restart/skinny/tranches/sk-v10/research/w1/w1-research.md`
39. `restart/skinny/tranches/sk-v10/research/w1/w1-plan.md`
40. `restart/skinny/tranches/sk-v10/research/w1/hardening/CHALLENGE-W1-CONSOLIDATED.md`
41. `restart/skinny/tranches/sk-v10/research/w1/w1-redress.md`
42. `restart/skinny/tranches/sk-v10/research/w2/w2-research.md`
43. `restart/skinny/tranches/sk-v10/research/w2/w2-plan.md`
44. `restart/skinny/tranches/sk-v10/research/w2/w2-redress.md`
45. `restart/skinny/tranches/sk-v10/research/w3/w3-research.md`
46. `restart/skinny/tranches/sk-v10/research/w3/w3-plan.md`
47. `restart/skinny/tranches/sk-v10/research/w3/w3-redress.md`
48. `restart/skinny/tranches/sk-v10/research/w4/w4-research.md`
49. `restart/skinny/tranches/sk-v10/research/w4/w4-plan.md`
50. `restart/skinny/tranches/sk-v10/research/w4/hardening/CHALLENGE-W4-CONSOLIDATED.md`
51. `restart/skinny/tranches/sk-v10/research/w4/w4-redress.md`
52. `restart/skinny/tranches/sk-v10/research/w5/w5-research.md`
53. `restart/skinny/tranches/sk-v10/research/w5/w5-plan.md`
54. `restart/skinny/tranches/sk-v10/research/w5/hardening/CHALLENGE-W5-CONSOLIDATED.md`
55. `restart/skinny/tranches/sk-v10/research/w5/w5-redress.md`
56. `restart/skinny/tranches/sk-v10/research/p3/root-typed-proof/ROOT-TYPED-PROOF.md`
57. `restart/skinny/tranches/sk-v10/research/w6/w6-research.md`
58. `restart/skinny/tranches/sk-v10/research/w6/w6-plan.md`
59. `restart/skinny/tranches/sk-v10/research/w6/hardening/CHALLENGE-W6-CONSOLIDATED.md`
60. `restart/skinny/tranches/sk-v10/research/w6/w6-redress.md`
61. `restart/skinny/tranches/sk-v10/research/w7/w7-research.md`
62. `restart/skinny/tranches/sk-v10/research/w7/w7-plan.md`
63. `restart/skinny/tranches/sk-v10/research/w7/hardening/CHALLENGE-W7-CONSOLIDATED.md`
64. `restart/skinny/tranches/sk-v10/research/w7/w7-redress.md`
65. `restart/skinny/tranches/sk-v10/research/w8/w8-research.md`
66. `restart/skinny/tranches/sk-v10/research/w8/w8-plan.md`
67. `restart/skinny/tranches/sk-v10/research/w8/hardening/CHALLENGE-W8-CONSOLIDATED.md`
68. `restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/W8-ESCAPE-MICROPROOF.md`
69. `restart/skinny/tranches/sk-v10/research/w8/w8-redress.md`
70. `restart/skinny/tranches/sk-v10/research/w9/w9-research.md`
71. `restart/skinny/tranches/sk-v10/research/w9/w9-plan.md`
72. `restart/skinny/tranches/sk-v10/research/w9/hardening/CHALLENGE-W9-CONSOLIDATED.md`
73. `restart/skinny/tranches/sk-v10/research/w9/w9-redress.md`
74. `restart/skinny/tranches/sk-v10/research/w10/w10-research.md`
75. `restart/skinny/tranches/sk-v10/research/w10/w10-plan.md`
76. `restart/skinny/tranches/sk-v10/research/w10/hardening/CHALLENGE-W10-CONSOLIDATED.md`
77. `restart/skinny/tranches/sk-v10/research/w10/w10-redress.md`
78. `restart/skinny/tranches/sk-v10/research/close/close-research.md`
79. `restart/skinny/tranches/sk-v10/research/close/close-plan.md`
80. `restart/skinny/tranches/sk-v10/research/close/close-redress.md`

## 2. Current State

SK-V9 closed its live implementation path with W1 admitted, W2 admitted, W3
rejected twice with measurement, class-lane-only rejected at CHALLENGE, and
REDRESS 98 retiring W3.

Current `skinny/RESULTS.md` state:

| Family | State |
|---|---|
| `parse_only` | 17 `S / NO-GO` |
| `direct_to_struct` | 6 `A / GO`, 11 `N-direct / NO-GO`; W10 `instruments` admitted |
| `real_typed_struct` | 7 `A / GO`; W4 `instruments` rejected by Track 2 floor miss, W6 `github_events` admitted |

## 3. Candidate Boundaries

| Candidate | Boundary |
|---|---|
| Direct output/control-path contract | Primary JSON frontier. Required before any direct digest row is promoted beyond guard-plane evidence; S-P1 must profile direct rows specifically. |
| `instruments` typed product admission | First typed generalization target; fixed top-level object shape fits the current typed DirectBuild schema model. |
| Root-type typed generalization | Required before `github_events` and `gsoc-2018` typed rows, because their root shapes are not current named-struct roots. |
| Existing-substrate unicode/string kernels | May target current string/unescape call sites only; W3 cannot be named as consumer; must pass micro-prove-first before production. W7 full-string proof failed; W8 C6 hex escape proof passed for `unescape_string`; W9 rejected production consumption because the caller was already wired and row floors failed. |
| Same-run sidecar freshness manifest | Gate-only evidence ingestion; no behavior movement by itself. |
| SK-V10 telemetry refresh | Gate-only baseline refresh; no row movement without same-wave behavior gate. |
| Pass Omega lock amendment | Route substrate-ceiling falsification into a lock amendment so future SK cycles do not reopen W3 without micro-proof and a live output-plane target. |
| Totality T-P1 | Route CSS L4 / Sheets / BBNF-self profiling to the totality track after Alpha closes. |

## 4. Next Move

1. Dispatch Pass Omega for the REDRESS 98 substrate-ceiling lock amendment.
2. Dispatch the totality track for CSS L4 / Sheets / BBNF-self grammar
   generalization risk.
3. Start any further JSON behavior work only from a new dispatch packet; SK-V10
   is closed.

## 5. Refusal Conditions

Refuse dispatch if a requested wave:

- reopens W3 or a renamed union substrate;
- cites W3 as W4's consumer or entry gate;
- treats parse-only `S / NO-GO` rows as SOTA admissions;
- scopes a substrate/kernel wave without same-host micro-proof;
- admits Canada typed rows without full fixture proof;
- edits generic crates, codegen, or runtime outside JSON with JSON-only policy
  or without a named CSS L4 / Sheets / BBNF-self proof;
- edits source before S-P3 names owner paths, gates, and revert protocol;
- weakens `gate-json` to admit missing comparator/run-id/provenance evidence.

## 6. Close Posture

SK-V10 Alpha, S-P1 Profile, S-P2 Research, S-P3 Synthesis-Plan, W0 telemetry
freeze, W1 direct contract, W2 direct reclamation, and W3 parse-only firewall
are closed, W4 is rejected under REDRESS 103, W5 is closed under REDRESS 104,
W6 is closed under REDRESS 105, W7 is rejected under REDRESS 106, W8 is closed
under REDRESS 107, W9 is rejected under REDRESS 108, and W10 is closed under
REDRESS 109. Close is closed under REDRESS 110. No further direct row movement
is authorized in SK-V10.
