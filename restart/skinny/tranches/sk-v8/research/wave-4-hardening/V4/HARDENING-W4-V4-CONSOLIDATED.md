# SK-V8 W4 Hardening V4 Consolidated

Date: 2026-05-18.

Verdict: ACCEPT.

Panel:

| Reviewer | Verdict | Confidence |
|---|---|---:|
| CH1 | ACCEPT | 97% |
| CH2 | ACCEPT | 96% |
| CH3 | ACCEPT | 95% |
| CH4 | ACCEPT | 95% |
| CH5 | ACCEPT | 97% |
| CH6 | ACCEPT | 97% |

Result: 6/6 ACCEPT. This is the second consecutive qualifying accept cycle
after V2 REVISE, following W4 V3's 6/6 ACCEPT.

## Accepted Disposition

W4 closes as rejected/routed:

- The scalar-parent fold candidate remains rejected. Apache cleared the
  selected row gate, but `random` remained below sonic/1.10 and `numbers`
  regressed by +6.3287% Track 2 time.
- The source patch is reverted and only archived at
  `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch`.
- `skinny/RESULTS.md` remains unchanged and remains W0 authority.
- No W4-aware report gate or Lock 14 W4 parent-diff allowance is added because
  no source or row-table admission survives.
- REDRESS 93 records the failed candidate, the strict same-run row evidence,
  and the residual direct-output-contract/control-path routing.
- HANDOFF may now move W4 from pending convergence to closed/routed and make
  W5 the next active wave under its own gates.

## Required Folds

Fold this closure into `restart/skinny/tranches/sk-v8/HANDOFF.md` and update
the W4 plan disposition text to cite V3+V4 hardening convergence. Do not update
`skinny/RESULTS.md`, `direct_struct.rs`, or Lock 14 source allowances for W4.
