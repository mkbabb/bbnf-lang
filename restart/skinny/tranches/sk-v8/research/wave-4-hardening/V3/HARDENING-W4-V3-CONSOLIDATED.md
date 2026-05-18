# SK-V8 W4 Hardening V3 Consolidated

Date: 2026-05-18.

Verdict: ACCEPT.

Panel:

| Reviewer | Verdict | Confidence |
|---|---|---:|
| CH1 | ACCEPT | 96% |
| CH2 | ACCEPT | 95% |
| CH3 | ACCEPT | 94% |
| CH4 | ACCEPT | 94% |
| CH5 | ACCEPT | 96% |
| CH6 | ACCEPT | 96% |

Result: 6/6 ACCEPT. This is the first qualifying accept cycle after the V2
REVISE.

## Accepted Disposition

W4's fail-closed rejection/routing disposition is coherent:

- The scalar-parent fold candidate failed selected-row gates: Apache passed,
  but `random` remained below sonic/1.10 and `numbers` failed while regressing
  +6.3287% Track 2 time.
- The source candidate is reverted.
- `skinny/RESULTS.md` remains unchanged and remains W0 authority.
- No W4-aware report gate or Lock 14 source allowance is added because no
  source or row-table admission survives.
- REDRESS 93 records the failed candidate and residual routing.
- HANDOFF no longer overclaims W4 closure or W5 activation.

## Required Next Step

Run an unchanged V4 challenge. W4 may close only if V4 returns a second
consecutive qualifying ACCEPT cycle.
