# SK-V12 S-P3 PIN-V2 CHALLENGE Consolidated

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V2.
Date: 2026-05-20.
Packet under review: commit `7316d87b`.
Output: this file consolidates CH1-CH6.

## Disposition

REVISE.

Five lenses pass. CH1 finds one blocking correctness ambiguity in the W1b-1
fallback wording. The packet must fold that before a clean S-P3 convergence
cycle.

| Lens | Disposition | Confidence | Result |
|---|---|---:|---|
| CH1 correctness | REVISE | 88% | FAIL |
| CH2 generality / Lock 14 | PASS | High | PASS |
| CH3 regression / REDRESS | PASS | 96% | PASS |
| CH4 cost / caps | ACCEPT | 93% | PASS |
| CH5 hidden coupling | ACCEPT | 92% | PASS |
| CH6 anti-paper-close | PASS | 97% | PASS |

## Blocking Finding

### CH1-1 - W1b-1 can be read as unlocking fallback before W1b-2 measured CSS redress

`SPEC.md` defines W1b-1 as the CSS generated Track 1 plus independent-oracle
scaffold, not the lightningcss comparator/admission attempt. Its BLOCKED/FAIL
wording says Sheets/BBNF fallback requires a later plan revision "after this
measured CSS redress attempt." That can be read as allowing fallback after a
W1b-1 scaffold/generation failure.

This conflicts with the user pin and with the rest of PIN-V2, which key
fallback eligibility to a measured W1b-2 CSS lightningcss comparator/admission
redress attempt. W1b-1 may record REDRESS for scaffold failure, but it must not
satisfy the post-CSS-redress fallback condition.

Required fold:

- Revise W1b-1 BLOCKED/FAIL text in `SPEC.md`.
- State that W1b-1 scaffold failure records REDRESS and returns to plan, but
  Sheets/BBNF fallback remains blocked until W1b-2 records measured CSS
  lightningcss comparator/admission redress, unless the user re-pins or S-P3
  explicitly revises topology.

## Accepted Findings

- CH2: Lock 14 legality is sequenced before CSS emission; generic policy leaks,
  fallback ordering, and public API/substrate expansion remain fail-closed.
- CH3: JSON guard floors, REDRESS 96/97/98 and 88/89/90 material-differential
  handling, REDRESS 111-120 blockers, FIXPOINT evidence, revert protocols, and
  stale-measurement gates are sufficient.
- CH4: Phase caps, W1b split, owner path breadth, generated-size budgets, W3
  attempt scope, and W4 orphan accounting are cost-safe enough to dispatch once
  CH1 is folded.
- CH5: W2/W1b-1 SIMD coupling, shared-file race handling, substrate
  cardinality, public API escape hatches, zero-orphan accounting, and same-wave
  consumer coupling are bounded.
- CH6: The packet does not permit CSS close without strict
  `track1_mbps > lightningcss_mbps + 1`, strict equality, independent oracle,
  gate consumption, zero orphans, Lock 14/16, and measured union/ASM-gen
  evidence for FIXPOINT.

## Verdict

PIN-V2 does not converge. Fold CH1-1 and rerun a clean six-lens cycle.
