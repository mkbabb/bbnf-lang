# SK-V12 S-P1 Hardening PIN-V5 Consolidation

Pass: S-P1 Profile. Cycle: PIN-V5 CHALLENGE.
Date: 2026-05-20.
Scope: consolidate the fifth user-pin S-P1 hardening cycle after PIN-V4's
first all-ACCEPT cycle.

## Lens Dispositions

| Lens | Disposition | Score | Required fold |
|---|---|---:|---|
| CH1 correctness | ACCEPT | 98% | None. |
| CH2 generality / Lock 14 | ACCEPT | 98% | None. |
| CH3 regression / REDRESS | ACCEPT | 98% | None. |
| CH4 cost / replayability | ACCEPT | 98% | None. |
| CH5 hidden coupling | REVISE | 88% | Demote stale pre-pin S-P1 convergence and SPEC profile authority paths. |
| CH6 anti-paper-close | ACCEPT | 97% | None. |

PIN-V5 is five ACCEPT and one REVISE. It does not satisfy §3Z convergence and
breaks the consecutive-clean-cycle count after PIN-V4.

## Fold Applied

The orchestrator fold after PIN-V5:

- rewrote `research/p1/hardening/HARDENING-S-P1-CONVERGED.md` from a live
  pre-pin convergence claim into a pin-aware S-P1 status document that marks
  the earlier convergence superseded and names the pin capture source, capture
  root, build root, replay TSV, status TSVs, and self-time TSVs as authority;
- updated `SPEC.md` so its authority list names
  `skv12-p1-pin-replay.tsv`, not the pre-pin replay TSV;
- rebound W0's profile-lock task from the pre-pin source/root pair to the
  pin-aware capture source `cf7848b2`, `/tmp/skv12-pin-p1`,
  `/tmp/skv12-pin-profile-target-cf7848b2`, the pin replay TSV, and the pin
  self-time TSVs;
- clarified that the current `SPEC.md` is pre-pin S-P3 context until the
  pin-aware S-P1 -> S-P2 -> S-P3 sequence rewrites it.

## Advancement

PIN-V5 routes to a new challenge cycle. Because PIN-V5 is REVISE, S-P1 now
needs two new consecutive all-ACCEPT cycles after this fold before S-P2
dispatch.
