# CH4 Cost Challenge - SK-V8 Alpha V2

Date: 2026-05-17.
Lens: CH4 Cost.
Scope: final SK-V8 tranche docs plus V1 CH4 and V1 consolidated findings.

Overall disposition: REVISE.

The final SK-V8 docs resolve the main cost-shape risks from V1: only W0 is
dispatchable after G-Alpha, W0 is telemetry-only, CostFacts gate binding moves
before behavior work, W2-W4 are conditional on post-W0/W1 plan updates, bitmap
asm is rejected as a default route, and every SPEC wave now has a revert
protocol.

The packet is not a CH4 reject. It does not authorize the broad alpha-F behavior
waves that V1 challenged. It still is not CH4 ACCEPT because the final docs do
not close the explicit cost accounting requested by V1: source LOC caps are
absent, hard caps are not defined as inclusive of verification and docs work,
and conditional waves do not record verification allowances or rerun ceilings.

## Evidence Reviewed

- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/CH4.md`
- `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/CONSOLIDATED.md`

## Resolution Matrix

| V1 cost requirement | V2 disposition | Finding |
|---|---|---|
| Wave caps | REVISE | `SPEC.md` and `DISPATCH-PROMPT.md` list wall-clock hard caps for W0-W6, but the final packet does not add the per-wave source LOC caps requested by V1 CH4. It also does not state that research, plan, implementation, verification, RESULTS refresh, REDRESS, and docs all consume the hard cap. |
| Agent counts | ACCEPT | Research phase caps are explicit: 30 min per agent, max 6 agents, and W0 dispatch uses 1-6 parallel research agents. |
| W0 narrowness | ACCEPT with cost caveat | W0 is now telemetry-only, has a narrow owner-path set, and forbids parser, scanner, SIMD, asm, codegen, and product-plane behavior changes. The remaining caveat is budget accounting: all-row profile capture, telemetry schema work, gate validation, sidecar freshness, malformed-manifest testing, and RESULTS refresh are still packed into a 180 min hard cap with no source LOC cap or verification allowance. |
| W1 CostFacts before behavior | ACCEPT | CostFacts gate binding is W1 and blocks behavior waves. `SPEC.md` requires W0 admission before W1 and says W1 rejection blocks behavior waves. |
| W2-W4 conditional scope | ACCEPT | W2, W3, and W4 are not dispatchable from the final packet. They require W0/W1 closure plus plan updates naming rows, thresholds, owner paths, and challenge acceptance where relevant. |
| Bitmap rejected as default | ACCEPT | `SYNTHESIS.md`, `HANDOFF.md`, and `SPEC.md` reject PMULL prefix-XOR and CTZ/bulk as default production routes. Bitmap work can return only as reserve research after fresh evidence and challenge acceptance. |
| Verification cost | REVISE | W0 has concrete dispatch verification commands, but the packet does not define per-wave verification allowances, maximum full-bench reruns, or whether reruns count against the hard cap. V1 CH4 explicitly required verification commands and maximum full-bench reruns per wave. |
| Revert protocols | ACCEPT | `SPEC.md` now gives a revert protocol for W0-W6, with downstream blocking effects for W0, W1, W3, and W5. |

## Remaining Blockers Before ACCEPT

1. Add a source LOC cap table to `SPEC.md` or the dispatch contract. Minimum
   shape from V1 CH4: W0 telemetry/gate 300-350 source LOC, W1 CostFacts gate
   300 source LOC, W2 typed product 650 source LOC, W3 parse 450 source LOC by
   default or 650 with template parity, W4 direct guard 300 source LOC, W5 0 by
   default or 150 for a concrete Lock 14 fix, and W6 0 source LOC except docs
   and REDRESS/HANDOFF/SPEC updates.
2. Define every hard cap as an inclusive budget covering research, plan,
   implementation, verification, RESULTS refresh, REDRESS, and docs. If the
   packet keeps phase caps, say whether parallel agents are counted as
   wall-clock time or agent-minutes, and state that phase work cannot exceed the
   wave hard cap.
3. Add per-wave verification allowances and rerun ceilings. At minimum, record
   the focused commands, gate refreshes, generated-output diff checks, full-table
   maintain checks, and maximum full bench reruns for W1-W4. Extra reruns should
   be REDRESS cost, not unbounded retry room.
4. Clarify W0 sidecar freshness cost. Either scope sidecar freshness to populated
   sidecar cells with explicit missing-sidecar non-admission reasons, or reserve
   enough budget for all-row sidecar completion. The current W0 text requires
   all 38 rows to satisfy Section 0.4 but does not say how absent sidecars are
   costed.
5. State that generated output and RESULTS refreshes are outside source LOC caps
   but inside review and verification cost, with byte-diff audit expectations
   where generated files or RESULTS move.

## Reject Triggers

No CH4 reject trigger is present in the final docs. The packet does not dispatch
W1-W6 by default, does not keep bitmap asm as a default implementation wave, and
does not leave behavior waves with broad preselected owner paths. The remaining
issues are missing cost caps and verification accounting, so the correct V2
outcome is REVISE rather than REJECT.
