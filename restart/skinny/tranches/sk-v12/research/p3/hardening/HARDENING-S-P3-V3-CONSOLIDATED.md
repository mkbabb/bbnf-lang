# SK-V12 S-P3 V3 Consolidated Hardening

Pass: S-P3 Synthesis-Plan.
Cycle: V3.
Date: 2026-05-20.
Scope: consolidate CH1-CH6 review of the SK-V12 S-P3 V3 packet.

## Verdict

REVISE.

ACCEPT-rate: 4/6 = 66.7%.

| Lens | Disposition | Summary |
|---|---|---|
| CH1 Correctness | REVISE | Secondary drift: P3-F weakens W2 oracle/Track 2 floor, P3-B omits W2 measured-reject route into W3, and P3-C says two close forms while defining three. |
| CH2 Generality / Lock 14 | ACCEPT | Executable Lock 14 and generated non-JSON proof are fail-closed. |
| CH3 Regression / REDRESS | ACCEPT | Guard no-touch/rerun rule and REDRESS 28/33, 70/71, 96/97/98, 111-120 carry-forward are sufficient. |
| CH4 Cost / Caps | ACCEPT | Split authority removed; LOC/risk/wall/redress caps align. |
| CH5 Hidden Coupling | ACCEPT | Sidecar/substrate, provider/template, Track1/Track2, and witness-route coupling are closed. |
| CH6 Anti-Paper-Close | REVISE | P3-F, P3-D Section 3, and DISPATCH still omit W2 oracle/Track 2 >= 1 Mbps in some admit prose. |

## Required V4 Folds

1. Change P3-F W2 gate language to require oracle/Track 2 >= 1 Mbps,
   independent, and strict-equal.
2. Change P3-D Section 3 intervention summary to require Track 2/oracle >= 1
   Mbps, independent, same-plane, and strict-equal.
3. Change DISPATCH W2 load-bearing fact to require Track 1 >=
   `ceil(baseline_mbps * 1.01)`, oracle/Track 2 >= 1 Mbps, strict equality PASS,
   and same-wave gate consumption.
4. Add the W1-admitted/W2-measured-reject route to P3-B's W3 entry/topology
   language.
5. Change P3-C W4 close wording from "one of two forms" to "one of three
   forms."

## Convergence Status

S-P3 has not converged. V4 must fold the REVISE list and then run another full
six-lens CHALLENGE cycle. Per `ORCHESTRATOR.md` Section 3Z and
`PASS-3-SYNTHESIS-PLAN.md` Section 4, convergence requires >=95% ACCEPT for two
consecutive cycles with no open critical defects and no orphan unresolved
REVISE.
