# SK-V12 S-P3 V2 Consolidated Hardening

Pass: S-P3 Synthesis-Plan.
Cycle: V2.
Date: 2026-05-20.
Scope: consolidate CH1-CH6 review of the SK-V12 S-P3 V2 packet.

## Verdict

REVISE.

ACCEPT-rate: 2/6 = 33.3%.

| Lens | Disposition | Summary |
|---|---|---|
| CH1 Correctness | REVISE | Close/routed paths drift: W2 reject and W3 routed block legal in SPEC/DISPATCH but not fully closeable in P3-C; W1 split gates lack authority; baseline Mbps aliases drift. |
| CH2 Generality / Lock 14 | ACCEPT | Generated non-JSON proof and provider/template boundaries are executable and fail-closed. |
| CH3 Regression / REDRESS | REVISE | Guard-floor no-touch rule weaker in SPEC/DISPATCH; P3-A local pre-block notes miss REDRESS 28/33 and 70/71 details. |
| CH4 Cost / Caps | REVISE | W1a/W1b split is unbudgeted; P3-B LOC budgets drift from SPEC/DISPATCH/P3-F. |
| CH5 Hidden Coupling | ACCEPT | Sidecar/substrate, provider/template, Track1/Track2, and witness-route bans are explicit. |
| CH6 Anti-Paper-Close | REVISE | W1a can close without measured row; P3-D allows admitting equality-only oracle Mbps as `n/a`. |

## Required V3 Folds

1. Remove the W1a/W1b split authority from P3-C and SPEC. A split requires a
   future S-P3 revision with explicit manifest entries, LOC budgets, risk,
   wall/redress caps, rerun ceilings, gates, revert protocols, and same-wave
   consumers.
2. Add W2 measured-reject close form to P3-C W4 and keep SPEC/DISPATCH aligned:
   W1 baseline admitted, W2 measured reject recorded, W3 adjudicated/routed,
   guards preserved or measured demotion recorded, close docs agree.
3. Rewrite P3-C W3 into two exit forms: behavior dispatch with selected residual
   row/floors, or routed block with no source/RESULTS movement, explicit
   material-reopen failure, and REDRESS evidence.
4. Standardize on `baseline_mbps` as the schema and gate name for W1 generated
   Track 1 Mbps; replace `baseline_track1_mbps`,
   `W1_baseline_mbps`, and `W1_baseline_track1_mbps`.
5. Fold the P3-C guard rule into SPEC Section 0.1, W1, W2, W3, and DISPATCH:
   behavior waves either rerun and maintain all 4 direct + 7 typed guards, or
   prove no JSON-producing path was touched and `skinny/RESULTS.md` stayed
   unchanged. Any guard miss fails unless recorded as explicit measured demotion
   in REDRESS.
6. Tighten P3-D so admitting W1/W2 rows require measured
   `track2_or_oracle_mbps >= 1`; `n/a` is allowed only for non-admitting support
   reports.
7. Mirror REDRESS 70/71 typed-output boundary in P3-A C1-C3 and REDRESS 28/33
   active TBL/NEON tiny-string block in P3-A C6.
8. Normalize P3-B LOC budgets to SPEC/DISPATCH/P3-F: W0 <=180, W1 <=520 CSS /
   <=480 Sheets / <=460 BBNF-self, W4 <=120.
9. Mirror W1 one-target no-fallthrough in DISPATCH.

## Convergence Status

S-P3 has not converged. V3 must fold the REVISE list and then run another full
six-lens CHALLENGE cycle. Per `ORCHESTRATOR.md` Section 3Z and
`PASS-3-SYNTHESIS-PLAN.md` Section 4, convergence requires >=95% ACCEPT for two
consecutive cycles with no open critical defects and no orphan unresolved
REVISE.
