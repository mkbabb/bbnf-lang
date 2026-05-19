ACCEPT

# SK-V11 S-P1 Hardening V4 CH3: Regression / Pre-Block

Date: 2026-05-19.
Scope: CH3 confirmation only. Reviewed the folded S-P1 packet `p1a` through
`p1f` at HEAD `cc8656b804ef`, `PASS-1-PROFILE.md` Section 3 CH3,
`ORCHESTRATOR.md` Section 3Z, `skinny/REDRESS.md` through REDRESS 110, and
the S-P1 V1/V2/V3 hardening consolidations.

## Findings

1. PASS-1 CH3 and ORCHESTRATOR CH3 are satisfied: S-P1 remains profile
   evidence only, and suggestive anomaly text does not silently re-propose or
   reopen routes already blocked in `skinny/REDRESS.md`.
2. The V1 CH3 citation defect was folded before V2. The folded packet carries
   the required pre-block coverage for REDRESS 50, 51, 53, 54, 55, 60-69, 72,
   80, 82-84, 88-90, 96-98, and 102; V2 accepted CH3 with no further fold, and
   V3 was the first all-ACCEPT cycle.
3. `p1a` fences parse-only masking, W3 substrate, sidecar/cursor/class-column,
   unicode/materialization, cap-policy, single-quartet, and StringBlock16
   temptations with REDRESS anchors and explicitly keeps parse evidence out of
   close/admission.
4. `p1b`, `p1c`, and `p1d` keep direct/typed hot leaves, W0 masking probes,
   structural scan, lazy tape, PMU, and cycles as diagnostic or nonproducer
   evidence. Their anomaly sections cite the W3 substrate and parse-only
   firewall pre-blocks where a reader might otherwise infer a route.
5. `p1e` carries the load-bearing matrix: structural rediscovery and parser
   sidecar/cursor routes map to REDRESS 50, 51, 53, 96, 97, 98, and 102;
   string/unicode routes map to REDRESS 54, 55, 60-62, 64, 66-69, 72, 82, and
   83; parser-control and value-byte carry map to REDRESS 63, 65, and 84;
   numeric fallback maps to REDRESS 80; bitmap/PMULL/CTZ body-fill temptations
   map to REDRESS 88, 89, and 90.
6. `p1f` handles row-surface anomalies fail-closed: current W0 telemetry does
   not carry SK-V10 W10/REDRESS-109 admission forward, stale or absent
   `numbers` direct telemetry is not treated as an admission, and structural
   scan/masking/PMU/cycles remain nonbehavior evidence.
7. REDRESS through 110 supports those anchors. The inspected entries reject or
   retire the sidecar/cursor/class-column family, string/unicode
   materialization variants, numeric fallback, object/value-byte carry,
   bitmap body-fill routes, and parse-only movement; SK-V10 close accounting is
   documentation-only and does not reopen those routes.

## Required Fold

None. V4 CH3 confirms the folded S-P1 packet ties suggestive anomalies to
REDRESS anchors and proposes no behavior route, row admission, gate change,
source edit, benchmark-body change, or retired-route reopen.
