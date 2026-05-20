# SK-V12 S-P1 Hardening V1 - Consolidated

Date: 2026-05-20.

Disposition: REVISE.

## Lens Results

| Lens | Disposition | V1 finding |
|---|---|---|
| CH1 correctness | REVISE | Fresh PMU/artifact coverage passes, but hot-leaf self-time percentages are absent, P1-E aggregate c/B does not match `/tmp/skv12-p1`, and P1-C has no fresh Mode III call-stack coverage. |
| CH2 generality / Lock 14 | REVISE | JSON-only evidence boundaries pass, but P1-A/P1-B retain non-canonical JSON-role vocabulary in load-bearing profile surfaces. |
| CH3 regression / REDRESS | ACCEPT | The packet honors W3, parse-only, JSON direct residual, W0-clamped row, and non-JSON baseline pre-blocks. |
| CH4 cost / reproducibility | REVISE | Capture evidence exists, but V1 lacks a replayable capture manifest/script, complete tool-version pinning, uniform run-identity separation, and a formal xctrace export/non-export policy. |
| CH5 hidden coupling | ACCEPT | Track 1 / Track 2 / oracle separation, report-lane vs generated-baseline separation, sidecar avoidance, and diagnostic evidence boundaries hold. |
| CH6 anti-paper-close | REVISE | The packet does not invent a close, but missing inline self-time summaries must be exported or explicitly downgraded before S-P2/S-P3 may consume hot-leaf claims. |

## Required V2 Fold

V2 must:

- add a replayable SK-V12 P1 capture manifest with exact commands, tool
  versions, run identity, CWD policy, `rc=54` interpretation, and xctrace
  export policy;
- reconcile P1-E aggregate c/B against `/tmp/skv12-p1` PMU TSVs;
- replace non-canonical JSON-role labels in P1-A/P1-B with the canonical
  grammar-neutral primitive vocabulary;
- decide the hot-leaf evidence status: either produce citable fresh self-time
  summaries or explicitly downgrade the V1 hot-leaf tables to source-map/PMU
  evidence only;
- preserve the generated non-JSON baseline-first requirement, REDRESS
  pre-blocks, and no-row-admission wording.

V1 is archived as a challenge cycle. The profile pass cannot advance to S-P2
until the V2 fold and hardening rerun converge.
