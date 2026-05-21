# SK-V12 W1b-2b A5 - REDRESS/RESULTS Outcome Routing

Date: 2026-05-20.
Phase: W1b-2b research.
Scope: SPEC Section 7.2 outcome routing for `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`.

## Inputs Read

- `restart/skinny/tranches/sk-v12/SPEC.md`, especially Section 7.2.
- Current `skinny/REDRESS.md` tail through item 124.
- Current `skinny/RESULTS.md`.

## Current Ledger State

REDRESS item 124 is already consumed by W1b-2a:

- gate: `G-W1b-2a-CSS-L4-LIGHTNINGCSS-COMPARATOR`;
- outcome: `PASS-COMPARATOR`;
- explicit route: W1b-2b owns strict-vs-strict CSS L4 admission or measured
  rejection against `track1_mbps > lightningcss_mbps + 1`;
- explicit no-move: no `skinny/RESULTS.md` edit in W1b-2a.

Therefore the next W1b-2b REDRESS item is item 125, not 124. Older W1b-2
research that names REDRESS 124 as the next slot is stale after W1b-2a landed.

## Outcome Routing Matrix

| W1b-2b outcome | Required REDRESS wording | Is REDRESS item 125 next? | Should `skinny/RESULTS.md` move? |
|---|---|---:|---|
| `PASS-ADMIT-CANDIDATE` | Item 125 must say W1b-2b closes `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA` as `PASS-ADMIT-CANDIDATE`; row `css_l4/declaration_values/direct_to_struct/main`; output plane `css_l4_declaration_value_fact_stream`; report schema `sk-v12-css-l4-sota-v1`; strict equality is byte-identical across generated Track 1, independent cssparser Track 2/oracle, and lightningcss sidecar; oracle independence is consumed; Track 1 Mbps, oracle Mbps, lightningcss Mbps, threshold `lightningcss_mbps + 1`, and positive margin are recorded; telemetry is gate-consumed; JSON guards held or a measured demotion is named; this is an admit candidate only, with SK-V12 close still waiting for W4 zero-orphan/disposition and W5 reconciliation. | Yes | Yes for the CSS ADMIT candidate row. Also move if JSON guard demotion is measured. |
| `PASS-MEASURED-BASELINE` | Item 125 must say W1b-2b closes `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA` as `PASS-MEASURED-BASELINE`, not ADMIT; row and output plane as above; strict equality and oracle independence passed; Track 1, oracle, and lightningcss throughput were measured from the W1b-2a Criterion artifacts or a valid same-host rerun; threshold and non-positive margin are recorded; JSON guards held or a measured demotion is named; the miss is REDRESS evidence enabling W3/W4 routing and possible later FIXPOINT evidence, but it does not satisfy CSS SOTA admission. | Yes | No for the CSS row. Move only if W1b-2b also records a measured JSON guard demotion. |
| `BLOCKED/FAIL` | Item 125 must say W1b-2b closes or routes `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA` as `BLOCKED/FAIL`; name the failed surface exactly: comparator artifact, equality, oracle independence, generated-size telemetry, throughput extraction, report validation, gate consumption, no-write rejection matrix, JSON guard root, or stale-results guidance; include whether a measured CSS lightningcss admission attempt exists. If no strict-equal measurable row exists, say fallback remains blocked except by subsequent S-P3 or wave-plan revision. If a measured attempt exists but fails the admission gate, route it as measured redress evidence rather than CSS ADMIT. | Yes | No for the CSS row. Move only if the failure includes a measured JSON guard demotion that the gate accepts. |

## Exact RESULTS Rule

`skinny/RESULTS.md` should move in W1b-2b only in these cases:

1. `PASS-ADMIT-CANDIDATE` records an actual CSS ADMIT candidate surface for
   `css_l4/declaration_values/direct_to_struct/main` with
   `track1_mbps > lightningcss_mbps + 1`, strict equality, independent oracle,
   and consumed telemetry.
2. Any outcome records a measured JSON guard demotion accepted by the gate.

`skinny/RESULTS.md` should not move for:

1. `PASS-MEASURED-BASELINE` when the CSS row is strict-equal and measurable but
   misses `lightningcss_mbps + 1`;
2. `BLOCKED/FAIL` without an accepted JSON guard demotion;
3. stale comparator/report artifacts, missing equality, missing oracle
   independence, missing throughput extraction, or producer-only telemetry.

## Required Item 125 Shape

Use item 125 for W1b-2b. The entry should include:

- gate name `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`;
- one of `PASS-ADMIT-CANDIDATE`, `PASS-MEASURED-BASELINE`, or `BLOCKED/FAIL`;
- row id `css_l4/declaration_values/direct_to_struct/main`;
- output plane `css_l4_declaration_value_fact_stream`;
- report path
  `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`;
- report schema `sk-v12-css-l4-sota-v1`;
- strict equality artifact paths for Track 1, cssparser oracle, and
  lightningcss sidecar;
- Criterion root and sample count for Track 1, oracle, and lightningcss;
- computed `threshold_mbps = lightningcss_mbps + 1` and margin;
- JSON guard root and held/demoted state;
- no-write/probe flag rejection evidence for the companion report gate;
- explicit `RESULTS.md` disposition following the matrix above;
- routed remainder: W3/W4 may continue after a measured CSS row; W5 owns final
  ADMIT/FIXPOINT reconciliation.
