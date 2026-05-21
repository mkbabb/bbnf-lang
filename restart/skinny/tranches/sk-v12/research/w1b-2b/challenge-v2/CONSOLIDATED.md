# SK-V12 W1b-2b CHALLENGE V2 - Consolidated Disposition

Date: 2026-05-20.
Wave: W1b-2b - CSS L4 lightningcss SOTA report gate.
Plan under review: `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN-V2.md`.

## Disposition

REVISE.

PLAN-V2 narrowed the W1b-2b scope enough to clear CH2, CH3, and CH6, but CH1,
CH4, and CH5 still find blocking issues. The wave may not enter redress until
the plan names an executable two-root evidence protocol, restores the required
CSS telemetry fields, consumes retained equality/fact artifacts rather than
checking paths only, and removes the unbounded RESULTS movement branch.

## Lens Results

- CH1 correctness: REVISE.
- CH2 generality / Lock 14: ACCEPT.
- CH3 regression / REDRESS: ACCEPT.
- CH4 cost / budget: REVISE.
- CH5 hidden coupling: REVISE.
- CH6 anti-paper-close: ACCEPT.

## Blocking Revisions

1. Criterion root authority must be split explicitly. A single
   `CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion` invocation cannot
   simultaneously feed the CSS `nonjson_css_l4` lanes and the JSON guard lanes.
   The next plan must either add a dedicated CSS Criterion-root source or,
   preferably, require two commands: CSS report validation against the CSS root,
   then JSON guard/stale check against `/tmp/skv12-w1a-json-guard-criterion`.

2. The CSS SOTA schema must restore SPEC-required telemetry:
   `track2_or_oracle_source_path`, `lightningcss_command`,
   `measured_validation_path`, and `profile_artifact`. Each field needs a
   bounded validator. Dropping these fields for budget reasons would make the
   report producer-side evidence instead of a gate-consumed CSS row.

3. Retained W1b equality and fact artifacts cannot be path-only proof. The gate
   must read the retained files or regenerated W1b-2b files and verify pass
   status, row id, fixture SHA/input-byte binding, fact-stream equality or a
   consumed `fact_stream_sha256`, and an explicitly accepted retained-artifact
   run id if the W1b run id remains.

4. The lightningcss measured lane must remain isolated from the cssparser
   oracle path. Redress needs an executable or tightly bounded source audit
   proving `lightningcss_facts` does not call `oracle_facts`, `ParserInput`,
   `Parser`, or other direct cssparser parser APIs.

5. The implementation budget must be priced honestly. The report schema plus
   Criterion verifier is larger than the V2 `<=220` LOC estimate. The next plan
   must either split the wave or raise the source budget to roughly 300
   report/gate/test LOC while preserving the 30-minute redress hard cap
   discipline.

6. `skinny/RESULTS.md` movement must be routed out of W1b-2b. The accepted cost
   shape is a measured companion-gate admit candidate only: W1b-2b records
   REDRESS-125 and the CSS SOTA report, but final RESULTS reconciliation moves
   to W5 close so the existing JSON stale-results gate remains bounded.

## Accepted Surfaces To Preserve

- Exact gate label `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA` and REDRESS-125.
- Exact row `css_l4/declaration_values/direct_to_struct/main`, plane
  `css_l4_declaration_value_fact_stream`, fixture SHA-256
  `cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374`, and
  `input_bytes == 187`.
- Comparator math: `threshold_mbps = lightningcss_mbps + 1`, margin =
  `track1_mbps - threshold_mbps`, and admission only when Track 1 is strictly
  greater than the threshold.
- Live Criterion `new/` artifacts remain throughput authority:
  `benchmark.json` byte count, finite positive `mean.point_estimate`, and
  `sample.json.iters.len() >= 30`.
- Dedicated schema `sk-v12-css-l4-sota-v1`, no widening of
  `sk-v12-nonjson-generated-v1`.
- No directive, BIR, `BackendShape`, public substrate API, sidecar substrate,
  generic JSON/CSS policy branch, SIMD claim, or ASM admission in this wave.

## Route

Return W1b-2b to plan. The next plan revision should be short and surgical:
supersede PLAN-V2, adopt the two-command evidence protocol, add the four
telemetry fields and artifact-freshness checks, pin `RESULTS.md` to W5 close,
and reprice the source budget. If that revised plan passes CHALLENGE, redress
can implement the companion gate and measured report without broad renderer or
RESULTS edits.
