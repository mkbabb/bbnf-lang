# SK-V12 W1b-2b CH1 Correctness Challenge

Date: 2026-05-20.
Lens: CH1 correctness.
Verdict: ACCEPT.

## Scope Read

- `restart/skinny/tranches/sk-v12/SPEC.md` Section 7.2.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN.md`.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN-AUDIT.md`.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/A1-report-schema.md`.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/A2-gate-cli.md`.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/A3-criterion-consumption.md`.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/A4-json-guard-lock14.md`.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/A5-outcome-routing.md`.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/A6-test-plan.md`.

## Correctness Verdict

The plan is acceptable for implementation because it resolves the correctness
hazards that would otherwise make the W1b-2b lightningcss SOTA gate
producer-only or stale. The accepted contract is the corrected `PLAN.md` plus
`PLAN-AUDIT.md`, not stale research wording in A1/A6.

Concrete acceptance reasons:

- Strict equality is treated as a gate condition, not as report decoration. The
  plan requires `strict_output_equality == pass`,
  `three_way_equality == pass:track1=cssparser=lightningcss`,
  `track2_independence_status == independent_verified`, the same output plane
  `css_l4_declaration_value_fact_stream`, and retained lightningcss fact
  evidence. A Track 1/lightningcss match without cssparser oracle agreement is
  therefore invalid.
- Criterion measurement authority is correctly placed on the three `new/`
  Criterion lanes under `skinny/target/criterion/nonjson_css_l4/`, with
  `benchmark.json.throughput.Bytes == 187`,
  `estimates.json.mean.point_estimate` as ns/iter, finite derived Mbps, and
  `sample.json.iters.len() >= 30`. The plan rejects `base/`, `change/`,
  hand-entered Mbps, malformed files, missing lanes, and sample-deficient
  measurements.
- Threshold math matches the pin and SPEC: derive
  `threshold_mbps = lightningcss_mbps + 1`, derive
  `admission_margin_mbps = track1_mbps - threshold_mbps`, and admit only on
  strict `track1_mbps > lightningcss_mbps + 1` with positive margin. Equality
  at the threshold is explicitly routed as `PASS-MEASURED-BASELINE`, not ADMIT.
- Stale artifact rejection is strong enough if implemented as written. The
  report must bind Track 1, cssparser oracle, lightningcss, equality facts,
  benchmark artifacts, row identity, fixture checksum, byte count, and run id
  to the W1b-2b CSS L4 declaration-values row. It must not fall back to W1b-1,
  `base/`, report-provided numbers, CSS-only JSON roots, or sidecar-only
  comparator evidence.
- Report validation is scoped to a separate
  `sk-v12-css-l4-sota-v1` validator and does not loosen
  `sk-v12-nonjson-generated-v1`. The plan requires exact row constants,
  unknown producer-field rejection, no-write/probe CLI rejection, JSON guard
  continuation only through accepted no-write flags, Lock14 process validation,
  explicit Lock16 non-claim state for this scalar row, and `REDRESS-125`.
- `skinny/RESULTS.md` movement is correctly constrained. A measured baseline
  must not move RESULTS; only a true `PASS-ADMIT-CANDIDATE` CSS row or an
  accepted measured JSON guard demotion may move it. Stale-results guidance for
  the CSS companion report is therefore not allowed to instruct an update for a
  measured CSS miss.

## Conditions For The Implementer

- Follow `PLAN.md` where it supersedes stale A1/A6 details: W1b-2b uses
  `REDRESS-125` and gate label
  `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`.
- Derive and compare measurement values inside the gate with a documented float
  tolerance; do not trust serialized `track1_mbps`, `lightningcss_mbps`,
  `threshold_mbps`, `admission_margin_mbps`, or `sample_count` as authority.
- Validate the CSS SOTA report before entering the existing JSON guard path,
  and require a populated JSON guard root whenever `--check-results` or
  `--with-cost-facts` is used.

With those conditions, CH1 correctness accepts the plan for W1b-2b
implementation.
