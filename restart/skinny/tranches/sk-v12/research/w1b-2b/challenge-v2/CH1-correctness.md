# SK-V12 W1b-2b CH1 Correctness Challenge V2

Date: 2026-05-20.
Lens: CH1 correctness.
Verdict: REVISE.

## Scope Read

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` §4.
- `restart/skinny/tranches/sk-v12/SPEC.md` §0.4 and §7.2.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN-V2.md`.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/challenge/CONSOLIDATED.md`.

## Blocking Findings

1. Criterion root ownership is internally inconsistent. `PLAN-V2.md` says the
   CSS gate reads
   `criterion_root()/nonjson_css_l4/{track1_generated_css_l4_decl_values,track2_cssparser_oracle,lightningcss_same_plane_fact_stream}/new/`,
   but its combined redress command sets
   `CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion` before invoking the CSS
   report gate with `--check-results`. Under the existing `criterion_root()`
   semantics, the CSS gate would look for `nonjson_css_l4` inside the JSON
   guard root, while the JSON guard later needs that same root for JSON rows.
   One root cannot be both authorities. This makes the otherwise measurable
   gate fail or, worse, tempts the implementer to silently skip one authority.

   Concrete fix: either add a dedicated CSS Criterion root source
   (`--skv12-css-l4-criterion-root <path>` or a gate-consumed report field
   resolved independently of `CRITERION_HOME`) while leaving `CRITERION_HOME`
   for JSON guards, or split redress into two explicit gate commands: first
   CSS report validation against the CSS Criterion root, then the existing JSON
   guard command against `/tmp/skv12-w1a-json-guard-criterion`. Do not rely on
   one `criterion_root()` for both.

2. The narrowed schema omits global CSS telemetry that SPEC §0.4 still marks
   mandatory for every new or refreshed CSS/non-JSON row. `PLAN-V2.md` carries
   most Section 7.2 fields, but drops at least `track2_or_oracle_source_path`,
   `lightningcss_command`, `measured_validation_path`, and
   `profile_artifact`. A `PASS-ADMIT-CANDIDATE` report without those fields is
   producer-side evidence, not the fully gate-consumed CSS row required by
   SPEC §0.4.

   Concrete fix: add those fields to `sk-v12-css-l4-sota-v1` and validate them
   with bounded string/path checks. `lightningcss_command` must bind the
   same-plane comparator/version path, `track2_or_oracle_source_path` must bind
   the cssparser oracle source, `measured_validation_path` must bind the
   strict-equality or gate artifact, and `profile_artifact` must bind the
   relevant W1b/W1b-2 measurement artifact or an explicit `n/a` value allowed
   by a SPEC revision. Do not drop them solely to meet the LOC budget.

## Accepted Surfaces

- The exact row identity
  `css_l4/declaration_values/direct_to_struct/main`, output plane
  `css_l4_declaration_value_fact_stream`, fixture SHA-256, and 187-byte input
  are correct.
- Strict three-way equality and oracle independence are correctly gate
  conditions: `strict_output_equality == pass`,
  `three_way_equality == pass:track1=cssparser=lightningcss`,
  `lightningcss_sequence_status == pass:ast_projection_matches_source_sidecar`,
  and `track2_independence_status == independent_verified`.
- The comparator math is correct: derive
  `threshold_mbps = lightningcss_mbps + 1`, derive
  `admission_margin_mbps = track1_mbps - threshold_mbps`, and admit only when
  `track1_mbps > lightningcss_mbps + 1`. Equality at threshold remains
  `PASS-MEASURED-BASELINE`.
- Criterion mean/byte/sample authority is correctly specified once the root
  conflict is fixed: `new/benchmark.json` must carry `throughput.Bytes == 187`,
  `new/estimates.json` must carry finite positive `mean.point_estimate`, and
  `new/sample.json.iters.len() >= 30`.
- W1b-2b routing now correctly uses gate
  `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA` and `REDRESS-125`.

With those two revisions, CH1 would accept the narrowed W1b-2b plan for
redress.
