# SK-V12 W1b-2 Research Consolidated

Status: research cohort complete.

## Disposition

W1b-2 is dispatchable for plan. The selected route is a lightningcss same-plane
comparator for the W1b-1 CSS L4 row:

```text
css_l4/declaration_values/direct_to_struct/main
```

The output plane remains:

```text
css_l4_declaration_value_fact_stream
```

The user-pin admission bar remains:

```text
track1_mbps > lightningcss_mbps + 1
```

## Load-Bearing Findings

1. lightningcss `1.0.0-alpha.71` exposes public `StyleSheet::parse`,
   `CssRuleList`, `CssRule`, `StyleRule`, `DeclarationBlock::iter()`, and
   `Property::property_id()` APIs sufficient to verify declaration sequence.
2. The public AST is semantic/canonicalized, not a raw token stream. The plan
   must not pretend AST-only emission can reproduce W1b-1 source-token facts.
3. The admissible comparator is hybrid: lightningcss AST parse/traversal plus an
   independent source scanner, with the emitted facts accepted only if the
   source scan matches the lightningcss declaration sequence.
4. Direct cssparser calls are forbidden in the lightningcss comparator; the
   existing cssparser oracle remains as Track 2 and stays separate.
5. Use a W1b-2-specific companion report schema for the lightningcss bar. Do
   not reuse W1b-1 `sk-v12-nonjson-generated-v1` intervention math.
6. Keep `RESULTS.md` unchanged unless W1b-2 records an actual CSS ADMIT surface
   or a JSON guard demotion.
7. No Lock 14 allowlist change is expected if W1b-2 stays inside Section 7
   owner paths. Prefer a direct `bbnf-bench` dependency on
   `lightningcss = { version = "=1.0.0-alpha.71", default-features = false }`.
8. REDRESS 124 is the W1b-2 outcome slot. A measured miss is
   `PASS-MEASURED-BASELINE`, not CSS ADMIT.

## Plan Requirements

The W1b-2 plan must name:

- the exact lightningcss dependency/version/features;
- the hybrid comparator API and forbidden couplings;
- the three retained fact artifacts;
- the W1b-2 companion report schema and gate flag;
- Criterion bench additions and sample count;
- exact admission math;
- no-write JSON guard command;
- rollback path `/tmp/skv12-waveW1b-2-rejected.patch`;
- REDRESS 124 outcome text for ADMIT, measured baseline, and fail/block.

## Expected Commands

```sh
cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture
cargo test -p bbnf-bench skv12_css_l4_sota_report -- --nocapture
cargo test -p bbnf-bench skv12_css_l4_sota_report_arg -- --nocapture
cargo test -p bbnf-bench lock14 -- --nocapture

RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench nonjson_css_l4 -- --sample-size 30

CRITERION_HOME=/tmp/skv12-w1b-2-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p bbnf-bench --bin gate -- \
  --skv12-css-l4-sota-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json \
  --advisory --check-results
```
