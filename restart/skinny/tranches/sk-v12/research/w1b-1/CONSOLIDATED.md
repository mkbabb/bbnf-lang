# SK-V12 W1b-1 Research Consolidated

Status: research cohort complete.

## Disposition

W1b-1 remains dispatchable, but only if the plan explicitly revises the owner
surface before redress. The old CSS preflight failure is no longer enough to
skip CSS under the user pin. The wave must either land a generated CSS L4
declaration-values scaffold with an independent oracle, or record a measured
W1b-1 blocker after attempting that exact scaffold surface.

## Load-Bearing Findings

1. The selected row is fixed:
   `css_l4/declaration_values/direct_to_struct/main`.
2. The output plane is fixed:
   `css_l4_declaration_value_fact_stream`.
3. The parser root is declaration-level CSS, not full stylesheet admission and
   not value-only admission. `properties.bbnf::declaration` is the semantic
   target.
4. W1b-1 is scalar-only. W2 is now PASS, but no SIMD helper is needed for the
   scaffold and adding one would expand Lock 16 risk.
5. W1b-1 must not claim lightningcss admission. It records Track 1 + oracle
   strict equality and finite Mbps only. W1b-2 owns lightningcss.
6. Current codegen is JSON-provider-bound. Redress requires a CSS provider or a
   generated CSS runtime path that does not clone JSON policy under a neutral
   name.
7. `runtime/src/lib.rs`, `codegen/src/grammar_profile.rs`, a CSS provider file,
   and possibly `xtask/src/main.rs` are owner-table gaps relative to SPEC
   Section 6.
8. The companion non-JSON report gate must be extended to consume generated
   size, grammar/input checksums, strictness, profile/validation artifacts,
   Lock 14/16 status, and parity status. Do not add outcome variants or main
   JSON table columns.
9. The fixture path is missing. The plan should create the exact owned fixture
   at `restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css`
   with the small declaration-values corpus from A5, rather than borrowing a
   broad CSS corpus or using Sheets/BBNF-self.

## Plan Requirements

The W1b-1 plan must name:

- amended owner paths;
- generated Track 1 source/runtime files;
- CSS fixture path and checksum;
- independent oracle module and equality artifact;
- generated LOC/module-byte accounting;
- companion report schema/gate changes;
- JSON guard rerun, because report/gate/runtime exports move;
- rollback patch path `/tmp/skv12-waveW1b-1-rejected.patch`.

## Redress Commands To Carry Forward

```sh
cargo test -p codegen css_l4_declaration_values_profile_fields_are_consumed -- --nocapture
cargo test -p runtime css_l4_declaration_values -- --nocapture
cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture

CARGO_TARGET_DIR=/tmp/skv12-w1b1-target \
CRITERION_HOME=/tmp/skv12-w1b1-css-l4-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench nonjson_css_l4 -- css_l4/declaration_values/direct_to_struct/main

RUSTFLAGS="-C target-cpu=native" \
cargo run -p xtask -- gate-json \
  --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json
```
