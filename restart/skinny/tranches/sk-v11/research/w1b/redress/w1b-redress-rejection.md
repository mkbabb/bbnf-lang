# SK-V11 W1b Redress: Generated Non-JSON Baseline Rejection

Date: 2026-05-20.
Wave: W1b - Generated Non-JSON Baseline And Oracle Lane.
Gate: `G-W1b-NONJSON-BASELINE`.
Disposition: REJECT.

## Selected Target

- Row: `css_l4/declaration_values/direct/main`.
- Grammar/domain: `css_l4` / `css_l4_bench`.
- Output plane: `css_l4_declaration_value_fact_bytes`.
- Required authority: generated non-JSON Track 1 plus independent same-plane
  Track 2/oracle, strict fact-byte equality, same-run throughput, and
  gate-consumed source/equality provenance.

## Blocker

The selected W1b gate cannot be made positive inside the accepted W1b owner
surface. The skinny generator still emits only the JSON runtime profile:

- `skinny/crates/codegen/src/lib.rs` calls
  `json_provider::ensure_runtime_profile` from both `emit_from_source` and
  `emit_typed_from_source`.
- `skinny/crates/codegen/src/json_provider.rs` accepts only
  `backend.grammar_name == "json"` and otherwise returns
  `runtime emission currently supports grammar profile 'json'`.
- `skinny/crates/runtime/src/grammars/` contains generated JSON plus the
  `sheets_witness` proof module; it contains no generated CSS L4 runtime under
  `css_l4` or `css_l4_declaration_values`.

Therefore no generated CSS L4 direct Track 1 exists for
`css_l4/declaration_values/direct/main`. The oracle path is not admitted because
Track 1 authority is absent, and W2 remains blocked from creating the first
measurable non-JSON baseline row.

## Evidence

Passed:

- `cargo test -p codegen --lib -- --nocapture`
- `cargo test -p bbnf-bench report::tests::w1a -- --nocapture`
- `cargo test -p bbnf-bench --bin gate w1a -- --nocapture`
- `cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json`
- `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results`
- `git diff --exit-code -- skinny/RESULTS.md`
- `find skinny/crates/runtime/src/grammars -maxdepth 3 -type f | sort`
- `rg -n "ensure_runtime_profile|runtime emission currently supports grammar profile|emit_from_source|emit_typed_from_source|json_provider::ensure_runtime_profile" skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/json_provider.rs`
- `test ! -e skinny/crates/runtime/src/grammars/css_l4 && test ! -e skinny/crates/runtime/src/grammars/css_l4_declaration_values`

No source patch was attempted. `/tmp/skv11-waveW1b-rejected.patch` is an empty
marker.

## Result

W1b records a measured gate rejection, not a baseline close. It changes no
behavior source, generated runtime, benchmark body, gate code, report schema, or
`skinny/RESULTS.md` row.
