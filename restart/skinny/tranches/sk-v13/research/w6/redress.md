# SK-V13 W6 Redress - Bounded E-Graph + Active Cost

Disposition: PASS with measured architectural block.

W6 lands bounded active-cost backend selection. The selector imports the local
root `egraph` crate as a direct skinny dependency, builds a skinny-local
backend candidate language, runs a bounded e-graph extraction per rule, and
writes the selected winner into `CostFacts.chosen`. The existing generated
lowering seam consumes that selected shape through
`codegen::lower::rust::lower_to_rust`.

The same-wave report records the measured architectural block
`JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME`: W6
replaces passive cost facts with active selection and proves the selected
candidate reaches lowering, but current emitted JSON/CSS runtime templates do
not render the extracted candidate into row-moving code. W7 owns the CSP and
cascade fail-closed route needed to turn the selector into row movement.

Evidence:

- Active-cost artifact:
  `restart/skinny/tranches/sk-v13/research/w6/active-cost-facts.json`.
- Gate report:
  `restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json`.
- Active-cost artifact SHA-256:
  `a7de15802b3794d0c1ead6cb7f1971ac4f5c05723bcbf1eb0d89468700a395f1`.

Verification:

- `cargo check -p egraph`.
- `cargo test -p passes active_cost`.
- `cargo test -p ir cost`.
- `cargo test -p codegen cost_facts`.
- `cargo test -p bbnf-bench --lib skv13_decision_active_cost_report`.
- `cargo test -p bbnf-bench --bin gate skv13_decision_active_cost_report`.
- `cargo test -p xtask gate_json_passthrough_accepts_skv13_decision_active_cost_report_flag`.
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-decision-regex-report ../restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json --skv13-decision-active-cost-report ../restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json`.
