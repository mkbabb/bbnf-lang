# SK-V15 Wave W7 Plan: Decision Engine Spine

Inputs: `skv15-W7-research.md`, W6 routed redress `c0c2fb6c4`, W7 sidecar
agent inventory.
Intervention: replace the zero-rule e-graph and tautological CSP proof with a
grammar-neutral rewrite plus a falsifiable capacity CSP predicate, then prove
codegen consumes the selected plan.

Owner paths:

- `skinny/crates/ir/src/cost.rs`
- `skinny/crates/passes/src/backend_egraph.rs`
- `skinny/crates/passes/src/decision_csp.rs`
- `skinny/crates/passes/src/lib.rs`
- `skinny/crates/codegen/src/lib.rs`
- `restart/skinny/tranches/sk-v15/research/w7/*`

Falsifiability gate:

- `cargo test --manifest-path skinny/Cargo.toml -p passes decision_egraph_rewrite_changes_selected_shape -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p passes decision_csp_rejects_missing_required_fact -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen decision_spine_changes_generated_selection_fixture -- --exact`

Hard cap: 75 minutes redress.

Same-wave consumer: `codegen::lower::rust::lower_to_rust` remains the W7
consumer and must reject missing/unsat CSP facts. The codegen fixture must
demonstrate different lowering plans from different generic decision inputs.

Pre-blocked routes: metadata-only rewrite counts, grammar-named `json_*` or
`css_*` decision drivers, sixth `BackendShape`, retained class/structural/cursor
streams, advisory-only cost facts, and unstaged unrelated dirty files.

Expected result: W7 admits only if the rewrite count is nonzero on the
direct-sink fixture, removing/altering a required capacity fact changes CSP
satisfiability, and codegen lowering changes under a valid generic selection
change.
