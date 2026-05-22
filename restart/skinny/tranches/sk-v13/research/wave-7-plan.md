# SK-V13 W7 Plan - CSP + Cascade Fail-Closed

Cycle: W7 Plan. Scope: SPEC Section 10.

## Selected Intervention

Add an authoritative CSP finalizer after the W6 active-cost extraction and
fail-close the legacy cascade/runtime fallback surfaces that can still synthesize
production backend choices. The W6 e-graph active-cost selector remains the
candidate/cost producer; W7 consumes that output through a bounded CSP resolver
and records whether the result reaches generated backend selection.

W7 has a binary close route:

1. ADMIT only if CSP-selected output changes executable emitted JSON or CSS code
   and moves or admits at least one row by P3-C `row_move_toward_sota`. The
   emitted runtime diff must be saved, hash-checked, and gate-consumed through
   `generated_runtime_diff_status`, `generated_runtime_diff_artifact_path`, and
   `generated_runtime_diff_sha256`.
2. Otherwise close as the measured architectural block
   `JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT`, with CSP metrics,
   fail-closed cascade evidence, generated-runtime static-template evidence,
   JSON/CSS guards, and Lock 14 witnesses consumed by `gate-json`.

`passes::compile()` wiring alone is not enough. `lower_to_rust` path strings
alone are not enough. Cascade retirement alone is not enough.

## Owner Paths

Primary SPEC owner paths:

- `skinny/crates/passes/src/`
- `skinny/crates/passes/Cargo.toml`
- `skinny/crates/codegen/src/`
- `skinny/crates/bbnf-bench/`

Required support owner paths for the selected intervention:

- `skinny/Cargo.toml` for the direct `csp-solver` workspace dependency.
- `skinny/crates/ir/src/cost.rs` for `DecisionCspFacts`.
- `skinny/xtask/src/main.rs` for `gate-json` passthrough.
- `restart/skinny/tranches/sk-v13/research/w7/` for W7 evidence artifacts.
- `skinny/REDRESS.md` for REDRESS-138.
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` only if a row
  moves or admits.

Root `crates/csp-solver/` is a dependency surface, not an edit surface.

Lock 14 redress must add an explicit `SK_V13_W7_OWNER_PATHS` set and a parent
diff subject matcher for `sk-v13-waveW7`. The owner set must cover the exact
generic modules touched by W7, including at minimum:

- `crates/ir/src/cost.rs`
- `crates/passes/Cargo.toml`
- `crates/passes/src/lib.rs`
- `crates/passes/src/backend_egraph.rs`
- `crates/passes/src/decision_csp.rs`
- `crates/passes/src/diagnostics.rs`
- `crates/codegen/src/lib.rs`
- `crates/codegen/src/lower/mod.rs`
- `crates/codegen/src/lower/rust.rs`
- `crates/bbnf-bench/src/report.rs`
- `crates/bbnf-bench/src/bin/gate.rs`
- `crates/bbnf-bench/src/lock14_baseline.rs`
- `xtask/src/main.rs`
- `Cargo.toml`

Lock 14 scanning must include every W7-touched generic module. Adding a generic
file outside current scan roots without scan coverage is a CH2 fail.

## Implementation Shape

1. Add `csp-solver = { path = "../crates/csp-solver" }` to skinny workspace
   dependencies and consume it from `passes`.
2. Add `passes::decision_csp`.
3. Build one CSP variable per `RuleId`. Domain values are backend-shape
   candidates after W5/W6 fact construction, but P1-P8 priority data,
   `hard_pruned`, `priority_fired`, and `shape_rank` are evidence only. They
   cannot hard-prune the CSP domain, cannot be the objective, and cannot admit a
   row. Legality is enforced only by W7 hard constraints; cost order must be
   derived from the W6 active-cost tuple without letting old cascade labels
   become authoritative.
4. Solve with `OptimizationMode::MinimizeCost`, `max_solutions = 1`, bounded
   `node_budget`, and explicit `Instant` wall-clock timing.
5. Treat all of the following as non-admission states:
   - empty solution.
   - UNSAT.
   - elapsed solve time >1.0 second for any named grammar.
   - `SolveStats::budget_exceeded`.
   - missing selected decision for any compiled rule.
   - missing active-cost facts.
   - missing backend shape or missing cost facts in codegen lowering.
   - structural/no-decision compatibility path.
   - legacy cascade fallback.
6. Encode hard constraints for parity, recognizer facts, substrate consumer,
   SIMD/collapsed-stage feature gates, and capacity policy.
7. Make the CSP result authoritative for `CostFacts.chosen` and
   `BackendShapePlan`.
8. Fail-close the production fallback surfaces:
   - `codegen::lower::rust::shape_for` may not silently select `EagerTape`.
   - `lower_to_rust` may not synthesize projected `CostFacts` as production
     admission evidence.
   - `codegen::default_backend_shape` / `default_cost_facts` may not serve a
     production row admit path.
9. Add the W7 gate report and xtask passthrough.
10. Add Lock 14 owner paths and report fields for CSS, Sheets, and BBNF-self
    witnesses. The witnesses must include artifact paths, SHA-256 hashes, and
    commands or named generated-role evidence. If a non-JSON runtime is not
    generated in W7, it must be a gate-consumed fail-closed witness, not
    silence.

The default redress target is CSP finalization plus fail-closed evidence and the
named measured block. Executable runtime row movement is a mandatory split
unless the CSP/fail-close slice lands far under budget and every gate predicate
above can still be implemented, tested, and measured. Routing runtime emission
to a later wave is non-admission in W7; it may only appear as remainder after
the measured block has passed.

## Falsifiability Gate

Primary gate: `G-W7-DECISION-CSP-CASCADE`.

Pass conditions:

1. W5 regex and W6 active-cost artifacts are hash-checked by the W7 gate.
2. W7 CSP problem and solution/abrogation artifacts are hash-checked by the
   W7 gate.
3. CSS L4, Sheets, and BBNF-self witness artifacts are hash-checked by the W7
   gate, or the report uses an explicit scoped-witness label accepted by
   CHALLENGE. Status-only witness fields are rejected.
4. CSP status is SAT with nonempty solution, or a measured timeout/UNSAT
   abrogation is recorded as non-admission.
5. Solve time is <=1.0 second per named grammar unless measured timeout
   abrogation is recorded.
6. `budget_exceeded = false` for any SAT admit/movement claim.
7. Every compiled rule has a CSP-selected decision backed by W6 active-cost
   facts. If the W6 active-cost selected shape remains legal under W7 hard
   constraints, CSP preserves that selected shape; any CSP-induced shape change
   is non-admission unless it also changes emitted executable runtime code and
   moves/admites a row.
8. P1-P8 priority labels, `hard_pruned`, and `shape_rank` are reported as
   evidence-only and cannot be domain-pruning or objective inputs.
9. `resolver_output_piping` is exactly
   `regex_facts->egraph_active_cost->csp->compile_codegen`.
10. `fused_solver_status = not-fused`.
11. Old cascade status is `deleted`, `fail_closed`, or `gated_retired`.
12. `choose_backend_shape_status`, `priority_table_status`,
   `p1_p8_fallback_status`, and `legacy_cascade_admission_status` prove that
   no production admission path remains.
13. `fallback_invoked = false` for any row movement/admit claim. If a
    compile-safety fallback remains reachable, it must be recorded as
    `compat_non_admission`.
14. `static_css_provider_status` and `json_sink_only_status` prove whether CSS
    static templates or JSON sink-only rendering blocked runtime consumption.
15. JSON and CSS guard rows maintain or improve.
16. Sheets and BBNF-self witnesses are present as generated-role or
    fail-closed evidence with artifacts and hashes.
17. `row_move_toward_sota_status` is `pass`, `admitted`, or
    `measured_architectural_block`.
18. `pass` or `admitted` requires `generated_runtime_diff_status = present`,
    a nonempty diff artifact path, a valid SHA-256, and gate hash validation.
    Without that generated-runtime diff, only the named measured block is
    accepted.

Reject states:

- `support_only`, `gate_only`, `telemetry_only`, `scaffold_only`, `wired`,
  `integrated`, or `future_consumer`.
- SAT with zero variables, zero constraints, no objective, or empty named
  grammar set.
- best-so-far solution accepted after node-budget exhaustion.
- elapsed CSP solve time >1.0 second without measured timeout abrogation.
- hidden fused CSP/e-graph solver.
- any silent fallback to legacy P1-P8, default backend shape, or projected
  cost facts.
- priority/P1-P8 labels hard-pruning the CSP domain or contributing to the
  objective.
- `pass` or `admitted` without a hash-checked generated-runtime diff artifact.
- `fallback_invoked = true` on a row movement/admit claim.
- status-only CSS/Sheets/BBNF-self witnesses.
- JSON/CSS guard regression not recovered in-wave.
- missing Sheets or BBNF-self witness.
- claiming row admission from REDRESS-136, REDRESS-137, or REDRESS-138 alone.

## Report Shape

Report schema: `sk-v13-decision-csp-cascade-v1`.

Required fields:

- Provenance: `schema_version`, `wave_id`, `run_id`, `source_commit`,
  `host_triple`, `build_flags`, `feature_mask`, `consumer_gate`,
  `g_omega_status`.
- Inputs: `regex_fact_artifact_path`, `regex_fact_sha256`,
  `active_cost_artifact_path`, `active_cost_sha256`,
  `selection_trace_sha256`.
- CSP: `csp_solver_source`, `csp_solver_version`,
  `csp_problem_artifact_path`, `csp_problem_sha256`,
  `csp_solution_artifact_path`, `csp_solution_sha256`, `csp_status`,
  `csp_variable_count`, `csp_constraint_count`, `csp_objective_count`,
  `csp_named_grammars`, `csp_solve_ms`, `csp_timeout_ms`,
  `csp_node_budget`, `csp_nodes_explored`, `csp_budget_status`.
- Constraints: `parity_constraint_status`, `recognizer_constraint_status`,
  `substrate_constraint_status`, `simd_constraint_status`,
  `capacity_constraint_status`.
- Witnesses: `css_l4_witness_artifact_path`, `css_l4_witness_sha256`,
  `css_l4_witness_command`, `sheets_witness_artifact_path`,
  `sheets_witness_sha256`, `sheets_witness_command`,
  `bbnf_self_witness_artifact_path`, `bbnf_self_witness_sha256`,
  `bbnf_self_witness_command`, `scoped_witness_label`.
- Consumers: `resolver_output_piping`, `fused_solver_status`,
  `generated_selection_path`, `compile_consumer_path`,
  `same_wave_consumer_path`, `same_wave_consumer_class`.
- Runtime diff: `generated_runtime_diff_status`,
  `generated_runtime_diff_artifact_path`, `generated_runtime_diff_sha256`.
- Cascade: `cascade_retirement_status`, `choose_backend_shape_status`,
  `priority_table_status`, `p1_p8_fallback_status`,
  `legacy_cascade_admission_status`, `priority_data_role`,
  `priority_hard_prune_status`, `priority_objective_status`,
  `fallback_invoked`, `compat_fallback_status`.
- Static runtime blockers: `static_css_provider_status`,
  `json_sink_only_status`.
- Guards: `json_guard_state`, `css_guard_state`,
  `sheets_fail_closed_status`, `bbnf_self_fail_closed_status`,
  `lock14_status`.
- Disposition: `row_move_toward_sota_status`, `affected_row_ids`,
  `block_id`, `abrogate_status`, `material_differential`, `redress_entry`.

Gate flag: `--skv13-decision-csp-cascade-report`.

Gate print:

`G-W7-DECISION-CSP-CASCADE <row_move_toward_sota_status> <path>`.

## Measurement Rows

If executable generated-runtime consumption is present, first row movement
attempts should target the closest JSON direct rows:

- `json/numbers/direct_to_struct/main`.
- `json/instruments/direct_to_struct/main`.
- `json/unicode_mixed/direct_to_struct/main`.
- `json/y_string_unicode/direct_to_struct/main`.
- `json/unicode_escapes/direct_to_struct/main`.
- `json/distinct_values/direct_to_struct/main`.

Admitted CSS rows are guards unless the CSP-selected output demonstrably
changes a CSS generated provider path in the same wave.

## Preblocked Routes

Binding preblocks:

- REDRESS 84 object-pair value-byte/control-tail replay.
- REDRESS 85-87 / 121 generic JSON policy or cost-facts-as-evidence closure.
- REDRESS 114/115 JSON-local number/container patch replay.
- REDRESS 119/120 as row close evidence.
- REDRESS 136 W5 regex facts alone as row movement.
- REDRESS 137 W6 active-cost facts/lowering path alone as row movement.
- hidden fused CSP/e-graph solver.
- old P1-P8 cascade fallback as an admission path.
- default `EagerTape` / projected `CostFacts` / OffsetTape projection as
  production admission evidence.
- support-only CSP scaffold.
- feature-gated scaffold that does not affect lowering.
- JSON-specific branch in a generic crate.

## LOC, Risk, Cap

Risk: HIGH. The wave touches generic decision selection, generated backend
selection, gate reporting, and fail-closed lowering behavior.

Hard cap: 45 min redress + 15 min measurement under the decision-fold
amendment. Source/test/report LOC budget: <=970. If executable runtime
selection cannot fit with every gate predicate, redress must close the measured
block. Runtime emission may be routed as remainder only after W7 has recorded
non-admission; it cannot satisfy W7 by future promise.

## Revert Protocol

On fail:

1. Revert CSP dependency, `passes::decision_csp`, codegen fail-close,
   report/gate/xtask/Lock 14, and evidence artifacts as one slice.
2. Save rejected patch at `/tmp/skv13-waveW7-rejected.patch`.
3. Record REDRESS-138 with the failed condition: CSP timeout, budget exceeded,
   UNSAT, missing decision, silent fallback, generated-runtime non-consumption,
   JSON/CSS guard regression, missing non-JSON witness, or row movement miss.

## Verification Commands

- `cargo test -p passes decision_csp`.
- `cargo test -p codegen csp`.
- `cargo test -p bbnf-bench --lib skv13_decision_csp_cascade_report`.
- `cargo test -p bbnf-bench --bin gate skv13_decision_csp_cascade_report`.
- `cargo test -p xtask gate_json_passthrough_accepts_skv13_decision_csp_cascade_report_flag`.
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-decision-regex-report
  ../restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json
  --skv13-decision-active-cost-report
  ../restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json
  --skv13-decision-csp-cascade-report
  ../restart/skinny/tranches/sk-v13/research/w7/skv13-W7-decision-csp-cascade.json`.

## CHALLENGE Questions

- CH1: Does W7 reject best-so-far CSP results after node budget exhaustion,
  UNSAT, timeout, empty solution, or missing decisions?
- CH2: Are CSS, Sheets, and BBNF-self witnesses present, and are generic crate
  edits Lock 14 clean?
- CH3: Are REDRESS 84, 85-87/121, 114/115, 119/120, 136, and 137 still blocked
  as row-close authority?
- CH4: Does the implementation stay within the W7 budget, or does it split
  before attempting executable runtime selection?
- CH5: Are actual fallback surfaces fail-closed: missing backend shape, missing
  cost facts, default shape facts, and old cascade labels?
- CH6: Does the gate require either generated-runtime row movement or the named
  measured block, instead of accepting CSP plumbing or path strings?
