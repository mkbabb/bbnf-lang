# SK-V13 W6 Plan - Bounded E-Graph + Active Cost

Cycle: W6 Plan. Scope: SPEC Section 9.

## Selected Intervention

Replace the passive `passes::recognizers::choose_backend_shape` priority pick
with a bounded active-cost selector that writes its extracted winner into the
existing `CostFacts.chosen` / `BackendShapePlan` surface. The generated
lowering path already consumes that surface through `CostFacts`; W6 will make
that consumption gate-visible and will reject any support-only e-graph telemetry
that cannot move or block a row.

Use the local root `crates/egraph` crate if it compiles cleanly as a direct
skinny path dependency. If that path is blocked by edition/MSRV or path-patch
coupling, redress may use a challenge-accepted equivalent representation, but
it must still emit the same active-cost report fields and deterministic replay
evidence. In either case, the selector is bounded by the W6 node, iteration,
memory, stale-rate, and rewrite-variance gates.

If the selected candidate is consumed only by `CostFacts`/lowering metadata and
not by emitted generated runtime code, W6 records the measured architectural
block:

`JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME`.

That block is acceptable only with a gate-consumed report, REDRESS evidence,
material differential from REDRESS 87/119/120/136, and maintained JSON/CSS
guards. It is not G2 completion; W7 owns CSP and cascade fail-closed.

## Owner Paths

Source owner paths:

- `skinny/Cargo.toml`.
- `skinny/crates/passes/Cargo.toml`.
- `skinny/crates/passes/src/lib.rs`.
- `skinny/crates/passes/src/backend_egraph.rs` or an equivalent W6 module.
- `skinny/crates/ir/src/cost.rs`.
- `skinny/crates/codegen/src/lower/mod.rs`.
- `skinny/crates/codegen/src/lower/rust.rs`.
- `skinny/crates/bbnf-bench/src/report.rs`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.
- `skinny/xtask/src/main.rs`.

Evidence/document owner paths:

- `restart/skinny/tranches/sk-v13/research/w6/`.
- `skinny/REDRESS.md`.
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` only if a row
  moves or admits.

Root `crates/egraph/` is a dependency surface, not a W6 edit surface, unless
redress hits a compile blocker that cannot be solved inside skinny.

## Implementation Shape

1. Add a W6 backend decision module in `passes`.
2. Build canonical backend-shape candidate expressions from the existing
   `BackendRule`, W5 regex facts, `TargetFeatures`, and structural preconditions.
3. Apply only conservative grammar-neutral rewrites:
   - sequence/alternative identity and flattening.
   - singleton collapse.
   - duplicate alternative removal.
   - W5-FIRST-proven dispatch canonicalization.
4. Score ranked candidates with integer active cost:
   - freshness rank.
   - measured/static proxy performance cost.
   - `CapacityPolicy` cost.
   - static-size cost.
   - fixed `BackendShape` rank.
   - canonical candidate SHA-256 tie-breaker.
5. Replace the old direct return path with the active selected `BackendShape`,
   while preserving existing `BackendShapePlan` and `CostFacts` consumers.
6. Extend `CostFacts` with optional W6 active-cost telemetry:
   - schema/cost formula version.
   - candidate totals.
   - ranked/hard-pruned/stale counts.
   - stale rate.
   - selected candidate id/hash.
   - e-graph node/class/iteration/memory telemetry.
   - determinism replay and rewrite variance.
   - generated selection consumer path.
7. Add `sk-v13-decision-active-cost-v1` report validation and xtask
   passthrough.
8. Add Lock 14 owner paths for W6.

The implementation must not add a new `BackendShape`, BIR variant, public
substrate API, hidden CSP route, or JSON-specific generic branch.

## Falsifiability Gate

Primary gate: `G-W6-DECISION-ACTIVE-COST`.

Pass conditions:

1. E-graph/active-cost selection is bounded:
   - final nodes <=100,000.
   - final nodes / initial nodes <=16.0.
   - iterations <=64 by default and never >100.
   - memory estimate or measurement <1 GiB.
2. Active cost deterministically selects a candidate; replay status is `pass`.
3. Rewrite-order extraction cost variance is <=10%.
4. Candidate stale-rate satisfies
   `candidate_stale_count * 10 <= candidate_ranked_count * 3`, and the selected
   candidate is not stale unless redress abrogates.
5. `gate-json --skv13-decision-active-cost-report` consumes the report.
6. JSON guards and admitted CSS rows maintain under advisory gate checks.
7. The selected candidate is consumed by generated backend selection and either
   moves/admit a JSON or CSS row, or records the measured architectural block
   named above.

Reject states:

- `support_only`, `gate_only`, `telemetry_only`, `scaffold_only`, `wired`,
  `integrated`, or `future_consumer`.
- empty generated selection or same-wave consumer path.
- hidden fused CSP/egraph solver.
- silent old P1-P8 fallback admission.
- stale-rate >30% without abrogation.
- nondeterministic selected winner.
- JSON/CSS guard regression not recovered in-wave.

## Report Shape

Report schema: `sk-v13-decision-active-cost-v1`.

Required fields:

- `schema_version`, `wave_id`, `run_id`, `source_commit`, `host_triple`,
  `build_flags`, `feature_mask`, `consumer_gate`, `g_omega_status`.
- W5 dependency: `regex_fact_artifact_path`, `regex_fact_sha256`.
- E-graph: `egraph_language_status`, `rewrite_set_id`, `egraph_node_count`,
  `egraph_eclass_count`, `egraph_iteration_count`,
  `egraph_memory_peak_bytes`, `egraph_budget_status`.
- Cost: `cost_function_source`, `cost_formula_version`,
  `candidate_total_count`, `candidate_hard_pruned_count`,
  `candidate_ranked_count`, `candidate_stale_count`,
  `candidate_cost_stale_rate`, `selected_candidate_id`, `selected_rule_id`,
  `selected_shape`, `selected_cost_freshness`,
  `capacity_policy_cost_status`.
- Determinism: `determinism_replay_status`, `rewrite_order_replay_count`,
  `rewrite_order_variance_pct`, `selection_trace_sha256`.
- Evidence: `cost_facts_artifact_path`, `cost_facts_sha256`,
  `generated_selection_path`, `same_wave_consumer_path`,
  `same_wave_consumer_class`, `row_move_toward_sota_status`, `block_id`,
  `cascade_fallback_status`, `abrogate_status`, `material_differential`,
  `redress_entry`, `csp_solve_ms`.

`csp_solve_ms` must be explicitly `n/a:w6-before-csp`; silent omission rejects.

## Measurement Rows

Primary row-movement attempt, if generated runtime consumption is present:

- `json/numbers/direct_to_struct/main`.
- `json/instruments/direct_to_struct/main`.
- `json/random/direct_to_struct/main`.
- `json/mesh/direct_to_struct/main`.
- `json/canada/direct_to_struct/main`.

The W10 admitted CSS rows are maintain guards unless the selected candidate is
shown to alter a CSS generated provider path in the same wave.

## Preblocked Routes

Binding preblocks:

- REDRESS 84 object-pair value-byte/control-tail replay.
- REDRESS 87 cost facts as evidence-only closure.
- REDRESS 114/115 JSON-local number/container patch replay.
- REDRESS 119/120 as row close evidence.
- REDRESS 121 generic grammar branch leakage.
- REDRESS 136 W5 regex facts alone as row movement.
- fused CSP/egraph solver before W7.
- old P1-P8 cascade fallback as an admission path.
- support-only e-graph scaffold.

## Revert Protocol

On fail:

1. Revert W6 source/report/gate/Lock 14/evidence changes as one slice.
2. Save the rejected patch at `/tmp/skv13-waveW6-rejected.patch`.
3. Record REDRESS with the failed condition: compile/path-dep blocker,
   e-graph budget hit, stale cost >30%, nondeterministic extraction,
   JSON/CSS guard regression, no generated selection consumer, or row movement
   miss.

## Verification Commands

- `cargo test -p passes active_cost`.
- `cargo test -p ir cost`.
- `cargo test -p codegen cost_facts`.
- `cargo test -p bbnf-bench --bin gate skv13_decision_active_cost_report`.
- `cargo test -p xtask gate_json_passthrough_accepts_skv13_decision_active_cost_report_flag`.
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-decision-regex-report ../restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json --skv13-decision-active-cost-report ../restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json`.

## CHALLENGE Questions

- CH1: Does the active selector preserve the current JSON/CSS selected shapes
  when no fresh row-moving candidate exists?
- CH2: Does the W6 e-graph language remain grammar-neutral and Lock 14 clean?
- CH3: Are REDRESS 84/87/114/115/119/120/121/136 still blocked?
- CH4: Do node, iteration, memory, stale-rate, and rewrite-order variance
  gates bound the implementation?
- CH5: Does codegen actually consume the selected candidate into emitted code,
  or must W6 record the generated-runtime block?
- CH6: Does the report reject support-only, telemetry-only, stale, hidden-CSP,
  and silent-cascade states?
