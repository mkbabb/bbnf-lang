# SK-V13 W7 Research - CSP + Cascade Fail-Closed

Date: 2026-05-21.
Wave: W7, SPEC Section 10.
Scope: Decision Fold C, CSP resolver plus old cascade fail-closed.
Disposition: research cohort converged; plan phase may proceed.

## Cohort

- Carver (`019e4ee9-a820-7ab3-8364-3d8b802e8c21`) audited the root
  `crates/csp-solver` API and skinny dependency route.
- Raman (`019e4ee9-a888-7be0-abc5-b2c060fb6c7d`) audited current
  `passes` backend selection and remaining P1-P8 cascade residue.
- Boole (`019e4ee9-a8f3-76b3-8a5e-7b57886215b4`) audited codegen and
  generated runtime consumer paths.
- Parfit (`019e4ee9-a95e-7322-999c-19817f595ee0`) specified the W7 gate
  report, Lock 14 owner set, and REDRESS evidence shape.
- Socrates (`019e4ee9-a9c1-7b91-8162-ae04f9b82a7b`) specified the
  measurement threshold, guard commands, and PASS-BLOCKED discipline.
- Ramanujan (`019e4ee9-aa27-74c0-b3e9-581ba64796f7`) audited
  abrogate-before-patch and deletion/fail-close routes for fallback
  compatibility.

## Findings

The root CSP crate is usable for W7. `csp_solver::Csp` exposes finite and
cost finite domains, hard and soft constraints, branch-and-bound optimization,
node budgets, and `SolveStats`. It does not expose a wall-clock timeout; W7
must time `solve_optimized` with `Instant` and record elapsed milliseconds.
The direct skinny dependency route is:

- `skinny/Cargo.toml`: add `csp-solver = { path = "../crates/csp-solver" }`
  under workspace dependencies.
- `skinny/crates/passes/Cargo.toml`: add `csp-solver.workspace = true`.

The old P1-P8 cascade is no longer a standalone production function, but it
still determines the candidate set in
`passes::recognizers::backend_candidates`. W6 then runs active e-graph cost
selection over those candidates. W7 must make CSP output authoritative after
that extraction. Priority labels may survive as evidence, but they must not
remain an admission path.

The remaining silent fallback surfaces are load-bearing:

- `codegen::lower::rust::shape_for` currently falls back to `EagerTape` when
  a backend shape is missing.
- `lower_to_rust` synthesizes projected `CostFacts` when facts are missing.
- `codegen::default_backend_shape` and `default_cost_facts` can synthesize
  OffsetTape projections for callers that do not provide pass output.

W7 should fail-close these surfaces for production compile/lowering evidence.
Compatibility fallbacks may remain only as explicit non-admission plumbing.
Any timeout, UNSAT, budget overflow, missing decision, structural-mode
no-decision path, or legacy fallback must be visible to the gate before a row
can claim admission.

Codegen remains the probable measured block. W6 already proves selected facts
reach `codegen::lower::rust::lower_to_rust`, but `emit_with_layout` discards
`rule_plans` and passes only the sink-only program into runtime rendering.
CSS L4 providers return static template files without `BackendIr` or
`CostFacts` inputs, while JSON appends a static sink-direct renderer. Therefore
W7 can wire resolver output into `compile()` now, but it admits only if the
resolver changes executable emitted JSON or CSS code and a row moves. If the
resolver stops at compile/lowering, W7 must close as a measured block.

## Gate Shape

W7 should add a gate-consumed report:

- Schema: `sk-v13-decision-csp-cascade-v1`.
- Struct: `SkV13DecisionCspCascadeReport`.
- Flag: `--skv13-decision-csp-cascade-report`.
- Gate print: `G-W7-DECISION-CSP-CASCADE <row_move_toward_sota_status>
  <path>`.

The report must hash-check W5 regex facts, W6 active-cost facts, the W7 CSP
problem artifact, and the W7 CSP solution or abrogation artifact. It must
record solver source/version, variable count, constraint count, objective
count, named grammars, elapsed solve milliseconds, node budget, nodes explored,
budget status, constraint family statuses, resolver output piping, fused solver
status, old cascade retirement status, JSON/CSS guard state, Sheets and
BBNF-self fail-closed witnesses, Lock 14 status, affected rows, block id,
material differential, and REDRESS entry.

The hard solve threshold is <=1.0 second per named grammar. A tighter 200 ms
target may be used for planning, but gate failure starts at >1.0 second unless
the report records measured timeout abrogation.

Accepted row movement states are `pass`, `admitted`, or
`measured_architectural_block`. Cascade retirement alone is not admission.
If no row moves, the likely block id is:

`JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT`.

## Plan Inputs

The minimum implementation route is:

1. Add `passes::decision_csp` and a direct `csp-solver` dependency.
2. Build one CSP variable per `RuleId`, with candidate values sourced from the
   W6 backend candidate set and costs derived from active-cost ordering.
3. Encode parity, recognizer, substrate, SIMD, and capacity constraints as
   hard constraints.
4. Capture `DecisionCspFacts` in `CostFacts` or a sibling resolver payload.
5. Make `compile()` / generated backend selection consume CSP facts.
6. Fail-close missing backend shapes and missing cost facts in production
   lowering evidence.
7. Add the W7 report/gate/xtask/Lock 14 owner-path plumbing and REDRESS-138.

Minimum validation:

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

## Material Differential

W7 is materially distinct from REDRESS 136 and REDRESS 137 because it makes the
resolver stage explicit and gate-consumed after regex facts and active e-graph
cost extraction. It is distinct from old P1-P8 priority behavior because the
priority cascade may only produce candidates/evidence; it cannot silently
admit JSON, CSS, Sheets, or BBNF-self rows after W7. Any compatibility fallback
is non-admission evidence.

W7 is not allowed to close on CSP plumbing alone. If selected resolver output
does not change emitted runtime code or move a row, W7 records a measured block
with solver metrics, guard status, and the static-template consumer evidence.
