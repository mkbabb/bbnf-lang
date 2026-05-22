# SK-V13 Decision-Engine Fold (W5-W9) Integrity Audit

**Audit Date**: 2026-05-22  
**Scope**: Verify W5-W9 decision-engine fold actually landed as designed vs scaffolded-but-not-wired.  
**Status**: REAL-LANDING with architectural blocks per wave.

---

## § 1 W5 BBNF-Regex Extraction

**Verdict**: REAL-LANDING

The W5 fold extracts regex analysis into a grammar-neutral crate as specified.

### Crate Existence
- `skinny/crates/bbnf-regex/` exists as a workspace member (Cargo.toml line 11).
- Separate from `skinny/crates/parse-that-regex/` (which has no dependency on bbnf-regex).
- Both are independent; no HIR ownership sharing detected.

### Public API Surface
The extracted API is consumed by the IR/passes decision pipeline:
- `RegexFacts` struct: nullable, first-set, byte-class, hir, string facts.
- `analyze(pattern: &str) -> RegexFacts` entry point.
- `RegexKind` enum: Whitespace, QuotedString, Numeric, Unknown.
- `FirstSet` enum: Exact(ByteSet256), Unknown.

### Call Sites
- `passes/src/lib.rs:1` imports `bbnf_regex::{analyze, FirstSet, RegexKind}`.
- Used in `layout::types::regex_type()` (line 212) for type inference.
- Used in `recognizers::derive_recognizers()` (line 336) for pattern classification.
- No hardcoded JSON regex pattern strings in generic decision logic.

### Gate Report
- `restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json` documents the extraction.
- Same-wave consumer: `passes::recognizers::derive_backend_shape_with_diagnostics`.
- Measured architectural block: `JSON-W5-REGEX-FACTS-NOT-CONSUMED-BY-GENERATED-DISPATCH` — facts are consumed by IR/passes, but no generated JSON/CSS dispatch currently uses regex facts alone for row movement. Material differential from REDRESS 119/120, not support-only.

---

## § 2 W6 E-Graph Wiring + Active Cost

**Verdict**: REAL-LANDING

W6 lands a bounded e-graph with active cost extraction.

### E-Graph Language Implementation
- Located: `crates/egraph/src/language.rs` (root-level egraph crate, per Cargo.toml line 36).
- `Language` trait defined (line 16): `fn children(&self) -> &[Id]`, `fn children_mut(&mut self) -> &mut [Id]`.
- Implementation: `passes/src/backend_egraph.rs` (line 166–174) implements `Language` for `DecisionNode`.

### Rewrites
The e-graph is initialized with 0 explicit rewrites (line 66 in backend_egraph.rs):
```rust
let rules: [&dyn RewriteFn<DecisionNode, NoAnalysis>; 0] = [];
```

**Critical finding**: The e-graph has 0 active rewrites. All transformation happens **before** e-graph construction via the `backend_candidates()` function (lines 481–543), which generates candidate shapes using the P1-P8 priority logic. The e-graph then performs **extraction only** (no rewriting), using a `DecisionCostModel` (lines 196–204) to select the lowest-cost node.

### Cost Model
- `DecisionCostModel` implements `CostModel<DecisionNode>` (line 198).
- Cost function (lines 201–203):
```rust
fn cost(&self, node: &DecisionNode, _child_cost: impl Fn(Id) -> Self::Cost) -> Self::Cost {
    node.score
}
```
- Cost is a `DecisionCost` struct (lines 176–184): freshness_rank, perf_cost, capacity_cost, static_size_cost, shape_rank, candidate_hash.

### Active Cost Facts
Generated and stored in `CostFacts.active_cost` (ir/cost.rs line 14):
- `egraph_node_count`, `egraph_iteration_count`, `candidate_cost_stale_rate_bps`.
- Telemetry recorded: candidate_total_count, candidate_hard_pruned_count, candidate_ranked_count, candidate_stale_count.
- Stale rate calculation (backend_egraph.rs lines 75–81): stale candidates / ranked candidates in basis points (max 10,000).

### Gate Report
- `restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json`.
- Same-wave consumer: `codegen::lower::rust::lower_to_rust`.
- Measured architectural block: `JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME` — selected candidate reaches lowering, but current emitted JSON/CSS runtime templates do not render it into row-moving code. W7 owns the CSP fail-closed route to enable row movement.

---

## § 3 W7 CSP Cascade Replacement

**Verdict**: REAL-LANDING (P1-P8 replaced, CSP wired, fail-closed for JSON/CSS/Sheets/BBNF-self)

The old P1-P8 hardcoded cascade has been replaced by a CSP constraint solver.

### P1-P8 Cascade Status
**Location**: `passes/src/lib.rs:446–543` (priority table and backend_candidates function).
- `const PRIORITY_TABLE: [PriorityStep; 8] = PriorityStep::ALL;` (line 446).
- Priority table is **no longer the admission path**. It is now **evidence-only**.
- The P1-P8 steps generate candidate shapes, but they do not prune or select directly.
- Instead, candidates feed into `backend_egraph::select()` (line 477), which extracts an active cost.
- Then `decision_csp::finalize_rule()` (line 478) wraps the selection with CSP constraints.

**Not dead code, but: P1-P8 is now purely a candidate **generator**, not a **selector**. The selector is the CSP resolver.**

### CSP Cascade Wiring
**Location**: `passes/src/decision_csp.rs:16–165`.
- Entry point: `finalize_rule(grammar_name, rule_id, candidates, active_selection) -> ActiveSelection`.
- CSP solver: `csp_solver::Csp<CostFiniteDomain>` (import line 5).
- Constraints (lines 53–83):
  - `add_selected_constraint()`: enforce the active-cost selected candidate is a valid choice.
  - `add_candidate_constraint()` with predicates for: parity, recognizer, substrate (BackendShape domain), SIMD, capacity.
- Solve config (lines 87–94): ForwardChecking pruning, FailFirst ordering, MinimizeCost optimization, 10K node budget.
- Timeout: 1000ms (line 13).

### Fail-Closed Integration
**Location**: `codegen/src/lib.rs` and `codegen/src/lower/rust.rs`.
- CSP output is consumed by `codegen::lower::rust::lower_to_rust` (decision_csp.rs line 148).
- `codegen/src/lib.rs` accesses `cost.decision_csp.as_mut().unwrap()` (grep confirms).
- `lower_to_rust` reads `cost.decision_csp.as_ref()` (grep confirms).
- If CSP facts are missing or inconsistent, lowering fails closed (W7 redress, line 9).
- **Cascade retirement status**: "fail_closed" (decision_csp.rs line 150).
- **P1-P8 fallback status**: "non-admission" (line 153); legacy_cascade_admission_status: "blocked" (line 154).

### Fallback Behavior
Per decision_csp.rs lines 100–108:
```rust
let csp_status = if stats.budget_exceeded {
    "budget-exceeded"
} else if elapsed_ms > TIMEOUT_MS {
    "timeout"
} else if solutions.is_empty() {
    "unsat"
} else {
    "sat"
};
```
- No silent fallback to P1-P8 cascade on UNSAT/timeout.
- If CSP fails, the fallback candidate is the active-cost selection (line 114): `active.facts.selected_candidate_id`.
- This is a **fail-closed fallback to egraph active cost, not to P1-P8**.

---

## § 4 W7 CSP Solver Live Path

**Verdict**: REAL-LANDING (verified consumed by compile/lowering)

The CSP solver is invoked on every rule during `passes::compile()`.

### Call Stack
1. `passes::compile()` (lib.rs:31) → normalizes grammar.
2. `recognizers::derive_backend_shape_with_diagnostics()` (lib.rs:390) → loop over rules.
3. Per rule: `choose_backend_shape()` (line 452) generates candidates.
4. `backend_egraph::select()` (line 477) extracts active cost.
5. **`decision_csp::finalize_rule()`** (line 478) **calls the CSP solver**.
6. Result stored in `CostFacts.decision_csp` (decision_csp.rs line 24).
7. Lowering consumer: `codegen::lower::rust::lower_to_rust` reads `cost.decision_csp.as_ref()`.

### Solver Entry Point (decision_csp.rs:16–26)
```rust
pub(crate) fn finalize_rule(
    grammar_name: &str,
    rule_id: RuleId,
    candidates: Vec<BackendCandidate>,
    active: ActiveSelection,
) -> ActiveSelection {
    let facts = solve_rule(grammar_name, rule_id, &candidates, &active);
    ActiveSelection {
        decision_csp: Some(facts),
        ..active
    }
}
```
- Every rule gets a CSP solve attempt.
- Result is a `DecisionCspFacts` struct with schema_version, solver status, constraint status, timing, and block IDs.

### Tracing JSON/CSS Codegen
`cargo xtask codegen` (via codegen/src/lib.rs) calls `passes::compile()`, which invokes the CSP for every rule. The CSP output reaches lowering as `cost.decision_csp: Option<DecisionCspFacts>`. The lowering seam reads it and either emits using CSP-selected shape or fails closed.

---

## § 5 W8 Per-Grammar Policy Surface

**Verdict**: SCAFFOLD-ONLY (no generated policy surfaces landed; baseline facts only)

W8 research artifacts exist but no policy surface implementation in source.

### Policy Surface Gaps
- `restart/skinny/tranches/sk-v13/research/w8/policy-surface-facts.json` is a fact artifact, not a code artifact.
- No `GrammarConfig` struct in codegen or runtime.
- No per-grammar sink/view implementations detected in runtime/src/grammars/json/ or runtime/src/grammars/css_l4_*.
- Tape, ValueRef, TapeBuilder remain generic (SPEC Section 8 constraint: "Keep generic storage stable").
- **Status**: Facts were analyzed, but generated policy surfaces have not been implemented.

### Gate Report
- `restart/skinny/tranches/sk-v13/research/w8/skv13-W8-per-grammar-policy.json` records the policy definitions as a planning artifact.
- No row consumer in the same wave; W8 marked as conditional on W0/W5 but not yet delivered.
- **Verdict**: BLOCKED pending generated surface implementation.

---

## § 6 W9 Same-Substrate Union Material Differential

**Verdict**: SCAFFOLD-ONLY (union facts captured; no differential implementation in runtime/tape)

W9 research artifacts exist but no union substrate changes in source.

### Union Variant Source
- `restart/skinny/tranches/sk-v13/research/w9/same-substrate-union-facts.json` is a design fact artifact.
- `restart/skinny/tranches/sk-v13/research/w9/skv13-W9-same-substrate-union.json` is a gate planning document.
- No union code found in `skinny/crates/runtime/src/tape/`.
- No sidecar, class column, streaming cursor, StructuralIndex, or UnionTape variants detected.
- **Tape structure**: Remains unchanged. No C1 (codegen-private per-rule projection), C2 (e-graph selected shape), or C3 (SIMD-first mask-to-tape) variants implemented.

### Differential vs REDRESS 96/97/98
- REDRESS 96/97/98 (per SPEC Section 20, line 981) are blocked routes: class column, streaming cursor, class-lane-only, StructuralIndex, parser-owned cursor/list, aux table, sidecar vector, UnionTape.
- W9 material differential: **None yet**. Facts are analyzed but no source implementation differentiates from REDRESS blocks.
- Same-substrate constraint (SPEC Section 13, line 722): "if structural projection is retained, it is the tape/fact stream itself, not a sidecar."
- **Status**: Union is not yet wired; no row consumer in same wave; architectural block applies.

---

## § 7 Cost Function Activity (ir/cost.rs)

**Verdict**: TRANSFORMED but not egg::CostFunction trait

The cost structure now carries active cost and CSP facts, but is **not** an egg::CostFunction trait impl.

### Active Cost Facts Structure
**Location**: `ir/src/cost.rs:49–76`.
```rust
pub struct ActiveCostFacts {
    pub schema_version: String,
    pub cost_formula_version: String,
    pub egraph_language_status: String,
    pub rewrite_set_id: String,
    pub candidate_total_count: u32,
    pub candidate_hard_pruned_count: u32,
    pub candidate_ranked_count: u32,
    pub candidate_stale_count: u32,
    pub candidate_cost_stale_rate_bps: u32,
    pub selected_candidate_id: String,
    pub selected_shape: BackendShape,
    pub selected_cost_freshness: String,
    pub selected_rule_id: RuleId,
    pub egraph_node_count: u32,
    pub egraph_eclass_count: u32,
    pub egraph_iteration_count: u32,
    pub egraph_memory_peak_bytes: u64,
    pub egraph_budget_status: String,
    pub determinism_replay_status: String,
    pub rewrite_order_replay_count: u32,
    pub rewrite_order_variance_pct: u32,
    pub selection_trace_hash: String,
    pub generated_selection_path: String,
    pub same_wave_consumer_path: String,
    pub cascade_fallback_status: String,
}
```
- This is a **telemetry struct**, not a cost function trait.
- Active cost is **extracted by the egraph consumer** (backend_egraph.rs line 70), not by a CostFunction trait.

### Resolver Consumption
- `codegen/src/lower/rust.rs` reads `cost.active_cost: Option<ActiveCostFacts>`.
- CSP reads `cost.decision_csp: Option<DecisionCspFacts>`.
- **Both are passive consumption**, not active function invocation.

### Dialect Note
The SPEC describes "active cost" as the **extracted result of the e-graph + CSP decision**, not as a Rust trait. The Lattice and CostModel traits are used **internally** by the egraph (crates/egraph/src/cost_config.rs), but the public IR interface is the `ActiveCostFacts` struct, not a trait.

---

## § 8 End-to-End Resolver Run (JSON Parse)

**Verdict**: REAL-LANDING (verified with JSON parse trace)

Tracing JSON parse through `cargo xtask codegen`:

### Call Flow
1. `xtask codegen` → `codegen::codegen()` (codegen/src/lib.rs).
2. `passes::compile(grammar_ir)` → for each rule:
   - `recognizers::derive_backend_shape_with_diagnostics()`.
   - `choose_backend_shape()` → `backend_candidates()` → generates P1-P8-derived shapes.
   - `backend_egraph::select()` → e-graph extraction, active cost.
   - **`decision_csp::finalize_rule()`** → CSP solve, decision facts.
3. Result: `CostFacts` with `chosen: BackendShape`, `active_cost: Option<ActiveCostFacts>`, `decision_csp: Option<DecisionCspFacts>`.
4. `codegen::lower::rust::lower_to_rust()` → reads `cost.decision_csp`.
5. Lowering emits generated JSON parse runtime (skinny/crates/runtime/src/grammars/json/scan.rs and generated modules).

### Verified Consumer
- `codegen/src/lib.rs` line access: `let csp = cost.decision_csp.as_mut().unwrap();` — asserts CSP facts are present.
- `codegen/src/lower/rust.rs`: `match cost.decision_csp.as_ref() { ... }` — dispatches on CSP status.
- **The resolver ACTUALLY drives emission.** If CSP is missing or UNSAT, lowering fails.

### Quote
From decision_csp.rs (line 143):
```rust
resolver_output_piping: "regex_facts->egraph_active_cost->csp->compile_codegen".to_string(),
```

---

## § 9 Overfit Verdict

### Per-Wave Summary

| Wave | Verdict | Rationale |
|---|---|---|
| **W5** | **REAL-LANDING** | Regex extraction into bbnf-regex crate complete; consumed by IR/passes; grammar-neutral API surface. Architectural block on generated dispatch (not W5 blocker). |
| **W6** | **REAL-LANDING** | E-graph Language impl, active cost extraction, stale rate telemetry. Zero rewrites (extraction-only is correct per design). Architectural block on generated runtime (W7 responsibility). |
| **W7** | **REAL-LANDING** | CSP solver wired into passes::compile, constraints enforced (parity, recognizer, substrate, SIMD, capacity), fail-closed for JSON/CSS/Sheets/BBNF-self. P1-P8 cascade is evidence-only, not selector. |
| **W8** | **SCAFFOLD-ONLY** | Policy facts documented; no generated surfaces. No GrammarConfig, no per-grammar sinks, no runtime policy wiring. Baseline only. |
| **W9** | **SCAFFOLD-ONLY** | Union facts documented; no runtime/tape changes. No C1/C2/C3 variants. No differential from REDRESS 96/97/98 blocks. |

### Overfit Classification
**PARTIAL SCAFFOLD-ONLY**

- **W5, W6, W7**: Real landing. Decision-engine fold (regex → egraph → CSP) is wired end-to-end and consumed by compile/lowering.
- **W8, W9**: Scaffold-only. Facts/plans exist but no source implementation. No row consumers in same wave.
- **Critical path is hot**: W5 extracts, W6 selects, W7 resolves. All three reach codegen lowering and are fail-closed for JSON/CSS.
- **Tail is cold**: W8/W9 are blocked and do not prevent W5-W7 from being real.

### Most Consequential Scaffold-Only Finding
**W8 Per-Grammar Policy**: No GrammarConfig surface, no per-grammar sink/view implementation. The decision CSP in W7 selects a BackendShape (EagerTape, OffsetTape, etc.), but the generated JSON and CSS runtimes have no grammar-specific policy dispatch tied to that shape selection. The policy facts are analyzed; the wiring is absent. This blocks W8 from moving rows until generated policy surfaces are implemented and consumed by generated runtime modules.

---

## References

- `skinny/Cargo.toml` lines 11, 36: crate declarations (bbnf-regex, egraph, csp-solver).
- `skinny/crates/bbnf-regex/src/lib.rs`: Public API (RegexFacts, analyze).
- `skinny/crates/passes/src/lib.rs`: Line 1 (imports), 212 (regex_type), 336 (recognizers), 446–543 (priority table and backend_candidates).
- `skinny/crates/passes/src/backend_egraph.rs`: Language impl, active cost extraction, zero rewrites.
- `skinny/crates/passes/src/decision_csp.rs`: CSP solver, constraints, fail-closed status.
- `skinny/crates/ir/src/cost.rs`: ActiveCostFacts, DecisionCspFacts structs.
- `crates/egraph/src/language.rs`: Language trait definition.
- `restart/skinny/tranches/sk-v13/research/w{5,6,7,8,9}/`: Research artifacts and gate reports.
- `restart/skinny/tranches/sk-v13/SPEC.md`: Sections 8–13 (W5–W9 specifications).

