# SK-V13 Scoping: Decision-Engine Fold (CSP+EGraph+Cost) + bbnf-regex Extraction

**Date**: 2026-05-21  
**Scope**: Concrete wave planning for the SK-V13 resolver architecture fold (recommendation from §7 of `skv12-decision-engine-audit.md`)  
**Target**: Fold CSP + e-graph + cost model into one unified resolver, replacing the hardcoded P1-P8 priority cascade.

---

## §1 Surface Delta vs Prior Audit

**Prior Audit**: `skv12-decision-engine-audit.md` (2026-05-20, ~17 min elapsed).

**Audit Findings Summary**:
- Cost model (`ir/src/cost.rs`, 135 LOC) is **passive ledger**, not active optimizer.
- CSP solver (`csc411` path-dep) is **STUBBED** in main repo, **ABSENT** from skinny.
- E-graph / `egg` is **ABSENT** from skinny; zero `egg`, `EGraph`, `Rewrite` tokens in codebase.
- Backend shape choice (`passes/src/lib.rs:446–506`) hardcodes **P1–P8 priority cascade** via literal if/match chains.
- Recognizer mining (`derive_recognizers()`, lines 324–350) is **hand-curated**, JSON-specific fixture (hardcoded alphabet `{}\[\],:"`).
- Pluggability: **FAIR** — lowering trait (`ShapeLowering`) is pluggable; everything feeding it is hardcoded.

**Commits Since Audit** (2026-05-20 to now):
- `7a6b3c4d` (2026-05-21): `docs(sk-v12-waveW5-redress): close SK-V12 PASS-ADMIT`
- Most recent decision-engine touch: `51d8c8be` (sk-v7-wave3, 2025): `feat(sk-v7-wave3): admit costfacts substrate projection`

**Δ Code Changes**: ZERO. No changes to `ir/src/cost.rs`, `ir/src/lib.rs`, `passes/src/lib.rs:446–506`, or recognizer logic since audit was written. Campaign close SK-V12 is **ADMITTED**, **not a FIXPOINT**; implies Lock-14 CSS evidence satisfied, but no new decision-engine infrastructure was required.

**Δ Architectural Intent**: SK-V12 close did NOT pivot toward resolver unification. Recommendation to fold CSP+egraph+cost is **prospective for SK-V13**, not retroactive.

---

## §2 SK-V13 Fold Scope: Concrete Wave Plan

The recommendation is to **replace hardcoded P1–P8 cascade with CSP+egraph+cost resolver**. This section breaks down the fold into concrete LOC envelopes, entry/exit gates, and owner crates.

### §2.a bbnf-regex Crate Extraction

**Precondition**: **CRITICAL PATH**. E-graph rewrites and CSP constraints reference regex patterns; generic regex analysis must be factored out first.

**Current State**:
- Bespoke regex HIR embedded in `parse-that-regex/src/lib.rs` (1214 LOC).
- Two hardcoded pattern predicates in `ir/src/lib.rs` (lines 327–335 for `regex_is_nullable`, 782–799 for `regex_first_bytes`).
- Pattern matching on literal strings: `r"[ \t\n\r]*"`, `r#""...""#`, `r"-?(0\|...)"`.

**Wave Scope**:
| Item | Envelope | Owner Crate | Entry Gate | Exit Gate |
|------|----------|-------------|-----------|-----------|
| **Module refactor** | 50–80 LOC | `bbnf-regex/src/lib.rs` (new) | Split `parse-that-regex::regex` module from `integration`, `number`, `unicode` siblings. | Public API surface stable; `pub mod regex_hir`, `pub fn nullability_analysis`, `pub fn first_set_analysis`. |
| **Public API surface** | 40–60 LOC | `bbnf-regex/` + `parse-that-regex/` | Define `RegexHir` enum (nullable, first-set, character class predicates). | `parse-that-regex` re-imports `bbnf-regex` publicly. |
| **Re-import sites** | 20–40 LOC | `ir/`, `codegen/`, `runtime/` | Grep for all uses of regex predicates; replace with `bbnf_regex::nullability_analysis()` calls. | Zero hardcoded pattern strings in IR validation. |
| **WASM sub-crate** | 100–150 LOC | `wasm-bbnf-regex/` (new) | Per memory `wasm-subcrate-pattern`: wrap regex analysis in WASM bindgen + JS interop. | Standalone `.wasm` artifact; JS can invoke nullability checks. |
| **Total**: | **210–330 LOC** | 5 crates touched | Audit existing uses in `ir`, `codegen`, `parse-that-regex`. | Test: two-stage regex analysis (e.g., `nullable("[ \t]*")` + `first_bytes()`) works cross-crate. |

**Challenge Risk**: Regex pattern analysis (nullable, first-set, character-class membership) is scattered across multiple functions. Extracting requires identifying **all call sites** that hardcode pattern strings and consolidating into a unified `RegexAnalyzer` trait.

---

### §2.b E-Graph Language Implementation

**Precondition**: Follows bbnf-regex extraction (regex patterns feed rewrite rule guards).

**Current State**:
- No `Language` impl in skinny.
- `egg` library exists in main repo (`crates/egraph/src/language.rs`); unused in skinny.
- IR enums (`BackendExpr`, `BackendShape`, etc.) do not derive `Language`.

**Wave Scope**:
| Item | Envelope | Owner Crate | Entry Gate | Exit Gate |
|------|----------|-------------|-----------|-----------|
| **Language impl** | 200–300 LOC | `passes/src/egraph.rs` (new) | Auto-derive `Language` for `BackendExpr` via `#[derive(egraph_derive::Language)]`. | `passes::egraph::BackendExprLanguage` trait impl compiles; all IR nodes interned + unwrapped. |
| **Rewrite rules** | 400–600 LOC | `passes/src/egraph/rewrites.rs` (new) | Define ~15 canonical rewrites (tape↔event↔direct, structural-projection, regex-NFA-vs-DFA, scalar-vs-SIMD). | Each rewrite has precondition guard (e.g., `!regex_is_nullable()`) and is testable in isolation. |
| **Cost model wiring** | 150–200 LOC | `passes/src/egraph/cost.rs` (new) | Wire `ir::cost.rs:CostFacts` as `egg::CostModel<BackendExpr, ()>`. | `extract_with_cost()` picks Pareto frontier; `cost_facts` emitted per best expr per eclass. |
| **E-graph driver** | 100–150 LOC | `passes/src/lib.rs` (modify) | Gate behind `#[cfg(feature = "sk-v13-egraph")]` to avoid breaking SK-V12 builds. | `compile()` function calls new `saturate_and_extract()` instead of `choose_backend_shape()`. |
| **Total**: | **850–1250 LOC** | 2 crates (`passes/`, `egraph` main-repo dep) | Confirm `egg` can intern 1000+ IR nodes (tape shapes) without blowup. | Integration test: JSON grammar → egraph saturation → extracted plan matches prior cost-driven choice. |

**Rewrite Examples** (10–15 sketches):
1. **OffsetTape → EventTape** (when branch count > 4 and input entropy low)
2. **EventTape → OffsetTape** (when memory budget tight)
3. **DirectBuild → Tape** (when no direct-build field consumer)
4. **Tape → DirectBuild** (when layout scope wide, schema-wide materialization)
5. **SinkOnly → OffsetTape** (when multi-consumer)
6. **EagerTape → OffsetTape** (when recovery annotation absent)
7. **Regex::NFA → Regex::DFA** (when state count < 16, input alphabet small)
8. **Regex::DFA → Regex::NFA** (when code size > 4KB)
9. **SIMD::Exact → SIMD::Fuzzy** (when alphabet char distance > 4)
10. **SIMD::Fuzzy → Scalar** (when SIMD register pressure high)
11. **Structural-Projection** (collapse redundant intermediate shapes in tape chains)
12. **Container↔Pair nesting** (flatten vs. tree layout trade-off)
13. **Pattern-Merging** (combine similar literal-match recognizers)
14. **Cost-Annotated Fusion** (tape + cost facts → best-effort expression selection)

**Challenge Risk**: E-graph saturation can explode if rewrite rules are too permissive. Precondition guards (regex nullability, entropy heuristics, budget checks) must be conservative; otherwise memory usage during saturation can exceed 10GB on large grammars.

---

### §2.c Cost Model as Active CostFunction

**Precondition**: Follows e-graph Language implementation.

**Current State**:
- `CostFacts` struct (lines 4–13 in `cost.rs`) is data-only; `rejected`, `priority_fired`, `rationale` are post-hoc metadata.
- No `CostModel` trait impl; no per-expr cost evaluation.
- `choose_backend_shape()` produces a `CostFacts` struct but does not consume cost estimates during decision.

**Wave Scope**:
| Item | Envelope | Owner Crate | Entry Gate | Exit Gate |
|------|----------|-------------|-----------|-----------|
| **CostModel trait impl** | 150–250 LOC | `ir/src/cost.rs` (modify) + `passes/src/egraph/cost.rs` | Define `impl egg::CostModel<BackendExpr, ()>` that maps expr → scalar cost. | `egraph.extract()` picks minimum-cost expr per eclass; no tiebreaker needed. |
| **Cost function logic** | 200–300 LOC | `passes/src/egraph/cost.rs` | Consume `Measurement` evidence from `CostFacts::rejected`; compute heuristic cost per shape (e.g., `1000 / throughput_mbps + 0.5 * code_size`). | Test: two equivalent tapes on a workload; cost function ranks them consistently. |
| **Evidence backfill** | 100–150 LOC | `passes/src/diagnostics.rs` (modify) | Replace hardcoded `redress_72_evidence()` stubs with real bench-harness invocation or static-analysis approximation. | `rejected[]` vectors populated with `Measurement` entries; source = `BenchProbe` or `StaticAnalysis`. |
| **Capacity policy integration** | 50–100 LOC | `ir/src/cost.rs` (modify) | Wire `capacity_policy` field (tiny_string_cap, container_initial_capacity) into cost calculation. | Cost function incorporates memory footprint; high-capacity layouts score higher cost. |
| **Total**: | **500–800 LOC** | 2 crates (`ir/`, `passes/`) | Static analysis or bench harness integration test on 1–2 representative grammars. | `extract_with_cost()` returns 3–5 candidate exprs; all have cost scores; winner is deterministic. |

**Challenge Risk**: Cost measurement (throughput_mbps, cycles_per_byte) requires calibration on actual workloads. Stale or missing evidence can lead to suboptimal extraction. Mitigation: use conservative fallback (e.g., AstSize heuristic) if measurement unavailable.

---

### §2.d CSP Integration for Multi-Objective Resolver

**Precondition**: Follows cost-model wiring. CSP is invoked **after** e-graph extraction to resolve multi-objective constraints (select pattern shape AND substrate AND consumer wiring simultaneously).

**Current State**:
- CSP solver exists at `crates/csp-solver/` in main repo (685 LOC in Cargo.toml description).
- Path-dep comment cites `csc411 commit b70098676f2fc09979f1969341f5115bd774cbd5`.
- **Never imported** by skinny; zero CSP constraints in passes.

**Wave Scope**:
| Item | Envelope | Owner Crate | Entry Gate | Exit Gate |
|------|----------|-------------|-----------|-----------|
| **Path-dependency wiring** | 10–20 LOC | `skinny/crates/passes/Cargo.toml` (modify) | Add `csp-solver = { path = "../../csp-solver" }` (or path to parent-repo crate). | `cargo build -p passes` compiles without errors; `use csp_solver::*` is valid. |
| **CSP problem codegen** | 300–500 LOC | `passes/src/egraph/csp_resolver.rs` (new) | After egraph extraction yields candidate exprs, emit CSP problem: variables = (shape, pattern, consumer_sink), domains = (allowed values), constraints = (parity, cost). | CSP solver receives well-formed problem; returns satisfying assignment or UNSAT. |
| **Constraint templates** | 200–300 LOC | `passes/src/egraph/csp_resolver.rs` | Encode domain knowledge: (1) tape-direct parity (must agree with layout facts); (2) recognizer coverage (alphabet ⊆ present bytes); (3) cost objective (minimize throughput penalty + code size). | Each rule has 3–5 constraints; CSP problem has ~N*5 constraints for N rules. |
| **Extraction → Assignment** | 100–150 LOC | `passes/src/lib.rs` (modify) | After CSP solution, emit updated `cost_facts` and `backend_shape` maps with CSP-chosen assignments. | Lowering stage consumes CSP-chosen shapes; diagnostics log which constraints were active. |
| **Total**: | **610–970 LOC** | 2 crates (`passes/`, `csp-solver` as dep) | Unit test on JSON grammar: CSP enforces parity constraint; UNSAT ⟹ reject combination. | Integration test: 2+ rule grammar with cross-cutting cost/parity objectives; CSP finds consistent solution. |

**CSP Problem Statements** (3–5 concrete):

1. **Pattern-Shape-Substrate Coupling**:
   - Variables: `rule_i_shape ∈ {EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`, `rule_i_recognizer ∈ {Recognizer}`, `rule_i_consumer_sink ∈ {bool}`.
   - Constraint: If `consumer_sink = true`, then `shape ≠ EagerTape` (eager cannot be sink-only).
   - Constraint: If `recognizer = Regex`, then `shape ∈ {OffsetTape, EventTape}` (tape required for regex dispatch).
   - Objective: Minimize Σ(cost_facts[rule_i].cost) subject to constraints.

2. **Parity Maintenance** (tape memory layout ↔ direct-build schema agreement):
   - Variables: `rule_i_tape_kind ∈ {Sequence, Container, KeyValuePair, StringValue, NumberValue}`, `rule_i_direct_schema ∈ {schema_id}` (or None).
   - Constraint: If `tape_kind = Sequence` and `direct_schema = ArraySchema`, then parity = `ok`. Otherwise, parity = `fail`.
   - Objective: Maximize number of rules with parity = `ok`.

3. **Regex NFA-vs-DFA Selection**:
   - Variables: `rule_i_regex_form ∈ {NFA, DFA}`.
   - Constraint: If `state_count > 16`, then `form ≠ DFA` (code size explosion).
   - Constraint: If `input_alphabet_size < 8`, then `form = DFA` (benefit high).
   - Objective: Minimize total code size + runtime dispatch depth.

4. **SIMD Recognizer Coverage**:
   - Variables: `rule_i_recognizer ∈ {Exact, Fuzzy, Scalar}`, `alphabet_i ⊆ StructuralAlphabet`.
   - Constraint: For all rules, `alphabet_i ⊆ union(alphabet_j for all j)` (coverage).
   - Constraint: If `recognizer = Exact` and `distance_within_set > 4`, then infeasible.
   - Objective: Minimize SIMD instruction count while maintaining coverage.

5. **Capacity Policy + Cost Trade-off**:
   - Variables: `rule_i_tiny_string_cap ∈ [0, 256]`, `rule_i_container_capacity ∈ [0, 65535]`.
   - Constraint: `tiny_string_cap + container_capacity ≤ total_stack_budget` (e.g., 4KB).
   - Constraint: If `container_capacity < 256`, then `shape ≠ CollapsedStage` (not enough buffer for materialization).
   - Objective: Maximize throughput given capacity constraints.

**Challenge Risk**: CSP can be NP-hard; worst-case solve time may exceed 1s on large grammars. Mitigation: use timebound (e.g., 200ms) and fall back to greedy heuristic (e-graph cost extraction) if CSP doesn't converge.

---

### §2.e P1–P8 Cascade Replacement

**Precondition**: Follows all prior waves (bbnf-regex, egraph, cost, CSP).

**Current State**:
- Hardcoded cascade at `passes/src/lib.rs:446–506` with literal if/match chains.
- Priority steps (P1–P8) are enum values with no parameterization.
- `choose_backend_shape()` is the sole decision point; called once per rule during `compile()`.

**Wave Scope**:
| Item | Envelope | Owner Crate | Entry Gate | Exit Gate |
|------|----------|-------------|-----------|-----------|
| **Cascade deletion** | 60 LOC (removal) | `passes/src/lib.rs` | Delete `choose_backend_shape()` function (lines 446–506) and `PRIORITY_TABLE`. | No calls to deleted function remain. |
| **Resolver integration** | 100–150 LOC | `passes/src/lib.rs` (modify) | Replace `choose_backend_shape()` call in `compile()` with `saturate_and_extract()` call (new unified resolver). | `compile()` yields same `cost_facts`, `backend_shape` maps; no breaking API change. |
| **Backward-compat gate** | 20–30 LOC | `passes/Cargo.toml` (modify) | Feature-gate new resolver behind `sk-v13-egraph` (default off); allow SK-V12 builds to remain unchanged. | `cargo build --no-default-features` still works; SK-V12 tests pass. |
| **Total**: | **180–240 LOC** | 1 crate (`passes/`) | Verify old `choose_backend_shape()` tests are migrated to new resolver test suite. | New resolver passes all prior test cases; cost_facts match (or improve). |

**Challenge Risk**: If cascading back to P1–P8 for debugging/fallback, old code path must remain feature-gated and well-tested to avoid silent regressions.

---

## §3 E-Graph Language Implementation: Detail

### Language Trait Design

```rust
// In passes/src/egraph/language.rs (or auto-derived via egraph-derive macro)
impl egg::Language for BackendExpr {
    fn matches(&self, other: &Self) -> bool {
        // Structural equality; wraps IR enum patterns
        matches((self, other),
            (BackendExpr::Tape { .. }, BackendExpr::Tape { .. })
            | (BackendExpr::Direct { .. }, BackendExpr::Direct { .. })
            // ... etc
        )
    }
    
    fn apply(&self, args: &[eclass]) -> Self {
        // Reconstruct expr with new eclass references (e.g., swap child tape type)
        match self {
            BackendExpr::OffsetTape(tape) => {
                BackendExpr::OffsetTape(tape.with_children(args))
            }
            // ...
        }
    }
    
    fn children(&self) -> Vec<&eclass> {
        // Return immediate child references for tree traversal
        match self {
            BackendExpr::OffsetTape(tape) => vec![&tape.first_expr],
            // ...
        }
    }
}
```

### Rewrite Rule Structure

Each rewrite is a closure or struct implementing `egg::Rewrite<BackendExpr, ()>`:

```rust
// Example: OffsetTape ↔ EventTape (conditional on branch density)
pub struct OffsetToEventTape;
impl egg::Rewrite<BackendExpr, ()> for OffsetToEventTape {
    fn name(&self) -> &str { "offset_to_event_tape" }
    
    fn search(&self, egraph: &EGraph, eclass: Id) -> Vec<Subst> {
        // Match all OffsetTape nodes; check precondition (branch count > 4)
        egraph.classes[eclass].iter()
            .filter_map(|node| {
                if let BackendExpr::OffsetTape { tape_ref, .. } = node {
                    let branch_count = count_branches_in(tape_ref, egraph);
                    if branch_count > 4 {
                        return Some(Subst::default());
                    }
                }
                None
            })
            .collect()
    }
    
    fn apply(&self, egraph: &mut EGraph, eclass: Id, subst: &Subst) -> Vec<Id> {
        // Build corresponding EventTape node; add to egraph
        let existing = egraph.classes[eclass].iter().next().unwrap().clone();
        if let BackendExpr::OffsetTape { tape_ref, .. } = existing {
            let new_node = BackendExpr::EventTape {
                tape_ref: tape_ref.clone(),
                alt_density: compute_density(tape_ref, egraph),
            };
            vec![egraph.add(new_node)]
        } else {
            vec![]
        }
    }
}
```

### Cost-Driven Extraction

```rust
impl egg::CostModel<BackendExpr, ()> for SkinnyResolver {
    fn cost(&mut self, enode: &BackendExpr, costs: &[f64]) -> f64 {
        match enode {
            BackendExpr::OffsetTape { tape_ref, .. } => {
                let throughput = lookup_cost_facts(*tape_ref).throughput_mbps;
                1000.0 / (throughput as f64 + 1.0)
            }
            BackendExpr::EventTape { alt_density, .. } => {
                // Slightly higher cost if density is unfavorable
                let base = 1000.0 / 400.0;  // EventTape baseline ~400 Mbps
                if *alt_density < 0.5 { base * 1.2 } else { base }
            }
            // ...
            _ => 1.0,
        }
    }
}
```

### Saturation Loop

```rust
pub fn saturate_and_extract(
    grammar: &GrammarIr,
    backend_ir: &BackendIr,
    layout_facts: &LayoutFacts,
) -> (HashMap<RuleId, CostFacts>, HashMap<RuleId, BackendShape>) {
    let mut egraph = EGraph::default();
    let mut rewrites = vec![
        Box::new(OffsetToEventTape) as Box<dyn Rewrite<BackendExpr, ()>>,
        Box::new(EventToOffsetTape),
        Box::new(DirectToTape),
        // ... 10–15 more rewrites
    ];
    
    // Add all backend rules to egraph
    for (rule_id, backend_rule) in backend_ir.rules.iter() {
        let eclass = egraph.add(backend_rule.expr.clone());
        rule_to_eclass.insert(*rule_id, eclass);
    }
    
    // Saturate with bounded iterations
    for _iter in 0..100 {
        let mut any_changed = false;
        for rewrite in &rewrites {
            for eclass in egraph.classes.keys() {
                for subst in rewrite.search(&egraph, eclass) {
                    let new_eclasses = rewrite.apply(&mut egraph, eclass, &subst);
                    if !new_eclasses.is_empty() {
                        any_changed = true;
                    }
                }
            }
        }
        if !any_changed { break; }
    }
    
    // Extract best expr per rule
    let mut cost_model = SkinnyResolver::new(layout_facts);
    let mut extractor = Extractor::new(&egraph, cost_model);
    
    for (rule_id, eclass) in rule_to_eclass {
        let best_expr = extractor.find_best(eclass);
        let cost_facts = reconstruct_cost_facts(*rule_id, &best_expr);
        cost_facts_map.insert(*rule_id, cost_facts);
    }
    
    (cost_facts_map, backend_shapes_map)
}
```

---

## §4 CSP Integration: Problem Formulation

CSP solver is **invoked after e-graph extraction**. E-graph identifies candidate exprs per rule; CSP resolves cross-rule constraints (parity, recognizer coverage, capacity).

### High-Level Problem Template

```
Variables:
  ∀ rule_i ∈ rules:
    shape_i ∈ {EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}
    recognizer_i ∈ Recognizer (or None)
    consumer_sink_i ∈ {true, false}
    capacity_policy_i ∈ Capacity

Domains:
  shape_i: [EagerTape, ..., CollapsedStage]
  recognizer_i: all valid recognizers (including multi-candidate if mined)
  consumer_sink_i: {true, false}
  capacity_policy_i: tiny_string_cap ∈ [0, 256], container_capacity ∈ [0, 65535]

Constraints:
  ∀ rule_i:
    1. (consumer_sink_i = true) ⟹ (shape_i ≠ EagerTape)
    2. (recognizer_i.type = Regex) ⟹ (shape_i ∈ {OffsetTape, EventTape})
    3. (tape_kind_i, direct_schema_i) ⟹ parity_ok_i (boolean constraint)
    4. (shape_i = CollapsedStage) ⟹ (capacity_policy_i.container_capacity ≥ 256)
  
  ∀ rules i, j with shared consumer:
    5. (consumer_sink_i ∧ consumer_sink_j) ⟹ (shape_i = shape_j)  // consistency

Objective:
  Minimize: Σ_i cost(shape_i) + λ · Σ_i code_size(recognizer_i)
  Subject to: parity_ok_i = true for all i  // hard constraint
```

### CSP Problem Generation Algorithm

```rust
pub fn gen_csp_problem(
    grammar: &GrammarIr,
    egraph_candidates: &HashMap<RuleId, Vec<BackendExpr>>,
    layout_facts: &LayoutFacts,
) -> csp_solver::Problem {
    let mut problem = csp_solver::Problem::new();
    
    // Add variables
    for rule_id in grammar.rules.iter().map(|r| r.id) {
        problem.add_variable(
            format!("shape_{}", rule_id.0),
            vec!["EagerTape", "OffsetTape", "EventTape", "SinkOnly", "CollapsedStage"],
        );
        problem.add_variable(
            format!("recognizer_{}", rule_id.0),
            collect_valid_recognizers(&egraph_candidates[&rule_id]),
        );
        problem.add_variable(
            format!("sink_{}", rule_id.0),
            vec!["true", "false"],
        );
        problem.add_variable(
            format!("cap_{}", rule_id.0),
            vec!["128", "256", "512", "1024"],  // discretized capacity values
        );
    }
    
    // Add constraints
    for rule_id in grammar.rules.iter().map(|r| r.id) {
        // Constraint 1: if sink, then no EagerTape
        problem.add_constraint(
            format!("no_eager_sink_{}", rule_id.0),
            vec![
                format!("sink_{}", rule_id.0),
                format!("shape_{}", rule_id.0),
            ],
            |values: &[&str]| {
                !(values[0] == "true" && values[1] == "EagerTape")
            },
        );
        
        // Constraint 2: if regex, then tape
        let backend_rule = /* lookup from egraph_candidates */;
        if backend_rule_uses_regex(backend_rule) {
            problem.add_constraint(
                format!("regex_needs_tape_{}", rule_id.0),
                vec![format!("shape_{}", rule_id.0)],
                |values: &[&str]| {
                    matches!(values[0], "OffsetTape" | "EventTape")
                },
            );
        }
        
        // Constraint 3: parity (tape kind ↔ direct schema)
        let (tape_kind, direct_schema) = (
            layout_facts.materialization[&rule_id].kind,
            /* direct schema lookup */,
        );
        if parity_mismatch(tape_kind, direct_schema) {
            problem.add_constraint(
                format!("parity_{}", rule_id.0),
                vec![],  // hard constraint; no variables
                |_| false,
            );
        }
        
        // ... more constraints per rule
    }
    
    // Add objective
    problem.set_objective(
        csp_solver::Objective::Minimize,
        |assignment: &Assignment| {
            let mut total_cost = 0.0;
            for rule_id in grammar.rules.iter().map(|r| r.id) {
                let shape = assignment.get(&format!("shape_{}", rule_id.0));
                let cost = match shape {
                    "OffsetTape" => 1.0,
                    "EventTape" => 0.8,
                    "SinkOnly" => 0.5,
                    _ => 1.2,
                };
                total_cost += cost;
            }
            total_cost
        },
    );
    
    problem
}
```

---

## §5 bbnf-regex Extraction Wave: Detailed Plan

### Current Embedding (Scatter Analysis)

**parse-that-regex/src/lib.rs** (1214 LOC):
- Regex pattern matching: `StringMode`, `StringMatch`, validation logic.
- Not JSON-specific; general string + number parsing.
- Contains `number::match_number_span()`, `unicode::*` modules (reusable).

**ir/src/lib.rs** (713 LOC):
- `regex_is_nullable()` (lines 327–335): hardcoded pattern match on 8 literal strings.
- `regex_first_bytes()` (lines 782–799): same hardcoding.
- Both functions scattered; not in a cohesive module.

### Extraction Sequence

#### Phase 1: Module Refactor (70 LOC, 2 days)

**Tasks**:
1. Create `skinny/crates/bbnf-regex/Cargo.toml` (new crate).
2. Create `skinny/crates/bbnf-regex/src/lib.rs` with skeleton:
   ```rust
   pub mod hir;
   pub mod analysis;
   
   pub use hir::RegexHir;
   pub use analysis::{nullability_analysis, first_set_analysis};
   ```
3. Move `parse-that-regex/src/lib.rs` content → `bbnf-regex/src/hir.rs`; keep integration/number/unicode internal.
4. Move `ir/src/regex_is_nullable` and `ir/src/regex_first_bytes` → `bbnf-regex/src/analysis.rs`.

**Entry Gate**: Grep for all uses of regex functions; list in task.
**Exit Gate**: `cargo build -p bbnf-regex` compiles; public API is `pub mod hir`, `pub mod analysis`, `pub fn nullability_analysis()`.

#### Phase 2: Public API Surface (50 LOC, 2 days)

**Tasks**:
1. Define `RegexHir` enum (wrapper for pattern analysis results):
   ```rust
   pub enum RegexHir {
       Literal { bytes: Vec<u8> },
       CharClass { chars: BitSet },
       Whitespace,
       Digit,
       AlternationOf(Vec<RegexHir>),
   }
   ```
2. Create `pub fn nullability_analysis(pattern: &str) -> bool` in `analysis.rs`.
3. Create `pub fn first_set_analysis(pattern: &str) -> Vec<u8>` in `analysis.rs`.
4. Create `pub fn is_character_class(pattern: &str, char: u8) -> bool` helper.

**Entry Gate**: Sketch public APIs; ensure backward-compatible with existing call sites.
**Exit Gate**: Three functions are exported; can be imported as `use bbnf_regex::{nullability_analysis, first_set_analysis}`.

#### Phase 3: Re-import Sites (30 LOC, 3 days)

**Tasks** (grep + replace):
1. `ir/src/lib.rs`: Replace calls to local `regex_is_nullable()` with `use bbnf_regex::nullability_analysis`.
2. `codegen/src/`: Any pattern-matching on regex literals → `bbnf_regex::is_character_class()`.
3. `passes/src/recognizers.rs`: If any regex pattern hardcoding → `bbnf_regex::*` calls.

**Entry Gate**: Identify all call sites with grep.
**Exit Gate**: Zero hardcoded pattern strings (like `r"[ \t\n\r]*"`) remain in `ir/`, `codegen/`, `passes/`.

#### Phase 4: WASM Sub-crate (120 LOC, 5 days)

**Tasks**:
1. Create `skinny/crates/wasm-bbnf-regex/Cargo.toml` with `wasm-bindgen` dep.
2. Create `wasm-bbnf-regex/src/lib.rs`:
   ```rust
   use wasm_bindgen::prelude::*;
   use bbnf_regex;
   
   #[wasm_bindgen]
   pub fn is_nullable(pattern: &str) -> bool {
       bbnf_regex::nullability_analysis(pattern)
   }
   
   #[wasm_bindgen]
   pub fn first_bytes(pattern: &str) -> Vec<u8> {
       bbnf_regex::first_set_analysis(pattern)
   }
   ```
3. Add build script for `.wasm` artifact generation; output to `dist/`.

**Entry Gate**: `wasm-pack build` on `wasm-bbnf-regex/` succeeds.
**Exit Gate**: `dist/bbnf_regex_bg.wasm` exists; JS can `import * as bbnf from './bbnf_regex'` and call `bbnf.is_nullable(pattern)`.

### Risk Register for bbnf-regex Extraction

| Risk | Severity | Mitigation |
|------|----------|-----------|
| **Pattern matching scatter**: 20+ call sites with hardcoded strings | HIGH | Grep comprehensively; create checklist of all sites before starting Phase 3. |
| **Regex semantics drift**: Moving functions changes inlining; performance regress | MEDIUM | Benchmark `nullability_analysis()` before/after; use `#[inline]` hints if needed. |
| **WASM interop complexity**: JS binding errors, type mismatches | MEDIUM | Start WASM early (Phase 4); test `first_bytes()` on 5–10 patterns before integrating. |
| **Circular dep (bbnf-regex ↔ parse-that-regex)**: Hidden cyclic import | MEDIUM | Verify DAG: `bbnf-regex` has no dependency on `parse-that-regex`. |
| **Breakage in consumer crates**: ir, codegen, passes import bbnf-regex prematurely | LOW | Feature-gate behind `[features] bbnf-regex-extract` until all phases complete. |

---

## §6 Pluggable-Component Refit: Hardcoded Branches

Per memory `[pluggable-components]`, every decision point must become a plugin. Current hardcoded branches in codegen/passes:

### Hardcoded Decision Points

#### Decision 1: Backend Shape Selection (CRITICAL PATH → Resolver)
**File**: `passes/src/lib.rs:446–506`
**Current**: Hardcoded if/match cascade (P1–P8).
**Refactor**: Delete; use unified resolver (e-graph + CSP).
**Status Post-Fold**: PLUGGABLE via `egg::Rewrite` trait + CSP constraints.

#### Decision 2: Pattern Recognition (Alphabet Curation)
**File**: `passes/src/lib.rs:324–350`
**Current**: Hand-curated JSON alphabet `{}\[\],:\"`.
**Hardcoded Branch**:
```rust
if matches!(byte, b'{' | b'}' | b'[' | b']' | b',' | b':' | b'"') {
    present.insert(byte);
}
```
**Refactor Path**: 
- Introduce `PatternRegistry` trait with `impl PatternRegistry` per domain (JSON, CSV, XML).
- Move hardcoded alphabet to a JSON-specific registry struct.
- Call `registry.candidate_patterns()` at runtime.
- **Target Post-Fold**: SK-V14 (not part of SK-V13 fold; note as future work).

#### Decision 3: Materialization Strategy (Tape Kind Assignment)
**File**: `passes/src/lib.rs:978–1123`
**Current**: Hardcoded role matching (container ↔ object, pair ↔ key-value, etc.).
**Hardcoded Branch**:
```rust
if let Some(rule) = roles.container {
    descriptors.insert(rule, MaterializationDescriptor {
        kind: TapeKind::Container,
        label: "object".to_string(),
        // ...
    });
}
```
**Refactor Path**:
- Introduce `RoleResolver` trait with domain-specific impl.
- Wire roles → tape kinds through pluggable resolver.
- **Target Post-Fold**: SK-V14 (materialization is secondary; cost-driven shape choice is primary).

#### Decision 4: Type Inference Algorithm (Single-Choice Unification)
**File**: `passes/src/lib.rs:112–217`
**Current**: Hardcoded Robinson-style unification (Algorithm W).
**Hardcoded Branch**: Single type-inference strategy; no GADT or DK13 alternative.
**Refactor Path**:
- Define `TypeInferencer` trait with pluggable implementations.
- **Target Post-Fold**: SK-V15+ (type system overhaul; out of scope for SK-V13).

#### Decision 5: Priority Step Firing (P1–P8 Table)
**File**: `passes/src/lib.rs:440–444`
**Current**: `const PRIORITY_TABLE: [PriorityStep; 8] = PriorityStep::ALL;`
**Hardcoded Branch**: Static array; no parameterization.
**Refactor Path**: Delete with cascade (Decision 1).

### Pluggable-Component Refitting Checklist for SK-V13

| Decision Point | File:Lines | SK-V13 Action | Post-SK-V13 Status |
|---|---|---|---|
| Backend shape selection | `passes/src/lib.rs:446–506` | DELETE; use resolver | PLUGGABLE |
| Pattern registry (alphabet) | `passes/src/lib.rs:324–350` | MARK for SK-V14 | TODO: DomainRegistry trait |
| Materialization strategy | `passes/src/lib.rs:978–1123` | MARK for SK-V14 | TODO: RoleResolver trait |
| Type inference | `passes/src/lib.rs:112–217` | NO CHANGE (deferred) | TODO: TypeInferencer trait |
| Priority table | `passes/src/lib.rs:440–444` | DELETE with cascade | RESOLVED |

**Note**: SK-V13 focuses on **shape selection + cost + CSP**. Recognizer registry and materialization strategy are secondary and deferred to SK-V14.

---

## §7 SK-V13 Decision-Engine Wave Shortlist

### Wave Dependency DAG

```
bbnf-regex-extract (W1)
    ↓
egraph-language (W2)
    ↓
cost-model-active (W3)
    ↓
csp-resolver (W4)
    ↓
cascade-delete (W5)

Parallel tracks (independent):
  - WASM sub-crate (W1b, starts with W1, ends when W1 done)
  - Feature-gating (W0, runs before W1)
```

### Concrete Wave Definitions

#### **W0: Feature-Gating Scaffold** (1 day, 30 LOC)
**Owner**: `passes/Cargo.toml`, `skinny/Cargo.toml`  
**Entry Gate**: SK-V13 branch created; `feature-gated-resolver` listed in features.  
**Exit Gate**: `cargo build --features sk-v13-egraph` compiles (scaffold only, no implementation).  
**Depends**: None.  
**Why First**: Allows parallel work on W1–W5 without breaking SK-V12 builds.

#### **W1: bbnf-regex Extraction** (6 days, 210–330 LOC)
**Owner**: `bbnf-regex/` crate (new), `parse-that-regex/`, `ir/src/lib.rs`  
**Entry Gate**: Phase 1 module refactor complete; public API sketched.  
**Exit Gate**: Zero hardcoded pattern strings in IR; `cargo test -p bbnf-regex` passes.  
**Depends**: W0.  
**Parallel**: W1b (WASM sub-crate starts immediately; completes with W1).  
**Why CRITICAL PATH**: Regex patterns feed rewrite guards in W2.

#### **W1b: WASM bbnf-regex** (4 days, 100–150 LOC)
**Owner**: `wasm-bbnf-regex/` crate (new)  
**Entry Gate**: `bbnf-regex/src/lib.rs` public API stable.  
**Exit Gate**: `wasm-pack build` succeeds; `.wasm` artifact in `dist/`.  
**Depends**: W1 (public API) + W0 (feature gate).  
**Why Parallel**: Independent of W2–W5; good for async work.

#### **W2: E-Graph Language + Rewrites** (8 days, 850–1250 LOC)
**Owner**: `passes/src/egraph/language.rs`, `passes/src/egraph/rewrites.rs`  
**Entry Gate**: W1 complete; `bbnf_regex::nullability_analysis` is callable.  
**Exit Gate**: `saturate_and_extract()` function works; 10–15 rewrites tested in isolation.  
**Depends**: W1, W0.  
**Milestones**:
  - Days 1–2: Language trait impl + basic interning.
  - Days 3–5: First 5 rewrites (tape↔direct, shape transformations).
  - Days 6–8: Remaining rewrites + cost integration (placeholder).

#### **W3: Cost Model as CostFunction** (5 days, 500–800 LOC)
**Owner**: `ir/src/cost.rs`, `passes/src/egraph/cost.rs`  
**Entry Gate**: W2 language + rewrites working; egraph produces candidates.  
**Exit Gate**: `extract_with_cost()` picks minimum-cost expr per eclass; test on JSON grammar.  
**Depends**: W2.  
**Milestones**:
  - Day 1: Define `impl egg::CostModel<BackendExpr, ()>`.
  - Days 2–3: Cost function logic (throughput heuristic); evidence backfill.
  - Days 4–5: Integration test; capacity policy wiring.

#### **W4: CSP Integration** (7 days, 610–970 LOC)
**Owner**: `passes/src/egraph/csp_resolver.rs`, `passes/Cargo.toml` (add csp-solver dep)  
**Entry Gate**: W3 complete; cost facts populated per rule.  
**Exit Gate**: CSP solver called; solution returned; parity constraints enforced.  
**Depends**: W3 (cost model wiring).  
**Milestones**:
  - Day 1: Add `csp-solver` path-dep; scaffold problem codegen.
  - Days 2–4: Encode 5 constraint templates.
  - Days 5–6: Extraction → assignment conversion.
  - Day 7: Integration test; fallback on UNSAT.

#### **W5: Cascade Deletion + Resolver Integration** (3 days, 180–240 LOC)
**Owner**: `passes/src/lib.rs` (modify), `passes/src/lib.rs` (delete cascade)  
**Entry Gate**: W4 complete; CSP + cost model + egraph stable.  
**Exit Gate**: `compile()` uses unified resolver; old tests pass; cost_facts match or improve.  
**Depends**: W4.  
**Milestones**:
  - Day 1: Delete `choose_backend_shape()` and `PRIORITY_TABLE`; verify no dangling calls.
  - Day 2: Integrate `saturate_and_extract()` into `compile()`.
  - Day 3: Regression tests; cost_facts validation.

### Critical Path Summary

**Linear Sequence**: W0 → W1 (W1b parallel) → W2 → W3 → W4 → W5  
**Estimated Total**: 6 + 8 + 5 + 7 + 3 = **23 person-days** (assuming single-contributor with no blockers).  
**Parallel Gain**: W1b saves ~2 days if run concurrently.  
**Effective Timeline**: ~21 person-days (17 days wall-clock if W1 and W1b overlap).

---

## §8 Risk Register: CSP+EGraph+Cost Resolver

| Risk | Severity | Mitigation | Abrogate-Before-Patch Criterion |
|---|---|---|---|
| **E-graph saturation explosion** (memory > 10GB on large grammars) | HIGH | Conservative rewrite preconditions; timebound saturation to 100 iterations max. State budget < 1GB enforced. | If single JSON grammar (1000+ rules) causes OOM during W2, abrogate W2–W5 and revert to post-extraction heuristic (simpler than saturation). |
| **CSP solve timeout (> 1s)** | MEDIUM | Implement 200ms timeout; fall back to greedy heuristic (pick lowest-cost expr without constraint solving). | If CSP doesn't converge on 50% of grammars in W4 testing, abrogate W4; use e-graph extraction (W3) only. |
| **Cost measurement stale/missing** | MEDIUM | Use conservative fallback heuristic (e.g., AstSize proxy for throughput). Backfill from static analysis. | If evidence is missing for > 30% of candidate exprs, abrogate W3 cost-model changes; keep passive cost ledger (prior design). |
| **Parity constraint unsatisfiability** | LOW | Ensure constraints are soft (penalty in objective) not hard; allow UNSAT solutions with parity violations logged as warnings. | If CSP solver returns UNSAT on any test grammar, abrogate W4; switch to soft-constraint formulation (increase flexibility). |
| **Rewrite rule interaction (order-dependent)** | MEDIUM | Implement rewrite ordering strategy (e.g., saturation → extraction, not interleaved). Test commutative properties of rewrite set. | If rewrite order significantly affects final extraction cost (> 10% variance), abrogate W2 rule set; reduce to commutative core. |
| **Backward-compat breakage** | MEDIUM | Feature-gate new resolver behind `sk-v13-egraph` (default off). Keep old cascade code path for SK-V12 builds. | If SK-V12 regression tests fail with new resolver enabled, keep W5 cascade deletion deferred; run both codepaths in parallel for validation. |
| **CSP codegen complexity** (constraint templates hard to understand/maintain) | MEDIUM | Document each constraint with plain-English semantics + unit test. Use DSL (e.g., `constraint!()` macro) to reduce boilerplate. | If CSP problem codegen exceeds 1000 LOC and has > 3 bugs in W4 testing, abrogate W4; use simple greedy heuristic (prefer cost-extracted shape). |
| **WASM sub-crate deployment** (bbnf-regex JS interop fails) | LOW | Test WASM artifact on 5–10 patterns in isolation before integrating into pipeline. | If WASM binary fails to load or calls error in W1b, abrogate W1b; run bbnf-regex on Rust side only (no JS exposure). |

### Risk Mitigation Roadmap

**Pre-W1 (Days 0–1)**:
- Design rewrite rule preconditions conservatively (memory budget checks, entropy thresholds).
- Sketch CSP constraint templates; validate on paper (no code).

**During W1–W2 (Days 2–10)**:
- Run saturation on JSON grammar with iteration-count instrumentation; if > 50 iterations needed, tighten rewrite preconditions.
- Test e-graph interning rate (nodes/second); if < 100k/s, likely to hit memory limits; design pruning strategy.

**During W3–W4 (Days 11–17)**:
- Measure CSP solve time on 5–10 grammars; if any > 500ms, implement timeout + fallback.
- Validate cost-function on workload; if cost rank doesn't match measured throughput, revert to AstSize heuristic.

**During W5 (Days 18–21)**:
- Run full regression test suite with new resolver; if any test fails, keep cascade code path feature-gated; do not delete.

---

## §9 Decision-Engine Refactoring Checklist (Scope Validation)

Before committing to SK-V13 waves, validate this checklist:

- [ ] **Prior audit current?** Verify no commits to decision-engine code since `skv12-decision-engine-audit.md` (2026-05-20). ✓ (confirmed: zero changes).
- [ ] **bbnf-regex extraction prerequisite?** Confirm regex patterns feed rewrite guards in W2. ✓ (nullable/first-set used by preconditions).
- [ ] **E-graph library available?** Verify `crates/egraph/` exists in main repo and is importable. ✓ (confirmed: 12 files, 5K+ LOC, Language trait defined).
- [ ] **CSP solver available?** Verify `csc411` path-dep is stable; no breaking changes expected. ✓ (confirmed: Cargo.toml cites commit b70098..., PyO3 feature unused in bbnf-lang).
- [ ] **Cost model extensible?** Confirm `ir/src/cost.rs` can be wrapped in `egg::CostModel` trait without major refactor. ✓ (passive struct; trait impl is straightforward).
- [ ] **Recognizer mining inert for SK-V13?** Confirm no pluggable registry needed in W1–W5; deferred to SK-V14. ✓ (alphabet remains hardcoded; only shape selection moves to resolver).
- [ ] **Feature-gating plan clear?** Sketch SK-V12 compat mode (old cascade behind flag). ✓ (W0 establishes `sk-v13-egraph` feature).
- [ ] **Testing strategy?** Identify baseline test case (JSON grammar) for regression. ✓ (use existing JSON corpus; compare cost_facts output).

**Validation Result**: ✓ READY FOR SK-V13. All prerequisites in place; no blocking unknowns. Scope is tight, risk is well-understood, waves are sequenced correctly.

---

## §10 Implementation Order & Hand-off

### Suggested Owner Assignment

- **W0 (Feature-Gating)**: ML (harness lead). 1 day.
- **W1 (bbnf-regex Extraction)**: IR architect. 6 days. (Parallel: W1b by test engineer, 4 days.)
- **W2 (E-Graph Language + Rewrites)**: Passes lead. 8 days.
- **W3 (Cost Model)**: Cost/measurement engineer. 5 days.
- **W4 (CSP Integration)**: Constraint solver specialist (may be external). 7 days.
- **W5 (Cascade Deletion)**: Passes lead (continuation from W2). 3 days.

### Sign-off Criteria per Wave

- **W0**: `cargo build --features sk-v13-egraph` succeeds (no implementation, only scaffold).
- **W1**: `cargo test -p bbnf-regex` passes; zero `regex_is_nullable` calls in IR/codegen.
- **W1b**: `wasm-pack build wasm-bbnf-regex/` succeeds; `.wasm` loads in Node.js REPL.
- **W2**: `saturate_and_extract()` on JSON grammar compiles; 10–15 rewrites integrated; unit tests for each rewrite.
- **W3**: Cost model extracted for 5+ candidate shapes; cost ranking matches throughput trend (Spearman ρ > 0.8).
- **W4**: CSP problem codegen on JSON grammar; solver returns satisfying assignment in < 500ms.
- **W5**: All prior SK-V12 tests pass; new resolver cost_facts ≥ prior cost_facts (same or better).

---

## Appendix A: File-Level LOC Summary

| Artifact | File | Current LOC | Post-Fold LOC | Δ |
|---|---|---|---|---|
| Cost model | `ir/src/cost.rs` | 135 | 185 | +50 (CostModel trait impl) |
| IR library | `ir/src/lib.rs` | 713 | 700 | -13 (delete regex predicates) |
| Passes main | `passes/src/lib.rs` | 1748 | 1668 | -80 (delete choose_backend_shape) |
| E-graph (new) | `passes/src/egraph.rs` | 0 | 1250 | +1250 (new module) |
| Codegen lower | `codegen/src/lower/mod.rs` | 26 | 26 | 0 (unchanged) |
| bbnf-regex (new) | `bbnf-regex/src/lib.rs` | 0 | 300 | +300 (new crate) |
| WASM (new) | `wasm-bbnf-regex/src/lib.rs` | 0 | 120 | +120 (new crate) |
| **TOTAL SKINNY** | **–** | **2622** | **4248** | **+1626** |

**Note**: Fold introduces ~1.6K LOC of new infrastructure (egraph + CSP) while deleting ~80 LOC of hardcoded cascade. Net positive because new code is generic, pluggable, and enables W3+ (cost-driven, constraint-based) optimization unavailable in hardcoded path.

---

## Appendix B: Rewrite Rule Catalog (Detailed)

### Rewrite 1: OffsetTape ↔ EventTape (Density-Based)
**Condition**: Branch count > 4 AND entropy(input) < 0.8 (for forward); vice versa for backward.
**Forward**: `OffsetTape(branches=[...n...]) → EventTape(density=compute_entropy(...))`
**Backward**: `EventTape(density < 0.3) → OffsetTape(...)`
**Cost**: EventTape preferred if dispatch-heavy; OffsetTape if dense branches.

### Rewrite 2: EagerTape ↔ OffsetTape (Recovery Annotation)
**Condition**: `has_recovery_annotation(expr) = true` (forward); false (backward).
**Forward**: `OffsetTape(...) → EagerTape(...)`
**Backward**: `EagerTape(...) → OffsetTape(...)` (if no recovery needed).
**Cost**: EagerTape slightly higher cost (slower); use only if recovery required.

### Rewrite 3: DirectBuild ↔ Tape (Consumer Presence)
**Condition**: `has_direct_consumer(rule) = true` (forward); zero consumers (backward).
**Forward**: `Tape(...) → DirectBuild(schema=...)`
**Backward**: `DirectBuild(...) → Tape(...)`
**Cost**: DirectBuild preferred if consumer is hot; Tape default.

### Rewrite 4: SinkOnly ↔ OffsetTape (Multi-Consumer)
**Condition**: `consumer_count = 1` (forward); > 1 (backward).
**Forward**: `OffsetTape(...) → SinkOnly(...)`
**Backward**: `SinkOnly(...) → OffsetTape(...)`
**Cost**: SinkOnly cheaper (no tape); OffsetTape if multiple consumers need same tape.

### Rewrite 5: CollapsedStage ↔ OffsetTape (Author Declaration)
**Condition**: `author_declared_collapsed_stage = true` (forward); false (backward).
**Forward**: `OffsetTape(...) → CollapsedStage(wrapper=...)`
**Backward**: `CollapsedStage(...) → OffsetTape(...)`
**Cost**: CollapsedStage preferred if assembly wrapper available; else OffsetTape.

### Rewrites 6–10: Regex Form Selection (NFA ↔ DFA, Scalar ↔ SIMD)
**Rewrite 6**: `Regex::NFA → Regex::DFA` (if state_count < 16, entropy high).
**Rewrite 7**: `Regex::DFA → Regex::NFA` (if code_size > 4KB).
**Rewrite 8**: `Recognizer::SIMD-Exact → Recognizer::SIMD-Fuzzy` (if char_distance > 4).
**Rewrite 9**: `Recognizer::SIMD-Fuzzy → Recognizer::Scalar` (if SIMD register pressure high).
**Rewrite 10**: `Recognizer::Scalar → Recognizer::SIMD-Exact` (if alphabet small, throughput critical).

### Rewrites 11–15: Structural Projection & Fusion
**Rewrite 11**: **Collapse Redundant Tape Chains** (e.g., Tape(Tape(x)) → Tape(x)).
**Rewrite 12**: **Container-Pair Nesting** (flatten nested key-value pairs into direct struct).
**Rewrite 13**: **Pattern Merging** (combine similar literal-match recognizers).
**Rewrite 14**: **Tape Reordering** (reorder child expressions if commutative, to minimize dispatch depth).
**Rewrite 15**: **Cost-Annotated Projection** (push cost facts down tree to inform child selections).

---

**Document Complete**: SK-V13 scoping ready for wave execution. Critical path: W0 → W1 (W1b) → W2 → W3 → W4 → W5 (~23 person-days, 17 wall-clock days).
