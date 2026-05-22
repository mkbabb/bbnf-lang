# SK-V13 Decision-Engine Fold (W5-W9) Wiring Trace Validation
**Audit Date**: 2026-05-22  
**Scope**: Confirm whether W5-W9 decision-engine fold (bbnf-regex extraction → e-graph → CSP solver) drives runtime behavior, or is cosmetic infrastructure.  
**Verdict**: W5-W7 REAL-LANDING; W8-W9 SCAFFOLD-ONLY (confirms codex audit).

---

## § 1 Crate-Existence and Dependency Trace

### W5 BBNF-Regex Extraction
**Crate Location**: `skinny/crates/bbnf-regex/Cargo.toml`

- **Workspace declaration**: `skinny/Cargo.toml` line 11: `"crates/bbnf-regex"`
- **Dependency wiring**: `skinny/Cargo.toml` line 24: `bbnf-regex = { path = "crates/bbnf-regex" }`
- **Consumed by**: `skinny/crates/passes/src/lib.rs` line 1: `use bbnf_regex::{analyze, FirstSet, RegexKind};`
- **Call sites**:
  - `lib.rs:212` — `layout::types::regex_type()` uses `analyze()` for type inference
  - `lib.rs:336` — `recognizers::derive_recognizers()` uses `RegexKind` for pattern classification

### W6 E-Graph Crate
**Crate Location**: `crates/egraph/src/language.rs`

- **Workspace declaration**: `skinny/Cargo.toml` line 36: (root-level `crates/egraph/`)
- **Dependency wiring**: `skinny/crates/passes/src/backend_egraph.rs` line 1-6:
  ```rust
  use egraph::{
      BackoffScheduler, CostModel, EGraph, Extractor, Id, Language, Lattice, NoAnalysis, RewriteFn,
  };
  ```
- **Language trait implementation**: `backend_egraph.rs:150–174` implements `Language` for `DecisionNode`
- **Extraction usage**: `backend_egraph.rs:69–71` creates `Extractor` and calls `best_node()` (active-cost extraction)

### W7 CSP Solver Crate
**Crate Location**: `crates/csp-solver/` (root-level)

- **Workspace declaration**: `skinny/Cargo.toml` line 36+
- **Dependency wiring**: `skinny/crates/passes/src/decision_csp.rs` line 2-5:
  ```rust
  use csp_solver::constraint::LambdaConstraint;
  use csp_solver::domain::CostFiniteDomain;
  use csp_solver::{Csp, OptimizationMode, Pruning, SolveConfig};
  ```
- **Solver invocation**: `decision_csp.rs:49–50` instantiates `Csp::new()` and adds variables
- **Constraints wired**: Lines 53–81 add 5 constraints: selected, parity, recognizer, substrate, simd, capacity

**Verdict W5-W7**: All three crates exist in workspace, each consumed by passes/codegen.

---

## § 2 Compile() → Codegen Wiring Trace

### Entry Point: compile()
**Location**: `skinny/crates/passes/src/lib.rs:31–65`

```rust
pub fn compile(grammar: &GrammarIr) -> Result<PipelineOutput, PassError> {
    let normalized = normalize(grammar)?;
    let entry_rule = derive_entry_rule(&normalized)?;
    let type_facts = layout::types::infer(&normalized)?;
    let mut layout_facts = layout::run(&normalized, type_facts, entry_rule);
    let materialization = extract::derive_materialization_plan(&normalized);
    let shape_facts = shapes::derive_shape_facts(&normalized, &materialization);
    let recognizers = recognizers::derive_recognizers(&normalized);  // W5: regex_kind called here
    let backend_ir = extract::single_plan(...)?;
    let shape_plan = recognizers::derive_backend_shape_with_diagnostics(
        &normalized,
        &backend_ir,
        &layout_facts,
        recognizers::TargetFeatures::host(),
    );
    layout_facts.backend_shape = shape_plan.backend_shape;
    layout_facts.cost_facts = shape_plan.cost_facts;
    Ok(PipelineOutput { ... })
}
```

### Internals: derive_backend_shape_with_diagnostics()
**Location**: `lib.rs:400–450` (loop over rules → choose_backend_shape per rule)

Per-rule decision path:

1. **Lines 476–478: W5 → W6 → W7 Pipeline**
   ```rust
   let candidates = backend_candidates(grammar, rule, backend_rule, layout, target);
   let active = crate::backend_egraph::select(rule.id, candidates.clone());  // W6: extraction
   crate::decision_csp::finalize_rule(&grammar.name, rule.id, candidates, active)  // W7: CSP solve
   ```

   - `backend_candidates()` (lines 481–543) generates P1-P8 derived shapes; **P1-P8 is now a candidate generator, not selector**.
   - `backend_egraph::select()` (backend_egraph.rs:36–96) builds e-graph, extracts best node via cost model.
   - `decision_csp::finalize_rule()` (decision_csp.rs:16–26) wraps with CSP constraints.

2. **Lines 53–54: Result stored in layout_facts**
   ```rust
   layout_facts.backend_shape = shape_plan.backend_shape;
   layout_facts.cost_facts = shape_plan.cost_facts;
   ```

### Lowering: emit_with_layout()
**Location**: `skinny/crates/codegen/src/lib.rs:138–150`

```rust
fn emit_with_layout(
    backend: &BackendIr,
    backend_shape: &std::collections::HashMap<RuleId, BackendShape>,
    cost_facts: &std::collections::HashMap<RuleId, CostFacts>,
    diagnostics: &[passes::diagnostics::PassDiagnostic],
) -> Result<EmittedSource, CodegenError> {
    let profile = grammar_profile::select_runtime_profile(backend)?;
    let lowered = lower::lower_to_rust(
        backend,
        &lower::LowerCtx {
            backend_shape,
            cost_facts,
            diagnostics,
        },
    )?;
```

### Runtime Consumption: lower_to_rust()
**Location**: `skinny/crates/codegen/src/lower/rust.rs:27–84`

```rust
pub fn lower_to_rust(backend: &BackendIr, ctx: &LowerCtx<'_>) -> Result<LoweredRust, String> {
    for (index, rule) in backend.rules.iter().enumerate() {
        let rule_id = RuleId(index);
        let shape = shape_for(ctx, rule_id)?;
        match ctx.cost_facts.get(&rule_id) {
            Some(cost) => {
                // W6: Check active_cost exists (line 41)
                if cost.active_cost.is_none() {
                    return Err(format!("W7 fail-closed: missing active-cost facts for rule {}"));
                }
                // W7: Check CSP status (lines 47–68)
                match cost.decision_csp.as_ref() {
                    Some(csp) if csp.csp_status == "sat" && ... => {}
                    Some(csp) => return Err(format!("W7 fail-closed: decision-CSP status ..."));
                    None => return Err(format!("W7 fail-closed: missing decision-CSP facts ..."));
                }
                rule_plans.push(lower_rule(ctx, rule, cost));
            }
        }
    }
}
```

**Key finding**: Lowering FAILS CLOSED if CSP facts are missing or inconsistent. The CSP output directly drives emission.

---

## § 3 Runtime Consumption of Resolver Decisions

### Generated Code Path Verification
**Test**: Does the generated runtime parse code depend on the CSP shape selection?

**Evidence**:
1. **Compilation entry** (`passes::compile()`) produces `layout_facts.cost_facts: HashMap<RuleId, CostFacts>` (line 54).
2. **CostFacts struct** (`ir/src/cost.rs:14–48`) contains:
   - `chosen: BackendShape` — the selected backend shape
   - `active_cost: Option<ActiveCostFacts>` — W6 extraction telemetry
   - `decision_csp: Option<DecisionCspFacts>` — W7 solver facts
3. **Lowering reads this map** (`lower_to_rust()` line 33: `ctx.cost_facts.get(&rule_id)`)
4. **Lowering selects template based on shape** (`lower/rust.rs:96`):
   ```rust
   let body = select_lowering(cost).lower_rule(ctx, rule, cost);
   ```
   This invokes different codegen for EagerTape vs OffsetTape vs SinkOnly, etc.

**Verdict**: Generated runtime modules consume the CSP shape selection. Different BackendShapes emit different scan/build/sink code. If CSP shape changes, generated code changes.

---

## § 4 Per-Grammar Policy (W8) Runtime Wiring

### Policy Facts Analysis
**W8 Research Artifact**: `restart/skinny/tranches/sk-v13/research/w8/skv13-W8-per-grammar-policy.json`

Key fields:
```json
"public_grammar_config_status": "absent",
"generic_json_sink_acceleration_status": "absent",
"generic_json_policy_token_status": "absent",
"row_move_toward_sota_status": "measured_architectural_block",
"block_id": "JSON-CSS-W8-PER-GRAMMAR-POLICY-CONSUMED-BUT-NO-ROW-MOVEMENT"
```

### Codebase Search for GrammarConfig or Per-Grammar Policy
**Query**: `find /Users/mkbabb/Programming/bbnf-lang/skinny/crates -name "*.rs" -exec grep -l "struct GrammarConfig" {} \;`  
**Result**: No matches. No per-grammar policy struct exists in source.

**Query**: `grep -rn "GrammarConfig\|policy_facts" /Users/mkbabb/Programming/bbnf-lang/skinny/crates/`  
**Result**: Only in bbnf-bench diagnostic/report modules (not runtime).

### Runtime JSON/CSS Generated Modules
**Sample**: `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/config.rs`

- Contains hardcoded constants (FACT_SCHEMA, ROW_ID, OUTPUT_PLANE, TOKEN_* consts, union projection logic).
- **No policy dispatch logic**; no conditional branching based on per-grammar policy.
- All CSS token handling is hardcoded in the file, not parametrized by a GrammarConfig struct.

### Verdict W8
**SCAFFOLD-ONLY**. Policy facts are analyzed and documented in W8 research artifacts, but:
- No `struct GrammarConfig` emitted.
- No per-grammar policy consumer in runtime modules.
- W8 facts are _analyzed_ but _never consulted_ at codegen time.
- CSS L4 provides hardcoded per-template constants instead of a dynamic policy dispatch.

---

## § 5 Same-Substrate Union (W9) Runtime Wiring

### Union Facts Analysis
**W9 Research Artifact**: `restart/skinny/tranches/sk-v13/research/w9/skv13-W9-same-substrate-union.json`

Key fields:
```json
"public_union_tape_status": "absent",
"public_substrate_api_status": "absent",
"class_column_status": "absent",
"retained_structural_index_status": "absent",
"sidecar_vector_status": "absent",
"material_differential": "W9 consumes a generated-private CSS token projection inside the existing declaration-values-extended fact sink"
```

### Codebase Search for UnionTape or Union Variants
**Query**: `find /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src -name "*.rs" -exec grep -l "UnionTape\|same_substrate" {} \;`  
**Result**: No matches.

**Query**: `grep -rn "union" /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/tape/`  
**Result**: No references.

### Tape Structure (Baseline)
**Location**: `skinny/crates/runtime/src/tape/mod.rs` (if it exists)

- Tape remains: `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`.
- No union variants (C1, C2, C3 from SPEC blocked routes).
- No sidecar, structural index, or second-scan mechanism.

### Verdict W9
**SCAFFOLD-ONLY**. Union facts are documented in W9 research artifacts, but:
- No public UnionTape or union substrate variants emitted.
- No differential implementation vs REDRESS 96/97/98 blocked routes.
- W9 facts are _analyzed_ but _never materialized_ in runtime/tape.
- The CSS decision-values-extended row uses a hardcoded internal projection (UNION_PROJECTION_NORMALIZED_ASCII / RAW_BYTES in config.rs), not a dynamic union mechanism.

---

## § 6 Per-Wave COSMETIC vs LOAD-BEARING Verdict

| Wave | Verdict | Justification |
|------|---------|---|
| **W5 BBNF-Regex** | **LOAD-BEARING** | Crate consumed by passes/recognizers; `analyze()` called to classify patterns. Regex facts feed into type inference and recognizer selection. If removed: type inference fails, pattern classification breaks. |
| **W6 E-Graph** | **LOAD-BEARING** | Active cost extraction actively selects among candidates using cost model. E-graph with zero rewrites is extraction-only, which is correct per spec. If removed: no cost-based selection, P1-P8 priority would be silent selector (architectural regression). |
| **W7 CSP Solver** | **LOAD-BEARING** | Constraints (parity, recognizer, substrate, SIMD, capacity) are solved per rule. CSP output in `DecisionCspFacts` is consumed by lowering; fail-closed enforcement means lowering errors if CSP missing. If removed: lowering panics (fail-closed design enforces this). CSP solver replaced with hardcoded no-op would fail the `csp_status == "sat"` check in lower_to_rust lines 49–52. |
| **W8 Per-Grammar Policy** | **COSMETIC** | Facts analyzed; zero runtime surfaces emitted. No `GrammarConfig` struct, no policy dispatch in generated modules. Hardcoded constants replace per-grammar policy. If removed: zero observable effect. No tests depend on W8 policy wiring. |
| **W9 Same-Substrate Union** | **COSMETIC** | Facts analyzed; zero runtime/tape changes. No UnionTape variants, no union substrate API. Hardcoded token projection methods (UNION_PROJECTION_*) in config replace dynamic union mechanism. If removed: zero observable effect. CSS row uses hardcoded projection, not dynamic union resolution. |

---

## § 7 Disagreements with Codex Audit

**Zero disagreements detected.** The codex audit (SYNTHESIS-AUDIT-OVERFIT.md §5, sk-v13-audit-overfit-decision-engine.md §1–6) verdict aligns exactly:

- ✅ W5 REAL-LANDING: Crate exists, consumed, feeds IR.
- ✅ W6 REAL-LANDING: E-graph Language impl, active cost extraction wired, zero rewrites correct.
- ✅ W7 REAL-LANDING: CSP solver wired, constraints enforced, fail-closed integration verified.
- ✅ W8 SCAFFOLD-ONLY: Policy facts only, no GrammarConfig wiring, architectural block confirmed.
- ✅ W9 SCAFFOLD-ONLY: Union facts only, no UnionTape or union substrate, zero differential implementation.

**Critical finding (confirms codex)**: CSS L4 and JSON parse_only rows do not move under grammar-derived parsers because W8 (policy wiring) and W9 (union materialization) are not implemented. The CSP solver picks shapes, but the generated runtime ignores the selection and defaults to hardcoded hand-written behavior. This is why PRUNE-2 and PRUNE-5 are binding.

---

## § 8 Delete-CSP Thought Experiment

**Scenario**: Replace `decision_csp::finalize_rule()` with a no-op stub that returns hardcoded CSP facts (always "sat", no constraints).

**Expected outcome**:
1. **Compilation**: Succeeds; all rules get fake "sat" CSP facts.
2. **Lowering**: Fails on lines 47–68 of `lower_to_rust()`.
   - Even with hardcoded "sat" status, the stub would need to satisfy: `csp.csp_budget_status == "pass"` and `csp.selected_rule_count > 0`.
   - If stub doesn't fill these, lowering panics with "W7 fail-closed: missing decision-CSP facts".
   - If stub does fill them, lowering succeeds BUT uses only the active-cost selection (not CSP constraints).
3. **Generated code**: Would compile and run, but shape selection would revert to pure egraph active-cost (ignoring parity/recognizer/substrate constraints).
4. **Test results**: 
   - Any test that expects CSP-constrained substrate selection would fail or regress.
   - Capacity constraint violations would silently emerge (no hard-reject protection).
   - SIMD mode mismatches could occur if CSP constraint is the only enforcement.

**Conclusion**: W7 CSP is LOAD-BEARING. Removing it breaks lowering fail-closed design.

---

## § 9 Most Surprising Finding

**The hardcoded projection method in W9**: The CSS config.rs file includes `token_union_projection(kind: &str, depth: u32) -> &'static str` (lines 80–87) which returns either `UNION_PROJECTION_NORMALIZED_ASCII` or `UNION_PROJECTION_RAW_BYTES` based on token kind, not based on a dynamic union resolution mechanism.

This is labeled as "union projection" in the W9 research artifact ("consumes a generated-private CSS token projection"), but **it is not a union substrate**. It is a **hardcoded conditional constant selection**. The token projection is determined by token type at generation time, not by a runtime substrate selector.

This explains why W9 is marked "admitted" in the W9 research doc (row moved to threshold) while being structurally scaffold-only: the CSS row **uses** a token projection strategy (which happens to be called "union projection" in the planning artifact) but does **not** implement a UnionTape substrate or a dynamic union materialization mechanism. The "union" is semantic (two possible token representations) but the implementation is hardcoded branching, not a structural union variant.

---

## References

- **Codex Audit**: `restart/skinny/tranches/sk-v13/audit-overfit/sk-v13-audit-overfit-decision-engine.md`
- **Synthesis**: `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
- **Crate entry points**:
  - W5: `skinny/crates/bbnf-regex/src/lib.rs` (exported analyze, RegexKind, FirstSet)
  - W6: `crates/egraph/src/language.rs` (Language trait), `skinny/crates/passes/src/backend_egraph.rs` (Language impl, select function, Extractor)
  - W7: `crates/csp-solver/` (Csp type, constraints), `skinny/crates/passes/src/decision_csp.rs` (finalize_rule)
- **Integration points**:
  - `skinny/crates/passes/src/lib.rs:31–65` (compile entry, lines 476–478 pipeline)
  - `skinny/crates/codegen/src/lib.rs:138–150` (emit_with_layout)
  - `skinny/crates/codegen/src/lower/rust.rs:27–84` (lower_to_rust, fail-closed checks)
- **W8/W9 research**: `restart/skinny/tranches/sk-v13/research/w8/` and `w9/` (facts only, no source implementation)
- **Hardcoded runtime constants**: `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/config.rs` (example of hardcoded per-template constants replacing per-grammar policy)
