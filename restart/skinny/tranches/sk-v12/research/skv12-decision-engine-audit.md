# SK-V12 Decision Engine Audit

**Date**: May 2026  
**Scope**: Audit of CSP solver, e-graph layer, cost model, and pluggable-component scaffolding in skinny codebase  
**Focus**: Today's wiring for Lock-14 generalization toward SK-V13

---

## §1 Component Inventory

| Component | Location | Live? | Consumed By | Last-Touched |
|---|---|---|---|---|
| **Cost Model** | `skinny/crates/ir/src/cost.rs` | LIVE | `passes/src/lib.rs` (recognizers) | 135 LOC; defines `CostFacts`, `PriorityStep::ALL` (8 priority levels P1-P8) |
| **CSP Solver** | `crates/csp-solver/Cargo.toml` (main repo) | STUBBED | None in skinny | Comment: "csc411 commit b70098676f2fc09979f1969341f5115bd774cbd5"; PyO3 binding unused |
| **E-Graph / egg** | `crates/egraph/Cargo.toml` (main repo) | ABSENT | None in skinny | Grep: zero `egg`, `EGraph`, `Rewrite` tokens in skinny/ |
| **Rewrite Engine** | N/A | ABSENT | N/A | No rewrites in skinny; canonical plan only per COMPILER.md §5.3 |
| **Regex Analyzer** | `skinny/crates/parse-that-regex/src/lib.rs` | LIVE | Consumed by codegen via `SpanKind` inference | 8 bespoke regex pattern checks: `[ \t\n\r]*`, `"..."`, `-?(0\|...)`, etc. in `ir/src/lib.rs` lines 208-216 |
| **Pluggable Pattern Registry** | `skinny/crates/codegen/src/lower/mod.rs` | LIVE / HARDCODED | `select_lowering(cost)` at line 17–25 | `match cost.chosen` over 5 `BackendShape` variants; decision hardcoded via enum dispatch |
| **Recognizer Mining** | Hand-curated at `passes::recognizers::derive_recognizers()` | LIVE / CURATED | `passes::extract::single_plan()` | Structural alphabet `{}\[\],:\"` hardcoded in `lib.rs:340` |
| **Shape Facts** | `passes::shapes::derive_shape_facts()` | LIVE | `extract::single_plan()` | Derived from materialization roles; no schema miner in skinny |
| **Backend Shape Resolver** | `passes::recognizers::choose_backend_shape()` | LIVE | Emits `BackendShape` enum choice | 449–509 lines; **P1-P8 priority table hardcoded** with literal if/match chains |

**§1 Summary**: 3 ABSENT, 1 STUBBED, 4 LIVE-HARDCODED, 1 LIVE-PLUGGABLE (cost model itself is passive data; consumption is hardcoded).

---

## §2 Cost Model Wiring

**File**: `ir/src/cost.rs` (135 lines)

**Exposed Types**:
```rust
pub struct CostFacts {
    pub rule_id: RuleId,
    pub chosen: BackendShape,
    pub rationale: ShapeRationale,
    pub rejected: Vec<RejectedAlternative>,
    pub priority_fired: PriorityStep,
    pub capacity_policy: Option<CapacityPolicy>,
}

pub enum PriorityStep {
    P1EagerForced,
    P2SinkOnlyConsumer,
    P3CollapsedStage,
    P4EventTapeAltDensity,
    P5OffsetTapeDispatchable,
    P6OffsetTapeSpeculative,
    P7OffsetTapeDefault,
    P8EagerFallback,
}
```

**Consumed By**:
1. **`passes/src/lib.rs:402–509`**: `choose_backend_shape()` reads `target` and grammar shape; **does NOT read cost model**. Instead, it **produces** a `CostFacts` struct and fires a priority step based on hardcoded preconditions (eager requirement, sink-only eligibility, collapsed-stage viability, event-tape density heuristic).
2. **`codegen/src/lower/mod.rs:17–25`**: `select_lowering(cost: &CostFacts)` pattern-matches `cost.chosen` to dispatch to one of five `ShapeLowering` trait implementations.

**Does codegen pattern selection call cost.rs symbols?** No. Cost model is **read-only metadata** passed to lowerer; lowerer consumes `cost.chosen` field only. No cost evaluation, no heuristic function call.

**Verdict**: Cost model is a **passive ledger**, not an active optimizer. The priority-firing logic is **hardcoded** in passes, not delegated to cost solver.

---

## §3 CSP Integration Status

**Cargo.toml Grep**: No `csc411` or `csp_solver` path-dependency in any `skinny/crates/*/Cargo.toml`.

**Main Repo Note**: `crates/csp-solver/Cargo.toml` exists and comments:
> "Generalized CSP/COP substrate; bench home of bbnf-lang. Source-of-truth tracked at csc411 commit b70098676f2fc09979f1969341f5115bd774cbd5"

**Skinny Status**: ZERO CSP calls. The narrow-choice decision points (eager requirement, sink-only admission, collapsed-stage viability, event-tape density) are **hardcoded predicates** in `passes::recognizers::choose_backend_shape()` (lines 449–509).

**Memory `[csp-solver-crate]` Check**: Stale. CSP exists in the parent repo but is not imported by skinny. The skinny ships **Option 1: Hardcoded decision rules** not Option 2: CSP-driven choice selection.

---

## §4 E-Graph Status

**Grep Results**: Zero `egg`, `EGraph`, `Language`, `Rewrite` in `skinny/crates/`.

**E-Graph in Main Repo**: `crates/egraph/` and `crates/egraph-derive/` exist in the parent bbnf-lang repo, but skinny does not depend on them.

**Rewrite Passes in Skinny**: Absent. COMPILER.md §5.3 states: "No rewrites in the skinny — pick canonical plan." Canonical = OffsetTape default with hardcoded fallbacks.

**Per Memory `[derive-language-macro]`**: If e-graph were to be integrated, it would belong in a new module alongside `passes::extract` (currently line 965–1479). The `Language` trait would wrap `BackendExpr` and define rewrite rules over tape shapes.

**Missing File/Line**: `skinny/crates/passes/src/egraph.rs` would be the entry point for future e-graph folding.

---

## §5 bbnf-regex Crate Status

**Directory Search**: No `bbnf-regex` crate found in skinny or parent bbnf-lang.

**Per Memory `[regex-generalized]`**: Expected location is `crates/bbnf-regex/` (separate crate). **Status: NOT YET EXTRACTED.**

**De-Facto Regex Surface**: `skinny/crates/parse-that-regex/src/lib.rs` — a bespoke regex HIR for JSON-specific string/number/whitespace patterns. Two functions in `ir/src/lib.rs`:
- `regex_is_nullable(pattern: &str)` (327–335)
- `regex_first_bytes(pattern: &str)` (782–799)

Both hardcoded pattern match on literal strings (`r"[ \t\n\r]*"`, `r#""...""#`, etc.). No generic regex analysis crate yet.

**Verdict**: Regex analysis is **embedded** in IR validation and passes; extraction to `bbnf-regex` is pending and would unblock Lock-14 regex-shape-driven rewrites.

---

## §6 Pluggable-Component Readiness

**Per Memory `[pluggable-components]`**: Decision points must be pluggable (cost model, pattern registry, rewrite rules), not hardcoded branches.

| Decision Point | File:Lines | Grade | Evidence |
|---|---|---|---|
| **Backend shape selection** | `passes/src/lib.rs:449–509` | HARDCODED-BRANCH | `if requires_eager_tape(...) => EagerTape`; `else if admits_sink_only(...) => SinkOnly`; cascade of literal checks. |
| **Pattern selection for lowering** | `codegen/src/lower/mod.rs:17–25` | PLUGGABLE | `select_lowering(cost)` uses `ShapeLowering` trait; each shape is a struct implementing trait. Adding new shapes requires only new trait impl. |
| **Recognizer nomination** | `passes/src/lib.rs:324–353` | HARDCODED-CURATED | `derive_recognizers()` scans for literal bytes `{}\[\],:\"` and returns a single hardcoded `SimdScan`. No registry; JSON-specific hand-curated fixture. |
| **Materialization strategy** | `passes::extract::derive_materialization_plan()` (lines 981–1123) | HARDCODED-PATTERN-MATCH | Rolls through rule name/literal/regex pattern matching to assign `TapeKind` roles. No pluggable role resolver. |
| **Type inference algorithm** | `passes/src/lib.rs:112–217` (nested `types` module) | HARDCODED | Algorithm-W unification + Robinson-style solver. No DK13, no GADT; V1 extension point marked but skinny has single choice. |
| **Priority-step priority table** | `passes/src/lib.rs:443–447` | HARDCODED | `const PRIORITY_TABLE: [PriorityStep; 8] = PriorityStep::ALL;` — no parameterization. |

**Overall Pluggability Grade**: FAIR.
- ✓ Lowering trait (`ShapeLowering`) is pluggable.
- ✗ Everything that feeds the lowering selector is hardcoded (shape choice, priority step logic, recognition nomination).

---

## §7 SK-V12 W2 Implication: Decision Engine Contact Points

**W2 Intervention**: Selected-baseline materialization (tape vs. direct) selection per rule, cost-driven evidence evaluation.

**Contact Points for Decision Engine**:

1. **Backend shape choice** (currently hardcoded P1–P8 cascade):
   - **W2 Entry**: Must extend `choose_backend_shape()` to include `SinkOnly` and `CollapsedStage` in cost frontier.
   - **Push**: Requires cost evaluation at each priority step. Today: hardcoded predicate. W2: cost-driven selection with evidence.

2. **Recognizer mining** (currently curated):
   - **W2 Entry**: Nominate multiple candidate recognizers per grammar.
   - **Push**: Requires recognizer CSP or e-graph exploration (cost per recognizer candidate).

3. **Materialization strategy** (currently fixed tape-direct):
   - **W2 Entry**: Choose between tape-only, direct-build-only, tape+direct per rule.
   - **Push**: Requires extraction CSP or rewrite rules to transform single BIR into frontier.

4. **Type inference / schema narrowing**:
   - **W2 Entry**: Narrow inferred type at call sites or after direct-build materialization.
   - **Push**: Requires GADT or Pierce-Turner check direction. Skinny HM does not support.

5. **Cost facts evidence collection**:
   - **W2 Entry**: Backfill measurement evidence for rejected alternatives.
   - **Today**: `redress_72_evidence()` hardcodes REDRESS-72 measurement stubs.
   - **W2**: Must invoke real bench harness or static analysis.

---

## Recommendation for SK-V13+ (SOTA-Axis)

**Fold or Stay Scalar?**

**RECOMMENDATION: Fold CSP+E-Graph+Cost into One Resolver**.

**Rationale**:
1. **W2 materializes choices**: tape vs. direct, SinkOnly vs. OffsetTape vs. EventTape per rule. Explodes choice space.
2. **Cost model is passive**: Decisions hardcoded in passes today; W2 must make cost-driven tradeoffs.
3. **E-graph is designed for this**: Rewrites naturally express equivalences (OffsetTape ↔ EventTape, Tape ↔ DirectBuild). Folding into one e-graph lets saturation+cost picking pick Pareto frontier.
4. **Pluggability at risk**: If cost model stays passive and decisions stay hardcoded, W2 will add more hardcoded branches (P9, P10, ...). E-graph + cost solver is the clean architectural exit.

**Action for SK-V13**:
- Integrate `egg` (e-graph) into passes.
- Wrap `BackendExpr` in `egg::Language` trait.
- Emit rewrite rules for shape transformations (e.g., `OffsetTape → EventTape` conditional on branch count).
- Connect CSP solver from `csc411` as cost function over e-graph saturation.
- Delete `choose_backend_shape()` hardcoded cascade; use e-graph+cost extraction.

**Lock-14 Impact**: Recognizer mining and bbnf-regex extraction are prerequisite for this pivot. Regex shape analysis must feed egraph pattern conditions; recognizer enumeration must generate rewrite rule premises.

---

## Appendix: File-Line Summary

| Symbol | File:Lines | Status |
|---|---|---|
| `CostFacts` | `ir/src/cost.rs:4–13` | Passive ledger struct |
| `PriorityStep::ALL` | `ir/src/cost.rs:68–79` | 8-element enum; static |
| `choose_backend_shape()` | `passes/src/lib.rs:449–509` | Hardcoded P1–P8 priority cascade |
| `select_lowering()` | `codegen/src/lower/mod.rs:17–25` | Pluggable via trait dispatch |
| `derive_recognizers()` | `passes/src/lib.rs:324–353` | Hand-curated JSON-only fixture |
| `derive_backend_shape_with_diagnostics()` | `passes/src/lib.rs:390–441` | Cost facts populator; no cost eval |
| `regex_first_bytes()` | `ir/src/lib.rs:782–799` | Hardcoded pattern matching |
| `ShapeLowering` trait | `codegen/src/lower/mod.rs:13–15` | Pluggable implementation registry |

---

**Audit Timestamp**: 2026-05-20 @ ~17 min elapsed
