# Tranche AL — Emission Unification, Per-Branch Materialization, Global CSP, Joint Optimization

## Context

Post-AK: citm 2,008 MB/s (+25% vs slab era, +23% vs simd-json). The tape substrate is sound. A six-agent deep audit reveals the optimization stack is **architecturally complete yet strategically dormant**:

1. **EmissionTier is dead infrastructure.** Direct tier's separate ABI (no tape param) causes `reconcile_cross_component_tiers` to promote everything to Tape. The 3-function triad at `grammar.rs:217-280` is never reached. TS/WASM don't even read `ir.emission_tier`. ~2,000 LOC of dead weight.

2. **mark_children runs unconditionally for all Alt branches.** Leaf branches (string, number, bool, null) push no children but pay mark_children overhead. On citm: ~9% of parse time (~70us of ~791us).

3. **CSP solves per-component, not globally.** Cross-component optimization impossible. The tier variable family is wasted (reconciliation defeats it). No feedback from e-graph extraction into the strategy solver.

4. **Per-pair boxing persists for heterogeneous Alts.** `join_types` falls back to `BoxedEnum`, forcing `ValuePlacement::Alloc` per variant. 153 slab alloc sites in CSS L4. The fix: `TypeDesc::TaggedUnion` — inline discriminated union avoiding heap allocation.

5. **Transform passes clone eagerly.** `fuse_token/mod.rs:63` clones every rule body. `inline.rs` and `fuse.rs` double-clone. `first_sets.rs` rebuilds a HashMap every fixed-point iteration.

**Already landed (verified by audit):** IIFE → labeled blocks (AC.2), SIMD string scanning via memchr2 (parse-that), SWAR digit parsing, nibble-LUT byte classification, u8x16 whitespace scanning. These need no further work.

---

## Dependency Graph

```
AL.0 (delete EmissionTier)              ← FIRST: refactor, ~2000 LOC deleted
  ↓
AL.1 (per-branch mark_children)         ← parse-time win (~9% citm)
  ↓
AL.2 (allocation surgery)               ← compile-time, independent of AL.0/1
  ↓
AL.3 (cost model rename + tuning)       ← depends on AL.0
  ↓
AL.4 (global CSP solve)                 ← depends on AL.0 (tier vars removed)
  ↓
AL.5 (e-graph → CSP feedback bridge)    ← depends on AL.4
  ↓
AL.6 (TaggedUnion boxing elimination)   ← independent; type system + codegen
  ↓
AL.7 (profile + verify + document)      ← after all
```

---

## AL.0 — Delete EmissionTier (~2,000 LOC net deletion)

MaterializationClass already determines emission shape. EmissionTier is a redundant axis that never activates.

### Files to DELETE (4 files, ~820 LOC)

| File | LOC |
|------|-----|
| `crates/ir/src/passes/materialization/emission_tier.rs` | 190 |
| `crates/ir/src/passes/csp_strategy/decode_tier.rs` | 254 |
| `crates/ir/src/passes/csp_strategy/constraints/tier.rs` | 119 |
| `crates/ir/src/passes/csp_strategy/constraints/parent.rs` | 257 |

### Files to MODIFY (source, ~400 LOC deleted)

**`crates/ir/src/passes/csp_strategy/mod.rs`**:
- Delete `StrategyValue::Tier(EmissionTier)` variant
- Delete `tier_vars` HashMap, `build_tier_domain` fn (~40 LOC)
- Delete `reconcile_cross_component_tiers` + `collect_refs` (lines 1422-1489)
- Change `solve_grammar_components` return: 3-tuple → 2-tuple `(RecognizerDecisionMap, HashMap<NodeId, MaterializationClass>)`
- Delete `Site::Tier(RuleId)` variant and decode arm
- Remove tier/parent constraint installation from `install_cross_rule_constraints`

**`crates/ir/src/passes/csp_strategy/constraints/mod.rs`**:
- Delete `pub mod parent;` and `pub mod tier;`
- Delete `tier_vars` field from `ConstraintCtx`, `legal_tiers` method

**`crates/ir/src/types/grammar.rs`**:
- Delete `emission_tier: HashMap<RuleId, EmissionTier>` field

**`crates/core/src/backend/rust/emitter/grammar.rs`**:
- Delete tier lookup + Direct dispatch (lines 108-118)
- Delete entire `emit_direct_tier_rule` (lines 217-280)
- `emit_rule_function_impl` goes directly to `emit_tape_tier_rule`

**`crates/core/src/backend/rust/emitter/tape_prelude.rs`**:
- Delete `emit_direct_inner_signature`, `emit_direct_shim_signature`

**`crates/core/src/backend/rust/emitter/binary.rs`**:
- Delete Direct-tier dispatch in `emit_call_impl`

**`crates/core/src/pipeline/compile.rs`**:
- Delete `tier_refined` handling, `decode_emission_tier` span, `reconcile_cross_component_tiers` span (lines 607-639)
- Adjust destructuring to 2-tuple

**Re-export hubs**: `passes/mod.rs`, `materialization/mod.rs`, `lower/mod.rs` — remove dead re-exports and field initializers.

### Tests (~50 files)
- DELETE `crates/ir/tests/lattices/emission_tier_lattice.rs` (534 LOC)
- Delete tier-specific tests from `cross_rule_csp.rs`, `cost_weights_unified.rs`
- Remove `emission_tier: HashMap::new()` from ~25 test files

---

## AL.1 — Per-Branch mark_children Dispatch

Move `mark_children` from rule-level prelude into individual compound branches. Leaf branches skip it.

### Design

**Prelude** for MustTape Alt-bodied rules changes from:
```rust
let __children = TapeBuilder::mark_children(tape);
```
to:
```rust
let mut __children = TapeOffset::NONE;
```

**Epilogue stays unchanged** — `push_compound` with `NONE` already sets `has_children = false` (builder.rs:133).

**Per-branch injection**: Compound branches get `__children = mark_children(tape);`. Leaf branches get `__children = TapeOffset::NONE;` (prevents stale marks from failed compound branches leaking).

### Branch classification

`branch_pushes_children(ir, node) -> bool` in `driver/alt.rs`:
- `Literal | Regex | Epsilon` → false
- `Map { inner, fn_id }` non-closure → recurse on inner
- `OptionalWhitespace(inner)` → recurse
- `Negate(_)` → false
- Everything else → true (conservative, always safe)

### Files to modify

| File | Change |
|------|--------|
| `backend/types.rs` | Add `pushes_children: bool` to `AltBranchInfo` |
| `backend/driver/alt.rs` | Add `branch_pushes_children` classifier, thread through `AltBranchInfo` |
| `backend/rust/emitter_types.rs` | Add `children_ident: Option<syn::Ident>` to `RustEmitCtx` |
| `backend/rust/emitter/mod.rs` | Set `children_ident` in `pre_compile_rule_body` for MustTape Alt rules |
| `backend/rust/emitter/grammar.rs` | MustTape Alt prelude: `let mut __children = TapeOffset::NONE;` |
| `backend/rust/emitter/alt.rs` | Inject per-branch `__children` assignment in dispatch + checkpoint paths |
| `backend/rust/emitter/dispatch.rs` | Same injection for dispatch table branches |

### Expected impact
~9% on citm: 2,008 → ~2,200 MB/s.

---

## AL.2 — Allocation Surgery (compile-time)

### AL.2a — FIRST-set loop HashMap rebuild
**File**: `crates/ir/src/passes/sets/first_sets.rs`

Build `nullable_of` once before the loop, update in-place. Remove per-iteration HashMap rebuild. Reuse for Phase 2 instead of rebuilding at lines 63-67.

### AL.2b — Eliminate double-clone in inline.rs and fuse.rs
**Files**: `crates/ir/src/passes/transform/inline.rs`, `fuse.rs`

Build lookup table directly with single clone per eligible body, skipping the intermediate candidate Vec.

### AL.2c — Ownership moves in lr.rs
**File**: `crates/ir/src/passes/lr.rs`

`std::mem::replace` to take Alt body, `into_iter` to consume branches, `strip_leading_ref_owned` to move instead of clone.

### AL.2d — Backend HashMap moves in analysis.rs
**File**: `crates/core/src/backend/driver/analysis.rs`

`std::mem::take` instead of `.clone()` for `delim_scan_configs` and `key_dispatch_configs` (function takes `ir` by value).

---

## AL.3 — Cost Model Overhaul

### AL.3a — Rename emission_tier_bonus → leaf_emission_bonus
**Files**: `crates/egraph/src/cost_weights.rs`, `crates/ir/src/egraph/cost.rs`

Pure rename. Bonus semantics (reward leaf-shaped bodies) unchanged; naming now reflects MaterializationClass reality. Global rename across all tests and consumers.

### AL.3b — Add compound_mark_children_cost term
**File**: `crates/egraph/src/cost_weights.rs`

New weight `compound_mark_children_cost: f64` (default: 0.5). In `cost.rs` Alt cost computation, penalize branches whose child cost exceeds the leaf ceiling — incentivizes extraction toward Alt decompositions where more branches are leaf-eligible.

---

## AL.4 — Global CSP Solve

### Problem
Per-component solve (AF.3) prevents cross-component optimization. A rule's classification cannot be informed by callers in other components. The X.6 blowup (9ms → 94ms) was before Y.-1's budget guard — with budget + fast-path skip, global solve is bounded.

### Design

**`crates/ir/src/passes/csp_strategy/mod.rs`**:

Add `CspSolveMode` to `CostConfig`:
```rust
pub enum CspSolveMode {
    PerComponent,                           // current default
    Global { node_budget: u64 },            // new: 10M for CSS-scale
}
```

Add `solve_grammar_global(ir)` that:
1. Creates a single `Csp<StrategyDomain>`
2. Installs variables for ALL rules (not per-component)
3. Installs ALL cross-rule constraints across the entire grammar
4. `SolveConfig::node_budget = 10_000_000`
5. Falls back to `decode_min_cost_per_variable` if budget exceeded

In `solve_grammar_components`, dispatch on mode:
```rust
match ir.cost_config.csp_solve_mode {
    CspSolveMode::Global { node_budget } => solve_grammar_global(ir, node_budget),
    CspSolveMode::PerComponent => { /* existing */ },
}
```

**Default**: `Global { node_budget: 10_000_000 }` for all grammars. Compile-time degradation acceptable for parse-time gains.

### Files to modify
| File | Change |
|------|--------|
| `crates/ir/src/passes/csp_strategy/mod.rs` | Add `solve_grammar_global`, `CspSolveMode` |
| `crates/ir/src/egraph/cost_config.rs` | Add `csp_solve_mode` field |
| `crates/core/src/pipeline/compile.rs` | Wire mode through |

---

## AL.5 — E-graph → CSP Feedback Bridge

### Problem
E-graph extraction and CSP strategy solve run sequentially with zero feedback. The e-graph picks cost-optimal forms, then the CSP picks strategies independently. Decisions can be incoherent.

### Design

After `write_back_optimized`, compute per-rule extraction costs and carry them to the CSP as soft constraints via `SoftLambdaConstraint` (already in csp-solver).

### Implementation

**New file**: `crates/ir/src/egraph/extraction_advice.rs`

```rust
pub type ExtractionAdviceMap = HashMap<RuleId, Vec<(StrategyValue, f64)>>;

pub fn derive_advice(
    egraph: &EGraph<GrammarENode, GrammarAnalysis>,
    ir: &GrammarIR,
    rule_body_ids: &HashMap<RuleId, Id>,
    cost: &GrammarCostModel,
) -> ExtractionAdviceMap { ... }
```

For each rule, the bridge examines the cost-optimal e-class for the rule body and produces soft penalties:
- Low extraction cost (leaf-like) → penalize MustTape materialization
- High dispatch-branch count with disjoint FIRST sets → reward ByteDispatch AltMode
- Regex pattern recognized by classify → reward specific RegexEngine choice

**`crates/ir/src/types/grammar.rs`**: Add `extraction_advice: ExtractionAdviceMap` field.

**`crates/core/src/pipeline/compile.rs`**: After e-graph write-back (line 507), derive advice:
```rust
ir.extraction_advice = bbnf_ir::egraph::derive_advice(&egraph, &ir, &rule_body_ids, &cost);
```

**`crates/ir/src/passes/csp_strategy/mod.rs`**: In `solve_component` (or `solve_grammar_global`), install advice as `SoftLambdaConstraint`s before solving.

### Expected impact
- CSS Tailwind parse: ~10% improvement (dispatch biases align with extraction structure)
- JSON: minimal (already well-structured)
- Compile time: ≤+2% (one extra bridge walk, e-graph kept alive slightly longer)

---

## AL.6 — TaggedUnion Boxing Elimination

### Problem
Every heterogeneous Alt falls back to `TypeDesc::BoxedEnum` in `join_types` (`constraint/helpers.rs:86-96`), forcing `ValuePlacement::Alloc` per variant — one slab allocation per pair. 153 alloc sites in CSS L4, 2 in JSON.

### Design

Introduce `TypeDesc::TaggedUnion` — an inline-storable discriminated union. When all variants of a heterogeneous Alt fit within a size threshold (≤64 bytes), emit a stack-allocated tagged enum instead of a heap-allocated boxed enum.

### Implementation

**Phase 1: Type Descriptor** (IR tier)

`crates/ir/src/types/type_desc.rs`:
```rust
pub enum TypeDesc {
    // ... existing variants ...
    TaggedUnion {
        variants: Vec<(u8, TypeDesc)>,  // discriminant → inner type
    },
}
```

`crates/ir/src/passes/types/constraint/helpers.rs`: Extend `join_types` at line 86-96 — when variants are heterogeneous but all small, produce `TaggedUnion` instead of `BoxedEnum`.

**Phase 2: Decision Logic** (backend tier)

`crates/core/src/backend/types/decisions.rs`: Add match arm:
```rust
TypeDesc::TaggedUnion { .. } => ValuePlacement::Inline,
```

**Phase 3: Codegen** (Rust emitter)

`crates/core/src/backend/rust/ir_enums.rs`: Generate the tagged union enum:
```rust
enum __TU_<id> {
    Variant0(TypeA),
    Variant1(TypeB),
    // ...
}
```

`crates/core/src/backend/rust/ir_types.rs`: Extend `type_desc_to_syn_raw` for TaggedUnion.

Alt emitter: Replace `slab().alloc(__v)` with `__TU_N::VariantK(__v)` for inline placement.

### Expected impact
- CSS L4: slab alloc sites drop from ~153 to ≤25
- JSON: minimal (already simple types)
- Parse-time: measurable on CSS (eliminates heap allocation in hot inner loops)

---

## AL.7 — Profile, Verify, Document

### Verification steps

1. `cargo test --workspace` — full suite
2. `cargo expand --bench json_monolithic` — verify:
   - No `_inner`/`_direct` functions
   - Leaf branches have `__children = TapeOffset::NONE`
   - Compound branches have `__children = mark_children(tape)`
   - No `slab().alloc` for TaggedUnion-eligible Alts
3. `cargo bench --bench json_monolithic` — all 5 files
4. `cargo bench --bench json_competitors` — vs sonic-rs/simd-json/serde
5. `cargo bench --bench css_monolithic` — CSS regression check
6. `samply record` on citm + canada + tailwind — verify:
   - mark_children moved into compound branches
   - Global CSP activated
   - TaggedUnion inline allocation
7. `cargo test -p bbnf-tape` — tape parity
8. Document in `docs/tranches/AL.md`

### Expected cumulative results

| Phase | Target |
|-------|--------|
| AL.0 | ~2,000 LOC deleted, zero parse-time change |
| AL.1 | ~9% on citm (2,008 → ~2,200 MB/s) |
| AL.2 | Compile-time improvement on CSS-scale grammars |
| AL.3 | Better e-graph extraction for mixed Alts |
| AL.4 | Cross-component optimization for CSS (~5% compile quality) |
| AL.5 | ~10% CSS parse-time from coherent extraction↔strategy |
| AL.6 | Measurable CSS parse-time from eliminated boxing |
| AL.7 | Documentation + profile evidence |

---

## Commit Strategy

1. **AL.0**: Single commit — all EmissionTier deletions + test updates
2. **AL.1**: Single commit — per-branch mark_children + branch classification
3. **AL.2**: Single commit — all allocation surgery (a-d)
4. **AL.3**: Single commit — cost model rename + new term
5. **AL.4**: Single commit — global CSP mode
6. **AL.5**: Single commit — extraction advice bridge
7. **AL.6**: 2 commits — TypeDesc::TaggedUnion (type system) then codegen
8. **AL.7**: Single commit — docs/tranches/AL.md with bench results
