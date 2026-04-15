# Tranche AL — Three-Tier Activation + Global CSP

## Audit summary

Post-AK state: citm 2,008 MB/s (+25% vs slab era, +23% vs
simd-json). The tape substrate is sound. But the full
optimization stack — CSP, e-graph, three-tier emission — is
**architecturally complete and strategically dead**:

1. **CSP fires, produces zero changes for JSON.** Five variable
   families (Alt mode, Wrap mode, RegexEngine, Materialization,
   EmissionTier) are created and solved. For JSON, all resolve
   to defaults. The infrastructure is designed for complex
   grammars (CSS L4: 265+ rules, multi-component call graphs).

2. **E-graph saturates, zero rules match for JSON.** Five rewrite
   rules exist (dedup, superset absorb, union merge, regex fuse,
   common suffix factor). JSON's simple Alt has no matching
   patterns. Zero rewrites applied.

3. **Three-tier system: "strategically defeated by
   reconciliation."** `reconcile_cross_component_tiers` (line
   1422) is a monotone widening pass: if a Direct-tier rule
   calls ANY Tape-tier rule, `tier_join` promotes the caller to
   Tape. Since `tier_join(Direct, Tape) = Tape` and most rules
   call other rules across components, **Direct tier never
   survives reconciliation.** The field `ir.emission_tier` is
   written but all values are Tape. The emitter checks it but
   always takes the Tape path.

4. **`emit_direct_tier_rule` exists but is dead code.** The
   three-function triad (`_inner`, tape wrapper, `_direct` shim)
   is fully implemented but never reached because no rule gets
   Direct tier in production.

5. **Expanded JSON: still 5 push_compound + 0 push_leaf + 3
   mark_children.** Every value match pushes a compound record
   with mark_children overhead, even for leaf values (string,
   number, bool, null) that need no children.

## Root cause: The Direct tier's function signature

The reconciliation kill-switch exists because Direct-tier
functions use `fn __<rule>_direct(state) -> Option<()>` — **no
tape parameter**. A Direct function cannot call a Tape function
because it has no tape to pass. So the reconciliation enforces
`parent.rank() >= child.rank()`, and since JSON's `value` rule
calls both leaf rules (Direct-eligible) and compound rules
(Tape), value gets promoted to Tape. This cascades: every rule
that calls value also becomes Tape.

## The fix: Call-site emission coercion

The three-tier system should work as follows:

**A Direct-tier rule has the same ABI as Tape**: `fn __<rule>(state,
tape) -> Option<TapeOffset>`. The difference is that the rule's
OWN record is a **push_leaf** (TapeSpanOnly path: no
mark_children, no has_children check), but it PASSES the tape
through to any Tape-tier children.

This means:
- Direct calling Tape child: passes tape, child pushes compound.
  Direct rule pushes push_leaf for itself.
- Tape calling Direct child: child pushes push_leaf, tape records
  it as a leaf record. Parent pushes push_compound wrapping all
  children (both leaf and compound).
- Direct calling Direct child: both push push_leaf. No compound
  overhead anywhere.

**The key insight:** "Direct" doesn't mean "no tape parameter." It
means "my OWN record is a leaf." The tape is a pass-through
resource, not an opt-in.

This eliminates the reconciliation problem entirely. A Direct rule
CAN call Tape children because it carries the tape. The
`reconcile_cross_component_tiers` function becomes unnecessary —
the CSP's `ParentCompatibility` constraint is sufficient.

## AL.0 — Unify Direct/Tape function signature

**Delete the `_direct` shim and `_inner` function.** A Direct-tier
rule has the SAME function signature as Tape:

```rust
fn __<rule>(state, tape) -> Option<TapeOffset>
```

The ONLY difference is the rule's own epilogue:
- Tape: `mark_children` + body + `push_compound`
- Direct: body + `push_leaf` (no mark_children)

This is exactly what `MaterializationClass::TapeSpanOnly` already
generates! The "Direct tier" IS `TapeSpanOnly` emission. The
distinction between EmissionTier and MaterializationClass is
unnecessary — they encode the same information.

**Simplification:** Delete EmissionTier entirely. The materialization
class (MustTape / TapeSpanOnly / TransparentElide) already
determines the emission shape. The emitter dispatches on class, not
tier. The CSP's tier variable family is eliminated (3 fewer
variables per rule).

**Changes:**
- Delete `crates/ir/src/passes/materialization/emission_tier.rs`
- Delete `crates/ir/src/passes/csp_strategy/decode_tier.rs`
- Delete `crates/ir/src/passes/csp_strategy/constraints/tier.rs`
- Delete `reconcile_cross_component_tiers`
- Remove `ir.emission_tier` field from `GrammarIR`
- Remove tier checks in `emit_rule_function_impl` and `emit_call_impl`
- Emitter dispatches solely on `MaterializationClass`

**This is a deletion tranche.** Net LOC: −500 to −800.

## AL.1 — Materialization-driven call-site coercion

With EmissionTier gone, the emitter needs per-call-site coercion:

When a MustTape rule calls a TapeSpanOnly rule, the callee pushes
a leaf record. When a TapeSpanOnly rule calls a MustTape rule, the
callee pushes compound records. The tape is always passed through.

**This already works.** The current `emit_call_impl` just calls
`Self::__callee(state, tape).map(|_| ())` regardless of tier. The
callee function pushes its own record (push_leaf or push_compound
depending on its materialization class). The caller wraps
everything in its own record.

**No code changes needed** for call-site coercion — it's implicit
in the current ABI. Removing the EmissionTier layer simply removes
the dead abstraction.

## AL.2 — TapeSpanOnly for the JSON value rule

With AL.0 landed, the JSON value rule's Alt body has the
materialization class from the classifier. Currently it's MustTape
because the Alt has compound branches (object, array).

But the per-branch emission from AK.1 already threads `__branch_idx`.
The question is: should the value rule be MustTape (compound record)
or TapeSpanOnly (leaf record)?

For the ENTRY RULE, the answer is MustTape: it needs a compound
record so the view layer can traverse children. But for leaf
BRANCHES within the entry rule, the branch's inner match doesn't
push children. The compound record for a leaf branch has
`has_children = false`.

The optimization: detect at CODEGEN time that a branch is leaf-only
and skip mark_children for those branches. This is the full
per-branch materialization dispatch from the AK plan:

```rust
match byte {
    b'"' => {
        // Leaf branch: no mark_children needed
        __branch_idx = 2;
        string_scan(state);
    }
    b'{' => {
        // Compound branch: needs mark_children
        __branch_idx = 0;
        // mark_children is already captured at rule entry
        object_parse(state, tape);
    }
}
// Epilogue: push_compound uses __children from entry
// For leaf branches, __children == current tape len → has_children = false
```

Wait — this is what ALREADY HAPPENS. The mark_children is captured
at rule entry. For leaf branches, no children are pushed between
mark and compound. push_compound clears has_children. The compound
record for leaf branches is effectively a leaf.

**The overhead for leaf branches is: one mark_children read + one
has_children comparison.** That's ~2 ns per value. For 35K leaf
values in citm: ~70 μs. At 791 μs parse time (samply): ~9%.

To truly eliminate this: DON'T capture mark_children at entry.
Instead, capture it INSIDE compound branches only:

```rust
let __span_lo = state.offset as u32;
let mut __branch_idx: u8 = 0;
let mut __children = ::bbnf::runtime::tape::TapeOffset::NONE;
match byte {
    b'"' => {
        __branch_idx = 2;
        string_scan(state);
    }
    b'{' => {
        __branch_idx = 0;
        __children = mark_children(tape);
        object_parse(state, tape);
    }
}
if __children.is_none() {
    push_leaf(tape, Rule, __span_lo, state.offset, __branch_idx)
} else {
    push_compound(tape, Rule, __children, __span_lo, state.offset, __branch_idx)
}
```

This moves mark_children INTO compound branches. Leaf branches
skip it entirely. The epilogue checks `__children.is_none()` to
choose push_leaf vs push_compound.

**This requires modifying the rule emission for Alt-bodied rules.**
When `ctx.branch_idx_ident` is set, the emitter generates the
split prelude/epilogue above.

**Changes:**
- `crates/core/src/backend/rust/emitter/grammar.rs` — modify
  Alt-bodied rule emission to use split mark_children
- `crates/core/src/backend/rust/emitter/alt.rs` — inject
  mark_children into compound branches

## AL.3 — CSP global solve

Currently the CSP solves per-component (connected components of the
call graph). This was a performance optimization to avoid the X.6
blowup (9 ms → 94 ms for CSS L4). But it prevents cross-component
optimization.

**Fix:** Solve the FULL grammar as one CSP instance, but with
a higher node budget. For JSON (10 rules), the budget is trivially
sufficient. For CSS L4 (265 rules), increase the budget from 1M to
10M nodes.

This enables the solver to make globally optimal decisions: a rule
that's TapeSpanOnly in isolation but MustTape when called by a
compound parent can be correctly classified by the global solver.

**Changes:**
- `crates/ir/src/passes/csp_strategy/mod.rs` — add a
  `solve_global` mode that processes the full grammar
- Increase default node budget for CSS-scale grammars

## AL.4 — E-graph emission-aware saturation

The current e-graph rules are structural (dedup, absorb, merge,
fuse). They don't consider emission cost. The `emission_tier_bonus`
in the cost model rewards Direct-eligible shapes, but since no
rules actually get Direct tier, the bonus is inert.

With AL.0-AL.2 landed (materialization-driven emission), the cost
model should reward TapeSpanOnly-eligible shapes:

- Reward shapes that reduce push_compound to push_leaf
- Reward Alt factoring that makes more branches leaf-eligible
- Penalize shapes that force mark_children (compound children)

**Changes:**
- `crates/ir/src/egraph/cost.rs` — rename `emission_tier_bonus`
  to `leaf_emission_bonus`, apply it to TapeSpanOnly-eligible
  nodes based on the materialization classifier
- `crates/egraph/src/cost_weights.rs` — update field name

## AL.5 — Profile + verify

After AL.0-AL.4:
1. `cargo expand --bench json_monolithic` — verify push_leaf for
   leaf branches
2. `cargo bench --bench json_monolithic` — all 5 files
3. `cargo bench --bench json_competitors` — vs simd-json/serde
4. `samply record` on citm + canada
5. `cargo test -p bbnf-tape -p bbnf-ir -p bbnf --test json_slab`
6. Document in `docs/tranches/AL.md`

## Dependency graph

```
AL.0 (delete EmissionTier) ← FIRST, simplification
  ↓
AL.1 (call-site coercion) ← implicit, no code changes
  ↓
AL.2 (per-branch mark_children) ← the performance win
  ↓
AL.3 (global CSP) ← independent
  ↓
AL.4 (emission-aware cost model) ← depends on AL.0
  ↓
AL.5 (verify) ← after all
```

## Expected impact

- **AL.0:** Net deletion of ~500-800 LOC. Zero performance change
  (removes dead code).
- **AL.2:** ~9% improvement on citm (eliminate mark_children for
  leaf values). citm 2,008 → ~2,200 MB/s.
- **AL.3+AL.4:** CSS-focused. JSON won't change (too simple), but
  CSS would benefit from global optimization.
- **Combined:** Architectural clarity (one axis: MaterializationClass,
  not two), with per-branch leaf emission for the hot path.
