# PROTO-1 — LOCK-1 (Solver Spine), pass-2, probe 1

**Question.** Does let-generalization graft *additively* into the running CSP
(project_types), or does it force the union-find re-seed? I.e. is the banked
HM(X) floor (per-rule let-generalization, for false-fusion soundness) a
cheapest-additive-diff for the running carrier 1B, or does adopting it
re-inflate 1B's build-cost to 1A's (re-opening INC-1)?

**Base-pin (XL-H).** Worktree HEAD was stale (`d2b32f6e3`); fast-forwarded
`--ff-only` to the pin. **Measured on `HEAD = ce51c36fcbdf5108eb028005586672d9a6d2ca1b`**
(`git merge-base --is-ancestor ce51c36fc HEAD` → OK). No wrong-artifact-risk.

---

## Method

1. Read the running carrier `crates/ir/src/passes/types/` end-to-end: the
   constraint generator (`generate.rs`), the Ref bridge (`constraint/reference.rs`),
   the lattice domain (`constraint/domain.rs`), the revise primitives
   (`constraint/revise.rs`), and the solve driver (`mod.rs`).
2. Authored a false-fusion fixture and RAN it through the real
   `project_types` via `compile_grammar(...)` (VM target), printing
   `ir.types` keyed by rule name. Fixture:
   `crates/core/tests/proto1_false_fusion.rs`.
3. Searched for the polymorphic surface the floor would govern (parametric
   rules / grammar closures) and its lowering status.

---

## What ran (measured)

`cargo test -p bbnf --profile ax-iter --test proto1_false_fusion` →
`test result: ok. 1 passed`. Fixture
(`top=a b; a=pair pair; b=pair pair; pair=k v; k=/.../ ->f64; v=/.../ ->u8`),
`structural: true` to preserve rule identity:

```
ir.types (rule -> TypeDesc):
  a     -> Tuple([BoxedEnum, BoxedEnum])
  b     -> Tuple([BoxedEnum, BoxedEnum])
  k     -> F64
  pair  -> Tuple([F64, U8])
  top   -> Tuple([BoxedEnum, BoxedEnum])
  v     -> U8
pair entry count in ir.types = 1
```

- `k -> F64`, `v -> U8`: distinct scalar leaves keep distinct primitives.
- `pair -> Tuple([F64, U8])`: the shared helper has **exactly one** entry in
  `ir.types` — one `RuleId` → one `TypeDesc`. `RefConstraint` scalar-projection
  composes k/v correctly.
- `a` and `b` reference `pair` at two distinct sites and **both** resolve to
  the identical `Tuple([BoxedEnum, BoxedEnum])`. The uses do not diverge, and
  there is no "monomorphize-to-first-use" contest because there is nothing
  polymorphic to monomorphize.

## The numbers / the structural facts

- **One rule → one type var.** `generate.rs:89` inserts exactly one
  `TypeVarId` per `RuleId` into `rule_vars`; every `IrNode::Ref(rule_id)`
  bridges to that **single shared** rule var (`generate.rs:176`). There is no
  per-call-site type variable.
- **The Ref bridge is unidirectional.** `RefConstraint::revise`
  (`constraint/reference.rs:98-131`) reads `vars[rule_var].solved` and writes
  `vars[var]` (the ref). It never writes back to the rule var. Use sites impose
  **zero** back-pressure on a rule's type. False fusion — two uses competing
  for one polymorphic binding's monotype, first-writer-wins — is therefore
  structurally impossible: uses never write the binding.
- **`TypeDesc` is a ground lattice with no type-variable variant** (`Span, F64,
  U8, …, Tuple, Vec, Option, Named, BoxedEnum, Enum, HeterogeneousAltJoin`).
  There are no free type variables in any rule's projected type — hence nothing
  to *generalize* and nothing to *fresh-instantiate*. `grep -rniE
  'generali|instantiat|polymorph|scheme'` over the carrier = 1 doc-comment hit,
  as briefed.
- **The only polymorphic surface (grammar closures) is beta-reduced pre-IR and
  is currently non-functional.** The pipeline partitions closures
  (`pipeline.rs:51`) and expands them inline during lowering (`pipeline.rs:77-78`,
  "Closures are first-class grammar functions expanded inline during
  lowering"). `IrNode` has **no** `Call`/`Closure`/parametric variant — the only
  cross-rule edge is `Ref(RuleId)`. Moreover the closure-lowering tests
  (`crates/core/tests/pipeline.rs:708-739`, `parens = |x| "(" x ")" ; value =
  parens("hello") ;`) are `#[ignore]`d: `lower::expression` panics "closure:
  missing body child" (AV.0.11 Category A, forward-ticketed). So the surface
  HM(X) let-generalization would govern is (a) *designed* to monomorphize by
  beta-reduction, and (b) not even wired in the running carrier.

## Graft assessment (LOC / monotone / runtime diff)

- **LOC added for a functional graft: N/A — inauthorable.** A generalize-at-
  rule-solve + fresh-instantiate-at-RefConstraint graft has no substrate: no
  `TypeDesc` type-variable to close over, no parametric IR node to instantiate
  at, and a unidirectional Ref bridge with no fusion to prevent. Fresh
  instantiation of a ground monotype is the identity; generalization of a type
  with no free vars is the identity.
- **Monotone:** the graft question ("stays monotone, no oscillation") does not
  arise — there is no generalization step to add to the `Changed`-bool sweep.
- **`git diff crates/core/src/runtime/` = 0 lines** (verified). The only file
  added is the probe test `crates/core/tests/proto1_false_fusion.rs`; **zero
  lines** were added to any type-projection pass, so `ir.types` for
  json/sheets/css_l4 is byte-identical to base *by construction* (no code path
  in the solver spine changed).

---

## Verdict

**NEITHER additive-graft NOR re-seed — the floor is INAPPLICABLE to 1B, and 1B
therefore ships false-fusion soundness for FREE (zero-diff), not via a graft.**

The dichotomy's premise — "the banked HM(X) floor requires per-rule
let-generalization for false-fusion soundness" — does not bind the running
carrier. 1B is monomorphic **by construction**: closures are beta-reduced /
inline-expanded before the IR exists, the IR has no parametric node, every rule
carries exactly one CSP type var, and the Ref→rule bridge is unidirectional.
False fusion is unreachable, so let-generalization is dead code against this IR
(measured: a shared helper `pair` gets exactly one type; two use sites resolve
identically and correctly).

**Consequence for the 1A ⊥ 1B carrier fork (§0 X10):**
- 1B's cheapest-additive-diff claim **SURVIVES** — strengthened: the diff for
  this floor is **0**, not "cheap-additive". Build-cost stays **LEAN-1B**.
- **INC-1 does NOT re-open on build-cost** from this axis: no union-find
  re-seed is forced, because no generalization machinery is imported.
- "1A and 1B are one architecture" holds only in *outcome* (both deliver
  false-fusion soundness), not in *mechanism*: 1A via HM(X) generalize/
  instantiate over a union-find; 1B via structural monomorphization
  (pre-IR beta-reduction + one-rule-one-type monomorphic CSP). On this axis
  1B needs none of 1A's machinery.

**Caveat / scope of claim.** Soundness here is contingent on BBNF keeping
polymorphic reuse *structural* (beta-reduction + inlining before types). If a
future tranche wires a **live parametric rule that survives into the IR as an
un-reduced call** (i.e. reintroduces polymorphism at the CSP layer rather than
monomorphizing it away), THEN — and only then — the false-fusion floor becomes
reachable and a generalize/instantiate mechanism (or per-call-site
specialization) becomes required. The present carrier does not do this; the
closure path is beta-reduced and currently non-functional. The floor is a
requirement for a polymorphism 1B does not have.

---

## Verdict line

`RAN_MEASURED — HEAD ce51c36fc — false fusion UNREACHABLE in 1B (monomorphic by
construction); let-generalization graft inauthorable/dead-code; floor delivered
zero-diff; runtime diff = 0; build-cost LEAN-1B; INC-1 stays closed on this axis.`
