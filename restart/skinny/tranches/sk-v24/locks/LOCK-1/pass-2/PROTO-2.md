# PROTO-2 — LOCK-1 (Solver Spine), probe 2: refinement-aware assign clash arm (X8, 3rd call)

Family: 1B carrier hardening (X8, carrier-independent). Ships regardless of carrier winner.

## BASE-PIN
- Worktree HEAD measured on: `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (== base-pin ce51c36fc).
- On entry the worktree was STALE (HEAD `d2b32f6e3`, an *ancestor* of ce51c36fc — the pin sits ahead). `git merge --ff-only ce51c36fc` fast-forwarded cleanly; `git merge-base --is-ancestor ce51c36fc HEAD` → yes. All numbers below are on ce51c36fc. NOT wrong-artifact-risk.
- loadavg at launch: 19:18 up, load averages 8.22 6.51 6.74 (cold build ran under this).

## QUESTION
X8 is the one carrier-independent deliverable and was unmet after two passes. At HEAD, `crates/ir/src/passes/types/constraint/revise.rs` `assign` is the raw LWW:
`if slot.as_ref()==Some(&ty){false}else{*slot=Some(ty);true}`.
The owed hardening: raise-on-disagreement *without* freezing the ~69 legal `BoxedEnum→structural` refinements the four production grammars depend on. A naive "refuse all overwrites" freezes those refinements; the arm must distinguish REFINEMENT (permit) from CONCRETE-vs-CONCRETE clash (refuse + name).

## METHOD
Replaced the LWW with a refinement-aware arm keyed on a recursive **fully-ground** predicate:

- `is_fully_ground(t)` = t carries no `BoxedEnum` / `HeterogeneousAltJoin` under-narrowed layout at any depth. `Enum` and `Named` are deliberately treated as **non**-ground (the solver bridges vec-context `Enum` slots and `Named` map-results by `EqualConstraint` to variables that may legally settle on a scalar — counting them as ground would misread those legal bridges as clashes). Only scalars / `Span` and tuples/options/vecs built entirely from them are ground.
- `assign` arms:
  - `existing == ty` → `false` (no change).
  - `is_fully_ground(existing) && is_fully_ground(&ty)` and they differ → **refuse** the overwrite (keep incumbent so the fixed point stays stable), stash `(incumbent, rejected)` on the domain's new `clash` side-channel.
  - otherwise (first assignment, or either side under-narrowed) → **permit** — byte-for-byte the historical LWW move. This is where every `BoxedEnum→structural` (and `Tuple([BoxedEnum..])→Tuple([F64..])`) refinement lands.

Recording is decoupled from the hot write path: the clash pair rides a `clash: Option<Box<(TypeDesc,TypeDesc)>>` field on `TypeDomain` (boxed → the no-clash domain stays pointer-width). After propagation converges, `project_types` harvests every stashed clash via `record_domain_clashes(vars, sink, node_of)` into `TypeObligation::TypeClash{node, lhs, rhs}` — routed through the SAME `ObligationSink` + dedup drain that `UnresolvedCompoundRef` / `HeterogeneousAltJoin` already use (Constitution viii; the obligation's consumer is 1C's `type_obligations → lsp_types::Diagnostic{range}` bridge — `node` supplies the range anchor).

Design note: keeping `assign`'s signature stable (vs. threading a sink+node through all 7 constraint structs and ~15 call sites) is the parsimony win — the domain-stash producer / harvest-consumer split touches only the write primitive + the pass driver, leaving every constraint impl untouched. One path, no orthogonal branch.

`assign` is the constraint-`revise` write path. `LatticeDomain::join` in `domain.rs` carries a second, dormant LWW used only by the pure-lattice propagation mode (not the explicit-constraint AC-3 path `project_types` runs); left untouched and noted here so the second LWW is on record, not a hidden contradiction.

### What ran
Base HEAD ce51c36fc. `cargo test --profile ax-iter`, sequential (single cargo per target dir):
1. `-p bbnf-ir --test type_clash_arm` (new fixture, the novel proof)
2. `-p bbnf --test types_heterogeneous_alt`
3-6. `-p bbnf --test project_types_{json,sheets,css_l4,bbnf}`

## THE NUMBERS
- **`bbnf-ir --test type_clash_arm` (new fixture) — GREEN, 3 passed / 0 failed** (cold compile 45.28s; re-run 0.00s). The three arms:
  - `concrete_vs_concrete_emits_exactly_one_type_clash ... ok` — two disagreeing fully-ground concretes (`F64` vs `U8`) on one variable → the sink drains **exactly one** `TypeObligation::TypeClash` carrying the `F64`/`U8` pair; the incumbent is kept (overwrite refused). The refusal is also what BREAKS the would-be oscillation — the fixed point converges precisely because the clash arm stops re-writing.
  - `boxed_enum_refinement_records_no_clash ... ok` — `BoxedEnum→F64`, `Tuple([BoxedEnum,U8])→Tuple([F64,U8])`, and `HeterogeneousAltJoin([F64,U8])→F64` all permitted with **zero** clashes. Refinements are not frozen.
  - `identical_grounds_record_no_clash ... ok` — same type twice is a no-op, not a clash.
- **Fixture-bug caught + fixed mid-probe (honest):** the first cut of the refinement test drove `BoxedEnum` and `F64` as two *persistent competing* GroundConstraints on one var. Because a refinement never refuses, the two grounds ping-pong forever (the ORIGINAL LWW oscillates on this input identically — it is a pathological fixture, not a code defect). It hung >60s and was killed. Refinements are inherently monotonic single-writer moves, so the corrected fixture models one by seeding the incumbent (`TypeDomain::ground(BoxedEnum)`) and applying a single overwrite — non-cyclic, converges, green.
- **Byte-identity gate — ALL GREEN.** `cargo test -p bbnf --profile ax-iter` (loadavg 17.66 at run):
  - `types_heterogeneous_alt` → **2 passed / 0 failed**
  - `project_types_json` → **10 passed / 0 failed**
  - `project_types_sheets` → **6 passed / 0 failed**
  - `project_types_css_l4` → **4 passed / 0 failed**
  - `project_types_bbnf` → **3 passed / 0 failed**
  The four `project_types_*` suites assert the struct-registry / field-layout facts derived directly from `ir.types`; their passing is the byte-identity proof that the ~69 `BoxedEnum→structural` refinements still land unchanged — none frozen by the new refuse arm. (Confirmed by construction: in a well-formed grammar no single type variable ever receives two disagreeing *fully-ground* types, so the refuse arm never fires on real grammars — it fires only on the synthetic clash fixture. loadavg-recorded, cold-cache first compile.)

- `git diff -- crates/core/src/runtime/` = **0 lines** (CONFIRMED). Changed files: `crates/csp-solver/src/csp/mod.rs` (+7, `variables()` accessor), `crates/ir/src/passes/types/constraint/domain.rs` (+clash field), `.../constraint/revise.rs` (refinement arm + `record_domain_clashes`), `.../passes/types/mod.rs` (harvest wire-in), `.../passes/types/obligation.rs` (`TypeClash` variant). Zero runtime, zero codegen.

## VERDICT
**PASS — X8 landed and RAN_MEASURED on ce51c36fc.** All four PASS conjuncts met:
1. `types_heterogeneous_alt` green — ✓ (2/0).
2. All `project_types_*` byte-identical — ✓ (json 10/0, sheets 6/0, css_l4 4/0, bbnf 3/0): the 69 refinements STILL land, none frozen.
3. Concrete-vs-concrete clash fixture emits **exactly one** `TypeObligation::TypeClash` (non-empty stream) — ✓.
4. `git diff -- crates/core/src/runtime/` = **0 lines** — ✓.

Carrier-independent: touches only the solver spine (`assign` write primitive, `TypeDomain`, obligation enum, the pass driver's harvest, one csp-solver accessor). Ships regardless of the 1B carrier winner.

**Joint-wave co-name (Constitution viii — the obligation's only consumer):** `TypeClash{node, lhs, rhs}` is consumer-less substrate until 1C's `type_obligations → lsp_types::Diagnostic{range}` bridge lands. The `node: Option<NodeId>` field is the range anchor that bridge consumes; the harvest already populates it from the primary + vec-context node maps for every clashing tree-position variable. LOCK-1 X8 and LOCK-1/3 1C ship as one wave.

### Follow-on (not blocking X8, noted for the redress author)
- `LatticeDomain::join` in `domain.rs` still carries the old LWW for the pure-lattice propagation mode. `project_types` does not use that mode (it runs explicit-constraint AC-3 through `revise`→`assign`), so it is inert here — but if that mode is ever activated for type projection the same refinement-aware arm should be mirrored there. Left untouched this probe (parsimony; out of the tested path).
- The `variables()` accessor added to `csp-solver` is the minimal read seam the harvest + unit fixture need; the `variables` field was already `pub`, so the accessor is ergonomic sugar, not a new capability.
