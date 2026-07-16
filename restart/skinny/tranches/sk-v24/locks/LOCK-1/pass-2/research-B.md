# LOCK-1 · Family 1B — ONE-CONSTRAINT-STORE (research, pass 2)

Every type judgment — structural equality, numeric width, decode class, capacity,
overload legality, record-vs-list disjointness — is one constraint system. Unification is
equality propagation inside a global store; local propagators discharge the decidable
core; a bounded post-solve search resolves only the genuinely under-determined residue
(width defaulting). The engine is the vendored `csp-solver` crate. This family's claim is
that the store already exists, is running, and needs hardening + widening, not invention.

---

## 1. The mechanism is already built and running

The strongest form of 1B is not a proposal; it is a mature subsystem in the legacy
bbnf-lang tree that must be recognized, hardened, and widened. Type inference there is a
CSP over `csp-solver`:

- **Driver:** `crates/ir/src/passes/types/mod.rs:51` — `project_types(ir)`. Assembles a
  `Csp<TypeDomain>` (`system.csp`), one variable per type node, then
  `system.csp.propagate()` (`mod.rs:67`) drives AC-3/sweep to a monotone fixed point. No
  search: the domain is a lattice singleton, so propagation alone converges.
- **Domain:** `crates/ir/src/passes/types/constraint/domain.rs:12` — `TypeDomain { solved:
  Option<TypeDesc> }`, a `LatticeDomain` (bottom = `None`; `join` grounds bottom or
  re-refines). This is exactly `csp-solver`'s `LatticeDomain` contract
  (`crates/csp-solver/src/domain/traits.rs:115`, `join(&mut self,&Self)->bool`), the same
  monotone shape the FIRST/FOLLOW `BitsetLatticeDomain` uses
  (`crates/csp-solver/src/domain/lattice.rs:77`).
- **Propagators (local, per-node):** `constraint/reference.rs:54` `RefConstraint`,
  `constraint/{alt,seq,operators,grounds,revise}.rs` — each an
  `impl Constraint<TypeDomain>` with a hand-written `revise` that reads neighbour slots and
  grounds its own. `RefConstraint::revise` (`reference.rs:89`) inherits scalar target types
  and collapses compound targets to `BoxedEnum`.
- **Clash record (the raise-on-disagreement seam already present):**
  `crates/ir/src/passes/types/obligation.rs:182` — `ObligationSink(Rc<RefCell<Vec<TypeObligation>>>)`,
  a `!Send` reference sink. `csp-solver` was explicitly widened to admit it: the `ThreadSafe`
  marker (`crates/csp-solver/src/constraint/traits.rs:43-50`) is vacuous off the `py`
  feature precisely so this `Rc<RefCell>` constraint compiles. Two obligation classes exist:
  `UnresolvedCompoundRef` (`reference.rs:74`) and `HeterogeneousAltJoin` (`revise.rs:123`).
  Drained and published to `ir.type_obligations` at `mod.rs:473`.
- **Cycle break:** `mod.rs:72-127` grounds rule-to-rule cycles, records one obligation per
  Ref, then re-propagates — a bounded second pass, the shape a residue post-solve takes.
- **The LOCK-3 sink (XL-F):** the store's product is `ir.types: Vec<(RuleId, TypeDesc)>`,
  copied verbatim by `TypeAnalysis::from_ir` at
  `crates/core/src/backend/driver/analysis.rs:22-31`, and the second `from_ir` (Rust named
  types) at `analysis.rs:162`. **The TS mirror** already consumes the same `TypeDesc`:
  `type_desc_to_ts` at `crates/core/src/backend/ts/projection.rs:11`. Any record this family
  produces lowers through both with no new seam if it stays a `TypeDesc`.

**Consequence.** 1B's thesis ("unification is equality propagation inside the global
store; local propagators + global search") is *already the running architecture* for
structural equality. The Thread-B baseline is this pass, not a strawman. What is missing is
(a) the other four judgment classes are not modelled, and (b) the disagreement policy is
last-writer-silent, not clash-recording. Both are additive.

---

## 2. The four judgment classes the store does not yet carry

The store today ranges over one axis: structural `TypeDesc`. The 1B claim requires the
same variable to carry a **product lattice** over disjoint axes, each with its own join:

| axis | today | 1B form | join semantics |
|---|---|---|---|
| structural equality | `TypeDomain.solved: Option<TypeDesc>` | unchanged | ground-or-refine |
| **numeric width** | `TypeDesc::{U8,U16,U32,I8,…}` exist (`ts/projection.rs:20-23`) but no defaulting/lattice | ordered width lattice `⊥ ⊑ u8 ⊑ u16 ⊑ u32 ⊑ u64` | `join = max` (widen-on-disagreement) |
| **decode class** | folded to `ProjectionKind{Float,Bool,Tag,Span}` in skinny (`emit/general_descent/view.rs:20-46`) — 4-way, lossy | small enum lattice `{RawSpan ⊏ Decoded, Scalar-materialize}` | least-upper-bound |
| **overload / legality** | none — every arrow target assumed legal | `BitsetDomain` of admissible projection targets per leaf, pruned by legality propagators | set intersection (AC-3 prune) |
| **List ≠ Row** (Open R) | `Type::List` vs record are both structural; no `Row`/`Sum` node | add `TypeDesc::{Row,Sum}` + a structural-kind cell; disjointness via `NotEqual` | tag inequality |

The product lattice is the crux: numeric width is a **total order**, so its `join = max` is
the *principled* version of the ad-hoc `domain.rs:70` overwrite ("re-assignment to a
different type: allow it"). Width disagreement is not a clash — it is a widening, which is
exactly why `csp-solver`'s monotone lattice fits. Decode class and legality are small
lattices; overload legality is a `BitsetDomain` (`crates/csp-solver/src/domain/bitset.rs`)
pruned by `NotEqual`-style propagators (`constraint/not_equal.rs:20`, singleton-prune
`revise_impl`). Genuine under-determination survives on exactly one axis — width when a `->`
is legal at u8/u16/u32 and nothing forces the choice — and that is the *only* thing the
post-solve search touches.

---

## 3. Thread B, made concrete

### B1 — additive raise-on-disagreement hardening
The disagreement site is `TypeDomain::join` (`domain.rs:70`), `(Some(existing),Some(ty)) if
existing != ty => { self.solved = Some(ty); true }` — silent last-writer. **Harden:** split
by axis. On the width axis take `max` and continue (widening is lawful). On the structural
axis, refuse the overwrite, keep `existing`, and `sink.record(TypeObligation::ProjectionClash{
node, lhs: existing, rhs: ty })` — reusing the *existing* `ObligationSink` machinery, a new
enum arm beside `UnresolvedCompoundRef`/`HeterogeneousAltJoin`. This is purely additive: no
propagator signature changes, the drain at `mod.rs:473` already publishes it, and
`§56 no-silent-epsilon` is satisfied by construction. **Gate:** the heterogeneous-alt corpus
(the grammars that today produce `HeterogeneousAltJoin`) must still solve — i.e. the store
must terminate and produce the same `ir.types` byte-for-byte, with the clash surfaced as an
obligation rather than a silent overwrite. Prototype must show `git diff` over emitted output
empty and the obligation stream non-empty on the clash fixture.

### B2 — head-to-head vs Thread A union-find re-seed
Thread A proposes re-seeding equality from a union-find HM core (PROVE-08's 221-LOC
`hm.rs`). The measured comparison is: hardened lattice store (this pass, +clash arm) vs a
union-find fold, on JSON / Sheets / CSS L4, three axes — derive wall-time, LOC-deleted,
runtime-diff (must be empty, §v zero-runtime-cost). The store's structural advantage: it is
*already the running pass* wired to `TypeAnalysis::from_ir`; a union-find re-seed must
reconcile-with or displace it, paying integration LOC the store does not. The store's
liability: `join`'s "re-refine to a different type" (`domain.rs:74`) can oscillate if a
propagator is non-monotone — the union-find core cannot (find/union is confluent by
construction). The prototype must therefore prove **monotone convergence** of the hardened
store (the `Changed`-flag honesty in `revise_binary_default`,
`csp-solver/src/constraint/traits.rs:138-153`, is the mechanism that lets the worklist drain
on a lattice — the lattice-hang repair is directly relevant). Race condition: if the store
oscillates on a real corpus, Thread A wins that corpus; if it converges at equal-or-lower
wall-time, the store wins on integration cost.

### B3 — residue census on an instrument that models overload/legality/decode-class AND
`List ≠ Row`
The census instrument is a `Csp` whose variables carry the §2 product domain. Pre-register
the gates: (i) width defaulting fixture — a `->` legal at u8/u16/u32, undetermined —
resolves by ordered-lattice `max`-with-default, not search, UNLESS a legality bitset
constrains it (then it is a genuine residue: measure defaulting-lattice vs
`solve_optimized` search, Open X); (ii) decode-class fixture — raw-span vs decoded — LUB;
(iii) `List ≠ Row` — the store expresses it only once `TypeDesc::Row` exists; assert a
`NotEqual(kind_var(List), kind_var(Row))` prunes the joint domain. Report the per-corpus
judgment mix: how many nodes solve by pure lattice propagation (decidable core) vs how many
enter the residue post-solve. The honest expectation is that the residue is *small* — width
defaulting is the only genuinely search-shaped axis, matching GN-3's finding that HM
unification is decidable and CSP is bounded post-solve (LOCK-1 charter: "CSP appears only
where unification is undecidable — widths, overloads — as a bounded post-solve").

### B4 — decode-class-depends-on-sum-variant: single store vs per-revise tag dispatch
The counterexample: a `Sum` (`value = number | string`) where the `number` variant decodes
as f64-materialize and the `string` variant as raw-span-or-decoded — the decode class is a
**function of the sum discriminant**, not a property of the composite. Build it both ways:
(a) **single flat store cell** — one decode-class variable for the sum node; join across
variants collapses `{f64-materialize, span}` to their LUB and loses the per-variant
distinction — *unsound* (represents "the sum decodes as X" when there is no single X). (b)
**per-variant store shape** — the sum node's domain is a `Row`/vector of cells indexed by
discriminant, each variant carrying its own decode-class lattice; the store carries
`Sum[decode₀, decode₁, …]`. This is sound. The finding: **the store must be product-shaped
per sum variant** — a flat scalar cell cannot represent variant-dependent decode class. This
is the sharpest constraint on store shape and directly motivates `TypeDesc::Sum` carrying
per-variant payload cells (aligns with GN-3 W1's `DirectTypeKind::Sum { variants:
Vec<DirectSumVariant> }`, `skinny/crates/codegen/src/direct_schema.rs:52`).

---

## 4. Open R / X / S through the store lens

- **Open R (record-widening).** Add `TypeDesc::Row` (repeat-of-pairs → record) and
  `TypeDesc::Sum` beside the existing width variants. The store *derives* `Row` from an
  `object = "{" pair* "}"` body by a propagator that reads the repeat-of-pair structure —
  no `{ }`-vs-`[ ]` hint. Because `TypeAnalysis::from_ir` copies `ir.types` verbatim
  (`analysis.rs:24`) and the TS mirror already switches on `TypeDesc`
  (`ts/projection.rs:11`), `List ≠ Row` lowers through **both** LOCK-3 sinks with zero new
  seam — structural fidelity becomes orthogonal to engine choice. This closes the masked
  `json_templates` fallback (R6 §5c: the JSON taxonomy is invented by role-classification
  heuristics `passes/src/lib.rs:1403-1490`) by making record-ness a *derived store fact*, not
  a byte-literal gate. Prototype must show a repeat-of-pairs body inferred `Row` without a
  name/brace hint (mirrors Thread D's make-or-break, but here the classifier is a store
  propagator, not an e-class).
- **Open X (pluggable residue solver).** The residue seam is `csp-solver`'s own config swap,
  already demonstrated in `skinny/crates/passes/src/decision_csp.rs`: the decidable core runs
  `propagate()` (lattice, no search, `csp/solve.rs:17`); the width residue runs
  `solve_optimized(&SolveConfig)` over a `CostFiniteDomain` (`decision_csp.rs:48-98`) with a
  cost model (defaulting order as costs). Two swappable implementations behind one `Csp`:
  (i) ordered-lattice defaulting (join=max, deterministic), (ii) branch-and-bound search
  (`OptimizationMode::MinimizeCost`). Decide on the width micro-fixture *independently of the
  equality carrier* — the seam is the `SolveConfig`, not the domain. `decision_csp.rs` is the
  existing proof that this exact two-mode seam compiles and runs.
- **Open S (attribute-grammar cross-rule seam).** The store *is* an attribute grammar: rule
  type vars are synthesized attributes, Ref propagation is inherited attributes, and the
  cycle-break loop (`mod.rs:72-127`) is the circular-attribute termination lattice. The
  monotone `LatticeDomain` bound is the termination proof (bounded fixpoint = lattice height).
  SCC-generalize and federation-sequencing are re-expressions of the same worklist over the
  adjacency graph (`csp-solver` `Adjacency`, `config.rs:14,133`). This is whole-system, not a
  fifth engine — the store already discharges all three as one propagation discipline.

---

## 5. Integration seams (path:line)

- Store driver: `crates/ir/src/passes/types/mod.rs:51,67,473`.
- Lattice domain to widen into a product: `crates/ir/src/passes/types/constraint/domain.rs:12,70`.
- Clash site to harden (B1): `domain.rs:70` + new `TypeObligation` arm at `obligation.rs`.
- Reference propagator to extend for legality: `constraint/reference.rs:89`.
- Alt/Sum join (B4 store shape): `constraint/revise.rs:123` (`join_types`).
- Obligation sink (reused for all clash records): `obligation.rs:182`; `ThreadSafe` admittance
  `crates/csp-solver/src/constraint/traits.rs:43`.
- Engine surface: `Csp::{add_variable,add_constraint,finalize,propagate,solve_optimized}` at
  `crates/csp-solver/src/csp/{mod.rs:30,44,115; solve.rs:17,282}`; `NotEqual`
  `constraint/not_equal.rs:16`; `CostFiniteDomain`/`BitsetDomain` `domain/{cost_finite,bitset}.rs`.
- Residue two-mode template: `skinny/crates/passes/src/decision_csp.rs:48-98`.
- LOCK-3 sinks: `crates/core/src/backend/driver/analysis.rs:22,162`. TS mirror:
  `crates/core/src/backend/ts/projection.rs:11`.
- skinny measurement substrate (structural fold to beat): inference discard
  `skinny/crates/passes/src/lib.rs:204`; `Type` enum `lib.rs:235`; 4-way fold
  `skinny/crates/codegen/src/emit/general_descent/view.rs:20-46`; `DirectTypeKind::Sum` home
  `skinny/crates/codegen/src/direct_schema.rs:52`.

---

## 6. Prior art (citations)

- **Ordered-lattice widths as CSP domains** — abstract interpretation's interval/width
  lattices (Cousot & Cousot 1977); the widen operator is precisely `join = max` on a total
  order with a top to guarantee termination. rustc's integer-literal `{integer}` inference
  fallback (`i32` default after unification leaves the width free) is the canonical
  "defaulting after the decidable core" — LOCK-1's width residue is the same pattern.
- **Type inference as constraint solving** — Pottier & Rémy, *The Essence of ML Type
  Inference* (HM(X)): unification is equality-constraint solving; overloading/records are
  extra constraint forms over the same store — the exact 1B decomposition (equality
  propagation + bounded post-solve). Odersky/Sulzmann/Wehr HM(X) formalizes the pluggable-X
  seam (Open X).
- **Row polymorphism** — Rémy / Leijen (*scoped labels*): `Row` as a first-class type node
  with a lacks-constraint; motivates `TypeDesc::Row` deriving record-ness structurally
  (Open R) rather than by brace-matching.
- **Congruence-closure vs union-find** — the store's `TypeDomain` grounding is union-find on
  type vars; egg/egglog's congruence closure is the e-class alternative (sibling family 1D).
  The measured B2 race is exactly union-find-fold vs lattice-store on the same corpus.
- **AC-3 / GAC** — Régin 1994 (in-crate `all_different.rs`); Mackworth AC-3 is the store's
  propagation core (`csp-solver` `propagate`). MiniZinc/Z3 are the reference for
  "one store, many constraint kinds" — the 1B architecture in the large.
- **Attribute grammars** — Knuth 1968; circular-AG termination via lattice height (Farrow
  1986) grounds Open S's cycle-break-as-fixpoint claim.

---

## 7. Named risks and counterexamples

- **R1 — `join` non-monotonicity oscillation.** `domain.rs:74` allows re-refinement to a
  *different* type; a non-monotone propagator can loop. The `Changed`-flag honesty
  (`traits.rs:138-153`) is the only thing that drains the worklist. **Test:** a fixture that
  today re-refines must be shown to converge under the product lattice (each axis monotone),
  or B1's clash-refusal must make structural re-refinement impossible.
- **R2 — width defaulting is a corpus fact, not a grammar fact** (mirrors GN-3 R1 capacity).
  A `->` under-determined at u8/u16/u32 has no grammar-carried answer; defaulting picks the
  minimal legal width, which may diverge from a hand schema. **Test:** the width micro-fixture
  must be *genuinely* under-determined (no legality bitset forcing it) to measure defaulting
  vs search honestly.
- **R3 — flat-cell decode class is unsound on sums** (B4). Proven above: a single store cell
  cannot represent variant-dependent decode class. The counterexample must be built both ways;
  if the flat cell is used, the store lowers a wrong decode class and the parity oracle fails.
- **R4 — the store is the legacy tree, not skinny.** Skinny@HEAD (the measurement substrate)
  carries a *monotone structural fold* (`passes/src/lib.rs:178`), NOT this CSP store. The
  design targets greenfield structure; the port scopes organ-level (`project_types` + the
  `types/` directory-module + `csp-solver` dep), never tree-level. B2's LOC-deleted must count
  the skinny fold this store *replaces*, not the legacy store it *is*.
- **R5 — `TypeDesc::Row`/`Sum` must lower through both `from_ir` sinks unchanged.** Adding
  variants to `TypeDesc` touches `type_desc_to_ts` (`ts/projection.rs:11`) and every match on
  `TypeDesc` — the risk is a non-exhaustive-match cascade. **Test:** the Rust and TS emitters
  must both round-trip the new variants (the isomorphism floor, Constitution iii).

---

## 8. Exactly what a prototype must test to move this family

1. **B1 hardening lands and the heterogeneous-alt corpus still solves** — clash recorded as
   obligation, `ir.types` byte-identical, `git diff` over emitted runtime empty. (Additive;
   reuses `ObligationSink`.)
2. **Product-domain census** — one `Csp` whose variable carries `(TypeDesc, width-lattice,
   decode-lattice, legality-bitset)`; report per-corpus decidable-core vs residue split on
   JSON / Sheets / CSS L4, with `List ≠ Row` expressed via `TypeDesc::Row` + `NotEqual`.
3. **B4 both ways** — flat cell (unsound) vs per-variant `Sum` cells (sound) on a
   decode-class-depends-on-discriminant fixture; report which parity-passes.
4. **Open X two-mode seam** — the *same* `Csp` solved by `propagate()` (lattice default) and
   by `solve_optimized(&SolveConfig)` (cost search) on the width micro-fixture; evidence is
   one seam, two swappable implementations, decided independently of the equality carrier.
5. **B2 head-to-head** — hardened store vs union-find re-seed on derive wall-time +
   LOC-deleted + runtime-diff; the store must prove monotone convergence to win integration
   cost.

The move that advances 1B: prove the store carries all five judgment axes in one product
lattice, that the residue is a single small width-defaulting seam, and that hardening the
disagreement policy costs zero runtime and zero emitted-output drift.
