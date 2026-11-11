# LOCK-1 · Family 1B — ONE-CONSTRAINT-STORE (pass 1, research-B)

## The thesis, stated sharply

Every type-plane judgment — leaf equality, document shape, sum membership, numeric
width, container capacity, decode class, and the residual cost that breaks ties — is a
constraint over inference variables, solved by one engine under one propagation
discipline. Unification is not a separate algorithm that runs first; it is *equality
propagation inside the same store* — a lattice join on a type-variable domain, driven to
a fixed point by the same worklist that drives width and class constraints. The academic
name for this position is **HM(X)/CLP(X)**: Hindley–Milner type inference is constraint
logic programming solving, generating constraints from program text and discharging them
in a constraint domain X ([Sulzmann/Stuckey, "HM(X) type inference is CLP(X) solving"](https://www.academia.edu/18661687/HM_X_type_inference_is_CLP_X_solving);
[Pottier & Rémy, *The Essence of ML Type Inference*, the HM(X) constraint framework];
[OutsideIn(X), Vytiniotis/Peyton Jones/Schrijvers/Sulzmann](https://simon.peytonjones.org/outsideinx/)).
1B is the wager that the *engine* the workspace already ships — `crates/csp-solver` — is
the X, and that a second bespoke union-find HM core (family 1A / GN-3's route) is a
duplicate solver the substrate makes unnecessary.

The decisive fact for this family: **1B is not speculative — a substantial fraction is
already built and running in the root `crates/` tree** (a different lineage than
`skinny/`, which is retired). Type inference there IS a CSP over a lattice type domain on
the csp-solver substrate. The family's work is to name that architecture as the type
system of record, close its open judgment kinds (widths, capacities, decode classes,
sum-unification), and fold GN-3's HM concerns into it as additional domains/propagators
rather than a parallel core.

---

## 1. The substrate — what `crates/csp-solver` already provides

The vendored solver (`crates/csp-solver/`, tracked at csc411 `02965a56`, byte-diffed by
`scripts/sync-csp-solver-vendor.sh`) supplies exactly the pieces 1B needs, and no more:

- **`LatticeDomain`** (`src/domain/traits.rs:115-121`) — `bottom()` + `join(&mut self,
  &other) -> bool` returning an LLVM-style *Changed* flag. This is unification's carrier:
  a type variable is a lattice cell that only grows, `join` is one unification step, the
  bool is the convergence signal. Matches the codebase edict *changed-flag-convergence*
  (fixed-point loops use a Changed bool, content hash as debug-assert only).
- **`propagate_monotonic`** (`src/solver/monotonic.rs:21-46`) — the specialized
  fixed-point sweep for lattice domains: no adjacency graph, no undo log, no
  backtracking; sweep all constraints until none report `Changed`. This is the entire
  HM propagation loop, already written, already honest about the lattice `remove`-is-noop
  hazard that made a naïve `changed = true` hang the worklist forever
  (`src/constraint/traits.rs:138-153` documents the exact repair).
- **`Constraint<D>`** (`src/constraint/traits.rs:53-80`) — `scope() / check() / revise()`.
  A unification propagator implements `revise` as a join; `check` validates a full
  assignment. `revise_unary_default` / `revise_binary_default` (`:92-211`) give AC-3 arc
  consistency for free to any constraint that does not supply its own join-based `revise`.
- **`CostDomain` + branch-and-bound** (`src/domain/traits.rs:94-108`;
  `src/csp/solve.rs:117-133,278-288`) — `CostFiniteDomain` (`src/domain/cost_finite.rs`)
  with per-value costs, `min_cost` cached lower bound, `solve_optimized` /
  `solve_with_cost_eval`. This is where *widths* and *capacities* live: an under-determined
  numeric width (`u8|u16|u32|u64`) or a capacity plan is a finite cost domain, solved by
  bounded branch-and-bound *inside the same store*, not a post-hoc phase.
- **`ThreadSafe` vacuous marker** (`src/constraint/traits.rs:19-50`) — the csp-solver was
  *deliberately modified* so that `!Send` constraints holding an `Rc<RefCell<…>>`
  obligation sink compile under the default (non-`py`) build. The doc names the downstream
  consumer by name: bbnf-lang's `RefConstraint`. **The substrate has already been shaped
  to carry a type-inference obligation channel.** This is the strongest possible evidence
  that 1B is the intended architecture, not a proposal.

Substrate gap 1B must be honest about: **`Csp<D>` is monomorphic in one domain `D`**
(`src/config.rs`; every variable in the arena shares `D`). A literally-single
heterogeneous store holding type-terms *and* widths *and* classes requires either (a) a
sum domain `JudgmentValue { Type(TypeDesc) | Width(u8) | Class(DecodeClass) | … }` with
per-value dispatch in every propagator, or (b) a **federation**: one typed `Csp<D>` per
judgment lattice, sequenced by dependency, sharing the engine + the obligation sink + the
propagation discipline but not the variable array. The root tree runs (b) today. §5 makes
the case that (b) is the honest form of "one constraint store" for this family and (a) is
the prototype's decision to make — this is the sharpest open question.

---

## 2. The mechanism made concrete — it already exists in `crates/ir/src/passes/`

Family 1B's architecture is not a design to invent; it is a running subsystem to name,
complete, and adopt. The root tree's `crates/ir/src/passes/` implements type inference,
FIRST/FOLLOW sets, span eligibility, and dispatch — **each as a CSP on the csp-solver
substrate, each with its own lattice domain**:

### 2a. Type inference IS a lattice CSP — `crates/ir/src/passes/types/constraint/`
- **`domain.rs`** — `TypeDomain { solved: Option<TypeDesc> }` implements
  `csp_solver::LatticeDomain`. `bottom() = None` (unsolved type variable); `join` assigns
  the type when bottom, and *refines* on disagreement during convergence
  (`domain.rs` `join`, the `(Some(existing), Some(ty)) if existing != ty` arm). **This is
  unification-as-join, verbatim.** A type variable's domain grows from ⊥ to a ground
  `TypeDesc`; the fixed point is the principal type.
- **Constraint modules** (`types/constraint/{alt,seq,operators,reference,grounds,revise}.rs`)
  — one propagator family per grammar former: `alt.rs` (alternation → sum/join),
  `seq.rs` (sequence → product/tuple), `reference.rs` (rule `Ref` → the referent's type),
  `operators.rs` (`*`/`?` → `Vec`/`Option` cardinality), `grounds.rs` (leaf/literal
  ground types), `revise.rs` (the join site where heterogeneous alternations that cannot
  collapse to a homogeneous payload are lifted). Every `->` former is a constraint; the
  grammar's structure *is* the constraint graph.
- **`obligation.rs`** — `TypeObligation` (`:56`) `{ UnresolvedCompoundRef, HeterogeneousAltJoin }`
  + `ObligationSink` (`:181`, `Rc<RefCell<Vec<TypeObligation>>>`). **This is the
  diagnostics-first channel the constitution demands**: when the solver would silently
  collapse an under-determined join into `BoxedEnum`, it instead *records a named
  obligation carrying the branch types and the grammar node id* (`NodeId`/`RuleId`), and
  `HeterogeneousAltJoin(branches)` becomes a real `TypeDesc` variant that lowers to
  `Box<Enum>` (`crates/ir/src/types/type_desc.rs:209`). Diagnostics are not bolted on;
  they are the solver's fail-explicit output, drained once at convergence
  (`obligation.rs` `drain`). This directly answers the constitution's *no-silent-epsilon*
  and *diagnostics-first* clauses.
- Consumers proving the substrate is not consumer-less (Y.13): `GrammarIR::type_obligations`
  (`crates/ir/src/types/grammar.rs:428`) is read by the Rust backend
  (`crates/core/src/backend/rust/ir_types.rs:343`), the TS backend
  (`crates/core/src/backend/ts/projection.rs:37`), and asserted end-to-end
  (`crates/core/tests/types_heterogeneous_alt.rs`, `crates/ir/tests/lattices/types.rs:598`).

### 2b. FIRST/FOLLOW & eligibility on the SAME substrate — `crates/ir/src/passes/csp_domains.rs`
- `CharSetDomain` — a monotone `CharSet128` lattice (FIRST/FOLLOW as growing byte sets),
  `join = union`. `BoolDomain` — top-down span-eligibility lattice (`Some(true)` refinable
  to `Some(false)`). Both implement `csp_solver::{Domain, LatticeDomain}` with matching
  `Constraint<D>` families. `crates/ir/src/passes/sets/dispatch/domain.rs` adds the
  dispatch-set domain. So four distinct analyses (types, FIRST/FOLLOW, eligibility,
  dispatch) already run on one engine — this *is* the "one constraint system, many
  judgments" claim, realized.

### 2c. Decision/backend/inline also on the substrate (the LOCK-2 neighborhood)
- `skinny/crates/passes/src/decision_csp.rs` and the root `crates/core/src/backend/`
  analyses (`rust/analysis/inline/constraints.rs` — `InlineDomain: LatticeDomain`;
  `backend/strategy/alt_strategy.rs` — "per-Alt decision produced by a real
  `csp_solver::Csp`") and `crates/egraph/src/csp_scheduler.rs` (`DirtyDomain`, "same
  LatticeDomain shape as `passes/csp_domains.rs`"). These are LOCK-2's territory but they
  matter to 1B because they *prove the pattern generalizes*: the cost-tie-break dimension
  of 1B's charter ("every judgment … cost — is one constraint system") already runs as
  `Csp<CostFiniteDomain>` with `OptimizationMode::MinimizeCost`, forward-checking, and a
  node budget (`decision_csp.rs:85-98`).

**Net mechanism**: constraint *generation* is a walk of the grammar IR emitting one
propagator per former into a `Csp<TypeDomain>` (equality/shape/sum), with numeric-width
and capacity variables in a coupled `Csp<CostFiniteDomain>` (bounded optimization), and
decode-class variables in a finite domain; `propagate_monotonic` drives the lattice
judgments to a fixed point; `solve_optimized` discharges the residual optimization
judgments; the `ObligationSink` carries every under-determination out as a
grammar-spanned diagnostic. Erasure to zero runtime cost is structural: the whole thing
runs in the generator (compile time), emits a schema, and adds no runtime instruction —
the same zero-cost invariant GN-3/PROVE-08 certified (VERIFY attack 2, HELD).

---

## 3. Integration seams (path:line)

| Seam | Location | Role for 1B |
|---|---|---|
| Lattice-join unification cell | `crates/ir/src/passes/types/constraint/domain.rs` (`TypeDomain::join`) | The unification primitive; grow it with the sum/width arms (§4). |
| Per-former propagators | `crates/ir/src/passes/types/constraint/{alt,seq,operators,reference,grounds,revise}.rs` | The constraint-generation surface; add width/capacity/decode-class propagators here. |
| Named-obligation channel | `crates/ir/src/passes/types/obligation.rs:56,181` | Diagnostics-first output; every new under-determination becomes a `TypeObligation` variant. |
| The type lattice value | `crates/ir/src/types/type_desc.rs:209` (`HeterogeneousAltJoin`, `BoxedEnum`) | Where `TypeDesc::Sum`/width-refined leaves land. |
| Obligation consumers | `crates/ir/src/types/grammar.rs:428`; `crates/core/src/backend/rust/ir_types.rs:343`; `crates/core/src/backend/ts/projection.rs:37` | Y.13 same-wave consumers; keep them fed. |
| Engine — monotonic sweep | `crates/csp-solver/src/solver/monotonic.rs:21` | The HM fixed-point loop; unchanged. |
| Engine — bounded opt | `crates/csp-solver/src/csp/solve.rs:282` (`solve_optimized`) + `src/domain/cost_finite.rs` | Widths/capacities as branch-and-bound in-store. |
| `!Send` sink accommodation | `crates/csp-solver/src/constraint/traits.rs:19-50` (`ThreadSafe`) | Already shaped for the obligation-sink constraint; do not regress. |
| GN-3 skinny discard sites (the *other* lineage) | `skinny/crates/passes/src/lib.rs:204` (`Project` type dropped), `:1349` (`ValueProject` lower), `:1136-1391` (`DirectBuildField.target = None`) | If the greenfield ports skinny's emit spine, these are the sites 1B *replaces* wholesale with the CSP inference above, rather than threading a bespoke HM (1A/GN-3 W2). |
| Direct-to-struct schema IR | `skinny/crates/codegen/src/direct_schema.rs:52-58` (`DirectTypeKind::Struct` only) | Wall 2: add `Sum`; the CSP's `HeterogeneousAltJoin` already carries the branch list to populate it. |
| Hand schema to delete | `skinny/xtask/src/real_typed_schema.rs:7` (1014 LOC) | Wall 1: the CSP-derived `DirectSchemaSet` replaces it (shared target with GN-3 W3). |

The greenfield port question is decisive: the SPEC retires `skinny/` and ports "proven
organs." **The root `crates/ir/src/passes/types/constraint/` CSP inference is itself a
proven organ** — 1B's central recommendation is that the greenfield adopts *that* as the
type plane and does not resurrect skinny's `Infer` fold (`skinny/…/lib.rs:149-233`, the
monotone structural fold R6 §5 documents as the thing to kill) nor stand up GN-3's second
union-find HM core.

---

## 4. What 1B must build on top of the extant CSP (the open judgment kinds)

The extant CSP resolves equality/shape/sum-membership. The charter's remaining judgments
are net-new *domains and propagators in the same store*, each with a named consumer:

1. **Sum unification** — `alt.rs`/`revise.rs` already lift N heterogeneous branches to
   `HeterogeneousAltJoin`. 1B strengthens the join to be **set-equal / order-insensitive**
   (`object|array|string|number|bool|null` unifies to the same canonical sum regardless of
   author order — the GN-3 R5 counterexample) by canonicalizing the branch list
   (`obligation.rs::dedup_branch_types` is the seed) and adding a `TypeDesc::Sum` value the
   schema IR (`DirectTypeKind::Sum`, Wall 2) consumes. Prior art for principled sums under
   unification: [Algebraic Subtyping / MLsub biunification, Dolan] and row-polymorphism
   for the document-shape variant (open vs closed rows for objects with runtime keys).
2. **Numeric width** — a `CostFiniteDomain` variable per width-polymorphic leaf
   (`u8|u16|u32|u64|usize`), constrained by the literal range observed at the `->` site
   and by the capacity plan; `solve_optimized` picks the minimal-cost legal width. This is
   the "CSP appears where unification is undecidable (widths)" seam — but 1B keeps it *in
   the same store*, coupled to the type solution, not a detached post-solve.
3. **Container capacity** — a cost variable carrying the `Vec::with_capacity` hint; GN-3
   R1 proves these are *corpus facts*, not grammar facts (`875`/`184`/`10_800`), so 1B's
   honest position is a colocated grammar annotation `-> Vec<Tweet> @cap(875)` that seeds
   the domain, and structural-modulo-capacity equality when the annotation is absent.
4. **Decode class** — a finite domain `{ StringSpan, StringDecoded, F64, U8Tag, … }`
   (PROVE-08's `TyCon` leaf vocabulary is the seed) resolved by a propagator reading the
   `SpanKind` of the projected regex (`Escaped → StringDecoded`, `Number → F64`).
5. **Cost tie-break** — the objective that disambiguates when a judgment is genuinely
   ambiguous; `OptimizationMode::MinimizeCost` over the coupled cost variables, exactly as
   `decision_csp.rs` already does for backend shape.

Each rides the substrate already in the tree. **No new solver, no e-graph, no second HM
core.** (Per constitution parsimony and the GN-3 §1.5 note that CSP/e-graph "full
inference hybrids remain a design input without a home" — 1B is precisely the family that
gives the CSP half a home, but must stay inside csp-solver, adding *domains*, not engines.)

---

## 5. Named risks and counterexamples

- **R-B1 — the monomorphic-`Csp<D>` wall (the sharpest).** One literal heterogeneous
  store is impossible without a sum `JudgmentValue` domain + runtime value-kind dispatch
  in every propagator, which fights Rust's type system and costs a tag branch per revise.
  The federated form (one `Csp<TypeDomain>`, one `Csp<CostFiniteDomain>`, one
  `Csp<DecodeClassDomain>`, sequenced) is what the tree runs and is clean — but "one
  store" then means *one engine + one obligation channel + one propagation discipline*,
  not one variable arena. **Counterexample that forces the decision:** a decode-class that
  *depends on which sum variant was chosen* (string-vs-number payload) is a genuine
  cross-judgment constraint — in the federated form it must be sequenced (types solved to
  ground, then classes read the solution as given facts, cf. `solve_with_given`,
  `solve.rs:139`); in the single-store form it is one propagator. The prototype must pick
  and measure. 1B's recommendation: **federated with typed dependency edges**, because
  the coupling depth is shallow (types → widths/classes → cost) and `solve_with_given`
  already threads solved facts between stores.
- **R-B2 — lattice refinement vs monotonicity.** `TypeDomain::join` currently *allows*
  re-assignment to a different type during convergence ("the solver may refine"). That is
  not a lattice join (not monotone) and can non-terminate or produce order-dependent
  results if a real disagreement (an honest unification clash) is silently overwritten
  instead of raised. **Counterexample:** two `->` arrows on one rule declaring
  incompatible leaves must produce a `DeriveError::Unify`-style obligation, not a silent
  last-writer-wins. 1B must make the disagreement arm *raise an obligation* (fail-explicit,
  per `no-silent-epsilon`), and reserve refinement for genuine bottom→ground steps only.
- **R-B3 — engine coupling / vendor gate.** csp-solver is vendored from csc411 and
  byte-diffed (`--check`). 1B needs new *domains* (fine — downstream crate) but if it needs
  new *engine* features (e.g. a heterogeneous-store search or a custom `DomainCostEval`),
  those changes must originate upstream in csc411 or they break the sync gate. The
  `ThreadSafe` episode is the template: the engine was changed upstream to admit bbnf's
  `!Send` sink. **Mitigation:** keep every 1B addition on the *consumer* side
  (`crates/ir/src/passes/`), using only the public `LatticeDomain`/`Constraint`/`CostDomain`
  traits; if an engine change is unavoidable, it is a csc411 tranche, not a bbnf hand-patch.
- **R-B4 — is CSP *worth it* over union-find for equality alone?** For pure HM equality,
  union-find (1A/PROVE-08's 221 LOC) is asymptotically better (near-linear vs the sweep's
  worklist re-scans). 1B's win is *not* faster equality; it is that widths, capacities,
  classes, cost, FIRST/FOLLOW, and dispatch are already one system, so type inference joins
  a mature substrate instead of adding a fifth bespoke solver. **The refutation to guard
  against:** if the prototype shows equality-only inference dominates the judgment mix and
  the other kinds are rare, 1B collapses toward 1A and should say so. The measurement in §6
  is designed to expose exactly this.
- **R-B5 — zero-runtime-cost must be re-proven, not assumed.** Like GN-3, every 1B wave
  must gate `git diff --name-only <base>..HEAD | grep crates/runtime/` empty. The CSP runs
  in the generator; any leak of a solver call onto the parse path refutes the family.

---

## 6. What a prototype must test to move this family

The pass-1 prototype is a **head-to-head on one grammar that exercises the full judgment
mix** (JSON leaves + a width-polymorphic numeric + a heterogeneous sum + a
capacity-hinted `Vec` + a decode-class split), measuring:

1. **Reachability (the primary gate).** Drive `crates/ir/src/passes/types/constraint/`
   over a typed grammar and prove the CSP derives a `DirectSchemaSet` structurally equal to
   PROVE-08's derived leaves *plus* a `TypeDesc::Sum` for the `value` alternation, with
   every under-determination surfaced as a `TypeObligation` (no silent `BoxedEnum`). Gate:
   the six-arm alternation yields one canonical order-insensitive sum (R5), and two
   incompatible arrows on one rule yield a clash obligation, not last-writer-wins (R-B2).
2. **The store-shape decision (R-B1).** Build the decode-class-depends-on-sum-variant
   counterexample and implement it *both* ways — federated-with-`solve_with_given` and a
   single sum-domain store — and report which is simpler and whether either measurably
   costs generation time. This is the decision the family cannot make on paper.
3. **Judgment-mix census (R-B4).** On the real corpus grammars (json/css/sheets/twitter),
   count constraints by kind (equality vs width vs capacity vs class vs cost). If equality
   is >95% and the rest are singletons, 1B converges toward 1A and must declare it; if the
   mix is rich (CSS `value = ident|length|percentage|color|…` peaks ~6-10 branches per
   `obligation.rs` dedup comment), 1B's one-store thesis is vindicated.
4. **Zero-runtime-cost (R-B5).** `crates/runtime/` diff empty; derivation wall-time
   in the hundreds-of-µs band PROVE-08/VERIFY established (167→231-241 µs, load-dependent —
   report the direction, not the magnitude).
5. **Diagnostics quality.** Every failure names its grammar span
   (`NodeId`/`RuleId` → source offset) — the constitution's diagnostics-first clause and
   1C's overlap; verify the obligation stream is human-legible (`TypeObligation::diagnostic`).

Success = the CSP inference derives the schema with the sum node, the obligation channel
carries every under-determination with a span, the store-shape decision is made with a
measured reason, and the runtime binary is untouched. Failure that still moves the family:
a judgment-mix census showing equality dominance would honestly hand the win to 1A and is a
real pass-1 result, not a null one.

---

## 7. Prior art, cited

- **HM(X) = CLP(X):** type inference is constraint solving over a parametric domain X —
  [Sulzmann/Stuckey, "HM(X) type inference is CLP(X) solving"](https://www.academia.edu/18661687/HM_X_type_inference_is_CLP_X_solving);
  the constraint framework in Pottier & Rémy, *The Essence of ML Type Inference*. This *is*
  1B's thesis; the csp-solver is the X.
- **OutsideIn(X):** GHC's constraint generator/solver split — generate constraints from the
  program, discharge in a domain-specific solver, with implication constraints layered on
  ([Vytiniotis/Peyton Jones/Schrijvers/Sulzmann](https://simon.peytonjones.org/outsideinx/)).
  The generation/solving separation is the shape 1B's constraint-generation walk should take.
- **Algebraic Subtyping / MLsub (Dolan):** biunification and lattice-structured types —
  the principled route to sums and to the "lattice cell that only grows" domain 1B uses for
  document-shape variants (open rows for runtime-keyed objects).
- **Rust's chalk / next-generation trait solver:** canonicalization + a fulfillment
  worklist that interleaves unification with obligation resolution — the industrial
  precedent for "unification is one obligation among many in one solving loop," and the
  template for 1B's `ObligationSink`.
- **Set-constraint-based type inference (Aiken–Wimmers):** inference as inclusion
  constraints solved to a fixed point — the lattice-CSP form the tree's `CharSetDomain`
  (FIRST/FOLLOW) already instantiates.
- **In-tree precedent (the load-bearing citation):** `crates/ir/src/passes/types/constraint/`
  (type inference as `Csp<TypeDomain>`), `crates/ir/src/passes/csp_domains.rs` (FIRST/FOLLOW
  + eligibility lattices), `crates/core/src/backend/**` + `crates/egraph/src/csp_scheduler.rs`
  (decision/inline/schedule CSPs) — the family is already the workspace's realized pattern
  for four analyses; 1B is its completion for the type plane, not its invention.
