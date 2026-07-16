# LOCK-1 — THE SOLVER SPINE · pass-2 prior-art research

Problem class: **compile-time type inference & solving for a grammar-projection language** —
`->` projections, document shapes (records + variants), sums/products, numeric widths, decode
classes — inferred, solved, and **erased to zero runtime cost**, with grammar-span diagnostics.
No family allegiance; this is a sweep of what exists, what won and lost elsewhere for this exact
problem class, the constraints our own evidence base already imposes, and the traps named in
comparable systems.

---

## 0. The evidence base already binds four hard constraints

Before any web art: four constraints are non-negotiable, drawn from our certified swath and the
extant tranche baseline. Every family (1A/1B/1C/1D) is judged against them.

- **C1 — ZERO runtime cost is owner-verified LAW.** SPEC §0 #08 (CONFIRMED_DELTA):
  the type plane "derives validated `DirectSchemaSet` in ~230µs compile-time, ZERO runtime cost
  (held)." Constitution clause v ("the type plane costs zero at runtime — verified invariant") and
  GN-3 §2 owner gate re-prove it each wave via `git diff crates/runtime/` = empty
  (`docs/tranches/sk-v24/GN-3.md:139-150`). Any solver mechanism that leaks an instruction onto the
  parse path is refuted at the seam. The solver is a **generator-time** artifact only.
- **C2 — the proven core is union-find HM, 221 LOC.** PROVE-08 built and measured it
  (`prototypes/PROVE-08/REPORT.md:16-40`): path-halving `find`, constructor-binding `unify`, a
  closed leaf vocabulary `TyCon{Bool,F64,U8Tag,StringDecoded,StringSpan}`, `classify_arrow`
  (ascription checks / bare synthesizes — already a bidirectional seam), `infer` minting one var per
  rule. VERIFY certified it perf-neutral. This is Algorithm J, and it is the *floor*, not the ceiling.
- **C3 — GN-3 scopes CSP and e-graph OUT of the type plane.**
  `GN-3.md:106-110`: "CSP and e-graph machinery are out of scope (Y.13 / §8, parsimony §1.5). Leaf
  and sum unification is a decidable Hindley–Milner problem solved by the proven 221-LOC union-find
  core; no CSP or e-graph consumer exists in this tranche, so no such substrate lands." This is the
  sharpest constraint on families **1B (one-constraint-store)** and **1D (types-as-eclasses)**: the
  constitution's clause viii (*consumer-less substrate is forbidden, Y.13*) means those families are
  admissible **only** if they exhibit a genuinely undecidable/optimization sub-problem — widths,
  overloads — as an in-wave consumer. Absent that, they are substrate without a caller.
- **C4 — the three walls are the system floor, not the system.** GN-3 §1.2: Wall-1 (information —
  document shapes must be *declared* in the grammar, not recovered from a generic `json.bbnf`),
  Wall-2 (IR expressiveness — need a sum node), Wall-3 (front-end — thread `ExprKind::Project`
  through infer). LOCK-1's task statement says explicitly "GN-3's three walls are the floor, not the
  system" — the lock must design the *full* type system (row-polymorphic document shapes, widths,
  overloads, decode classes), of which GN-3 lands the leaf+sum slice.

### 0.1 A measured refutation from our OWN tree — the CSP-as-theatre trap

`skinny/crates/passes/src/decision_csp.rs` is a live counterexample the lock must not repeat. It
builds a **single-variable** `Csp<CostFiniteDomain>` whose domain is `0..N` candidate indices, with
cost `0.0` assigned to the *already-selected* index and `1.0` to all others (`:43-50`), then
"solves" it to recover that same index (`:112-117`). Five `LambdaConstraint`s (`:52-81`) merely
re-assert properties of the pre-chosen candidate. The solver cannot do anything but return the seed.
This is **consumer-less substrate realized** — a CSP whose "decision" is a decision already made
upstream by `backend_egraph::select`. It emits ~40 `*_status: "pass"` string fields as evidence
theatre (`:137-188`). LOCK-1's families 1B/1D are one careless step from reproducing exactly this:
a constraint store / e-graph that re-derives what union-find already decided. **The named trap:
a solver with no free variable it alone can bind is a status report, not a solver.**

---

## 1. Prior art — what exists, what won, what lost, for THIS problem class

### 1.1 The unification core: Algorithm W vs Algorithm J (union-find)

The settled adjudication in the literature and in practice: **Algorithm J (mutable union-find over
type-variable equivalence classes) won over Algorithm W (substitution-threading)** for real
implementations. Bernstein's "DHM two ways" [1] names the tradeoff precisely: W is side-effect-free
but forces "threading all the state through by hand"; J "mutates equivalence classes… making type
variables point to their resolved types," and the unification logic "becomes noticeably cleaner." It
runs "roughly O(n)… but there are pathological cases if you have tuple or record types." PROVE-08
already chose J (path-halving find). **Verdict: 1A's HM-first premise is the empirically-won core;
this is settled, not a live design question.**

Named implementation pitfalls [1] that bind 1A directly:
- **`.find()` before generalization** — you must fully resolve a var's forwarding chain before
  collecting free vars, "or you might just see `'t0` as your only free type variable" and generalize
  the wrong ones.
- **let vs letrec** — recursive rules (grammar is *deeply* recursive: `value → object → pair →
  value`) demand pre-binding the rule name to "tie the knot" before inferring its body. A grammar
  type plane is essentially a mutually-recursive `letrec` block over the whole rule set.
- **error-message canonicalization** — raw `'t123467` confuses; canonicalize to letter sequences.
  This is 1C's diagnostics concern, seeded already.

### 1.2 Document shapes: row polymorphism is the settled mechanism (records AND variants)

The task names "row polymorphism for document shapes" (1A) and "sums and products" (task
statement). The literature verdict is strong and directly transferable:

- Row polymorphism (Wand; Rémy; Leijen's scoped labels [2,3]) is the **settled** way to type
  extensible records *and* extensible variants. The Neubauer/executable-spec work [2] states it
  "adds little complexity to the classic Hindley–Milner algorithm by only affecting **unification**
  and introduc[ing] a basic **kind system**. Rows are **order-irrelevant**." Wand introduced row
  variables specifically to type objects as records-and-variants.
- **This directly refutes GN-3's ad-hoc sum plan's weakest seam.** GN-3 W1 plans a bespoke
  `Ty::Sum(Vec<Ty>)` with "set-equal (order-insensitive) unification" plus a hand-rolled "canonical
  variant ordering in `sum.rs`" (`GN-3.md:170-186, R5:501-506`). Row-typed variants give
  order-insensitivity **for free** (rows are order-irrelevant *by construction*) and give the
  open/closed distinction (a row variable = "and possibly more variants") that document sub-schemas
  need — where GN-3's fixed `Vec<Ty>` cannot express "this value is object|array|… **or more**." The
  row/kind machinery is the principled home for both the JSON `value` sum **and** the CITM
  closed-enum variant tables (GN-3 R2, 17 StringEnum sites). **Recommendation for 1A: adopt
  row-polymorphic records+variants (Leijen scoped-labels or Rémy) as the shape representation,
  not an ad-hoc `Vec<Ty>` sum** — it is the same union-find unification with a label-row extension,
  it is what won everywhere (Elm, PureScript, Ur/Web, Koka's effect rows [7]), and it dissolves
  GN-3's R5 order-sensitivity risk architecturally rather than by a canonicalization patch.
- **Trap [2,3]:** rows need a **lacks/kind constraint** ("this row does not already contain label
  ℓ") or you get unsound duplicate-label record merges. This is a small kind system, not free.

### 1.3 Bidirectional typing: the won pragmatics for a language with *declared* shapes

Family 1C's premise. Dunfield–Krishnaswami "Bidirectional Typing" [4] and the practitioner
reconstructions [5,6] give the settled verdict: bidirectional typing (a **check** mode against a
known type + a **synthesize** mode that infers) "improves error **locality**" and is the mechanism
of record for languages that mix inference with **annotations**. The decisive fit for us: a `->`
projection *is a declared type annotation* — `... -> Vec<Tweet>` is exactly the top-down type the
checking mode propagates. PROVE-08's `classify_arrow` ("ascription `:T` **checks**, bare target
**synthesizes**", `REPORT.md:24-26`) is already a bidirectional discriminator. The DSL-specific
literature [8] ("Type Error Diagnosis for Embedded DSLs by Two-Stage Specialized Type Rules")
confirms the diagnostics dividend: **two staged phases of constraint gathering+solving localize
errors better than a single interleaved pass** — directly serving 1C's "diagnostics-first, every
failure names its grammar span." **Verdict: 1C is not an alternative to 1A — it is the *driver
discipline* over the 1A core.** Declared projections check; unannotated rule bodies synthesize;
global unification runs only at cross-rule seams. This is how TypeScript, Scala, Rust, and Agda all
actually work, and it is the honest reading of "GN-3's three walls are the floor."

### 1.4 Widths / overloads: the legitimate home for a *bounded* solver (1B's real consumer)

The task confines CSP to "where unification is undecidable (widths, overloads) as a bounded
post-solve" (1A) — and here the prior art is affirming and specific:

- **Bit-width inference is a real, formally-verified constraint problem.** FIRRTL width inference
  [9] "infer[s] the minimum (bit) widths for all components with unspecified widths while
  maintaining the legality of all incoming connections" — a **monotone least-fixed-point** over a
  width lattice (each width = max of its constraints), *not* backtracking search. Our own
  `csp-solver` advertises exactly this fit: "**Lattice domains for monotonic fixed-point
  propagation**" (`crates/csp-solver/src/lib.rs:8`). So numeric-width resolution (PROVE-08's
  `U8Tag→u32` promotion, R6 §5's decode-class widths) maps onto the csp-solver's **lattice/monotonic
  domain** (`domain/lattice.rs`, `solver/monotonic.rs`) — a min-solve, not a search.
- **Overload / decode-class resolution → refinement/SMT precedent.** Refinement-type systems [11]
  discharge range and width predicates to an SMT solver (Z3). This is the shape of "decode classes"
  (which leaf decoder — span vs escaped vs numeric-scalar — is legal under the projected type).
- **The honest boundary:** width/overload solving is where 1B earns a consumer *distinct from
  unification*. But note the csp-solver's **backtracking-search** face (AC-3, forward-checking,
  GAC-all-different — `solver/{ac3,search,gac}.rs`) is for combinatorial CSPs; width inference wants
  the **lattice-monotonic** face. Using the search engine for a monotone fixpoint would be the
  decision_csp.rs mistake in a new costume. **1B is admissible only as: unification = union-find;
  widths/overloads = the lattice-monotonic sub-solver; both reporting into one store — NOT a single
  backtracking CSP swallowing unification.**

### 1.5 e-graphs for types (1D): congruence closure IS unification — but where is saturation's consumer?

- **The isomorphism is real.** An e-graph maintains a congruence-closed equivalence relation over
  terms [12,13]: "if the e-graph represents f(x), f(y) and x=y, it represents f(x)=f(y)." That *is*
  unification's congruence rule. Haskell's `hegg` [12] and egg exist. So 1D "unification is
  congruence closure" is theoretically sound.
- **But the trap is decisive.** HM unification is already **near-linear union-find**; congruence
  closure via an e-graph buys nothing *unless there is non-confluent rewriting to saturate* — i.e.
  the type lattice has genuine **rewrite rules** with a **cost function** whose extraction picks a
  representative (1D: "projections canonicalize by saturation; extraction picks by cost"). The
  consumer question (C3, clause viii) is unforgiving: **what type-level rewrite is non-confluent
  enough to need saturation, and what cost model extracts?** If the answer is "canonicalize the
  variant order of a sum," row polymorphism (§1.2) already does that for free with no e-graph. 1D
  must name a real saturating rewrite (e.g. width-lattice joins ⊗ decode-class subsumption ⊗
  projection normalization interacting non-confluently) or it collapses to "union-find with extra
  steps" — precisely the consumer-less substrate the constitution forbids. **1D shares LOCK-2's
  engine by charter; its burden is to prove the type plane is a *saturation* problem, not merely a
  *unification* problem.**

---

## 2. Traps named in comparable systems (the hazard list)

1. **Let-generalization exponential blowup.** DHM type-checking is DEXPTIME-complete; naive
   generalization copies type terms and "let polymorphism allows… an exponential number of calls…
   with different types" [14,15]. **Mitigation (okmij [15]):** *level/rank-based* generalization —
   never scan the type environment for free vars; tag vars with binding levels and generalize by
   rank. For a grammar plane the rule set is finite and top-level, so the pathology is unlikely to
   *fire*, but 1A's "let-generalization over rules" MUST use the level trick, not env-scan, or a
   large grammar (CSS, the CITM 31-field instrument) pays quadratically.
2. **The value restriction** [16] — only bites with mutable refs / effects. Grammar projections are
   pure, so generalization is sound without the restriction; note it and move on (do not import an
   unneeded restriction as cargo-cult).
3. **Row unification unsoundness without a kind/lacks system** [2,3] — duplicate labels merge
   silently. A basic kind system is mandatory with rows; it is small but non-optional.
4. **Union-find generalize-too-early** [1] — resolve forwarding chains fully before collecting free
   vars (§1.1).
5. **CSP/e-graph as theatre** — the decision_csp.rs realized trap (§0.1). A solver whose only
   variable is pinned to a pre-computed answer is a status report. Every constraint substrate must
   own a free variable no cheaper mechanism has already bound.
6. **Monotone fixpoint run on a search engine** — width inference is a lattice least-fixed-point
   (FIRRTL [9]); routing it through backtracking search (forward-checking/GAC) is over-engineering
   that will look like progress and deliver the identity function slowly. Use `solver/monotonic.rs`.
7. **Silent-epsilon on unify clash** — GN-3 §1.3 (§56) binds: a rule whose arrows disagree is an
   explicit `DeriveError::Unify`, never a fallback to a default leaf. The MEMORY law
   `no-silent-epsilon` reinforces: lowering catch-alls panic, never silently return a default.
8. **Diagnostics regression** — raw type-var names confuse [1]; single interleaved constraint
   passes localize errors worse than two staged passes [8]. 1C's "span-first" must canonicalize vars
   and stage gathering/solving.

---

## 3. Synthesis for the pass — how the families stand against the evidence

- **1A UNIFICATION-FIRST is the spine, and it is already partly proven (C2).** Its one substantive
  upgrade the literature demands: **make document shapes row-polymorphic records+variants** (§1.2),
  which subsumes GN-3's ad-hoc `Vec<Ty>` sum and dissolves R5 order-sensitivity by construction. Its
  hazards are the classic four (§2.1–2.4).
- **1C BIDIRECTIONAL-LOCAL is not a rival to 1A but its driver discipline** (§1.3) — declared `->`
  checks, bodies synthesize, global unify only at cross-rule seams; it is the won pragmatics for a
  DSL-with-annotations and the natural home of "diagnostics-first." The strongest reading of the
  lock is **1A⊕1C fused: a row-polymorphic HM core driven bidirectionally.**
- **1B ONE-CONSTRAINT-STORE is admissible only with a real undecidable consumer** — widths/overloads
  via the csp-solver's **lattice-monotonic** face (§1.4), *not* the backtracking face, and *not*
  swallowing unification into a single global CSP (that path leads to §0.1). Against C3, its "the
  csp-solver crate is the engine" claim must be scoped to the width/overload post-solve, or it is
  substrate without a caller.
- **1D TYPES-AS-ECLASSES carries the heaviest burden of proof** (§1.5): it must exhibit a genuinely
  *saturating*, non-confluent type-level rewrite with a cost-based extraction, or it is union-find in
  costume — and it inherits C3's scope-out explicitly. Its legitimate opening is the LOCK-2 shared
  engine + a type lattice where widths ⊗ decode-classes ⊗ projection-normalization interact
  non-confluently; that has to be *shown*, not asserted (constitution clause vii, concrete artifacts).

**The through-line:** the problem class is a *unification* problem (won by union-find HM + rows +
bidirectional driving), with a small, bounded *lattice-fixpoint* sub-problem (widths/decode-classes)
that is the only honest home for a "solver" beyond unification — and any *search* or *saturation*
engine must first defeat the consumer-less-substrate test our own `decision_csp.rs` already failed.

---

## Citations

1. Bernstein, "Damas-Hindley-Milner inference two ways" (Algorithm W vs J/union-find; pitfalls) —
   https://bernsteinbear.com/blog/type-inference/
2. "An Executable Specification of Typing Rules for Extensible Records based on Row Polymorphism"
   (rows affect only unification + a basic kind system; order-irrelevant) — https://arxiv.org/abs/1707.07872
3. Leijen, scoped labels / "write-you-an-inference" row polymorphism (F#) —
   https://github.com/7sharp9/write-you-an-inference-in-fsharp/blob/master/Hindley-Milner-Mutable-Rowpolymorphism.md
   ; "Restrictable Variants" (ECOOP 2023, Boolean unification for variants) —
   https://drops.dagstuhl.de/storage/00lipics/lipics-vol263-ecoop2023/LIPIcs.ECOOP.2023.17/LIPIcs.ECOOP.2023.17.pdf
4. Dunfield & Krishnaswami, "Bidirectional Typing" (error locality; check/synthesize) —
   https://arxiv.org/pdf/1908.05839
5. Jaked, "Reconstructing TypeScript, part 1: bidirectional type checking" —
   https://jaked.org/blog/2021-09-15-Reconstructing-TypeScript-part-1
6. "Simple Bidirectional Type Inference" — https://ettolrach.com/blog/bidirectional_inference.html
7. Leijen, "Koka: Programming with Row Polymorphic Effect Types" (rows generalized to effects) —
   https://arxiv.org/pdf/1406.2061
8. "Type Error Diagnosis for Embedded DSLs by Two-Stage Specialized Type Rules" (staged
   gather+solve localizes DSL errors) — https://link.springer.com/chapter/10.1007/978-3-662-49498-1_26
9. "A Formally Verified Procedure for Width Inference in FIRRTL" (min-width monotone fixpoint) —
   https://arxiv.org/pdf/2601.12813
10. inferno — OCaml constraint-based HM (generator→constraint→solver decomposition) —
    https://ocaml.org/p/inferno/20220603/doc/inferno/Inferno/Solver/Make/index.html
11. "Local Refinement Typing" / refinement types discharge width+range predicates to SMT —
    https://arxiv.org/pdf/1706.08007
12. `hegg`: fast equality saturation in Haskell (congruence closure over expressions) —
    https://hackage.haskell.org/package/hegg
13. Zucker, "E-graphs" notes (congruence-closure invariant) —
    https://www.philipzucker.com/notes/Logic/egraphs/
14. Mairson / DHM complexity — let polymorphism is DEXPTIME (exponential blowup) — summarized in
    CMU 15-xxx type-inference notes: https://www.cs.cmu.edu/~janh/courses/ra19/assets/pdf/lect03.pdf
15. Oleg Kiselyov, "Efficient and Insightful Generalization" (level/rank generalization, no
    env-scan) — https://okmij.org/ftp/ML/generalization.html
16. "Subtype Inference by Example Part 11: The Value Restriction" —
    https://blog.polybdenum.com/2020/09/19/subtype-inference-by-example-part-11-the-value-restriction.html

## In-tree evidence cited

- `prototypes/PROVE-08/REPORT.md:16-40,99-130` — 221-LOC union-find HM core; the three walls.
- `docs/tranches/sk-v24/GN-3.md:106-110` (CSP/e-graph scoped out), `:139-150` (zero-runtime gate),
  `:170-186,501-506` (ad-hoc sum + R5 order-sensitivity), `:31-51` (three walls verifier-exact).
- `restart/skinny/tranches/sk-v24/SPEC.md:31` (§0 #08 CONFIRMED_DELTA, zero runtime cost held).
- `restart/skinny/tranches/sk-v24/locks/PORTFOLIO.md` CONSTITUTION clauses v, vii, viii.
- `research/R6-value-type-plane.md:144-214` — `->` discarded at `passes/src/lib.rs:204`; 4-way
  `ProjectionKind` fold is the entire type expressiveness reaching emission today.
- `skinny/crates/passes/src/decision_csp.rs:29-117` — the realized CSP-as-theatre trap.
- `crates/csp-solver/src/lib.rs:8` (lattice/monotonic fixpoint domain), `solver/{monotonic,ac3,
  search,gac}.rs`, `domain/lattice.rs` — the two engine faces; width inference wants the monotone one.
