# LOCK-1 — THE SOLVER SPINE — Prior-Art Research (pass 2)

Prior-art sweep for **the full type system of the grammar plane**: `->` projections, document
shapes, sums/products, capacities, numeric widths, decode classes — inferred, solved, erased to
zero runtime cost, grammar-sourced diagnostics. No family allegiance: the four charter routes
(1A UNIFICATION-FIRST, 1B ONE-CONSTRAINT-STORE, 1C BIDIRECTIONAL-LOCAL, 1D TYPES-AS-ECLASSES)
are judged against what has won and lost elsewhere for **this problem class**, and against the
constraints our own evidence base already imposes.

The problem class, stated precisely, is: *offline, compile-time-only, whole-program type
inference over a closed set of grammar rules, where the "terms" are EBNF rule bodies with `->`
ascriptions, the "types" are a lattice of (leaf decode classes ∪ products/structs ∪ N-ary sums
∪ Vec/Option cardinality ∪ numeric widths ∪ capacity hints), and the sole output is a
`DirectSchemaSet` consumed by an emitter.* It is a schema-derivation problem wearing a
type-inference hat. That framing controls every adjudication below.

---

## 0. What our own evidence base already fixes (binding constraints, not open questions)

These are settled by measured artifact or frozen law; any family that relitigates them is
out of bounds before web prior-art is even consulted.

1. **The HM core is proven and is the floor.** PROVE-08 built + measured a 221-LOC Hindley–Milner
   union-find core (`hm.rs`): path-halving `find`, constructor-binding `unify` with an honest
   clash error, closed leaf vocabulary `TyCon{Bool,F64,U8Tag,StringDecoded,StringSpan}`, a
   bidirectional leaf lexer `classify_arrow` (ascription `:T` *checks*, bare target
   *synthesizes*), `infer(arrows)` minting one var per rule and unifying every arrow into it.
   VERIFY = CONFIRMED_DELTA; census `arrows=5 / rules=4 / unify=1` and `schema_hash bd0ed1b6…`
   reproduce exactly (`PROVE-08/VERIFY.md`). **A design that discards this and starts from a
   different inference substrate must defeat a *working, certified* core — a high bar.**

2. **Zero runtime cost is owner-verified LAW, re-proven every wave.** The type plane is a schema
   *producer* in the generator; it emits no runtime instruction. The gate is literally
   `git diff --name-only <base>..HEAD | grep crates/runtime/` must be **empty** (GN-3 §3).
   Constitution clause (v): the type plane costs zero at runtime, verified invariant. Any
   family whose mechanism leaks into the parse path is refuted at the seam.

3. **The three walls are the spec, not obstacles to route around** (GN-3 §1.2, PROVE-08 §4):
   - Wall 1 (information): document shapes are *corpus facts* — field names, nesting,
     capacity hints `875/184/10_800/5_759` (12 sites, verified). Cure = the info moves **into
     the grammar** (`twitter_search.bbnf` `->` chains). No inference trick recovers it from a
     generic `json.bbnf`; that refutation stands scoped-and-superseded (GN-3 §1.5).
   - Wall 2 (IR expressiveness): `DirectTypeKind` has one variant, `Struct`. The grammar's
     natural output is a recursive **sum** (`value = object|array|string|number|bool|null`).
     Cure = add `DirectTypeKind::Sum`; grow `unify` over sums.
   - Wall 3 (front-end): `ExprKind::Project{body,target,ty}` now EXISTS at HEAD (`ir/src/lib.rs:261`,
     verifier-confirmed); the projected type is still discarded at `passes/src/lib.rs:204`
     (R6 §5a). Cure = thread `Project` through `infer` into `DirectBuild.type_ref`.

4. **CSP / e-graph substrate is scope-gated by Y.13 (consumer-less substrate forbidden).**
   GN-3 §1.5 is explicit: *leaf + sum unification is a decidable HM problem solved by the
   union-find core; no CSP or e-graph consumer exists in this tranche, so no such substrate
   lands.* This is the sharpest constraint on families 1B and 1D — they must **name a
   judgment that HM union-find genuinely cannot decide** (widths, overloads, capacity
   optimization) to earn their engine, or they violate Y.13 / §8.

5. **The engines already exist in-tree, with a live consumer precedent.** `crates/csp-solver`
   (AC-3, forward-checking, GAC-all-different Régin'94, branch-and-bound over `CostFiniteDomain`,
   **lattice domains for monotonic fixed-point** — `domain/lattice.rs`) and `crates/egraph`
   (+`egraph-derive` `Language` macro, `csp_scheduler.rs` an egraph↔CSP seam, `extract.rs`
   cost-model extraction). `passes/src/decision_csp.rs` already drives the CSP for *backend-shape
   selection* — one var, five constraints, `MinimizeCost`, node budget 10k, 1ms timeout. So 1B/1D
   are not greenfield-substrate proposals; the cost is *integration*, and the lattice domain is a
   ready-made home for a monotone type-set fixpoint (`lattice.rs` doc even names "types").

6. **`->` is a leaf tag today; types never compose** (R6 §5, verified). Inference throws the
   projection away (`passes:204`); the general-descent path folds every `->` to a lossy 4-way
   `ProjectionKind{Float,Bool,Tag,Span}` (`general_descent/view.rs:20-46`); struct shape is
   *invented* by heuristic role classification (`derive_materialization_roles`, byte-literal
   gates). The typed layer (`DirectSchemaSet`) is full-fidelity **but hand-authored**
   (`xtask/real_typed_schema.rs`, 1014 LOC). The lock's job is to make the grammar *be* the
   schema — a composition problem, which is exactly where naive leaf-tagging fails.

---

## 1. UNIFICATION-FIRST (1A) — the mainline, and what the literature says about growing it

**Charter:** grow the HM core — ADTs, row polymorphism for document shapes, let-generalization
over rules; CSP only where unification is undecidable (widths, overloads) as a bounded post-solve.

### 1A.1 Row polymorphism for document shapes — the decisive body of prior art

Our documents ARE records with named fields; `->` chains author record shapes. The canonical
technique is **row-polymorphic record inference**, with a clear winner for *inference-with-
principal-types*:

- **Rémy (1989) — presence/absence "field flags" + "lacks" predicates.** Row types keyed by
  labels; qualified types constrain row variables to lack certain fields, keeping labels unique
  while preserving principal types
  ([Row polymorphism, Wikipedia](https://en.wikipedia.org/wiki/Row_polymorphism)). Sound +
  complete inference. The cost: *qualified-type* machinery (predicate solving) bolted onto HM —
  already a "unification + a side constraint solver" shape, which is the seam 1B claims.
- **Leijen (2005) — Extensible records with *scoped labels*.** Lifts the uniqueness restriction,
  *allows duplicate labels*, and gives a **sound and complete unification algorithm** for row
  types with scoped labels — no lacks-predicates, no qualified types, pure unification
  ([Leijen, MSR PDF](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf);
  [tomprimozic/type-systems `extensible_rows`](https://github.com/tomprimozic/type-systems/tree/master/extensible_rows)).
  **This is the win for 1A:** it keeps row inference *inside unification* (no post-solve
  constraint domain) — exactly what UNIFICATION-FIRST wants and what Y.13 demands (no extra
  substrate). The `tomprimozic` repo is a compact OCaml reference impl — a direct template for
  growing `hm.rs`'s `unify` with a `Row`/`RowTail` constructor.

  **Trap named:** scoped labels permit duplicate fields, which for a *schema producer* (we emit
  a Rust struct — fields must be unique) is a foot-gun. Adjudication: our records are **closed**
  (a `twitter_search.bbnf` declares exactly its fields; no open-ended record extension at parse
  time). Closed records don't need full extensible-row machinery — a *product constructor over a
  fixed field list* suffices, with row-tail unification reserved for grammar-level struct
  reuse/inheritance. **Recommendation: adopt Leijen's unification discipline but restrict to
  closed rows; the row-tail is a growth axis, not a day-1 need.**

### 1A.2 ADTs / sums — the Wall-2 cure, and the order-sensitivity trap

`value = object|array|...` is a canonical **N-ary sum (variant/ADT)**. Sum unification in HM is
standard (each variant a constructor; `unify` recurses arm-wise). Two prior-art traps map onto
GN-3's named risks:

- **R5 sum order-sensitivity** (GN-3 §7): `object|array|string|number|bool|null` and any
  reordering must unify to the *same* canonical sum. Prior art: variant sets are unified
  **set-equal**, not positionally — the same discipline row-poly systems use for labels. `sum.rs`
  must canonicalize variant ordering (Leijen's rows are order-insensitive across distinct labels
  for exactly this reason). Solved problem; the risk is implementation, not theory.
- **R2 closed enums / variant tables** (GN-3 §7): CITM's fixed-key string enums (`"A"->A|"B"->B`)
  are sums-of-nullary-constructors. The sum node represents them natively; grammar length is not
  a gate (correct call — the variant set is a genuine document contract).

### 1A.3 let-generalization over rules — do we even need it?

The charter says "let-generalization over rules." **Prior-art caution:** full let-generalization
(∀-quantification at rule boundaries) buys *polymorphic rule reuse* — a rule used at two types.
Our rules are **monomorphic schema producers**: `json`'s `value` has one type, period.
Generalization earns its keep only if grammars share *generic* rules (a reusable
`list<T> = "[" T ("," T)* "]"` — which BBNF first-class closures / `GrammarCall` could
introduce). **Adjudication: let-gen is a *conditional* growth axis gated on parameterized grammar
rules; absent them it is consumer-less substrate (Y.13 violation) and should NOT land in the base
lock.** A concrete over-reach guard for 1A.

### 1A.4 The CSP post-solve seam — is it real, or Y.13 bait?

1A says "CSP appears only where unification is undecidable (widths, overloads) as a bounded
post-solve." The prior art on *numeric width / literal defaulting* is unambiguous that this is
**not undecidable and rarely needs a general CSP**:

- Swift/C#/Zig/Go/Odin resolve integer-literal width by **peer type resolution / defaulting
  rules** — pick the first of {i32,u32,i64,u64} that fits, or default to a canonical type
  ([Swift numerics](https://numerics.diploid.ca/integers-part-1.html);
  [Zig peer type resolution, DEV](https://dev.to/lerno/thoughts-on-numeric-literal-type-inference-rules-for-a-c-like-programming-language-4fpb)).
  Kotlin's `sumOf` Int-vs-Long ambiguity ([KT-46360](https://youtrack.jetbrains.com/issue/KT-46360))
  is the canonical *failure* case — resolved by **defaulting priority**, not search.
- **This is a decidable defaulting fold, not a CSP.** For our plane, widths come from the `->`
  ascription (`-> u8`, `-> f64`) — *declared*, not inferred-then-searched. The only true search
  would be *capacity optimization* — and GN-3 §7/R1 already rules that capacity is a **corpus
  fact carried as a grammar annotation** (`-> Vec<Tweet> @cap(875)`), NOT a solved quantity.

  **Sharpest adjudication of the whole lock:** *for the frozen GN-3 scope there is NO undecidable
  judgment. Widths are declared; capacities are annotated; sums/rows/products are decidable HM.
  The CSP post-solve seam is consumer-less at GN-3 and must not land there.* 1A's CSP clause is a
  **forward-looking design input** (GN-series), realizable only if a future grammar introduces
  genuine overload resolution or cost-optimized capacity — exactly how GN-3 §1.5 already frames
  "Full-inference CSP/e-graph hybrids remain a GN-series design input without a GN-3 home." **1A's
  unification-first spine is the mainline; its CSP tail is a banked future, not a base
  deliverable.**

**Verdict on 1A:** the strongest base route, the one our evidence already committed to. Prior art
hands it a proven recipe: HM union-find (have it) + closed-row product inference (Leijen
discipline, restricted) + N-ary set-equal sum unification. The named growth axes (scoped/extensible
rows, let-gen, CSP post-solve) are each Y.13-gated on a consumer that does not exist at GN-3 — the
research yields a *staging discipline*, not a mandate to build them now.

---

## 2. ONE-CONSTRAINT-STORE (1B) — the HM(X) / constraint-based tradition

**Charter:** every judgment (equality, width, capacity, decode class, cost) is one constraint
system; unification is equality propagation inside the global store; local propagators + global
search; the csp-solver crate is the engine.

### 2.1 The winning prior art: HM(X) / OutsideIn(X) — inference IS constraint solving

- **HM(X)** (Odersky/Sulzmann/Wehr): HM parameterized over a constraint domain X; inference
  generates constraints from program text, solved by a domain-specific solver. "HM(X) type
  inference is CLP(X) solving" — inference literally *is* constraint logic programming
  ([HM(X) is CLP(X), academia.edu](https://www.academia.edu/18661687/HM_X_type_inference_is_CLP_X_solving)).
- **OutsideIn(X)** (GHC's actual engine): parameterized over constraint domain X exactly as HM(X);
  generates constraints, adds **implication constraints** for local assumptions (GADTs, type
  families), solved by a solver for X; **the solver's properties guarantee principal types even
  when the type system alone does not**
  ([OutsideIn(X), MSR](https://www.microsoft.com/en-us/research/publication/outsideinx-modular-type-inference-with-local-assumptions/)).
- FreezeML gives a modern constraint-based reference architecture
  ([Constraint-based type inference for FreezeML, arXiv](https://arxiv.org/pdf/2207.09914)).

**The decisive lesson from GHC's own history:** OutsideIn(X)'s solver for X = {type classes +
GADTs + type families} was "a non-trivial challenge" (their words). The constraint-store
architecture *pays off precisely when you have implication constraints and local assumptions* —
GADT-style "in this branch, assume `a ~ Int`." **Our grammar plane has no local type assumptions,
no GADTs, no type-class dictionaries.** Every judgment is a whole-program equality over a closed
rule set.

### 2.2 The trap: constraint-store generality with no constraint to justify it

The **Y.13 / §8 crux for 1B**. The constraint-store's value is *heterogeneity* — unifying
equality + subsumption + implication + class/dictionary constraints into one solver. Our GN-3
judgments are: equality (→ union-find, already optimal), decode class (→ declared by `->`), width
(→ declared), capacity (→ annotated), cost (→ *no cost judgment* — the plane emits zero runtime
code, so nothing to cost). **The heterogeneity that justifies HM(X)/OutsideIn(X) is absent.**
Reducing pure equality to "the degenerate single-constraint case of a global CSP store" is
*architecturally honest* but *mechanically wasteful*: union-find is the asymptotically optimal
equality engine; wrapping it in `CostFiniteDomain` + AC-3 buys nothing and costs generation time.

**Named comparable-system trap:** the classic "everything is a constraint" over-generalization.
The `decision_csp.rs` precedent is instructive — it uses the CSP for a *genuine selection over
alternatives* (which backend shape, from a candidate set), a real search. Equality unification has
no candidate set to search; it has a most-general-unifier computed in near-linear time. **1B is
the right *narrative* (inference is constraint solving) but the wrong *mechanism* for GN-3's actual
judgments.** It becomes real the moment a judgment acquires a *choice* (capacity optimization,
overload defaulting, cost-model-fed layout selection) — exactly the GN-series futures 1A also
banks. **1B and 1A's CSP-tail converge on the same banked consumer.**

**Verdict on 1B:** correct as a unifying *principle*, premature as a *base mechanism*. Adopt it as
the **story governing the GN-series future** (when overloads/capacities/costs arrive, they land in
the one csp-solver store, not a bespoke second solver); explicitly NOT the GN-3 equality engine.
The lattice domain (`domain/lattice.rs`, monotone `join`, "types" named in its doc) is the correct
*home* for a future monotone type-set fixpoint and the concrete bridge from 1A's union-find to 1B's
store.

---

## 3. BIDIRECTIONAL-LOCAL (1C) — the diagnostics-first tradition

**Charter:** bidirectional checking, rule-local (synthesize/check against declared projections);
global solving only at cross-rule seams; diagnostics-first — every failure names its grammar span.

### 3.1 The prior art is settled and strongly favorable

- **Dunfield–Krishnaswami (2013), "Complete and Easy Bidirectional Typechecking."** The reference
  algorithm: terms **synthesize** a type or are **checked** against a known one; "remarkably simple
  and well-behaved, despite being both sound and complete"
  ([arXiv 1306.6032](https://arxiv.org/pdf/1306.6032);
  [ollef/Bidirectional Haskell impl](https://github.com/ollef/Bidirectional)).
- **"Bidirectional Typing," ACM Computing Surveys (2021)** states the exact benefits our lock
  needs: **scalability** (decidable even for very expressive systems, unlike Damas–Milner),
  **superior error reporting**, **ease of implementation**
  ([ACM CSUR 3450952](https://dl.acm.org/doi/10.1145/3450952)).

**The killer alignment:** our `->` projections *are declared types*. `-> Tweet`, `-> Vec<f64>`,
`-> u8` are precisely the **checking-mode annotations** that make bidirectional typing shine. A
rule body with an ascription is *checked against* it; a bare-target `->` *synthesizes*. PROVE-08's
`classify_arrow` **already implements exactly this split** (ascription `:T` checks, bare target
synthesizes — REPORT §1). So 1C is not a new architecture; it is *the discipline the proven core
already embodies*, made explicit and rule-local.

### 3.2 What 1C uniquely delivers that 1A/1B do not: diagnostics with spans

Constitution (vii) demands concrete artifacts and, for a type system, **grammar-sourced
diagnostics**. Bidirectional typing's decisive practical advantage over global constraint-solving
(per CSUR) is that **errors are local**: a check failure is reported *at the term being checked
against its expected type*, not as a downstream unification clash whose blame assignment is
notoriously poor in Damas–Milner/constraint systems. For a *grammar* type system where every
failure must "name its grammar span," this is the mechanism by which blame lands on the offending
`->` in `.bbnf` source. **A pure 1B constraint store produces the worst diagnostics** (the classic
"unification failed somewhere" blame-at-a-distance); 1C produces the best.

**Trap named (from the DK tradition):** bidirectional systems need **η-expansion / annotation
predictability** — the programmer must know where annotations are required. For us the annotations
are the `->` projections; the "predictability" question becomes *which rules must carry `->` for
the schema to be derivable*. This is Wall-1 restated (information must be declared), and 1C gives
the principled answer: a rule that is *checked* needs no annotation (its type flows from the
caller's expectation); a rule at a *synthesis* position (a root, or a sum arm) needs one. A
**concrete rule for minimal grammar annotation burden** — a real design product no other family
offers.

**Verdict on 1C:** not a competitor to 1A but its **discipline and diagnostics layer**. The proven
core is already bidirectional in miniature (`classify_arrow`). 1C's contribution: (a) formalizing
synthesize/check at rule boundaries so global unification is confined to cross-rule seams (matching
"global solving only at cross-rule seams"), and (b) delivering the span-precise diagnostics the
constitution mandates and 1B structurally cannot. **The strongest *composition*: 1A's unification
core + 1C's bidirectional envelope for locality and blame.** Not rival families; trunk and bark.

---

## 4. TYPES-AS-ECLASSES (1D) — the e-graph / congruence-closure route

**Charter:** the type lattice lives in an e-graph; unification is congruence closure; projections
canonicalize by saturation; extraction picks representative types by cost. Shares the engine with
LOCK-2; the FAMILY is the type-theoretic center.

### 4.1 The prior art is real and the analogy is exact — but load-bearing only under saturation

- **egg / equality saturation** (Willsey et al., POPL 2021): an e-graph "efficiently represents a
  congruence relation over many expressions"; rebuilding makes it fast; extraction picks a best
  representative by cost model
  ([egg, arXiv 2004.03082](https://arxiv.org/pdf/2004.03082); [egraphs-good](https://egraphs-good.github.io/)).
- **The deep truth 1D leans on:** *unification IS the union-find half of congruence closure.*
  E-graphs literally use a union-find (our `hm.rs` already is one; `crates/egraph/unionfind.rs` is
  the same structure). Congruence closure = union-find + the *congruence* rule (if `f(a)≡f(b)` merge
  whenever `a≡b`) — exactly *constructor* unification (if `List(a) ~ List(b)` then `a ~ b`). **HM
  unification and e-graph congruence closure are the same algorithm from two ends.** 1D is not
  wrong; it is *a re-description of 1A's core in e-graph vocabulary.*
- egglog (Datalog + eq-sat) generalizes it further
  ([egglog, EmergentMind](https://www.emergentmind.com/topics/egglog)).

### 4.2 Where the e-graph *earns its keep* — and where it is pure overhead

The e-graph's distinctive value over plain union-find is **keeping multiple equivalent forms alive
for cost-guided extraction under a rewrite system** — worth it when there are *non-confluent
rewrites* and a *cost model choosing among equivalent representations*. Type unification has
**neither**: MGU is confluent and canonical; there is one most-general type, not a space of
equivalent types to extract-by-cost. **"Projections canonicalize by saturation" and "extraction
picks representative types by cost" describe a machine with no work to do** at GN-3 — there are no
competing type representations to saturate over and no cost dimension on a type (the plane is
zero-runtime-cost; a type has no runtime cost to minimize).

**Named trap (the strongest anti-1D argument):** applying equality saturation where a canonical
form already exists is a documented over-engineering pattern — the e-graph's amortized rebuilding
and extraction are *pure overhead* against a problem union-find solves in near-linear time with a
unique answer. Our own `csp_scheduler.rs` / `extract.rs` machinery is built for LOCK-2's *genuine*
rewrite/extraction problem (IR canonicalization, kernel selection) where many equivalent forms and
a PMU-fed cost model are real. **Homing type inference there would be borrowing a cannon to swat a
fly, violating extreme-parsimony (§1.5) and Y.13.**

### 4.3 Where 1D *does* become the answer — the LOCK-2 shared-engine future

1D's charter says "shares the engine with LOCK-2." The honest reading: **types-as-eclasses is the
right architecture *iff* the type lattice must participate in the same rewrite/extraction the IR
does** — e.g. if projection canonicalization interacts with IR canonicalization under one
saturation, or if a *cost model over type representations* emerges (choosing `SmallVec` vs `Vec`
layout, enum-niche vs tagged representation — genuine cost choices with equivalent semantics). That
is a real GESTALT-B question (one e-graph home?), not a GN-3 question. **1D is banked as the
LOCK-2-convergent route: adopt only where type plane and IR plane genuinely share a
saturation/extraction; reject as the GN-3 equality engine.**

**Verdict on 1D:** theoretically the most elegant unification (literally the same union-find), but
its distinguishing machinery (saturation + cost-extraction) is **consumer-less for canonical MGU
type inference** — a Y.13 violation at GN-3. It converges with 1B's banked future: both become real
exactly when a *choice with a cost* enters the type plane, and both then live in the *one shared
engine* (csp-solver + egraph) rather than a bespoke second copy. Research verdict: **bank 1D against
GESTALT-B's "one e-graph home?" question; do not spend it on GN-3.**

---

## 5. Cross-family synthesis — how the four routes actually relate

The four families are **not four rival architectures for the same job**; the prior art reveals
them as **one trunk with three growth conditions**:

| Route | What it is, honestly | Base-lock role (GN-3) | Banked future (GN-series / GESTALT-B) |
|---|---|---|---|
| **1A** unification-first | Proven HM union-find + closed-row products + set-equal sums | **THE TRUNK** — mainline, evidence-committed | scoped/extensible rows, let-gen — each gated on a parameterized-grammar consumer |
| **1C** bidirectional-local | The synthesize/check discipline the core already embodies (`classify_arrow`) + span diagnostics | **THE ENVELOPE** — locality + constitution-mandated blame; composes with 1A | higher-rank if grammar functions gain polymorphism |
| **1B** one-constraint-store | Inference-is-constraint-solving (HM(X)/OutsideIn(X)); the csp-solver store | **PRINCIPLE ONLY** — no heterogeneous constraint exists at GN-3 to justify the store | home for overload/capacity/cost judgments when a *choice* arrives; lattice domain is the bridge |
| **1D** types-as-eclasses | Congruence closure = the same union-find; saturation+extraction on top | **CONSUMER-LESS at GN-3** — MGU is canonical, no cost-extraction to do | real iff type-plane and IR-plane share one saturation/cost model (LOCK-2 weave) |

**The single most important research finding:** *at GN-3's frozen scope there is no undecidable
judgment and no cost-bearing type choice.* Widths and decode classes are **declared** by `->`;
capacities are **annotated** corpus facts (R1); sums/rows/products are **decidable HM**; the plane
emits **zero runtime code** so no type carries a cost to optimize. This vacates the distinguishing
mechanism of *both* 1B (heterogeneous constraint store) and 1D (cost-guided saturation) — their
engines have nothing to search or extract. **1A+1C is the complete base architecture; 1B and 1D
are correct *principles for the GN-series future* whose consumers do not yet exist, and Y.13
forbids landing them before those consumers do.**

Not a defeat of 1B/1D — the exact staging discipline GN-3 §1.5 already wrote ("Full-inference
CSP/e-graph hybrids remain a GN-series design input without a GN-3 home"). Prior art *confirms* that
framing from the outside: the traditions that make constraint-stores (HM(X)) and e-graphs (egg) win
are heterogeneity and cost-guided multi-form extraction respectively, and our GN-3 problem has
neither.

---

## 6. Traps named in comparable systems (checklist for whichever route synthesis adopts)

1. **Row/label order-sensitivity** (Leijen; GN-3 R5): variant + field sets must unify *set-equal*
   with canonical ordering, or derived ≠ hand and byte-identity fails. Solved discipline; the risk
   is implementation.
2. **Duplicate labels vs unique struct fields** (Leijen scoped labels): scoped labels *permit*
   duplicates; a Rust struct emitter *forbids* them. Restrict to closed rows; do not import full
   scoped-label extensibility without a de-dup gate.
3. **Blame-at-a-distance** (the constraint-store diagnostics failure, CSUR): a pure 1B store gives
   the worst grammar-span error localization; 1C bidirectional checking is the antidote and is
   constitutionally mandated.
4. **Over-generalization to a constraint store with no heterogeneous constraint** (Y.13 / §8):
   wrapping canonical union-find in a CSP buys nothing; `decision_csp.rs` is the *right* use of the
   CSP (real candidate search) — equality is not.
5. **Equality-saturation on an already-canonical problem** (egg over-use): MGU has one answer;
   saturation + cost-extraction is pure overhead absent competing type representations. Reject 1D
   for GN-3 equality; bank for the LOCK-2 weave.
6. **let-generalization / higher-rank without a polymorphic-rule consumer** (Y.13): both need
   parameterized grammar rules (grammar closures / `GrammarCall`) to have a caller; premature
   otherwise.
7. **Capacity as an inferred quantity** (GN-3 R1, the sharpest): capacities are corpus facts, not
   grammar facts. A family that proposes to *solve* for capacity fails the byte-identity gate.
   Capacity is an annotation (`@cap(875)`), full stop.
8. **The zero-runtime-cost seam** (constitution v; PROVE-08 attack 2): any family whose mechanism
   emits a runtime instruction is refuted by `git diff crates/runtime/`. All four satisfy this by
   construction *today*; a 1D/1B future feeding a *runtime* cost model must prove it stays
   generation-time.

---

## 7. Concrete recommendation to synthesis (research verdict, no family allegiance)

The convergent design is **1A trunk + 1C envelope**, with **1B and 1D banked as named GN-series /
GESTALT-B supersession-candidates**, precisely because our own evidence base (§0) already fixed the
problem to a scope where 1B's and 1D's distinguishing machinery is consumer-less:

- **Base (GN-3, buildable today from proven parts):** grow `hm.rs` with (i) an N-ary **sum**
  constructor unified set-equal (Wall 2 / GN-3 W1); (ii) a **closed-row product** constructor for
  document structs, unified field-set-equal per Leijen's discipline *restricted to closed rows*;
  (iii) **Vec/Option cardinality** from `Repeat`/`Optional` (R6 §8b); (iv) a **bidirectional
  envelope** (`classify_arrow` generalized) so `->` ascriptions are check-mode and bare targets
  synthesize, confining global unification to cross-rule seams and giving span-precise diagnostics.
  Widths/decode classes are read from `->`; capacities from annotations. **No CSP, no e-graph —
  Y.13-clean.**
- **Banked (GN-series, land with their consumer):** 1B's one-constraint-store as the home for the
  *first genuine type-plane choice* (overload defaulting à la Zig peer-resolution, or cost-fed
  capacity/layout selection) — routed into the existing `csp-solver` (lattice domain as the
  monotone-type-set bridge, `decision_csp.rs` as the integration precedent). 1D's types-as-eclasses
  as the route *iff* GESTALT-B unifies the type lattice into LOCK-2's one e-graph under a shared
  cost model. Both are supersession candidates the gestalt adjudicates, not GN-3 deliverables.

The literature's verdict matches the codex's own §1.5 from the outside: **constraint stores and
e-graphs win on heterogeneity and cost-guided multi-form extraction; GN-3's type plane has neither;
so the mainline is unification-first with a bidirectional envelope, and the general engines are
correct futures awaiting their first real choice.**

---

## Citations

Prior art (web):
- Row polymorphism, presence/absence + lacks (Rémy 1989): [Wikipedia](https://en.wikipedia.org/wiki/Row_polymorphism)
- Extensible records with scoped labels (Leijen 2005): [MSR PDF](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf) · reference impl [tomprimozic/type-systems](https://github.com/tomprimozic/type-systems/tree/master/extensible_rows)
- egg: Fast and Extensible Equality Saturation (Willsey et al., POPL 2021): [arXiv](https://arxiv.org/pdf/2004.03082) · [egraphs-good](https://egraphs-good.github.io/) · [egglog](https://www.emergentmind.com/topics/egglog)
- Complete and Easy Bidirectional Typechecking (Dunfield–Krishnaswami 2013): [arXiv 1306.6032](https://arxiv.org/pdf/1306.6032) · impl [ollef/Bidirectional](https://github.com/ollef/Bidirectional) · survey [Bidirectional Typing, ACM CSUR 2021](https://dl.acm.org/doi/10.1145/3450952)
- OutsideIn(X) / HM(X): [OutsideIn(X), MSR](https://www.microsoft.com/en-us/research/publication/outsideinx-modular-type-inference-with-local-assumptions/) · [HM(X) is CLP(X)](https://www.academia.edu/18661687/HM_X_type_inference_is_CLP_X_solving) · [FreezeML constraints, arXiv](https://arxiv.org/pdf/2207.09914)
- Numeric-literal width / defaulting / overload: [Swift numerics](https://numerics.diploid.ca/integers-part-1.html) · [Zig peer resolution, DEV](https://dev.to/lerno/thoughts-on-numeric-literal-type-inference-rules-for-a-c-like-programming-language-4fpb) · [Kotlin KT-46360](https://youtrack.jetbrains.com/issue/KT-46360)

Codebase (verified, absolute-anchored under `/Users/mkbabb/Programming/bbnf-lang/`):
- `restart/skinny/tranches/sk-v24/SPEC.md` §0 (certified swath + refutation laws), §1 (union architecture)
- `restart/skinny/tranches/sk-v24/locks/PORTFOLIO.md` (CONSTITUTION i–ix; LOCK-1 1A–1D)
- `docs/tranches/sk-v24/GN-3.md` (three walls as spec; wave manifest; R1–R5 risks; §1.5 CSP/e-graph out-of-scope)
- `restart/skinny/tranches/sk-v24/prototypes/PROVE-08/{REPORT.md,VERIFY.md}` (HM core 221 LOC; census; three walls; zero-runtime-cost HELD)
- `restart/skinny/tranches/sk-v24/research/R6-value-type-plane.md` (`->` discarded at `passes:204`; 4-way fold `general_descent/view.rs:20-46`; hand schema `xtask/real_typed_schema.rs`; `ExprKind::Project` at `ir/src/lib.rs:261`)
- `crates/csp-solver/src/{lib.rs,domain/lattice.rs,solver/optimize.rs}` (AC-3/GAC/branch-and-bound; monotone lattice domain naming "types"; `CostFiniteDomain` cost eval)
- `crates/egraph/src/{lib.rs,csp_scheduler.rs,extract.rs,unionfind.rs}` + `egraph-derive` (`Language` macro; egraph↔CSP seam; cost extraction)
- `skinny/crates/passes/src/decision_csp.rs` (live CSP consumer precedent: backend-shape selection, 1 var / 5 constraints / MinimizeCost)
