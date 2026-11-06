# LOCK-1 — THE SOLVER SPINE · Pass-2 Prior-Art Sweep

Scope: the type system for the grammar plane — `->` projections, document shapes, sums/products,
capacities, numeric widths, decode classes — inferred, solved, erased to zero runtime cost, with
grammar-sourced diagnostics. This sweep judges the four family charters (1A unification-first, 1B
one-constraint-store, 1C bidirectional-local, 1D types-as-eclasses) against what has won and lost
for THIS problem class elsewhere, and against the constraints our own evidence base already fixes.
No family allegiance; citations throughout.

---

## 0. The problem class, precisely

Type inference for a *decidable* fragment: rank-1 monomorphic-plus-let ADT/record shapes recovered
from grammar `->` arrows, at **generation time only** (constitution v; SPEC §0 feature 08). This is
not a general-purpose PL type checker — there is no runtime, no mutable state, no higher-rank
polymorphism demanded by the charter. That framing is decisive: it collapses several "hard" traps
to vacuity and makes the mainstream HM answer (1A) the default, with the other three families
bearing a burden of proof our own prior adjudications have already partly discharged against them.

---

## 1. The certified constraints our evidence base imposes (before any web art)

These are LAW for LOCK-1, not open questions:

- **Zero runtime cost is a verified invariant.** PROVE-08/VERIFY attack 2 (HELD): the type plane
  touches only the `codegen` generator crate; `crates/runtime/` is untouched, so zero parse-path
  instructions are added. Any family that leaks a runtime instruction is refuted at `git diff`
  (GN-3 §1.4, invariant gate every wave). The metric is **derive wall-time + structural census**
  (~230µs measured, VERIFY), never cyc/B — there is no runtime delta to pair. (`prototypes/PROVE-08/VERIFY.md`)
- **The mechanism half is already proven.** A 221-LOC HM union-find core (path-halving `find`,
  constructor-binding `unify`, honest clash error) + `classify_arrow` (ascription `: T` *checks*,
  bare target *synthesizes* — this is already bidirectional) + `type_ref` fill, all measured
  perf-neutral. (`PROVE-08/REPORT.md §1`, `patch.diff:230-456`)
- **The three walls are the floor.** Wall 1 = document information isn't in a generic grammar (cure:
  per-document typed grammars). Wall 2 = `DirectTypeKind` has only `Struct`, no sum (cure: add
  `Sum`). Wall 3 = `->` half-threaded, `Project.ty` still discarded at `passes/src/lib.rs:204`
  (cure: thread it). (`docs/tranches/sk-v24/GN-3.md §1.2`)
- **A standing negative adjudication against 1B/1D at this altitude.** GN-3 §1.5, in force:
  "CSP and e-graph machinery are **out of scope** … Leaf and sum unification is a **decidable**
  Hindley–Milner problem solved by the proven 221-LOC union-find core; no CSP or e-graph consumer
  exists in this tranche, so no such substrate lands" (Y.13 / consumer-less-substrate forbidden,
  constitution viii). LOCK-1 may supersede this only with named evidence of an undecidable
  sub-problem that unification cannot reach — the charter names two candidates (widths, overloads),
  examined in §5.

---

## 2. 1A UNIFICATION-FIRST — the mainstream winner, with a clean architecture for "the rest"

**Prior art that won.** The canonical answer to "HM core plus a bounded solver only where
unification is undecidable" is already a named, 25-year-settled framework:

- **HM(X)** — Odersky, Sulzmann, Wehr, *Type Inference with Constrained Types* (TAPOS 1999): HM
  parameterised over a constraint domain X; type inference generates constraints from program text,
  solved by a domain-specific solver; principal types hold under sufficient conditions on X. For
  plain HM, X is the **Herbrand domain solved by unification**; the framework's own examples are
  **polymorphic records and subtypes** — precisely our document-shape and width extensions.
  ([HMX PDF](https://www.cs.tufts.edu/~nr/cs257/archive/martin-odersky/hmx.pdf))
- **OutsideIn(X)** — Vytiniotis, Peyton Jones, Schrijvers, Sulzmann (JFP 2011, shipped in GHC 7):
  HM(X) extended with *implication constraints* (local assumptions), a solver strategy that only
  accepts programs with **principal types even when the spec admits programs that lack them**.
  ([OutsideIn(X)](https://simon.peytonjones.org/outsideinx/))

This is exactly the 1A shape: unification core, bounded domain solver at the seams, principal types
as the acceptance criterion. **Verdict: 1A is the load-bearing spine; the charter's phrasing is a
direct restatement of HM(X).** The row-polymorphism sub-claim has its own settled tool:

- **Row polymorphism for document shapes** — Leijen, *Extensible Records with Scoped Labels* (2005):
  a **sound and complete unification algorithm for rows**, admitting duplicate labels, orthogonal to
  standard HM, simpler than `lacks`-predicate systems (Rémy/Wand). This is the right unifier for
  object/record shapes with named fields, and it composes with the union-find core rather than
  needing a separate engine. ([scoped labels PDF](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf))

**Traps 1A must dodge (all named, most vacuous here):**
- *Value restriction* (Wright; Garrigue, *Relaxing the Value Restriction* 2004): let-generalization
  is unsound with mutable references. **Vacuous for LOCK-1** — the type plane is a pure compile-time
  producer over side-effect-free grammar `->` chains; there are no runtime refs to unsoundly
  generalize. Full let-generalization is *safe here*, a freedom most HM implementations lack. Name it
  so no one imports a value-restriction workaround as cargo cult.
  ([Garrigue PDF](https://caml.inria.fr/pub/papers/garrigue-value_restriction-fiwflp04.pdf))
- *Let should not be generalised* (Vytiniotis, Peyton Jones, TLDI 2010): GHC **abandoned** local-let
  generalization once OutsideIn/GADT local assumptions entered, because generalization under local
  equalities loses principality. **Relevant only if** LOCK-1 grows local assumptions (GADT-like shape
  refinement); rank-1 document grammars avoid it.
  ([Let should not be generalised](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf))

---

## 3. 1B ONE-CONSTRAINT-STORE — theoretically exact, engine-mismatched, already adjudicated down

**The theory 1B invokes is real:** Sulzmann & Stuckey, *HM(X) Type Inference is CLP(X) Solving*
(JFP 2007) — type inference *is* constraint logic programming; one global constraint store is the
honest formulation. So "every judgment is one constraint system; unification is equality propagation
inside the global store" is not a fantasy — it is CLP(X).
([HM(X) is CLP(X)](https://dl.acm.org/doi/abs/10.1017/S0956796807006569))

**But the charter names the wrong engine, and our own tree proves it.** 1B says "the csp-solver crate
is the engine." The vendored `crates/csp-solver` is a **finite-domain backtracking solver** — AC-3,
forward checking, GAC-all-different (Régin), cost-finite lattice domains (`csp-solver/src/lib.rs`).
That is the correct tool for a **finite choice over a bounded candidate set** — and it is *already*
consumed exactly there: `passes/src/decision_csp.rs` uses `Csp<CostFiniteDomain>` to pick one backend
shape per rule from a finite candidate list under a cost objective (LOCK-2 territory). Herbrand/feature
unification lives over an **infinite domain of type terms**; you cannot enumerate a finite domain for
a type variable, and forcing it into a finite-domain CSP either (a) degenerates to union-find wearing
a CSP costume, or (b) invites exponential backtracking where union-find is near-linear. This is a
**category mismatch**, the sharpest prior-art finding for the lock:

- The decidable HM fragment wants **union-find (Huet-style), O(n·α(n))** — not search.
- General CLP(X)/CSP search is worst-case exponential; HM(X)'s "sufficient conditions on X" exist
  precisely to keep X's solver cheap and complete, which for equality *is* unification.
- **Simple-sub** (Parreaux, ICFP 2020) is the decisive counterexample: even *subtyping* inference —
  strictly harder than ours — is done in **<500 LOC without any general constraint solver**,
  distilling Dolan's Algebraic Subtyping/MLsub biunification to plain type-variable bounds. If
  subtyping needs no CSP, our decidable ADT/row fragment certainly does not.
  ([Simple Essence of Algebraic Subtyping](https://cse.hkust.edu.hk/~parreaux/publication/icfp20/))

**Verdict on 1B:** the *idea* (one uniform constraint store) is CLP(X) and honest; the *engine claim*
(finite-domain csp-solver crate for unification) is refuted by domain mismatch and by GN-3 §1.5.
1B survives only as **notation** — writing equality, width, and capacity judgments in one vocabulary
— while *solving* stays stratified (union-find for equality; a small dedicated propagator for widths,
§5). A genuinely-unified store must clear constitution viii: no CSP variable for a type term unless a
caller reads a CSP solution union-find could not have produced.

---

## 4. 1C BIDIRECTIONAL-LOCAL — the diagnostics-first winner, partly already built

**Prior art that won on exactly our stated goals** (diagnostics-first, rule-local, span-named
failures): Dunfield & Krishnaswami, *Bidirectional Typing* (ACM Computing Surveys 2021) + *Complete
and Easy Bidirectional Typechecking for Higher-Rank Polymorphism* (ICFP 2013). The survey's thesis
maps one-to-one onto the charter:
- **checking** supports features whose inference is undecidable (our escape hatch for widths/overloads
  without a global solver);
- **synthesis** avoids annotation burden (bare `-> f64` synthesizes);
- **error locality** is the headline benefit — errors report at the syntactic site, which *is* the
  constitution's grammar-span diagnostics and GN-3 §56 fail-explicit (`DeriveError::Unify` names its
  span, never silent-fallback).
([Bidirectional Typing, arXiv](https://arxiv.org/abs/1908.05839);
[Complete and Easy, PDF](https://www.cl.cam.ac.uk/~nk480/bidir.pdf))

**Already prototyped.** PROVE-08's `classify_arrow` *is* a bidirectional leaf rule: `: T` ascription
**checks**, bare target **synthesizes** (`REPORT.md §1`). So 1C is not net-new at the leaf; it is a
discipline to lift over the whole `->` chain. Its natural division of labor — *local* bidirectional
checking inside a rule, *global* unification only at cross-rule reference seams — is exactly HM(X)
with a bidirectional front-end, the modern production default. **Verdict: 1C is not a rival to 1A but
its front-end; the two compose. The strongest LOCK-1 design is "1A engine + 1C surface,"** and the
diagnostics-first requirement makes 1C's contribution mandatory, not optional.

**Trap:** bidirectional subsumption interacts with generalization; the survey warns mode boundaries
must be placed carefully or annotation-burden/incompleteness leaks. For rank-1 document grammars this
is mild, but mode-switch points (synthesis→checking) should be grammar-structural (rule boundary,
ascription), not ad hoc.

---

## 5. The "bounded post-solve" — widths, capacities, overloads (the only place a solver earns its seat)

The charter reserves CSP "only where unification is undecidable (widths, overloads)." Prior art shows
these are **not undecidable** — they are *decidable by dedicated procedures*, which weakens the case
for a general CSP even here:

- **Numeric widths = linear integer constraints, complete procedure.** Chisel/**FIRRTL width
  inference** (formally verified, arXiv 2601.12813): the width-unknown components of a hardware IR
  yield a *subclass of linear integer constraints* with a **complete solving procedure** — not
  search. Precedent for our "numeric widths" (u8Tag→u32, i64/u64/f64 selection): a small **linear/
  lattice width solver**, monotone fixed-point over a width lattice, mirroring csp-solver's
  `domain/lattice.rs` machinery — but as a *dedicated propagator*, not backtracking search.
  ([FIRRTL width inference](https://arxiv.org/pdf/2601.12813))
- **Capacities** are corpus facts, not shape facts (GN-3 R1 names this the sharpest risk:
  `875`/`184`/`10_800` come from *counting a document*). They are **not a type-inference obligation
  at all** — they belong in grammar annotations (`-> Vec<Tweet> @cap(875)`), runtime-erased hints.
  Modeling them as CSP variables is a scope error.
- **Overload/defaulting** is decidable only under instance-shape restrictions, and **ambiguity must be
  a hard error** (Haskell's rule; type-class resolution terminates under Haskell-98 conditions),
  aligning with §56: an unresolved overload is `DeriveError`, never a silent default — except the
  *deliberate* numeric default (Haskell `default`, Rust `i32` fallback), the one sanctioned defaulting
  move for width selection.
  ([Type Classes and Constraint Handling Rules](https://arxiv.org/pdf/cs/0006034))

**Net:** the "bounded post-solve" is real but small and *decidable by dedicated propagators* (linear
width lattice + defaulting), not a general finite-domain CSP. This is the residue of truth in 1B —
one constraint vocabulary — realized as *stratified propagators*, not one search engine.

---

## 6. 1D TYPES-AS-ECLASSES — powerful for the wrong phase; congruence-closure the only shared thread

**The true observation:** an e-graph maintains a **congruence closure**, and union-find *is* the core
of both HM unification and the e-graph (egg's `UnionFind` + `rebuild`). "Unification = congruence
closure" is literally correct at the data-structure level (egg, Willsey et al POPL 2021).
([egg, arXiv](https://arxiv.org/pdf/2004.03082))

**Why it loses for LOCK-1:** e-graphs earn their keep for **equational reasoning under many rewrite
rules with a saturation schedule and cost-based extraction** — that is *optimization* (LOCK-2), not
decidable inference. For a deterministic decidable unify, an e-graph is pure liability:
- **Non-termination without a schedule** — saturation over an unbounded type lattice needs a fuel/rule
  schedule; plain unification just terminates.
- **Extraction is NP-hard** (min-cost e-class extraction) — a hard optimization to recover a type
  union-find hands you directly.
- **Rebuilding overhead** — hash-consing + deferred congruence repair buys nothing with no rules to
  fire.
- Our own **GN-3 §1.5 already ruled e-graph out** for this exact leaf/sum problem as over-engineering,
  parsimony precept behind it.

**Where 1D could contribute a sliver:** R5's *sum-variant order-insensitivity* (`object|array|…`
canonicalizes regardless of source order). But that is a **canonical sort in `sum.rs`**, not
saturation. **Verdict: 1D is the LOCK-2 engine mis-homed in LOCK-1; the only lawful shared substrate
is the union-find/congruence-closure *primitive*, which 1A already owns.** If a shared e-graph home
exists it is decided by LOCK-2, and LOCK-1 consumes at most its union-find, never its saturation.

---

## 7. Comparable parser-generator systems — what "types from grammar" looks like in the wild

- **tree-sitter / ANTLR**: emit CST/AST nodes; **no type-projection plane** — ANTLR embeds host-action
  code for symbol tables/type checks, i.e. types are *hand-written host actions*, exactly the
  `real_typed_schema.rs` hand-schema anti-pattern GN-3 kills. None derive a struct schema from the
  grammar. bbnf's grammar-`->`-derived schema is genuinely novel against this cohort (SPEC §1
  differentiator).
  ([parser-generator comparison](https://www.pistack.xyz/posts/2026-06-20-parser-generator-combinator-libraries-antlr-treesitter-nom-pest-nearley/))
- **serde / typed parser combinators**: the schema is *authored in the host type system* (the
  `#[derive]` struct), inference flowing host→parser — the reverse of grammar→schema. The nearest true
  precedent is **schema-from-data induction** (JSON-schema inference), which is heuristic — GN-3
  rejects heuristic role-classification (R6 §5c names the byte-literal heuristics as the thing to
  delete). The principled path is grammar-*declared* shapes + HM, not induced shapes.

**Lesson:** there is *no incumbent* doing grammar-`->`→HM→struct-schema; neighbors either hand-author
types (ANTLR host actions, serde derive) or induce them heuristically. LOCK-1's contribution is
defensibly novel, but inherits **no battle-tested reference implementation** — so correctness must
lean on the *general* HM(X)/bidirectional/row literature (§2, §4), not a parser-generator precedent.

---

## 8. Synthesis for the lock (what the art adjudicates)

| Family | Prior-art standing | Adjudication |
|---|---|---|
| **1A unification-first** | HM(X) (Odersky/Sulzmann/Wehr), OutsideIn(X) (GHC 7), Leijen rows — mainstream, shipped, principal | **The spine.** Charter = HM(X) restated. Rows via scoped-labels unification. |
| **1C bidirectional-local** | Dunfield & Krishnaswami (CSUR 2021 / ICFP 2013) — diagnostics-first default | **Its front-end, not a rival.** Already prototyped in `classify_arrow`. Composes with 1A. |
| **1B one-constraint-store** | CLP(X) (Sulzmann/Stuckey) real; Simple-sub proves *no* CSP needed even for subtyping | **Down-adjudicated to notation.** csp-solver crate is finite-domain (right for LOCK-2 backend pick, wrong for Herbrand unify). Widths = linear/lattice propagator (FIRRTL), not search. |
| **1D types-as-eclasses** | egg/egglog — congruence closure real, but an optimization tool | **Mis-homed.** LOCK-2's engine. Shares only the union-find primitive; saturation is liability for decidable unify. GN-3 §1.5 already ruled it out. |

**The convergent design the art points to:** a **1A HM(X) core with a 1C bidirectional surface**, its
unification a union-find over ADT/row/sum constructors, its "bounded post-solve" a pair of *dedicated
decidable propagators* (a linear width/defaulting lattice à la FIRRTL; overload resolution that errors
on ambiguity per §56) — *not* a general CSP or e-graph. 1B's grain of truth (one constraint
vocabulary) survives as uniform *notation over stratified solvers*; 1D's grain (union-find =
congruence closure) survives as the shared primitive 1A already owns. Both must clear constitution
viii (no consumer-less substrate) and the standing GN-3 §1.5 adjudication before any CSP/e-graph term
enters LOCK-1 — a bar the width fragment is the only plausible candidate to clear, and even it is
decidable without search.

---

## Citations

- Odersky, Sulzmann, Wehr — *Type Inference with Constrained Types* (HM(X)), TAPOS 1999. https://www.cs.tufts.edu/~nr/cs257/archive/martin-odersky/hmx.pdf
- Sulzmann, Stuckey — *HM(X) Type Inference is CLP(X) Solving*, JFP 2007. https://dl.acm.org/doi/abs/10.1017/S0956796807006569
- Vytiniotis, Peyton Jones, Schrijvers, Sulzmann — *OutsideIn(X): Modular Type Inference with Local Assumptions*, JFP 2011. https://simon.peytonjones.org/outsideinx/
- Vytiniotis, Peyton Jones, Schrijvers — *Let Should Not Be Generalised*, TLDI 2010. https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf
- Leijen — *Extensible Records with Scoped Labels*, TFP 2005. https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf
- Dunfield, Krishnaswami — *Bidirectional Typing*, ACM Computing Surveys 2021. https://arxiv.org/abs/1908.05839
- Dunfield, Krishnaswami — *Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism*, ICFP 2013. https://www.cl.cam.ac.uk/~nk480/bidir.pdf
- Parreaux — *The Simple Essence of Algebraic Subtyping* (Simple-sub), ICFP 2020. https://cse.hkust.edu.hk/~parreaux/publication/icfp20/
- Dolan, Mycroft — *Polymorphism, Subtyping, and Type Inference in MLsub*, POPL 2017. https://www.researchgate.net/publication/316898428
- Garrigue — *Relaxing the Value Restriction*, FLOPS 2004. https://caml.inria.fr/pub/papers/garrigue-value_restriction-fiwflp04.pdf
- Willsey et al — *egg: Fast and Extensible Equality Saturation*, POPL 2021. https://arxiv.org/pdf/2004.03082
- *A Formally Verified Procedure for Width Inference in FIRRTL*, arXiv 2601.12813. https://arxiv.org/pdf/2601.12813
- Duck et al — *Type Classes and Constraint Handling Rules*, 2004. https://arxiv.org/pdf/cs/0006034
- Local evidence base: `prototypes/PROVE-08/{REPORT.md,VERIFY.md}`, `docs/tranches/sk-v24/GN-3.md`, `research/R6-value-type-plane.md`, `crates/csp-solver/src/lib.rs`, `skinny/crates/passes/src/decision_csp.rs`, SPEC §0 / PORTFOLIO constitution.
