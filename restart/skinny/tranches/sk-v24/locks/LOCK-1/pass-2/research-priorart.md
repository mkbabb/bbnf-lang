# LOCK-1 — THE SOLVER SPINE · pass-2 · PRIOR-ART RESEARCH

Problem class: **compile-time type inference & solving for a grammar/`->`-projection plane**
— HM core over algebraic + row/document shapes, erased to zero runtime cost, with
grammar-span diagnostics; CSP/lattice only where unification is undecidable. No family
allegiance; this sweep names what won and lost elsewhere for THIS class, the constraints our
own evidence already imposes, and the traps comparable systems documented.

---

## 0. The one domain analogue that IS our problem (read first)

**PADS / PADS/ML — "The Next 700 Data Description Languages" (Fisher, Mandelbaum, Walker,
POPL 2006) + "PADS/ML: A Functional Data Description Language" (Mandelbaum, Fisher, Walker,
Gleich, Zhu, POPL 2007).** A declarative description of ad-hoc data compiles to parsers +
accessors + a *type*. This is bbnf's exact shape: description → types → parser. Their calculus
has **dependent, polymorphic, and recursive datatypes**; the "700" paper is a semantic
framework (a DDL is an interpretation into a base type theory). Lesson we inherit: the
document-shape information is *declared in the description*, never inferred from a generic
grammar — which is precisely PROVE-08 Wall 1 (§0 feature 08) restated by an independent
lineage 20 years prior. The refutation "schema-from-generic-json is a category error" is not
novel; it is the settled DDL position. Corollary trap: **schema-inference-from-samples**
(F# Type Providers; "shape inference from representative documents") is *unsound/heuristic* —
exactly the current `>=4 value-refs ⇒ value` role-classifier heuristic (R6 §5c, name-gate
census). The DDL verdict: declare shapes in the grammar (`->` chains), do not infer them from
corpora.
[POPL06 700-DDL](https://dl.acm.org/doi/10.1145/1111037.1111039) ·
[PADS/ML PDF](https://www.cs.princeton.edu/~dpw/papers/padsml06.pdf) ·
[PADS DSL](https://dl.acm.org/doi/10.1145/1065010.1065046)

Also directly on-point: **regular-expression types inferred from a parsing grammar by type
inference** (surfaced in the DDL/attribution literature) — the LOCK-5 regex-HIR↔bbnf-HIR
isomorphism means regex leaf types are a *sub-lattice* of the same inference, not a separate
system (constitution ii).

---

## 1A — UNIFICATION-FIRST (HM + ADTs + row polymorphism + let-generalization)

**Row polymorphism for document shapes — the winning line, three variants:**
- **Wand** (records, original row-poly) and **Rémy** (presence/absence + `lacks` qualified
  predicates, principal types) — the classical account.
  [Row polymorphism survey](https://en.wikipedia.org/wiki/Row_polymorphism)
- **Leijen, "Extensible Records with Scoped Labels" (TFP 2005)** — **allows DUPLICATE
  labels** via label scoping, with a *sound and complete* unification algorithm. This is the
  correct choice for our class over Rémy's lacks-predicates: JSON objects legitimately carry
  duplicate keys (the tape's duplicate-flag surface, R6 §1), and scoped labels model that
  natively where lacks-predicates would reject it.
  [Scoped labels PDF](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf)
- A concrete, minimal implementation to mine: **tomprimozic/type-systems `extensible_rows`**
  (row unification in a few hundred LOC of OCaml) — same order of magnitude as our proven
  221-LOC HM core, i.e. row-poly is *affordable* at our size budget.
  [extensible_rows](https://github.com/tomprimozic/type-systems/tree/master/extensible_rows)

Row-poly is the principled cure for PROVE-08 **Wall 2** (the missing sum node): variants and
records are dual rows; GN-3 W1's `DirectTypeKind::Sum` is a *closed row of variants*, and R5's
"order-insensitive sum unification" is exactly row unification's set-based equality — a solved
problem, not a bespoke one.

**Let-generalization trap (documented, must be guarded):** local type *assumptions* (GADT/
type-family style) destroy principal types and make generalization "prohibitively difficult"
— the explicit motivation of **OutsideIn(X)** (Vytiniotis, Peyton Jones, Schrijvers,
Sulzmann, JFP 2011) and the FreezeML/HMF line. For our class the guard is easy: **per-rule
generalization only, no first-class/higher-rank polymorphism** — the proven core already
"mints one var per rule and unifies every arrow into it," which is prenex HM, principal by
construction. Do NOT chase arbitrary-rank/first-class (FreezeML, HMF, "Practical type
inference for arbitrary-rank types") — undecidable-adjacent, out of scope.
[OutsideIn(X)](https://www.researchgate.net/publication/220676837_OUTSIDEINX_modular_type_inference_with_local_assumptions) ·
[FreezeML constraints](https://homepages.inf.ed.ac.uk/slindley/papers/freezeml-constraints.pdf)

---

## 1B — ONE-CONSTRAINT-STORE (unification = equality propagation; CSP for the rest)

**The theory that makes 1B rigorous, not a slogan:** *"HM(X) type inference IS CLP(X)
solving"* (Sulzmann & Stuckey) — HM parameterized over a constraint domain X, where inference
= constraint generation + a domain solver; unification is the equality theory inside X.
**OutsideIn(X)** (GHC's real engine) layers implication constraints on top for local
assumptions. **Pottier & Rémy, "The Essence of ML Type Inference"** and **"HM style type
systems in constraint form"** are the canonical constraint-based formulation.
[HM(X)=CLP(X)](https://www.academia.edu/18661687/HM_X_type_inference_is_CLP_X_solving) ·
[HM in constraint form](https://www.researchgate.net/publication/2802561_HindleyMilner_style_type_systems_in_constraint_form)

**Our vendored engine already fits X = a lattice domain.** `crates/csp-solver/src/domain/
lattice.rs` is explicitly documented "*LatticeDomain — monotonic CSP (types, FIRST/FOLLOW
sets)*": a singleton that grows by `join`, reaching a fixed point by AC-3 propagation with no
backtracking — the exact monotone-fold shape R6 §5 found the *current* type surface to be. So
1B's "equality propagation in a global store + local propagators + global search" has a built
engine (`solver/{ac3,propagate,monotonic,optimize}.rs`, GAC all-different Régin 1994).

**The trap (sharp, from the same literature):** CLP(X)/CSP is NP-general and SMT-based solving
is expensive; **route only genuinely-undecidable judgments through it** — the task's own
"widths, overloads" caveat. Unification is near-linear (union-find); do not demote the
decidable core into a CSP store for uniformity. GN-3 §1.5 already forbids a CSP consumer in
the type tranche (Y.13): **a one-constraint-store design must name a consumer that unification
cannot serve** — numeric-width solving, decode-class overloads, capacity bounds — or it is a
consumer-less substrate (constitution viii).

---

## 1C — BIDIRECTIONAL-LOCAL (synth/check rule-local; diagnostics-first)

**Canon:** Pierce & Turner, "Local Type Inference" (POPL 1998); Dunfield & Krishnaswami,
**"Bidirectional Typing" survey (ACM Computing Surveys 54(5), 2021)** — the definitive map;
and "Complete and Easy Bidirectional Typechecking…" (ICFP 2013). The survey's load-bearing
claim for us: **bidirectional typing improves *error locality*** — checking against a *known*
type (our declared `->` projection / ascription `: T`) localizes the failure to a span, which
is exactly the "every failure names its grammar span" mandate. The proven core's
`classify_arrow` (ascription `: T` **checks**, bare target **synthesizes**) is already
textbook bidirectional — 1C *extends what PROVE-08 shipped*, it is not a new axis.
[Bidirectional Typing survey](https://arxiv.org/abs/1908.05839)

**Diagnostics-first prior art (the design center of 1C, and the strongest differentiator
available):**
- **Helium** (Heeren, Hage, Swierstra) — "Type inference and type error diagnosis for
  Hindley/Milner with extensions"; heuristics + top-quality messages, a *constraint-based*
  checker built for error quality.
  [Helium/HM diagnosis](https://www.researchgate.net/publication/241685751_Type_inference_and_type_error_diagnosis_for_HindleyMilner_with_extensions)
- **Type-error slicing** (Haack & Wells) — report *all* contributing spans, not one blame
  point.
- **MaxSMT minimum-error-source localization** (Pavlinovic, King, Wies, ICFP 2015) — reduces
  "smallest blame set" to weighted MaxSAT/SMT: this is where 1B's CSP/cost engine *legitimately*
  serves 1C (a real consumer — optimization over a constraint store, not unification).
  [SMT type-error localization](https://arxiv.org/pdf/1508.06836)
- "Getting into the Flow: Better Type Error Messages for Constraint-Based Inference"
  (OOPSLA 2023) — current SOTA on message quality.

Trap: greedy first-fail unification blames the *last* edge, not the *cause* (the Helium
motivation). Diagnostics-first means **deferring blame to a post-solve minimization**, which
couples 1C to a solver (1B) — a genuine cross-family interlock, not a duplication.

---

## 1D — TYPES-AS-ECLASSES (congruence closure = unification; saturation; extraction)

**The exact system already exists and does our job — and it subsumes 1B+1D together:**
**egglog, "Better Together: Unifying Datalog and Equality Saturation" (Zhang, Wang, Willsey,
Tatlock et al., PLDI 2023).** It fuses congruence closure (⇒ efficient unification),
Datalog + **lattice-based reasoning** (⇒ our monotone type/FIRST-FOLLOW folds), rewrite
saturation, and cost-based **extraction** — and its *named applications include HM type
inference and STLC type analysis*. So 1D is not speculative; it is a shipped, faster-than-
handwritten engine for precisely "unification as congruence closure + a type lattice +
representative extraction by cost." (`egg`, Willsey et al. POPL 2021, is the substrate.)
[Better Together (egglog)](https://arxiv.org/abs/2304.04332) ·
[egglog PDF](https://ztatlock.net/pubs/2023-pldi-egglog/2023-pldi-egglog.pdf)

Theory link (why "unification IS congruence closure" is a theorem, not an analogy):
Downey–Sethi–Tarjan / Nelson–Oppen congruence closure and HM union-find are the same
equivalence-class machinery; egglog makes the identity operational and adds lattice joins on
top — i.e. it is the csp-solver `LatticeDomain` and the HM union-find *in one home*, which is
the LOCK-1/LOCK-2 shared-engine hypothesis (1D "shares the engine with LOCK-2").

**The trap that governs the 1D verdict:** for the *pure decidable core*, an e-graph is heavier
than plain union-find HM. egglog's measured win is on **cooperating analyses + lattices +
extraction**, not on single-theory unification. The proven 221-LOC union-find core beats an
e-graph on the leaf/sum problem GN-3 actually solves. So 1D must justify itself where its
*extra* power pays — canonicalizing projections by saturation, cost-ranked representative
types across overloaded decode classes, sharing LOCK-2's rewrite engine — or it loses to 1A
on parsimony (§1.5). Same discipline as 1B's CSP guard: don't route the decidable core through
the heavy engine.

---

## 2. Constraints our own evidence base already imposes (binding on every family)

1. **Zero runtime cost is LAW, re-proven per wave** (SPEC §0 feat-08 CONFIRMED_DELTA; GN-3
   invariant: `git diff crates/runtime/` empty). Prior-art frame: this is **"zero-cost
   annotation lifetime"** type erasure (annotations consumed at check-time, no reified runtime
   type) — standard ML/System-F/Rust-monomorphization erasure. No family may add a runtime
   instruction.
2. **No performance claim for the type plane** (09 shim-refutation lesson; GN-3 §1.4). The win
   is *generality by construction*, never speed. A family arguing "faster typing" is refuted
   at the `git diff` seam.
3. **The three walls are settled LAW, scoped** (feat-08; GN-3 §1.5): generic-grammar schema
   equality is a category error (PADS confirms independently). Cure = per-document typed
   grammars (Wall 1) + a sum/row node (Wall 2) + `Project` threading (Wall 3). Families extend
   the cure; they do not relitigate the walls.
4. **Capacity hints are corpus facts, not grammar facts** (GN-3 R1 — "the sharpest risk"):
   `875`/`184`/`10_800` come from *counting a document*. Inference must NOT claim to derive
   them; they enter as explicit colocated annotations (`-> Vec<Tweet> @cap(875)`). This bounds
   1A/1B: capacity is a *hint on a constraint*, never an inferred type — matching the
   bitwidth-inference literature (widths solved as *predicates over placeholder vars* `W1<=8`,
   not read off values).
   [Polymorphic Type Inference for Machine Code](https://arxiv.org/pdf/1603.05495)
5. **Consumer-less substrate forbidden (Y.13, constitution viii).** csp-solver `LatticeDomain`
   and any e-graph must land *with* a same-wave consumer. Today the type surface is a monotone
   fold with NO CSP/e-graph consumer (GN-3 keeps them out of the type tranche) — so 1B/1D must
   *manufacture* their consumer (widths, overloads, blame-minimization, projection
   canonicalization) or stay banked.
6. **One-regex-home isomorphism (constitution ii).** Regex leaf types are a sub-lattice of the
   same inference (LOCK-5 verdict consumed); no separate type system for regex spans.

---

## 3. Traps named in comparable systems (the negative catalogue)

- **Local assumptions ⇒ loss of principal types** (OutsideIn/GHC): guard by per-rule prenex
  generalization only; no first-class/higher-rank (FreezeML/HMF out of scope).
- **Sample-based schema inference is unsound** (F# Type Providers): the current `>=4-refs` and
  byte-literal role heuristics (R6 §5c/§6) are this trap already in-tree — declare, don't infer
  shapes. A heuristic/name-gate census is the standing GN-5.W3 grep-gate.
- **Row unification with duplicate labels** rejects valid JSON under Rémy lacks-predicates —
  use **Leijen scoped labels** instead.
- **Sum/variant order-sensitivity** (GN-3 R5): canonical variant ordering + set-based row
  unification; two orderings must unify identically (row-poly gives this free).
- **Over-routing to CSP/SMT** (NP/exponential): CLP(X) is principled but only pay it where
  unification is undecidable (widths, overloads) or for *optimization* consumers (MaxSMT
  blame-minimization) — never the decidable equality core.
- **E-graph heavier than union-find on the pure core** (egglog's own measured envelope): 1D
  earns its keep only on cooperating analyses/extraction/LOCK-2 engine-sharing, not leaf/sum
  unification.
- **Greedy first-fail blame** (Helium motivation): last-edge blame misleads; defer to
  post-solve minimization — the legitimate 1C→1B coupling.
- **Deleting the 4-way `ProjectionKind` fold breaks sheets** (GN-3 R4): the sole live `->`
  consumer today; any inference redesign must reproduce sheets' generated accessors
  byte-identically (the gate's teeth).

---

## 4. Synthesis pointer (for the plan/critique fleet — not a verdict)

The four families are not rivals across the whole plane; the literature says they **stratify**.
1A (row-poly HM union-find) owns the decidable core — proven, cheap, principal, and the DDL/
PADS-endorsed shape-declaration model; 1C (bidirectional + diagnostics) is the *interface* to
it and is already half-shipped (`classify_arrow`); 1B (HM(X)=CLP(X) over the vendored
`LatticeDomain`) and 1D (egglog-style congruence-closure + lattice + extraction) are the
*same* generalization viewed as CSP vs e-graph, both **gated on manufacturing a genuine
undecidable/optimization consumer** (widths, decode-class overloads, MaxSMT blame) — absent
which Y.13 banks them. The strongest single external corroboration is **egglog**: it shows 1B
and 1D are one engine (congruence closure + lattice + Datalog), which is exactly the
LOCK-1/LOCK-2 shared-substrate question the gestalt must adjudicate.
