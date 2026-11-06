# LOCK-1 — THE SOLVER SPINE · pass-1 · PRIOR-ART RESEARCH

*Type inference & solving for the bbnf grammar plane: `->` projections, document
shapes, sums/products, capacities, numeric widths, decode classes — inferred, solved,
erased to zero runtime cost, with grammar-sourced diagnostics.*

Scope note: written without REGISTRY / favored-approach knowledge (per pass protocol).
No family allegiance — the four families (1A unification-first, 1B one-constraint-store,
1C bidirectional-local, 1D types-as-eclasses) are each measured against the same body of
prior art and the same evidence-base constraints below.

---

## 0. THE PROBLEM CLASS, NAMED

This is not a general PL type-checker. It is a **compile-time schema producer** over a
grammar: arrows (`->`) annotate leaf/rule projections, and the task infers the document
*shape* (structs, sums, Vec/Option cardinality, leaf decode + numeric width) that the
direct-to-struct emitter consumes. The nearest three prior-art families are:

1. **Constrained Hindley–Milner** (HM(X) / OutsideIn(X)) — the algebra of leaf/composite
   unification with a stratified theory solver for the non-unification residue.
2. **Row-polymorphic record/variant inference** — the theory of *document shapes*
   (records = products, variants = the `object|array|string|number|bool|null` sum).
3. **Attribute grammars** (Silver, JastAdd) — the *only* mature "type system attached to
   a grammar" prior art; `->` projections are synthesized attributes, cross-rule seams
   are reference/circular attributes. This thread is under-cited in the family charters
   and is the deepest analogue to "the full type system for the grammar plane."

---

## 1. WHAT THE EVIDENCE BASE ALREADY IMPOSES (binding constraints, not options)

These are settled by SPEC §0, PROVE-08/VERIFY, GN-3, and the Constitution. A LOCK-1
route that violates one must *defeat* it by name (Constitution vii, ix), not ignore it.

- **C1 — Zero runtime cost is LAW (verified invariant).** The type plane is a generator
  construction; it emits *no* runtime instruction. PROVE-08/VERIFY attack 2 HELD; GN-3's
  per-wave gate is `git diff crates/runtime/ == empty`. Any solver — HM, CSP, or e-graph
  — runs at generation time only. (SPEC §0 row 08; GN-3 §1.4, §3.)
- **C2 — No perf claim for the plane (SPEC §0 refutation 09 posture).** The plane's
  runtime delta is identically zero; "zero" is a null claim, never a win. The only perf
  numbers LOCK-1 may touch are PROVE-07's direct-to-struct *non-regression* ratios
  (citm 1.85–1.92× vs serde; sonic-typed 0.746–0.786×). (GN-3 §1.4.)
- **C3 — The three walls are LAW, scoped to generic `json.bbnf`.** (a) *information* —
  document shapes (46 struct sites / 19 roots, capacity hints 875/184/10_800/5_759) live
  in `xtask/real_typed_schema.rs`, not in a generic grammar; cure = per-document typed
  grammars. (b) *IR expressiveness* — `DirectTypeKind` has one variant `Struct`; the
  natural recursive **sum** has no home; cure = add `Sum`. (c) *front-end* — `->` is now
  `ExprKind::Project{body,target,ty}` at HEAD but `ty` is discarded at
  `passes/src/lib.rs:204`; cure = thread it. (PROVE-08 §4; VERIFY "three walls"; GN-3 §1.2.)
- **C4 — GN-3 has ALREADY ADJUDICATED CSP + e-graph OUT OF SCOPE for the type plane.**
  Verbatim (GN-3 §1.5): *"Leaf and sum unification is a decidable Hindley–Milner problem
  solved by the proven 221-LOC union-find core; no CSP or e-graph consumer exists in this
  tranche, so no such substrate lands."* This is the sharpest constraint on families **1B**
  and **1D**: they contradict a settled adjudication and a Constitution law (viii /
  Y.13 — consumer-less substrate forbidden). To survive they must produce a *named,
  in-wave type-plane consumer* that unification cannot serve — i.e. a genuinely
  multi-valued decision (width, overload, decode-class choice), not equality.
- **C5 — Refutation guards apply vacuously and as scope-fences.** No digit-scan SIMD, no
  block scheduling, fold-over-cursor dead, tape-navigation ≠ get lane — all satisfied
  because the plane adds no runtime code; but any solver route that *reaches into* runtime
  emission (e.g. "solve the scan plan") has crossed into LOCK-2/GN-2, a scope breach.
- **C6 — The proven seed is 221 LOC of union-find HM.** PROVE-08 `hm.rs`: path-halving
  `find`, constructor-binding `unify` with honest clash error, closed leaf vocab
  `{Bool,F64,U8Tag,StringDecoded,StringSpan}`, `classify_arrow` (ascription `:T` **checks**,
  bare target **synthesizes** — a working bidirectional split already in-tree),
  `infer(arrows)` unifying every arrow of a rule into one var. CONFIRMED_DELTA. Any route
  starts from this, not a blank page.

---

## 2. WHAT EXISTS ELSEWHERE — WON / LOST FOR THIS PROBLEM CLASS

### 2A. Constrained HM: HM(X) / OutsideIn(X) — the stratification that WON
The field's settled answer to "unification plus other judgments" is **stratification, not
fusion**: unification is a normalization procedure for *equality* constraints; a separate
theory solver X handles the rest, and the composite has principal types iff X is well-behaved
(Odersky–Sulzmann–Wehr HM(X); Sulzmann "HM(X) type inference is CLP(X) solving"). GHC's
production system OutsideIn(X) layers *implication* constraints atop X and is parameterised
over the constraint domain exactly so the metatheory and inference algorithm stay common
([OutsideIn(X), Vytiniotis–Peyton Jones–Schrijvers–Sulzmann, JFP 2011](https://www.researchgate.net/publication/220676837_OUTSIDEINX_modular_type_inference_with_local_assumptions);
[HM(X) = CLP(X)](https://www.academia.edu/18661687/HM_X_type_inference_is_CLP_X_solving)).
**Lesson for 1A:** "HM core grows; CSP appears only where unification is undecidable
(widths, overloads) as a bounded post-solve" IS the HM(X) recipe — the most battle-tested
architecture in the space. It is the same shape as C4's forced verdict.

### 2B. Swift's constraint solver — the CAUTIONARY TALE that LOST (the 1B/CSP trap)
Swift models overload + literal + operator resolution as **one global constraint system with
disjunctions**, and it blows up: 17 `+` overloads × 9 `ExpressibleByStringLiteral` types →
combinatorial disjunction explosion; a **12-line string-concat expression takes 42 s on an
M1 Pro** and errors "cannot type-check in reasonable time"
([cocoawithlove](https://www.cocoawithlove.com/blog/2016/07/12/type-checker-issues.html);
[Hooper "Why Swift's Type Checker Is So Slow"](https://danielchasehooper.com/posts/why-swift-is-slow/);
[swift/docs/TypeChecker.md](https://github.com/apple/swift/blob/main/docs/TypeChecker.md)).
Swift's own mitigations are *heuristic disjunction ordering + solver budgets* — a patch on a
model that put too much into search. **Direct threat to 1B "every judgment is one constraint
system" and 1C/1D width-as-search:** if equality is dissolved into general CSP search rather
than near-linear union-find, worst-case is exponential. The whole field's answer (2A) is to
keep equality OUT of the search and give the theory solver only the genuine residue.

### 2C. Row polymorphism — the theory of DOCUMENT SHAPES (1A's row clause)
Two principled inference algorithms, both giving principal types; they are a real design fork:
- **Gaster–Jones** (presence/absence + lacks predicates): record extension only when the
  label is absent; qualified types. Clean for **closed structs** (a schema field is present
  or absent) — the natural fit for the direct-to-struct product shapes.
- **Leijen scoped labels** ([Extensible records with scoped labels](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf)):
  permits **duplicate labels** with scoping; sound+complete unification without lacks
  predicates. JSON *allows duplicate keys*, so scoped labels model raw document reality —
  but a *derived struct schema* wants uniqueness, so Gaster–Jones is likely the better fit.
- **Trap (won-and-lost history):** Wand's original overwriting semantics **loses principal
  types** — the mistake both fixes exist to avoid ([Wikipedia: Row polymorphism](https://en.wikipedia.org/wiki/Row_polymorphism);
  [tomprimozic/type-systems extensible_rows](https://github.com/tomprimozic/type-systems/tree/master/extensible_rows)
  — a compact reference implementation of both). Whichever family lands document shapes
  must pick Gaster–Jones *or* Leijen and state which; "records, informally" re-derives Wand's bug.

### 2D. Bidirectional typing — the 1C reference, and error locality is the point
Dunfield & Krishnaswami's survey ([Bidirectional Typing, ACM CSUR 2021, arXiv:1908.05839](https://arxiv.org/abs/1908.05839))
is the definitive treatment: **check** supports features inference cannot decide; **synthesize**
cuts annotation burden; and — decisive for LOCK-1 — *bidirectional typing improves error
locality*. That is exactly 1C's "diagnostics-first; every failure names its grammar span."
PROVE-08's `classify_arrow` is already a two-line bidirectional split (§C6), so 1C has an
in-tree seed, not a green field. Design guidance from the survey: annotations at let/rule
boundaries, mode-correctness, recursion on the *declared projection* — maps cleanly onto
"synthesize/check against declared `->` projections, rule-local."

### 2E. Attribute grammars — the DEEPEST analogue, under-cited (relevant to all families)
Silver and JastAdd are mature "type-system-attached-to-a-grammar" systems: synthesized +
inherited attributes, **higher-order** attributes (trees as attribute values), **reference**
attributes (non-local lookups = 1C's cross-rule seams), **circular** attributes with
fixed-point evaluation ([Silver, van Wyk et al., SCP 2010](https://www-users.cse.umn.edu/~evw/pubs/vanwyk10scp/vanwyk10scp.pdf);
[JastAdd tutorial](https://link.springer.com/chapter/10.1007/978-3-642-18023-1_4)). Silver
*type-checks its own AG spec language*. **Lesson:** `->` projections are synthesized
attributes; the type plane is an attribute-grammar evaluation. The AG world's hard-won
lessons — demand-driven vs statically-scheduled evaluation, circularity detection, and that
reference/circular attributes are where complexity concentrates — pre-figure LOCK-1's
cross-rule seam handling. **Trap AG names:** unrestricted circular attributes need a
fixed-point lattice with a termination guarantee; naive scheduling loops.

### 2F. E-graphs / congruence closure — where 1D is sound, and where it is idle
The theory is friendly: **congruence closure generalizes union-find with the congruence
rule**, and EUF-over-e-graphs is exactly how Z3/CVC decide equality
([E-graph, Wikipedia](https://en.wikipedia.org/wiki/E-graph)); egg/egglog give production
Rust implementations ([egg](https://arxiv.org/pdf/2004.03082); egglog = Datalog + eq-sat),
and the in-tree `egraph` + `egraph-derive` crates exist. So "unification is congruence
closure; types are e-classes" is *technically* sound. **But equality saturation buys you
extraction-by-cost over MANY equal forms** — and pure HM has a *unique principal type*, so
there is nothing to extract. 1D's "projections canonicalize by saturation; extraction picks
representative types by cost" only earns its keep where the type is genuinely *multi-valued*:
numeric widths, overload choice, decode-class selection, capacity. **That is precisely the
CSP residue of 2A/C4 — 1B and 1D converge on the same small non-unification decision set,**
and both must clear C4 (a named in-wave consumer) to land any substrate at all.

### 2G. Numeric width / decode class — this is DEFAULTING, not search
Widths ("null → U8Tag → u32", already ad-hoc in the PROVE-08 artifact §5) are the canonical
*defaulting* problem. Prior art is unanimous and cheap: **Rust** defaults an
under-constrained integer literal to `i32`
([rust-lang/reference#104](https://github.com/rust-lang/reference/issues/104)); **Haskell**
walks an ordered default list (Int → Integer → Double) picking the first that solves the
class constraints ([GHC type-variable defaulting](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_defaulting.html)).
Both are bounded, deterministic, near-instant — **not CSP search**. **Trap:** modeling width
as a full CSP (1B) is over-engineering a defaulting problem; the honest resolution is an
ordered default lattice applied post-unification. Overloads (if any exist in the grammar
plane) are the *only* candidate for genuine search — and Swift (2B) warns exactly there.

### 2H. Schema-from-grammar / typed decoders — the emitter half is a solved genre
Deriving an ADT + decoder from a schema/grammar is established: XML TypeLift generates a
Haskell parser + ADT from XML Schema; serde-derive, capnproto, Rendel–Ostermann invertible
syntax descriptions all bridge grammar↔typed value
([XML TypeLift](https://arxiv.org/pdf/2011.03536);
[extensible type checker for parser generation](https://arxiv.org/pdf/1002.1549)). The novelty
LOCK-1 owns is *inferring* the schema from `->` projections rather than being handed it — the
inference is the hard half; the emit is genre-standard (R6 §5d: the `json_typed_direct`
renderer already exists and works, fed only by a hand schema).

---

## 3. IN-TREE ASSETS AND THEIR CURRENT CONSUMERS (Constitution viii audit)

- **`crates/csp-solver/`** (vendored csc411): backtracking + AC-3 + forward-checking, GAC
  all-different (Régin), **`CostFiniteDomain` for min-cost optimization**, **lattice domains
  for monotone fixed-point propagation** (`domain/lattice.rs`), node budget + fail-first
  ordering + timeout + cancel token (`lib.rs`). The budget/ordering vocabulary is exactly
  Swift's mitigation kit (2B) — the engine is *built* to be bounded.
- **Current CSP consumer = LOCK-2, not LOCK-1.** `skinny/crates/passes/src/decision_csp.rs`
  uses `Csp<CostFiniteDomain>` to select *backend shapes* (recognizer/substrate/simd/
  capacity constraints, `MinimizeCost`, 1 var / 5 constraints / node-budget 10k). This proves
  the engine works in-tree — but its consumer is kernel/backend selection. **LOCK-1 families
  1B/1D inherit no type-plane consumer from this**; per C4 they must mint one or stand as
  forbidden consumer-less substrate.
- **`egraph` + `egraph-derive`** (in-tree, LOCK-2 seed): the `Language`-derive path exists;
  same consumer-audit applies to 1D.
- **PROVE-08 `type_plane/{hm,derive}.rs`** (221+211 LOC): the ONLY in-tree asset whose
  consumer *is* the type plane. Union-find HM, arrows→leaf, `type_ref` fill, validated
  `DirectSchemaSet`. This is 1A's foundation and 1C's bidirectional seed.

---

## 4. TRAPS NAMED IN COMPARABLE SYSTEMS (checklist for every family)

1. **Equality-in-the-search explosion (Swift, 2B).** Dissolving unification into general CSP
   is worst-case exponential. Keep equality as near-linear union-find; give the solver only
   the non-unification residue. Threatens 1B centrally, 1D and 1C secondarily.
2. **Row inference losing principal types (Wand, 2C).** Pick Gaster–Jones *or* Leijen and
   say which; informal records re-derive the bug.
3. **Width-as-CSP over-engineering (2G).** Defaulting (ordered lattice) suffices; reserve
   search for real overloads only.
4. **Let-generalization × effects unsoundness (the value restriction).** Generalizing a
   binding that could leak mutable state is unsound; Wright's fix restricts generalization to
   syntactic values ([Subtype inference pt.11](https://blog.polybdenum.com/2020/09/19/subtype-inference-by-example-part-11-the-value-restriction.html)).
   1A's "let-generalization over rules" must state its generalization criterion — rules are
   pure schema producers so the risk is low, but the criterion must be explicit, not assumed.
5. **Consumer-less substrate (Constitution viii / Y.13; GN-3 §1.5 already adjudicated).**
   Any CSP/e-graph machinery without a same-wave type-plane consumer is forbidden AND
   contradicts a settled verdict. Hardest gate for 1B/1D.
6. **Circular-attribute non-termination (AG world, 2E).** Cross-rule/recursive-rule
   inference needs a fixed-point over a lattice with a termination argument (LLVM-style
   `Changed` flag per the in-tree convergence discipline), not a structural-hash loop.
7. **Scope creep into runtime (C1/C5).** "Solve the scan plan / decode kernel" is LOCK-2.
   LOCK-1's output is a schema; it must add zero runtime instructions or the invariant gate
   refutes the wave regardless of its own gate.

---

## 5. SYNTHESIS — WHERE PRIOR ART POINTS (family-neutral)

The convergent verdict of HM(X)/OutsideIn(X) (2A), Swift's failure (2B), the defaulting
consensus (2G), and GN-3's own adjudication (C4) is a **stratified spine**: union-find
equality unification (the proven 221-LOC core, grown with a Sum node and row records) as the
decidable trunk; a **bounded, budgeted** theory solver for the genuine residue — numeric
widths (defaulting lattice first), decode classes, and overloads if any — only where a named
in-wave consumer exists. Bidirectional check/synthesize (2D) is the diagnostics-and-locality
layer, seeded in-tree already. The e-graph (1D) and the one-constraint-store (1B) are sound
*only* on the residue, never on equality; on that residue they compete with plain defaulting
and must beat it on a measured, consumer-backed basis. The under-cited attribute-grammar
tradition (2E) is the closest whole-system analogue and supplies the cross-rule-seam and
fixed-point discipline the charters gesture at. GN-3's five waves already encode the trunk;
LOCK-1's open question is whether the residue is rich enough to justify any solver beyond an
ordered default lattice — and the burden of proof (C4, trap 5) is on the family that says yes.

---

## Sources
- [OutsideIn(X): modular type inference with local assumptions](https://www.researchgate.net/publication/220676837_OUTSIDEINX_modular_type_inference_with_local_assumptions)
- [HM(X) type inference is CLP(X) solving](https://www.academia.edu/18661687/HM_X_type_inference_is_CLP_X_solving)
- [Swift type-checker exponential time (cocoawithlove)](https://www.cocoawithlove.com/blog/2016/07/12/type-checker-issues.html) · [Why Swift's Type Checker Is So Slow (Hooper)](https://danielchasehooper.com/posts/why-swift-is-slow/) · [swift/docs/TypeChecker.md](https://github.com/apple/swift/blob/main/docs/TypeChecker.md)
- [Extensible records with scoped labels (Leijen)](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf) · [Row polymorphism (Wikipedia)](https://en.wikipedia.org/wiki/Row_polymorphism) · [tomprimozic/type-systems extensible_rows](https://github.com/tomprimozic/type-systems/tree/master/extensible_rows)
- [Bidirectional Typing survey (Dunfield & Krishnaswami, arXiv:1908.05839)](https://arxiv.org/abs/1908.05839)
- [Silver: an Extensible Attribute Grammar System](https://www-users.cse.umn.edu/~evw/pubs/vanwyk10scp/vanwyk10scp.pdf) · [JastAdd tutorial](https://link.springer.com/chapter/10.1007/978-3-642-18023-1_4)
- [E-graph (Wikipedia)](https://en.wikipedia.org/wiki/E-graph) · [egg: Fast and Extensible Equality Saturation](https://arxiv.org/pdf/2004.03082)
- [Rust integer literal defaulting (reference#104)](https://github.com/rust-lang/reference/issues/104) · [GHC type-variable defaulting](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_defaulting.html)
- [The value restriction (Subtype inference pt.11)](https://blog.polybdenum.com/2020/09/19/subtype-inference-by-example-part-11-the-value-restriction.html)
- [XML TypeLift](https://arxiv.org/pdf/2011.03536) · [Extensible type checker for parser generation](https://arxiv.org/pdf/1002.1549)
- In-tree: PROVE-08 `REPORT.md`/`VERIFY.md`, GN-3.md, R6-value-type-plane.md, `crates/csp-solver/src/lib.rs`, `skinny/crates/passes/src/decision_csp.rs`
