# LOCK-1 — THE SOLVER SPINE · pass-1 · FAMILY-SPECS (synthesis, post-prototype)

*Type inference & solving for the bbnf grammar plane: `->` projections, document shapes,
sums/products, capacities, numeric widths, decode classes — inferred, solved, erased to zero
runtime cost, with grammar-sourced diagnostics. GN-3's three walls are the floor.*

One cogent spec per family while the routes remain incompatible, then the incompatibility map.
Elegance earns nothing on its own; a family stalled at a missing primitive equal in difficulty
to the original problem is marked BLOCKED-candidate. Verified against source (§0) and against
the two prototypes this pass ran — PROTO-1 (judgment-mix census) and PROTO-2 (1D injectivity) —
so the dispositions are *measured*, not asserted (Constitution vii).

---

## 0. THE DECISIVE FACT THE CHARTERS SPLIT ON — verified, and it re-frames the whole lock

The four charters silently reference **two different code lineages**, and the prior-art
report's sharpest constraint (C4: *"families 1B/1D inherit no type-plane consumer … forbidden
consumer-less substrate"*) is **factually superseded for the tree the greenfield is cut from.**
Confirmed on disk this pass:

- **`skinny/crates/` (older lineage).** Carries 1A's dormant HM scaffold —
  `passes::types::TypeFacts { subst, obligations: Vec<TypeObligation{ span, solver_stage }> }`
  (`skinny/crates/passes/src/lib.rs:126-145`), **populated by nothing**; the `->` discard is
  verbatim at `:204`. This scaffold is *itself* consumer-less substrate today (a Y.13
  violation) that 1A's activation repairs. 1A and 1C target this lineage.
- **root `crates/` (live lineage).** Carries a **running CSP type inference** —
  `crates/ir/src/passes/types/constraint/{domain,alt,seq,operators,reference,grounds,revise}.rs`,
  driven by `project_types()` → `csp.propagate()` (`types/mod.rs:51`), emitting
  `TypeDesc::HeterogeneousAltJoin` (`types/type_desc.rs:63,209`), threaded through
  `GrammarIR::type_obligations` (`grammar.rs:428`) into **both backends**
  (`core/backend/rust/ir_types.rs:346`, `core/backend/ts/projection.rs:40`), asserted
  end-to-end (`ir/tests/lattices/types.rs:521`). **It is the type system of record that already
  ships.** The e-graph engine 1D wants (`crates/egraph/` + `csp_scheduler.rs`) is equally live,
  with a working `Language` consumer (`crates/ir/src/egraph/node.rs`) — but **no type Language
  (`TypeENode`) exists yet**; 1D's third tier is genuinely net-new.

**Consequence.** The lock is not "green HM core vs three exotic proposals." It is a genuine
**engine-of-record fork**: a *proven-elsewhere* union-find HM core (1A/1C) versus an
*already-running-here* lattice-CSP inference (1B) versus an *already-shipping-engine* e-graph
whose type tier is net-new (1D). C4's consumer veto **does not bind 1B or 1D**. The honest gate
on all four became the prior-art report's *other* verdict (§2G/§4.3/R-B4): **is the
non-equality residue rich enough to justify any solver beyond an ordered defaulting lattice?**
That question was **measured this pass — PROTO-1 — and the answer is NO** (§0.1).

**A second verified fact:** the in-tree `.bbnf` grammars (`json`, `css/pretty`, `google-sheets`,
`bnf/ebnf/bbnf`) carry essentially **zero `->` projections**; the typed per-document grammars
(twitter/citm/canada) GN-3.W3 names **do not yet exist**. Row-polymorphism (1A) and diagnostic
locality (1C) therefore have **no corpus to be measured on** — both are currently unfalsifiable
and sequence *behind* GN-3.W3 authoring typed grammars.

### 0.1 The measured verdict — PROTO-1 (residue census over the live root pass)

PROTO-1 instrumented the **live** `project_types` constraint generation over the four real
corpora (JSON, CSS L4, Sheets, BBNF), tagging every propagator by judgment kind and reading the
homogeneous/heterogeneous split from *ground truth* (`join_types` → `HeterogeneousAltJoin`):

| grammar | eq+sum-shape | **genuine multi-valued residue** (hetero-alt) | overload/cost |
|---|---:|---:|---:|
| JSON | 82.0% | **2.0%** | 0 |
| CSS L4 | 89.1% | **0.3%** | 0 |
| Sheets | 89.2% | **0.0%** | 0 |
| BBNF | 89.4% | **0.0%** | 0 |

Genuine multi-valued residue is **≤2.0% in every grammar** (an order of magnitude under the 15%
RICH bar); **overload/cost residue is exactly 0** everywhere. Wide alternations exist (CSS
`declaration` fans to arity 28) but **collapse to a single homogeneous type — arity is not
richness**. The width/capacity/decode buckets are all **deterministic monotone functions** (one
fixed decode per site, one tuple arity, one bound), never a >1-candidate search. **Verdict:
THIN.** The store never holds a variable needing search or cost to choose. **This retires the
justification for both 1B's one-store and 1D's cost-extraction on the evidence** — not on
metatheory, on measured constraint counts. Record **1A + an ordered defaulting lattice** as the
surviving spine.

---

## FAMILY 1A — UNIFICATION-FIRST (the HM core grows) · disposition: **ADVANCE (the spine)**

**Architecture.** One Hindley–Milner inference in the generator, HM(X)/OutsideIn(X) verbatim:
first-order union-find unification is the decidable, principal, terminating **trunk**; the
vendored `csp-solver` — or, per PROTO-1/§2G, an **ordered defaulting lattice** — is a bounded
post-solve for the genuine residue only (widths, decode-class/overload, capacity). Document
shapes are row-polymorphic records; rules are let-generalized schemes; recursion is
iso-recursive **nominal folding** (`unify(Rule i, Rule j)` compares ids, never descends).

**Mechanism.** Fused monotype `τ ::= Var|Leaf|List|Option|Prod(ρ)|Sum(ρ)|Rule(id)`, rows
`ρ={l:τ…|r}`. `unify` grows the Robinson arms; the six-arm `value` sum unifies
order-insensitively (GN-3 R5 free from row theory). Rule bodies `generalize(infer(body))`;
`Ref` instantiates fresh; recursive SCCs seeded monomorphic, unified, then generalized.
Widths/overloads become `TypeObligation`s solved *after* structural generalization (rustc-style
late defaulting).

**Integration seams (skinny lineage).** Activate `TypeFacts.subst/.obligations`
(`skinny/…/lib.rs:126-145`); replace the fold (`:178-231`) and `->` discard (`:204`); nominal
seed → scheme (`:172`,`:195-201`); grow `DirectTypeKind::Sum` + `DirectScalar` width
(`schema/direct_schema.rs:52`); reuse PROVE-08 `hm.rs` union-find + `classify_arrow` verbatim.

**Gates.** (G1) recursive `value` terminates → one 6-variant sum, ~230 µs floor held. (G2) open
row absorbs extra keys without a closed per-document grammar. (G3) generic `pair`/`entry` at two
field types does not cross-unify. (G4) width post-solve recovers PROVE-08's `U8Tag→U32` loss →
`u8`. (G5) `git diff crates/runtime/` empty; the second type system (`type_plane::hm` **or** the
running root CSP) is deleted, not coexisted. (G6) ascription clash names the arrow span.

**Exact open questions.** (O1) **Row discipline: Leijen (duplicate labels) vs Gaster–Jones
(uniqueness).** research-A picks Leijen; priorart §2C argues a *derived struct* wants
uniqueness. Wand's overwrite bug lurks either way (and — see §1B.O1 — *already ships* in the
root join). Unresolved, principal-type-critical, but **currently unmeasurable** (no typed
grammars). (O2) Is the width residue a lattice-CSP or **ordered defaulting**? PROTO-1 §2G:
overload/cost residue is exactly 0 → **defaulting dominates; the CSP is over-engineering for
widths.** (O3) **Lineage displacement:** 1A re-seeds an equality engine the greenfield may have
already chosen (the running root CSP). 1A's win requires *deleting running root code* — an
in-scope-migration question, the family's quietest liability.

---

## FAMILY 1B — ONE-CONSTRAINT-STORE · disposition: **ADVANCE the federation as system-of-record; BANK the single-store thesis (BLOCKED-candidate)**

**Architecture.** Every judgment is a constraint over inference variables, one engine
(`csp-solver`), one discipline (`propagate_monotonic`, the LLVM-style *Changed* sweep);
unification = lattice join. HM(X)=CLP(X). **Uniquely, not a proposal — the running root tree**,
to be named, completed, and *hardened*, not invented.

**Mechanism.** Generation walks the IR emitting one propagator per former into
`Csp<TypeDomain>` (`TypeDomain=Option<TypeDesc>`, `join` grows ⊥→ground); under-determination
drains as a named `TypeObligation` (`HeterogeneousAltJoin`). Remaining kinds are net-new domains
in the same engine (width `CostFiniteDomain`+`solve_optimized`, decode-class finite domain).

**Integration seams (root lineage — already wired).** Grow `TypeDomain::join`
(`constraint/domain.rs`); add propagators in `constraint/*.rs`; extend `obligation.rs`; land
`TypeDesc::Sum` beside `HeterogeneousAltJoin`; keep the shipping backend consumers fed. Engine
stays vendored — additions consumer-side; any engine change is a csc411 tranche.

**Gates.** (G1) six-arm alt → one order-insensitive canonical sum. (G2) **two incompatible
arrows on one rule raise a clash obligation, not last-writer-wins — presently RED** (O1). (G3)
width via `solve_optimized`, bounded. (G4) residue census RICH — **FALSIFIED by PROTO-1 (≤2%
everywhere)**. (G5) runtime diff empty. (G6) obligations name spans.

**Exact open questions.** (O1 — **the confirmed shipping defect**) `TypeDomain::join`
(`constraint/domain.rs:72-77`) **allows re-assignment to a different type on disagreement**
("the solver may refine") — **non-monotone last-writer-wins = Wand's principal-type-losing bug
(priorart trap 2) in production**, silently swallowing a genuine clash (violating
no-silent-epsilon). Must become raise-on-disagreement; refinement reserved for ⊥→ground only.
**Un-run this pass — Brief 1 targets it.** (O2 — the R-B1 wall) `Csp<D>` is monomorphic; a
literal single heterogeneous store needs a tag-dispatched `JudgmentValue` domain (a per-revise
branch fighting Rust) or a **federation** (one `Csp` per lattice). The tree runs federation →
"one store" reduces to "one engine + one obligation channel + one discipline," **the thesis
defeated in its literal form, and the federation IS 1A's stratification wearing a CSP coat.**
(O3) PROTO-1 shows the residue is THIN → 1B buys nothing a defaulting lattice does not; its win
was only the shared substrate, now vacuous.

**BLOCKED-candidate:** the literal single heterogeneous store admits no primitive short of
per-revise tag dispatch (R-B1) — a wall of the same character as the problem. The federation
survives but *is 1A*. Its equality is **unsound as shipped** (O1).

---

## FAMILY 1C — BIDIRECTIONAL-LOCAL · disposition: **FOLD (diagnostics/locality layer over the winner)**

**Architecture.** Synthesize (⇒) / check (⇐) per the Pfenning recipe; one subsumption
reconciler as the sole error site; "global" bounded to SCC condensations of the `Ref` graph
solved by a `Changed`-flag fixpoint; widths hand off to a bounded lattice. Thesis: the expected
type is known at the failure site → **every diagnostic names the exact grammar span.**

**Mechanism.** Split `infer_expr` → `synth_expr` + `check_expr(ExprId,&Expected)`; `Project`
becomes the mode switch; `Ref` consults `RuleSig` in SCC order; the reconciler reuses PROVE-08's
`Unifier` for equality and a subtype-join at the numeric case.

**Integration seams (skinny lineage).** `infer_expr` split (`:178-215`); `Ref`→`RuleSig`
(`:201`); `Project` mode switch (`:204`); SCC pre-pass (`:149`); **`PassDiagnostic` gains a
`span` field** (`diagnostics.rs:3-8` is `RuleId`-only — a named substrate gap).

**Gates.** (G1) mode switch fills `DirectBuildField.target` non-`None`. (G2 — the justifying
experiment) on an erroneous typed grammar, `check_expr`'s `TypeError.span` equals the `Project`
span while unify-first reports at a synthesized interior. (G3) json's `value/object/array/pair`
land in one SCC; SCC-local reconciliations are a handful. (G4) width lattice → `u32` in one AC-3
pass. (G5) runtime diff empty; sheets regen byte-identical.

**Exact open questions.** (O1 — the bidirectional critique) with **zero `->` in the real
corpus**, 1C degenerates to pure synthesis = 1A; its value materializes only on typed grammars
that **do not yet exist** → G2 **cannot run** until GN-3.W3 lands. (O2) Is 1C an engine family
at all, or a discipline layer over 1A's union-find (which it reuses verbatim)? Per the
Constitution's shared-mechanism rule, **1C = 1A + a checking front + an SCC seam + a span
field**, orthogonal to the engine axis (§5). (O3) Sub-span granularity: `Project.ty` is one
`String`; a `ty_span` sub-span is the quality ceiling.

**Not blocked; not an engine family.** Fold as the diagnostics plane over whichever equality
engine wins; bank the `PassDiagnostic.span` gap and the SCC seam regardless.

---

## FAMILY 1D — TYPES-AS-ECLASSES · disposition: **BANK — narrow recursion-axis keep only; cost-extraction VACUOUS on the evidence**

**Architecture.** The type lattice lives in an e-graph; unification = congruence closure +
constructor injectivity; recursive sums are native cyclic class graphs (no occurs-check, no
μ-binder); widths are an analysis-lattice fact on one `Int` leaf class resolved by extraction.
Claim: the engine is already paid for — types are the third `Language` instance.

**Mechanism & measured result (PROTO-2, RAN).** The one net-new primitive — a constructor
injectivity propagator in `Analysis::modify` (~22 LOC) — **was built on the shipped engine and
proven**: (a) `Sum([a,b])≡Sum([c,d]) ⟹ a≡c,b≡d` holds (genuine unification via the existing
rebuild worklist); (b) cyclic `value` extracts to a **finite** 6-arm μ-representative with a
back-edge; (c/d) extraction is order-independent and idempotent (byte-identical); (e) the
combined injectivity+width fixpoint **converges in 2 passes (bound 10), no oscillation**. **1D
does NOT hit its retirement condition on the recursion axis** — native cyclicity *beats* 1A's
nominal fold (finite μ-extraction free via the visited set), it does not merely duplicate it.

**But two measured costs bound the keep:** (i) PROTO-2 required a **core-engine amendment** to
`crates/egraph/src/egraph.rs` `union` (schedule `modify` on every real merge, not only on
lattice change) — a change to the shared/vendored engine, not a consumer-side addition. (ii)
Positional injectivity is **unsound for associative-commutative constructors** (`Sum`): two
differently-ordered same-arity sums in one class would falsely unify — a production 1D **must
tag constructors injective-vs-AC and gate the hook**. And decisively: **PROTO-1 shows the
cost-extraction that is 1D's raison d'être is idle** — overload/cost residue is exactly 0, so
there is *nothing to extract over*. 1D's engine is idle on the equality that dominates
(priorart §2F) and idle on the residue that is empty.

**Integration seams.** `TypeENode`+`type_rules`+`TypeAnalysis` net-new beside the engine (the
grammar tier `node.rs`/`build_egraph.rs`/`rules/`/`write_back.rs` is the template);
`schema/type_plane/derive.rs` extractor → `DirectSchemaSet`. Reuses `unionfind.rs`,
`egraph.rs`, `analysis.rs`, `csp_scheduler.rs`, `extract.rs`.

**Gates.** (G1–G5 of PROTO-2, all HELD) injectivity, finite recursive extraction,
order-independence, idempotence, width+injectivity convergence. **The live make-or-break is now
G6:** does native cyclicity ever change the *output schema* vs 1A's nominal fold on a real
grammar — or is the advantage cosmetic given the thin residue? **Un-run — Brief 2 targets it.**

**Exact open questions.** (O1) Is the recursion-axis win worth an engine amendment +
AC-tagging + a whole net-new Language, when cost-extraction is vacuous (PROTO-1)? (O2)
constructor injective-vs-AC tagging is a non-negotiable design obligation carried from PROTO-2
finding (4). (O3) determinism of extraction under real `FxHashMap` iteration at grammar scale
(PROTO-2 proved it on a 10-class fixture; GN-3.W3 byte-identity has teeth at 191-rule CSS).

**Not BLOCKED-candidate on the missing-primitive test** (injectivity is small, classical,
proven). **BANK** behind Brief 2: highest net-new substrate, engine idle on the dominant
judgment and empty on the residue, sole surviving advantage (native recursion) possibly
cosmetic.

---

## 5. THE INCOMPATIBILITY MAP

The families are **not** four coequal rivals. One exclusive axis (the equality engine) carries
three mutually-exclusive members; one cross-cutting discipline (1C) folds onto the winner.

### 5.1 The exclusive axis — the equality engine (choose exactly one)

| | 1A union-find | 1B CSP lattice | 1D e-graph |
|---|---|---|---|
| equality primitive | Robinson `unify` (near-linear) | `TypeDomain::join` (monotonic sweep, re-scans; **overwrite-unsound as shipped**) | `union`+`rebuild`+injectivity |
| seed | PROVE-08 / skinny `TypeFacts` | **running** `types/constraint/` (has consumers) | grammar e-graph (template; type tier net-new + engine amendment) |
| lineage | skinny | root | root |

**Why they cannot coexist as "the spine":** each defines *equality* over a *different variable
store* (union-find map vs `Csp` arena vs e-class union-find+hashcons), a different type of
record (`TypeDesc` vs fused monotype vs `TypeENode`), and one owner of the `Project`
discard-site. Running two equality engines is the "two type systems" failure both research-A R6
and research-B indict; the greenfield ports **one** organ. **1A ⊥ 1B ⊥ 1D pairwise on the seat.**

- **1A ⊥ 1B** — but they **collapse toward each other under contact**: R-B1 forces 1B to the
  federated form, which *is* 1A's stratification (equality trunk + finite residue); PROTO-1's
  THIN verdict removes 1B's only distinguishing claim; the live residual difference is merely
  "union-find map vs `Csp<TypeDomain>` as the equality carrier," and O1 shows the CSP carrier is
  currently unsound. Adjudicated by Brief 1.
- **1A ⊥ 1D** — union-find vs congruence-closure. PROTO-2 shows 1D *beats* 1A on recursion
  (free finite μ-extraction) but PROTO-1 shows 1D's cost-extraction is vacuous. The open seat
  question reduces to: does 1D's recursion win change any real output schema (Brief 2)? One store
  owns `Project`; not both.
- **1B ⊥ 1D** — both root-lineage, both claimed the *same* residue as justification; PROTO-1
  **empties that residue for both**. Neither is justified over 1A on the merits; they still
  cannot co-own the `TypeDesc`→schema lowering.

### 5.2 The cross-cutting discipline — 1C is not on the axis

**1C is compatible with all three engines** — it is a checking front + SCC seam + span field
over an equality reconciler it does not define (it reuses PROVE-08's `Unifier`, sharing 1A's
mechanism). Per the Constitution's shared-mechanism rule it is closest kin to 1A. **1C ∦
(1A|1B|1D): it folds onto whichever wins.** Its only incompatibility is degenerate: with no
`->` in the real corpus, 1C ≡ its host engine until typed grammars exist.

### 5.3 The shared residue — a convergence point PROTO-1 collapsed

All four routed widths/overloads/decode-class/capacity to a bounded solver. PROTO-1 measured
that residue THIN and its overload/cost fraction **exactly 0** → **an ordered default lattice
dominates all four here** (priorart §2G). This is no longer an open fork; whoever wins the
engine axis inherits an ordered-defaulting residue solver, not a CSP and not an extractor.

---

## 6. BLOCKED-CANDIDATE REGISTER + SYNTHESIS FLOOR

- **1A — none. ADVANCE (the spine).** Liability: lineage displacement (O3) + undecided row
  discipline (O1), both currently unmeasurable (no typed grammars).
- **1B — the literal single heterogeneous store is BLOCKED-candidate** (R-B1: no primitive short
  of per-revise tag dispatch). The federation survives but *is 1A*; its equality is unsound as
  shipped (O1). **ADVANCE federation as system-of-record; BANK the thesis BLOCKED-candidate.**
- **1C — not blocked; not an engine family. FOLD** as a discipline layer; value unmeasurable
  until typed grammars exist.
- **1D — not BLOCKED-candidate** (injectivity proven, PROTO-2). **BANK — narrow recursion-axis
  keep only:** cost-extraction is vacuous on the evidence (PROTO-1), engine idle on dominant
  equality, sole advantage (native recursion) possibly cosmetic (Brief 2), and it costs an engine
  amendment + AC-tagging + a net-new Language.

**Synthesis floor for pass-2 (measured, not tasted).** The spine is **a union-find equality
trunk (1A) + an ordered defaulting lattice for the thin residue (PROTO-1) + a bidirectional
diagnostics layer (1C folded in)**, seeded by **one** equality carrier. The remaining live fork
is which carrier: 1A's union-find (raises clashes correctly) vs the running root
`Csp<TypeDomain>` (has consumers, but overwrite-unsound as shipped). Two un-run measurements
decide it (§7). 1B's one-store and 1D's cost-extraction are **evidence-retired**; 1D keeps a
watching brief on the recursion axis alone.

---

## 7. THE TWO SHARPEST UNCERTAINTIES → prototype briefs (both un-run this pass)

PROTO-1 (census) and PROTO-2 (1D injectivity) are done; the next two adjudicate the *surviving*
fork — which sound equality carrier seeds the 1A-shaped spine — against running code.

**Brief 1 — CSP-equality soundness (bbnf-worktree).** Does the shipping `TypeDomain::join`
(`constraint/domain.rs:72-77`) raise a clash on two incompatible `->` arrows, or silently
last-writer-win (Wand's bug, in production)? This decides whether the running root CSP can be
the spine's equality carrier *as-is*, or must be repaired to raise-on-disagreement (= union-find
behavior, ceding distinctiveness to 1A). Confirmed-defect-targeting, immediately runnable.

**Brief 2 — 1A-nominal-fold vs running-CSP recursion output parity (bbnf-worktree).** PROTO-2
proved 1D's native cyclicity *beats* a nominal fold in the abstract, but the **running root CSP
already derives recursive `value` without an e-graph** (JSON compiled in PROTO-1, `value` arity
6). Does the running pass produce a correct, finite, terminating schema for the deepest real
recursive grammars, byte-equal to 1D's e-graph extraction — establishing whether 1D's recursion
advantage buys *anything the shipping system lacks*, or is cosmetic given the thin residue? This
is the decisive retire-or-keep for 1D now that cost-extraction is off the table.
