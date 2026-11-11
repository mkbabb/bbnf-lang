# 28 — SOTA Type Inference for Grammar-Derived Typed ASTs

## Thesis

A BBNF grammar attaches typed projections to productions via the `->` operator; the
compiler must turn those projections into a coherent, backend-agnostic type for every
rule, check user annotations against inferred types, and lower the result to native
Rust (now) and TypeScript (later). The frontier question is: *what is the strongest
type-inference core that is actually right-sized for this domain* — neither the toy
lattice that exists today nor a full research-grade dependent/higher-rank system that
the domain cannot use.

The answer this report defends: a **three-layer inference core** —

1. a **structural type projection** fold for grammar combinators (already the right
   tool, mis-named today),
2. a small **rank-1 Hindley–Milner unification core with let-polymorphism** for the
   two genuinely open-ended surfaces (grammar-closure generics and `->`/`@host`
   value expressions and chains), and
3. **closed-world row-and-sum typing** for record-shaped and alternation-shaped
   productions, resolved during projection rather than reconstructed after it.

Bidirectional checking is the *interface discipline* that glues the three together
(annotations check, un-annotated sites synthesize). Algebraic subtyping (MLsub /
Simple-sub / MLstruct) is the most seductive alternative and is explicitly *not*
recommended as the core — its power is aimed at a problem BBNF does not have, and its
compact-type machinery pays a complexity tax the grammar domain never redeems. This
report gives the reasoning behind each of those calls and the concrete algorithmic
shape to build.

---

## 1. Ground truth: what exists at HEAD, and what the domain actually is

The type system that ships today is a **monotone lattice fold over a closed
`TypeDesc` enum**, not the inference it is documented to be. Verified at HEAD:

- `TypeDesc` is a closed enum with scalar leaves (`Span`, `F64`, `Bool`, `I8/U8/I64/
  U64`), three compound constructors (`Option(Box)`, `Vec(Box)`, `Tuple(Vec)`), an
  opaque `Named(StringId)`, and two erasure sinks `BoxedEnum` and
  `HeterogeneousAltJoin(Vec<TypeDesc>)`
  (`crates/ir/src/types/type_desc.rs:13-63`). There is **no type variable, no record/
  row constructor, and no tagged-sum constructor**.
- The `->` value algebra `MapExpr` is a full expression tree — `IntLit`, `FloatLit`,
  `InputProp{prop}`, `FnCall{name,args}`, `BinOp{op,lhs,rhs}` — with a working
  `constant_fold` (`crates/ir/src/types/map_expr.rs:17-128`) but **no `synth`, no
  `infer_type`, no `type_of`** (grep of that file returns nothing). Its type is never
  computed.
- The map constraint takes the rule type **only** from the explicit annotation,
  defaulting to `Span`: `return_type.clone().unwrap_or(TypeDesc::Span)`
  (`crates/ir/src/passes/types/generate.rs:371` and again `:399`). An un-annotated
  `-> input.len * 2` is silently typed `Span`.
- There is **no Hindley–Milner machinery anywhere**: no `TyVar`, no `Scheme`, no
  `unify` over types, no `instantiate`/`generalize`, no `passes::layout` module. (The
  only `unify`/`fn unify` hits are in `passes/csp_strategy/constraints` and
  `passes/payload` — those are CSP-domain revisions and payload layout, not type
  unification.)
- Heterogeneous `Alt` branches that disagree are lifted to
  `HeterogeneousAltJoin`, which `is_boxed_enum_layout` collapses back to a single
  `Box<Enum>` codegen (`crates/ir/src/types/type_desc.rs:197-209`) — an erasure
  dressed as a diagnostic, not a resolved sum type. `join_types`
  (`crates/ir/src/passes/types/constraint/revise.rs:116`) is a least-upper-bound over
  the flat lattice with a CSS-named escape hatch.

So the domain, stated precisely:

- **Grammar structure is monomorphic and total.** `Seq/Alt/Repeat/Optional/Skip`
  projection is a total function of child types. There is nothing to *solve* here —
  it is a bottom-up fold, and the current code is correct at it. The only genuinely
  iterative edge is rule-to-rule `Ref`, which converges by a `Changed`-bool fixed
  point with a cycle-break to `BoxedEnum`.
- **Two surfaces are actually open-ended.** (a) The `->` value expressions and
  `-> f1 -> f2 -> f3` host chains — arithmetic and function composition whose result
  type is *derived*, not declared. (b) Grammar closures / generic rules
  (`Object<V>`, `List<T>`), which today are beta-reduced at lowering
  (monomorphized-as-macro) and thus never carry a polymorphic scheme.
- **Two shapes are structurally rich.** Record-shaped productions (a rule whose
  projection is a set of named fields — the CSS declaration case, the JSON object
  case) and alternation-shaped productions (a rule that is genuinely a tagged union).
  Today both collapse to `Named` opacity or `BoxedEnum`.

This shape — first-order, mostly-monomorphic, with two narrow polymorphic surfaces
and two rich data shapes — is the single most important fact for sizing the
algorithm. It rules some things in and some things decisively out.

---

## 2. The right-sizing decision, candidate by candidate

### (a1) Rank-1 Hindley–Milner with let-polymorphism — **YES, as the unifier core**

Damas–Milner (Algorithm W / the modern Algorithm J with union-find and ranked
levels) is the correct *core* for the two open-ended surfaces, for four reasons:

1. **Principality is free and load-bearing.** The domain has no GADTs, no higher-rank
   polymorphism, no impredicativity — the exact features that a 2021 ACM Computing
   Surveys treatment and the 2013 Dunfield–Krishnaswami result identify as the things
   that *break* HM principality
   (`https://dl.acm.org/doi/10.1145/3450952`,
   `https://www.cl.cam.ac.uk/~nk480/bidir.pdf`). Because BBNF stays inside the
   fragment where every well-typed term has a unique most general type, HM gives
   principal schemes with no caveats. That is exactly what a *generic rule*
   (`List<T>`, `Object<V>`) needs: infer `List : ∀a. Elem(a) -> Vec<a>` once,
   instantiate per call site. This is the capability the current
   monomorphize-at-lowering approach forecloses.
2. **The unifier is the one tool that types host chains correctly.** `-> f1 -> f2`
   requires unifying `f1`'s output with `f2`'s input; a lattice fold cannot express
   "these two unknowns must be equal." A first-order syntactic unifier with the
   occurs-check is ~150 lines and is the minimum viable mechanism for chain typing
   and for `@host fn` signature checking (`BBNF-HOST-SIGNATURE-MISMATCH` becomes a
   real unification failure, not a dead diagnostic).
3. **It composes with the existing fold, it does not replace it.** HM does not have
   to run over the whole grammar. The structural projection fold stays for
   `Seq/Alt/Repeat/Optional`. The unifier is invoked only at (i) generic-rule
   instantiation and (ii) `MapExpr`/`FnCall`/chain edges. This keeps the fast,
   correct part fast and correct, and pays the unification cost only where inference
   is genuinely required.
4. **Modern HM is incremental-friendly.** Rémy-style *level/rank-based
   generalization* (the technique OCaml uses) replaces the O(n) free-variable scan of
   textbook `generalize` with an O(1) level stamp, and it is the mechanism that makes
   HM cheap to re-run — the substrate on which Salsa-style incrementality (§b) sits.

The scope is **rank-1** deliberately. Rank-1 (let-polymorphism, prenex ∀) is
decidable, principal, and syntax-directed. Higher-rank is unnecessary: BBNF has no
place to write a polymorphic *argument*.

### (a2) Bidirectional checking — **YES, as the interface discipline, not the core**

Bidirectional typing (Pierce–Turner local type inference; the Dunfield survey) is not
an *alternative* to HM here — it is the *contract for annotations*. The architecture's
own stated intent is "annotations check inferred types rather than bypassing them,"
which is precisely the check-vs-synthesize split. The rule:

- **Synthesize (⇒)** at un-annotated `->` bodies, `FnCall`s, and generic-rule bodies:
  the type flows *up* from structure.
- **Check (⇐)** an explicit `-> T` annotation and each `@host fn : ... -> R`
  declaration: the annotation is the *expected* type pushed *down*, and the inferred
  type must unify with it.

This is what turns today's `unwrap_or(Span)` (`generate.rs:371`) — annotation
*replaces* inference — into annotation *checks* inference. Bidirectionality also buys
the good error-message locus the survey literature credits it with: the failure is
reported at the annotation edge, not deep in a substitution. Crucially,
bidirectionality *reduces* the amount of unification variables in flight (checking
mode consumes an expected type instead of minting a fresh var), which keeps schemes
compact without any subtyping machinery.

### (a3) Algebraic subtyping — MLsub / Simple-sub / MLstruct — **NO for the core**

This is the most important negative recommendation, because it is the most tempting.
Algebraic subtyping (Dolan's MLsub, `https://dl.acm.org/doi/10.1145/3409006`;
Parreaux's Simple-sub, `https://lptk.github.io/programming/2020/03/26/demystifying-mlsub.html`;
MLstruct with Boolean-algebra structural types) delivers principal type inference *in
the presence of subtyping* — union/intersection types, width-and-depth record
subtyping, all inferred with no annotations, Simple-sub in under 500 lines.

The reason it is wrong here is that **BBNF's subtyping obligations are trivial and
directional, not a lattice that needs biunification.** The places BBNF "needs
subtyping" are: `f32 → f64` promotion (already hard-coded,
`type_desc.rs:81`), `Span` as a universal shorthand, and heterogeneous `Alt`
branches. The first two are coercions, not subtyping. The third is genuinely a union
— but it is a *closed, first-order, non-recursive* union over grammar branches, which
is exactly a tagged sum (§c), not the polar-variable union-with-intersection that
MLsub's compact-type coalescing exists to simplify. Adopting algebraic subtyping would
mean:

- Every type becomes a pair of polar (positive/negative) variables with implied
  subtyping constraints, decomposed then coalesced
  (`https://arxiv.org/pdf/2409.01841` BinSub restates the mechanism). That polar
  machinery is pure overhead for a domain whose "subtyping" is a two-element coercion
  table.
- The compact-type *simplification* pass (the hard, subtle part — the thing MLsub
  itself is criticized for being "difficult to understand") exists to make inferred
  union/intersection types readable. BBNF emits to Rust structs/enums; it does not
  surface inferred subtyping types to a user at all. The entire payoff is unspent.
- It fights the multi-backend goal (§e): Rust and TS both model unions as *tagged*
  (enum / discriminated union), not as structural subtype lattices. A structurally-
  subtyped inferred type has to be *reified* back into a tag anyway.

The one idea worth *stealing* from Simple-sub is its engineering ethos — mutation-
based union-find, constraints-then-coalesce, principality by construction — which is
exactly what a well-built rank-1 HM core already gives without the polar-variable
tax. The extensible-records-for-Simple-sub line
(`https://arxiv.org/abs/2407.06747`, 2024) is interesting for a genuinely open-world
record language; BBNF's records are closed-world (see §c), so it is not needed.

### (a4) Row polymorphism for record-shaped productions — **YES, but closed-world**

Record-shaped productions (CSS declarations sharing a `property`/`value` shape,
JSON objects, `Object<V>`) are the highest-leverage upgrade: they replace the opaque
`Named(StringId)` sink (`type_desc.rs:47`) and the *post-hoc* `StructRegistry`
reconstruction with a `TypeDesc::Record(Row)` produced *during* projection, so that
`obj.field` path access type-checks against an inferred row instead of a side-table.

The decisive sizing call: **closed rows (no row-extension variable) suffice.** Full
row polymorphism (Wand / Gaster–Jones presence-absence types; Rémy; the 2025
"Extensible Data Types with Ad-Hoc Polymorphism" line,
`https://dl.acm.org/doi/10.1145/3776662`) exists to type *record extension* — adding a
field to an unknown record — while preserving principality. BBNF productions do not
extend records at runtime; each production has a *statically known, complete* field
set. So what is needed is **records with a row of known labels**, plus a single
polymorphic *field-type* variable when the field type varies across instances
(`Object<V>` = record whose value column is `V`). That is row-*shaped* records with
HM field-type variables — not Rémy row *extension* variables. This keeps records
inside the rank-1 HM fragment (a record type is just an n-ary constructor over its
field types) and preserves principality without the presence/absence lattice.

### (a5) CSP / unification hybrids — **the fold stays, but re-home it off the CSP crate**

The current type pass is built on the generic `csp-solver` crate driven in a
singleton-domain (`Option<TypeDesc>`) monotone-lattice mode
(`crates/ir/src/passes/types/constraint/*`). This is a *category error in naming*: a
singleton domain admits no search, no backtracking, no narrowing — it is a worklist
dataflow fold, not constraint satisfaction, and calling it "AC-3" misleads every
reader. The correct hybrid is the inverse of what "CSP type inference" usually means:

- **Unification handles equality constraints** (chains, generic instantiation, host
  edges) — this is what union-find does and what CSP domains do badly.
- **The finite CSP stays where it belongs**: bounded *implementation* choices —
  layout selection, recognizer eligibility, materialization strategy
  (`passes/csp_strategy/*`) — where a genuine domain of candidate values is narrowed.
- **The structural fold** (grammar projection) becomes a plain worklist over
  `TypeDesc`, off the CSP crate, so the "search" framing disappears.

There *is* one legitimate CSP-flavored idea in the type literature worth noting:
type-inference-as-constraint-generation-then-solving (HM(X), the constraint-based
presentation of HM). That is not a reason to keep the CSP crate — it is a reason to
structure the HM core as *generate equality/instance constraints, then solve by
unification*, which is the standard modern HM presentation and is incremental-friendly
(§b). Constraints yes; finite-domain CSP for *types*, no.

---

## 3. Incrementality: Datalog/egglog and Salsa — where each belongs

**Salsa-style query incrementality — YES, as the outer shell.** rust-analyzer's core
invariant — "typing inside a function body never invalidates global derived data" —
maps directly onto BBNF: typing one rule's `->` body should not re-run projection for
the whole grammar (`https://rustc-dev-guide.rust-lang.org/queries/salsa.html`,
`https://rust-analyzer.github.io/book/contributing/architecture.html`). The natural
query graph is: per-rule structural projection → per-rule `MapExpr` synthesis →
whole-grammar `Ref` fixpoint → registry (records/sums). Rémy-level generalization (§a1)
is what makes each query cheap to recompute. This is the right long-run architecture
for editor/watch-mode responsiveness and for the skinny tree's fast-iteration goal —
but it is an *engineering shell around* the algorithm, not the algorithm.

**egglog / Datalog type inference — NO for the type core; the e-graph stays a
consumer.** egglog unifies Datalog and equality saturation with semi-naïve
incrementality (`https://dl.acm.org/doi/10.1145/3591239`, PLDI 2023; PLDI 2025
tutorial). Two facts from the egglog literature settle this: (1) the egglog authors
themselves note that *e-class analysis is the wrong abstraction for type inference*,
because analysis values propagate bottom-up while typing contexts must propagate
top-down; type inference in egglog has to be encoded as *Datalog rules*, not e-class
analysis. (2) BBNF's e-graph is already correctly scoped as a **cost/rewrite side-
table that reads `TypeDesc` read-only** to decide scalar-vs-compound rewrite
eligibility — it never produces a type, and it must not start. Pushing typing into the
e-graph would invert a clean consumer relationship into a circular one. Datalog *is*
a reasonable engine for the whole-grammar `Ref` fixpoint (mutually-recursive rule
types as a least fixed point is a natural Datalog computation, and semi-naïve
evaluation gives incrementality for free) — but that is a small, optional
implementation choice for one pass, not the inference paradigm.

---

## 4. Prior art in parser-generator typing — what to borrow, what to beat

- **tree-sitter static node types** (`node-types.json` → typed wrappers via
  `type-sitter`, `dts-tree-sitter`, ast-grep's typed napi;
  `https://tree-sitter.github.io/tree-sitter/using-parsers/6-static-node-types.html`).
  This is the closest living analog to the BBNF goal and the bar to beat. tree-sitter
  *emits a JSON description of every node's children/fields* and downstream tools
  generate typed wrappers. Its limitation is exactly BBNF's opportunity: node types
  are **structural and untyped at the leaf** — every field is "a node," there is no
  `-> f64`, no value projection, no sum-of-typed-variants. BBNF's `->` gives it *leaf
  semantics* tree-sitter lacks. Borrow the *artifact* idea (emit a backend-agnostic
  type manifest; §e), beat it on *typed leaves and inferred value expressions*.
- **LALRPOP** (`https://lalrpop.github.io/lalrpop/`). LALRPOP "offers type inference so
  you can often omit the types of nonterminals," with compact defaults. But this is
  *Rust's* inference doing the work: LALRPOP emits action code and lets `rustc` infer
  the nonterminal type. That is a defensible shortcut for a Rust-only generator and a
  cautionary tale for BBNF: it is **not backend-agnostic** (TS has no equivalent
  inference over emitted actions) and it gives *no* pre-emission type manifest. BBNF
  must infer types *itself*, in its own `TypeDesc`, precisely so the same inference
  serves Rust and TS.
- **Menhir** (`https://gallium.inria.fr/~fpottier/menhir/manual.html`). Semantic
  actions are typed by *OCaml*, and nonterminal types are declared or inferred by the
  host compiler — same delegation as LALRPOP, same non-portability. Menhir's real
  lesson is orthogonal: predictable linear-time actions and inspection API discipline.
- **ANTLR.** Typed visitor/listener generation is *structural* — one method per rule,
  return type `Object`/generic parameter, no value-level type inference. BBNF's `->`
  is strictly more expressive; ANTLR is a floor, not a target.
- **Typed combinator libraries — chumsky, winnow, nom.** chumsky uses *the Rust trait
  system itself* as the type checker: parser types are `impl Parser<..., Output = T>`,
  and long `.or()` chains cause "exponential behavior in Rust's trait solver" and long
  compile times (`https://lib.rs/crates/chumsky`); winnow had to add `PhantomData`
  scaffolding to help inference (`https://epage.github.io/blog/2023/07/`). The lesson
  is a *warning*: BBNF must **not** offload inference onto the backend's type system —
  that path does not port to TS and it blows up compile time. BBNF's own `TypeDesc`
  inference, resolved *before* emission, is the correct architecture and its
  differentiator: the generated Rust/TS is already monomorphic and trivially typed, so
  neither backend's solver does any inference work.

The synthesis across all five: **every mainstream parser generator either does no
value-level typing (ANTLR, tree-sitter leaves) or offloads it to the host language
(LALRPOP, Menhir, chumsky).** BBNF's lacuna-and-opportunity is to own a *portable,
pre-emission* inference over `->` projections. There is no prior art doing this well;
it is genuinely novel ground, which is why the algorithmic core must be built, not
imported.

---

## 5. Multi-backend: what "most powerful reasonably possible" means concretely

"Most powerful inference reasonably possible" for grammar→maps targeting Rust-now /
TS-later resolves to a precise, buildable specification:

**The core: rank-1 HM (Algorithm J, union-find + Rémy levels) + bidirectional
check/synth, over a `TypeDesc` extended with type variables, records, and sums.** The
inference produces backend-agnostic `TypeDesc` (`TypeDesc::Named` is already the
backend-agnostic device — Rust resolves it to a struct/enum, TS to an interface/union;
`type_desc.rs:76-82`). The pipeline:

1. **Structural projection fold** over `Seq/Alt/Repeat/Optional/Skip` — unchanged in
   spirit, re-homed off the CSP crate, renamed *structural type projection*. Total,
   fast, principal by construction. (Keeps the correct part correct.)
2. **`MapExpr::synth(env) -> TypeDesc`** — bottom-up synthesis over the closed operator
   set (`IntLit⇒I64`, `FloatLit⇒F64`, numeric `BinOp⇒` operand join, comparison
   `⇒Bool`, `InputProp{len}⇒` width int, `FnCall⇒` host-signature lookup). ~80 lines,
   trivially principal because the algebra is monomorphic. This is the single change
   that kills the `unwrap_or(Span)` lie (`generate.rs:371`,`:399`) and makes
   annotations *check* instead of *replace*.
3. **A `HostSigTable: name → (params, ret)`** from `@host fn` decls, consulted at
   `FnCall` and at `-> f1 -> f2` chain edges via the first-order unifier. This is the
   *only* place unification is strictly required for the value layer, and it makes
   `BBNF-HOST-SIGNATURE-MISMATCH` a real unification failure.
4. **Generic rules typed with rank-1 schemes** — instead of beta-reducing closures at
   lowering, keep `GrammarCall` to the type layer, infer `List : ∀a. … -> Vec<a>`,
   instantiate per call. (This is the one place the current
   monomorphize-as-macro forecloses real power; it is what lets `Object<V>` be *one*
   inferred scheme rather than N copies.)
5. **Records and sums resolved during projection** — `TypeDesc::Record(Row)` (closed
   row, §a4) replaces `Named` opacity for field-shaped rules; `TypeDesc::Sum(Vec<
   (Tag, TypeDesc)>)` replaces `HeterogeneousAltJoin`/`BoxedEnum` erasure
   (`type_desc.rs:63,197-209`) so a heterogeneous `Alt` becomes a *typed tagged union*
   the registry consumes directly, not a `Box<Enum>` collapse.
6. **A backend-agnostic type manifest** as the emission interface: the inferred
   `TypeDesc` graph is the single source both the Rust emitter and the (future) TS
   emitter consume. Rust: `Record→struct`, `Sum→enum`, `Vec→Vec`, `Option→Option`,
   `Span→parse_that::Span`. TS: `Record→interface`, `Sum→discriminated union`,
   `Vec→T[]`, `Option→T|null`, `Span→string`. Because inference is complete *before*
   emission, neither backend's own type checker does any inference — the chumsky/
   LALRPOP portability trap is structurally avoided.

**Explicitly out of scope** (the "reasonably" in "reasonably possible"): higher-rank
polymorphism, impredicativity, GADTs, subtyping-as-a-lattice (MLsub polar variables),
open-world row extension, dependent types, effect typing. Each is a real research
frontier and each is dead weight for a first-order grammar value algebra. Including any
of them trades principality-for-free and small-code-size for expressiveness the domain
cannot spend — the precise mistake §a3 warns against.

**Incrementality shell** (§3): wrap the six-step pipeline in Salsa-style queries keyed
per-rule, with Rémy-level generalization making recompute O(1)-ish; keep the e-graph a
downstream read-only consumer of the richer `TypeDesc`.

---

## 6. Concrete recommendation, named

**Algorithmic core: constraint-based rank-1 Hindley–Milner — Algorithm J with a
union-find unifier and Rémy/OCaml-style level-ranked generalization — driven
bidirectionally (synthesize at un-annotated projections, check at annotations and
`@host` signatures), extended with closed-row records and first-order tagged sums,
sitting atop the existing structural projection fold for grammar combinators, wrapped
in a Salsa-style per-rule incremental query graph.** Constraints are generated and
solved by unification (the HM(X)/constraint presentation), never by a finite-domain
CSP; the CSP crate is retained only for genuine implementation-choice search, and the
e-graph remains a read-only cost/rewrite consumer of inferred types.

Named alternatives and their verdicts, for the record:

- *Full Damas–Milner / higher-rank (Dunfield–Krishnaswami):* over-powered; principality
  is already free in the rank-1 fragment BBNF inhabits.
- *Algebraic subtyping (MLsub/Simple-sub/MLstruct):* wrong target — its power addresses
  inferred subtyping BBNF does not have; borrow the union-find engineering ethos, not
  the polar-variable/coalescing machinery.
- *Omnidirectional inference (arXiv 2511.10343, 2025 — "principality any way," via
  suspended match constraints and incremental instantiation):* genuinely SOTA and the
  most relevant *new* result, but it exists to preserve principality under
  *order-sensitive fragile constructs* (GADTs, first-class polymorphism, label
  overloading). BBNF has none of these, so the flexibility is unused; its *idea* —
  constraints that suspend until information is available — is however the right mental
  model for the `Ref` fixpoint and for a Salsa-incremental scheduler, and worth reading
  before building the constraint solver.
- *egglog/Datalog as the type engine:* rejected by the egglog authors' own analysis
  (e-class analysis is bottom-up, typing needs top-down contexts); Datalog is at most a
  clean engine for the recursive-`Ref` least-fixpoint, not the paradigm.

The build order that respects the domain: (1) `MapExpr::synth` + annotation-as-check;
(2) `HostSigTable` + first-order unifier for host edges and chains; (3)
`TypeDesc::Record`/`Sum` during projection, retiring `HeterogeneousAltJoin`-as-type;
(4) rank-1 schemes for generic rules (un-monomorphize the closure at the type layer);
(5) re-home the structural fold off `csp-solver` onto a plain worklist / Datalog
fixpoint; (6) Salsa-query the whole thing for skinny-tree iteration speed. Steps 1–3
are the load-bearing honesty-and-capability fixes; 4–6 are the power-and-speed
frontier.

---

## Sources

- Bidirectional Typing survey — `https://dl.acm.org/doi/10.1145/3450952`
- Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism
  (Dunfield–Krishnaswami) — `https://www.cl.cam.ac.uk/~nk480/bidir.pdf`
- Omnidirectional type inference for ML: principality any way (2025) —
  `https://arxiv.org/abs/2511.10343`
- The Simple Essence of Algebraic Subtyping (Simple-sub, ICFP 2020) —
  `https://dl.acm.org/doi/10.1145/3409006`,
  `https://lptk.github.io/programming/2020/03/26/demystifying-mlsub.html`
- Towards Algebraic Subtyping for Extensible Records (2024) —
  `https://arxiv.org/abs/2407.06747`
- BinSub: Simple Essence of Polymorphic Type Inference for Machine Code (2024) —
  `https://arxiv.org/pdf/2409.01841`
- Extensible Data Types with Ad-Hoc Polymorphism (2025) —
  `https://dl.acm.org/doi/10.1145/3776662`; Row polymorphism —
  `https://en.wikipedia.org/wiki/Row_polymorphism`
- Better Together: Unifying Datalog and Equality Saturation (egglog, PLDI 2023) —
  `https://dl.acm.org/doi/10.1145/3591239`; PLDI 2025 tutorial —
  `https://pldi25.sigplan.org/details/pldi-2025-tutorials/4/`
- Salsa / rust-analyzer incremental architecture —
  `https://rustc-dev-guide.rust-lang.org/queries/salsa.html`,
  `https://rust-analyzer.github.io/book/contributing/architecture.html`
- tree-sitter static node types —
  `https://tree-sitter.github.io/tree-sitter/using-parsers/6-static-node-types.html`;
  type-sitter — `https://github.com/Jakobeha/type-sitter`
- LALRPOP — `https://lalrpop.github.io/lalrpop/`; Menhir —
  `https://gallium.inria.fr/~fpottier/menhir/manual.html`
- chumsky — `https://lib.rs/crates/chumsky`; winnow —
  `https://epage.github.io/blog/2023/07/winnow-0-5-the-fastest-rust-parser-combinator-library/`
