# Deferral audit — type system

Audit role: deferral-audit agent #1 — TYPE SYSTEM. Greenfield mandate is
audacious-functional, Rust-like ergonomics, **inference stronger than Rust if
possible**, schema generation without explicit annotations wherever HM-class
algorithms can recover them. The 14-lock set is settled; the type-system
**boundaries inside Lock 4 + Lock 10** are not, and the V6 corpus has drawn
those boundaries conservatively. This audit catalogues the conservative cuts,
proposes folds where the architectural-nailing value justifies the V1 cost,
and routes the residue.

## §1 — Scope and corpus references

Required reading consumed in full:

| Surface | Path | Lines |
|---|---|---:|
| Anthem and gestalt | `restart/README.md` | 1-479 |
| Type system block | `restart/README.md` | 260-273 |
| Locks (4, 10, 14) | `restart/locks/LOCKS.md` | 30-60 |
| Type system | `restart/ARCHITECTURE.md` | 1067-1208 |
| Optimization (codegen lower) | `restart/ARCHITECTURE.md` | 1263-1281 |
| Substrate verdicts and algorithm | `restart/audit/pass-1-substrate/PASS-1.md` | 1-298 |
| HM foundations | `restart/research/topic-1-hm-foundations.md` | 1-737 |
| Bidirectional (Pierce-Turner / DK) | `restart/research/topic-2-bidirectional.md` | 1-532 |
| CSP / GADTs / generics | `restart/research/topic-3-csp-gadts.md` | 1-594 |
| Pass-1 fold | `restart/research/fold-pass-1.md` | 1-235 |
| Synthesis fold | `restart/research/fold-synthesis.md` | 1-358 |
| V6 hardening | `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md` | 1-391 |

Audit lens: every type-system commitment in the V6 corpus is read for the
**explicit deferral language** (post-V1, future amendment, reserved, out of
V1, future-gated, future proof gate, requires amendment) plus **implicit
deferral** (annotations the author must write today that audacious inference
could eliminate; surfaces the grammar IR cannot represent today; algorithms
the corpus declines to deploy).

The user's posture: V1 is the architecture-nailing pass. Folds that prevent
future refactor (changing Grammar IR variants, BIR variants, the public
checker API, the side-table contract) are HIGH greenfield value even at
Medium V1 cost. Folds that only enrich existing surfaces with no architectural
shift are MEDIUM. Folds whose only value is research-novelty — without a
foreseeable user — stay DEFER.

## §2 — Identified deferrals

### §2.1 Higher-rank polymorphism (System F, ML rank-2, Dunfield-Krishnaswami)

| Field | Value |
|---|---|
| Source | `restart/README.md:264`; `restart/ARCHITECTURE.md:1161-1166`; `restart/audit/pass-1-substrate/PASS-1.md:75` |
| Current language | "Higher-rank, existential, indexed, or GADT-like grammar types are out of V1 unless a later architecture amendment opens a Dunfield-Krishnaswami or OutsideIn-style proof gate with ordered existential contexts, principality tracking, decidability, soundness, and completeness." |
| V1 fold proposal | Adopt **Dunfield-Krishnaswami complete-and-easy bidirectional** (`restart/research/topic-2-bidirectional.md:103-114`) as the V1 algorithmic frame. Rank-1 stays the *default*, rank-N becomes available without re-architecting the checker: ordered algorithmic contexts, existential variables (`α̂`), and the application judgment land V1; explicit `forall` annotation surface in `Type` opens for `@host fn` + generic rule signatures. |
| Implementation impact | `passes/types`: replace plain Robinson unification with DK's ordered-context unification (existential solving + occurs-check + context articulation). `Type` enum gains `Forall(Vec<TypeVar>, Box<Type>)` and `Existential(α̂)`. Grammar surface gains `forall` annotation form for explicit cases; inferred rank-1 stays unannotated. BIR unaffected. Diagnostics gain `BBNF-RANK-N-ANNOTATION` for the rare unannotated rank-N program. |
| Risk | Medium. DK13 is decidable and has a published-correct algorithm (Pfenning-Krishnaswami sound + complete). Implementation surface is ~600-1200 LOC for the checker core; principality reasoning is the subtle part but well-documented. |
| Greenfield fold value | **HIGH.** The substrate of the checker is the load-bearing artefact — adding higher-rank later means re-typing every constraint shape, re-tooling diagnostics, and re-validating principality. DK's algorithm is forward-compatible with rank-1: rank-1 programs typecheck identically; rank-N becomes a graceful extension. |
| Recommendation | **FOLD.** This is the canonical "nail it now or refactor everything later" decision. |

### §2.2 GADTs / branch-local equality / OutsideIn(X)

| Field | Value |
|---|---|
| Source | `restart/README.md:264`; `restart/ARCHITECTURE.md:1161-1166`; `restart/audit/pass-1-substrate/PASS-1.md:73,75`; diagnostic `BBNF-LOCAL-EQUALITY-ANNOTATION` reserved at `restart/audit/pass-1-substrate/PASS-1.md:111` |
| Current language | "OutsideIn-style implication constraints are reserved for future local-equality constructs and are not a V1 generic-rule requirement." "V1 has no GADT branch-local equality surface. A later indexed/local-equality feature must arrive by amendment with annotation rules and `BBNF-LOCAL-EQUALITY-ANNOTATION`." |
| V1 fold proposal | DEFER the *user-visible GADT syntax* but FOLD the *substrate*: the type checker carries `Implication { givens: Vec<Equality>, wanted: Vec<Constraint> }` from V1. The DK19 paper (`restart/research/topic-2-bidirectional.md:115-124`) handles existentials and indexed types via implications; if DK13 lands per §2.1, the implication-constraint shape is incremental cost. No grammar syntax for GADTs in V1; the constraint solver is GADT-ready. |
| Implementation impact | Constraint type gains `Implication`. Solver gains given-aware decomposition (givens widen the equational theory locally). No surface change. No BIR impact. |
| Risk | Low-Medium. OutsideIn(X) is well-studied (`restart/research/topic-3-csp-gadts.md:106-117`). Without a user surface, the implication path is dead code in V1, but the *shape* of the checker survives the day GADTs land. |
| Greenfield fold value | **MEDIUM.** Folds with §2.1; isolated, the value is moderate. With DK13 folded, the marginal cost is small and the architectural consistency is large. |
| Recommendation | **FOLD-CONDITIONAL on §2.1.** If §2.1 folds, fold this. If §2.1 defers, defer this. |

### §2.3 Row polymorphism / open structural records / extensible records

| Field | Value |
|---|---|
| Source | `restart/README.md:270`; `restart/ARCHITECTURE.md:1168-1171`; `restart/audit/pass-1-substrate/PASS-1.md:75` |
| Current language | "Record narrowing is limited to finite generated-shape coercions where source and target shapes are both known; row-polymorphism and open structural record subtyping are out of V1." |
| V1 fold proposal | FOLD **Leijen-style scoped labels** (`restart/research/topic-1-hm-foundations.md:234-243`) for **internal** layout reasoning; do not expose row variables in the BBNF surface syntax in V1. The grammar's `RecordType` non-terminal stays closed-shape; the checker carries row variables internally so layout-narrowing decisions across grammars (`{a, b, c}` narrowing to `{a, b}` for a typed projection) become a *single* row-unification step rather than N² generated coercions. |
| Implementation impact | `Type::Record { fields, row: RowVar }` with row absent at the surface (closed by default). Layout-lowering uses row unification to derive narrowing automatically rather than enumerating finite coercion candidates. `BBNF-SUBSUMPTION-EDGE` becomes a row-unification failure rather than a missing-candidate failure. |
| Risk | Medium. Leijen's design is an HM-compatible row calculus with proven equality/unification; integration touches every record-typing site. Diagnostic quality on row mismatches is the main implementation risk. |
| Greenfield fold value | **HIGH.** Record narrowing is one of three coercion classes in `restart/README.md:270`; the current finite-coercion table is a *combinatorial* surface in disguise. Row unification collapses it to one rule. The user's "schema generation without annotations" mandate is much easier with row variables in the internal type. |
| Recommendation | **FOLD (internal only).** Surface row syntax stays deferred; internal row variables land V1. |

### §2.4 Function values / first-class function types as `Type` non-terminals

| Field | Value |
|---|---|
| Source | `restart/ARCHITECTURE.md:1106` (grammar production) and §8.4 (`:1187-1208`); `restart/audit/pass-1-substrate/PASS-1.md:222` (`Type = Ident GenericArgs? \| TupleType \| RecordType \| BorrowType`) |
| Current language | The `Type` production today admits named, tuple, record, and borrow types but **not function types**. `@host fn parse_hex_pair(s: &str) -> u8` declares a function but the function value cannot itself appear *as a type* (e.g. a generic rule cannot be parameterised over a function `Object<V, F: V -> W>`). |
| V1 fold proposal | Extend `Type` with `FnType ::= "fn" "(" TypeList? ")" "->" Type` and `Closure` with a corresponding type. `@host fn` signatures become first-class `Type` values. Generic rules can take function-type parameters: `Object<V, F: fn(&str) -> V>`. |
| Implementation impact | `ir/grammar_ir/`: `Type::Fn { params: Vec<Type>, ret: Box<Type> }`. Backend IR: closure conversion at lowering time (Rust: `dyn Fn` or generic bound depending on call-site monomorphisation; WASM: function-table dispatch). `host/signature/` already represents function arity; promotion to a typed `FnType` is shape-preserving. |
| Risk | Medium. Closure conversion semantics need clarity (capture set, lifetime of captures). The DK13 application judgment (per §2.1) handles function values out of the box. Without DK13 the restoration cost is awkward but tractable. |
| Greenfield fold value | **HIGH.** Without function-as-type, host primitives cannot be parametric over user-supplied transforms; the user's "stronger than Rust" mandate is hollow if `Object<V>` cannot be `Object<V, MapFn: fn(&str) -> V>`. The Backend IR already lowers closures; the Grammar IR is the gap. |
| Recommendation | **FOLD.** Composes naturally with §2.1. |

### §2.5 Closure capture (language-level closures)

| Field | Value |
|---|---|
| Source | `restart/ARCHITECTURE.md:1187-1208` (closure semantics narrow to four forms with explicit "no arbitrary host process state through grammar syntax"); README §8 (`:285-318`) implicitly forbids first-class closures by limiting to four roles |
| Current language | "Closure semantics are intentionally narrow. They exist to model host chains and typed grammar mappings without turning BBNF into a general programming language." Forbidden: capturing arbitrary host process state through grammar syntax. |
| V1 fold proposal | The narrow closure surface stays. The DEFERRED item is **first-class closure values escaping the grammar lexical scope** — closures stored in side tables, returned from rules, etc. Audit verdict: this remains correctly deferred. The four current closure forms (host chain, map, predicate, recovery) cover the intent of grammar-derived semantic schemas. Going further opens semantic complexity (escape analysis, lifetime extension, allocation discipline) without a clear V1 user. |
| Implementation impact | None for V1 fold (item stays deferred). |
| Risk | N/A. |
| Greenfield fold value | **LOW.** The user's mandate is grammar-derived semantic schemas, not a general-purpose language. Closures-as-values would expand the runtime contract into territory the tape/direct-to-struct substrate is not designed to host. |
| Recommendation | **DEFER-WITH-REASON.** The current closure surface is sufficient; the deferral is principled and not a future refactor risk because §2.4's function-as-type fold absorbs the legitimate user-facing case (functions passed in, not captured-and-stored). |

### §2.6 Existential types / abstract data types

| Field | Value |
|---|---|
| Source | `restart/ARCHITECTURE.md:1163` ("higher-rank, existential, indexed, or GADT-like grammar types are out of V1") |
| Current language | Reserved for amendment alongside higher-rank and GADTs. |
| V1 fold proposal | The DK13 ordered-context calculus already carries existentials (`α̂`) for **inference**. Surface existentials (`exists T. T -> u8`) for *programmer-facing abstraction* are a different concern. V1 fold: keep DK existentials internal (§2.1), defer surface existentials. |
| Implementation impact | None beyond §2.1. |
| Risk | Low. |
| Greenfield fold value | **LOW** as a separate item; **MEDIUM** if folded into §2.1. The internal existential machinery is worth folding via §2.1; the surface form is research-novelty without a V1 user. |
| Recommendation | **DEFER (surface)** + **FOLD (internal, via §2.1).** |

### §2.7 Type-level computation / type families / associated types

| Field | Value |
|---|---|
| Source | Implicit; nowhere in the V6 corpus is type-level computation discussed. The `Type` production at `restart/audit/pass-1-substrate/PASS-1.md:222` admits no application syntax. |
| Current language | Silent. |
| V1 fold proposal | DEFER. Type families add a rewrite system to type-checking that interacts non-trivially with HM principality (`restart/research/topic-3-csp-gadts.md:106-117`). The user's mandate is grammar-derived schemas, which the existing type vocabulary covers. |
| Implementation impact | None for fold (defer). |
| Risk | N/A. |
| Greenfield fold value | **LOW.** No grammar in `bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math` has surfaced a type-family-shaped need. |
| Recommendation | **DEFER-WITH-REASON.** The deferral is principled; type families are research-novelty without a V1 user. |

### §2.8 Refinement types / dependent typing

| Field | Value |
|---|---|
| Source | Implicit; not discussed. Idris 2's elaborator is cited only as architectural pressure (`restart/research/topic-2-bidirectional.md:165-175`). |
| Current language | Silent. |
| V1 fold proposal | DEFER. Refinement types collide with the principal-scheme guarantee unless a SMT-backed solver layer joins; the corpus already routes SMT to `cost-model/solve/` rather than the type checker (`restart/audit/pass-1-substrate/PASS-1.md:154`). |
| Implementation impact | None. |
| Risk | N/A. |
| Greenfield fold value | **LOW.** |
| Recommendation | **DEFER-WITH-REASON.** |

### §2.9 Subtyping enhancements beyond bounded coercion

| Field | Value |
|---|---|
| Source | `restart/README.md:270`; `restart/ARCHITECTURE.md:1153`; coercion is currently a finite registered-candidate list at the check/synth edge |
| Current language | Coercion is finite, registered, and explicit at edges (numeric widening, lifetime escalation, generated-record narrowing). Anything else fails with `BBNF-SUBSUMPTION-EDGE`. |
| V1 fold proposal | FOLD **Pottier-style structural subtyping with explicit lower/upper bounds** for the lifetime-coercion class (`&'i str ⊑ Cow<'i, str> ⊑ String`) — the V6 corpus already implies a partial order; making it a formal subtyping lattice with `BoundedTypeVar { lower: Vec<Type>, upper: Vec<Type> }` is a natural generalisation. The numeric and record classes stay finite-coercion. |
| Implementation impact | `Type::BoundedVar` joins `Type::Var`; the unifier learns lattice-aware constraint solving for the lifetime axis only. |
| Risk | Medium-High. Subtyping interacts subtly with let-generalisation; Pottier's framework is sound but not as widely deployed as DK13. |
| Greenfield fold value | **MEDIUM.** The current finite-coercion list works; the architectural cost of a partial subtyping lattice is non-trivial; the value is mostly diagnostic clarity (one rule covers all lifetime escalations). |
| Recommendation | **DEFER-WITH-REASON.** Reconsider after V1 if user friction with lifetime coercions surfaces. |

### §2.10 Type telemetry / shape-mining-driven schema inference

| Field | Value |
|---|---|
| Source | `restart/README.md:181-183` ("auto-detected (no directive)... shape mining"); `restart/audit/pass-1-substrate/PASS-1.md:79` (cost model with extraction evidence); shape mining is currently scoped to recogniser eligibility (Pratt, SIMD, PHF, error-recovery boundaries, lookbehind window widths) but **not** type/schema inference |
| Current language | Shape miners observe Grammar IR and emit `Hint`s; the hints feed cost decisions, not type decisions. The user's explicit invocation of "telemetry-driven schema inference" has no V6 corpus surface. |
| V1 fold proposal | FOLD **schema-mining miner** as a peer of the existing recogniser miners. The miner observes `(rule_shape, layout_decision, value_shape)` triples and proposes named-record/named-enum/sum-type **identities** to the type checker — analogous to how Pratt miners propose operator tables. The proposal is a *hint*, not a constraint; the checker accepts when consistent with HM facts. |
| Implementation impact | New `passes/recognizers/schema_miner.rs`; new `Hint::Schema { rule, proposed_identity }`. The checker gains a "name suggestion" pass that turns inferred row-typed records into named records when the schema-miner identifies a stable shape across N call sites. Naming is the user-friction win: `JsonObject` rather than `{string: Json}`. |
| Risk | Low-Medium. Naming is cosmetic at the type-system level (does not affect soundness); it affects diagnostic and generated-API quality. The risk is mining-overfitting (proposing names that misclassify the user's domain), addressed by hint dampening identical to Pratt/SIMD. |
| Greenfield fold value | **HIGH.** This *is* the user's "telemetry-driven schema inference" mandate. Without it, schemas are either explicit annotations (Rust-equivalent) or anonymous structural rows (Haskell-equivalent). With it, BBNF emits **named, queryable schemas** without requiring annotations. This is the "stronger than Rust" claim made concrete. |
| Recommendation | **FOLD.** |

### §2.11 Let-generalisation / let-rank-N polymorphism

| Field | Value |
|---|---|
| Source | `restart/research/topic-1-hm-foundations.md:103-105` (Damas-Milner let-generalisation cited); `restart/audit/pass-1-substrate/PASS-1.md:75` (V1 generic rules are rank-1 schemes generalised at definition) |
| Current language | Generic rules generalise at *rule definition*, instantiate at *Ref*. There is no explicit **let-binding** within a rule body that would trigger let-generalisation. The grammar surface admits `RuleDecl` and `HostFn`, both top-level. |
| V1 fold proposal | DEFER. BBNF has no `let x = e in body` form; the let-generalisation question is moot. Rule-level generalisation already covers the user-facing case. |
| Implementation impact | None. |
| Risk | N/A. |
| Greenfield fold value | **LOW.** |
| Recommendation | **DEFER-WITH-REASON.** Conceptually irrelevant to BBNF's surface. |

### §2.12 Principal types tracking and type-class resolution

| Field | Value |
|---|---|
| Source | `restart/research/topic-2-bidirectional.md:118` (DK13 principality), `restart/research/topic-3-csp-gadts.md:158-163` (CHR-style improvement for host overloads) |
| Current language | "Host overloads with determining arguments emit explicit improvement constraints, CHR-shaped where applicable, before finite CSP selection" — `restart/audit/pass-1-substrate/PASS-1.md:73`. CHR-shaping is named but not specified. |
| V1 fold proposal | FOLD a **CHR-improvement layer** (`restart/research/topic-3-csp-gadts.md:158-163`) explicitly for host-overload resolution. Host primitives like `parse_int_radix(s, radix) -> u32 | u64 | i32 | ...` get an improvement rule that determines result type from `radix` argument when known. The CHR shape lives in `host/signature/`; the resolver is a peer of finite CSP selection. |
| Implementation impact | `host/signature/`: `ImprovementRule { trigger: ArgPattern, improves: Vec<TypeVarBinding> }`. Solver consults improvement rules before falling through to finite CSP. |
| Risk | Low. CHRs are a clean formal device; bbnf's host-overload surface is finite and well-bounded. |
| Greenfield fold value | **MEDIUM-HIGH.** The corpus already names "CHR-shaped where applicable" but does not specify the layer. Specifying it V1 prevents the layer from later becoming an ad-hoc match in `host/registry/`. |
| Recommendation | **FOLD.** This is the load-bearing part of the user's "stronger inference than Rust" — Rust has no analogue for type-class-style improvement at host-call sites. |

### §2.13 Annotation surface as currently required for `@layout`

| Field | Value |
|---|---|
| Source | `restart/README.md:178` ("Optional override when type inference is ambiguous. Inference is default"); `restart/audit/pass-1-substrate/PASS-1.md:36` (`LayoutDirective` carries directive value, owner rule, override reason) |
| Current language | `@layout(struct \| enum \| tuple \| slice)` is the explicit-control surface. |
| V1 fold proposal | The corpus already says inference is default and the directive is override-only. Audit verdict: well-scoped; no fold needed. The schema-mining miner (§2.10) reduces the residual annotation pressure further. |
| Recommendation | Already well-scoped; no fold. |

### §2.14 Annotation surface for generic rule type parameters

| Field | Value |
|---|---|
| Source | `restart/README.md:266` ("First-class explicit annotations are welcome where the author wants control... generic-rule type parameters") |
| Current language | Type parameters are explicit at rule declaration: `Object<V> = ...`. |
| V1 fold proposal | The grammar requires `<V>` at the *declaration*; instantiation `Object<Json>` is also explicit at the call site. **Could** the call-site `<Json>` be inferred? Yes — the call context determines `V`. Folding eliminates a Rust-equivalent annotation. |
| Implementation impact | The parser already parses `Ref ::= Ident TypeArgs?` (`restart/audit/pass-1-substrate/PASS-1.md:216`), and `TypeArgs` is optional. The checker fills omitted type arguments by unification against the call context. DK13 (§2.1) does this natively. |
| Risk | Low. Call-site type-argument inference is well-studied (Pierce-Turner local synthesis, `restart/research/topic-2-bidirectional.md:118-124`). |
| Greenfield fold value | **HIGH.** "Stronger than Rust" requires call-site type-arg inference for generic rules; Rust requires `::<Json>` turbofish. |
| Recommendation | **FOLD.** Composes with §2.1. |

### §2.15 `pub` / visibility on generic rule schemes

| Field | Value |
|---|---|
| Source | Implicit. The grammar surface has no visibility modifier. |
| Current language | Silent. |
| V1 fold proposal | DEFER. Visibility is a module-system concern, and BBNF's module system is the workspace metadata block — visibility is per-grammar (the grammar exports its top-level rules; everything else is private). The current `Item` set is sufficient. |
| Recommendation | **DEFER-WITH-REASON** (out of scope for type system). |

### §2.16 Effect types / linear types / capability tracking

| Field | Value |
|---|---|
| Source | Implicit; not discussed. |
| Current language | Silent. |
| V1 fold proposal | DEFER. The closure-semantics table at `restart/ARCHITECTURE.md:1193-1198` already encodes a coarse effect classification (predicate closures forbid tape side effects; recovery closures emit recovery code; map closures lower to projection). Promoting this to a full effect system is research-novelty without a current user. |
| Recommendation | **DEFER-WITH-REASON.** |

### §2.17 Bidirectional checker as full DK rather than implicit Pierce-Turner

| Field | Value |
|---|---|
| Source | `restart/README.md:262`; `restart/audit/pass-1-substrate/PASS-1.md:73`; `restart/research/topic-2-bidirectional.md:103-114` |
| Current language | "Pierce-Turner-style bidirectional checking owns local expected-type flow at annotations, host calls, chain edges, and subsumption sites." |
| V1 fold proposal | FOLD DK13's algorithmic-completeness frame at `passes/types`. Pierce-Turner is a *strict subset* of DK13's bidirectional discipline (`restart/research/topic-2-bidirectional.md:188-200`); DK13 adds the application judgment and ordered contexts that handle subsumption rigorously. Pierce-Turner's local synthesis remains the user-visible posture; DK13's algorithm is the implementation. |
| Implementation impact | Same as §2.1 (this is the same fold viewed from a different angle). |
| Risk | Same as §2.1. |
| Greenfield fold value | **HIGH** (composes with §2.1). |
| Recommendation | **FOLD** (via §2.1). |

## §3 — Annotation-elimination candidates

The user's directive: **inference stronger than Rust if possible**. Here are
the surfaces where the V6 corpus currently requires annotations and audacious
inference could eliminate them.

| Annotation surface | Source | Current requirement | Inference candidate | Verdict |
|---|---|---|---|---|
| Generic rule type-arg at call site | `restart/audit/pass-1-substrate/PASS-1.md:216` (`Ref ::= Ident TypeArgs?`); `restart/audit/pass-1-substrate/PASS-1.md:75` | Optional today; the corpus does not commit to inferring it. | DK13 application judgment infers from call context (`restart/research/topic-2-bidirectional.md:118-124`). | **ELIMINATE via §2.14.** |
| `@layout(struct\|enum\|tuple\|slice)` | `restart/README.md:178` | Override-only; default inference. | Already eliminated by Q21 hybrid posture; schema miner (§2.10) reduces residual cases. | **ALREADY ELIMINATED**; reinforced by §2.10. |
| Rule-level `-> Type` terminal | `restart/README.md:266` | Explicit-control surface. | HM principal scheme + structural row inference recovers `Type` from rule body. Annotation becomes documentation, not requirement. | **ELIMINATE for inferable cases**; retain as documentation surface. |
| `@host fn` return type | `restart/ARCHITECTURE.md:1081` (`HostFn ::= "@host" "fn" Ident GenericParams? "(" ParamList? ")" "->" Type ...`) | Required today (return type is part of grammar production). | DK13 inference from block body recovers return type for monomorphic host functions; rank-N still needs annotation. | **PARTIALLY ELIMINATE.** Return-type annotation becomes optional for monomorphic bodies; required only for rank-N or recursive cases. |
| `@host fn` parameter types | `restart/ARCHITECTURE.md:1081` (`Param = Ident ":" Type`) | Required today. | Bidirectional check from call sites can recover parameter types when host functions are monomorphic and called from typed grammar contexts. | **PARTIALLY ELIMINATE.** Same posture as return type; require annotation only when call sites underspecify. |
| Generic rule's `<V>` parameter list at definition | `restart/audit/pass-1-substrate/PASS-1.md:75` | Required: `Object<V> = ...`. | Could be elided for fully-monomorphic cases, but the parser needs *some* way to know `Object` is generic. | **RETAIN**: parser-level concern; not a type-system annotation. |
| Schema names | n/a | Today every typed record gets an inferred shape from grammar-derived hybrid (`restart/README.md:115`). | Schema miner (§2.10) proposes *names* for structurally-stable shapes; user accepts via metadata or override. | **NEW INFERENCE SURFACE via §2.10.** |
| Coercion rules | `restart/README.md:270` | Finite registered candidates. | Row-polymorphism (§2.3) collapses record-narrowing class into one rule. | **ELIMINATE for record class** via §2.3. |

The corpus's "annotation surface: hybrid" claim (`restart/README.md:266`) is
*defensible* for V1 but *understates* what audacious inference could buy. The
folds in §2 push the hybrid further toward the inference end of the spectrum
without sacrificing the explicit-control safety valve.

## §4 — Recommended V1 folds (sorted high to low greenfield value)

| Rank | Item | Source | Fold class | Cost | Architectural value |
|---:|---|---|---|---|---|
| 1 | §2.1 DK13 higher-rank bidirectional algorithm at `passes/types` (`restart/research/topic-2-bidirectional.md:103-114`) | `restart/ARCHITECTURE.md:1161-1166` | Algorithm replacement | ~600-1200 LOC | HIGH — substrate of the checker; future refactor avoidance is the entire point |
| 2 | §2.10 Schema-mining miner as peer of recogniser miners (`restart/research/INDEX.md`-via-`fold-pass-1.md` referenced shape-mining infrastructure) | `restart/README.md:181-183`; `restart/audit/pass-1-substrate/PASS-1.md:79` | New miner + new hint variant | ~300-500 LOC | HIGH — the user's "telemetry-driven schema inference" made concrete |
| 3 | §2.4 Function-as-type via `FnType` in `Type` production | `restart/audit/pass-1-substrate/PASS-1.md:222`; `restart/ARCHITECTURE.md:1106` | Grammar IR variant + lowerer rule | ~400 LOC | HIGH — without it, generic rules cannot be parametric over user-supplied transforms |
| 4 | §2.3 Internal row polymorphism (Leijen scoped labels for layout reasoning, surface stays closed) | `restart/research/topic-1-hm-foundations.md:234-243`; `restart/ARCHITECTURE.md:1168-1171` | Internal type-system refinement | ~500 LOC | HIGH — collapses combinatorial finite-coercion table to one rule |
| 5 | §2.14 Call-site type-arg inference for generic rules | `restart/audit/pass-1-substrate/PASS-1.md:216` | Composes with #1 | minimal beyond #1 | HIGH — eliminates Rust's turbofish equivalent |
| 6 | §2.12 Specified CHR-improvement layer at `host/signature/` | `restart/research/topic-3-csp-gadts.md:158-163`; `restart/audit/pass-1-substrate/PASS-1.md:73` | New host-resolver layer | ~250 LOC | MEDIUM-HIGH — already named, not specified |
| 7 | §2.2 Implication-constraint shape in solver, surface stays deferred | `restart/audit/pass-1-substrate/PASS-1.md:73` | Constraint-type variant + solver decomposition | minimal (folds with #1) | MEDIUM — composes with #1 |
| 8 | §2.6 (internal) Existentials via DK contexts | `restart/research/topic-2-bidirectional.md:103-114` | Composes with #1 | none beyond #1 | MEDIUM — already implicit in #1 |

The cumulative V1 cost of folds 1-8 is on the order of **2500-3500 LOC** in
`passes/types/`, `host/signature/`, and `passes/recognizers/`. The total is
within Lock 13's per-directory ceilings if the type checker is split into
the conventional sub-modules (`elaborate/`, `unify/`, `improve/`, `check/`,
`miner/`).

The cumulative V1 *architectural* value is the difference between a
greenfield BBNF that can absorb every classical extension to ML-family
inference without changing its public type contract, and one that must
re-architect at every research opening.

## §5 — Cross-cutting concerns

The folds are not independent. The dependency structure is:

```
                §2.1 DK13 algorithm (foundation)
                  ├──► §2.2 Implication constraints (extends solver)
                  ├──► §2.6 Internal existentials (already in DK contexts)
                  ├──► §2.14 Call-site type-arg inference (uses application judgment)
                  └──► §2.4 Function-as-type (uses higher-order in DK)

                §2.3 Internal row polymorphism (independent; touches unifier)
                  └──► reduces work for §2.10's schema naming

                §2.10 Schema-mining miner (independent of checker)
                  └──► consumes §2.3 row inference for cleaner names

                §2.12 CHR-improvement layer (independent; touches host resolver)
                  └──► precedes finite CSP selection
```

**Recommended fold order**:

1. §2.1 first (foundation; everything else extends it).
2. §2.4 + §2.14 immediately after (compose with §2.1; cheap given §2.1).
3. §2.3 in parallel (independent track; interacts with unifier).
4. §2.12 after (independent; small).
5. §2.10 last (depends on §2.3 for clean shape comparison).
6. §2.2 + §2.6 absorb into §2.1 by design.

The strong cross-cut: **§2.1 sets the ceiling for every other type-system
fold**. If §2.1 defers, §2.2 / §2.6 / §2.14 / §2.4 all become awkward
retrofits (each adds checker complexity that DK13 absorbs uniformly). If
§2.1 folds, the marginal cost of §2.2 / §2.6 / §2.14 / §2.4 is small.

The user's "nail it now or refactor everything later" posture maps exactly
onto §2.1: if the V1 checker is plain rank-1 HM, every later research
opening is a re-architect; if the V1 checker is DK13, the later openings
land as additive features. Lock 4 (per-domain orthogonal optimization)
**survives** under DK13 because DK13 stays inside `passes/types`/`passes/layout`;
the bridge to CSP/egraph is unchanged.

## §6 — Open questions for synthesis

Q1. **Does the user's "stronger inference than Rust" mandate require §2.1?**
The synthesis agent should ratify this. The corpus says "Pierce-Turner-style
bidirectional checking" (`restart/README.md:262`); upgrading to DK13 is
*algorithm-level* refinement that preserves user-visible posture. The user's
mandate suggests yes; the V6 corpus suggests no. The deferral-audit verdict:
fold.

Q2. **Should the schema-mining miner (§2.10) propose names automatically
or surface candidates for review?** Auto-proposal maximises the
"no-annotation" goal; review-gate maximises naming quality. Recommendation:
auto-propose with a metadata override; metadata fence is the user's
correction surface.

Q3. **Internal row polymorphism (§2.3) — does the surface ever benefit?**
Audit verdict: no V1 surface form. Reconsider after V1 only if the user-
facing record-narrowing diagnostic friction surfaces.

Q4. **Function-as-type (§2.4) — closure conversion at lower time or at
call site?** Backend IR concern; `runtime/grammars/<name>/` already emits
generic types where Rust handles natively (`restart/README.md:170`). Defer
the answer to the codegen orchestrator.

Q5. **Effect classification on closures (§2.16) — does §2.10's schema
miner observe effect-shape?** Audit verdict: not in V1; the four-form
closure table is sufficient.

Q6. **What is the BBNF surface for `forall` annotation under §2.1?** The
DK13 paper uses `forall a. T`. BBNF's `Type` production needs an explicit
form: `ForallType ::= "forall" Ident+ "." Type`. Synthesis decision.

Q7. **How does §2.1's DK13 algorithm interact with the existing
`LayoutFacts` public side-table?** DK13 is a `passes/types` internal; the
public artefact remains `LayoutFacts`. Lock 2 holds. The internal
`TypeObligationLog` (`restart/research/topic-1-hm-foundations.md:485-491`)
gains DK ordered-context entries.

Q8. **CSP boundary under §2.1?** The corpus's seven finite-choice classes
(host overload, layout, recognizer, materialisation, recovery, backend,
extraction) are DK-orthogonal. CSP runs after DK13 settles on principal
schemes; the bridge to egraph is unchanged. Lock 4 holds.

Q9. **How does the V1 deferral of GADT *surface* interact with the V1 fold
of GADT *substrate* (§2.2)?** The substrate carries `Implication`
constraints; no grammar production introduces them; diagnostic
`BBNF-LOCAL-EQUALITY-ANNOTATION` stays reserved. When the surface lands
post-V1, the algorithm absorbs it without re-architect.

Q10. **Where does the audit's verdict disagree with V6's?** §2.1, §2.10,
§2.4, §2.3 are the four explicit disagreements. Each cites its rationale
above. The synthesis agent decides whether the V6 boundary or the audit's
fold-recommended boundary becomes the V1 commitment.

---

Closing posture: the V6 corpus draws a defensible V1 type-system boundary
that is **conservative under a greenfield mandate**. The four high-value
folds (§2.1, §2.10, §2.4, §2.3) cost on the order of 2500 LOC in `passes/types`
and `passes/recognizers`, preserve every settled lock, and convert the type
system from "rank-1 HM with annotation escape valves" to "audacious DK13
inference with grammar-derived schema mining" — the system the user's
mandate names. The remaining items are correctly deferred. The synthesis
agent's call.
