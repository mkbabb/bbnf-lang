# LOCK-3 · Family 3C — GRAMMAR-HOMOIOUSIA (pass 1)

*The IR is itself grammar-specified: the IR node languages are bbnf grammars, the
transpositions between them are grammar-to-grammar rewrite maps, and emission is
grammar-derived unparse. The generator eats itself — bbnf-in-bbnf, IR-in-bbnf — with a
committed stage-0 and a regen fixpoint, aligned with the LOCK-7 bootstrap. Homoiousia:
the IR is *of like substance* with the grammars it processes; the compiler's own middle is
not privileged hand-written Rust but another document the grammar plane derives.*

---

## 0. The thesis, and what makes this family orthogonal

Today the IR tower is a sequence of **hand-written Rust enum transformations**. The node
languages are Rust enums: `ExprKind` (10 variants, `ir/src/lib.rs:233-271`), `BackendExpr`
(11 variants, `:375-413`), `Recognizer`/`SimdMode`/`SpanKind`/`TapeKind` (`:415-463`), the
`DirectBuild*` schema lattice (`:465-535`). The transpositions are hand-written matches:
`Infer::infer_expr` (`passes/src/lib.rs:203`, which *discards* `Project.ty` — R6 §5a),
`lower_expr`'s `Project→ValueProject` arm (`passes/src/lib.rs:1349`), `materialize_rule`'s
IR-node-from-IR-node splice (`:1357-1391`), `derive_materialization_plan` (`:1136-1287`), and
`lower::lower_to_rust` (`codegen/src/lower/mod.rs`, `select_lowering`→per-shape modules
`eager_tape`/`offset_tape`/`sink_only`/…). The final transposition — backend IR → Rust text —
is **raw-string template splicing**: eight `include_str!("json_templates/*.rs")` files
(`runtime_generator.rs:104-133`, R6 §3-4) plus the `CSS_GENERATED_RS`/`CSS_MOD_RS`/`JSON_*`
raw strings (`runtime_generator.rs:339-505`). PROVE-12 residualization was applied as a
**hand-edit of one raw-string template** (`runtime_generator.rs`: the `delim_mask` const fn +
const-generic call sites — PROVE-12 REPORT §2), and its §7 explicitly promised the pattern
"generalizes to any grammar-derived delimiter/STOP-set scan" but demonstrated it on CSS alone.

Family 3C's genuinely distinct architectural center: **make the IR levels and their
transpositions grammar-specified data**, so the generator becomes mostly *derived*, not
*written*. This is not the contract-tower question (how many levels, what invariants), nor
the single-vs-multi-IR question, nor per-site staging as such — it is the *substance* of the
IR: whether the middle is hand-Rust or bbnf-specified-and-generated. A homoiousious IR is
orthogonal to level-count — it may be a 3-level tower or a single annotated graph; what makes
it 3C is that each level is a `.bbnf` document and each transposition a declarative rewrite.

### 0.1 Three facts in this tree that make the family concrete, not aspirational

1. **bbnf is already self-describing, and its type vocabulary was already reserved for IR
   nodes.** `grammar/bbnf/bbnf.bbnf` (89 LOC) expresses the bbnf grammar *in bbnf*, with:
   typed projections (`identifier = /…/ -> Span`); `@import { … } from "…"` module composition
   (line 4-5); a typed host-function directive (`host_directive = "@host" ?w , identifier ?w ,
   ( ":" ?w , type_name ?w ) ?`, line 73 — *host fns already carry a declared return type*);
   an inline host/interpolation block (`"@{" , rhs , "}"`, line 33); grammar-level closures and
   parameterized calls (`closure = "|" identifier … "|" rhs`, line 51; `call_arg`/grammar-fn
   calls, line 27-30); and `@pretty` unparse directives (line 68, 87-89). `expressions.bbnf`
   (37 LOC) is the *value-construction DSL* — `value_fn_call = value_path "(" (value_expr …)? ")"`,
   `value_path`, `value_input`, precedence-climbed operators, `value_closure`. **The decisive
   evidence:** `types.bbnf`'s own comment reads *"arbitrary identifiers (for IrNode, etc.)"* —
   the self-hosting grammar's type name production (`type_name = "u8" | … | /[_a-zA-Z]…/`) was
   *authored to name IR node types*. The homoiousia direction is not a novel imposition; the
   grammar plane's designers reserved a slot for it. Siblings `grammar/{ebnf,bnf}/…bbnf`,
   `grammar/misc/{regex,g4,csv}.bbnf` prove the meta-grammar pattern is load-bearing. It is
   presently **unwired**: grammar source is parsed by a 2142-LOC hand parser
   (`grammar/src/lib.rs`), and `bbnf.bbnf` is a dormant self-host (LOCK-7's bootstrap target).

2. **The IR-enum *emitter* already exists and works — only its *input* is hand-authored.**
   This is the precise, corrected statement (the tempting over-claim is that derivation
   exists — it does not). The working machine is: `direct_schema.rs` (the schema IR:
   Struct/Vec/MapString/StringEnum/Option/Scalar, presence/capacity hints, `:4-120`) →
   `lower/schema_direct.rs:11` (the bridge: `schema.validate()?` + copy roster) →
   `json_typed_direct::render` (the **1245-LOC direct-to-struct emitter**, `json_typed_direct.rs:10`),
   invoked by `emit_typed_from_source(name, src, &DirectSchemaSet)` (`codegen/src/lib.rs:130`).
   The `DirectSchemaSet` is an **input parameter**; the *only* producer is hand-written —
   `xtask/src/real_typed_schema.rs:7` literal rows (R6 §5d). **There is no `type_plane` module
   and no `derive_direct_schema` pass** (verified: `rg type_plane` → 0 hits;
   `find … type_plane*` → none). Schema *derivation* from `->` projections is GN-3.W2's
   deliverable (R6 §8b: "add the `derive_direct_schema` producer"; two edits — stop discarding
   `Project.ty` at `passes/src/lib.rs:203`, add the producer).

The corrected homoiousia proposition, precisely: *the compiler's front (grammar→AST) and its
back-half emitter (schema→Rust struct) already exist; the middle organs — `ir/src/lib.rs`'s
enums, `passes`'s hand-match transpositions, and `runtime_generator`'s raw templates — are the
last hand-written surface. 3C specifies those three as grammars + rewrite rules + unparse, and
it **shares GN-3's not-yet-built derivation pass**: one `bbnf-`→`-schema` derivation, two
consumers (user document shapes AND the compiler's own IR shapes).* This is the generator
eating itself — with no new emitter, a shared derivation, and a bootstrap fixpoint as the
proof obligation. Payoffs beyond parsimony: bootstrap alignment (LOCK-7), rust/ts skeleton
isomorphism by construction (LOCK-6 6B — one spec, two skeletons), and dogfooding the type
plane (LOCK-1/GN-3) on the compiler itself.

---

## 1. The mechanism, made concrete

Three substrates, each with an existing consumer (Y.13-honest by construction — every piece
below is read by a mechanism already in the tree or by GN-3's declared pass).

### 1.1 Substrate A — the IR metagrammars (the node languages as bbnf)

Author `crates/ir/grammars/{hir,mid,backend}.bbnf`. Each IR node kind is a bbnf rule; each
Rust variant's payload is a `->` projection over child-refs and scalar leaves. `ExprKind`'s
ten arms become ten alternation branches:
`expr_kind = seq | alt | repeat | optional | literal | regex | ref | annotation | project |
discard ;`, each branch a `->` to a named constructor. The arena indices `ExprId(usize)`
(`:19`) are the graph edges — a `Named { type_id: "expr" }` child-ref in `DirectBuildTypeRef`
terms (`:493`); `Seq(Vec<ExprId>)` is a constructor with `DirectBuildCardinality::Vec`
(`:517`); `Project { body, target, ty }` a three-field product; `Optional(ExprId)` a
`DirectBuildPresence::Optional` (`:511`). **The schema lattice already carries every shape the
IR grammar needs *except one*: sum-of-variants (an enum).** `DirectBuildTypeRef` today is
`Named | Scalar` (`:492-496`) — no variant/sum node. So IR-enum-from-grammar has a **named,
concrete dependency on GN-3.W1** (add `Sum`/variant to `DirectTypeKind`; grow HM `unify` over
sums — SPEC §2 GN-3.W1). This is not a blocker but a *sequencing fact*: 3C's Substrate A rides
GN-3.W1's sum-type work, then the same emitter turns `hir.bbnf` into `enum ExprKind`.

An **attribute vocabulary** carries what a bare grammar cannot — the same
"information-moves-into-the-grammar" cure GN-3 applies to capacity hints:
`@derive(Serialize,Deserialize)` (the ubiquitous derive on every IR type), `@box body` (heap
indirection — `Box<BackendExpr>` at `:377,387,410`), `@newtype RuleId = usize` (`:16-19`),
`@serde(skip_if_none) target` (`DirectBuildField.target`'s `skip_serializing_if`, `:469`).
These are not novel syntax — they mirror Rust's own `derive`/attribute system, and bbnf
*already parses* an open-ended `@directive` surface (`bbnf.bbnf:60-81`) plus per-factor
annotations. The metagrammar attribute set is the exact, finite delta the fixpoint (§2) forces
you to enumerate.

The consumer is **GN-3's derivation pass + the existing renderer**: `derive_direct_schema(hir.bbnf)`
(GN-3.W2, to be built) produces a `DirectSchemaSet`; `json_typed_direct::render` emits the
Rust; committed `ir/src/lib.rs` becomes `@generated`, its stage-0 form frozen. This is the
generator eating itself: the pass that turns `twitter_search.bbnf`'s `->` chains into user
structs turns `hir.bbnf` into the compiler's own `ExprKind` — one derivation, two consumers.
Homoiousia's Substrate A is therefore **a new input (`hir.bbnf`) + an attribute extension to
`DirectSchemaSet` + a shared dependency on GN-3.W1/W2** — *not* a new emitter.

### 1.2 Substrate B — the transpositions as grammar-to-grammar rewrite maps

A bbnf-native rewrite-rule form. The LHS is a pattern over a source IR grammar's node rules;
the RHS is a `value_expr` — *the construction DSL bbnf already has* (`expressions.bbnf`:
`value_fn_call`, `value_path`, `value_input`) — over the target grammar's node rules:

```
# passes/lower.rewrite   (interpreted at generation time; zero runtime cost — constitution v)
lower : Project{ body, target, ty }  ->  ValueProject{ body: lower(body), target, ty } ;
lower : Repeat{ body, min, max }     ->  RepeatLoop{ body: lower(body), min } ;
lower : Discard{ left, right, keep }  ->  discard_lower(lower(left), lower(right), keep) ;  # @host
# every unmatched node is the IDENTITY map — auto-derived from the node grammar
```

The default per-node arm is the **identity catamorphism, auto-derived from the node grammar**,
so a pass writes only the productions it *changes*. This is the single defining trick of the
Nanopass framework (§3) and of Haskell's `recursion-schemes`: from a functor/grammar with `n`
constructors, the identity `fold` over child-refs is mechanically generated; a pass overrides
the handful of arms it touches, and the framework fills the rest. In bbnf terms the child-ref
projections in the node grammar *are* the functor's recursive positions, so "recurse into every
child, rebuild the node" is derivable with zero hand code — exactly what makes a 300-line
hand-match collapse to a 3-line rewrite file.

Structural rewrites are grammar; non-structural kernels are **typed `@host` functions** (the
`hybrid-grammar-host` law; the syntax exists — `host_directive`, `bbnf.bbnf:73`, with a
*declared return type*). The rewrite file names the *schedule and the structure*; the host
lattice supplies *analysis and semantics*. This is LOCK-7-C's host-function-lattice scoped to
the IR spine, with a measured floor: the family lives or dies on the **structural-fraction**
— the proportion of each pass expressible as identity+rewrite vs escaped to host (§4 risk 1;
§5 PROVE-HOMO-2 measures it).

**Crucially, not every "analysis" is a host escape.** Some are *attribute-grammar* computations
— declarative fixpoints over the tree — and the tree *already contains one*: `nullability`
(`ir/src/lib.rs:320-349`) is a monotone `Changed`-flag fixpoint over `ExprKind` (Seq=∀children,
Alt=∃branch, Optional=true, …) — a textbook synthesized attribute. First-set/FOLLOW-set
derivation (`passes::derive_recognizers`) is likewise a lattice fixpoint. These are expressible
as **reference attribute grammars** (JastAdd/Silver lineage, §3), *not* opaque host calls,
which materially raises the achievable structural-fraction above a naive "rewrites only" count.
The honest taxonomy has *three* tiers, not two: (i) structural rewrites (grammar), (ii)
attribute fixpoints (declarative, grammar-adjacent), (iii) genuine host kernels (cost-model
search, HM union-find, NEON leaves). PROVE-HOMO-2 must classify each pass arm into these three.

The seed already exists: `materialize_rule` (`passes/src/lib.rs:1357-1391`) *already* builds IR
nodes from IR nodes by splicing `SpanMark(Start) + body + SpanMark(End) + TapeEmit + DirectBuild
+ Return` — a hand-written rewrite. Homoiousia lifts that splice into a rewrite rule over
`hir.bbnf`→`backend.bbnf`.

### 1.3 Substrate C — emission as grammar-derived unparse

The raw-string templates (`runtime_generator.rs:104-133,339-505`) are the antithesis of
grammar-to-grammar: the backend-IR→Rust map is opaque string splicing. Homoiousia replaces the
*structural glue* with unparse: a Rust-surface grammar run in reverse over the backend IR. The
seed is `GrammarIr::pretty`/`pretty_expr` (`ir/src/lib.rs:124-215`) — an *already-grammar-driven
unparser* for bbnf — plus the `@pretty` directives in `bbnf.bbnf` (`@pretty grammar block ;`,
line 87-89) and the `gorgeous`/`pprint` line (MEMORY: aesthetics-critical). BNFC's
grammar→AST+pretty-printer is the direct model (§3).

**The bounded claim (constitutional).** Hand-written kernels stay hand-written — the JSON
string/number/NEON scanners, the `parse_that_regex` leaves (constitution: "everything is
grammar-driven … *save primitives and combinators*"; MEMORY: no-combinators-monolithic,
one-codegen-path, preserve-rich-ast). Unparse emits **structural descent glue and kernel
call-sites**, not kernels. The raw templates do not fully vanish; they collapse to a *leaf
vocabulary of `@host`-referenced kernels* that the unparser emits *calls* to. R6 §3-4 shows the
shape: `json_templates/parser.rs` interleaves structural descent (unparse-emittable) with calls
to `parse_that_regex::number::materialize_f64` (kernel leaf, stays `@host`). Naming this
honestly — Substrate C is a *partial* win bounded to structural glue — is the difference between
a real family and an over-claim (§4 risk 3).

### 1.4 Residualization (PROVE-12) as a grammar-to-grammar corollary — the differentiator

PROVE-12's win — const-fold the CSS delimiter membership to a `u128` immediate, monomorphize
per call site, +18 generated LOC, 0.81-0.98× cyc/B, byte-identical output (REPORT §3-6) — is,
in homoiousia terms, **one rewrite rule** over `backend.bbnf` with a stage-0 annotation:

```
residualize : find_delim{ set: @const s }  ->  find_delim_masked{ mask: delim_mask(s) } ;  # delim_mask @host, comptime
```

The `@const`/stage marker rides the existing `ExprKind::Annotation { name, value }`
(`ir/src/lib.rs:257-260`); the mask fold is a comptime `@host` fn; the emitted immediate is the
residual (PROVE-12's `mov x27, #0x2800…` immediate — REPORT §3). PROVE-12 §7 asserted this
generalizes "to any grammar-derived delimiter/STOP-set scan (JSON's structural scan,
general-descent)" but demonstrated it on CSS alone by hand-editing one template. Homoiousia *is*
that generalization: **one rewrite rule, N grammars**, vs PROVE-12's one-template-per-grammar.
This is the family's cleanest falsifiable, over-the-baseline win (§5 PROVE-HOMO-3). It also
names the LOCK-2 seam cleanly: the rewrite *schedule* (when residualization fires, at what
depth) is where an e-graph/CSP driver plugs — but 3C's center is that residualization is a
*directed rewrite over the IR grammar*, not a bespoke template patch; the scheduling engine is
LOCK-2's to supply (§3, egg/egglog note).

---

## 2. The substrate & integration seams (path:line)

| Homoiousia piece | Extant code it re-homes / re-uses | Seam / status |
|---|---|---|
| IR node grammars (Substrate A) | `ExprKind` `ir/src/lib.rs:233-271`; `BackendExpr` `:375-413`; `Recognizer`/`SimdMode`/`TapeKind` `:415-463`; `DirectBuild*` `:465-535` | these enums become `@generated` from `{hir,mid,backend}.bbnf` |
| IR-enum emitter (**reuse, exists**) | `direct_schema.rs:4-120`; `json_typed_direct.rs:10` (render, 1245 LOC); `lower/schema_direct.rs:11` (bridge); `emit_typed_from_source` `codegen/src/lib.rs:130` | feed IR grammars to the *same* schema→struct engine |
| schema derivation (**shared dep, GN-3**) | R6 §8b sketch; `passes/src/lib.rs:203` (stop discarding `Project.ty`) | GN-3.W2 `derive_direct_schema`; homoiousia is its second consumer |
| sum/variant type (**named dep, GN-3.W1**) | `DirectBuildTypeRef` `:492-496` (Named\|Scalar — **no Sum**) | GN-3.W1 adds Sum to `DirectTypeKind`; required for `enum ExprKind` |
| self-host bootstrap (LOCK-7) | `grammar/bbnf/{bbnf,expressions,types}.bbnf` (dormant; `@host`/`@pretty`/closures live); hand parser `grammar/src/lib.rs` (2142 LOC) | regen the parser/IR from grammar; committed stage-0 + fixpoint |
| transposition rewrites (Substrate B) | `infer_expr` `:203`; `lower_expr` Project arm `:1349`; `materialize_rule` `:1357-1391`; `derive_materialization_plan` `:1136-1287`; `lower_to_rust` `codegen/src/lower/mod.rs` | lift each hand match into a `.rewrite` rule set over IR grammars + `@host` |
| construction DSL (**reuse, exists**) | `expressions.bbnf` (`value_expr`, `value_fn_call`, `value_path`, `value_input`, `value_closure`) | the rewrite RHS *is* this existing value grammar |
| attribute fixpoints (declarative) | `nullability` `ir/src/lib.rs:320-349`; `derive_recognizers` first-set `passes/src/lib.rs:338` | reframe as reference-attribute-grammar, not host escape (raises structural-fraction) |
| host-fn lattice (LOCK-7-C) | `host_directive` `bbnf.bbnf:73` (typed); `@{…}` block `bbnf.bbnf:33`; `RuntimeFrontendRequirements` `grammar_provider.rs:38-81` | escape hatch for cost search / HM `unify` / kernels |
| emission-as-unparse (Substrate C) | `runtime_generator.rs:104-133,339-505` (raw templates); `GrammarIr::pretty` `ir/src/lib.rs:124-215`; `@pretty` `bbnf.bbnf:68,87-89` | structural glue unparse-emitted; kernels stay `@host` leaves |
| residualization rewrite (§1.4) | PROVE-12 patch (`runtime_generator.rs` `delim_mask`); `ExprKind::Annotation` `:257-260`; `lower/css_scan.rs` STOP-set (712 LOC) | `@const` stage marker + comptime `@host` fold, one rule, N grammars |
| regen/bootstrap host (LOCK-7) | `xtask/src/regen.rs:19,36`; `codegen/src/lib.rs:101,130` | add IR-grammar regen targets; fixpoint gate `regen∘regen = regen` |

**Codex consciousness (constitution ix)** — 3C **re-cuts** two GN boundaries as *named
supersessions*, never silent contradictions, both subtractive (fewer hand organs, parsimony vi):
(a) GN-1's `codegen/templates/{json,css,csv}/` raw-string home is superseded — Substrate C
dissolves *structural* templates into a `backend.bbnf` unparse grammar; kernel leaves remain,
named explicitly. (b) GN-3's `derive_direct_schema` is **generalized** from *user-document*
producer to *universal* producer (user shapes AND IR shapes), and GN-3.W3's derived≡hand gate
becomes the template for the IR-enum bootstrap gate (§5 PROVE-HOMO-1). Neither contradicts the
GN codex; both extend a declared GN deliverable to a second consumer.

---

## 3. Prior art (SOTA, with citations) — proven at production scale

- **Nanopass framework (Chez Scheme).** Sarkar, Waddell, Dybvig, *"A Nanopass Infrastructure
  for Compiler Education"* (ICFP 2004); Keep & Dybvig, *"A Nanopass Framework for Commercial
  Compiler Development"* (ICFP 2013); Keep's dissertation. Each IR level is a `define-language`
  form — *a grammar*, and levels are defined incrementally (`define-language L1 (extends L0) …`)
  so the metagrammar surface stays small. Each pass is a `define-pass` writing **only the changed
  productions**, the identity catamorphism auto-derived. Chez's *production* compiler is built
  this way. **This is homoiousia's flagship proof**: IR-as-grammar + transposition-as-declarative
  -map is a *shipping commercial compiler architecture*. Substrate B is nanopass in bbnf; the
  `extends` idea directly answers the "three metagrammars is a lot of surface" worry (mid extends
  hir, backend extends mid).
- **MLIR — ODS + DRR + PDL.** Operation Definition Specification (TableGen `.td`) declares
  ops/dialects *as data*; Declarative Rewrite Rules (`Pat<(SrcOp …),(ResOp …)>`) specify
  lowerings; PDL (Pattern Descriptor Language) makes rewrite patterns first-class IR. Lattner et
  al., *"MLIR: Scaling Compiler Infrastructure for the Domain"* (CGO 2021). **The second
  flagship**: IR grammar-specified + transpositions as declarative rewrite maps, at LLVM scale.
  Names the build-time-tax risk (TableGen codegen) homoiousia must measure (§4 risk 4).
- **BNFC (BNF Converter).** Forsberg & Ranta, *"Labelled BNF"*. A labelled BNF grammar generates
  AST types + parser + **pretty-printer/unparser** across multiple backends. Direct model for
  Substrate C (emission-as-unparse) *and* Substrate A (grammar→AST types) — the two halves 3C
  needs, in one production tool.
- **Reference attribute grammars — JastAdd / Silver / Kiama.** Hedin, *"Reference Attributed
  Grammars"*; Van Wyk et al. (Silver); Sloane (Kiama). Analyses (name resolution, type checking,
  dataflow) expressed as *declarative attribute equations over the grammar tree*, with automatic
  demand-driven fixpoint evaluation. **This is the answer to risk 1's sharpest edge**: a large
  class of "analyses" (nullability `ir/src/lib.rs:320-349`, first/follow sets, cardinality
  inference) are attribute computations, not host escapes — they belong to tier (ii), raising the
  measurable structural-fraction. JastAdd is used in the production ExtendJ Java compiler.
- **Stratego/XT + Spoofax + Rascal.** SDF (Syntax Definition Formalism) + Stratego strategic
  term rewriting = the academic lineage of "the transposition is a rewrite map *with a
  schedule*." Confirms the strategy/schedule separation (structure vs traversal control) that
  bounds host-escape and cleanly cedes scheduling to LOCK-2.
- **egg / egglog.** Willsey et al., *"egg: Fast and Extensible Equality Saturation"* (POPL
  2021). Rewrite rules as data + equality saturation — the *optimization* cousin of Substrate B.
  LOCK-2 owns the e-graph, so 3C uses **directed** rewrite-as-lowering (nanopass/DRR), and cedes
  saturation-scheduling of residualization (§1.4) to LOCK-2.
- **Negative precedent (why this is non-trivial).** rustc (HIR/MIR are hand-written Rust; the
  query system is not grammar-specified), GHC Core (hand-written; only the RULES pragma is
  declarative), and `regex-syntax` HIR (hand-written) *all chose* hand-written IR. The reason is
  uniformly **rewrite/analysis expressiveness** — borrow-check, type inference, strictness
  analysis are not structural rewrites. This is precisely why homoiousia's honest claim is
  *hybrid* (grammar-structure + attribute-fixpoint + host-analysis), and why the
  structural-fraction measurement (§5) is the family's make-or-break, not a formality.

---

## 4. Named risks, counterexamples, resolutions

**Risk 1 — the rewrite-expressiveness wall (the sharpest).** Not every transposition is a
structural rewrite. `derive_backend_shape` (`passes/src/lib.rs:398`) is a cost-model search; HM
`unify` is union-find; `derive_recognizers` (`:338`) is a first-set fixpoint. *Counterexample:*
if a pass is 80% `@host`, the "grammar-specified" label is hollow — consumer-less-substrate
-adjacent (Y.13). *Resolution:* the **three-tier taxonomy** (§1.2) — structural rewrite /
attribute fixpoint / host kernel — and the hybrid-grammar-host boundary; the honest claim is
that *structure and monotone attributes* are grammar and only *search/union-find/kernels* are
host. **The prototype must measure the split across all three tiers**, not two.

**Risk 2 — bootstrap byte-identity reachability.** The hand `ExprKind` carries `#[derive(Clone,
Debug, PartialEq, Eq, Serialize, Deserialize)]`, `#[serde(default, skip_serializing_if=…)]` on
`DirectBuildField.target` (`:469`), `Box<BackendExpr>` indirections (`:377,387,410`), newtype
`RuleId(usize)`/`ExprId(usize)` (`:16-19`), and — the hardest — **enum-of-variants**, which the
schema lattice cannot yet express (`DirectBuildTypeRef` = Named\|Scalar, `:492`). *Counterexample:*
derived enum ≠ hand enum → fixpoint fails. *Resolution:* (a) the attribute vocabulary (`@derive`,
`@box`, `@serde`, `@newtype`) — the same information-into-grammar cure as GN-3 Wall-1 capacity
hints; (b) the **named GN-3.W1 sum-type dependency** — 3C cannot bootstrap `enum ExprKind` until
`DirectTypeKind` grows `Sum`. This is a reachability risk of *like class* to GN-3's own walls and
must be proven by fixpoint, not asserted.

**Risk 3 — emission-as-unparse cannot fully retire templates.** `json_templates/parser.rs`
interleaves structural descent with kernel calls (R6 §3). *Counterexample:* if unparse emits only
glue, raw strings survive wherever glue and kernel interleave → a *partial* win. *Resolution:*
bound Substrate C to structural emission; kernels become `@host` leaves the unparser emits calls
to; name the residual template surface explicitly (constitution vii). The claim is "structural
templates dissolve," not "all templates vanish."

**Risk 4 — build-time tax + debuggability (parsimony vi).** A comptime rewrite interpreter is
zero *runtime* cost (constitution v — satisfied by construction, the rewrites run at generation
time) but may tax regen and obscure debugging (the MLIR-TableGen lesson). *Counterexample:* if
grammar-specified regen is 3× slower or the generated Rust is unreadable, KISS/parsimony is
violated. *Resolution:* measure regen wall time as a gate; require the generated Rust to be
**byte-identical** to the hand form (so debuggability is *unchanged* — you read the same output).
Byte-identity is both the correctness proof and the debuggability guarantee.

**Risk 5 — MEMORY invariants.** `preserve-rich-ast`, `typed-materialization-invariant` ("every
`->` reaches the tape emitter"), `no-ts-ir` ("AST IS the combinator"), `clean-regen-discipline`
(generated files are regen-only). Homoiousia must *strengthen*, not flatten: the IR grammar's
`->` projections *are* the materialization contract, made checkable by the regen fixpoint. No IR
layer is *added* — the existing tower is *re-substanced*; and the fixpoint enforces
clean-regen-discipline structurally (the IR enums join the `@generated` set).

**Risk 6 — the metagrammar-surface objection.** Three `.bbnf` node grammars plus a rewrite DSL
plus an attribute layer could be *more* total surface than the hand Rust, inverting parsimony.
*Resolution:* Nanopass `extends` (mid extends hir extends backend deltas) + identity-default
means each grammar and each pass states only its *delta*; the win is measured as *total
hand-authored LOC* (grammars + rewrites + `@host` bodies) vs the current 1020 (ir) + the
transposition slice of passes' 2750 + the template slice of codegen's 10201. If total surface
does not shrink, the family banks.

---

## 5. Exactly what a prototype must test (falsifiable, cheapest-first)

- **PROVE-HOMO-1 — IR-enum-from-grammar bootstrap (decisive, cheap; gated on GN-3.W1).** Author
  `crates/ir/grammars/hir.bbnf` for `ExprKind`'s 10 variants + the attribute vocabulary. With
  GN-3.W1's `Sum` type in place, run GN-3.W2's `derive_direct_schema` + the *existing*
  `json_typed_direct::render` to emit a Rust enum. Gate: emitted enum ≡ hand `ExprKind`
  (`ir/src/lib.rs:233-271`) byte-identical (modulo the enumerated attribute vocabulary), and a
  real grammar (json.bbnf) parses identically through generated vs hand enum. *Measures:* how much
  attribute machinery the metagrammar needs (risk 2) and whether `Sum` suffices (the GN-3.W1
  coupling). Mirrors GN-3.W3's derived≡hand gate, reusing its engine — near-zero new emitter code.
- **PROVE-HOMO-2 — one transposition as a rewrite set (the make-or-break).** Express
  `lower_expr`'s Project/Discard/Repeat/Seq/Alt/Optional arms (`passes/src/lib.rs:1349,…`) as a
  `.rewrite` file + `@host` calls, interpreted at comptime; gate byte-identical `BackendIr` vs
  hand `lower_expr` on json/css/sheets. *Measures:* the **three-tier structural-fraction** (risk
  1) — classify every arm as structural-rewrite / attribute-fixpoint / host-kernel. This is the
  number that decides the family. Target: ≥70% of arms in tiers (i)+(ii); the rest named `@host`
  with a one-line reason each.
- **PROVE-HOMO-3 — PROVE-12 as generalized residualization rewrite (the differentiator).**
  Re-derive PROVE-12's const-fold as ONE rewrite rule over `backend.bbnf` (`@const` annotation +
  `delim_mask` comptime `@host`), applied to BOTH the CSS delimiter scan AND a second grammar's
  STOP-set (JSON structural scan or general-descent `lower/css_scan.rs` STOP-set toolkit). Gate:
  reproduce PROVE-12's byte-identical CSS `generated.rs` (+18 LOC, 0.81-0.98× cyc/B — REPORT
  §4-6) AND fire the *same rule* on the second grammar with a measured win. Proves the "one
  rewrite, N grammars" claim PROVE-12 §7 asserted but never demonstrated — 3C's concrete
  over-the-baseline win, and the cheapest to run against the real bench.

**Convergence pivot for pass 2:** PROVE-HOMO-2's three-tier structural-fraction number. If tiers
(i)+(ii) ≥70%, the family is a nanopass-style hybrid worth carrying; if search/union-find/kernels
dominate and structure is a thin veneer, homoiousia collapses to "hand-written passes with a
cosmetic grammar wrapper" and should bank in favor of a sibling. A secondary pivot: PROVE-HOMO-1's
GN-3.W1 coupling — if `Sum` is not landed, Substrate A cannot bootstrap and 3C sequences strictly
after GN-3.W1. Those two facts are what pass 1 hands pass 2.
