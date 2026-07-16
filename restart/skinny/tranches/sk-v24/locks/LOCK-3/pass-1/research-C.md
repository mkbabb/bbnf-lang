# LOCK-3 · Family 3C — GRAMMAR-HOMOIOUSIA (pass 1)

*The IR is itself grammar-specified: the IR node languages are bbnf grammars, the
transpositions between them are grammar-to-grammar rewrite maps, and emission is
grammar-derived unparse. The generator eats itself — bbnf-in-bbnf, IR-in-bbnf — with a
committed stage-0 and a regen fixpoint, aligned with the LOCK-7 bootstrap. Homoiousia:
the IR is *of like substance* with the grammars it processes; the compiler's own middle
is not privileged hand-written Rust but another document the grammar plane derives.*

---

## 0. The thesis, and what makes this family orthogonal

Today the IR tower is a sequence of **hand-written Rust enum transformations**. The node
languages are Rust enums: `ExprKind` (10 variants, `ir/src/lib.rs:233-271`), `BackendExpr`
(`:376-413`), `Recognizer`/`SimdMode`/`TapeKind` (`:415-463`), the `DirectBuild*` schema
lattice (`:465-535`). The transpositions are hand-written matches: `Infer::infer_expr`
(`passes/src/lib.rs:203`), `lower_expr`'s `Project→ValueProject` arm
(`passes/src/lib.rs:1349`), `materialize_rule`'s IR-node-from-IR-node splice
(`passes/src/lib.rs:1357-1391`), and `lower::lower_to_rust` (`codegen/src/lower/mod.rs`).
The final transposition — backend IR → Rust text — is **raw-string template splicing**:
eight `include_str!("json_templates/*.rs")` files (`runtime_generator.rs:101-133`) and the
`CSS_GENERATED_RS`/`CSS_MOD_RS`/`CSS_PARSER_RS`/`CSS_SINK_RS`/`JSON_*` raw strings
(`runtime_generator.rs:339-505`). PROVE-12 residualization was applied as a **hand-edit of
one raw-string template** (`runtime_generator.rs`, the `delim_mask` const fn + const-generic
call sites — PROVE-12 REPORT §2).

Family 3C's genuinely distinct architectural center: **make the IR levels and their
transpositions grammar-specified data**, so the generator becomes mostly *derived*, not
*written*. This is not the contract-tower question (how many levels, what invariants), nor
the single-vs-multi-IR question, nor per-site staging as such — it is the *substance* of the
IR: whether the middle is hand-Rust or bbnf-specified-and-generated. A homoiousious IR is
orthogonal to level-count — it may be a 3-level tower or a single annotated graph. The two
things already true in this tree make the family concrete rather than aspirational:

1. **bbnf is already self-describing.** `grammar/bbnf/bbnf.bbnf` (89 LOC),
   `expressions.bbnf` (37), `types.bbnf` (7) express the bbnf grammar *in bbnf*, with typed
   projections (`identifier = /.../ -> Span`, `int_lit = /.../ -> i64`,
   `float_lit = /.../ -> f64`), `@import { … } from "…"` module composition, and sum types by
   alternation-of-refs (`directive = import_directive | recover_directive | … ;`). Siblings
   `grammar/ebnf/ebnf.bbnf`, `grammar/bnf/bnf.bbnf`, `grammar/misc/{regex,g4,csv}.bbnf` prove
   the meta-grammar pattern is load-bearing, not a toy. It is presently **unwired** — grammar
   source is parsed by a 2142-LOC hand parser (`grammar/src/lib.rs`), and `bbnf.bbnf` is a
   dormant self-host. The homoiousia bootstrap target already exists on disk.
2. **The type plane already derives Rust types from bbnf `->` projections.** GN-3's
   `type_plane::derive` → `DirectSchemaSet` → `json_typed_direct::render` (the working
   1245-LOC direct-to-struct emitter, `json_typed_direct.rs:10`) turns a grammar's `->`
   chains into Rust structs. **Turn that same engine on the compiler's own IR grammars and it
   emits the IR enums.** The generator-that-eats-itself needs no new emitter — it needs the
   IR expressed as grammar and fed to the emitter that already exists.

The family's proposition, in one sentence: *`ir/src/lib.rs`, `passes::lower_expr`, and
`runtime_generator`'s templates are the last three hand-written organs in a pipeline whose
front (grammar→types) and whose analyses (HM, cost, recognizers) are already declarative or
derivable; homoiousia closes the loop by specifying those three as grammars + rewrite rules +
unparse, with a bootstrap fixpoint proving it.* Payoffs beyond parsimony: bootstrap alignment
(LOCK-7), rust/ts skeleton isomorphism by construction (LOCK-6 6B — one spec, two skeletons),
and dogfooding the type plane (LOCK-1/GN-3) on the compiler itself.

---

## 1. The mechanism, made concrete

Three substrates, each with an existing consumer (Y.13-honest by construction — every piece
below is read by a mechanism already in the tree).

### 1.1 Substrate A — the IR metagrammars (the node languages as bbnf)

Author `crates/ir/grammars/{hir,mid,backend}.bbnf`. Each IR node kind is a bbnf rule; each
Rust variant's payload type is a `->` projection. `ExprKind`'s ten arms become ten rules:
`expr_kind = seq | alt | repeat | optional | literal | regex | ref | annotation | project |
discard ;`, each alternative a `->` to a named constructor over typed/child-ref fields. The
`ExprId(usize)` arena indices (`ir/src/lib.rs:18-19`) are the graph edges — a child-ref
projection. `Project { body, target, ty }` is a three-field product; `Seq(Vec<ExprId>)` a
constructor over a repeated child-ref. An **attribute vocabulary** carries what a bare grammar
cannot: `@derive(Serialize,Deserialize)`, `@box body` (heap indirection, e.g.
`Box<BackendExpr>` `:377`), `@newtype RuleId = usize` (`:16`), `@serde(skip_if_none) target`
(`:469`).

The consumer is **the type plane itself**: `type_plane::derive(hir.bbnf)` produces a
`DirectSchemaSet` whose `DirectTypeKind::Struct`/`Sum` (GN-3.W1 adds `Sum`) map exactly onto
`ExprKind`'s enum-of-structs shape, and `json_typed_direct::render` emits the Rust. The
committed `ir/src/lib.rs` becomes `@generated`, its stage-0 form frozen; regen from
`hir.bbnf` must reproduce it byte-identical. This is the *generator eating itself*: the
schema-derivation path (GN-3) that produces user document structs from `twitter_search.bbnf`
produces the compiler's `ExprKind` from `hir.bbnf` — one path, two consumers.

**Reuse, not new build:** `direct_schema.rs` (the schema IR), `json_typed_direct.rs` (the
renderer), `lower/schema_direct.rs:11` (the bridge), and `type_plane/` (GN-3.W1-W3) are the
entire machine. Homoiousia's Substrate A is a **new input** (`hir.bbnf`) and an **attribute
extension** to `DirectSchemaSet`, not a new emitter.

### 1.2 Substrate B — the transpositions as grammar-to-grammar rewrite maps

A bbnf-native rewrite-rule form. The LHS is a pattern over a source IR grammar's node rules;
the RHS is a `value_expr` (the construction DSL bbnf *already has* — `expressions.bbnf`:
`value_fn_call`, `value_path`, `value_input`) over the target IR grammar's node rules:

```
# passes/lower.rewrite  (interpreted at generation time; zero runtime cost)
lower : Project{ body, target, ty }  ->  ValueProject{ body: lower(body), target, ty } ;
lower : Discard{ left, right, keep } ->  discard_lower(lower(left), lower(right), keep) ;   # @host
lower : Repeat{ body, min, max }     ->  RepeatLoop{ body: lower(body), min } ;
# every unmatched node is the identity map — auto-derived (the Nanopass catamorphism)
```

The default per-node arm is the **identity map, auto-derived from the node grammar**, so a
pass writes only the productions it changes — Chez's nanopass discipline. Structural rewrites
are grammar; non-structural kernels are **typed `@host` functions** (the
`hybrid-grammar-host` law): `discard_lower`, the HM `unify`, `derive_backend_shape`'s cost
selection, `derive_recognizers`' first-set fixpoint. The rewrite file names the *schedule and
the structure*; the host lattice supplies *analysis and semantics*. This is exactly
LOCK-7-C's host-function-lattice, but scoped to the IR spine, with a measured floor: the
family lives or dies on the **structural-fraction** — the proportion of each pass expressible
as identity+rewrite vs escaped to host (§3, risk 1; §4, PROVE-HOMO-2 measures it).

The seed already exists: `materialize_rule` (`passes/src/lib.rs:1357-1391`) *already* builds
IR nodes from IR nodes by splicing `SpanMark(Start) + body + SpanMark(End) + TapeEmit +
DirectBuild + Return` — a hand-written rewrite. Homoiousia lifts that splice into a rewrite
rule over `hir.bbnf`→`backend.bbnf`.

### 1.3 Substrate C — emission as grammar-derived unparse

The raw-string templates (`runtime_generator.rs:101-133,339-505`) are the antithesis of
grammar-to-grammar: the backend-IR→Rust map is opaque string splicing. Homoiousia replaces
the *structural glue* with unparse: a Rust-surface grammar (or the existing `@pretty`
machinery) run in reverse over the backend IR. The seed is `GrammarIr::pretty`
(`ir/src/lib.rs:124-215`) — an *already-grammar-driven unparser* for bbnf — and the `@pretty`
directives in `bbnf.bbnf` (`@pretty grammar block ;`) plus the `gorgeous`/`pprint` line
(MEMORY: aesthetics-critical, doc-integration-style). BNFC's grammar→AST+pretty-printer is
the model.

**The bounded claim (constitutional):** hand-written kernels stay hand-written — the JSON
string/number/NEON scanners, the `parse_that_regex` leaves (constitution: "everything is
grammar-driven … *save primitives and combinators*"; MEMORY: no-combinators-monolithic,
one-codegen-path, preserve-rich-ast). Unparse emits **structural descent glue and kernel
call-sites**, not kernels. The templates do not fully vanish; they collapse to a leaf
vocabulary of `@host`-referenced kernels the unparser emits *calls* to. Naming this honestly
is the difference between a real family and an over-claim (§3, risk 3).

### 1.4 Residualization (PROVE-12) as a grammar-to-grammar corollary

PROVE-12's win — const-fold the CSS delimiter membership to a `u128` immediate, monomorphize
per call site, +18 generated LOC, 0.81-0.98× cyc/B, byte-identical output (REPORT §3-6) — is,
in homoiousia terms, **one rewrite rule** over `backend.bbnf` with a stage-0 annotation:

```
residualize : find_delim{ set: @const s }  ->  find_delim_masked{ mask: delim_mask(s) } ;  # delim_mask is @host, comptime
```

The `@const`/stage marker rides the existing `ExprKind::Annotation { name, value }`
(`ir/src/lib.rs:257-260`); the mask fold is a comptime `@host` fn; the emitted immediate is
residual. PROVE-12 §7 explicitly promised this generalizes "to any grammar-derived
delimiter/STOP-set scan (JSON's structural scan, general-descent)" but demonstrated it on CSS
alone by hand-editing one template. Homoiousia *is* that generalization: **one rewrite rule,
N grammars**, vs PROVE-12's one-template-per-grammar. This is the family's differentiator win
and its cleanest falsifiable prototype (§4, PROVE-HOMO-3). It also names the LOCK-2 seam: the
rewrite *schedule* (when residualization fires, at what depth) is where an e-graph/CSP driver
would plug — but 3C's center is that residualization is a *rewrite over the IR grammar*, not a
bespoke template patch; the scheduling engine is LOCK-2's to supply.

---

## 2. The substrate & integration seams (path:line)

| Homoiousia piece | Extant code it re-homes / re-uses | Seam |
|---|---|---|
| IR node grammars (Substrate A) | `ExprKind` `ir/src/lib.rs:233-271`; `BackendExpr` `:376-413`; `Recognizer`/`SimdMode`/`TapeKind` `:415-463`; `DirectBuild*` `:465-535` | these enums become `@generated` from `hir/mid/backend.bbnf` |
| IR-enum emitter (reuse) | `type_plane::derive`→`DirectSchemaSet` (GN-3); `direct_schema.rs`; `json_typed_direct.rs:10` (render); `lower/schema_direct.rs:11` | feed IR grammars to the *same* schema→struct engine that serves user shapes |
| self-host bootstrap (LOCK-7 align) | `grammar/bbnf/{bbnf,expressions,types}.bbnf` (dormant); hand parser `grammar/src/lib.rs` (2142 LOC) | regen the parser/IR from `bbnf.bbnf`; committed stage-0 + fixpoint |
| transposition rewrites (Substrate B) | `infer_expr` `passes/src/lib.rs:203`; `lower_expr` Project arm `:1349`; `materialize_rule` `:1357-1391`; `derive_materialization_plan` `:1136-1287`; `lower_to_rust` `codegen/src/lower/mod.rs` | lift each hand match into a `.rewrite` rule set over IR grammars + `@host` calls |
| construction DSL (reuse) | `expressions.bbnf` (`value_expr`, `value_fn_call`, `value_path`, `value_input`) | the rewrite RHS *is* this existing value-expression grammar |
| host-fn lattice (LOCK-7-C) | `@host` directive `bbnf.bbnf:host_directive`; `RuntimeFrontendRequirements` `grammar_provider.rs:38-81` | the escape hatch for non-structural passes (cost, HM, recognizers) |
| emission-as-unparse (Substrate C) | `runtime_generator.rs:101-133,339-505` (raw templates); `GrammarIr::pretty` `ir/src/lib.rs:124-215`; `@pretty` `bbnf.bbnf` | structural glue unparle-emitted; kernels stay `@host` leaves |
| residualization rewrite (§1.4) | PROVE-12 patch (`runtime_generator.rs` `delim_mask`); `ExprKind::Annotation` `ir/src/lib.rs:257-260`; `css_scan.rs` STOP-set (712 LOC) | `@const` stage marker + comptime `@host` fold, one rule, N grammars |
| regen/bootstrap host (LOCK-7) | `xtask/src/regen.rs:19,36`; `codegen/src/lib.rs:101,130` (`emit_from_source`, `emit_typed_from_source`) | add IR-grammar regen targets; fixpoint gate `regen∘regen = regen` |

Codex consciousness (constitution ix): family 3C **re-cuts** two GN-boundaries as named
supersessions, never silent contradictions. (a) GN-1's `codegen/templates/{json,css,csv}/`
raw-string home (GN-1.md §4.0) is superseded — homoiousia's Substrate C dissolves structural
templates into a `backend.bbnf` unparse grammar; kernel leaves remain. (b) GN-3's
`type_plane::derive` is generalized from *user-document* schema producer to *universal* schema
producer (user shapes AND IR shapes) — GN-3.W3's derived≡hand gate becomes the template for
the IR-enum bootstrap gate. Both re-cuts are subtractive (fewer hand-written organs), aligned
with parsimony vi.

---

## 3. Prior art (SOTA, with citations) — this is proven at production scale

- **Nanopass framework (Chez Scheme).** Sarkar, Waddell, Dybvig, *"A Nanopass Infrastructure
  for Compiler Education"* (ICFP 2004); Keep & Dybvig, *"A Nanopass Framework for Commercial
  Compiler Development"* (ICFP 2013). Each IR level is a `define-language` form — *a grammar*;
  each pass is a `define-pass` that writes **only the changed productions**, the identity
  catamorphism auto-derived. Chez's production compiler is built this way. **This is
  homoiousia's flagship proof**: IR-as-grammar + transposition-as-declarative-map is a
  *shipping commercial compiler architecture*, not a research toy. Substrate B is the
  nanopass discipline in bbnf.
- **MLIR — ODS + DRR + PDL.** Operation Definition Specification (TableGen `.td`) declares
  ops/dialects *as data*; Declarative Rewrite Rules (`Pat<(SrcOp …),(ResOp …)>`) specify
  lowerings; PDL makes rewrite patterns first-class IR. Lattner et al., *"MLIR: Scaling
  Compiler Infrastructure for the Domain"* (CGO 2021). **The second flagship**: IR
  grammar-specified + transpositions as declarative rewrite maps, at LLVM scale. Names the
  build-time-tax risk (TableGen) homoiousia must measure (§3 risk 4).
- **BNFC (BNF Converter).** Forsberg & Ranta. A labelled BNF grammar generates AST types +
  parser + **pretty-printer/unparser** across backends. Direct model for Substrate C
  (emission-as-unparse) and Substrate A (grammar→AST types).
- **Stratego/XT + Spoofax + Rascal.** SDF (Syntax Definition Formalism) + Stratego strategic
  term rewriting = the academic lineage of "the transposition is a rewrite map with a
  schedule." Confirms the strategy/schedule separation (structure vs traversal control) that
  bounds host-escape.
- **egg / egglog.** Rewrite rules as data, equality saturation (Willsey et al., POPL 2021).
  The *optimization* cousin of Substrate B; LOCK-2 owns the e-graph, so 3C uses **directed**
  rewrite-as-lowering (nanopass/DRR), and cedes saturation-scheduling of residualization to
  LOCK-2 (§1.4).
- **Negative precedent (why this is non-trivial):** rustc (HIR/MIR hand-written Rust, query
  system not grammar-specified), GHC Core (hand-written; only RULES pragma is declarative),
  and `regex-syntax` HIR (hand-written) all chose hand-written IR. The reason is uniformly
  **rewrite expressiveness** — the analyses (borrow-check, type inference, strictness) are not
  structural rewrites. This is precisely why homoiousia's honest claim is *hybrid*
  (grammar-structure + host-analysis), and why the structural-fraction measurement (§4) is the
  family's make-or-break, not a formality.

---

## 4. Named risks, counterexamples, and exactly what a prototype must test

**Risk 1 — the rewrite-expressiveness wall (the sharpest).** Not every transposition is a
structural rewrite. `derive_recognizers` (`passes/src/lib.rs:338`) computes a
`StructuralAlphabet` from a first-set fixpoint; `derive_backend_shape` (`:398`) is a
cost-model search; HM `unify` is union-find. *Counterexample:* if a pass is 80% `@host`, the
"grammar-specified" label is hollow — consumer-less-substrate-adjacent (Y.13). *Resolution:*
the hybrid-grammar-host boundary; the family's honest claim is that *structure* is grammar and
*analysis* is a typed host lattice. **The prototype must measure the split.**

**Risk 2 — bootstrap byte-identity reachability.** The hand `ExprKind` carries
`#[serde(default, skip_serializing_if="Option::is_none")]` on `DirectBuildField.target`
(`ir/src/lib.rs:469`), `Box<BackendExpr>` indirections (`:377`), and newtype `RuleId(usize)`
(`:16`). A bare grammar expresses none of these. *Counterexample:* derived enum ≠ hand enum on
attributes → fixpoint fails. *Resolution:* an attribute vocabulary (`@derive`, `@box`,
`@serde`, `@newtype`) in the metagrammar — the same information-moves-into-the-grammar cure as
GN-3 Wall-1's capacity hints (GN-3.md R1). This is the reachability risk of like class and
must be proven, not asserted.

**Risk 3 — emission-as-unparse cannot fully retire templates.** `json_templates/parser.rs`
interleaves structural descent with kernel calls. *Counterexample:* if unparse emits only
glue, the raw strings survive wherever glue and kernel interleave → a *partial* win.
*Resolution:* bound Substrate C to structural emission; kernels become `@host` leaves the
unparser calls. Name the residual template surface explicitly (constitution vii).

**Risk 4 — build-time tax + debuggability (parsimony vi).** A comptime rewrite interpreter is
zero *runtime* cost (constitution v, satisfied) but may tax regen and obscure debugging (the
MLIR-TableGen lesson). *Counterexample:* if grammar-specified regen is 3× slower or the
generated Rust is unreadable, KISS/parsimony is violated. *Resolution:* measure regen wall
time; require the generated Rust to be byte-identical to the hand form (so debuggability is
unchanged — you read the same output).

**Risk 5 — MEMORY invariants.** `preserve-rich-ast`, `typed-materialization-invariant`
("every `->` reaches the tape emitter"), `no-ts-ir` ("AST IS the combinator"). Homoiousia must
*strengthen* not flatten: the IR grammar's projections *are* the materialization contract,
checkable by the regen fixpoint. No IR layer is *added*; the existing tower is *re-substanced*.

### The prototypes that move the family (falsifiable, cheapest-first)

- **PROVE-HOMO-1 — IR-enum-from-grammar bootstrap (decisive, cheap).** Author
  `crates/ir/grammars/hir.bbnf` for `ExprKind`'s 10 variants + an attribute vocabulary. Run the
  *existing* GN-3 `type_plane::derive` + `json_typed_direct::render` to emit a Rust enum. Gate:
  emitted enum ≡ hand `ExprKind` (`ir/src/lib.rs:233-271`) byte-identical (modulo the attribute
  vocabulary), and a real grammar (json.bbnf) parses identically through generated vs hand enum.
  *Measures:* how much attribute machinery the metagrammar needs (risk 2). Mirrors GN-3.W3's
  derived≡hand gate, reusing its engine — near-zero new code.
- **PROVE-HOMO-2 — one transposition as a rewrite set (the make-or-break).** Express
  `lower_expr`'s Project/Discard/Repeat/Seq/Alt arms (`passes/src/lib.rs`) as a `.rewrite` file
  + `@host` calls; interpret at comptime; gate byte-identical `BackendIr` vs hand `lower_expr`
  on json/css/sheets. *Measures:* the structural-fraction (risk 1) — the number that decides
  whether the family is real. Target: ≥70% of arms are identity+structural-rewrite, the rest
  named `@host`.
- **PROVE-HOMO-3 — PROVE-12 as generalized residualization rewrite (the differentiator).**
  Re-derive PROVE-12's const-fold as ONE rewrite rule over `backend.bbnf` (`@const` annotation
  + `delim_mask` comptime `@host`), applied to BOTH the CSS delimiter scan AND a second
  grammar's STOP-set (JSON structural scan or general-descent). Gate: reproduce PROVE-12's
  byte-identical CSS `generated.rs` (+18 LOC, 0.81-0.98× cyc/B — REPORT §4-6) AND fire the *same
  rule* on the second grammar. Proves the "one rewrite, N grammars" claim PROVE-12 §7 asserted
  but never demonstrated — the family's concrete over-the-baseline win.

**Convergence pivot for pass 2:** PROVE-HOMO-2's structural-fraction number. If ≥70%, the
family is a nanopass-style hybrid worth carrying; if the analyses dominate and structure is a
thin veneer, homoiousia collapses to "hand-written passes with a cosmetic grammar wrapper" and
should bank in favor of a sibling. That measurement is the single fact pass 1 hands pass 2.
