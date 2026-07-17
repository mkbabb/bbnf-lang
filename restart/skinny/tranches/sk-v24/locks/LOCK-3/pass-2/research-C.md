# LOCK-3 · Family 3C — GRAMMAR-HOMOIOUSIA · pass-2 research

**Measurement/pointer base.** All `path:line` below are read against the live working tree
`bbnf-lang/skinny @ 06e70053f` (the portfolio-mint commit `b3cb8c16b` is its ancestor; nothing
here re-uses a stale-base number — the family lands no perf claim). skinny@HEAD is the sole
living implementation substrate; the design targets greenfield, ports scope organ-level.

---

## 0. What homoiousia *is*, stated against the live tree

Homo**i**ousia = "of *like* substance," deliberately not homo-ousia ("same substance"). The
strong-form family is therefore **not** ONE-HIR: it does not demand a single node type shared by
the grammar plane and the regex plane. XL-C (ratified) keeps the grammar tier's regex opaque —
`ExprKind::Regex { pattern: String }` (`crates/ir/src/lib.rs:250-252`) is a leaf string, never
exploded into grammar nodes — so 3C no longer hangs on any ONE-HIR verdict. The claim narrows to
its defensible core:

> Every IR level is a **grammar-shaped sum-of-products node algebra with a `.bbnf` face**, and
> every transposition between adjacent levels is a **total grammar-to-grammar map** carrying a
> round-trip oracle. The generator, applied to the grammar that specifies an IR level, emits that
> level's Rust types — the generator eats itself (bootstrap alignment with LOCK-7).

The live tree already realises this for the **front** IR and refutes it for the **back** IR. That
split is the whole finding.

---

## 1. The live witness — the front IR is already homoiousia

`GrammarIr` (`ir/src/lib.rs:33-39`: `name, source_hash, rules: Vec<Rule>, exprs: Vec<Expr>`) is a
textbook sum-of-products algebra. `ExprKind` (`ir/src/lib.rs:234-271`) is a **10-arm sum**; the
grammar's own `Alt { branches, mode }` (`:236`) is the sum node and `Seq(Vec<ExprId>)` (`:235`) the
product node. The bidirectional grammar face **exists and is total over all ten arms**:

- forward: `parse_grammar(name, source) -> GrammarIr` (`crates/grammar/src/lib.rs:29`).
- backward: `GrammarIr::pretty() -> String` renders `.bbnf` text (`ir/src/lib.rs:124-204`) —
  every arm handled, including `Project` → `body -> target : ty` (`:185-193`) and `Discard` →
  `<< / >>` (`:194-202`).

So the homoiousia **functor witness is buildable on live today**:
`parse_grammar(g.name, g.pretty()) ≅ g`. It is the E1∘E2 round-trip and it costs zero runtime
(pure generator-side). This is the concrete substrate the family stands on — not aspiration.

The **self-hosting grammar sources are already authored** (the "generator eats itself" corpus):
`grammar/bbnf/bbnf.bbnf` (bbnf-in-bbnf), `grammar/bbnf/expressions.bbnf` (the `->` value
sub-grammar — precedence-climbing `value_or…value_atom`, with `int_lit … -> i64`,
`float_lit … -> f64`, `string_lit … -> Span`), `grammar/bbnf/types.bbnf`, `grammar/misc/regex.bbnf`,
`grammar/bnf/bnf.bbnf`, `grammar/misc/g4.bbnf`. The meta-circular front-end is real source, not a
plan.

---

## 2. The live refutation — the back IR is *not* homoiousia (the net-new work)

`BackendExpr` (`ir/src/lib.rs:376-413`) is a **13-arm sum** but has **no grammar face**: its only
`Display` is `Debug`-format one-liners (`ir/src/lib.rs:589-600`) — no `.bbnf`, no `parse_grammar`
inverse, no round-trip oracle. Likewise `DirectSchemaSet`/`DirectTypeKind`
(`crates/codegen/src/direct_schema.rs:4-58`) has no grammar face. Concretely, the labelled
transposition edge-DAG of the live spine, read through the homoiousia lens:

| edge | function (path:line) | source→target | grammar-faced target? |
|---|---|---|---|
| E1 | `parse_grammar` (`grammar/src/lib.rs:29`) | text → `GrammarIr` | yes (is a grammar) |
| E2 | `GrammarIr::pretty` (`ir/src/lib.rs:124`) | `GrammarIr` → text | yes (round-trip lives) |
| E3 | `Infer::infer_expr` (`passes/src/lib.rs:185-213`) | `GrammarIr` → `Type` (`passes:234-243`) | no (`Type` enum, no face) |
| E4 | `lower_expr` (`passes:1330-1360`) + `materialize_rule` (`passes:1362-1389`) | `GrammarIr` → `BackendIr` | no (`BackendExpr` Debug only) |
| E5 | `lower_to_rust` (codegen `lower/mod.rs`) | `BackendIr` → Rust | — (Rust is the sink) |
| E6 | `type_plane::derive` (GN-3, born-RED) | `GrammarIr` → `DirectSchemaSet` | no (schema, no face; **sum-gated**) |

`materialize_rule` (`passes:1362-1389`) splices exactly five backend constructors —
`SpanMark(Start)` · `body` · `SpanMark(End)` · `TapeEmit{kind}` · `DirectBuild{shape,fields}` ·
`Return` — that together are **one value level** (rule-materialisation into a tape node + direct
struct). A homoiousia BackendIr would name that five-constructor sub-algebra as grammar rules with
a `.bbnf` and an E4 round-trip oracle mirroring E1∘E2. **That is the net-new homoiousia work: give
E4's and E6's targets grammar faces so the whole tower is grammar-to-grammar.**

---

## 3. The consumer question (constitution viii / Y.13 — consumer-less substrate forbidden)

A BackendIr `.bbnf` that nothing regenerates is dead substrate. The family therefore stands or
falls on **naming a downstream wave that consumes an IR expressed as a grammar**. There are exactly
two live-adjacent consumers, and the honest verdict differs between them.

### 3a. Consumer #1 — GN-3, THE TYPE PLANE (on/near live; the family's real anchor)

The type plane's `type_plane::derive` authors `DirectSchemaSet` **by reading the grammar's own
`Project` and `Alt` nodes** — it consumes the grammar-as-IR structurally instead of re-deriving it
by heuristic. This is the only place a grammar-shaped IR is genuinely eaten on/near live. Two
**born-RED gates own the homoiousia sum edge (E6)**:

- **Gate A — `DirectTypeKind::Sum`** (owner **GN-3.W1**; seam `direct_schema.rs:52-58`, today
  `Struct`-only; +420 LOC; consumer `json_typed_direct::render`'s new sum arm, same wave). This is
  **exactly** the "schema construct beyond today's Struct-only `DirectTypeKind`" that gates the
  sum-node algebra. Without it the grammar's natural sum — `value = object|array|string|number|
  bool|null` (`ExprKind::Alt`) — has no schema image and the formulation collapses to the
  product-only subset. **The directive forbids that subset from standing for the family.**
- **Gate B — thread `Project.ty` at `passes/src/lib.rs:204`** (owner **GN-3.W2**). This is the
  shared `:204` obligation, and homoiousia's answer to "*consume one threaded node-type, do not
  re-derive it*" is structural: the `->`'s `ty` is a **node** in the grammar-IR
  (`ExprKind::Project.ty`, `ir/src/lib.rs:264`), carried verbatim by `lower_expr`
  (`passes:1349-1353`) into `BackendExpr::ValueProject.ty` (`ir/src/lib.rs:407-411`). The
  type-preserving grammar-to-grammar map reads that node into `DirectBuildTarget.type_ref` — the
  field `DirectBuildField.target` that is **always `None` today** (`ir/src/lib.rs:466-471`). It does
  **not** re-classify. The threaded node is consumed at `derive_materialization_plan`'s
  `DirectBuildTarget.type_ref` fill (the skinny seam for the `TypeAnalysis::from_ir`-class
  consumer the shared directive names).

homoiousia **retires** the two live re-derivations that stand in for the missing map:
`ProjectionKind` + `classify_projection` — the lossy 4-way fold
(`crates/codegen/src/general_descent/view.rs:20-46`, the *entire* type expressiveness reaching
emission today) — and the byte-literal role classifier `derive_materialization_roles`
(`passes:1403-1490`). Both are re-derivations of information the grammar already declares; the
grammar-to-grammar map makes them dead.

### 3b. Consumer #2 — LOCK-7 bootstrap (the generator eats the IR grammar) — **born-RED edge, not a GN-3 deliverable**

If `ir/src/lib.rs`'s node enums are themselves emitted by codegen from an IR-describing `.bbnf`,
the consumer is the **bootstrap fixpoint gate** (regen∘regen = regen, byte-identical). Live
enabling evidence: codegen already emits Rust `struct`/`enum` from `ShapeFacts`
(`ir/src/lib.rs:537-581`, `Shape::{Struct,Enum}`), and the self-hosting `.bbnf` corpus exists (§1).
**But** no live wave regenerates the `ir` crate from a grammar — codegen emits *parsers*, not the
compiler's own type definitions. So this consumer is carried as a **born-RED dependency edge owned
by LOCK-7 (the GN bootstrap tranche)**, never landed speculatively. Reporting it as an edge with a
named owner is the correct disposition; landing a BackendIr `.bbnf` with no regen consumer would
violate clause viii.

---

## 4. The trap the directive warns against, made concrete

`derive_shape_facts` (`passes/src/lib.rs:285-294`) **already emits a 6-variant `JsonValue` enum** —
`Object/Array/String/Number/Bool/Null`. It looks like the sum already works. **It does not consume
the grammar's `Alt`.** The variant list is a hardcoded array literal (`passes:285-291`); the
per-kind shape names come from byte-literal role heuristics (`derive_materialization_roles`,
`passes:1403`) and the `has_rule("value")` name-gate (`runtime_generator.rs:99`). This is precisely
the "product-only, sum-excluding subset" the directive says must not stand for the family: a
**fabricated** product masquerading as a derived sum. The homoiousia deliverable is to **replace**
`derive_shape_facts`' hardcoded list + `derive_materialization_roles`' classifier with the
`ExprKind::Alt → DirectTypeKind::Sum` grammar-to-grammar map, and prove the derived sum is
byte-identical to today's fabricated list — the equality *is* the proof the map recovers what is
now invented.

---

## 5. Prior art (cited)

- **Nanopass** (Sarkar, Waddell, Dybvig, *A Nanopass Infrastructure for Compiler Education*, ICFP
  2004; Keep & Dybvig, *A Nanopass Framework for Commercial Compiler Development*, ICFP 2013). The
  direct precedent: `define-language` forms specify **each IR as a grammar**; each pass is a total
  map between adjacent grammars; boilerplate + differential checking auto-generated. Homoiousia is
  Nanopass with a bbnf as the schema language and the *same* generator producing both the parsers
  and the IR types.
- **MLIR** (Lattner et al., *MLIR: Scaling Compiler Infrastructure for the Domain*, CGO 2021).
  Dialects declared in ODS/TableGen generate op C++ classes from a spec — the "generator eats
  itself" analogue. Weaker than homoiousia: MLIR ops are not a context-free grammar; the bbnf spec
  is.
- **Spoofax / SDF3 + Stratego** (Visser et al.). Object grammar generates the term algebra;
  transformations are term rewrites — literal grammar-to-grammar transformation. Closest full-system
  precedent.
- **GHC "Trees That Grow"** (Najd & Peyton Jones, 2017) — the **contrast/counterexample**: one AST
  type extended per phase via type families (one decorated type, many phases) = the 3B single-IR
  choice. Homoiousia deliberately chooses *distinct grammar-shaped IRs* with round-trip oracles;
  TTG is the "don't" for this family.
- **rustc `rustc_ast → HIR → THIR → MIR`** — the tower *without* homoiousia: distinct IRs, lowering
  functions, but no grammar spec and no round-trip oracle. It shows the tower is standard practice;
  what homoiousia adds is the grammar face + the differential oracle per edge.
- **regex-automata HIR** (BurntSushi) — a hand-written sum type, no grammar spec; the ONE-HIR
  temptation XL-C now sidesteps by keeping `Regex(String)` opaque.

---

## 6. Named risks & counterexamples

- **R-1 (the governing risk): consumer-less substrate.** Giving `BackendExpr`/`DirectSchemaSet` a
  `.bbnf` face has no same-wave live consumer unless GN-3 (schema, E6) or LOCK-7 (bootstrap) reads
  it. **Resolution:** the only homoiousia work that lands *in GN-3* is the E6 sum edge with GN-3's
  own renderer as consumer; the BackendIr grammar face is a born-RED LOCK-7 edge, never landed on
  spec.
- **R-2: the live round-trip is lossy — `pretty()` is not information-complete.** `pretty`
  (`ir/src/lib.rs:145-152`) emits `Alt` as bare `|` and **drops `AltMode`** (Dispatch vs
  Speculative, `ir/src/lib.rs:273-277`); it also erases `SourceSpan` and renumbers `ExprId`. So
  `parse_grammar(g.pretty()) ≠ g` under `==`; the functor holds only as *structural equality modulo
  ExprId-renumber + span-erasure + AltMode-recovery*. The homoiousia oracle needs a face that
  **preserves AltMode and spans** — proving `pretty` insufficient as-is is the first concrete
  finding a prototype must surface.
- **R-3: regex opacity boundary (XL-C).** homoiousia holds `Regex{pattern:String}` opaque
  (`ir:250`); the regex sub-tree is a homoiousia **leaf**, not a grammar-to-grammar image. An author
  who expects `->` *inside* a regex to type cannot — arrows live only at grammar-node granularity.
  Whether regex earns its own homoiousia is LOCK-5's, not 3C's.
- **R-4: sum canonicalisation.** `object|array|…` and any reordering must map to the *same*
  `DirectTypeKind::Sum`, or the E6 map is not a well-defined functor and derived ≠ the fabricated
  list. The map must be set-equal variant unify with canonical ordering (GN-3.W1's
  order-insensitivity gate, restated as a functor law).
- **R-5: capacity/enum facts are document facts, not grammar facts.** The hand schema carries corpus
  counts (`vec_with_capacity(…, 875/5_759/10_001)`) and closed enum tables. A grammar-to-grammar map
  cannot invent them; they enter only if the typed grammar *declares* them (`-> Vec<T> @cap(875)`).
  This bounds where byte-identity is honestly reachable — the sharpest limit on the "generator eats
  itself" claim.

---

## 7. What a prototype must test to move family 3C

1. **Front-IR round-trip oracle on live** (`bbnf-lang/skinny @ 06e70053f`). Build
   `parse_grammar(g.name, g.pretty()) ≅ g` (structural-eq modulo ExprId/span) over the three live
   grammars (json, css-l4, sheets) **and** the self-hosting `grammar/bbnf/bbnf.bbnf` +
   `grammar/misc/regex.bbnf`. Report per-arm loss; **prove R-2** (AltMode/span drop). Cheapest
   concrete homoiousia proof; zero runtime.
2. **The E6 sum edge, born-RED** (GN-3.W1 anchor). Land `DirectTypeKind::Sum`
   (`direct_schema.rs:52`) + a `derive` mapping `ExprKind::Alt` over the `value` rule →
   `DirectTypeKind::Sum`; assert `json_typed_direct::render` emits a 6-arm enum **byte-identical to
   the hardcoded `derive_shape_facts` output** (`passes:285-291`). Consumer = the renderer, same
   wave. Passing proves the grammar-to-grammar map recovers what is fabricated today, retiring the
   hardcode.
3. **Consumption-not-re-derivation, shared `:204`** (GN-3.W2 anchor). Thread `Project.ty`
   (`passes:204`) into `DirectBuildTarget.type_ref` (was `None`, `ir:466-471`); assert the type
   reaching `json_typed_direct` equals the grammar-declared `ty` with **zero** `classify_projection`
   calls (grep count `== 0` in `general_descent/`). Teeth: `check-sheets` regen byte-identical.
4. **Bootstrap consumer feasibility (LOCK-7 edge, report-only).** Hand-author an `ir.bbnf`, run
   codegen through the existing `Shape::Enum` path (`ir:571-581`), diff the emitted enum against the
   hand-written `ExprKind`. If codegen emits only parsers (not bare type defs), *that* is the exact
   born-RED gap LOCK-7 owns — report the edge, do not land it.

**Disposition.** 3C is *real for the front IR today* (E1∘E2 lives; self-hosting `.bbnf` authored)
and *born-RED for the back IR and the sum edge*. Its single legitimate live consumer is GN-3's type
plane via the two named born-RED gates (Sum node + threaded arrow); its second consumer (IR-grammar
bootstrap) is a LOCK-7 dependency edge, carried, not landed. The family must never be represented by
the product-only, name-gated `derive_shape_facts` subset — that is the fabrication homoiousia exists
to retire.
