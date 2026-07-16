# LOCK-7 pass-2 — research-B (family 7B: TOTAL-SELF-HOST)

Measurement HEAD: skinny@HEAD (the repo-root `skinny/` workspace, per R1 §0). Paths
absolute-resolvable under `/Users/mkbabb/Programming/bbnf-lang`. Read-only recon; no build.

## 0. The family thesis, restated as a falsifiable claim

7B asserts: *bbnf grammar, regex grammar, IR grammar, emitter templates — all
grammar-specified; the generator becomes mostly data; hand code shrinks to kernels and
primitives.* The measurable content of "mostly data" splits into three planes, each with a
different verdict on this substrate: (a) the **front-end** plane (parse `bbnf.bbnf` itself),
(b) the **IR** plane (is `GrammarIr`/`BackendExpr` grammar-expressed), (c) the **emitter-
template** plane (is codegen control-flow/dispatch/tape-assembly grammar-plane data). The
distinctive 7B scope is (b) and (c) — (a) is the 7A/7C surface. This report measures (b) and
(c) on concrete substrate and names the irreducible kernel escape.

## 1. The substrate as it stands (given, verified)

- **The general emitter is a RECOGNIZER emitter, not a GrammarIr builder.** `grep -rn
  "GrammarIr\|add_expr\|ExprKind" crates/codegen/src/general_descent/` returns **zero**. The
  path (`general_descent/{mod,emit,view,regex_first}.rs`, 1206 LOC) emits one
  `parse_<rule>(bytes, &mut cursor, &mut tape)` **recognizer** per rule
  (`emit.rs:260 render_rule`), each of which `push_offset`s onto the existing `TapeBuilder`
  (`emit.rs:218,251`), plus a lazy `{Prefix}Document` view (`view.rs:255 render_view`). It
  reconstructs **no** `GrammarIr` — it produces a sparse offset-tape + an on-demand cursor
  decode (`view.rs:335 rule_kind`, `:342 span`). This is the load-bearing fact under the
  shared empirical question (§4).
- **`->` is consumed by the VIEW, discarded by the RECOGNIZER.**
  `emit.rs:376 SinkOnlyExpr::ValueProject` emits only the body — "the projection (target/ty)
  is consumed by the VIEW, not the recognizer." The view re-derives a value category from the
  projection TEXT (`view.rs:35 classify_projection`: `f64`→Float, `true/false`→Bool,
  `Nu8`→Tag, else Span) and emits typed accessors (`as_f64`/`as_bool`/`as_tag`, `view.rs:118`).
  So a `-> Span`/`-> T` on a structural rule is recovered *lazily from the tape span*, never
  materialized into a typed node at parse time.
- **The IR tower is hand-written Rust, NOT grammar-expressed.** `find grammar -name '*ir*.bbnf'`
  → nothing; **`ir.bbnf` does not exist.** `GrammarIr` (`ir/src/lib.rs:34`), `ExprKind`
  (`:234`, nine arms), `BackendExpr` (`:376`), `BackendShape` (`:361`) are hand ADTs. LOCK-3's
  homoiousia (3C) is unrealised on this substrate.
- **Construction is host-bound to two mutation primitives.** `add_expr` (arena append,
  `ir/src/lib.rs:51`) and `resolve_refs` (cross-rule link, `ir/src/lib.rs:84`). `resolve_refs`
  is a two-phase `&mut self` TreeFold: build `name→RuleId` (`:86`), then mutate every
  `ExprKind::Ref{target}` in the arena to the resolved id (`:95-105`). It is a **global
  mutation over the whole expr arena**, invoked once post-parse at `grammar/src/lib.rs:33`.
- **`bbnf.bbnf` is NOT wired to any regen target.** `grep "regen-bbnf\|bbnf.bbnf\|fixpoint"
  xtask/src` → **empty**. There is no `regen-bbnf` command in `xtask/src/main.rs` (only
  json/css/sheets/real-typed). Routing `bbnf.bbnf` through the emitter is net-new xtask work
  (a new target mirroring `regen.rs:19 write_targets`), and the legacy ~88s→0.5s fixpoint
  datum has **no transplant present in skinny** — it is a legacy-tree number, absent here
  (relevant chiefly to 7A; recorded so 7B does not lean on it).

## 2. Q-SCOPE — does the IR tower go homoiousia, and who consumes `ir.bbnf`?

**Substrate verdict: LOCK-3 has NOT adopted grammar-homoiousia; `ir.bbnf` has no consumer
today, and the constitution (clause viii, Y.13) forbids landing it without one.** The design
space, drawn concretely:

- **If LOCK-3 adopts 3C**, the joint LOCK-3⊗LOCK-7 IR-plane family is: `ir.bbnf` defines the
  `ExprKind` sum (Seq/Alt/Repeat/Optional/Project/Discard/Literal/Regex/Ref — the nine arms at
  `ir/src/lib.rs:234`) as a grammar with `->` projections naming each node's payload. **Its
  three named consumers, in dependency order:** (1) the **parser builder** — `add_expr`
  (`ir/src/lib.rs:51`) becomes the grammar's construction target, i.e. the projection tower's
  `->` output IS an `ExprKind` node; (2) **`resolve_refs`/`validate`** (`ir/src/lib.rs:84,110`)
  — the shared semantic pass that must run on the grammar-defined IR exactly as on the hand IR
  (7D's equivalence pivot); (3) the **lowering fold** `lower_expr`
  (`codegen/src/lower/sink_only.rs:133`, the `BackendExpr→SinkOnlyExpr` map at `:217-233`),
  which reads the IR to emit. Naming (1)-(3) satisfies Y.13: `ir.bbnf` is a substrate with a
  same-wave consumer, not a spec-only artifact.
- **If LOCK-3 does NOT adopt**, what remains hand-written: the entire IR tower
  (`GrammarIr`/`BackendIr`/`BackendExpr`/`BackendShape`) and the two mutation primitives. 7B's
  totality then **caps at the grammar-AST front-end + the emitter-template plane** — "the
  generator becomes mostly data" is FALSE for the middle IR. The honest scope of 7B without
  LOCK-3 is: front-end self-host (7A/7C) + emitter templates as data (§3), with the IR sum an
  admitted hand kernel. This is the strongest defensible 7B if LOCK-3 declines: **totality is
  bounded at the two enum boundaries** (`ExprKind` in, `BackendExpr`/`SinkOnlyExpr` out), and
  those boundaries are the named non-total kernels.

The lawful default is *narrow-first*: draft `ir.bbnf` only as the grammar that `derive`s the
existing enum (the in-tree `egraph-derive` + memory `derive-language-macro` precedent: a
`Language` impl projected from an existing enum, recursive fields → `Id`). The grammar does
not replace the enum; it is the enum's single source, and the hand enum is generated FROM it.
That keeps consumers (1)-(3) reading a real Rust type while the type's *definition* is
grammar-data.

## 3. Q-STRUCTURE — codegen control-flow as grammar-plane data; the named kernel escape

**The measured blob: `general_descent/emit.rs`, 452 LOC (the ONE emitter blob).** The
data-vs-escape ratio, counted:

- **195 / 452 LOC (43%) are pure raw-string template DATA** — the fixed recognizer scaffolding
  emitted verbatim inside `r#"…"#` blocks (`ParseError`, `match_literal`, `match_leaf`,
  `skip_ws`, `consume_quoted`, `mark`; `emit.rs:39-77, 96-255`). Grammar-plane-expressible as a
  constant template with no holes.
- **257 / 452 LOC (57%) are host dispatch logic** — `emit_expr` (`:288`) is a `match` over the
  13-arm `SinkOnlyExpr` sum (`lower/sink_only.rs:69-100`), each arm writing a per-node template
  with a handful of holes (`pad`, `lit`, `callee_ident`, `first_lit`, `nullable`, `node_kind`).
  **This dispatch is itself ~90% mechanically data-convertible:** a `match`-over-a-sum-that-
  emits-a-template-per-arm is exactly a `->`-projection tower over the `SinkOnlyExpr` sum type —
  one grammar alternation branch per variant, each projecting to its template string. The
  recursion (`emit_expr` self-calls for Seq/Alt/Repeat) is a grammar rule that recurses.
  Net: **the emitter is ~95% expressible as grammar-plane data (constant templates + a fold
  over a sum).**

**THE NAMED KERNEL-ESCAPE (the deliverable): the speculative-cursor reborrow.** The
irreducible residue is not lines of the emitter — it is a *semantic primitive the emitter must
emit* that has no `->` denotation. `emit_expr_probe` (`emit.rs:438-452`) emits
`let cursor = &mut _probe;` and every speculative construct (Alt `emit.rs:400`, Optional
`:344`, RepeatLoop `:311`) wraps its branch in an IIFE `(|| { … Ok(()) })()` then commits
`if _r.is_ok() { *cursor = _probe; }` (`:354, :421-424, :327-331`). **15-19 escape-idiom
sites in this one blob** (`grep "_probe\|(|| {\|&mut _probe"`). This closure-with-reborrow
exists **solely to satisfy Rust's borrow checker under PEG ordered-choice**: backtracking needs
cursor save/restore, and you cannot hold `&mut TapeBuilder` plus a mutable `*cursor` across a
speculative branch that may fail and roll back without either (a) a capturing closure that
*reborrows* the cursor, or (b) an explicit snapshot-state struct. A pure `->` projection is a
*value function* `input → value` with no effects and no notion of "undo"; the speculative-
reborrow is *effectful control with rollback semantics*. **The named kernel-escape failure
signature: the transactional cursor reborrow — `let cursor = &mut _probe` inside an IIFE,
committed on `Ok`.**

**The data-vs-escape ratio as a design number:** ~95% of the emitter's *substance* is
grammar-plane data (template + sum-fold); the residue is not a percentage of lines but a
single semantic kernel — *speculative rollback over a mutable tape+cursor* — that stays host.
7B's honest form is therefore: emitter templates ARE data; the parse-driver kernel (backtrack
+ commit) is a primitive alongside the combinators, per constitution clause i's "save
primitives and combinators."

## 4. The shared empirical question — does a `-> Span`/`-> T` builder augment round-trip?

**Substrate-informed answer (still RED-unknown until a prototype drives it, but the substrate
points hard one way):** the recognizer emitter as it stands does **NOT** reparse a corpus into
GrammarIr-equivalent structure, because it **builds no GrammarIr** — it builds a sparse
offset-tape + a lazy view (§1). A `-> Span` on a structural rule is discarded by the recognizer
(`emit.rs:376`) and recovered lazily from the tape span by the view (`view.rs:342`). To get
*GrammarIr-equivalent* structure you must author a **net-new builder augment** that materializes
`ExprKind` nodes as a parse effect — i.e. calls `add_expr` (append) during descent and runs
`resolve_refs` after — precisely the host-bound construction census. That augment does **not**
ride free over the recognizer; it is a new emit mode (a `DirectBuild`-style shape for
*grammar-AST* nodes rather than JSON value nodes; today `DirectBuild` exists only for the JSON
sink, `sink_only.rs:93,221`).

**Where it will cascade, named:** the builder augment must confront the same speculative-
rollback kernel (§3) *plus a second effect* — **the tape/build is NOT rolled back on
speculative failure.** The probe idiom restores the CURSOR (`_probe`/`_alt_start`) but the
`tape` is the outer `&mut TapeBuilder`, un-snapshotted; a branch that runs `mark(tape, …)`
(`emit.rs:388`) or `match_leaf`'s `push_offset` (`:218`) and then fails leaves a **stale
offset on the tape**. A GrammarIr builder augment would leave a **stale `add_expr` node in the
arena** on the same failure. This is the cascade point: ordered-choice + effectful construction
= transactional-undo requirement. The recognizer survives today only because Sheets' Alts are
first-byte-disjoint / leaf-terminal (no earlier branch pushes before a later element fails);
`bbnf.bbnf`'s `term` alternation (`bbnf.bbnf:29-36`) and its `factor`/`binary_factor` towers
are NOT so disjoint, so the augment will trip here first.

**On `regen∘regen` byte-identity:** the emitter is a fixed-template + sum-fold (§3), so for a
*fixed input grammar* it stamps deterministic bytes trivially — **this certifies nothing about
the builder augment** (the 7A warning generalizes: a const-emit path is byte-stable for any
input). The non-vacuous fixpoint subject for 7B is **GrammarIr equivalence** — parse
`bbnf.bbnf` with the augmented generated parser, assert the resulting `GrammarIr` equals
(modulo `Empty ≡ Concat([])`, 7D's relation) the hand parser's `GrammarIr` on the same
source — NOT generated-`.rs` byte identity.

## 5. Prior art (SOTA, cited)

- **Semantic actions are universally host, never grammar-data.** yacc/bison `{ $$ = …; }`,
  ANTLR actions, tree-sitter (no actions — construction is engine-internal C). Across every
  major generator the *value-construction plane escapes to host code*; the grammar carries
  structure, the host carries effectful build. bbnf's `->` is the same split: a projection is a
  pure value function, but *effectful* construction (tape push / `add_expr`) is host. Direct
  precedent that 7B's kernel-escape is a category boundary, not a bbnf deficiency.
- **tree-sitter external scanner (C).** grammar.js is data; context-dependent lexing drops to a
  hand C `external scanner` — the exact "grammar-plane data + host escape" shape 7C formalizes
  and 7B must respect. tree-sitter node/tape construction is engine C, not grammar-expressed —
  precedent that recognizer construction stays host even in a maximally declarative generator.
- **rustc / GHC bootstrap.** Both hand-write the AST as ADTs and bootstrap via *staged binary*
  (7A), never an "AST-as-grammar." No production compiler grammar-defines its own IR sum.
- **egg/egglog & MLIR.** Rewrite rules are a data DSL; the *application engine* is host
  Rust/C++. "Mostly data, kernel host" is the validated SOTA shape for exactly this problem —
  7B's thesis is achievable precisely to the boundary these systems draw, and no further.
- **Derive-macro IR projection** (in-tree `egraph-derive`; memory `derive-language-macro`). A
  `Language` impl derived from an existing enum, recursive fields → `Id`. The concrete
  mechanism for §2's narrow-first: `ir.bbnf` as the single source that *generates* the hand
  enum, rather than replacing it — grammar-data definition, real-type consumer.
- **Parser-combinator rollback** (nom `alt`, chumsky). All thread immutable-input+index or an
  explicit `&mut State`; the mutable append tape is the specific effect that resists purity and
  forces the reborrow idiom (§3). Confirms the escape is Rust-borrow-model-intrinsic.

## 6. Named risks & counterexamples

1. **The un-rolled-back tape/arena on speculative failure** (§4) — a latent correctness bug the
   recognizer dodges by grammar accident (disjoint Sheets Alts), which a `bbnf.bbnf` builder
   augment will surface at the `term` alternation. The cure (a transactional tape:
   snapshot-len + truncate-on-fail, or an arena checkpoint) is itself a **host `&mut` facility**,
   confirming tape-assembly is effect-not-data.
2. **`ir.bbnf` as consumer-less substrate** (clause viii breach) if drafted before naming
   consumers (1)-(3) of §2. Risk is high because the enum is small and the temptation to
   "grammar-ify it" for purity is exactly the Y.13 pathology.
3. **The front-end still can't parse `bbnf.bbnf`** — three bounded gaps block routing it through
   the emitter at all: `@{` `}` atom (`bbnf.bbnf:33`; `parse_atom` at `grammar/src/lib.rs:1495`
   handles `"`,`/`,`(` but not `@{`), `|params|` closures (`bbnf.bbnf:51`), and `?w`→`->`
   attachment in `parse_seq`. 7B cannot measure IR-equivalence until these land — the empirical
   question is **blocked on the 7A/7C parser augment**, not independently drivable.
4. **Emitter-as-grammar debuggability tax.** Converting the 257 LOC of `emit_expr` dispatch into
   a template-projection tower trades a readable `match` for grammar indirection; 7B must budget
   against the charter's "build-time and debuggability budgets." A template-DSL bug is harder to
   `cargo expand` than a Rust `match` — measure before committing the dispatch plane.

## 7. What a prototype must test (to move family 7B)

1. **The escape-count falsification.** On a *second* emitter blob (e.g. `css_scan_direct` or
   `json_sink_direct/dispatch.rs`), reproduce the §3 split: template-data LOC vs the reborrow/
   rollback escape sites. If the escape is <5% of substance on both, the "95% data" claim
   generalizes; if a blob shows a *second* irreducible kernel (e.g. first-set analysis,
   `regex_first::first_bytes` at `emit.rs:366` — a real LOCK-5 analysis, not a template), name
   it as a second host primitive.
2. **The tape-rollback counterexample.** Author a minimal grammar with an Alt whose earlier
   branch pushes a tape offset (a leaf) then fails on a later required element; run the general-
   descent recognizer; assert the finished tape carries **no stale offset**. Expected: it DOES
   today (latent bug) — that failing assertion IS the borrow-checker escape made concrete.
3. **The builder-augment round-trip (the gated headline).** Once the parser augment lands
   (`@{}`/`|params|`/`?w`→`->`), add a `DirectBuild`-for-grammar-AST emit mode, drive the
   `!has_rule("value")` branch on `bbnf.bbnf`, parse the corpus, and assert the produced
   `GrammarIr ≡ hand-parser GrammarIr` (modulo `Empty ≡ Concat([])`) within the `add_expr` +
   `resolve_refs` host budget. Report the first construct where it cascades. **Never touch
   `json_sink_direct`.** Do NOT seed the verdict from the legacy emitter's existence.
4. **`ir.bbnf` derive round-trip.** Prototype the narrow-first §2: `ir.bbnf` → generated
   `ExprKind` enum; assert the generated enum is field-identical to the hand `ir/src/lib.rs:234`
   enum, and that `resolve_refs`/`validate` compile unchanged against it. Proves consumers
   (1)-(3) before the grammar-IR lands.

## 8. Integration seams (path:line, where extant code serves)

- Shape-dispatch to the general emitter: `codegen/src/runtime_generator.rs:98`
  (`!has_rule("value")` → `emit_general_descent`).
- The emitter blob (measured): `codegen/src/general_descent/emit.rs` — `render:12`,
  `emit_expr:288`, `emit_alt:400`, `emit_expr_probe:438` (the escape).
- The lazy view + projection classifier: `codegen/src/general_descent/view.rs` —
  `classify_projection:35`, `build_typed_accessors:118`, `render_view:255`.
- The IR sum + mutation kernels: `ir/src/lib.rs` — `add_expr:51`, `resolve_refs:84`,
  `validate:110`, `ExprKind:234`, `BackendExpr:376`, `BackendShape:361`.
- Construction driver: `grammar/src/lib.rs:33` (`resolve_refs` call), `parse_projection:1481`,
  `parse_postfix:1404`, `parse_atom:1495` (the `@{}`/closure gaps).
- The IR→sink fold (the `ir.bbnf` consumer if 3C lands):
  `codegen/src/lower/sink_only.rs:133 lower_expr`, `:217-233` (`BackendExpr→SinkOnlyExpr`),
  `SinkOnlyExpr:69`, `DirectBuild:93`.
- The regen host to extend for `bbnf.bbnf`: `xtask/src/regen.rs:19 write_targets`,
  `main.rs:39-52` dispatch (no `regen-bbnf` today — net-new).
