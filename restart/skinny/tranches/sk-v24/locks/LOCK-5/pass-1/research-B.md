# LOCK-5 · Family 5B — LAWFUL-FUNCTOR (the regex continuum as two domain-fit HIRs bridged by a shared e-graph)

## Thesis

Two HIRs stay distinct — parse-that's regex `Hir` (a byte/class/look owning tree, arena-free,
`Eq + Hash`) and bbnf's grammar `ExprKind` (arena-indexed `ExprId`, span- and projection-carrying).
Neither collapses into the other (that is 5A's move, and it forces regex-only constructors —
character classes, look-assertions — up into the grammar surface where they do not belong). Instead
each surface type carries a **total functor into one shared e-node `Language`**, and the round-trip
oracle over that shared vertex is the lawfulness proof. Isomorphism is real precisely at the
e-graph layer — the shared rewrite library — and stops there; each surface stays domain-fit. This
is constitution clause ii (true isomorphism) realized as an isomorphism *at the e-node vertex*, and
clause iii ("to a reasonable point") given a precise boundary: the reasonable point is the shared
sub-signature (`Concat/Alt/Repeat/Literal/Empty`); genuinely regex-only constructors are shared
only as *atomic* e-nodes.

The decisive fact: **parse-that already ships this exact pattern internally, one repo over.**
`rust/regex/src/egraph/node.rs:41` defines `HirENode` (`#[derive(Language)]`, children projected to
`egraph::Id`) as the e-node twin of the owning `Hir` (`rust/regex/src/hir/mod.rs:218`), and
`rust/regex/src/egraph/translate.rs:19` `insert_hir` / `:63` `extract_hir` are the two total
translators. The node.rs docstring (`:1-40`) makes 5B's own argument verbatim — "these two shapes
are structurally incompatible… every published e-graph library (egg, egglog, Cranelift ægraphs)
draws this same owning-type / e-node-type distinction — it is load-bearing, not incidental." 5B is
the **generalization of that one-repo pattern across the repo boundary**: add a second surface
(`bbnf` grammar-IR regex fragment) that inserts into and extracts from the *same* `HirENode`
Language, and the two functors compose.

```
   parse-that Hir  ──insert_hir──▶  HirENode e-graph  ◀──insert_bbnf──  bbnf regex-fragment (ExprId)
   (owning tree)   ◀─extract_hir──  (SHARED rewrite   ──extract_bbnf─▶  (arena, span-carrying)
                                      library, egraph crate)
     F  =  extract_bbnf ∘ insert_hir          G  =  extract_hir ∘ insert_bbnf
```

`insert_hir` / `extract_hir` already exist; 5B adds `insert_bbnf` / `extract_bbnf` (mirror
translate.rs) and the composite oracle. The functor factors through the e-graph, so lawfulness is
inherited from congruence closure, not asserted.

## The substrate (verified extant code)

**The shared vertex is already cross-repo.** `crates/egraph` and `crates/egraph-derive` are
patch-depended by *both* repos (`parse-that/rust/regex/Cargo.toml:35-37` → the same in-tree
`bbnf-lang/crates/egraph{,-derive}`, per R3 §1's patch table). The Language trait
(`crates/egraph/src/language.rs:16`: `children()`, `children_mut()`, `matches()`), the derive
(`crates/egraph-derive/src/lib.rs:216` `FieldKind::{Leaf,SingleId,SliceId,ArrayId}` — classifies a
struct field as leaf-data vs `Id` vs `Box<[Id]>`/`Vec<Id>`/`[Id;N]`), the `Analysis<N:Language>`
lattice (`crates/egraph/src/analysis.rs:18`: `type Data`, `make`, `merge`→bool changed-flag), the
extractor (`extract.rs`), the CSP scheduler (`csp_scheduler.rs`) and cost config
(`cost_config.rs`/`cost_weights.rs`) are all one home, one dependency. The e-graph rewrite library
IS the shared part the charter names — nothing new to build for it.

**The regex saturation rules** (the "rewrite library" content) live at
`parse-that/rust/regex/src/egraph/rules/{flatten,redundant,repetition,superset,union}.rs` (1253 LOC
total per R3 §3) plus `translate.rs:132`, `cost.rs:102`, `saturation_cache.rs:68` (keyed on the
`Eq+Hash` `Hir`). These consume `HirENode` exclusively (translate.rs docstring) — so they run
unchanged whether the source term arrived from a `Hir` or a bbnf fragment. That is the whole point:
one rewrite library, two front doors.

**bbnf side — three regex representations today, none lawful to each other:**
- `ExprKind::Regex { pattern: String }` (`skinny/crates/ir/src/lib.rs:250`) — an **opaque string
  blob** in the grammar IR. Structure invisible to the grammar.
- `BackendExpr::RegexProgram { pattern: String, span_kind }` (`skinny/crates/ir/src/lib.rs:389`) —
  the backend echo, still a string.
- `bbnf_regex::RegexFacts` / `RegexHir { kind: RegexKind }`
  (`skinny/crates/bbnf-regex/src/lib.rs:2,11,16`, 322 LOC) — a coarse **4-variant classify**
  (`Whitespace | QuotedString | Numeric | Unknown`) plus `nullable`, `FirstSet`, `ByteClass`. Its
  `analyze(pattern: &str)` (`:45`) **re-parses the string** every call. It is invoked at
  `ir/lib.rs:331` (`ExprKind::Regex { pattern } => analyze(pattern).nullable`) — so the grammar
  re-lexes each regex leaf from scratch to learn one nullability bit.

This trio is the un-lawful seam 5B abolishes. `ExprKind::Regex`, `BackendExpr::RegexProgram`, and
`RegexFacts` are three lossy shadows of one structure; the functor makes the parse-that `Hir` the
single structural source and re-homes `RegexFacts` as an `Analysis` over the shared e-node (below).

**The classifier consumer is already cross-repo.** `classify_regex` / `RegexClass`
(`parse-that/rust/regex/src/classify.rs:55,157`, the 1148-LOC file the charter flags) "drives
FnDescriptor specialization on the bbnf side" (R3 §3) — it has **zero consumer inside
`parse_that/src`** (grep: 0 hits). So the classifier is *already* a functor image consumed across
the boundary; 5B makes that crossing lawful and structural instead of string-mediated.

## The mechanism, concretely

1. **`insert_bbnf(egraph, gir: &GrammarIr, root: ExprId) -> Id`** — mirror of `insert_hir`. Walk the
   arena (`GrammarIr.expr(id)`, `ir/lib.rs`): `ExprKind::Seq(ids)` → `HirENode::Concat`,
   `Alt{branches}` → `Alternation`, `Repeat{body,min,max}` / `Optional` → `HirENode::Repetition`,
   `Literal{bytes}` → `HirENode::Literal`, and `ExprKind::Regex{pattern}` → parse the pattern to
   `Hir` once and splice via `insert_hir` (the string leaf **dissolves** into structural e-nodes —
   this is where the blob dies). Non-regex constructors (`Ref`, `Project`, `Annotation`, `Discard`)
   are **outside the functor's domain** — the functor is total on the *regex-expressible sublattice*
   only, and must say so (see risks).
2. **`extract_bbnf(egraph, gir: &mut GrammarIr, root: Id) -> ExprId`** — mirror of `extract_hir`
   (`translate.rs:63`), but `materialize` calls `gir.add_expr(kind, span)` instead of building
   `Box<Hir>`. `HirENode::Class` / `HirENode::Look` (no bbnf-surface analog) round-trip to a single
   `ExprKind::Regex{pattern}` leaf carrying the atom's canonical pattern (`Hir::canonical` /
   `RegexClass::canonical_pattern`, `classify.rs:132`) — atomic, verbatim, so round-trip holds.
3. **`RegexFacts` becomes `impl Analysis<HirENode>`** (`analysis.rs:18`). `nullable` and `FirstSet`
   are lattice `Data` computed by `make`/`merge` over e-classes — computed **once** on the shared
   graph, read by both `ir/lib.rs:331`'s nullability and parse-that's `first.rs` /
   `info/classify.rs`. This deletes `analyze(pattern)`'s string re-parse and satisfies Y.13
   (consumer-ful substrate: two named consumers, one on each repo).

The oracle: `parse(p) → Hir → insert_hir → [saturate] → extract_bbnf → insert_bbnf → extract_hir →
Hir'`, assert `Hir ≡ Hir'`. With saturation off: strict structural equality. With the existing
rules on: canonical-equal (same e-class representative under `cost.rs`). Both directions run over the
PT-1 50+ pattern corpus + PROVE-10's six patterns. A stronger oracle asserts **DFA-equivalence**
(compile both `Hir` and `Hir'` via `automata/dfa.rs` and compare minimized DFAs) — semantic, not
syntactic, the honest lawfulness test when saturation legitimately rewrites structure.

## Prior art (cited)

- **egg** (Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha — *POPL 2021*, "egg: Fast and Extensible
  Equality Saturation", `arxiv.org/abs/2004.03082`): the owning-term / e-node split, the `Language`
  trait, `define_language!`. `crates/egraph-derive` is a local reimpl of `define_language!`'s field
  classification; the functor factoring through congruence closure is egg's core guarantee.
- **egglog** (Zhang, Wang, Flatt, Willsey, Tatlock — *PLDI 2023*, `arxiv.org/abs/2304.04332`):
  relational e-matching; if 5B's lawfulness is stated as a congruence obligation, egglog's
  datalog-over-e-graph is the formal frame for "the functor preserves the shared signature."
- **Cranelift ægraphs / aegraph** (Chris Fallin, 2023, `github.com/bytecodealliance/rfcs` "aegraph"):
  the acyclic e-graph + single-pass rewrite discipline — the model for *bounded* saturation on the
  generate path (no unbounded fixpoint at codegen time). node.rs already cites it.
- **MLIR dialect conversion** (`TypeConverter` / `ConversionPattern`, 1:N type conversion,
  `mlir.llvm.org/docs/DialectConversion`): the canonical "two IRs, lawful total lowering between
  them, legality-gated" framework. Its `TypeConverter` with source-materialization is *exactly* the
  metadata-side-channel answer to the span/projection-loss problem below.
- **Nanopass** (Sarkar, Waddell, Dybvig — *ICFP 2004*, "A Nanopass Infrastructure for Compiler
  Education"): many small IRs with near-total, checked passes between them; the "round-trip oracle
  per level" discipline the charter demands is nanopass's language-definition + pass-verification.
- **regex-syntax `Hir` / regex-automata** (BurntSushi): the reference owning HIR (`Hir`+`HirKind`,
  `translate`); parse-that's `Hir` is a leaner isomorph, and `Hir → NFA → DFA` (R7 §4) is the
  classical pipeline the shared e-node feeds after extraction — and the DFA-equivalence oracle's
  compiler.
- **Galois connection / reflective subcategory** (Lawvere; adjoint functors): the honest statement
  of "isomorphism to a reasonable point" is **not** a strict iso but a *reflection* — `F ⊣ G` with
  `F∘G = id` on canonical forms and `G∘F ≥ id` (may introduce structure the e-graph then
  normalizes). The e-graph's canonical extractor is the reflector. State the law this way, not as
  bijection.

## Named risks & counterexamples

- **Totality is over the sublattice, not all of `ExprKind`.** `Ref`, `Project{target,ty}`,
  `Annotation`, `Discard` have no regex image. The functor is total on the *regex-expressible
  subset*; calling it "total each way" without naming the domain is a lie. Mitigation: a
  `RegexFragment` newtype certifying membership (the subset is decidable by a single arena walk),
  functor typed `RegexFragment → HirENode`.
- **Class/Look force a choice, and the wrong one is 5A.** bbnf's surface cannot express `[a-z]` or
  `\b` except inside a `Regex{pattern}` string. Lifting them to new `ExprKind` variants IS 5A
  (ONE-HIR) and defeats 5B's two-types thesis. 5B's lawful move: keep them as **atomic** `Regex`
  leaves — only the shared constructors (`Concat/Alt/Repeat/Literal/Empty`) cross structurally.
  Round-trip survives because `HirENode::Class`/`::Look` round-trip verbatim (node.rs `:24-27`), but
  the *win* (shared optimization) reaches only the shared sub-signature. Counterexample to guard:
  `[a-z]+` — the `Repetition` is shared, the `Class` is an opaque atom; if a rule tries to rewrite
  *inside* the atom cross-repo, it must be rejected as out-of-signature.
- **Span/projection loss on the bbnf side.** `HirENode` has no `SourceSpan`; bbnf `Expr` carries one
  (`ir/lib.rs`), plus `Project{target,ty}`. `F∘G` erases spans unless the functor threads an
  `Id → (span, projection)` side-table (MLIR's source-materialization). So the functor is lawful on
  **structure**, and either (a) carries a metadata side-channel, or (b) is honestly a structural
  functor with parity only up to span-erasure. This is the true edge of "reasonable point" — name it,
  measure it, don't paper over it.
- **`RegexFacts` is a third, lossy shadow (RETIRE or re-home).** Its 4-variant `RegexKind` is a
  classify, not a functor image. If it survives as a plain struct it re-introduces the un-lawful
  seam. It must become `Analysis<HirENode>` or be deleted — no middle "keep the string analyzer too."
- **Two `Hir` codebases could drift.** parse-that's `Hir` (`rust/regex`, read-only, the real engine)
  vs `skinny/crates/bbnf-regex`'s `RegexHir` (a stub). 5B must pick parse-that's `Hir` as the one
  structural source and make the bbnf side consume it via the functor — not maintain a parallel HIR.
  Cross-repo `Eq+Hash` on `HirENode` is the drift alarm (differential test).
- **Saturation on the generate path costs time.** Factoring every regex leaf through insert →
  saturate → extract adds work vs the current `analyze(pattern)` string re-parse. Must be measured
  **neutral-or-better on the generate axis** (runtime is zero — type/analysis plane, clause v). The
  `saturation_cache.rs` (keyed on `Eq+Hash Hir`) is the amortizer.

## What a prototype must test (to move this family)

1. **The round-trip oracle passes.** Implement `insert_bbnf`/`extract_bbnf` (≈120 LOC, mirror
   `translate.rs`), compose `F`/`G`, and assert `Hir ≡ Hir'` over PT-1's 50+ patterns + PROVE-10's 6:
   (a) saturation-off → strict structural equality; (b) saturation-on (existing rules) →
   canonical-equal, and (c) DFA-equivalence of the compiled minimized DFAs. A single failing
   pattern under (a) refutes strict lawfulness and forces the reflection framing.
2. **Nullability/first-set parity.** Route `ir/lib.rs:331`'s `nullable` through the new
   `Analysis<HirENode>` and assert bit-identical vs `analyze(pattern).nullable` over the whole
   grammar corpus. Any divergence names a bug in the coarse classifier or the functor.
3. **Span/projection survival fraction.** Quantify what bbnf `Expr` metadata survives `F∘G`; decide
   side-table vs erasure by the measured loss, not by assertion.
4. **Generate-axis cost.** Cold, paired: functor-through-e-graph vs current string re-parse, per
   regex leaf, over the corpus, with `saturation_cache` warm and cold. Gate: neutral-or-better;
   a regression names the amortization gap.
5. **Sublattice-membership decidability.** Confirm the `RegexFragment` subset is a single-walk
   predicate over the arena (no fixpoint), and that `Ref`/`Project`/`Discard` are cleanly excluded —
   the honest domain of the "total" functor.

Passing (1)+(2) with (4) neutral converts 5B from claim to mechanism; (3) fixes the exact
"reasonable point" numerically; (5) makes "total each way" a true statement over a named domain.
