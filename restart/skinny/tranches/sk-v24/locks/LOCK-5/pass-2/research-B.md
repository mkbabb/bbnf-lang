# LOCK-5 pass-2 — Family 5B: THE LAWFUL FUNCTOR

Two HIRs, one total functor each way, round-trip oracles, domain-fit representations
preserved; the e-graph rewrite library is the shared part. This report proves the functor
is not a greenfield proposal — a *degenerate, lossy* form of it already runs in production —
and specifies the lawful replacement, its coproduct variant, the rule-death it earns, and
the oracle that certifies it.

## Tree of record (binding on every number below)

Formulation B's substrate exists ONLY in the top-level `crates/ir` tree (the tree holding the
434-LOC grammar-tier regex e-graph) joined with parse-that's `bbnf-regex`. The skinny
`crates/ir` carries neither an e-graph nor a rich regex leaf — `ExprKind::Regex{pattern:String}`
(`skinny/crates/ir/src/lib.rs:250`) is a hand-scanned opaque string (`skinny/crates/grammar/src/lib.rs:1561` `parse_regex`). Every measurement in this report is a top-level-tree measurement; the
reconciled tree of record MUST be the e-graph-bearing tree or Formulation B has no home. Numbers
here are structural counts read from HEAD source; throughput/saturation numbers are named as
prototype obligations, un-run.

## The functor already exists — degenerate and stringly-typed

The two HIRs are already two e-node types, both auto-derive `egraph_derive::Language`
(`trait Language: Clone+Eq+Hash`, `crates/egraph/src/language.rs:16`), both run on the one
`egraph` crate, both are costed by `GrammarCostModel` (`crates/ir/src/egraph/mod.rs:15` names it
"the shared cost model, also consumed by the bbnf-regex HIR e-graph in Tranche H"):

- **Grammar side** — `GrammarENode` (`crates/ir/src/egraph/node.rs:24`). Regex leaf is
  `Regex(StringId)` (`:27`): **OPAQUE**. No structure, no span, no projection.
- **Regex side** — `HirENode` (`parse-that/rust/regex/src/egraph/node.rs`), mirroring `Hir`
  variant-for-variant with `Class(CharClass)`, `Look(Look)`, `Repetition{sub,min,max,greedy}`,
  `Group`, `Concat`, `Alternation`. Its doc comment already states the owning-tree
  (`Box<Hir>`) vs e-node (`Id`) distinction as load-bearing, citing egg/egglog/Cranelift
  ægraph precedent — the two-representation split my charter defends.

The bridge between them today is **the string**. When the grammar-tier e-graph needs regex
algebra it calls `bbnf_regex::algebra::{pattern_is_superset, try_union_patterns}`
(`parse-that/rust/regex/src/algebra/mod.rs:81,95`), and those **re-parse the pattern to `Hir`
on every call**: `String → parser::parse_with → hir_to_byteset → ByteSet → byteset_to_pattern
→ String` (`algebra/mod.rs:19,95`). `FuseAltRegexBranches` fuses `Alt([Regex,Lit,Regex])` into
one regex by **`parts.join("|")`** string concatenation (`crates/ir/src/egraph/rules/regex.rs:300`),
hand-rolling `regex_escape_literal` (`:318`) and `pattern_has_top_level_pipe` (`:332`) — string
reimplementations of what `Hir` structure gives for free. This IS the functor `F/G`, written in
its worst possible form: `String` as the interchange type, `parse`/`print` as the (lossy,
per-call, un-cached) natural transformations.

Family 5B's thesis: replace the `String` interchange with a **lawful pair of total functors over
the two e-node types**, share the rewrite engine + cost model + byte-set algebra, keep each
side's domain-fit representation. The change is not "add a bridge" — it is "make the bridge
already present lawful, structural, and cached."

## The functor, concretely

- `F : HirENode → GrammarENode` (the free embedding / left adjoint). `Class → char-class leaf`,
  `Concat → Seq`, `Alternation → Alt`, `Repetition → Repeat{lo,hi}`, `Literal(bytes) → Literal`.
  Total: every regex fragment has a grammar image.
- `G : GrammarENode ⇀ HirENode` (the reflection / partial right adjoint). Total ONLY on the
  regex-expressible sublanguage `{Literal, Regex, Seq, Alt, Repeat}`; **undefined** on `Ref`
  (recursion — no finite-automaton image), `Map`/`->` (projection), `TokenDispatch`, `Minus`,
  `Negate`. The adjunction `F ⊣ G` is precisely the statement *regex is a coreflective
  subcategory of grammar*: `G∘F = id` on regex, `F∘G ⊑ id` on the grammar sublanguage where `G`
  is defined.

The model implementation already exists on the regex side: `insert_hir`/`extract_hir`
(`parse-that/rust/regex/src/egraph/translate.rs:22`) are the `Hir ↔ HirENode` translators — the
exact template for the directive's `insert_bbnf`/`extract_bbnf`. Family 5B lifts these to cross
the domain boundary rather than stay within the regex e-graph.

## The joint coproduct variant (MLIR-dialect shape)

The directive's third route: `enum JointENode { Left(HirENode), Right(GrammarENode) }` over ONE
union-find arena, with a bridge rewrite `Right(Regex(sid)) ≡ Left(insert_hir(parse(sid)))`
materialized as an **`unrealized_conversion_cast`-style node folded pre-emission** — exactly
MLIR's dialect-conversion discipline, where cross-dialect materialization casts are inserted
during conversion and must be legalized away before lowering. One saturation then runs the
regex-side rules (`bbnf-regex::egraph::rules` — `superset.rs` 90, `union.rs` 108, `flatten.rs`
107, `repetition.rs` 156, `redundant.rs` 68) AND the grammar rules together; the regex algebra
is written once and reaches grammar Alts through the bridge. This is MLIR's coproduct-of-dialects
made literal (multiple IRs joined by conversion patterns). Whether it is WORTH the doubled match
surface is the leaf-richness question below.

## Rule-death count (the directive's headline evidence, in the named tree)

The grammar-tier regex e-graph is `crates/ir/src/egraph/rules/regex.rs` = **434 LOC, 4 rules +
2 string helpers**. Under the lawful functor:

| Rule (regex.rs) | Fate | Why |
|---|---|---|
| `DeduplicateAltBranches` (:49) | **SURVIVES** | pure e-class identity; grammar-general, not regex-specific |
| `SupersetAbsorbAlt` (:109) | **DIES** (~80 LOC) | duplicate of regex-side `superset.rs` (90 LOC); exists only because the grammar tier can't reach into `Hir` |
| `UnionMergeAlt` (:194/362) | **DIES** (~90 LOC) | duplicate of regex-side `union.rs` (108 LOC) |
| `FuseAltRegexBranches` (:229) | **REPLACED** (~85 LOC → ~structural) | `parts.join("|")` string fusion → structural `HirENode::Alternation` build + regex-side rules + extract-back |
| `regex_escape_literal` (:318), `pattern_has_top_level_pipe` (:332) | **DIE** (~45 LOC) | string reimplementations of `Hir` structure |

Plus the string wrappers on the regex side die: `pattern_is_superset`, `try_union_patterns`,
`byteset_to_pattern`, and the per-call `extract_char_class_bytes` re-parse round-trip
(`algebra/mod.rs:19,26,81,95`) — replaced by the structural functor operating on shared
`HirENode`. **Structural estimate: ~215 of 434 grammar-tier LOC + ~4 string wrappers deleted,
1 rule (Dedup) retained, 1 (Fuse) re-expressed.** This is the number the prototype must MEASURE
(LOC before/after) — the structural argument is strong but the exact count is prototype-earned.

## Span & projection survival (the round-trip oracle target)

Today the bridge is **already span- and projection-lossy on regex leaves**: `IrNode::Regex(StringId)`
(`crates/ir/src/types/node.rs:36`) carries no span (leaves are span-free, `:142`) and no
projection; write-back is `GrammarENode::Regex(sid) => IrNode::Regex(sid)`
(`crates/ir/src/egraph/write_back.rs:209`). `HirENode` leaves carry only structural data — no
span field. So span cannot live INSIDE the shared HIR without leaking a grammar concern into the
regex e-node (the same annotation-vs-leaf tension Formulation A hits). Family 5B's answer, and its
oracle: the round-trip law is **DFA-equivalence, not syntactic identity** (strict off, canonical
on — `parse("a|b|c")` may extract as `[abc]`), and span/projection ride **alongside** the HIR as
a side-annotation keyed by the bridge cast node, never inside `HirENode`. The oracle is:
`extract_bbnf(insert_bbnf(hir))` DFA-equivalent to `hir` over the corpus, AND the side-annotation
(span, `->` projection) recovered bit-identical.

## Named risks & counterexamples (family-specific)

1. **Projection-dropping union is a latent SOUNDNESS bug the functor must NOT inherit.**
   `UnionMergeAlt` today merges `[a-c] | [d-f] → [a-f]` by byte-set (`regex.rs:387`). Under LOCK-1
   `->` typing, `[a-c] -> A | [d-f] -> B` would merge to `[a-f]` and **drop the distinct
   projections A,B** — a wrong answer. Counterexample the oracle must build: fusion/merge is sound
   only when projections are equal. The functor makes this checkable (projection is an explicit
   side-annotation); the string bridge cannot see it.
2. **`G` is partial — fusion must be bounded to the regex-expressible sublanguage.**
   Counterexample: `Alt([Regex([a-z]), Ref(ident)])` — `G` is undefined on the `Ref` branch;
   fusion must skip, never crash, never fuse across the non-regex branch. Soundness test required.
3. **Round-trip as syntactic identity spuriously fails.** `insert∘extract` canonicalizes;
   a naive `==` oracle rejects lawful rewrites. Law MUST be DFA-equivalence.
4. **Coproduct arena cost may exceed the 215 LOC it deletes.** A `JointENode` doubles the e-node
   match surface; every grammar rule matches past `Right(...)`. If the fleet's regex leaves are
   mostly bare char-classes (leaf-richness census below), the two-e-graph + functor-helper variant
   (keep both e-graphs, replace only the string wrappers with structural functors) wins over the
   full coproduct. The prototype must price both.

## Leaf-richness census (does the fleet carry a rich regex leaf?)

On the grammar side, **NO**: `GrammarENode::Regex(StringId)` / `IrNode::Regex(StringId)` are
opaque strings by design — the grammar pipeline deliberately does not walk regex internals. The
richness lives entirely on the regex side (`HirENode`/`Hir`). This is the CORE argument FOR the
functor and AGAINST Formulation A's merge-into-`ExprKind`: the two representations are
domain-fit — grammar wants regex opaque, the engine wants the rich `Hir` — and the functor is the
lawful bridge that lets the grammar tier borrow the regex algebra without absorbing the rich leaf.
The prototype must count, across the engine corpus, what fraction of regex sites are non-trivial
(alternation/concat/repetition vs bare char-class); that fraction decides coproduct vs
functor-helper.

## Prior art (SOTA anchors, with citations)

- **MLIR dialect conversion + `unrealized_conversion_cast`** — the canonical multi-IR-joined-by-
  conversion-patterns design; materialization casts inserted then legalized away pre-lowering.
  Direct model for the coproduct bridge cast. (MLIR "Dialect Conversion" framework.)
- **"Trees That Grow"** (Najd & Peyton Jones, IFL 2016 / GHC Core) — one AST parameterized so each
  phase carries domain-fit decorations; the strongest prior-art anchor for TWO domain-fit HIRs
  over one god-type.
- **Bidirectional transformations / lenses** (Foster, Greenwald, Moore, Pierce, Schmitt, POPL
  2005) — `GetPut`/`PutGet` round-trip laws ARE the functor round-trip oracle law.
- **Category theory: adjoint functors `F ⊣ G`, coreflective subcategory** — the exact algebra of
  "regex ⊂ grammar" with `G∘F=id`, `F∘G⊑id`.
- **egg / egglog** (Willsey et al., POPL 2021; Zhang et al., PLDI 2023) — the `Language` trait and
  equality saturation; egglog's multi-sort reasoning is the coproduct's saturation precedent.
- **Cranelift ægraphs** (acyclic e-graphs) — cited in-tree in `HirENode`'s doc; the owning-tree /
  e-node split as standard practice.
- **regex-automata / regex-syntax `Hir`** — the `Hir`→NFA→DFA lineage `bbnf-regex` re-implements
  bespoke; the functor keeps that engine untouched.

## Integration seams (path:line)

- `crates/ir/src/egraph/node.rs:27` — `GrammarENode::Regex(StringId)`, the bridge's grammar end.
- `crates/ir/src/egraph/rules/regex.rs:229` — `FuseAltRegexBranches` (string fusion to re-express);
  `:109`,`:362` `SupersetAbsorbAlt`/`UnionMergeAlt` (dying duplicates); `:318`,`:332` string helpers.
- `crates/ir/src/egraph/write_back.rs:209` — `Regex(sid)=>IrNode::Regex(sid)`, span/projection-free.
- `crates/ir/src/types/node.rs:36` — `IrNode::Regex(StringId)`, no span/projection on the leaf.
- `crates/ir/src/egraph/mod.rs:15,96` — `GrammarCostModel` + `default_rules` (the shared engine).
- `crates/egraph/src/language.rs:16` — `trait Language: Clone+Eq+Hash` (JointENode satisfies it).
- `parse-that/rust/regex/src/egraph/node.rs` — `HirENode` (the functor's regex end).
- `parse-that/rust/regex/src/egraph/translate.rs:22` — `insert_hir`/`extract_hir` (model for
  `insert_bbnf`/`extract_bbnf`).
- `parse-that/rust/regex/src/algebra/mod.rs:81,95,19` — the re-parsing string functor to delete.

## What the prototype MUST test to move family 5B

1. **`insert_bbnf`/`extract_bbnf` + round-trip oracle** over the corpus: DFA-equivalence (strict
   off, canonical on), modeled on `insert_hir`/`extract_hir`; report span + projection (side-
   annotation) survival across `F∘G`. State generate-axis cost vs `analyze()`.
2. **`FuseAltRegexBranches` re-expressed structurally** (build `HirENode::Alternation`, run
   regex-side `union`/`flatten`/`superset`, extract back); assert byte-identical fused DFA vs the
   current `parts.join("|")`; MEASURE grammar-tier LOC deleted (the rule-death count).
3. **Projection-preservation soundness:** a test that FAILS on the current string bridge
   (`[a-c]->A | [d-f]->B` merges and drops A/B) and PASSES under the functor (merge blocked when
   projections differ).
4. **Partiality soundness:** `Alt([Regex, Ref])` — `G` returns None on `Ref`, fusion skipped, no
   crash, no cross-branch fusion.
5. **Coproduct vs functor-helper cost:** leaf-richness census first (non-trivial regex-site
   fraction across the engine corpus), then price full `JointENode` coproduct saturation against
   two-e-graph + structural functor-helper; report which the ~215-LOC deletion justifies.

## Cross-lock (shareable)

- The type-plane choice (LOCK-1) decides whether the regex leaf's `->` projection is a side-
  annotation on the bridge cast (family 5B's position) or a field — this gates the oracle's
  projection-survival test.
- The `GrammarCostModel` shared home (`crates/ir/src/egraph/cost.rs`) is the charter's "shared
  part"; it already spans both e-graphs — the functor formalizes, not invents, that sharing.
- The prefilter/tier CSP (double-homed with LOCK-2) sits DOWNSTREAM of extraction and is
  orthogonal to the functor; family 5B touches only the HIR-canon island, not tier selection.
