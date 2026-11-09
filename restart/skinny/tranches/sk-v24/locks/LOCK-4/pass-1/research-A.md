# LOCK-4 · Family 4A — CLOSURE-TO-GRAPH · research (pass 1)

*The combinator API materializes a first-class, analyzable parser graph — a flat node arena with
integer ids — instead of eagerly nesting opaque closures. Fusion, packrat placement, and
diagnostic points become graph passes over that arena. The graph shape is a shared artifact across
Rust and TS (a node array, isomorphic to the tape); only extraction to executable form is
idiomatic per language.*

---

## 1. The thesis, and the defect it dissolves

Today parse-that's combinator is an opaque closure. Rust: `Parser<'a,Output>` wraps
`parser_fn: SmallBox<dyn ParserFn<'a,Output>+'a, S32>` (`rust/parse_that/src/parse.rs:61-64`),
built by `Parser::new` = `SmallBox::new(closure)` per combinator (`:71-77`). TS: `Parser<T>` holds
`parser: ParserFunction<T>`, a function pointer, built once per `new Parser(...)`
(`typescript/src/parse/parser.ts:24-32`). A nested `a.then(b).or(c)` is a tree of closures each of
which can only be *called*, never *inspected*. Every optimization parse-that ships is therefore a
**hand-written escape from that opacity**:

- **Fusion is hand-unrolled at fixed arities.** `seq!`/`alt!` "create a single `Box<dyn ParserFn>`
  instead of N-1 intermediate boxes" — but only for arities 2..8, spelled out longhand
  (`rust/parse_that/src/combinators/macros.rs:6-302`, each arm a copy). TS mirrors this with
  `fuseAll` arity-2/arity-3 unrolled arms and a general `for` arm (`leaf.ts:179-273`) and arity-2
  unrolled `any` (`leaf.ts:39-57`). Beyond arity 8 (Rust) or 3 (TS) you fall off the fused path.
- **First-set dispatch is hand-authored.** `dispatch()` takes a `Record<string, Parser>` the *user*
  fills in by hand, building an `Int8Array(128)` LUT (`leaf.ts:100-151`). The user must know each
  alternative's first characters and keep the table in sync with the `any(...)` chain it shadows.
- **Packrat is a manual opt-in.** `memoize()` is called by hand; `PACKRAT_ARMED` stays false and
  the tables are a true no-op until the first hand `memoize()` (`typescript/src/parse/packrat.ts:156,290`;
  Rust `lazy.rs`). The user decides *which* nodes memoize.
- **Diagnostics accrete at runtime behind a global.** The "expected" set is gated by the
  module-global `diagnosticsEnabled` and accumulated per-parse via `mergeErrorState` furthest-offset
  bookkeeping (`typescript/src/parse/utils.ts:6,28-49`; Rust `ParseError.expected: Vec<String>`
  populated "from parser context when available", `parse.rs:18-21`). It is structurally cold and
  cross-parse-contaminating.

Each is a point-solution to "the closure is opaque." **4A removes the opacity at the root.** The
combinator methods stop constructing closures and instead *append nodes to a graph*; the four
hand-optimizations above become four *analyses over the graph*, total (all arities, all shapes),
derived instead of maintained. This is the "closure-to-graph" charter verbatim.

## 2. The mechanism, made concrete

### 2.1 The reified node ADT (the substrate)

A parser is a handle into a shared arena:

- **Rust.** `enum Node { Terminal(Term), Seq(NodeRange), Alt(NodeRange), Rep{ child, min, max, sep:
  Option<NodeId> }, Map{ child, f: MapId }, Look{ child, positive }, Not{ child }, Minus{ child, by },
  Bind{ child, k: BindId }, Ref(RuleId) }`. Held in `ParserGraph { nodes: Vec<Node>, terms:
  Vec<Term>, maps: Vec<MapFn>, binds: Vec<BindFn>, rules: Vec<NodeId> }`, `NodeId(u32)` indices,
  `NodeRange` a `(start,len)` slice into a shared child list — a DAG (Ref supplies sharing/recursion,
  making cycles). `Parser<'a,O>` becomes `{ graph: Rc<ParserGraph>|&'a ParserGraph, root: NodeId,
  _pd: PhantomData<O> }`. Crucially the arena is **plain `'static` data** — no `'a` closures inside —
  which is the key to §5's ownership dividend.
- **TS.** the isomorphic tagged union in a flat array: `type Node = { tag:'seq'|'alt'|..., kids:
  number[], ... }`, `ParserGraph { nodes: Node[], terms: Term[], maps: Function[], ... }`,
  `Parser<T> = { graph, root:number }`. Same shape as the offset-tape crossing already proven
  byte-exact node-for-node across the language boundary (the tape-decoder isomorphism), so the graph
  is a shared serializable artifact, not two parallel inventions.

The combinator methods (`then/or/many/sepBy/map/opt/not/minus/peek/lookAhead/wrap` —
`parser.ts:81-707`; Rust `combinators/methods/{sequence,alternation,repetition,sep_by,minus,map,
recover}.rs`) each become a **one-line node-append** returning a new handle over the same graph.
The public fluent API is byte-for-byte unchanged (constitution vi/vii: combinators stay lawful
hand-written primitives — the *surface* is preserved; only its internal materialization changes).

### 2.2 The graph passes (the consumers — Y.13 satisfied in-design)

1. **Fusion pass.** Normalize by the parser-combinator laws: flatten nested `Seq`/`Alt`
   (associativity), fuse `Map∘Map`, drop `Alt` `empty` arms, hoist `pure`. Then *extract one fused
   evaluator per maximal static region* — reproducing what `seq!`/`alt!` do by hand
   (`macros.rs`) but for **arbitrary arity** and **arbitrary nesting**, and what `fuseAll`
   arity-unroll does (`leaf.ts:179-273`) as a derived output. The N-1-boxes-to-1 collapse becomes a
   theorem of the pass, not a set of copy-pasted macro arms.
2. **First-set dispatch pass.** Compute FIRST sets bottom-up over the graph (fixpoint over `Ref`
   SCCs for recursion). Any `Alt` whose arms have disjoint FIRST sets lowers to a `Dispatch` node
   with a derived `Int8Array(128)` / `[u8;256]` LUT — *exactly* the table `dispatch()` asks the user
   to hand-fill (`leaf.ts:100-151`), now synthesized and provably consistent with the `any(...)` it
   replaces. Non-disjoint arms keep ordered-choice PEG semantics (no behavior change).
3. **Packrat-placement pass.** Detect `Ref` back-edges (SCCs = left-recursion / re-entrant nodes)
   and cross-references (a node reachable from ≥2 call sites). Mark *only* those as memo points; the
   WDM/left-recursion runtime machinery (`packrat.ts`) stays, but its arming set is derived, killing
   the manual `memoize()` opt-in and the "which nodes" burden.
4. **Diagnostics pass.** FIRST/FOLLOW sets are the *static* "expected token" set at every `Alt`/
   `Terminal`. This yields diagnostics *from structure*, computed once at graph-build, rather than
   the cold module-global runtime accumulation (`utils.ts:6,28-49`). Runtime furthest-offset stays as
   a complement for the actual failure locus; the static expected-set is the structural expectation.
   Error-recovery sentinels (`recover.rs`, `parser.ts:653-688`) attach to nodes with known FOLLOW.

### 2.3 Execution: extraction to a fused closure tree (the 4A internal choice)

4A's essence is the *analyzable graph as a build-time IR*; the runtime it lowers to is a design
choice internal to the family. The recommended lowering **keeps runtime as closures** but makes them
*products of the optimized graph* rather than hand-nested: after the passes, an **extraction** step
walks the optimized arena once and emits one specialized closure per fused region (Rust: one
`SmallBox`/`Box` per fused rule, not per combinator; TS: one specialized function per region, which
*reduces* the megamorphic `this.parser(state)` IC pollution the current design suffers, R4 §6c, by
collapsing many closure shapes into few). This is the staged-parser discipline: **reify → optimize
→ generate**. The alternative lowering — a graph-walking interpreter / small VM over the arena — is
a compact dispatch loop but shades into a different architectural center (bytecode execution); 4A
takes it only as a *measurement baseline* for extraction, not the product. The graph is the family's
commitment; extraction-to-fused-closures is its lawful, constitution-conformant execution.

### 2.4 The e-graph option (constitution-aligned, in-tree machinery)

The fusion+normalization pass (§2.2.1) is precisely an **equality-saturation domain**: the parser
laws (`pure f <*> pure x = pure (f x)`, `p <|> empty = p`, `<|>`/`<*>` associativity, `map`
fusion, `many p = (p `then` many p) <|> pure []`) are rewrite rules; a cost model (node count,
prefilter-eligibility, dispatch-ability) extracts the representative. This is not speculative
infrastructure: parse-that already path-deps the in-tree `egraph`/`egraph-derive`/`csp-solver`
crates (`egraph -> ../../bbnf-lang/crates/egraph`, verified to exist), and **bbnf-regex already runs
exactly this pattern on its regex HIR** (`rust/regex/src/egraph/`, 1253 LOC: rewrite rules
`repetition/union/flatten/superset/redundant`, `cost.rs`, `saturation_cache.rs`). 4A's fusion can
reuse the *same* engine — physical isomorphism between the combinator optimizer and the regex
optimizer, one e-graph home, no orthogonal subsystem. This is the strongest constitution-aligned
form of the fusion pass; a prototype must decide e-graph-saturation vs a fixed rewrite ordering by
measurement (does saturation find fusions a linear pass misses, at acceptable build cost?).

## 3. Prior art (SOTA, cited)

- **Parsley** — Willis, Wu, Pickering, *Staged Selective Parser Combinators* (ICFP 2020); *Garnishing
  Parsec with Parsley*. The closest prior art: the parser is reified as a first-class AST, a
  law-based optimizer simplifies it, it is compiled to an abstract machine, and staging (Scala LMS /
  Typed Template Haskell partial evaluation) removes the machine to emit specialized code. 4A is
  Parsley's reify→optimize→generate applied to parse-that's Rust+TS surface, with e-graph saturation
  as the optimizer and closure-extraction (not TTH) as the generator.
- **Selective applicative functors** — Mokhov, Lukyanov, Marlow, Dimoulas (ICFP 2019 / JFP 2021).
  The theory of *why* reification analyzes branching: applicative structure is static, selective
  (`branch`/`select`) keeps branch *analyzability* while allowing choice, monadic `bind` is opaque.
  Governs §4.1's static/dynamic split precisely.
- **Swierstra & Duponcheel** — *Deterministic, Error-Correcting Combinator Parsers* (AFP 1996). The
  historical anchor for "diagnostics as graph analysis": combinators carry a `(nullable, FIRST)`
  pair so choice is deterministic and errors are recoverable. 4A generalizes their fixed pair to
  arbitrary graph passes (full FIRST/FOLLOW, dispatch, packrat), not just the empty/first pair.
- **Parsing with Derivatives** — Might, Darais, Spiewak (ICFP 2011, *Functional Pearl*);
  Adams, Hollenbeck, Might (PLDI 2016, complexity/performance). PWD builds a *graph* of
  combinator nodes with laziness and computes nullability by fixpoint over cycles — the exact
  cyclic-graph + SCC-fixpoint machinery 4A needs for `Ref`/left-recursion (§4.4). Difference: PWD
  derives per-token at runtime; 4A optimizes the graph ahead-of-time.
- **Krishnaswami & Yallop** — *A Typed, Algebraic Approach to Parsing* (PLDI 2019). Reified typed
  grammar descriptions with a type system that statically rejects ambiguity and drives an LL(1)
  staged parser — the theoretical backbone for deriving FIRST/FOLLOW and dispatch soundly from the
  graph. **flap** (Yallop et al., PLDI 2023, *A Deterministic Parser with Fused Lexing*) extends the
  reify-then-fuse method to DFA lexer/parser fusion — the shared method with the regex continuum.
- **Jonnalagedda et al.** — *Staged Parser Combinators for Efficient Data Processing* (OOPSLA 2014):
  LMS-staged combinators that eliminate per-combinator overhead by generating specialized code —
  direct evidence that reification+staging beats opaque closures on throughput.
- **Landscape.** chumsky (Rust) is the nearest production combinator library moving toward a more
  analyzable/zero-copy representation; nom/winnow are pure-closure (the design 4A leaves).
  tree-sitter/LALRPOP/pest reify grammar into tables (the table is a graph-derived artifact). GHC
  stream fusion / rewrite RULES (Coutts–Leshchinsky–Stewart, ICFP 2007) and ApplicativeDo are the
  fusion-as-rewrite and reify-to-analyze precedents from the compiler world.

## 4. Named risks & counterexamples

1. **Monadic-bind opacity (the hard boundary).** `chain` (value→parser, `parser.ts:124-144`) and
   `mapState` (`parser.ts:162-187`) are dynamic: the sub-parser is computed at runtime from a value,
   so the graph cannot see past a `Bind` node. Global analyses (FIRST/FOLLOW, cross-region fusion)
   are **bounded by `Bind` boundaries**. *Counterexample:* a grammar built entirely with `chain`
   gets zero fusion/dispatch benefit. *Mitigation & the decisive measurement (§6.1):* parse-that is
   overwhelmingly applicative/alternative — only `chain`/`mapState` are dynamic, and the hot
   consumers use `all`/`any`/`dispatch` (value.js: 59 `all()` sites + a `dispatch` bucket, per R4).
   `Bind` remains a first-class *opaque node* (a reanalysis barrier); per-node passes still fire on
   its statically-known child and on everything around it.
2. **Reification/extraction construction cost.** Building an arena + running passes + extracting is
   *more* work at grammar-construction than directly nesting closures. *Counterexample:* if grammars
   were rebuilt per parse this would dominate. *Reality:* grammars are built **once** (R4 §6e:
   "combinator closures allocate once per `new Parser` at grammar build"; construction on JSON is
   ~6 allocs total), amortized to ≈0 over parses — but value.js/latex-paper build grammars at module
   load, so §6.3 must show module-init time does not regress.
3. **Extraction must not lose to today's hand-tuned closures.** If extraction emits `Box<dyn>` per
   node it loses to the macro-collapsed single box. *The pass must provably fuse to at-least the
   macro shape.* *Counterexample:* naive per-node extraction. *Guard:* §6.2 gates
   derived-fusion ≥ hand-`fuseAll`/`seq!` on the divan corpora.
4. **Cyclic graphs / left-recursion.** `Ref` back-edges make the graph cyclic (replacing `lazy()`,
   `lazy.rs`/`lazy.ts` whose `Box<dyn LazyParserFn>` per-lazy alloc dies). FIRST/nullable need
   SCC-fixpoint (standard; PWD precedent). Real work, not free.
5. **rust/ts isomorphism scope.** The node array + integer ids is trivially isomorphic (proven by
   the tape crossing). What is *mirrored*: the `Node` enum, the pass algorithms, the FIRST/FOLLOW
   tables. What stays *idiomatic*: extraction (Rust monomorphized closures/`SmallBox` vs TS
   V8-shape-stable functions). 4A owns the graph-shape half of the isomorphism seam; it must state
   the boundary as measured, not asserted.
6. **Static-expected vs runtime-furthest diagnostics differ.** A FIRST-set "expected" is the
   *structural* expectation; the runtime furthest-offset is the *actual* failure locus. They are
   complementary, and at a `Bind` boundary the static set is unknown (opacity again). 4A must ship
   both and label which is which — not claim the static set replaces furthest-offset.

## 5. The ownership dividend (why 4A dissolves the arena question, not patches it)

The vtable-box question — `Parser` owns a `SmallBox<dyn ParserFn+'a, S32>` per combinator
(`parse.rs:61`) — has resisted the natural fix: a bump-slab arena over the 86 `Parser::new` sites is
**unsound** because `ParserFn<'a>` is parameterized by the input lifetime `'a`, so a shared/`'static`
arena holding `'a`-closures cannot type. That is a large, lifetime-invasive, high-risk / low-reward
rewrite (it moves only the ~6 construction allocs, never the ~5390 per-parse allocs).

4A **sidesteps it structurally**: the reified `Node` arena is **plain `'static` data with no `'a`
closures inside** — allocating nodes is trivially arena-friendly, `Rc`/`Vec`-owned, no lifetime
threading. `'a` enters only at *extraction* (which builds the executable closures) and at *parse*
(which borrows the input), each already `'a`-scoped by construction. The 86-site lifetime rewrite
that blocks the vtable arena simply does not arise for the node arena. This is a genuine
architectural payoff of reification, distinct from any point-fix.

## 6. What a prototype MUST test to move family 4A

Falsifiable, cheapest-first. Bench law: cold paired divan (Rust; corpora apache-builds/canada/
citm/data/data_xl/twitter), cold-per-op paired `hrtime.bigint` single-build-per-process (TS); no
warm benches; parse-that built via the scratch-copy recipe with an isolated `CARGO_TARGET_DIR`
(original tree read-only).

1. **Static-structure census (decisive, cheap).** Reify value.js, latex-paper, and parse-that's own
   JSON/CSS grammars into the `Node` arena; report the fraction of nodes that are static
   (Seq/Alt/Rep/Map/Terminal/Look) vs dynamic (`Bind`/`mapState`). **If ≥~85% static, the passes
   have broad reach; if not, 4A's analytic ceiling is low — a go/no-go for the whole family.**
2. **Derived fusion ≥ hand fusion.** Build the JSON grammar as a graph, run the fusion pass, extract
   to a closure tree; show cold per-parse ≥ the current hand-`fuseAll`/`seq!` build on twitter+citm,
   paired. Proves derivation matches or beats maintenance. (If it only *ties*, 4A is a
   maintainability + generality win — state it honestly, do not inflate.)
3. **Construction / module-init non-regression.** Grammar-build (module-load) time with
   reify+passes+extract ≤ ~1.2× the eager-closure build; per-parse amortization drives it to free.
   Measure value.js module init directly.
4. **Derived dispatch ≡ hand dispatch.** The FIRST-set pass over a linear `any(...)` chain emits an
   `Int8Array(128)` LUT byte-identical in effect to the hand `dispatch()` table (`leaf.ts:100-151`)
   and matches its throughput.
5. **Derived packrat placement.** On the packrat left-recursion corpus, SCC-placement marks exactly
   the nodes hand-`memoize()` marks, and passes the existing `packrat-{cross-input,reentrant,
   large-offset,armed}` proofs.
6. **Graph-shape isomorphism.** Serialize the `Node` arena and round-trip it Rust↔TS identically
   (the tape-crossing method), proving the graph is one shared artifact.

## 7. Integration seams (path:line)

- Reify the core type: `rust/parse_that/src/parse.rs:61-77` (`Parser` = `SmallBox<dyn ParserFn>` +
  `Parser::new`) → graph handle + node-append. Combinator builders:
  `rust/parse_that/src/combinators/methods/{sequence,alternation,repetition,sep_by,minus,map,
  recover}.rs`; TS `typescript/src/parse/parser.ts:81-707`.
- Fusion pass generalizes the hand macros: `rust/parse_that/src/combinators/macros.rs:6-302`
  (`seq!`/`alt!`, arities 2..8) and TS `fuseAll`/`any` unrolls `leaf.ts:39-57,179-273`.
- Dispatch pass replaces hand table: `typescript/src/parse/leaf.ts:100-151` (`dispatch`, the
  `Int8Array(128)` LUT the user hand-fills); Rust `leaf.rs:182` (`TakeUntilScan::Lut(Box<[bool;256]>)`).
- Packrat-placement pass drives: `typescript/src/parse/packrat.ts:156,290` (`PACKRAT_ARMED`,
  `makeMemoized`); Rust `lazy.rs:21` (`Box<dyn LazyParserFn>` per `lazy()` → `Ref` node).
- Diagnostics pass supplies the "expected" set: `rust/parse_that/src/parse.rs:18-21`
  (`ParseError.expected`); TS `utils.ts:6,28-49` (`diagnosticsEnabled` + `mergeErrorState`).
- E-graph reuse: `rust/regex/src/egraph/` (1253 LOC, the isomorphic optimizer already in-tree);
  path-deps `egraph`/`egraph-derive`/`csp-solver` (`rust/regex/Cargo.toml:35-37`, `.cargo/config.toml`).
- Isomorphism substrate: the offset-tape node-array crossing (byte-exact rust↔ts) is the proven
  shape the `Node` arena inherits.
