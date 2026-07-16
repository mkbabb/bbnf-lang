# LOCK-4 · Family 4A — CLOSURE-TO-GRAPH · pass-1 research

*Combinators construct a static, analyzable parser graph — not opaque nested closures. Fusion,
packrat placement, and diagnostics become graph analyses. The graph shape is isomorphic across
Rust and TypeScript.*

---

## 0. The thesis, sharpened against its siblings

The combinator core today is a **final (closure) encoding**: every `.then`/`.or`/`.many`
allocates a fresh function that captures its children, and the parser's structure is destroyed at
construction — it survives only as the call graph of opaque function pointers
(`SmallBox<dyn ParserFn>` in Rust, `parser: ParserFunction<T>` in TS). Nothing downstream can
read that structure, so every optimization must be hand-installed at the call site as a
special-cased smart constructor.

4A's move is **defunctionalization** (Reynolds, *Definitional Interpreters*, 1972; the
initial-vs-final framing of Kiselyov's tagless-final line): reify the combinator algebra as a
first-class **data graph** of tagged nodes, then make the interesting decisions — fusion, packrat
placement, diagnostic points — into **passes over that graph** rather than construction-time
tricks. Execution is a tree-walking, enum-dispatched interpreter over the (rewritten) graph.

The family boundary that keeps 4A distinct from its siblings — I develop only 4A:
- **vs 4B (COMPTIME-FUSION):** 4B eliminates the runtime graph — monomorphized chains (Rust) or a
  build-time fuser emitting specialized closures (TS). 4A *keeps the graph at runtime* precisely
  because its value is that it stays analyzable, debuggable, and isomorphic. Fusion *rewrites* the
  graph; it does not erase it.
- **vs 4C (VM-CORE):** 4C lowers the graph to bytecode and runs an instruction VM (Parsley's
  endpoint — see §4). 4A's interpreter walks the node tree directly; it never loses the grammar
  structure to a flat instruction stream. (4C is a lawful *continuation* of 4A — the graph is the
  thing 4C would lower — but 4A's center of gravity is the graph and its analyses, not the
  execution engine.)
- **vs 4D (GRAMMAR-SUGAR):** 4D claims the parser graph *is* the bbnf grammar HIR (one type). 4A
  claims only structural kinship — the node set (Seq/Alt/Rep/Ref/…) is the same *family* as bbnf's
  grammar AST, so a lawful functor to bbnf HIR exists (the LOCK-5 continuum), but 4A keeps two
  homes and a boundary, not a merge.

---

## 1. The mechanism made concrete

### 1.1 The node graph (the reified algebra)

A parser is a value of a tagged union — one variant per combinator — carried as a rule map plus an
entry point:

```
Graph = { rules: Map<RuleId, Node>, entry: RuleId }

Node =
  // leaves (structural, no child parser)
  | StringLit(bytes) | Regex(dfa) | AnyOf(aho_corasick)
  | TakeWhile(pred) | TakeUntil(set) | NextN(n) | Epsilon | Eof
  // structural combinators (analyzable children)
  | Seq([Node]) | Alt([Node]) | Many{inner,lo,hi}
  | SepBy{inner,sep,lo,hi} | Opt(inner) | Wrap{l,inner,r}
  | Skip(a,b) | Next(a,b) | Not(a,b) | Minus(a,b)
  | LookAhead(a,b) | Negate(inner) | Peek(inner)
  // the knot-tier (recursion)  — §3.3
  | NonTerminal(RuleId)
  // the semantic seam — opaque-leaf compromise  — §3.1
  | Map{inner, f: OpaqueFn}
  // escape hatch (host combinator that resists reification)
  | Boxed(dyn ParserFn)
```

This is not speculative: **it already exists and ships** as `SpanParser`/`SpanKind` in parse-that
(§2). The 4A design generalizes that enum three ways — a value/projection layer (`Map`), an
explicit recursion node (`NonTerminal`), and the lifting of construction-time cleverness into
named passes.

### 1.2 The analyses (the payoff — cleverness becomes passes)

Each pass is a **total function `Graph → Graph`** (or `Graph → Annotated-Graph`), pure, and — the
constitution-iii dividend — written once *per shape* and mirrored across Rust/TS because the node
union is a plain tagged union.

1. **Flatten/normalize.** `Seq([Seq([a,b]),c]) ⇒ Seq([a,b,c])`; `Alt` likewise; `Opt(Opt x)⇒Opt x`;
   `Wrap` desugars to `Skip(Next(l,inner),r)` or stays fused. Today this is inlined into
   `then_span`/`or` (§2.2) — the pass makes it a separable, testable rewrite with a fixpoint.
2. **Fusion.** The manual smart-constructors become law-driven rewrites over the normalized graph:
   `SepBy{inner,sep}` under a whitespace-trim context ⇒ `SepByWs` (one trim per step, no
   double-trim); a `Not(byte-class)`/`TakeWhile(negated)` leaf ⇒ the `TakeUntilAny{1,2,3,Lut,SIMD}`
   ladder by cardinality of the excluded set; adjacent `Map` ⇒ `Map{f=g∘f}`; `Alt` of string
   literals ⇒ `AnyOf(aho_corasick)`. Every one of these is *already implemented* in parse-that as a
   hand-coded constructor branch (§2) — 4A's contribution is to prove they are a **rewrite system**
   the graph carries, not scattered construction logic.
3. **Dispatch-table synthesis.** `Alt([p0,p1,…])` where each alternative's FIRST set is a computable
   byte/char class and the classes are disjoint ⇒ `Dispatch(lut: [idx;128], parsers)` — an O(1)
   first-char jump instead of sequential trial. This is exactly the hand-written `dispatch()`
   (TS `leaf.ts:100`, an `Int8Array(128)` LUT) — but *derived* from an `Alt` node by a FIRST-set
   analysis, not authored by the grammar writer. FIRST/FOLLOW over a reified grammar is the
   Krishnaswami–Yallop typed-parsing result (§4).
4. **Packrat placement.** A cycle/strongly-connected-component analysis over the `NonTerminal`
   edges identifies the rules that actually need memoization (left/indirect recursion, or
   provably-exponential backtracking); it annotates *only those* `NonTerminal`s as memo points.
   This replaces today's **global arm-on-first-`memoize()`** latch (TS `packrat.ts:290`;
   `PACKRAT_ARMED` at `:156`) with per-node placement — Warth's left-recursion seeding (§4) becomes
   a graph annotation keyed exactly where `packrat.ts:71` already keys (`id*OFFSET+offset`), the id
   now being the `RuleId`.
5. **Diagnostics annotation.** A pass walks the graph and attaches `expected`/label metadata at the
   nodes that are diagnostic-worthy (leaves and named rules), so the furthest-error machinery has a
   grammar-sourced name at every failure. This subsumes the `diagnostics`-feature `label` field on
   `SpanParser` (§2) and the TS lazy-diagnostics work (see §5) — labels become a derived property of
   graph position, not a per-combinator hand-set string.

### 1.3 Execution

The (rewritten, annotated) graph is walked by an `#[inline(always)]` `match` on `&node`
(`SpanParser::call_inner`, §2.1) — vtable-free, no per-combinator heap box on the steady path.
Flat `Seq([Node])`/`Alt([Node])` (a `Vec`, not a right-nested cons of boxes) means a chain of N
combinators is one loop over N nodes, not N nested closure calls — this is the property that makes
the interpreter competitive with a monomorphized closure chain (§6 names the counterexample where
it is not).

---

## 2. The substrate — it is already built (and in production)

parse-that ships a **working reified-graph tier**: `rust/parse_that/src/span_parser/` (1,437 LOC,
10 files). This is the 4A seed, three-quarters grown. What exists, precisely:

### 2.1 The node union + enum dispatch
`span_parser/mod.rs:57` `struct SpanParser{ kind: SpanKind, flags: u8, [cfg(diagnostics)] label }`.
`SpanKind` (`mod.rs:63–133`) enumerates every node 4A needs: leaves `StringLiteral`, `CompiledDfa`,
`AhoCorasickMatch`, `TakeWhileByte/Char`, `NextN`, `Epsilon`, `Eof`; the negated-class fast-path
ladder `TakeUntilAny1/2/3`, `TakeUntilAnyLut(Box<[bool;256]>)`, `TakeUntilAnySIMD{lo_lut,hi_lut}`;
flat combinators `Seq(Vec)`, `OneOf(Vec)`, `Many`, `SepBy`, `SepByWs`, `Opt`, `Wrap`, `Skip`,
`Next`, `Not`, `Minus`, `LookAhead`, `Negate`, `Peek`; a monolithic `Scanner(SpanScanner)`; and the
escape hatch `Boxed(dyn ParserFn)`. Dispatch is `call_inner` (`mod.rs:186`) — a single
`match &self.kind` with `#[inline(always)]`, plus a `flags`-branch fast path (`call` `mod.rs:139`,
cold `call_with_flags_cold` `mod.rs:154`) exactly mirroring the primary `Parser::call`
(`parse.rs:80`).

### 2.2 Construction-time analyses (the proto-passes, currently inlined)
`span_parser/methods.rs:16` `then_span` **already flattens** nested `Seq` (`SpanKind::Seq(v) if
flags==0 => reuse v, extend/push`); `methods.rs:29` `or` flattens `OneOf` identically. The fusion
ladder is baked into the constructors: `SepByWs` is a hand-fused sep_by+trim node (`mod.rs:104`
doc: "single trim between each step, no redundant double-trims"); `constructors.rs` selects the
`TakeUntilAny*` variant by excluded-set cardinality. **These are the passes of §1.2 — but
implemented as construction branches, not as a separable rewrite over the graph.** Lifting them out
is the core of the 4A prototype (§7).

### 2.3 It has real consumers (Y.13 satisfied by construction)
The graph tier is not a toy: `parsers/json.rs`, `parsers/css/declaration.rs`,
`parsers/css/selector.rs`, `parsers/css/mod.rs` build their parsers from `sp_*` constructors
(`rg` census, §sources). JSON and CSS — the two flagship grammars of the whole campaign — already
run on the enum graph. The `Boxed` escape hatch (`mod.rs:129`) and the `methods.rs` "bridge to
Parser" (`methods.rs:1` comment) let the graph tier interoperate with the closure `Parser`.

### 2.4 What the substrate LACKS (the 4A gap, exactly)
1. **Span-only, no value plane.** `SpanKind` produces `Span<'a>` (byte extents), never a typed
   value — there is no `Map`/projection node. The typed value path still lives in the closure
   `Parser<Output>` tier (`parse.rs:61`). 4A must add the semantic seam (§3.1) so one graph serves
   both, or the two-tier split persists (the null hypothesis to beat).
2. **A parallel second codepath.** `SpanParser` runs *alongside* the closure `Parser`, not instead
   of it — violating the one-path precept if left as-is. 4A's ambition is that the graph *is* the
   representation; the closure `Parser` becomes either a thin builder API over it or the `Boxed`
   escape hatch only.
3. **Fusion is not a pass** (§2.2) — it cannot be re-ordered, iterated to fixpoint, unit-tested in
   isolation, or shared with TS.
4. **No `NonTerminal` node / recursion is external.** Recursion is handled by the *closure*
   `LazyParser` (`lazy.rs:21`, `Box<dyn LazyParserFn>` + `cached_parser` memo) outside the enum, so
   the graph is a finite tree with opaque `Boxed` recursion points — it cannot see its own cycles,
   which is exactly what packrat-placement and left-recursion analysis need (§3.3).
5. **No TS counterpart.** The TS side (§2.5) has *no* enum tier at all.

### 2.5 The TS side — reification islands, no graph
TypeScript has only hand-monomorphized *fragments* of a graph, all in `leaf.ts`:
`dispatch()` (`leaf.ts:100`) is a reified disjoint-`Alt` (the `Int8Array(128)` LUT + interned
parser array — §1.2 pass 3 done by hand); `fuseAll()` (`leaf.ts:179`) is a reified `Seq` with
arity-2/arity-3 fully unrolled and a general indexed arm (§1.2 pass 2, by hand, "ONE result array,
no push-growth, no for…of"). `all()` (`leaf.ts:154`) picks `fuseAll` for n>1. These prove the
*value* of the passes on V8 — but each is a bespoke closure factory, not a node in a shared union.
4A on the TS side is a larger lift: introduce `type ParserNode = {tag,…} | …` and the pass
functions, so `dispatch`/`fuseAll` become **outputs** of the FIRST-set/fusion passes rather than
public constructors. Recursion indirection already has its seam: `lazy.ts` +
`createLazyCached`/`LAZY_PARSER_CACHE` WeakMap (`lazy.ts:5,19`).

---

## 3. The three hard problems (and their resolutions)

### 3.1 The semantic seam — `Map` and the opaque-leaf compromise
A pure data graph cannot represent an arbitrary user `map(f)` action as inspectable data (Rust
closures, JS lambdas are not reifiable without staging). The honest, load-bearing answer:
**analyzable skeleton, opaque leaves.** `Map{inner, f: Box<dyn Fn>}` keeps `f` opaque; the graph
sees the *structure around* every action, which is all the passes need — fusion of adjacent maps is
runtime closure composition `g∘f` (no need to see inside), and structural passes (flatten, dispatch,
packrat, diagnostics) never touch `f`. Parsley reaches full transparency via Template-Haskell-staged
`WQ` lambda-ASTs (§4) — a route open to bbnf's own generator path but *not* to hand-written
parse-that combinators, so 4A commits to the opaque-leaf compromise and names its ceiling (§6, R1).

### 3.2 Interpreter dispatch cost vs monomorphized closures
A 25-arm `match` per node per call *can* lose to a monomorphized closure chain — that gap is 4B's
entire raison d'être. `span_parser` mitigates it with `#[inline(always)]` dispatch + flat
`Seq/OneOf(Vec)` so linear chains are one loop, not nested calls. Whether this holds under the
value/`Map` plane (§3.1) — where each node's payload widens — is the primary open measurement
(§6 R2, §7 P1).

### 3.3 Recursion, cycles, and observable sharing (the central risk)
A combinator grammar for `expr = term ("+" expr)?` is **cyclic**. The closure encoding ties the knot
for free via laziness/`lazy()`. A data graph cannot hold an infinite tree — it needs an explicit
`NonTerminal(RuleId)` node and a rule map (`Graph = {rules, entry}`, §1.1). Two ways to recover the
sharing:
- **Observable sharing** (Gill, *Type-Safe Observable Sharing in Haskell*, Haskell 2009;
  `data-reify`): detect shared/cyclic structure by node identity during an `IO`/side-effecting
  traversal. In Rust this means `Rc`-pointer-identity or an `unsafe` address map — fragile, and it
  fights the borrow checker; in TS a `WeakMap` by object identity (which `lazy.ts:5` already uses).
- **Explicit named rules** (the clean path): a `rule!`/grammar-binding API where recursion is an
  explicit `NonTerminal(name)`. parse-that's existing `lazy()` *is already this seam* — it forces
  the user to break every cycle explicitly — so 4A promotes `lazy()`/named bindings to
  `NonTerminal(RuleId)` and builds the rule map from them. This makes packrat placement (§1.2 pass
  4) and diagnostics (pass 5) fall out for free (a rule has a name and a memo boundary), at the cost
  of an **API-surface change** to how recursive grammars are written — the sharpest consumer risk
  (§6 R3), because value.js/latex-paper author recursion via inline `lazy()`.

---

## 4. Prior art (citations)

- **Parsley — the canonical CLOSURE-TO-GRAPH.** Willis, Wu, Pickering, Mühlbauer,
  *Staged Selective Parser Combinators*, ICFP 2020
  (https://mpickering.github.io/papers/parsley-icfp.pdf). Combinators are **smart constructors for
  a combinator-tree AST**; high-level grammar optimizations and analyses run on that tree
  (law-driven rewrites — `pure f <*> pure x ⇒ pure (f x)`, alternation factoring, etc.); the tree
  is then compiled to an **abstract machine** (CPS instruction stream) via a staged final
  interpreter. This is 4A's mechanism end-to-end, with the 4C VM as the compile target. Production
  impls: Haskell (`hackage/parsley`, Typed Template Haskell), Scala (`github.com/j-mie6/parsley`).
- **Design Patterns for Parser Combinators.** Willis & Wu, Scala 2022
  (https://dl.acm.org/doi/pdf/10.1145/3550198.3550427) — the "parser bridge" / smart-constructor
  patterns that keep the reified tree ergonomic; directly informs the 4A construction API.
- **flap — reify, analyze, compile.** Yallop, Xie, Krishnaswami, *flap: A Deterministic Parser with
  Fused Lexing*, PLDI 2023 (https://www.cl.cam.ac.uk/~nk480/flap.pdf; arXiv:2304.05276). Typed
  combinators are reified, checked for a **deterministic Greibach normal form** (1-token
  lookahead), and staged to token-free specialized code **2–6× ocamlyacc**. The "analyze the
  reified grammar, then compile" spine, plus the fused-lexing angle (relevant to the CSS/JSON
  scanner fusion the graph would express).
- **Typed algebraic parsing / FIRST-FOLLOW as analysis.** Krishnaswami & Yallop,
  *A Typed Algebraic Approach to Parsing*, PLDI 2019 — reified typed combinators carrying
  computable FIRST/FOLLOW sets; the theoretical basis for 4A's dispatch-table synthesis (§1.2 pass
  3) and unambiguity checks.
- **Static properties on reified parsers.** Swierstra & Duponcheel, *Deterministic, Error-Correcting
  Combinator Parsers*; Swierstra's `uu-parsinglib` — combinators augmented with nullability/FIRST so
  the grammar structure supports error correction; the lightweight ancestor of the full graph.
- **Observable sharing / cyclic reification.** Gill, *Type-Safe Observable Sharing in Haskell*,
  Haskell 2009 (http://ku-fpg.github.io/files/Gill-09-TypeSafeReification.pdf) + `data-reify` — the
  answer to §3.3 (turning recursive closures into explicit graphs).
- **Defunctionalization & initial-vs-final.** Reynolds, *Definitional Interpreters for Higher-Order
  Programming Languages*, 1972 — closures ⇒ data + apply, the formal frame; Kiselyov's
  tagless-final line — the final(closure) vs initial(data) encoding trade the whole family debates.
- **Packrat placement / left recursion.** Warth, Douglass, Millstein, *Packrat Parsers Can Support
  Left Recursion*, PEPM 2008 — the seeding algorithm 4A's placement pass (§1.2 pass 4) applies only
  at analyzed `NonTerminal`s, superseding global arming.
- **In-repo precedent for the pattern.** bbnf-regex ships an **e-graph equality-saturation** rewrite
  system over its regex HIR (`rust/regex/src/egraph/`, 1,253 LOC — rules `repetition/union/flatten/
  superset/redundant`, `cost.rs`, `saturation_cache.rs`, over the external `egraph`/`egraph-derive`/
  `csp-solver` crates). 4A's fusion pass is the *same shape of rewrite system* applied to the parser
  graph rather than the regex HIR — the isomorphism the constitution (clause ii) and LOCK-2/LOCK-5
  want, reachable because the parser graph is a proper IR.

---

## 5. Integration seams (path:line — where extant code serves)

- **The node union to generalize:** `rust/parse_that/src/span_parser/mod.rs:63` (`SpanKind`),
  `:57` (`SpanParser`), dispatch `:186` (`call_inner`), fast path `:139`/`:154`.
- **The proto-passes to lift out:** `span_parser/methods.rs:16` (`then_span` flatten), `:29` (`or`
  flatten); the fusion ladder in `span_parser/constructors.rs` + the `SepByWs` fusion (`mod.rs:104`);
  the flat-combinator dispatch bodies `span_parser/combinators.rs` (`dispatch_seq/one_of/many/opt/
  skip/next`).
- **The graph's real consumers (Y.13):** `rust/parse_that/src/parsers/json.rs`,
  `parsers/css/{declaration,selector,mod}.rs` — build on `sp_*`; keep them as the parity workload.
- **The value plane to fold in (§3.1):** primary closure core `rust/parse_that/src/parse.rs:61`
  (`Parser{parser_fn: SmallBox<dyn ParserFn+'a,S32>, flags}`), map/methods under
  `rust/parse_that/src/combinators/methods/{sequence,alternation,repetition,sep_by,map,recover}.rs`.
- **The recursion seam (§3.3):** Rust `rust/parse_that/src/lazy.rs:21` (`LazyParser`,
  `Box<dyn LazyParserFn>` + `cached_parser`); TS `typescript/src/parse/lazy.ts:5,19`
  (`LAZY_PARSER_CACHE` WeakMap, `createLazyCached`).
- **TS islands to convert to pass outputs (§2.5):** `typescript/src/parse/leaf.ts:100`
  (`dispatch` LUT), `:154` (`all`), `:179` (`fuseAll` arity-unroll).
- **Diagnostics annotation target:** `SpanParser.label` (`mod.rs:59`, `cfg(diagnostics)`); TS
  furthest-error `parser.ts:51` (`parseStateInner`) + `utils.ts` merge/collect; the per-parse-scoped
  diagnostics refactor (moving off the module-global `diagnosticsEnabled`, `utils.ts:6`) is the
  natural consumer of pass 5.
- **The rewrite-engine precedent to mirror:** `rust/regex/src/egraph/` (whole dir) — the in-tree
  saturation rewrite system whose shape the fusion pass adopts.
- **Isomorphism CI hook (LOCK-6 seam):** the node union + pass set is the machine-checkable
  correspondence a mirror-tree linter would verify — one enum, mirrored `type ParserNode`.

---

## 6. Named risks & counterexamples

- **R1 — the opaque-`Map` ceiling.** If a grammar's cost lives inside `map` actions (semantic
  assembly), reifying the cheap structural glue buys little: the interpreter is no faster than
  closures because the hot work is behind opaque leaves. *Counterexample to hunt:* a projection-heavy
  grammar (value.js's `all()`-of-mapped-parts) where graph ≈ closures. *Bound:* 4A wins on
  *structure-dominated* grammars (JSON/CSS scanning, `Alt` dispatch, `SepBy` loops); the win on
  action-heavy grammars is analyzability (packrat/diagnostics), not raw speed — state this honestly.
- **R2 — interpreter dispatch loses to monomorphization.** A widened node payload (value plane) can
  push the `match` over the point where a monomorphized closure chain (4B) wins. *Counterexample:*
  deeply nested `Skip/Next/Wrap` that does NOT flatten to a flat `Seq` — per-node `match` overhead
  compounds. *Guard:* aggressive flatten pass; measure the graph interpreter head-to-head with the
  closure `Parser` on the SAME grammar, not against the two-tier status quo.
- **R3 — recursion API surface break.** Promoting `lazy()`→`NonTerminal(RuleId)` (§3.3) changes how
  recursive grammars are authored; value.js and latex-paper write inline anonymous recursion via
  `lazy()`. *Counterexample:* a consumer grammar with anonymous mutual recursion the rule-map can't
  name without a rewrite. *Guard:* keep `lazy()` as sugar that auto-mints a `RuleId` via the
  existing cache identity (Rust `Rc`-identity / TS WeakMap) — observable sharing as a fallback so
  the public API is preserved; measure the identity-recovery cost.
- **R4 — TS megamorphic tag dispatch.** A `switch(node.tag)` interpreter reads many node shapes
  through one site — the exact megamorphic IC R4 flags at the closure `.parser(state)` call. A
  per-tag handler table (`handlers[tag](node,state)`) may be better or worse. *Counterexample:* the
  dispatch-LUT lever already under-delivered (+~5% measured vs +10% claimed — the certified delta),
  proving TS dispatch wins are fragile. *Guard:* the differential must measure the tag interpreter
  against today's closures on V8, cold, paired — not assume the Rust win transfers.
- **R5 — two-tier collapse regresses the shipped path.** Folding the closure `Parser` into the graph
  (or vice versa) must not regress the JSON/CSS benches that already run on `SpanParser`. *Guard:*
  the existing `parsers/json.rs` + `parsers/css/*` are the parity floor; byte-identical output +
  ≥0.98× cold-paired per corpus, the same gate PT-2.W1 uses.
- **R6 — the graph is not the bbnf HIR (scope creep into 4D).** The temptation to unify with bbnf
  HIR belongs to 4D. 4A must stop at *structural kinship + a lawful functor* and not merge homes —
  otherwise the family loses its identity and its independent measurability.

---

## 7. What a prototype must test to move this family

The prototype operates on a scratch copy of parse-that per the R3 §8 recipe (rsync, repoint the 7
external sibling patch paths, isolated `CARGO_TARGET_DIR`, nightly-2026-04-11 pin, `data/` copied);
`/Users/mkbabb/Programming/parse-that` stays READ-ONLY. Four falsifiable experiments, smallest-first:

- **P1 — Fusion-as-a-separable-pass (the keystone; cheapest, highest signal).** Extract the
  construction-time flatten+fusion out of `then_span`/`or`/the `TakeUntilAny*`+`SepByWs`
  constructors into a standalone `fuse(Graph)→Graph` rewrite with a fixpoint. **Gate:** on the
  existing JSON+CSS `SpanParser` grammars, `fuse` produces a **byte-identical** optimized graph to
  the current hand-fused constructors (structural equality oracle) AND the JSON/CSS cold-paired
  divan benches are **≥0.98×** the status quo (`parse_that_combinator`, `parse_that_css`, R3 §6).
  Proves the passes are real and free.
- **P2 — The value plane on the graph (the hard one).** Add `Map{inner,f}` (§3.1) and re-express one
  fully typed parser (the `parsers/json.rs` value path) as a single graph, retiring its closure tier.
  **Gate:** typed output parity with the closure `Parser` on the JSON test corpus (37 tests, R3 §7)
  AND the interpreter is **≥0.98×** the closure core head-to-head on the *same* grammar (R2/R5).
  Falsifies or confirms the opaque-leaf compromise.
- **P3 — Packrat placement as analysis.** Build the `NonTerminal` rule map (§3.3) for a
  left-/mutually-recursive grammar (a math-expression grammar, or value.js's `calc`), run an SCC
  analysis that annotates only the recursive rules, and compare to today's global `PACKRAT_ARMED`
  arming. **Gate:** identical parse results; placement-memo **≥** armed-everywhere on the recursive
  grammar AND **strictly faster** on a non-recursive grammar (where global arming taxes the whole
  parse and placement taxes nothing). Proves packrat is a graph analysis, not a global switch.
- **P4 — Rust/TS isomorphism (the constitution-iii proof).** Define the `ParserNode` union + the
  flatten/fuse/dispatch passes in TS; run the same abstract grammar through both. **Gate:** a
  differential test asserts the optimized graph is **node-for-node identical** in structure across
  Rust and TS, and both produce identical parse output on a shared conformance corpus; the TS tag
  interpreter is measured cold-paired against today's TS closures (R4) — win, neutral, or a named
  negative result. Proves the graph shape is genuinely isomorphic and the analyses port.

**The one measurement that decides the family:** P1+P2 together — a *single* graph tier (no parallel
closure path) that keeps JSON/CSS at parity while making fusion a pass. If the value plane forces the
interpreter below the closure core (R1/R2), 4A narrows to "analyzable graph for
packrat+diagnostics+isomorphism, closures retained for hot value assembly" — still a real result,
but a smaller one, and the honest boundary to report.

---

### Sources
Parsley / Staged Selective Parser Combinators (Willis et al., ICFP 2020):
https://mpickering.github.io/papers/parsley-icfp.pdf · https://github.com/j-mie6/parsley ·
https://hackage.haskell.org/package/parsley — Design Patterns for Parser Combinators (Willis & Wu):
https://dl.acm.org/doi/pdf/10.1145/3550198.3550427 — flap (Yallop, Xie, Krishnaswami, PLDI 2023):
https://www.cl.cam.ac.uk/~nk480/flap.pdf · https://arxiv.org/abs/2304.05276 ·
https://github.com/yallop/ocaml-flap — Type-Safe Observable Sharing (Gill, Haskell 2009):
http://ku-fpg.github.io/files/Gill-09-TypeSafeReification.pdf — in-repo:
`rust/parse_that/src/span_parser/`, `rust/parse_that/src/parse.rs`, `rust/parse_that/src/lazy.rs`,
`typescript/src/parse/leaf.ts`, `rust/regex/src/egraph/`.
