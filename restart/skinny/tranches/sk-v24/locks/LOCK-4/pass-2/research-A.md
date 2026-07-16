# LOCK-4 · FAMILY 4A — CLOSURE-TO-GRAPH · pass-2 research

The 4A wager: parse-that's combinators stop constructing opaque nested closures and start
constructing a **static, analyzable parser graph** — a data structure whose nodes fusion,
packrat placement, and diagnostics read as ordinary tree/graph analyses, and whose shape is
one representation across Rust and TS. This report develops that family to its strongest form,
grounds every mechanism in extant code by `path:line`, names the counterexamples that bound
it, and states exactly what a prototype must measure to move it.

---

## 1. The load-bearing discovery: the graph already exists, built and analyzable

The graph 4A wants is not greenfield. `bbnf_ir::IrNode`
(`crates/ir/src/types/node.rs:30`) is a fully-owned, serializable, 14-variant expression
graph that is a **verified near-bijection with parse-that's combinator constructor set**, and
it already carries the three organs 4A demands — a fusion pass suite, an equality-saturation
engine, and two runtimes (a bytecode VM and Rust AOT codegen), plus a MessagePack bus for
crossing the language boundary. 4A's task is therefore not to invent a parser-graph but to
**make parse-that construct this graph (or a parse-that-private isomorph of it) in place of
closures**, and to prove the analyses ride cheaply.

**The 14 variants and their combinator pre-images** (`node.rs:30-98`):

| IrNode variant | parse-that constructor (R3 `parse.rs`, R4 `parser.ts`/`leaf.ts`) |
|---|---|
| `Literal(StringId)` | `string()` (`leaf.ts:276`), Rust leaf |
| `Regex(StringId)` | `regex()` (`leaf.ts:317`) |
| `Epsilon` | the always-succeed leaf |
| `Seq(Vec<IrNode>)` | `.then()` (`parser.ts:81`), `all()`/`fuseAll` (`leaf.ts:154,179`) |
| `Alt(Vec<AltBranch>, Option<AltDispatch>)` | `.or()` (`parser.ts:105`), `any()` (`leaf.ts:28`), `dispatch()` (`leaf.ts:100`) |
| `Repeat{inner,lo,hi}` | `.many()`/`.opt()`/`sepBy` (`parser.ts:523,234,569`) |
| `Ref(RuleId)` | `lazy()` (`lazy.ts`), the named-rule reference |
| `Skip(a,b)` | `.skip()` (`parser.ts:189`) |
| `Next(a,b)` | `.next()` (`parser.ts:212`) |
| `Minus(a,b)` | `.minus()` (`parser.ts:309`) |
| `Negate(inner)` | `.not()`/`.negate()` (`parser.ts:251`) |
| `Map{inner,fn_id}` | `.map()` (`parser.ts:146`) |
| `OptionalWhitespace(inner)` | `.trim()`/`whitespace` (`parser.ts:480`) |
| `TokenDispatch{token,arms,fallback}` | the fused first-char dispatch (`dispatch()`, `leaf.ts:100`) |

The bijection is *near*, not total — §5 enumerates the four combinators with **no node**
(`peek`, `lookAhead`, `recover`, `mapState`), which are 4A's hardest surface and the exact
place a naive "everything becomes a node" claim breaks.

---

## 2. The mechanism, made concrete

### 2.1 The node substrate — projection-as-data, not projection-as-closure

The decisive property that makes the graph *analyzable* is that parse-that's `.map()` closure
becomes **data**, not an opaque `Box<dyn Fn>`. `IrNode::Map{inner, fn_id}` (`node.rs:79`)
references a `FnDescriptor` (`crates/ir/src/types/fn_descriptor.rs:18`) whose only user-facing
variant, `Expr{expr: MapExpr, return_type}`, carries a **symbolic value-expression tree**
(`MapExpr`, `crates/ir/src/types/map_expr.rs`) that IR passes read for constant folding, type
projection, and pattern-based optimization — the doc comment at `fn_descriptor.rs:51-57` states
exactly this. The remaining `FnDescriptor` variants (`EnumWrap`, `BoxWrap`, `NumberConvert`,
`HexConvert`, `SpanCapture`) are compiler-produced specializations, all analyzable data. This
is the resolution of the closure-core's opacity: in parse-that a `.map(f)` node is a wall to
any analysis; in `bbnf_ir` a `Map` node is transparent.

**The gap that bounds this** (directive-confirmed, code-verified): `FnDescriptor` carries **no
host-closure variant**. Every projection must be expressible as a `MapExpr` tree or one of the
five internal specializations. parse-that's public API accepts *arbitrary* host closures
(`x => x.toUpperCase()`, a Rust `|s| custom(s)`), which do not fit `MapExpr`. A faithful 4A
graph therefore needs an **`Opaque(HostFnId)` escape node** that fusion and the type plane
treat as a **barrier** (no fold-through, no projection). The strength of 4A is measured by how
small that barrier surface is on real grammars (§5, §7).

### 2.2 Fusion as a graph→graph pass — it exists and is law-driven

4A's `fuse(Graph)→Graph` is not hypothetical. The transform suite under
`crates/ir/src/passes/transform/` already performs the fusions 4A frames as graph analyses:

- **`fuse.rs:388`** — single-use rule fusion: inlines `Ref(X)→body(X)` when `X` is referenced
  exactly once, explicitly to widen `Alt` FIRST-set coverage for O(1) dispatch (`fuse.rs:6-19`
  worked example). This is the arity-unrolling / `all()`-fusion of `fuseAll` (`leaf.ts:179`)
  recast as a graph rewrite over `Seq`/`Alt`.
- **`fuse_token/factor.rs:339`** — factors common token prefixes across `Alt` branches into a
  single `TokenDispatch` node: the graph form of parse-that's hand-written `dispatch()` LUT.
- **`pattern_dedup.rs:439`** — deduplicates isomorphic subtrees (the sharing/CSE analysis).
- **`inline.rs:331`** — threshold-bounded acyclic inlining, with an `InlineTrace` sink so
  downstream passes re-resolve absorbed rule identities (`fuse.rs:33-40`).

A `fuse(Graph)→Graph` producing output byte-identical to a hand-fused arena parser is thus:
run this pass pipeline over the constructed graph, then lower (§2.4). The oracle is
structural-equality on the lowered artifact plus the paired-cold ratio the 4A charter names.

### 2.3 The equality-saturation engine is CONSUMED, not cloned (LOCK-2 steering answer)

4A's cross-lock question — does the fuse pass *consume* an existing in-tree
equality-saturation engine or *clone* one — has a decisive, code-level answer: **consume**.
`crates/ir/Cargo.toml:19-20` depends on `egraph = { path = "../egraph" }` and
`egraph-derive`, and `crates/ir/src/egraph/` (2643 LOC) is a *client* of that engine, not a
reimplementation. The client wiring:

- `crates/ir/src/egraph/build_egraph.rs:106` translates `IrNode → egraph e-nodes`;
- rewrite rule groups live at `egraph/rules/{universal.rs:418, regex.rs:434, suffix.rs:146}`;
- `egraph/cost.rs:164` supplies the cost model; `egraph/analysis/{mod,facts}.rs` the analyses;
- **`egraph/write_back.rs:378`** extracts the cost-best e-node per class and rebuilds a
  concrete `IrNode` tree, **preserving rule boundaries** (`write_back.rs:8-16`: descending into
  another rule's root class emits `Ref(Y)` rather than inlining — essential for JSON's mutual
  recursion `value↔object↔array`).

Crucially, the **same** `../egraph` engine is the one `bbnf-regex` uses for HIR saturation
(R3 §3, `rust/regex/src/egraph/`), so a single equality-saturation home serves the regex
continuum *and* the combinator graph — the reuse the constitution's clause (ii) and LOCK-2
both want. 4A's fusion-as-saturation is therefore a matter of *pointing the existing
`build_egraph`/`write_back` cycle at the combinator-constructed graph*, not standing up new
infrastructure. `csp-solver = "0.1"` (`Cargo.toml:18`) is likewise already present for the
extraction/scheduling step (`egraph/mod.rs`, external `egraph::csp_scheduler`).

### 2.4 What survives to runtime — all three answers already run (Q-runtime axis)

The 4A charter treats "what survives to runtime" as open. In-tree, **all three terminal forms
consume the same `IrNode` graph and already run**, so the runtime fork is *selectable*, not
architectural:

1. **Nothing (AOT Rust)** — the codegen backend lowers `IrNode` to specialized Rust; the graph
   is compile-time-only, matching parse-that's monomorphized target (this is 4B's terminal, and
   4A can be *formulated as its source representation* — see §6).
2. **The graph (tree-walking VM)** — `parse_with_ir(ir, input)` at
   `crates/ir/src/vm/interpreter/mod.rs:373` compiles to bytecode then interprets; a
   value-producing parse with diagnostics (`ParseResult` at `interpreter/mod.rs:359`).
3. **A bytecode program** — `vm/bytecode.rs:45` defines `Op` (a ~40-opcode ISA: `MatchString`,
   `MatchRegex`, `Dispatch(u16)`, `DispatchToken(u16)`, `RepeatBegin/End`, `SaveState/
   RestoreState/DropState`, `MakeArray`, `MemoCheck/MemoStore`, `Minus`, `Negate`, …). The
   compiler (`vm/compiler/compound.rs:392`, `mod.rs:116`) lowers `IrNode → Vec<Op>`. This is a
   language-neutral program (LOCK-4C's terminal); 4A supplies its *source graph*.

The consequence for 4A: it is the **common front** for every LOCK-4 candidate runtime. The
"common-baseline runtime fork" keystone — the runtimes measured over the *same* graph with the
*same* leaf substrate — is directly enabled here: build one `IrNode`, then race `parse_with_ir`
(interpret), the bytecode VM (compile), and the Rust AOT lowering, all off the identical graph.

### 2.5 The cross-language face — the graph crosses as data, already wired

The graph is fully owned (no lifetimes) and MessagePack-serializable (`crates/ir/src/lib.rs:7`;
`rmp-serde = "1"` at `Cargo.toml:12`; every type derives `Serialize/Deserialize`, e.g.
`node.rs:29`). parse-that's Rust workspace **already depends on this crate** —
`~/Programming/parse-that/rust/.cargo/config.toml:9` reads
`bbnf-ir = { path = "../../bbnf-lang/crates/ir" }`. So the substrate for "the graph shape is
isomorphic across rust/ts" is: serialize `IrNode` in Rust, ship the bytes, decode + interpret
in a small TS VM whose `Op` switch mirrors `bytecode.rs:45`. This is the first-cross-language
number the directives call high-leverage, and it reuses the existing bus rather than inventing
a wire format.

---

## 3. Integration seams (where extant code serves 4A)

| Seam | Path:line | Role for 4A |
|---|---|---|
| Node substrate | `crates/ir/src/types/node.rs:30` | the parser graph (14 variants) |
| Projection-as-data | `crates/ir/src/types/fn_descriptor.rs:18`, `types/map_expr.rs` | analyzable `.map`; names the host-closure gap |
| Fusion passes | `passes/transform/fuse.rs:388`, `fuse_token/factor.rs:339`, `pattern_dedup.rs:439`, `transform/inline.rs:331` | `fuse(Graph)→Graph`, law-driven |
| FIRST/dispatch analysis | `passes/sets/first_sets.rs:431`, `AltBranch.first_set` (`node.rs:122`), `AltDispatch` (`node.rs:128`) | packrat/dispatch placement as graph analysis |
| Egraph client | `egraph/build_egraph.rs:106`, `egraph/write_back.rs:378`, rules `egraph/rules/universal.rs:418` | saturation over the graph, engine consumed |
| Egraph engine (shared) | `crates/egraph/{rewrite,extract,analysis,csp_scheduler}.rs`, dep at `crates/ir/Cargo.toml:19` | the one equality-saturation home (also serves bbnf-regex) |
| VM interpret runtime | `vm/interpreter/mod.rs:373` (`parse_with_ir`) | graph-survives-to-runtime terminal |
| Bytecode ISA + compiler | `vm/bytecode.rs:45` (`Op`), `vm/compiler/compound.rs:392` | program-survives-to-runtime terminal (4C source) |
| Serialization bus | `crates/ir/src/lib.rs:7`, `Cargo.toml:12` (`rmp-serde`) | cross-language crossing (LOCK-6 face) |
| Cross-repo dep (live) | `parse-that/rust/.cargo/config.toml:9` | parse-that already links `bbnf-ir` |
| parse-that closure core | `parse-that/rust/.../parse.rs:61` (`Parser<'a,O>{SmallBox<dyn ParserFn>}`), TS `parser.ts:24` | the closures 4A replaces with graph construction |

---

## 4. Prior art (with citations)

- **Staged Selective Parser Combinators** — Willis, Wu, Pickering, *ICFP 2020* (and Willis's
  *Parsley* / "Garnishing Parsec with Parsley" thesis, 2023). The closest prior art to 4A:
  combinators build an *analyzable free/selective applicative structure*, passes run law-based
  rewrites and recover let-bindings for recursion via **observable sharing**, then the graph is
  *staged* (compiled) to fast code. 4A is this pattern with `IrNode` as the free structure,
  `passes/transform/` as the law rewrites, and `Ref(RuleId)` as the recovered let-binding.
- **Type-safe Observable Sharing** — Andy Gill, *Haskell Symposium 2009*; and Oliveira &
  Löh, "Functional Programming with Structured Graphs," *ICFP 2013*. These name 4A's central
  hazard: a combinator graph built from closures is a *tree with implicit sharing*; recovering
  the DAG (so a recursive `Ref` is a back-edge, not an infinite unfolding) is the hard part.
  In-tree this is already handled — `Ref(RuleId)` plus the `visiting`-set cycle guard in
  `write_back.rs:16-20` and the `dag` module (`crates/ir/src/dag/`).
- **egg / egglog** — Willsey et al., "egg: Fast and Extensible Equality Saturation," *POPL
  2021*. The model `crates/egraph` implements and 4A consumes; the answer to "consume vs clone."
- **tree-sitter** (Brunsfeld). Grammar-as-data compiled to LR tables — precedent that a
  parser's structure can be a first-class analyzable artifact rather than nested procedures.
- **MLIR** (Lattner et al., *CGO 2021*). The multi-level lowering `IrNode → bytecode Op → Rust`
  is an MLIR-style dialect tower; the write-back/extract cycle mirrors MLIR's pattern-rewrite
  + canonicalization.
- **nom / winnow** (Rust) — the *counter-model*: fast monomorphized combinators with **no**
  analyzable graph. They set 4B's bar and mark exactly what 4A trades a runtime graph to gain
  (analysis) and must prove it does not lose (speed) — the projection-dense paired bench (§7).
- **Swierstra, "Combinator Parsing: A Short Tutorial"** (2009) and the UU "error-correcting"
  combinators that compute FIRST/FOLLOW at construction — precedent for `AltBranch.first_set`
  being populated as the graph is built.

---

## 5. Named risks and counterexamples

1. **The state-effecting combinators have no node (the near-bijection's break).** `IrNode` has
   `Negate` (zero-width negative) but **no** positive `peek`/`lookAhead`, **no** `recover`, **no**
   `mapState` (`parser.ts:162,339,365,653`). These thread or inspect parser state in ways `Seq/
   Alt/Map` cannot express. 4A must extend the vocabulary — a minimal `Peek(inner)`,
   `Recover{body, sentinel}`, `MapState{inner, fn_id}` set — and each new node must earn its
   place with a paired regression lane (this is the same enumeration LOCK-4D owes; 4A supplies
   the *node forms*). **Counterexample to guard:** adding a node whose only lowering is "call an
   opaque host closure" gains nothing over the closure core — such a node must be a genuine
   analyzable form or it is just `Opaque` wearing a variant name.

2. **Arbitrary `.map` closures cannot become data.** `FnDescriptor` has no host-closure variant.
   value.js's 59 `all()` sites and its color/units/easing projections, latex-paper's BibTeX
   projections — an unknown fraction use closures with no `MapExpr` form. Every such site
   becomes an `Opaque` fusion barrier. **If the barrier surface is large, 4A's analysis story
   collapses to the closure core with extra indirection.** This is the family's single biggest
   risk and the first thing a prototype must quantify (§7.1, §7.5).

3. **Projection-dense grammars may lose to closures.** The directives flag that the value-plane
   `Box<dyn Fn>` indirection confines to scalar leaves while the structural spine inlines. On an
   `all()`-of-mapped-parts grammar, the VM pays per-`Map` dispatch where the closure core's
   monomorphized `.map` inlines. The honest negative result — "closures win on the LL(1)
   projection-dense shape" — must be *named if measured*, per the charter.

4. **Graph construction is not free.** parse-that builds closures once at grammar-construction
   time (R4 §6e — per-parse closure alloc is nil). Building an `IrNode` tree, running the fuse
   pipeline, and (for the VM) compiling to bytecode is heavier construction. For long-lived
   parsers this amortizes; for one-shot parses it may not. The paired-cold protocol must include
   construction cost honestly, not amortize it away.

5. **The VM interpret lane is not the AOT lane.** `parse_with_ir` (`interpreter/mod.rs:373`) is a
   tree/bytecode walker; it will not beat monomorphized Rust or the tuned parse-that closure core
   on raw throughput. 4A's value is the *analyzable graph as the common front*, with the AOT
   lowering (§6) as the speed terminal — conflating "4A the representation" with "the VM as the
   product runtime" is a category error the report guards against.

6. **Two-arm window asymmetry** (the standing §0/09 lesson): any "graph ties/beats closures"
   number gathered without cold, sample-interleaved A,B,A,B pairing is disinformation. Every 4A
   number is paired-cold, both arm orders, two load windows, same leaf substrate.

---

## 6. The one formulation choice 4A must state (Q-ownership × terminal)

Two sub-questions decide 4A's shape:

- **Whose representation does `.then()` build?** Either (a) parse-that owns a *private*
  isomorph of `IrNode` (Rust struct + TS mirror), or (b) parse-that builds the shared
  `bbnf_ir::IrNode` directly — the dep is already wired (`config.toml:9`). Route (b) buys the
  entire pass/egraph/VM suite for free and satisfies the constitution's isomorphism clause by
  *construction* (one type, one home); route (a) keeps parse-that decoupled but re-clones the
  substrate — a Y.13 "consumer-less duplicate" smell unless the isomorph is a thin newtype. The
  strongest 4A takes route (b) with an `Opaque(HostFnId)` escape for the closure surface the
  shared IR cannot name.

- **Is 4A a terminal runtime or a source representation?** The evidence says **source
  representation**: 4A is best formulated as *the analyzable front that lowers to* whichever
  terminal LOCK-4 selects (VM interpret, bytecode program, or AOT Rust — all three consume
  `IrNode` today). The observable distinguishing "4A-as-terminal" from "4A-as-source-of-4B/4C":
  whether a graph *survives to runtime at all*. If the fastest measured lane is AOT Rust with
  the graph erased, 4A is the source of 4B; if the VM/bytecode ties within the cost model, the
  graph survives and 4A is a terminal in its own right. **The prototype's runtime fork decides
  this — it is currently under-determined and must be measured, not asserted.**

---

## 7. What a prototype must test to move 4A

Six numbers, each paired-cold (A,B,A,B, N≥200, two windows, both arm orders, same leaf
substrate, median + dispersion), on a scratch copy per R3 §8:

1. **Projection-dense vs structure-dominated tie test.** Two grammars — one `all()`-of-mapped
   parts (projection-dense, e.g. a value.js-shaped color/unit grammar), one structural (JSON
   skeleton). Race the analyzable interpreter (`parse_with_ir`) against the parse-that closure
   core on both. **Deliverable:** the decisive paired ratio per grammar, and — if closures win
   the projection-dense LL(1) shape — that named negative result.

2. **Fused-region size distribution.** Run `passes/transform/fuse.rs` + `fuse_token/factor.rs`
   over JSON, CSS, and value.js grammars; histogram fused-region node counts. **Deliverable:** a
   measured distribution showing regions grow large enough to amortize per-region dispatch (or
   the counter-finding that they stay small).

3. **`fuse(Graph)→Graph` byte-identity + parity.** A structural-equality oracle between
   (a) fuse-then-lower `IrNode` and (b) a hand-fused arena parser over a shared corpus, plus
   the ≥0.98× paired-cold ratio. **Deliverable:** oracle pass + the ratio.

4. **Egraph-reuse dependency map.** A cargo/dep-graph artifact proving `crates/ir::egraph`
   consumes `crates/egraph` (not a fork), with the `build_egraph`→saturate→`write_back` cycle
   producing a graph structurally equal to the destructive fuse pipeline's output on JSON.
   **Deliverable:** the dependency map + a fuse≡saturate equality on one grammar (LOCK-2 steering
   evidence).

5. **Host-closure barrier surface.** Over value.js and latex-paper grammars, count the fraction
   of `.map` sites expressible as `MapExpr` vs those needing `Opaque(HostFnId)`, and the fraction
   of combinator surface using `peek`/`lookAhead`/`recover`/`mapState`. **Deliverable:** the
   barrier percentage — the number that decides whether 4A's analysis story holds on real
   grammars (risk 2).

6. **First cross-language number (isomorphism proof).** Serialize an `IrNode` graph via
   `rmp-serde` (`lib.rs:7`) in Rust; decode + interpret it in a ~200 LOC TS VM whose switch loop
   mirrors `bytecode.rs:45`. Dual oracle: byte-identical opcode stream **and** identical parse
   over a shared corpus, at a stated no-regression floor vs today's TS closures. **Deliverable:**
   the graft bench — the first honest rust/ts paired number, which no LOCK-4 terminal can be
   named without.

---

## 8. Summary judgment for 4A

The closure-to-graph family is the **strongest-substantiated** LOCK-4 route because its graph,
its fusion passes, its equality-saturation engine (consumed, shared with the regex continuum),
its two runtimes, and its cross-language bus **all exist and run in-tree today**, and parse-that
**already links the crate**. 4A's genuine open work is narrow and measurable: (i) close the
four-node vocabulary gap for state-effecting combinators with lawful analyzable forms, (ii)
bound the `Opaque` host-closure barrier surface on real grammars, and (iii) measure the runtime
fork to decide whether the graph survives to runtime (4A-as-terminal) or erases into AOT Rust
(4A-as-source-of-4B). The single result that would most advance the family is prototype-test 5
(barrier surface) paired with test 1 (projection-dense tie) — together they falsify or confirm
that a general analyzable graph earns its keep against the tuned closure core.
