# LOCK-4 · Family 4A — CLOSURE-TO-GRAPH · pass-3 research

*The combinator core as an analyzable static parser graph, not opaque nested closures.
Fusion, packrat placement, and diagnostics become graph passes; the graph shape is
isomorphic across Rust and TS. Every measurement names its HEAD; prototypes base-pin the
scratch copy per R3 §8.*

---

## 0. The thesis in one line

parse-that's combinator constructors return **opaque closures** — Rust
`Parser<'a,O>{ parser_fn: SmallBox<dyn ParserFn+'a, S32> }`
(`rust/parse_that/src/parse.rs:61`), TS `Parser<T>{ parser: ParserFunction<T> }`
(`typescript/src/parse/parser.ts:24-32`). A closure is an analysis barrier: fusion,
packrat, and dispatch are done by hand at construction time (`seq!`/`alt!` macro box-collapse
`combinators/macros.rs:2,80`; TS arity-unrolled `fuseAll` `leaf.ts:179-273`; hand-built
`Int8Array(128)` dispatch `leaf.ts:100-151`). 4A **reifies** each combinator application into a
node — a smart constructor returns a graph node, not a closure — so those hand optimizations
become *derivable passes over a graph the compiler can see through*. The graph already exists,
built, on the bbnf side: `bbnf_ir::IrNode` — and it is a 14-variant near-bijection of exactly
this combinator set.

---

## 1. The substrate — the graph already exists (`bbnf_ir`)

`IrNode` (`crates/ir/src/types/node.rs:30`) is Serde-derived
(`#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]`, `:29`) and has **14 variants**,
a near-bijection of parse-that's combinator surface:

| IrNode variant (node.rs) | parse-that combinator (Rust `parse.rs`/TS `parser.ts`) |
|---|---|
| `Literal(StringId)` :33 | `string()` `leaf.ts:276` |
| `Regex(StringId)` :36 | `regex()` `leaf.ts:317` |
| `Epsilon` :39 | `succeed`/`eof` |
| `Seq(Vec<IrNode>)` :43 | `all()`/`then` `parser.ts:81`, `fuseAll` |
| `Alt(Vec<AltBranch>, Option<AltDispatch>)` :48 | `any()`/`or` `parser.ts:105`, `dispatch` |
| `Repeat{inner,lo,hi}` :54 | `many`/`opt`/`sepBy` `parser.ts:523,234,569` |
| `Ref(RuleId)` :61 | `lazy()` `lazy.ts` |
| `Skip(a,b)` :65 | `skip` (`<<`) `parser.ts:189` |
| `Next(a,b)` :68 | `next` (`>>`) `parser.ts:212` |
| `Minus(a,b)` :71 | `minus` `parser.ts:309` |
| `Negate(inner)` :75 | `not`/`negate` `parser.ts:251` |
| `Map{inner,fn_id}` :79 | `.map(f)` `parser.ts:146` |
| `OptionalWhitespace(inner)` :84 | `trim`/`FLAG_TRIM_WS` `parser.ts:480` |
| `TokenDispatch{token,arms,fallback}` :90 | (compiler-synthesized lexer-parser fusion) |

The graph-walk substrate is already there: `for_each_child` / `for_each_child_mut` /
`count_nodes` (`node.rs:145,173,202`) — the visitor every analysis pass rides. The
**canonical hash-consed form** is `GrammarDag` (`crates/ir/src/dag/mod.rs:62`): each distinct
sub-expression interns to a stable `NodeId(u32)` (`:83` `ir_node_to_id`), giving structural
sharing (`sharing_ratio` `:140`) — this is defunctionalization made concrete, the "combinator
tree" of Parsley (§4).

**The analyses that 4A claims are already graph passes** (`crates/ir/src/passes/mod.rs`):
- **Diagnostics points** → `compute_first_sets`, `compute_follow_sets` (`passes/sets.rs`,
  re-exported `passes/mod.rs:71-73`); FIRST is stored *on the node* (`AltBranch.first_set:
  Option<CharSet128>` `node.rs:122`).
- **Dispatch (the hand LUT, derived)** → `generate_dispatch_tables` (`passes/sets.rs`) emits
  `AltDispatch{table:Vec<u8>, fallback_idx}` (`node.rs:128`) — the 128-entry byte table that
  is `leaf.ts:100-151`'s `Int8Array` LUT, but *derived from disjoint FIRST sets* instead of
  hand-written.
- **Fusion** → `fuse_single_use`, `fuse_token_dispatch`, `merge_literals`, `inline_acyclic`,
  `eliminate_epsilon` (`passes/transform.rs`, re-exported `:75-77`); plus the grammar-tier
  e-graph rules (`crates/ir/src/egraph/` — DeduplicateAltBranches, SupersetAbsorbAlt,
  UnionMergeAlt, FuseAltRegexBranches, per `passes/mod.rs:27-32`).
- **Packrat placement** → `eliminate_direct_lr` / `eliminate_indirect_lr` (`passes/lr.rs`,
  `:66`); memo emission is `Op::MemoCheck`/`MemoStore` (`vm/bytecode.rs:137,140`), placed by
  the compiler where LR is detected, not armed globally at runtime as TS does
  (`packrat.ts:290`).

**Two lowerings already consume this graph** (the Q5 observable, §Q5):
1. Codegen → monomorphized Rust (the generator path; 4B's terminal).
2. Bytecode VM → `BytecodeProgram` (`vm/bytecode.rs:155`, Serde-derived) → `Interpreter`
   (`vm/interpreter/mod.rs:71`).

**The parse-that-native precedent that this is buildable there:** `span_parser/` is already
an **enum-dispatched, vtable-free** parser (`rust/parse_that/src/span_parser/span_scanner.rs:10`
`pub enum SpanScanner`; modules `constructors/combinators/methods/leaves/sep_by/wrap/scan`).
It is a partial, hand-rolled node graph over a scan subset. 4A on parse-that = generalize
`span_parser`'s enum-dispatch to the *full* combinator set, made structurally isomorphic to
`IrNode` — replacing `SmallBox<dyn ParserFn>` with an `enum Node` the same 14 shapes.

---

## 2. The mechanism made concrete

**Smart constructors, not closures.** Each combinator method builds and returns a node:
`string("a")` → `Node::Literal(intern("a"))`; `a.then(b)` → `Node::Seq(vec![a,b])` (with the
existing left-nesting flattened, as `Seq` is N-ary flat by `node.rs:43`); `a.or(b)` →
`Node::Alt([a,b], None)`; `a.map(f)` → `Node::Map{inner:a, fn_id:register(f)}`. Construction
is pure tree-building; **no parse logic runs until a lowering step consumes the graph.** This
is the drop of monadic `chain` for a selective/applicative core (§4, Parsley): a runtime-
dependent `chain(|x| ...)` produces *dynamic* structure the graph cannot see, so it either (a)
stays a leaf opaque barrier, or (b) is expressed selectively (`branch`/`select`).

**The graph is the API's return type.** The public surface is unchanged for the applicative
fragment; a `Parser` *value* is now a `Node` (or an id into a node arena), and `.parse(input)`
is `lower(node)(input)` where `lower` is chosen once. Rust: `Node` is an owned enum (or an
arena id) — no `dyn`, no `SmallBox` heap box per combinator (dissolves the R3 §5 alloc hotspot
`parse.rs:74`). TS: `Node` is a shape-stable POJO with a numeric `tag` (monomorphic hidden
class — the R4 §6c megamorphic `this.parser(state)` IC is replaced by a `switch(tag)`).

**Lowerings (the graph → runnable).** Three, all extant on the bbnf side, all fed by one graph:
- **Interpret** — walk the node graph directly, or its `BytecodeProgram` compilation
  (`vm/compiler/`). Portable, fast to stand up, isomorphic by serialization. Per-parse setup
  tax = `Interpreter::new` (§Q7).
- **Monomorphize (Rust)** — a proc-macro / `const fn` graph feeds codegen to specialized Rust
  (4B). Requires the graph be comptime-known (§Q1 — the crux).
- **Build-time emit (TS)** — a fuser over the comptime graph emits specialized JS chains (the
  arity-unroll generalized). XL-D's settled fact: the fast TS face is this emitter; runtime
  decoders survive only as oracles.

**Isomorphism by construction.** The node shapes are the *same 14* across Rust and TS; the
serialized graph (`IrNode`/`BytecodeProgram`, Serde) is the crossing artifact (§Q5/Q6).
`GrammarSpan` is deliberately owned/lifetime-free "so it survives serialization across the WASM
boundary" (`node.rs:16`) — the crossing was designed for.

---

## 3. Integration seams (path:line where extant code serves)

- **Graph type + visitor:** `crates/ir/src/types/node.rs:30,145,173,202` — reuse `IrNode`
  wholesale as the parse-that node enum, or mint a parse-that-native `Node` isomorphic to it.
- **Hash-consed canonical form:** `crates/ir/src/dag/mod.rs:62-148` (`GrammarDag`, `NodeId`,
  `node_for`, `sharing_ratio`).
- **Diagnostics/dispatch passes:** `crates/ir/src/passes/sets.rs` (`compute_first_sets`,
  `compute_follow_sets`, `generate_dispatch_tables`, `sort_alt_branches`).
- **Fusion passes:** `crates/ir/src/passes/transform.rs` (`fuse_single_use`,
  `fuse_token_dispatch`, `merge_literals`, `inline_acyclic`); e-graph rules
  `crates/ir/src/egraph/`.
- **Packrat placement:** `crates/ir/src/passes/lr.rs:66`; memo opcodes `vm/bytecode.rs:137,140`.
- **VM lowering + interpreter:** `crates/ir/src/vm/compiler/{node,emit,rule,compound}.rs`,
  `vm/bytecode.rs:155`, `vm/interpreter/mod.rs:71,114`.
- **Value/host descriptor:** `crates/ir/src/types/fn_descriptor.rs:18` (`FnDescriptor`),
  `types/map_expr.rs` (`MapExpr`).
- **parse-that closure sites 4A replaces:** Rust `parse.rs:61,74` (`SmallBox`),
  `lazy.rs:21,31` (`Box<dyn LazyParserFn>`); TS `parser.ts:24-32` (`.parser` field),
  `leaf.ts:100-151,179-273` (hand LUT + arity-unroll). parse-that-native node-graph seed:
  `rust/parse_that/src/span_parser/`.

---

## 4. Prior art (citations)

- **Willis, Wu, Pickering — "Staged Selective Parser Combinators" (ICFP 2020, PACMPL 4).**
  THE canonical 4A. Combinators are smart constructors for an AST ("the combinator tree") on
  which high-level grammar analysis/optimization runs; the tree lowers to a second AST (an
  abstract machine) where control flow is explicit for low-level opts, then Typed Template
  Haskell **stages** it to hand-quality code. Crucial constraint: monadic `chain` generates
  *dynamic* structure and must be dropped; **selective functors** (`branch`) recover the lost
  power statically. Parsley (Scala/Haskell) outperforms the parsec family. This is the exact
  two-IR shape bbnf already has (IrNode graph → BytecodeProgram). https://mpickering.github.io/papers/parsley-icfp.pdf
- **Krishnaswami & Yallop — "A Typed, Algebraic Approach to Parsing" (PLDI 2019, Distinguished
  Paper + Artifact).** A type system for CFEs identifying the LL(1)/unambiguous fragment;
  combinators guarantee linear-time, single-token-lookahead parsing; **multi-stage programming**
  over the type information beats ocamlyacc. Uses grammar-independent **FLast** sets (a
  compositional FOLLOW) — directly relevant to bbnf's per-node `first_set` (`node.rs:122`) and
  `compute_follow_sets`. Artifact `ocaml-asp`. https://www.cl.cam.ac.uk/~jdy22/papers/a-typed-algebraic-approach-to-parsing.pdf
- **Reynolds — "Definitional Interpreters" (1972); defunctionalization.** The theoretical root:
  replace higher-order closures with a first-order data type + an apply function. `IrNode` +
  the interpreter IS the defunctionalized combinator. `GrammarDag` hash-consing is the
  standard refinement.
- **rustc/LLVM & MLIR — pass pipelines over an owned IR.** The "one rich IR refined in place by
  annotating passes" shape (LLVM); `AltDispatch`/`first_set` as node-borne attributes mirror
  MLIR op attributes. tree-sitter's compiled table automaton is the fully-interpreted extreme;
  egg/egglog is the e-graph saturation `crates/ir/src/egraph/` already vends.

---

## 5. Directive answers (through the 4A lens)

**Q1 — runtime graph → comptime monomorphization.** No such path exists today, and Rust
forbids the naive one. parse-that's runtime API builds *values* (`Parser`/closures); a value
built by `string("a").then(b)` at runtime cannot reach comptime monomorphization absent staging
or reflection, which Rust lacks. bbnf's monomorphization path is **grammar-TEXT → IrNode
(comptime-known) → codegen** (proc-macro/build-script), never runtime-graph → comptime. A
monomorphizing lowering therefore *demands* the API surface be one of: (i) a `const fn`
combinator set returning a `const`/`&'static [Node]` graph a proc-macro/const-eval can read; or
(ii) a build-time codegen step over a comptime-known graph (the generator model). The honest
finding: **runtime-graph monomorphization is a category error in Rust** — the runtime graph's
only lowerings are *interpretation* (VM) or *serialize-then-offline-codegen*. Monomorphization
belongs to the comptime graph (4B), and 4A's contribution is making the runtime graph *the same
shape* so a serialized runtime graph can be fed offline to that codegen. **Prototype must show:**
a runtime-built `Node` graph, serialized, fed to `vm/compiler` → codegen, produces the same
specialized parser as the grammar-text path (proving graph-of-record parity).

**Q2 — value plane through an arbitrary host closure.** `FnDescriptor` (`fn_descriptor.rs:18`)
has **no `HostClosure` variant**: its six variants are `EnumWrap`/`BoxWrap`/`NumberConvert`/
`HexConvert`/`SpanCapture` (all compiler-internal specializations) + `Expr{MapExpr, return_type}`
(the sole user-facing form, `:54`). Every `->` today must be **MapExpr-expressible** or one of
the five internal specializations. But parse-that's `.map(f)`/`.chain(f)`/`.mapState(f)` take
*arbitrary* host closures (`parser.ts:146,124,162`; Rust `combinators/methods/map.rs`) — value.js
alone drives 59 `all()` sites plus pervasive `.map` (R4 §7). A serializable descriptor for those
must carry, minimally: a stable **host-fn id** (into a per-backend registry), the **arg arity/
positional typed shapes**, and a **return `TypeDesc`** — i.e. a `HostClosure{fn_id: FnId,
sig: TypeDesc}` rung that is an *opaque* node (analyses cannot see through the body). The implied
host-fn lattice: `MapExpr` (transparent, foldable) ⊐ the 5 internal specializations (transparent
to codegen) ⊐ `HostClosure` (opaque barrier). **Prototype must census** value.js / latex-paper /
keyframes `->`+`.map` sites: count MapExpr-expressible vs not, and record the non-projection
typed shapes.

**Q3 — opaque-barrier surface size.** The barrier is exactly the `HostClosure` (Q2) plus the
`Regex(StringId)` leaf's engine internals (opaque by XL-C design — the grammar tier is opaque,
the Hir engine tier is separate). An analysis "cannot see through" a Map iff its fn is not
MapExpr. **Uncounted here** (requires building the graph over the real corpora, a prototype
task); the target floor is ≤2% of nodes. Named risk: parse-that combinators are *more* closure-
happy than grammar `->`, so the parse-that barrier ratio is expected **above** the grammar-side
ratio — the census must be run on the parse-that combinator graph, not the .bbnf grammar graph.

**Q4 — fusion policy / blowup budget.** The pluggable {inline-monomorphize | enum-node |
box-here} cost model has concrete anchors already: **Seq-collapse** (`macros.rs:2,80` collapses
N−1 boxes to 1), **arity-unroll** (TS `fuseAll` arity-2/3 `leaf.ts:183-251`; general `for`-arm
beyond), **boxing threshold** (`SmallBox … S32` inline-vs-heap spill `parse.rs:61`), **dispatch-
fanout** (`AltDispatch` 128-entry table `node.rs:128`, capped by FIRST-set disjointness). Named
caps a firing number must set: `fuse-arity` (TS proves ≤3 unroll wins; measure where the general
arm ties), `dispatch-fanout` (max branches before the LUT loses to sequential trial), `boxing-
depth` (S32 = 32-byte inline ceiling), and a `.rlib`/`dist` + incremental-compile budget bound to
the constitution's generated-size budget. **Prototype must measure** code-size / compile-time
growth vs fuse-arity across CSS **and** value.js (the full consumer surface, not a microbench).

**Q5 — serializable-plan observable.** **YES, and it is intrinsic to the design, not the Rust
type system.** `IrNode`, `GrammarIR`, and `BytecodeProgram` are all Serde-derived
(`node.rs:29`, `bytecode.rs:8,155`); `GrammarSpan` is lifetime-free for WASM crossing
(`node.rs:16`). A fused region is derivable from one serializable graph that drives *both*
lowerings. The distinguishing observable: **serialize the Rust-built graph, decode it in TS,
drive the TS lowering, and get byte-identical parses** (opcode stream + Value tree) against the
Rust lowering of the same graph. This is one plan → two lowerings → identical output. The proof-
against would be a fused region expressible only via Rust monomorphized types with no serializable
residue — which the extant `BytecodeProgram` refutes (fusion is already baked into a serializable
opcode stream, e.g. `Op::Dispatch`/`DispatchToken` `bytecode.rs:100,104`).

**Q6 — isomorphism-of-record standard + value-plane home.** The dual oracle for a program
crossing as data: (a) **opcode-stream identity** — serialize the Rust `BytecodeProgram`, decode
in TS, compare the `Vec<Op>` stream against a **live** Rust golden emitted this-run (not a hand-
faked blob); (b) **Value-tree identity** — the TS-decoded parse's `Value` tree vs the Rust
`Interpreter`'s (`vm/interpreter/value.rs`), node-for-node. Corpus: json / css / csv (the three
grammar corpora with existing oracles). **The internal-descriptor evaluator (Hex/Number/Enum) is
owned by LOCK-3 (IR-VM lowering), not LOCK-7 (host dispatch):** `NumberConvert`/`HexConvert`/
`EnumWrap` (`fn_descriptor.rs:20-49`) are *compiler-internal specializations* produced during
lowering, evaluated in the VM/codegen path, never dispatched to a user host fn. Only the user-
facing `Expr{MapExpr}` (and a future `HostClosure`) touches LOCK-7's host boundary.

**Q7 — setup vs value-plane tax vs recognizer cost.** If the 4A graph is *interpreted*,
`Interpreter::new` (`vm/interpreter/mod.rs:114`) is the per-parse tax: **8 `Vec`s** — `values`
(cap 64), `call_stack` (32), `checkpoints` (32), `repeats` (16), `memo_starts` (8),
`value_depth_stack` (8), `rule_stack` (16), `diagnostics` (0) — **plus an `FxHashMap` memo**
(`:84,126`), 10 allocations before a byte is read. Pooling/arena-reuse: keep one `Interpreter`,
`clear()` each Vec (retain capacity) and the map between parses → the 10 allocations vanish on
steady state. Amortization: the tax is fixed, so its share of single-parse cost falls as
input length grows and *dominates* for the stop-early get lane (resolves at 0.032% of doc, SPEC
§0/06). The clean three-way split a prototype must produce: **recognizer** (the opcode loop /
node walk), **value-plane** (`Value` construction — `MakeArray`/`MakeTagged` `bytecode.rs:128,131`),
**setup** (the 10 allocations). **Prototype must measure** setup's share of a single short-input
parse and the amortization curve as setup is pooled.

**Q8 — predecessor shapes (report only).** **LOCK-5 (shared-substrate semantics):** XL-C already
retired the regex functor — regex is now two domain-fit types, an opaque `Regex(StringId)`
grammar tier (`node.rs:36`, the combinator-graph leaf) and a rich `Hir` engine tier, bridged
cheaply at compile (`Dfa::compile` `bytecode.rs:238`). For 4A this means the `Regex` node is an
opaque leaf whose engine internals are *not* a graph-analysis concern; the shared-IR question no
longer hinges on ONE-HIR. Open sub-question feeding 4A: whether the combinator graph's `Regex`
leaf and the Hir tier share the e-graph rewrite library (`crates/ir/src/egraph/` ↔ bbnf-regex
egraph). **LOCK-7 (host-fn lattice):** `FnDescriptor` (`fn_descriptor.rs:18`) *is* the lattice;
its missing rung is `HostClosure` (Q2). 4A consumes LOCK-7's verdict on whether an arbitrary
host closure earns a serializable descriptor (a bounded opaque barrier with a typed signature)
or is disallowed on the analyzable path (selective-only, Parsley's constraint). No attempt to
settle either here.

---

## 6. Named risks & counterexamples

- **R1 — the monadic fragment (`chain`).** parse-that ships `chain` (`parser.ts:124`,
  C-16-fixed) taking a runtime closure that *begets* a parser — inherently dynamic, unanalyzable.
  Counterexample to "all combinators reify": any grammar using `chain` for context-sensitivity
  (bibtex, latex-paper's `commands.ts`) has a node the graph cannot expand. Guard: express as
  selective `branch`/`TokenDispatch` where possible; else the `chain` node is a labeled opaque
  barrier counted against the ≤2% floor (Q3).
- **R2 — Q1 category error.** A prototype that "monomorphizes the runtime graph" is either
  secretly a build-time codegen step (fine, but re-label it) or unsound. Guard: the Q1 prototype
  must state which of {const-graph, serialize-then-offline, interpret} it exercises.
- **R3 — value-plane opacity blows the barrier budget.** If most `.map` sites on the parse-that
  side are non-MapExpr closures (plausible — parse-that is closure-first), the barrier ratio
  exceeds ≤2% and the "analyzable graph" claim weakens to "analyzable skeleton with opaque
  leaves." Guard: Q2/Q3 census before any fusion claim.
- **R4 — VM setup tax swamps the win on short inputs.** The 10-allocation `Interpreter::new`
  can erase a recognizer speedup on stop-early/short parses. Guard: Q7 pooling must land in the
  same prototype as any interpret-path perf claim; warm benches forbidden (SPEC §0), cold
  per-parse only.
- **R5 — TS shape churn.** A `Node` POJO with a polymorphic `value` field re-introduces the
  hidden-class transitions PROVE-11 fought (`ParserState` lazy fields). Guard: the `Node` tag
  must be a fixed-from-birth field; the interpreter's `switch(tag)` replaces the megamorphic
  `this.parser(state)` IC (R4 §6c) — prove same-map stability with `%HaveSameMap` (PT-3.W4 shape
  oracle).
- **R6 — isomorphism drift.** Rust and TS node enums diverge silently. Guard: the Q6 dual oracle
  (opcode-stream + Value-tree identity) against a *live* Rust golden, CI-run; a shared serialized
  fixture, never a hand-faked blob.

---

## 7. What a prototype must test to move family 4A

1. **Graph-of-record parity (Q1/Q5):** build a small grammar's parser via *runtime* smart-
   constructor combinators (parse-that scratch copy, R3 §8), serialize the `Node` graph, feed it
   to the bbnf `vm/compiler` and to the VM interpreter; assert byte-identical parses vs the
   grammar-text → IrNode path over json/csv. Establishes the runtime graph ≡ comptime graph.
2. **Barrier census (Q2/Q3):** build the combinator graph over value.js, latex-paper, keyframes
   real usage; count MapExpr-expressible vs opaque `.map`/`chain` sites; report the ratio vs ≤2%
   and the non-projection typed shapes → the `HostClosure` signature.
3. **Fusion firing numbers (Q4):** measure code-size + compile-time (Rust `.rlib`, TS `dist`)
   growth vs fuse-arity on CSS and value.js; fix `fuse-arity`, `dispatch-fanout`, `boxing-depth`
   caps against the generated-size budget.
4. **Setup-tax split (Q7):** instrument the VM interpret path — recognizer vs value-plane vs the
   10-allocation setup; show the amortization curve as `Interpreter` is pooled/reset-reused;
   cold per-parse, N≥200, paired.
5. **Isomorphism oracle (Q6):** the dual oracle (opcode-stream identity + Value-tree identity)
   Rust↔TS over the shared serialized graph, on json/css/csv, wired as a differential CI gate.

Sources: [Staged Selective Parser Combinators (Willis et al., ICFP 2020)](https://mpickering.github.io/papers/parsley-icfp.pdf) ·
[A Typed, Algebraic Approach to Parsing (Krishnaswami & Yallop, PLDI 2019)](https://www.cl.cam.ac.uk/~jdy22/papers/a-typed-algebraic-approach-to-parsing.pdf) ·
[ParsleyHaskell](https://github.com/j-mie6/ParsleyHaskell) · [ocaml-asp](https://github.com/yallop/ocaml-asp)
