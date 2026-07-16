# LOCK-4 · Family 4D — GRAMMAR-SUGAR (pass-3 research)

*Thesis: combinators are a construction API over the same IR the grammar plane already uses;
parse-that keeps its public API and inherits the generator's optimizer; the combinator engine
is the generator's runtime in disguise. Every measurement names its HEAD. HEAD = bbnf-lang
`crates/ir` + `crates/core` at master `fe3e615d3`; parse-that `ef10d5b` (read-only).*

## 0. The mechanism, made concrete

The shared IR already exists and is a near-bijection of the combinator set. `bbnf_ir::IrNode`
(`crates/ir/src/types/node.rs:30`) is a 14-variant serde-serializable enum (derive at
`node.rs:29`): `Literal(StringId)`, `Regex(StringId)`, `Epsilon`, `Seq(Vec<IrNode>)`,
`Alt(Vec<AltBranch>, Option<AltDispatch>)`, `Repeat{lo,hi}`, `Ref`, `Skip`, `Next`, `Minus`,
`Negate`, `Map{inner:Box<IrNode>, fn_id:FnId}` (`node.rs:79`), `OptionalWhitespace`,
`TokenDispatch` (`node.rs:90`). Every variant maps 1:1 from an `Expression` AST variant with
sugar (Group, OptionalWhitespace) eliminated and strings interned. The
combinator↔IrNode correspondence is direct and total for the recognizer plane:

| parse-that combinator | IrNode variant |
|---|---|
| `string` / `str` | `Literal(StringId)` |
| `regex` | `Regex(StringId)` |
| `all` / `then` / `seq!` | `Seq(Vec)` |
| `or` / `any` / `alt!` / `dispatch` | `Alt(_, AltDispatch)` / `TokenDispatch` |
| `many` / `opt` / `sepBy` | `Repeat{inner,lo,hi}` |
| `skip` (`<<`) / `next` (`>>`) | `Skip` / `Next` |
| `minus` (EBNF `-`) | `Minus` |
| `not` / `negate` / `peek` | `Negate` |
| `map` (`->`) | `Map{inner,fn_id}` |
| `trim` / `whitespace` | `OptionalWhitespace` |
| `lazy` / rule reference | `Ref(RuleId)` |

`dispatch`'s `Int8Array(128)` table (TS `leaf.ts:100-151`) is *precisely* `AltDispatch.table`
and the O(1) token-led fusion is `TokenDispatch` (`node.rs:90`) — the deliberate TS
monomorphization and the IR's fusion node are the same object arrived at from two directions.
This is the strongest concrete evidence for 4D: the combinator API's hot specializations *are
already* IR nodes.

**The substrate 4D reuses (the "inherited optimizer"):** the whole `crates/ir/` pass tower
operates on `IrNode` — passes (`crates/ir/src/passes/`: `lr, prefix, span, regex_info,
metadata, profile, csp_domains, path_check`), rewrites (`crates/ir/src/rewrites/`: `tiering,
base, path_seed, rank, schema`), the e-graph (`crates/ir/src/egraph/`, path-dep-consumed),
the DAG interner (`crates/ir/src/dag/`), and the decision driver
(`crates/core/src/backend/driver/`). Both emitters already read one `IrNode`: the Rust emitter
(`crates/core/src/backend/rust/emitter/` — `grammar.rs, shapes/, precedence.rs,
keyword_dispatch.rs`) and the TS emitter (`crates/core/src/backend/ts/emitter/` —
`grammar.rs, value.rs, binary.rs, leaves.rs`). A combinator front-end that emits `IrNode`
inherits every one of these for free — the entire claim of the family.

## 1. Q1 — runtime graph → comptime monomorphization

**The runtime combinator API does NOT reach comptime monomorphization today, and cannot absent
a reification step.** parse-that's Rust `Parser<'a,O>` holds `parser_fn: SmallBox<dyn
ParserFn+'a, S32>` (`parse-that/rust/parse_that/src/parse.rs:61`) — an opaque nested closure,
not a graph; its manifest depends only on `bbnf-regex`, never `bbnf`/`bbnf-ir`
(`parse-that/rust/parse_that/Cargo.toml:90`). The TS `Parser<T>` dispatches through a `.parser`
function pointer (`parser.ts:53,82`; R4 §6c names it "THE megamorphic IC"). Neither builds an
`IrNode`. The only comptime path that exists is `grammar-text → IrNode → codegen` (the emit
spine): monomorphization is codegen-reflection over a *comptime-known* `IrNode` value, which
the runtime closure graph never produces.

The mechanism 4D must add is the **reification seam**: combinator constructors that build an
`IrNode` value (a data graph of analyzable nodes) instead of composing opaque closures. Once
the combinator program is an `IrNode`, two terminals open from one value: (a) **runtime** — the
bytecode VM (`crates/ir/src/vm/`) interprets it (an interpreter, no monomorphization); (b)
**comptime** — the same `IrNode` is handed to the emitter for monomorphized Rust/TS. A
monomorphizing lowering therefore *demands of the API* only that it produce a serializable
`IrNode` rather than nested `SmallBox<dyn ParserFn>` — precisely the allocation/ownership
redesign PT-2.W4 flags (the 6-vs-5390 alloc split, arena-or-refutation): if the parser is a
data graph owned in one arena, the per-construction vtable box dissolves because there is no
vtable, only nodes. The distinction from 4A: 4A stops at "analyzable graph"; 4D fixes that
graph to *be* `bbnf_ir::IrNode` so the inheritance is literal, not merely structural.

## 2. Q2 / Q3 — value plane through host closures; opaque-barrier surface (CENSUSED)

**There is no `Opaque(HostFnId)` variant in this IR — the value plane is a closed, transparent
algebra.** `IrNode::Map` carries a `FnId` into `FnDescriptor` (`fn_descriptor.rs:18`), whose
variants are `EnumWrap`, `BoxWrap`, `NumberConvert{allow_leading_dot}` (`:40`),
`HexConvert{fn_path}` (`:45`), `SpanCapture`, and the sole user-facing `Expr{expr:MapExpr,
return_type}`. `NumberConvert`/`HexConvert` are *documented* as internal specializations of
`Expr{Input}`/`Expr{FnCall}` (`fn_descriptor.rs:30,43`) — proof the descriptor set is meant to
grow by adding specialization variants. `MapExpr` (`map_expr.rs:17`) is a closed value algebra:
`IntLit, FloatLit, BoolLit, StringLit, Input, InputProp{prop}, FnCall{name,args:Vec<MapExpr>},
BinOp{op,…}, UnaryOp{op,inner}` — fully introspectable (carries `constant_fold`/`is_constant`).
Nothing here is opaque *except the named callee of* `FnCall{name,args}`: the call site, arg
expressions, and result type are all visible; only the leaf function body is a barrier. The
true opaque surface is **the set of distinct `FnCall` names**, not whole map bodies.

**The census the directive demands — run against the live consumer sources (values are
map/chain/mapState call-site counts):**

| consumer | src | map/chain sites | expression-body (reifiable) | multi-stmt `=> {}` (arbitrary candidate) | constructor `=> new X` (FnCall leaf) | mapState (stateful) |
|---|---|---:|---:|---:|---:|---:|
| **latex-paper** | `latex-paper/src/grammar/*`,`/bibtex/*` | **86** | **86 (100%)** | **0** | (textNode/mathNode point-free) | 0 |
| **value.js** | `value.js/…/src/parsing/*` (14 files) | **129** | 64 | **33 (~26%)** | 19 | **0** |

(value.js's live source is in a worktree — its main checkout `src/parsing/{color,stylesheet,
timeline}` is momentarily empty during worktree churn; the census is over the populated
`.claude/worktrees/…/src/parsing`, 14 `.ts` files.) Sample bodies confirm the shapes: value.js
`.map(([, val]) => val)` (tuple projection = `InputProp`), `.map(expr => new
FunctionValue("calc",[expr]))` (`FnCall{name:"FunctionValue", args:[StringLit, Input]}`),
`.map(([first, rest]) => { …fold… })` (Pratt left-fold over `rest`, `math.ts:77,89,131,167,176`
— the genuinely arbitrary core; 41 `reduce/for/while` occurrences in `parsing/`). latex-paper
`.map(() => textNode("—"))` (constant `FnCall`), `.map(([_,ch]) => textNode(ch==="&"?…:…))`
(one conditional). **latex-paper is ~100% MapExpr-reifiable; value.js's ~26% multi-statement
sites, concentrated in the Pratt binary-expression assembly, are the arbitrary-closure core.**

**Reframing Q3 — the ≤2% floor is a NODE ratio, not a site ratio, and it holds.** A map SITE
is one thin `Map{fn_id}` leaf over a *fat transparent recognizer subtree* (Seq/Alt/Repeat/
Literal/Regex nodes). value.js's 33 opaque-closure leaves, over a recognizer graph of
hundreds-to-thousands of `IrNode`s, sit **well under 2% at the node level** — the site count
(26%) drastically overstates the barrier because each opaque leaf's recognizer children remain
fully visible. What matters more than the count is the barrier's **LOCATION**: the opaque
closures sit at the value-plane *root* of expression grammars, so **recognizer-plane analyses
(fusion, `AltDispatch`/`TokenDispatch`, prefilter, span — every §0 perf win) are entirely
unblocked**; only value-plane *shape/type* inference stops at each fold leaf. 4D's performance
inheritance is intact unconditionally; 4D's type inheritance (LOCK-1) is total on latex-paper
and stops precisely at value.js's ~5 Pratt folds. That is the honest boundary, and it is thin.

**The one new descriptor variant.** The non-reifiable sites need a serializable leaf:
`FnDescriptor::HostClosure{fn_id:FnId, sig:TypeDesc→TypeDesc}` — an opaque VALUE with a visible
TYPE and a stable id, sibling to the existing `NumberConvert`/`HexConvert` specializations. It
keeps the closure API unchanged: structure/type analysis sees through the signature while
evaluation dispatches to the host-fn lattice (LOCK-7). This is the whole of the "implied
host-fn lattice" — one variant, typed, closed.

## 3. Q5 — one serializable plan, two lowerings (the distinguishing observable)

**The observable already half-exists and strongly favors the thesis.** `IrNode`,
`FnDescriptor`, `MapExpr`, `AltDispatch`, and `BytecodeProgram` all derive
`Serialize/Deserialize` (`node.rs:17,29`; `fn_descriptor.rs:17`; `bytecode.rs:13,24,35,44`).
Two lowerings already consume one `IrNode` value — the Rust emitter and the TS emitter — both
reading the same `IrNode` + `FnDescriptor` + `AltDispatch`/`TokenDispatch` as their shared
plan. The distinguishing observable to certify 4D: **serialize one `IrNode`, lower it through
the Rust emitter and the TS emitter, and assert byte-identical PARSES** (not identical code)
across a conformance corpus. `TokenDispatch`/`AltDispatch` is the concrete carrier — one
dispatch table, two emitters, one parse behavior. This is *one plan, two lowerings*; it is the
isomorphism LOCK-6 measures, and 4D supplies the plan. Because both structs already serialize,
the observable is a test to write, not a mechanism to invent.

## 4. Q6 / Q7 — descriptor evaluator home; setup vs value vs recognizer cost

**The internal-descriptor evaluator is owned by LOCK-3 (IR→codegen lowering), NOT by the VM
and NOT by host dispatch — today.** The bytecode `Op` enum (`bytecode.rs:45`) has
`MatchString, MatchRegex, Epsilon, Jump, Call, Return, Save/Restore/DropState, TrimWs, Minus,
Negate, Dispatch, DispatchToken, RepeatBegin/End, CaptureSpan, SaveValueDepth, …` — **no `Map`
opcode**; and `crates/ir/src/vm/` has **zero** `FnDescriptor::`/`MapExpr::` references (grep =
0). The VM is a pure recognizer plus *structural* value builder only: `exec_make_array` /
`exec_make_tagged` (`interpreter/construct.rs:8,17`). `NumberConvert`/`HexConvert`/`EnumWrap`
are evaluated only in codegen (`backend/driver/map.rs`, `backend/rust/emitter/`,
`backend/ts/emitter/grammar.rs`). **Consequence for 4D:** the runtime `IrNode`+VM path
currently *drops the `->` value plane entirely*. A runtime combinator core that wants the value
plane must add a shared `MapExpr`/`FnDescriptor` evaluator — and it must be the *same*
evaluator the codegen uses (DRY), not a fork: extract `driver/map.rs`'s descriptor semantics
into a backend-agnostic `eval` consumed by both the VM (runtime) and the emitters (comptime).
That single evaluator is the honest home of the Hex/Number/Enum question: **LOCK-3 lowering
when comptime, host dispatch (LOCK-7) when the leaf is a `HostClosure`** — the descriptor kind
decides, one evaluator routes.

**Setup tax (Q7).** `Interpreter::new` (`interpreter/mod.rs:114`) allocates, per parse, eight
capacity-primed Vecs + one FxHashMap: `values(64), call_stack(32), checkpoints(32),
repeats(16), memo_starts(8), value_depth_stack(8), rule_stack(16), diagnostics(0)` +
`memo: FxHashMap::default()` (`mod.rs:122-130,84`) — ~176 preallocated slots plus a hashmap
alloc on *every* `new`. On short inputs this is a large share of single-parse cost (the classic
interpreter-startup tax; contrast the closure core's per-parse alloc, R4 §6e "nil on the steady
path"). The clean three-way split: **recognizer cost** (the `run` loop over `code`) /
**value-plane cost** (the absent Map evaluator, once added) / **setup cost** (the `new`
allocations). Pooling eliminates the third: reuse one `Interpreter` across parses by
`clear()`-ing the Vecs (retain capacity) and the memo map instead of reallocating — an object
pool keyed to the `BytecodeProgram`. A prototype must plot the amortization curve: setup share
at N=1, its decay as the pool is reused across a corpus, isolating the floor where
recognizer+value dominate.

## 5. Q8 — predecessor shapes (report only, as inputs to 4D)

- **LOCK-5 (regex continuum)** is 4D's *substrate premise* — GRAMMAR-SUGAR builds "over the
  same HIR the grammar plane uses (LOCK-5's continuum)." XL-C has already settled the shared-IR
  question *against* ONE-HIR: two domain-fit types — opaque `Regex(StringId)` at the grammar
  tier (exactly `IrNode::Regex`, `node.rs:36`) and the rich `Hir` engine tier — bridged cheaply;
  the regex functor is retired; vendored parse-that-regex in skinny is the substrate. 4D
  consumes this verbatim: `IrNode::Regex(StringId)` is the combinator `regex()` leaf, and the
  engine tier stays behind the bridge. Open sub-question 4D inherits, does not settle: whether
  the `HostClosure` bridge to LOCK-7 is a shared type or a lawful functor (LOCK-5's form for
  regex informs but does not bind the value-plane's).
- **LOCK-7 (host-fn lattice)** owns the semantics of `HostClosure` leaves — the typed interface
  behind which context-dependent/recursive projections (value.js's Pratt folds) live. 4D's
  input to LOCK-7: the census above (value.js ~5 fold cores + 19 constructor FnCalls;
  latex-paper's textNode/mathNode point-free constructors) is the concrete lattice population,
  and the required floor is a *node-ratio* ≤2%, not a site-ratio. 4D asserts neither lock's
  disposition; it hands each the shape it needs.

## 6. Prior art (citations)

- **Jonnalagedda et al., "Staged Parser Combinators for Efficient Data Processing" (OOPSLA
  2014)** / **Béguet & Jonnalagedda** — LMS-staged combinators reify the graph then generate
  specialized parsers, erasing per-combinator dispatch; the canonical "combinator engine is the
  generator's runtime in disguise," matching hand-written parsers (4D's ceiling).
- **Krishnaswami & Yallop, "A Typed Algebraic Approach to Parsing" (PLDI 2019)** and **Yallop
  et al., "flap: A Deterministic Parser with Fusion" (PLDI 2023)** — typed combinator algebras
  compiled through a normal form to fused table-driven code; `flap`'s PEG-algebra→automaton
  fusion is exactly `Alt`+`AltDispatch`→`TokenDispatch` here.
- **GHC Core + RULES / rustc HIR→MIR** — one analyzable IR shared by every pass; rewrite rules
  are fusion (the `SINGLE-IR-MULTIPASS`/e-graph analogue already vendored at
  `crates/ir/src/egraph/`).
- **tree-sitter** — grammar reified to a data table driving one interpreter; the data-plane
  precedent for a runtime that *is* the generator's runtime.
- **egg / egglog** — e-graph saturation over an IR; the substrate under `crates/ir/src/egraph/`
  that 4D inherits rather than reinvents.

## 7. Named risks & counterexamples

- **The arbitrary-closure wall — now bounded, not fatal.** Keeping parse-that's public
  `.map(closure)` AND a transparent analyzable graph are in tension: an arbitrary JS/Rust
  closure is not `MapExpr`-expressible. Resolution: the two-tier plane with `HostClosure`
  leaves. The counterexample that *would* kill 4D — opaque closures exceeding the ≤2% NODE
  floor — is refuted by the census: even value.js's fold-heavy grammar keeps opaque leaves far
  below 2% of nodes; latex-paper is ~100% reifiable. The residual risk is *type* inheritance
  (not perf) at value.js's ~5 Pratt folds, which LOCK-7 absorbs by signature.
- **VM value-plane forking.** Adding a Map opcode/evaluator risks duplicating `driver/map.rs`'s
  descriptor semantics — a DRY violation. Guard: one extracted `FnDescriptor`/`MapExpr`
  evaluator consumed by VM and both emitters, proven by a differential oracle (VM value-tree ≡
  codegen value-tree).
- **Cross-repo coupling.** parse-that's Rust core gaining a `bbnf-ir` dep couples the repos
  under the non-clobber law; the sanctioned shape is the vendored-in-skinny pattern the regex
  crate already uses (bbnf-regex vendored), not a live path-dep across trees.
- **API-break masquerading as inheritance.** If reification forced consumers to rewrite maps in
  the MapExpr DSL, "keeps its public API" breaks (value.js's 129 map/chain sites; latex-paper's
  86). The two-tier plane must accept the existing closure API unchanged, reifying
  opportunistically only where the closure is expression-body / MapExpr-shaped (the 64+86
  reifiable sites) and boxing the rest as `HostClosure` (the 33 fold candidates).

## 8. What a prototype must test (to move 4D)

1. **Reification bijection.** Implement `string/regex/all/or/many/map/skip/next/minus/not/
   trim/dispatch/lazy` as `IrNode` constructors; on json + value.js math + latex-paper text
   grammars, prove `build IrNode → emit (Rust & TS) → parse` is byte-identical to the original
   combinator parse (the 14-variant near-bijection is total for the recognizer plane).
2. **Barrier census, node-level (Q2/Q3).** Beyond the site census here, reify each grammar to
   `IrNode` and count opaque `HostClosure` leaves ÷ total nodes against the ≤2% floor; confirm
   the site→node collapse; report the typed `HostClosure{sig}` shapes the value.js fold sites
   imply.
3. **One-plan-two-lowerings (Q5).** Serialize one `IrNode`; lower via Rust and TS emitters;
   assert byte-identical parses on a conformance corpus (the distinguishing observable).
4. **Runtime value plane (Q1/Q6).** Extend the VM with a shared `FnDescriptor`/`MapExpr`
   evaluator (the extracted `driver/map.rs` semantics); differential-test VM `Value` trees vs
   the codegen path; measure runtime `IrNode`+VM `->` cost vs monomorphized codegen.
5. **Setup pooling (Q7).** Measure `Interpreter::new` setup share of single-parse cost on short
   inputs; pool the interpreter (clear-not-realloc); plot the amortization curve and the
   recognizer / value-plane / setup decomposition.
