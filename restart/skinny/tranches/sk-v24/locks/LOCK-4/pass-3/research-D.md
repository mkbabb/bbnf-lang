# LOCK-4 · Family 4D — GRAMMAR-SUGAR (pass-3 research)

*Thesis: combinators are a construction API over the same IR the grammar plane already uses;
parse-that keeps its public API and inherits the generator's optimizer; the combinator engine
is the generator's runtime in disguise. Every measurement names its HEAD.*

## 0. The mechanism, made concrete

The shared IR already exists and is a near-bijection of the combinator set. `bbnf_ir::IrNode`
(`crates/ir/src/types/node.rs:30`) is a 14-variant serde-serializable enum:
`Literal, Regex, Epsilon, Seq(Vec), Alt(Vec<AltBranch>, Option<AltDispatch>), Repeat{lo,hi},
Ref, Skip, Next, Minus, Negate, Map{inner,fn_id}, OptionalWhitespace, TokenDispatch`. Every
variant maps 1:1 from an `Expression` AST variant with sugar (Group, OptionalWhitespace)
eliminated and strings interned (node.rs:26-27). The combinator-to-IrNode correspondence is
direct and total for the recognizer plane:

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
(node.rs:128-135, a 128-entry `Vec<u8>`) and the O(1) token-led fusion is `TokenDispatch`
(node.rs:90-97) — the deliberate TS monomorphization and the IR's fusion node are the same
object arrived at from two directions. This is the strongest concrete evidence for 4D: the
combinator API's hot specializations *are already* IR nodes.

**The substrate 4D reuses (the "inherited optimizer"):** the whole `crates/ir/` pass tower
operates on `IrNode` — passes (`crates/ir/src/passes/`: `lr, prefix, span, regex_info,
metadata, profile, csp_domains, path_check`), rewrites (`crates/ir/src/rewrites/`: `tiering,
base, path_seed, rank, schema`), the e-graph (`crates/ir/src/egraph/`, path-dep-consumed),
the DAG interner (`crates/ir/src/dag/`), and the decision driver
(`crates/core/src/backend/driver/mod.rs:347` + `alt/seq/map/wrap/repeat/reference/analysis`).
A combinator front-end that emits `IrNode` inherits every one of these for free — which is
the entire claim of the family.

## 1. Q1 — runtime graph → comptime monomorphization

**The runtime combinator API does NOT reach comptime monomorphization today, and cannot absent
a reification step.** parse-that's Rust `Parser<'a,O>` holds `parser_fn: SmallBox<dyn
ParserFn+'a, S32>` (`parse.rs:61`) — an opaque nested closure, not a graph; its Cargo manifest
depends only on `bbnf-regex`, never `bbnf`/`bbnf-ir`
(`parse-that/rust/parse_that/Cargo.toml:90`). The TS `Parser<T>` dispatches through a
`.parser` function pointer (`parser.ts:53,82`; R4 §6c names it "THE megamorphic IC"). Neither
builds a `IrNode`. The only comptime path that exists is `grammar-text → IrNode → codegen`
(the emit spine): monomorphization is codegen-reflection over a *comptime-known* IrNode value,
which the runtime closure graph never produces.

The mechanism 4D must add is the **reification seam**: combinator constructors that build an
`IrNode` value (a data graph of analyzable nodes) instead of composing opaque closures — this
is the 4A "closure-to-graph" prerequisite that 4D subsumes and makes *specifically* the
generator's IR. Once the combinator program is a `IrNode`, two terminals open from one value:
(a) **runtime** — the bytecode VM (`crates/ir/src/vm/`) interprets it (no monomorphization, an
interpreter); (b) **comptime** — the same IrNode is handed to the emitter for monomorphized
Rust/TS. A monomorphizing lowering therefore *demands of the API* only that it produce a
serializable IrNode rather than nested `SmallBox<dyn ParserFn>` — the demand is exactly the
allocation/ownership redesign PT-2.W4 flags (the 6-vs-5390 alloc split, arena-or-refutation):
if the parser is a data graph owned in one arena, the per-construction vtable box dissolves
because there is no vtable, only nodes.

## 2. Q2 / Q3 — value plane through host closures; opaque-barrier surface

**There is no `Opaque(HostFnId)` variant in this IR — the value plane is a closed, transparent
algebra, and the barrier is smaller than the directive fears.** `IrNode::Map` carries a `FnId`
into `FnDescriptor` (`fn_descriptor.rs:18`). The single user-facing variant is
`Expr { expr: MapExpr, return_type }`; the rest (`EnumWrap, BoxWrap, NumberConvert,
HexConvert, SpanCapture`) are compiler-produced specializations. `MapExpr`
(`map_expr.rs:17`) is a *closed* value algebra — `IntLit, FloatLit, BoolLit, StringLit, Input,
InputProp{prop}, FnCall{name,args}, BinOp, UnaryOp` — fully introspectable: it carries its own
`constant_fold` (map_expr.rs:109) and `is_constant` (map_expr.rs:165). Nothing here is opaque
*except the named callee of* `FnCall{name, args}`: the call site, arg expressions, and result
type are all visible; only the leaf function body is a barrier. So the true opaque surface is
**the set of distinct `FnCall` names**, not whole map bodies — an analysis sees through every
`->` that is arithmetic, projection, wrap, or literal, and stops only at a named host call.

The census the directive asks for cannot be run against `.bbnf` grammars for these consumers,
because value.js / latex-paper / keyframes author their maps as **arbitrary JS closures** in
the runtime TS combinator API (`.map(v => …)`), not as `->` DSL. That is the crux of 4D's
tension: an arbitrary JS closure is **not** MapExpr-expressible, and `FnDescriptor` has no
`HostClosure` variant to hold one. Bringing a consumer under `IrNode` therefore forces a
choice per `.map` site:
- **MapExpr-expressible** (a projection / arithmetic / wrap — e.g. value.js's numeric and
  color assembly, latex-paper's token wrapping): reify into `MapExpr` — transparent, fusible,
  zero barrier. This is the majority and is the win.
- **Arbitrary closure** (context-dependent, stateful, recursive): must become a declared
  leaf. The serializable descriptor a `HostClosure` needs is a typed shape:
  `HostClosure { fn_id: FnId, sig: TypeDesc→TypeDesc }` — an opaque VALUE with a visible
  TYPE and a stable id, so structure/type analysis still sees through it while evaluation
  dispatches to the host-fn lattice (LOCK-7's `HOST-FUNCTION-LATTICE`). This is the one new
  descriptor variant 4D introduces; it is the whole of the "implied host-fn lattice."

**The barrier ratio (Q3) is the family's viability gate.** A prototype must count, per consumer
grammar, `#{MapExpr-reifiable maps} : #{arbitrary-closure maps}`, and prove the opaque leaves
sit ≤2% of nodes. If arbitrary closures dominate value.js's timeline/easing/scroll surfaces,
the transparent-IR claim collapses into "a graph of opaque leaves" and 4D reduces to 4A.

## 3. Q5 — one serializable plan, two lowerings (the distinguishing observable)

**The observable already half-exists and strongly favors the thesis.** `IrNode`, `FnDescriptor`,
`MapExpr`, `AltDispatch`, and `BytecodeProgram` are *all* `#[derive(Serialize, Deserialize)]`.
Two lowerings already consume one IrNode value: the Rust emitter
(`crates/core/src/backend/rust/emitter/`) and the TS emitter
(`crates/core/src/backend/ts/emitter/grammar.rs`) — both read the same `IrNode` +
`FnDescriptor`, both read `AltDispatch`/`TokenDispatch` as their shared "plan." The
distinguishing observable to certify 4D is: **serialize one IrNode, lower it through the Rust
emitter and the TS emitter, and assert byte-identical PARSES** (not identical code) across a
conformance corpus. TokenDispatch/AltDispatch is the concrete carrier — one dispatch table,
two emitters, one parse behavior. This is *one plan, two lowerings*, and it is the isomorphism
LOCK-6 measures; 4D supplies the plan.

## 4. Q6 / Q7 — descriptor evaluator home; setup vs value vs recognizer cost

**The internal-descriptor evaluator is owned by LOCK-3 (IR→codegen lowering), NOT by the VM
and NOT by host dispatch.** Grep is decisive: `NumberConvert / HexConvert / EnumWrap` are
evaluated only in codegen — `crates/core/src/backend/driver/map.rs`,
`backend/rust/emitter/mod.rs`, `.../shapes/hregex.rs`, `.../shapes/flat/mod.rs`, and
`backend/ts/emitter/grammar.rs`. The bytecode VM evaluates **none** of them: the `Op` enum
(`crates/ir/src/vm/bytecode.rs:48`) has `MatchString/MatchRegex/Call/…` but **no `Map`
opcode**, and `crates/ir/src/vm/interpreter/` has zero `FnDescriptor::`/`MapExpr::` sites — the
VM is a pure recognizer plus structural value builder (`exec_make_array`, `exec_make_tagged` in
`interpreter/construct.rs`). **Consequence for 4D:** the runtime IrNode+VM path currently
*drops the `->` value plane entirely*. A runtime combinator core that wants the value plane
must add a shared `MapExpr`/`FnDescriptor` evaluator — and it must be the *same* evaluator the
codegen uses (DRY), not a fork: extract `driver/map.rs`'s descriptor semantics into a
backend-agnostic `eval` consumed by both the VM (runtime) and the emitters (comptime). That
single evaluator is the honest home of the Hex/Number/Enum question: **LOCK-3 lowering when
comptime, host dispatch (LOCK-7) when the leaf is a `HostClosure`** — the descriptor kind
decides, one evaluator routes.

**Setup tax (Q7).** `Interpreter::new` (`crates/ir/src/vm/interpreter/mod.rs:114`) allocates,
per parse, eight capacity-primed Vecs + one FxHashMap: `values(64), call_stack(32),
checkpoints(32), repeats(16), memo_starts(8), value_depth_stack(8), rule_stack(16),
diagnostics(0)` + `memo: FxHashMap::default()` — ~176 preallocated slots plus a hashmap
allocation on *every* `Interpreter::new`. On short inputs this setup is a large share of
single-parse cost (the classic interpreter-startup tax). The clean split is
**recognizer cost** (the `run` loop over `code`) / **value-plane cost** (the absent Map
evaluator, once added) / **setup cost** (the `new` allocations). Pooling eliminates the third:
reuse one `Interpreter` across parses by `clear()`-ing the Vecs (retain capacity) and the
memo map instead of reallocating — an object pool keyed to the `BytecodeProgram`. A prototype
must plot the amortization curve: setup share at N=1 parse, and its decay as the pool is
reused across a corpus, isolating the floor where recognizer+value dominate.

## 5. Prior art (citations)

- **Jonnalagedda et al., "Staged Parser Combinators for Efficient Data Processing"
  (OOPSLA 2014)** and **Béguet/Jonnalagedda** — LMS-staged combinators that reify the
  combinator graph then generate specialized parsers, erasing per-combinator dispatch. This is
  the canonical form of "the combinator engine is the generator's runtime in disguise"
  (4D/4B). Their result: staged combinators match hand-written parsers — the ceiling 4D aims at.
- **Krishnaswami & Yallop, "A Typed Algebraic Approach to Parsing" (PLDI 2019)** and
  **Yallop et al., "flap: A Deterministic Parser with Fusion" (PLDI 2023)** — typed combinator
  algebras compiled through a normal form to fused, table-driven code; `flap`'s fusion of a
  PEG-like algebra into a deterministic automaton is exactly `Alt`+`AltDispatch`→`TokenDispatch`
  here.
- **GHC Core + RULES / rustc HIR→MIR** — one analyzable IR shared by every pass; rewrite rules
  are fusion (the `SINGLE-IR-MULTIPASS`/`egraph` analogue already vendored at
  `crates/ir/src/egraph/`).
- **tree-sitter** — grammar reified to a data table driving one interpreter; the data-plane
  precedent for a runtime that is "the generator's runtime."
- **egg / egglog** — e-graph saturation over an IR; already the substrate under
  `crates/ir/src/egraph/` that 4D inherits rather than reinvents.

## 6. Named risks & counterexamples

- **The arbitrary-closure wall.** Keeping parse-that's public `.map(closure)` API AND getting a
  fully transparent analyzable graph are in tension: you cannot reify an arbitrary JS/Rust
  closure into `MapExpr`. Resolution is the two-tier value plane (§2) with `HostClosure` leaves;
  the counterexample that kills 4D is a consumer where opaque closures exceed the ≤2% barrier
  (Q3) — then the "inherited optimizer" sees only opaque leaves and 4D degenerates to 4A.
- **VM value-plane forking.** Adding a Map opcode/evaluator to the VM risks duplicating
  `driver/map.rs`'s descriptor semantics — a DRY violation the constitution forbids. Guard: one
  extracted `FnDescriptor`/`MapExpr` evaluator consumed by VM and both emitters, proven by a
  differential oracle (VM value-tree ≡ codegen value-tree).
- **Cross-repo coupling.** parse-that's Rust core gaining a `bbnf-ir` dep couples the repos
  under the non-clobber law; the sanctioned shape is the vendored-in-skinny pattern the regex
  crate already uses (bbnf-regex vendored), not a live path-dep across trees.
- **API-break masquerading as inheritance.** If reification forces consumers to rewrite maps in
  the MapExpr DSL, the "keeps its public API" promise is broken — a semver counterexample
  (value.js's 59 `all()` + `.map` sites). The two-tier plane must accept the existing closure
  API unchanged, reifying opportunistically only where the closure is MapExpr-shaped.

## 7. What a prototype must test (to move 4D)

1. **Reification bijection.** Implement `string/regex/all/or/many/map/skip/next/minus/not/
   trim/dispatch/lazy` as `IrNode` constructors; on the json + value.js math grammar, prove
   `build IrNode → emit (Rust & TS) → parse` is byte-identical to the original combinator parse
   (the 14-variant near-bijection is total for the recognizer plane).
2. **Barrier census (Q2/Q3).** Over value.js (math/units/color/timeline) + latex-paper
   grammar + keyframes, classify every `.map` site MapExpr-reifiable vs arbitrary-closure;
   report the opaque-leaf node ratio against the ≤2% floor and the typed `HostClosure` shapes
   the non-reifiable sites imply.
3. **One-plan-two-lowerings (Q5).** Serialize one IrNode; lower via Rust and TS emitters;
   assert byte-identical parses on a conformance corpus (the distinguishing observable).
4. **Runtime value plane (Q1/Q6).** Extend the VM with a shared `FnDescriptor`/`MapExpr`
   evaluator; differential-test VM `Value` trees against the codegen path; measure runtime
   IrNode+VM `->` cost vs monomorphized codegen (the runtime-vs-comptime split).
5. **Setup pooling (Q7).** Measure `Interpreter::new` setup share of single-parse cost; pool
   the interpreter (clear-not-realloc); plot the amortization curve and the recognizer /
   value-plane / setup decomposition.
