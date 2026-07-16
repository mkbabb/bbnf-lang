# LOCK-4 · Family 4D — GRAMMAR-SUGAR · pass-1 research

*The combinator API as a construction front over the bbnf IR; parse-that keeps its public
surface and inherits the generator's optimizer and runtime.*

---

## 0. The family thesis, made falsifiable

4D asserts one identity: **a parse-that combinator expression IS a `GrammarIR` under
construction.** When a user writes `a.then(b).or(c).many()`, the combinator methods do not
close over `move |state| {…}`; they build `bbnf_ir::IrNode` nodes — the exact same IR the
grammar plane (`.bbnf` text → typed HIR → mid-IR) produces. The constructed IR then flows
through the *same* analysis passes (the "optimizer") and executes on the *same* runtime
(interpreter, bytecode VM, or native codegen) that the generator uses. parse-that's public
method names survive unchanged (a hard constraint: PT-3 §1.1 binds bbnf to consume the TS
backbone "unmodified" and PT-2 forbids gratuitous surface churn); only the *implementation*
of each method changes from shallow-closure to deep-IR. The distinguishing claim over a
private analyzable graph is that the graph is **not parse-that-private** — it is `bbnf_ir`,
so isomorphism (rust/ts) and the optimizer come *for free* rather than being rebuilt.

This is the deep-vs-shallow embedding dichotomy of DSLs made concrete (Gibbons & Wu,
*Folding Domain-Specific Languages: Deep and Shallow Embeddings*, ICFP 2014): today's
parse-that is a **shallow** embedding (each combinator = a function that immediately does
the work); 4D converts it to a **deep** embedding (each combinator = a constructor of an
inspectable AST that a separate interpreter/compiler consumes).

---

## 1. The substrate — the near-bijection already exists

The construction target is `bbnf_ir::IrNode` at `crates/ir/src/types/node.rs:30`. Its
variant set is, module leaf naming, the parse-that combinator surface:

| `IrNode` variant (node.rs) | parse-that Rust (`combinators/methods/`) | parse-that TS (`parser.ts`/`leaf.ts`) |
|---|---|---|
| `Literal(StringId)` :33 | `string()` `leaf.rs` | `string` `leaf.ts:276` |
| `Regex(StringId)` :36 | `regex()` | `regex` `leaf.ts:317` |
| `Epsilon` :39 | ε (opt seed) | ε |
| `Seq(Vec<IrNode>)` :43 | `then`/`all`/`seq!` `sequence.rs` | `then` :81 / `all`→`fuseAll` `leaf.ts:179` |
| `Alt(Vec<AltBranch>, Option<AltDispatch>)` :48 | `or`/`any`/`alt!` `alternation.rs` | `or` :105 / `any` `leaf.ts:28` |
| `Repeat{inner,lo,hi}` :54 | `many`/`opt`/`sep_by` `repetition.rs`,`sep_by.rs` | `many` :523 / `opt` :234 / `sepBy` :569 |
| `Ref(RuleId)` :61 | named-rule ref / `lazy()` | `lazy` `lazy.ts` |
| `Skip(a,b)` :65 | `.skip()` | `skip` :189 |
| `Next(a,b)` :68 | `.next()` | `next` :212 |
| `Minus(a,b)` :71 | `.minus()` `minus.rs` (EBNF `-`) | `minus` :309 |
| `Negate(inner)` :75 | `.not()`/`.negate()` | `not`/`negate` :251 |
| `Map{inner,fn_id}` :79 | `.map()` `map.rs` | `map` :146 |
| `OptionalWhitespace(inner)` :84 | trim / `whitespace` | `trim` :480 / `whitespace` `leaf.ts:395` |
| `TokenDispatch{…}` :90 | (dispatch specialization) | `dispatch` `leaf.ts:100` |

The near-bijection is why 4D is not a rewrite but a *re-homing*: the combinator library is
already, structurally, an IR builder that happens to eagerly evaluate. `AltDispatch` and
`TokenDispatch` are populated by the dispatch analysis pass — meaning the very
first-char/LUT dispatch optimization parse-that hand-codes today (`leaf.ts:100-151`
`Int8Array(128)`; R4 §6d) is a *derived output* of the bbnf optimizer, not a hand artifact.
Inheriting the optimizer is thus a strict superset of what the closure core hand-tunes.

---

## 2. The mechanism, concretely

**Front (construction).** A thin builder module — Rust `parse_that::Parser` and TS
`Parser<T>` — whose every combinator method appends an `IrNode` instead of a closure. The
existing AST→IR lowering (`crates/core/src/lower/expression/mod.rs:157`, which already emits
`IrNode::Seq(parts)` etc.) is the proof that IrNode construction from a structured source is
a solved, in-tree operation; the combinator front is a *second front door* onto the same
node factory, skipping the `.bbnf` text parse. Interning (`StringId`, `crates/core/src/
lower/string_interner.rs`) and the fn table (`crates/core/src/lower/fn_table.rs`) are reused
verbatim.

**Middle (the inherited optimizer).** The constructed `GrammarIR` is run through the extant
IR passes at `crates/ir/src/passes/`: dispatch/FIRST-set facts (`passes/facts/`,
`facts/mod.rs:161` `alt_dispatch_admissible`), fusion (`passes/transform/fuse.rs`,
`transform/fuse_token/`), inlining/pruning (`transform/inline.rs`, `prune.rs`), the e-graph
canonicalization (`crates/ir/src/egraph/`, `build_egraph.rs`→`write_back.rs`) with its cost
model (`egraph/cost.rs`, `cost_config.rs`), and CSP strategy selection
(`passes/csp_strategy/`, `csp_domains.rs`). This is the literal content of "parse-that
inherits the optimizer": no new optimizer is built.

**Runtime (the generator's runtime in disguise).** Two lawful execution modes, both extant:
- *Interpret* — `crates/ir/src/vm/interpreter/mod.rs:373` `parse_with_ir(ir, input)`
  tree-walks the compiled program. This is the **runtime-combinator** mode: grammars built
  at runtime (the combinator use-case) execute immediately, no codegen.
- *Compile* — `crates/ir/src/vm/compiler/` lowers `IrNode`→`BytecodeProgram`
  (`vm/bytecode.rs`), and the full backend (`crates/core/src/backend/{rust,ts,wasm}/`)
  emits native code. This is the **build-time-combinator** mode: a combinator grammar known
  at build time becomes AOT-generated parser code — the combinator engine *is* the
  generator.

The two modes are the deep-embedding payoff: the same IR feeds an interpreter for dynamism
and a compiler for speed, chosen by whether the grammar is static.

**Isomorphism (rust/ts, constitution iii).** `GrammarIR` is "fully owned (no lifetimes),
serializable via MessagePack for WASM boundary transfer" (`crates/ir/src/lib.rs:6`). A
combinator grammar built in Rust serializes to the identical IR a TS front would build;
both drive the same interpreter semantics. rust/ts parity is therefore a property of a
shared data structure, not of hand-mirrored code — the strongest possible form of
constitution clause (iii).

---

## 3. The central tension — host actions vs a declarative IR (the honest wall)

parse-that's combinators admit **arbitrary host closures** that a declarative IR cannot
represent. `IrNode::Map{fn_id}` (`node.rs:79`) resolves through `FnDescriptor`
(`crates/ir/src/types/fn_descriptor.rs:18`), whose only user-facing variant is
`Expr{expr: MapExpr, return_type}` — a *transparent, analyzable* value-expression tree
(`crates/ir/src/types/map_expr.rs:17`: literals, `Input`, `InputProp`, `FnCall`, `BinOp`,
`UnaryOp`; constant-folded by passes, `map_expr.rs` `constant_fold`). There is **no opaque
runtime-closure variant.** But parse-that's `.map(|s| …)` (parser.ts:146), `.chain(fn)`
(:124), `.mapState(fn)` (:162), and `.recover(fn)` (:653) carry *general* user functions
that are not `MapExpr`-expressible. This is the family's load-bearing gap:

- **The gap.** To "keep its public API," 4D must add a `FnDescriptor::HostClosure(HostFnId)`
  escape hatch — an opaque slot into a runtime fn-table (Rust `Box<dyn Fn>`, TS function
  ref). This aligns with LOCK-7's HOST-FUNCTION-LATTICE and the standing hybrid-grammar-host
  law (grammar expresses leaf semantics via `->`; host functions carry
  context-dependent/recursive semantics behind typed signatures).
- **The poison risk (named counterexample).** An opaque `HostClosure` inside an `Alt` branch
  has an *unknown FIRST set*, so `alt_dispatch_admissible` (`facts/mod.rs:161`) returns
  false and the dispatch/LUT optimization *silently disables* for that alternation — a
  perf cliff exactly where the closure core is fastest. The escape hatch must be *scoped*:
  an opaque map on the *result* of a branch must not defeat FIRST analysis of the branch's
  *structure* (map is post-hoc; the inner node's FIRST is still knowable). This is precisely
  the applicative-vs-monadic split: `map`/`then`/`skip` are applicative (structure static,
  analyzable) while `chain`/`bind` are monadic (structure data-dependent, opaque). The prior
  art's answer is **selective functors** — Willis & Wu, *Staged Selective Parser
  Combinators* (ICFP 2020), and *Garnishing Parsec with Parsley* (2018): restrict the
  data-dependent surface to `branch`/`select` so the parser AST stays statically inspectable
  and compilable, reserving fully-opaque `bind` for a measured minority of sites. 4D should
  classify parse-that's methods into {applicative → transparent IR} vs {`chain`/`mapState`
  → `HostClosure`, analysis-opaque but structurally isolated}.

---

## 4. Prior art (citations)

- **Deep vs shallow embedding** — Gibbons & Wu, ICFP 2014. The theoretical frame: 4D is the
  shallow→deep conversion of parse-that. Deep buys analyzability + multiple interpreters
  (interpret *and* compile) from one construction API.
- **Staged parser combinators** — Jonnalagedda, Coppey, Stucki, Rompf, Odersky, *Staged
  Parser Combinators for Efficient Data Processing* (OOPSLA 2014, LMS/Scala). Demonstrates a
  deep-embedded combinator DSL that *generates specialized code*, ~2× over closure
  combinators — direct evidence that "inherit the codegen" is a real win, and the caution
  that the win comes from *staging/codegen*, not from the interpreter.
- **Parsley / selective combinators** — Willis & Wu (2018, ICFP 2020, JFP 2023). The closest
  prior art: combinators compiled to an analyzable AST then to optimized Java bytecode;
  selective functors preserve static structure through data-dependent choice. This is the
  blueprint for §3's host-closure classification and the applicative/selective/monadic
  tiering.
- **chumsky** (Rust) — a typed combinator tree with a separate execution engine; evidence
  that a deep combinator library is ergonomic in Rust without runtime graphs of closures.
- **MLIR dialect lowering** (Lattner et al., CGO 2021) — the "combinator dialect lowers into
  a shared IR that owns the passes" pattern; 4D is a combinator dialect over `bbnf_ir`.
- **GHC RULES / worker-wrapper** — the "one front-end inherits a shared optimizer" idiom.

---

## 5. Integration seams (path:line)

- `crates/ir/src/types/node.rs:30` — `IrNode`, the construction target (§1 table).
- `crates/ir/src/types/fn_descriptor.rs:18` — `FnDescriptor`; **add** `HostClosure` variant
  (§3). Note the crate re-exports this enum as `n` (`types/mod.rs`), a colocation-edict alias.
- `crates/ir/src/types/map_expr.rs:17` — `MapExpr`, the transparent map path (applicative).
- `crates/core/src/lower/expression/mod.rs:157` — extant AST→IrNode factory; the combinator
  front is a parallel producer onto the same nodes (reuses `string_interner.rs`,
  `fn_table.rs`).
- `crates/ir/src/vm/interpreter/mod.rs:373` `parse_with_ir` — the runtime-combinator engine.
- `crates/ir/src/vm/compiler/{mod,compound,node,rule}.rs` + `vm/bytecode.rs` — IrNode→VM
  (build-time compile mode).
- `crates/ir/src/passes/{facts,transform/fuse,transform/fuse_token,csp_strategy}/` +
  `crates/ir/src/egraph/` — the inherited optimizer.
- `crates/ir/src/lib.rs:6` — `GrammarIR` MessagePack serialization = the rust/ts iso bus.
- parse-that Rust front to preserve: `rust/parse_that/src/parse.rs:61` (`Parser<'a,O>`),
  `combinators/methods/{sequence,alternation,repetition,sep_by,minus,map,recover}.rs`.
- parse-that TS front to preserve: `typescript/src/parse/parser.ts:24` + `leaf.ts`
  (`then/or/many/sepBy/map/skip/next/opt/not/minus/dispatch/string/regex`). R4 §3 confirms TS
  has **no** IR/NFA/DFA today — a TS `GrammarIR` port or the WASM-core crossing (LOCK-6 6C) is
  required for the TS half; this is the largest 4D surface.

Consumer (constitution viii / Y.13): the combinator front's in-tree consumers are the extant
interpreter + backend and the JSON/CSS/CSV domain parsers with their ~400 Rust and 13 TS test
suites — no dormant substrate.

---

## 6. Named risks & counterexamples

1. **Interpreter regression vs the tuned closure core.** parse-that's shallow core is
   heavily monomorphized: arity-unrolled `fuseAll` (leaf.ts:179), sticky-regex zero-alloc
   `test()` (leaf.ts:344), `dispatch` LUT, `call()` `flags===0` fast path (R4 §6d);
   Rust-side `SmallBox<S32>` inline vtable + flag fast-path (`parse.rs:80`). A tree-walking
   `parse_with_ir` inserts an opcode-dispatch layer that unrolling deleted. **4D must
   pair-measure the interpreted-IR path against the closure core** (SPEC §0/09 law: paired
   interleaving mandatory even for "obvious" wins — the shim's 1.34–1.57× that evaporated on
   fresh paired windows is the standing lesson). The *compile* mode (§2) is where a genuine
   win is plausible; the *interpret* mode is the regression-risk lane and must not be sold as
   speed it cannot pair-prove.
2. **Host-closure analysis poison** (§3) — opaque `chain`/`mapState` defeating FIRST/dispatch
   on siblings. Counterexample: `dispatch()` over a bucket whose arms each `.chain(fn)`.
   Guard: applicative/selective tiering + scoped opacity (map-on-result ≠ structure-opaque).
3. **State-effecting combinators outside the IR vocabulary.** `peek` (parser.ts:339),
   `lookAhead` (:365), `recover` (:653), `mapState` mutate `ParserState` (offset,
   diagnostics) with effects `IrNode` does not model (it has only `Negate`/`Minus` for
   lookaround-ish negation). Either new IrNode variants (`Peek`/`LookAhead`) or `HostClosure`
   with state access — both expand the IR surface; the minimal lawful set must be measured,
   not guessed.
4. **TS has no IR** (R4 §3). 4D-in-TS = port `GrammarIR` + interpreter to TS *or* route
   through WASM-core (LOCK-6 6C, byte-exact tape crossing proven at 4.4× stop-early but
   ~0.91× full-walk — the full-materialization boundary is real). This is the family's
   biggest cost and its rust/ts-iso claim rides on it.
5. **Build-time vs runtime bifurcation.** The compile mode needs the grammar at build time;
   the interpret mode serves runtime grammars. 4D is honestly *two* runtimes behind one API;
   the value proposition (inherit codegen) is strongest only for the build-time subset.

---

## 7. What a prototype MUST test to move 4D

1. **Parity.** A minimal Rust combinator front building `GrammarIR` for the JSON grammar
   (`then/or/many/sepBy/map/string/regex/opt`), executed via `parse_with_ir`. Assert
   byte-identical parse trees against (a) the hand JSON parser and (b) parse-that's closure
   JSON parser (`rust/parse_that/benches/parse_that/combinator.rs` corpus). Falsifies "the
   near-bijection is real."
2. **The decisive number.** Paired-interleaving cold per-parse (SPEC §0/09 protocol, N≥200,
   two windows, opposite arm order) of the interpreted-IR path AND the compiled-IR path vs
   the closure core, on the six JSON corpora. The family lives or dies on: does inheriting
   the optimizer + compiling beat the tuned closures, and does the interpret mode stay within
   a stated no-regression floor? A refutation (interpret loses, compile only ties) is a
   valid, publishable close.
3. **Host-closure soundness.** Add `FnDescriptor::HostClosure`; route a `.chain(fn)` and a
   `.map(|s| custom)` through interpret; assert (a) correct results and (b) that dispatch/
   FIRST analysis of *sibling* branches still fires (the poison counterexample #2 does not
   trigger). Proves the escape hatch is scoped, not analysis-defeating.
4. **rust/ts iso.** Serialize the JSON combinator `GrammarIR` (MessagePack) and drive it from
   both the Rust interpreter and a minimal TS interpreter (or the WASM core); assert
   byte-identical trees on the JSON test vectors. Proves constitution (iii) is a data
   property, the family's strongest single claim.
