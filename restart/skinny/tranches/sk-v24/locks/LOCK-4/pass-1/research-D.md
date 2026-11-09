# LOCK-4 · Family 4D — GRAMMAR-SUGAR (the combinator engine is the generator's runtime in disguise)

*Research, pass 1. Combinators are a construction API over the same HIR/IR the grammar plane
uses; parse-that keeps its public API and inherits the optimizer.*

---

## 1. The thesis, stated mechanically

Parser combinators today are a **shallow embedding**: `string("a").then(regex(/\d/)).map(f)`
is a tower of closures that *directly does the parsing* — each combinator both denotes a grammar
fragment AND executes it, fused into one opaque `SmallBox<dyn ParserFn>` (Rust
`parse_that/src/parse.rs:61`) or one `ParserFunction<T>` (TS `parser.ts:14`). Nothing between the
public API and the byte loop is analyzable: no fusion across `then`, no prefilter derivation, no
cost-model kernel selection — the closure *is* the program.

4D converts the combinator library into a **deep embedding** (Gibbons & Wu, *Folding
Domain-Specific Languages*, ICFP 2014): each combinator constructor becomes a thin factory that
**builds a bbnf `IrNode`** instead of (only) a closure. The public surface — `Parser<T>`,
`string`, `regex`, `any`, `all`, `dispatch`, `.then`, `.map`, `.many`, `lazy` — is preserved
byte-for-byte in signature; internally each now denotes a node in the grammar plane's own IR.
Once a combinator program is an `IrNode`, it flows through the **already-built** bbnf pipeline —
the e-graph rewrites, the CSP strategy solver, the prefilter/recognizer derivation, the
residualizer — and is executed either by the shared VM interpreter (the flexible lane) or by
emitted specialized Rust+NEON / TS (the staged lane). parse-that inherits every SPEC §0 win
(prefilter 8.8–29.5×, CSS token kernel, residualized delimiter masks) **without hand-writing any
of it**, because the optimizer is the generator's, consumed not duplicated.

This is precisely the **first Futamura projection** (Futamura 1971): a parser-generator is a
specializer of a parsing interpreter with respect to a fixed grammar. bbnf's generator *is* that
specializer. Combinators-that-build-IR + the generator = staged combinators; "the combinator
engine is the generator's runtime in disguise" is the literal identity `parse ≡
run(specialize(interp, grammar))`.

## 2. The substrate ALREADY EXISTS (this family is 90% plumbed, not speculative)

The decisive fact, verified in-tree: bbnf's IR is a near-perfect 1:1 with parse-that's combinator
algebra, its runtime interpreter is written, its bytecode compiler is written, its recognizer
kernels are written, and **parse-that already path-depends on all of it** (R3 §1 `.cargo/config.toml`:
`bbnf -> ../../bbnf-lang/crates/core`, `bbnf-ir -> ../../bbnf-lang/crates/ir`, plus shared
`egraph`, `egraph-derive`, `csp-solver`). The types are linkable from parse-that today.

**`IrNode` (`crates/ir/src/types/node.rs:30`) vs the parse-that combinator surface:**

| parse-that (Rust `leaf.rs`/`combinators/methods/`; TS `leaf.ts`/`parser.ts`) | `IrNode` variant |
|---|---|
| `string(s)` / `str` — `leaf.ts:276` | `Literal(StringId)` |
| `regex(r)` — `leaf.ts:317` (TS native RegExp); Rust bbnf-regex `Hir` | `Regex(StringId)` |
| `eof` / empty | `Epsilon` |
| `all(a,b,…)` / `.then` / `seq!` — `leaf.ts:154`, `parser.ts:81` | `Seq(Vec<IrNode>)` |
| `any(a,b,…)` / `.or` / `alt!` — `leaf.ts:28`, `parser.ts:105` | `Alt(Vec<AltBranch>, Option<AltDispatch>)` |
| `.many` / `.opt` / `sepBy` — `parser.ts:523,234,569` | `Repeat{inner,lo,hi}` |
| `lazy(()=>rule)` / named recursion — `lazy.ts`, `parser.ts:702` | `Ref(RuleId)` |
| `.skip(b)` — `parser.ts:189` | `Skip(a,b)` |
| `.next(b)` — `parser.ts:212` | `Next(a,b)` |
| `.minus(b)` (EBNF `-`) — `parser.ts:309` | `Minus(a,b)` |
| `.not` / `.negate` — `parser.ts:251` | `Negate(inner)` |
| **`.map(f)`** — `map.rs:10` (`fn(Output)->Output2`), `parser.ts:146` | **`Map{inner, fn_id: FnId}`** |
| `.trim` / `whitespace` / `FLAG_TRIM_WS` — `parser.ts:480`, `parse.rs:86` | `OptionalWhitespace(inner)` |
| **`dispatch(...)`** O(1) `Int8Array` LUT — `leaf.ts:100-151` | **`TokenDispatch{token,arms,fallback}`** + `AltDispatch` |

The alignment is not coincidental — the IR was co-designed with the combinator vocabulary. Even
the *optimizations* pre-exist as first-class IR: `AltDispatch` (set "when all branches have
pairwise disjoint FIRST sets") is exactly the TS `dispatch` first-char table (`leaf.ts:100`);
`TokenDispatch` is the lexer-parser fusion parse-that hand-built as `dispatch`; `OptionalWhitespace`
is the `FLAG_TRIM_WS` fast-path.

**The runtime is written.** `crates/ir/src/vm/interpreter/mod.rs:373`
`pub fn parse_with_ir(ir: &GrammarIR, input: &str) -> ParseResult` — a ready-made interpreter over
the IR (the "generator's runtime"). `crates/ir/src/vm/compiler/mod.rs:31`
`pub fn compile(ir: &GrammarIR) -> BytecodeProgram` — IR→bytecode (the 4C-adjacent lane, but over
bbnf's *existing* bytecode, not a new one). `crates/codegen/` — IR→specialized Rust+NEON (the AOT
lane, carrying the SPEC §0 kernels). The runtime primitives combinators would call are already
named: `crates/ir/src/recognizer/mod.rs` — `LiteralRecognizer:86`, `RegexRecognizer:32`,
`TokenRecognizer:154`, `DispatchGroupRecognizer:201`, `DelimScanRecognizer:255` — the *same*
scanning kernels parse-that hand-wrote in `scanners.rs`/`scan/`.

**The host-fn lattice is written.** `.map(f)` closures need a home; it exists: `FnId = u32`
(`crates/ir/src/types/mod.rs:59`), `FnDescriptor` registry `GrammarIR.fns` (`grammar.rs:38`),
`HostFnDecl` (`crates/core/src/types.rs:81`), `FnTable` (`crates/core/src/lower/fn_table.rs:7`).
A `.map` becomes a `Map{inner, fn_id}` where `fn_id` indexes a registered host function — a
**typed opaque leaf** (the hybrid-grammar-host law, LOCK-7 7C). Note parse-that's base `map`
already takes a bare `fn(Output)->Output2` pointer (`map.rs:10`), trivially registerable; only
`map_with_ctx`/`map_with_state` carry true closures (the honest boundary, §7).

## 3. The mechanism, concretized

### 3.1 Dual interpretation preserves the public API (tagless-final)

The way to keep the API "byte-for-byte" while re-homing the denotation is the **tagless-final /
finally-tagless** technique (Carette, Kiselyov & Shan, *Finally Tagless, Partially Evaluated*,
JFP 2009): express the combinator vocabulary as an *interface* with more than one interpretation.
Rust: a `Combinator` trait (`fn literal(s)->Self; fn seq(Vec<Self>)->Self; …`) with two impls —
`RuntimeParser` (today's closure core, the dynamic/fallback lane) and `IrBuilder` (produces
`IrNode`). TS: the `Parser<T>` class holds an `IrNode` handle (a `#node` field) *alongside* the
closure; construction populates the node lazily. The consumer writes `string("a").then(b).map(f)`
unchanged; whether that yields a closure that runs or a node that gets specialized is an internal
policy. This is the mechanism by which "parse-that keeps its public API" is satisfied *by
construction*, not by adaptor.

Gibbons & Wu's theorem underwrites it: a shallow embedding is a fold over the deep embedding; the
`RuntimeParser` interpreter is recoverable from `IrBuilder` + `parse_with_ir`, so no behavior is
lost in the conversion — the shallow lane is a corollary of the deep one.

### 3.2 Two execution lanes over the built IR

- **Staged / AOT (the strong form).** A build-time step evaluates the combinator program to its
  `IrNode`, runs bbnf's generator, and emits specialized code. Rust: a proc-macro or `build.rs`
  captures the (statically-known) combinator expression → `GrammarIR` → `crates/codegen` → a
  monomorphic `parse` fn with the SPEC §0 NEON kernels inlined. TS: a **vite build-time fuser**
  (the PT-3 "build-time fuser emits specialized chains" flavor, but routed through the *shared* IR
  + bbnf codegen compiled to wasm, not a bespoke TS pass) emits a specialized TS/wasm parser. This
  is where parse-that *beats* its own closure core — it inherits prefilter derivation, residualized
  masks, and cost-model kernel selection.
- **Interpreted / JIT (the flexible form).** For grammars only known at runtime (combinators built
  from data), `parse_with_ir` runs the IR directly. The IR is still optimized once at
  grammar-construction time (e-graph fusion, `AltDispatch` FIRST-set computation) — but interpreted,
  not emitted. This lane is the honest fallback; §7 predicts it is ≈ or slower than the tuned
  closure core, so it is *not* the win — it is the generality guarantee.

### 3.3 Regex is the sub-language, not a second engine (compose with 5D)

`regex(r)` → `IrNode::Regex(StringId)`; on the Rust/staged path this lowers directly into
bbnf-regex's `Hir` (`rust/regex/src/hir/mod.rs:216`) and its NFA→DFA→prefilter pipeline
(`Dfa::find`, the PROVE-10 tier). The TS `regex()` (native `RegExp`, `leaf.ts:317`) stays the
runtime-lane leaf but its *staged* form compiles through the same HIR. This is the 5D
GRAMMAR-FRAGMENT identity ("regex literals lower directly into the grammar's own IR") reached from
the combinator side — 4D and 5D meet at `IrNode::Regex`.

## 4. Prior art (with citations)

- **Willis & Wu, *Staged Selective Parser Combinators* (ICFP 2020)** — the canonical proof of 4D.
  `parsley` combinators build a free **selective**-applicative AST (Mokhov et al., *Selective
  Applicative Functors*, ICFP 2019), which is analyzed and **staged via LMS** to emit specialized
  JVM bytecode, erasing combinator overhead. Their "selective" is the exact answer to the map-opacity
  wall (§7): data-dependence is confined to `branch`/`select` leaves while the recognizer skeleton
  stays statically analyzable. Direct template for the IR-builder + host-fn-leaf design.
- **Krishnaswami & Yallop, *A Typed Algebraic Approach to Parsing* (PLDI 2019)** — typed combinators
  build a description whose *type* is its static analysis (unambiguous, linear-time). The types-as-
  analysis discipline ties 4D to LOCK-1's solver: the built IR carries the projection types.
- **Yallop, Ramsay et al., *flap: Deterministic Parsing with Fused Lexing* (PLDI 2023)** — PEG-ish
  combinators normalized to a deterministic automaton and staged; "fused lexing" = the
  `TokenDispatch`/`DispatchGroupRecognizer` lexer-parser fusion already in `IrNode`.
- **Carette, Kiselyov & Shan, *Finally Tagless, Partially Evaluated* (JFP 2009)** — the
  multi-interpreter mechanism for API preservation (§3.1).
- **Gibbons & Wu, *Folding DSLs: Deep and Shallow Embeddings* (ICFP 2014)** — the deep⇄shallow
  theorem guaranteeing the conversion loses nothing (§3.1).
- **Rompf & Odersky, *Lightweight Modular Staging* (LMS, GPCE 2010)** and **Futamura (1971,
  partial evaluation / the projections)** — the "generator's runtime in disguise" identity (§1).
- **Contrast anchors:** `pest`/`tree-sitter` compile an *external* grammar → code (one direction);
  `nom`/`chumsky` are internal shallow combinators (chumsky 1.x adds an internal representation for
  error recovery but no shared optimizer). 4D **unifies** the external-DSL and internal-DSL fronts
  onto *one* IR + optimizer — the differentiator none of them have. `attoparsec`/`megaparsec`
  (Haskell) get no fusion; GHC's rewrite-rule stream fusion is the "inherit the optimizer" flavor
  4D makes explicit by owning the IR.

## 5. Integration seams (path:line)

**Rust (parse-that side, scratch-copy law R3 §8 — never mutate the original):**
- `rust/parse_that/src/parse.rs:61` — `Parser<'a,Output>{ parser_fn: SmallBox<…>, flags }`; add an
  `IrNode` handle (deep-embedding node). Constructors: `leaf.rs`,
  `combinators/methods/{sequence,alternation,repetition,sep_by,minus,map,recover}.rs`.
- `rust/parse_that/src/combinators/methods/map.rs:10` — `map(f: fn(Output)->Output2)` → register `f`
  as `FnDescriptor`, emit `Map{inner, fn_id}`. `map_with_ctx:19`/`map_with_state:32` are the
  closure boundary (§7).
- `rust/parse_that/src/lazy.rs:21,31` — `LazyParser`/`Box<dyn LazyParserFn>` → `IrNode::Ref(RuleId)`
  (the recursion / named-rule seam).
- The `.parse()` entry routes to `crates/ir/src/vm/interpreter/mod.rs:373 parse_with_ir` (interpret)
  or a proc-macro to `crates/codegen` (stage).

**bbnf side (in-campaign, landable):**
- `crates/ir/src/types/node.rs:30` — `IrNode` (the target enum, §2 table).
- `crates/ir/src/types/grammar.rs:27` — `GrammarIR`; `:38 fns: Vec<FnDescriptor>` (the `.map` home);
  `crates/ir/src/types/mod.rs:59 FnId`; `crates/core/src/lower/fn_table.rs:7 FnTable`.
- `crates/ir/src/vm/{interpreter,compiler}/mod.rs` — the two runtime lanes.
- `crates/ir/src/recognizer/mod.rs:{32,86,154,201,255}` — the shared scan kernels.
- `crates/ir/src/passes/` (egraph, csp_strategy, transform/inline, materialization) and
  `crates/ir/src/egraph/` — the inherited optimizer. Post-GN-0/GN-1 the emit home is
  `crates/codegen/src/emit/` (greenfield).

**TS (parse-that/typescript):** `src/parse/parser.ts:24` (Parser class — add `#node`),
`leaf.ts` (leaf builders), `state.ts` (per-parse state). Staging = a vite plugin over the
bbnf codegen compiled to wasm (LOCK-6 verdict).

## 6. rust/ts isomorphism (constitution iii — measured, defers to LOCK-6)

Because both sides build the *same* `IrNode` (one definition, `crates/ir`), the graph shape is
isomorphic **by construction** — 4D does not maintain two parallel graphs, it maintains two
front-ends onto one. The open question LOCK-6 owns: where the optimizer runs for the TS side —
**6C WASM-core** (compile `crates/ir` + `crates/codegen` to wasm; TS marshals `IrNode` in, gets a
specialized parser out — one optimizer, physically shared) vs **6A mirror-tree** (a TS
reimplementation of the passes). 4D's honest position: WASM-core for the *staged* lane (the
optimizer is Rust and heavy; do not reimplement it in TS), a thin TS interpreter only where the
runtime lane must stay pure-TS for a consumer that ships no wasm. This is a measurement (marshalling
cost vs reimpl-drift cost), not an assertion.

## 7. Named risks & counterexamples

1. **The map-opacity fusion wall.** Arbitrary `.map(f)` closures are optimizer black boxes; you
   cannot fuse *across* a `Map` node the way you fuse `Seq(Literal,Literal)`. value.js has 59
   `all()` sites, most carrying a `.map`. If every projection is a fusion barrier, the "inherits the
   optimizer" win shrinks to intra-recognizer (prefilter/dispatch/string-kernel) gains — real
   (PROVE-10, the CSS token kernel) but not full grammar-plane fusion. *Mitigation & honesty:* the
   **selective-functor** discipline (Parsley) — the recognizer *skeleton* (Seq/Alt/Repeat/dispatch/
   prefilter) stays fully analyzable; `Map{FnId}` is a typed leaf the solver reasons about without
   inlining. State the win as "recognizer-skeleton + prefilter + dispatch fusion, projections as
   typed leaves," never "whole-program fusion."
2. **The interpreter is slower than the closure core.** parse-that's closure core is already
   monomorphized, arity-unrolled (`fuseAll` `leaf.ts:183-251`), dispatch-LUT'd, and zero-per-parse-
   alloc (R4 §6e; PROVE-11 removed the last per-parse arrays). A tree/bytecode interpreter
   (`parse_with_ir`) will very plausibly **lose** to it on LL(1) grammars — this is the 4C VM lesson
   ("measured against the closure core, not assumed"). *Consequence:* the interpreted lane is the
   *generality* guarantee, **not** the perf claim; the perf claim lives only in the *staged/AOT* lane
   and must be paired-cold-measured against the closure core, never asserted.
3. **Value-model mismatch.** bbnf's IR runtime is tape/offset-value-fabric first (SPEC §1 four
   lanes); combinators produce arbitrary user-typed values through `.map`. The `Map{FnId}` bridge
   forces a materialization at every projection; if the combinator value model and the tape model
   do not compose cheaply (`crates/ir/src/passes/materialization`), the staged parser pays a
   materialization tax that eats the kernel win. *Test it (§8.4).*
4. **Dynamic grammars defeat staging.** Combinators built from runtime data cannot be AOT-specialized.
   The dual interpreter (§3.1) is the honest boundary: staged for build-time-known grammars, `RuntimeParser`
   for the rest. The percentage of real consumer grammars that are build-time-static is a *measured*
   figure (value.js/latex-paper are static; report the fraction), not an assumed 100%.
5. **Consumer-less substrate (constitution viii, Y.13).** The shared `IrNode` must have a live
   consumer on *both* fronts in-wave — the combinator builder AND the grammar text front-end — or
   4D has forked a second IR, not unified. The gate is: one `IrNode`, two producers, one optimizer.
6. **The refutation discipline (SPEC §0/09).** The de-nightly shim's 1.34–1.57× was a two-arm
   window artifact; the honest value was *stability, not speed*. 4D must not repeat it: any
   "inherits the optimizer → faster" claim is paired-cold-measured against the closure-core baseline
   in ≥2 windows, opposite arm order, or it is reported as an architectural/maintenance win
   (one optimizer, not two) with *zero* speed claim — a legitimate close by itself.
7. **Per-parse alloc regression.** The deep-embedding node must be built *once* at grammar
   construction (like today's closures, R4 §6e), never per `.parse()`. A naive `IrBuilder` that
   re-walks per parse regresses the hot path below the PROVE-11 floor. Gate: steady-state per-parse
   allocs unchanged from baseline.

## 8. What a prototype MUST test to move this family

1. **Unification round-trips (the "same IR" proof).** Build `parsers/json.ts` / a small Rust JSON
   combinator grammar via the *public* API through the `IrBuilder`; assert the emitted `GrammarIR`
   is oracle-equal to the `GrammarIR` bbnf produces from the equivalent `.bbnf` text. Two producers,
   one IR — or the family is a fork. **Falsifier:** any node the combinator front can express but the
   IR cannot represent (find it — e.g., `map_with_state`'s stateful closure).
2. **Inherit ONE optimizer pass, measured (the flagship).** Route the combinator-built parser's
   `IrNode::Regex`/self-loop scan site through PROVE-10's `Dfa::find` prefilter derivation and show
   the combinator parser gains ≥8× on a prefilterable site **without a line of hand-written prefilter
   in parse-that**. This is the single most convincing "inherits the optimizer" datum.
3. **Public API + hot-path invariance.** value.js's imported surface
   (`Parser,all,any,dispatch,regex,string,whitespace,.map,.then`) compiles unchanged; 124/124 TS +
   ~400 Rust tests green; paired-cold steady-state per-parse allocs == baseline (risk 7).
4. **The map-opacity boundary, quantified (risk 1 & 3).** Take a projection-dense grammar; report
   what fuses (skeleton/prefilter/dispatch) vs what does not (across `Map`), the materialization tax
   (risk 3), and the *net* staged-vs-closure-core ratio, paired-cold. **Name the negative result** if
   the closure core already wins on the LL(1) shape.
5. **Interpreter vs closure core (risk 2).** `parse_with_ir` on the built IR vs the tuned closure
   core, paired-cold — to *establish the honest boundary*: interpreter for generality, staged for
   speed. A prototype that skips this and claims the interpreter is the win repeats §0/09.
6. **Isomorphism cost (feeds LOCK-6).** Same combinator grammar → same `IrNode` shape in Rust and
   TS (oracle table); measure the TS staged lane (wasm-core marshalling of `IrNode`) so LOCK-6 can
   adjudicate 6C-vs-6A with a number.

**The one-sentence bar this family must clear:** a JSON (or CSS) parser written in parse-that's
*unchanged public combinator API* is specialized by bbnf's *existing* generator into code that
beats parse-that's own closure core on a prefilterable site — with the built IR proven identical to
the IR the `.bbnf` grammar text produces — or the family closes on the honest architectural win
(one IR, one optimizer, two fronts) with no speed claim.
