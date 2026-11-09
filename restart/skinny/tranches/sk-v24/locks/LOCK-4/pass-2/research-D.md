# LOCK-4 · FAMILY 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP (pass-2 research)

**Charter.** parse-that's combinator surface (`.then()`, `.or()`, `.map()`, …) is a *construction
API over the same `bbnf_ir` the generator plane uses* — not a private closure algebra. `.then()`
builds `IrNode`, not a `Box<dyn Fn>`; the combinator engine is the generator's runtime in disguise;
parse-that keeps its public API and inherits the IR optimizer, tape emitter, and serialization bus.
This report develops that family alone to its strongest form and names its decisive counterexamples.

---

## 1. The substrate that already exists (build on these; do not re-derive)

The shared-IR ownership route is unusually *cheap to prototype* because the target representation,
its runtime, and its serialization bus are already in-tree and consumer-live.

- **The node algebra — a 14-variant near-bijection with the combinator set.** `bbnf_ir::IrNode`
  (`crates/ir/src/types/node.rs:30`) has exactly 14 variants: `Literal`, `Regex`, `Epsilon`, `Seq`,
  `Alt(_, Option<AltDispatch>)`, `Repeat{lo,hi}`, `Ref`, `Skip`, `Next`, `Minus`, `Negate`, `Map{fn_id}`,
  `OptionalWhitespace`, `TokenDispatch`. The map onto parse-that's combinators (R4 §2, R3 §5) is
  near-total: `then`/`all`→`Seq`, `or`→`Alt`, `many`/`opt`/`sepBy`→`Repeat`(+`Seq` for separators),
  `skip`(`<<`)→`Skip`, `next`(`>>`)→`Next`, `minus`(EBNF `-`)→`Minus`, `not`/`negate`→`Negate`,
  `wrap`→`Next`+`Skip`, `lazy`→`Ref`, `trim`→`OptionalWhitespace`, `dispatch`→`Alt`+`AltDispatch`
  or `TokenDispatch`, `map` (restricted)→`Map`. `for_each_child`/`for_each_child_mut`
  (`node.rs:145,173`) already give the uniform structural walk every graph analysis needs. This is
  the load-bearing fact: the combinator surface is *within an epsilon* of being a builder for this enum.

- **A live value-producing runtime.** `bbnf_ir::vm` ships a compiler (`vm/compiler/mod.rs:31`
  `compile(&GrammarIR)->BytecodeProgram`) and a bytecode interpreter (`vm/interpreter/mod.rs:144`
  `run`), fronted by the convenience seam `parse_with_ir(ir,input)` at `vm/interpreter/mod.rs:373`.
  The `Op` set (`vm/bytecode.rs:45`) is 30 opcodes with a tight, branch-predictable dispatch loop and
  an O(1) byte-`Dispatch` table (`bytecode.rs:100`, interp `mod.rs:233`).

- **A serialization bus.** `Op` and `BytecodeProgram` derive `Serialize`/`Deserialize`
  (`bytecode.rs:44,155`); the crate advertises MessagePack for WASM-boundary transfer
  (`lib.rs:7`). The only non-serialized field is `compiled_regexes: #[serde(skip)]`
  (`bytecode.rs:183`) — re-derived post-decode by `prepare_regexes` (`bytecode.rs:220`). This is
  exactly the 4C graft's transport: a program crosses *as data*, the decoder re-hydrates regexes.

- **The projection DSL.** `->` map syntax lowers to `FnDescriptor::Expr{expr: MapExpr, return_type}`
  (`types/fn_descriptor.rs:54`). `MapExpr` (`types/map_expr.rs:17`) is a closed, IR-transparent
  expression tree (literals, `Input`, `InputProp`, `FnCall`, `BinOp`, `UnaryOp`) with a
  `constant_fold` pass (`map_expr.rs:109`) — analyzable, not opaque.

- **The ownership edge already runs ir→parse_that.** `crates/ir/Cargo.toml:13` declares
  `parse_that = "0.4"`; the VM consumes `parse_that::regex::Dfa` (`bytecode.rs:184,238`). Parse-that's
  side patches the reverse edge available (`bbnf-ir -> ../../bbnf-lang/crates/ir`, R3 §1). Both
  half-edges exist in the build graph today — see §6 for the cycle this creates.

---

## 2. The mechanism made concrete

**A three-layer construction API, no private runtime graph.**

1. **`parse-that/src/build/` — the sugar layer (new, thin).** Each combinator method becomes a pure
   constructor returning an `IrNode` (or a small `Builder` wrapping `Rc<IrNode>` for sharing). `a.then(b)`
   ≡ `IrNode::Seq(vec![a,b])` with adjacent-`Seq` flattening; `a.or(b)` ≡ `IrNode::Alt(...)`;
   `p.many()` ≡ `IrNode::Repeat{inner,0,MAX}`; `p.map(f)` ≡ `IrNode::Map{inner, fn_id}` where `f`
   lowers to a `MapExpr` or (the gap, §4) a registered host closure. The public API surface is
   *unchanged* — the return type changes from `Parser<'a,O>` (`parse.rs:61`, a `SmallBox<dyn ParserFn>`)
   to an IR builder. No `Box<dyn Fn>` is ever constructed on this path — the SmallBox/vtable question
   (R3 §5, LOCK-4/4B's problem) *dissolves by never arising*.

2. **`bbnf_ir` — the inherited optimizer + the two terminals.** The built `GrammarIR`
   (`types/grammar.rs:27`) is handed to the existing pass pipeline (dispatch-table synthesis giving
   `AltDispatch`, FIRST/FOLLOW, TokenDispatch fusion) and then to *either* terminal:
   - **interpret**: `compile`→`BytecodeProgram`→`Interpreter::run` (the portable/staged path, and the
     substrate for the TS graft);
   - **compile**: the Rust AOT emitter (`crates/core/src/backend/rust/emitter/`) that already consumes
     `IrNode` to emit specialized parser source.
   parse-that's domain parsers (JSON, CSS) get *both* backends for free.

3. **The TS face — a decoder, not a re-implementation.** The serialized `BytecodeProgram` crosses to
   TS; a ~200-LOC switch-loop decoder (the 4C graft) executes the same opcode stream, re-deriving
   regexes as native sticky `RegExp` (matching leaf.ts:317). The combinator *builders* can also be
   mirrored in TS returning the same JSON-shaped `IrNode`, so `.then()` builds an identical tree on
   both sides — layout isomorphism by construction (Constitution iii, LOCK-6).

**Why this is "the generator's runtime in disguise":** the generator already turns a `.bbnf` file into
`GrammarIR` and runs it through `compile`/emitter. 4D makes the *combinator* front produce the same
`GrammarIR`. One IR, one optimizer, one tape emitter, two front-ends (grammar text · combinator API).

---

## 3. The decisive negative result — the VM does NOT execute `.map`

**This is the family's sharpest finding and it must be stated first, not buried.** The bytecode `Op`
enum (`bytecode.rs:45`) has **no `Map`/`ApplyFn`/`Reduce` opcode**. The interpreter's dispatch loop
(`interpreter/mod.rs:168-296`) enumerates every opcode; none applies a host function. The runtime
`Value` (`interpreter/value.rs:52`) is a purely *structural* spine: `Span(u32,u32)` · `Tagged{tag,span,
children}` · `Array` · `Nil`. There is no `Int`, `Float`, `String`, `Object`, or user-enum inhabitant.

**Consequence for 4D's Q1 (does `.map`/projection reconcile with a tape-first runtime?).** On the
*interpret* terminal, arbitrary `.map` output does **not** materialize at all — the `Map` node is
lowered away/ignored by `compile_node`, and projection is reconstructed only by the *AOT Rust
emitter*. So the honest answer, split by shape:

- **Structure-dominated / recognizer grammars (JSON shape, LL(1)):** reconcile *for free* — the VM
  already emits the exact `Span`/`Tagged`/`Array` tape a tape-first consumer wants; there is **no
  materialization tax** because there is no materialization. The closure core's per-leaf `Box<dyn Fn>`
  (R3 §5, confined to scalar leaves per the settled fact) is *removed*, not merely fused.

- **Projection-dense grammars (`all()`-of-mapped-parts, value.js's units/color/easing):** the VM
  path **cannot run them today** — there is nowhere to put `parse_hex(input)` or a bespoke enum
  constructor. This is the tax, and it is not a slowdown but an *expressivity wall*: the interpret
  terminal needs a new `Op::ApplyFn(FnId)` + a host-fn dispatch table (the closure the settled fact
  says `FnDescriptor` lacks) to reach parity with the AOT terminal. **Name the negative result:** on
  the projection-dense shape, the staged/interpret path is *not yet a peer of closures* — it is
  behind them by a whole value plane, and closing that gap is precisely 4D's cost, measured in §7.

The corollary: 4D's honest claim is **"one IR, two terminals, but the interpret terminal is a
recognizer+structural-builder until an `ApplyFn` opcode + host-fn table land."** The AOT terminal has
full projection today; the paper-thin combinator front inherits it immediately for *compiled* mode.

---

## 4. The vocabulary gap — minimal lawful variant set (measured, not guessed)

Mapping every parse-that combinator (R4 §2) onto `IrNode` yields a precise gap list. State-effecting
and host-closure combinators are exactly the residue the directive asks to locate.

| combinator | IR home | status |
|---|---|---|
| `then`/`all`/`skip`/`next`/`or`/`many`/`opt`/`sepBy`/`minus`/`not`/`negate`/`wrap`/`lazy`/`trim`/`dispatch` | `Seq`/`Alt`/`Repeat`/`Skip`/`Next`/`Minus`/`Negate`/`Ref`/`OptionalWhitespace`/`TokenDispatch` | **covered** |
| `map` with a `MapExpr`-expressible body | `Map{FnDescriptor::Expr}` | **covered** |
| `map` with an **arbitrary host closure** | — (`FnDescriptor` has no closure variant — settled fact) | **GAP-1** |
| `peek` (positive zero-width lookahead) | `Negate(Negate(inner))` is *lawful* but double-backtracks | **GAP-2 (encodable, inefficient)** |
| `lookAhead` (zero-width, value-returning) | — (`Negate` discards value; no value-preserving look) | **GAP-3** |
| `recover` (error-recovery + sentinel) | — (VM has FOLLOW-set diagnostics `mod.rs:335`, no recovery combinator) | **GAP-4** |
| `mapState` (stateful transform, `Object.create(state)` R4 §6b) | — (no mutable user state in `Value`/`Interpreter`) | **GAP-5** |
| `eof` | — (no end-of-input assertion opcode; `Halt` is program-end, not input-end) | **GAP-6** |

**Minimal lawful variant set to close the gaps** (the smallest addition, per KISS/parsimony):
one IR node `Look{inner, positive: bool, keep_value: bool}` subsumes GAP-2/3 and lets `Negate` stay a
pure recognizer; one `IrNode::ApplyFn`/extended `Map` variant + `FnDescriptor::HostClosure(FnId)` +
`Op::ApplyFn(FnId)` + an interpreter-side `fn_table: Vec<Box<dyn Fn(&[Value])->Value>>` closes GAP-1
and unlocks §3's projection plane; `recover`/`mapState` (GAP-4/5) are the *host-function-lattice*
questions and belong to LOCK-7 (do not resolve here — §5). `eof` (GAP-6) is a trivial `Op::AssertEof`.
**Each variant gets a paired regression lane** (§7): construct the same grammar with the combinator
front and with the hand parser, assert byte-identical `Value`/tape and ≥0.98× cold.

---

## 5. Hard DAG predecessors (report their shape; do not resolve)

- **LOCK-5 (regex continuum) — substrate ruling.** The VM's `MatchRegex` opcode already binds
  `parse_that::regex::Dfa` (`bytecode.rs:184,238`), and `bbnf_ir` re-exports `bbnf_regex`'s
  `CharSet128`/`regex_first`/`classify` (`lib.rs:32-36`). Whether regex HIR is *one* type with bbnf
  HIR (5A) or a *lawful functor* (5B) decides whether the combinator front's `regex(...)` builds a
  shared HIR node or crosses a functor boundary. 4D consumes that verdict; it cannot pick it. The
  ir→parse_that Cargo edge (§6) is downstream of this ruling.

- **LOCK-7 (grammar totality / host-fn lattice) — GAP-1/4/5 shape.** The `FnDescriptor::HostClosure`
  variant, `mapState`, and `recover` are host-function-lattice members: typed signatures for
  context-dependent/recursive semantics behind declared interfaces (7C). The *minimal typed host-fn
  interface* that 4D's `ApplyFn` opcode dispatches through is LOCK-7's to define; 4D reports the
  need (§4) and the seam (`Interpreter.fn_table`), and defers the lattice's shape.

---

## 6. The ownership cycle — the family's central architectural risk

`crates/ir` depends on `parse_that` (Cargo.toml:13) for the regex `Dfa`. 4D asks `parse_that` to depend
on `bbnf_ir` (to build `IrNode`). **That is a Cargo dependency cycle**, which Cargo forbids across
crates. Three lawful resolutions, in ascending disruption:

1. **Regex extraction (preferred, and it is LOCK-5's likely verdict anyway).** Move the `Dfa` the VM
   needs out of `parse_that` into `bbnf_regex` (already a dep of `bbnf_ir`, `lib.rs:32`). Then
   `bbnf_ir` no longer needs `parse_that`; `parse_that` freely depends on `bbnf_ir`. This *dissolves*
   the cycle and unifies the regex home — coherent with the Constitution's "one regex home" (SPEC §1).

2. **A third crate owns the node algebra.** Factor `IrNode`+`GrammarIR`+VM into a leaf crate depended
   on by both `parse_that` and the regex engine. Heavier; risks a god-crate.

3. **parse-that stays the owner; bbnf consumes it.** Invert: `IrNode` lives in `parse_that`, `bbnf_ir`
   re-exports it. Contradicts "the generator's IR" framing and the extant `crates/ir` home; rejected
   unless GESTALT-P rules parse-that primacy over the node algebra.

**This cycle is the first thing a 4D prototype must confront** — resolution #1 is both the cleanest and
the one that couples 4D to LOCK-5's substrate ruling as a hard predecessor.

---

## 7. What a prototype must test to move this family

A single scratch-copy prototype (R3 §8 recipe, isolated `CARGO_TARGET_DIR`) settles the family:

1. **The cycle break (gate 0).** Prove resolution #1 (§6) compiles: `Dfa` in `bbnf_regex`,
   `parse_that` depends on `bbnf_ir`. If it does not compile cleanly, 4D is blocked on LOCK-5.

2. **The JSON build oracle.** A minimal Rust combinator front (`~150 LOC`, `src/build/`) constructs
   the JSON grammar as `GrammarIR`. Assert its `IrNode` tree is **byte-identical** (structural-equality
   oracle) to (a) the `.bbnf`-generated JSON IR and (b) the shape the hand parser recognizes.

3. **The two-terminal paired number (the decisive datum).** Over six corpora (apache-builds, canada,
   citm, data, data_xl, twitter — R3 §6), paired-cold A,B,A,B, N≥200, two windows, both arm orders:
   - **interpret** (`parse_with_ir`) vs the closure core, on the **structure-dominated** JSON shape —
     expect the recognizer VM to *win or tie* (no `Box<dyn Fn>` leaves; the §3 free-tape claim);
   - **compile** (AOT emitter) vs the hand-tuned Rust parser — report the shrink honestly; the PROVE-09
     refutation (VERIFY.md, window-asymmetry killed the 1.34–1.57× shim claim) is the standing warning
     that a favorable single window is self-deception.

4. **The projection-dense counter-measurement (name the tax).** Take a value.js-shaped grammar
   (`all()`-of-mapped-parts). Confirm the interpret terminal **cannot run it** without `Op::ApplyFn`
   (§3), then land the minimal `ApplyFn`+`fn_table` variant and re-measure the materialization tax:
   paired-cold interpret-with-ApplyFn vs closures. **If closures win on the LL(1)/projection shape,
   that is the family's negative result and must be reported as such** (Constitution vii).

5. **The TS graft floor (first cross-language number — high-leverage per the directive).** Serialize
   a JSON `BytecodeProgram` (msgpack, `compiled_regexes` skipped), decode in a ~200-LOC TS switch-loop,
   re-derive regexes as sticky `RegExp`. Dual oracle: **opcode-stream identity** (Rust-serialized ==
   TS-decoded) **and parse identity** (`Value` tape == `JSON.parse`). Gate: no-regression vs today's TS
   closures (leaf.ts fused `all`), stated armed/unarmed (packrat OFF by default, R4 §6g).

6. **Per-variant regression lanes.** For each closed gap (`Look`, `ApplyFn`, `AssertEof`): a paired
   lane asserting byte-identical output and ≥0.98× cold vs the combinator+hand baselines.

---

## 8. Prior art (citations)

- **tree-sitter** compiles a JS grammar DSL to a table-driven C parser — the "construction API →
  shared IR → generated runtime" shape 4D mirrors, minus tree-sitter's GLR (parse-that is
  ordered/PEG-like, matching `Alt`'s ordered semantics `node.rs:46`).
- **LALRPOP / Menhir** generate parsers from a grammar; the combinator-vs-generator duality here is
  4D's "one IR, two front-ends" claim in reverse (they have one front-end, one terminal).
- **GHC** desugars source to a small typed Core and lowers *combinator-like* surface (do-notation,
  list comprehensions) to the same Core the whole compiler optimizes — direct precedent for "sugar
  over the shared IR, inherit the optimizer."
- **MLIR dialect lowering** — the `build/` sugar → `bbnf_ir` → {interpret, AOT} tower is a two-terminal
  progressive lowering; MLIR's lesson is that the *analysis passes* (dispatch synth, FIRST/FOLLOW)
  must live on the shared IR, not either front-end. Extant here (`crates/ir/src/passes/`).
- **nom / winnow** — the *anti-pattern* 4B/4D must beat: no IR at all, pure monomorphized closure
  chains. R3's dev-deps already bench against both (`Cargo.toml:89`); the two-terminal number in §7.3
  is measured against exactly this class.
- **egg / egglog** — the `fuse(Graph)→Graph` equality-saturation pass (LOCK-2 steering) that a shared
  IR *reuses* rather than clones; `bbnf_ir::egraph` (`crates/ir/src/egraph/`) is the in-tree engine,
  and 4D's fusion must consume it, not fork one.
- **PROVE-14** (SPEC seed, LOCK-6) — byte-exact tape crossing Rust→TS with 4.4× stop-early; the
  empirical proof the §7.5 msgpack graft's crossing boundary is real and measurable.

---

## 9. Named risks and counterexamples

- **R-CYCLE.** The Cargo cycle (§6) is a hard build failure, not a design smell. *Counterexample:* a
  prototype that "builds `IrNode` from parse-that" without first extracting `Dfa` simply will not
  compile. *Guard:* §7 gate 0 is born-RED on the cycle break.
- **R-MATERIALIZE.** The interpret terminal is a recognizer, not a value engine (§3). *Counterexample:*
  a demo that claims "combinators inherit the VM" on a projection-dense grammar — it silently drops
  every `->`. *Guard:* §7.4 forces the projection-dense measurement and the honest tax.
- **R-HOSTCLOSURE.** GAP-1 tempts an opaque `Box<dyn Fn>` smuggled into `FnDescriptor` — reintroducing
  the exact vtable indirection 4D's builder eliminated, now *inside* the shared IR and unserializable
  (breaking the msgpack bus and the TS graft). *Guard:* the host-fn table is `FnId`-indexed and
  registered, never inlined into the node; LOCK-7 owns its typed shape.
- **R-DIVERGENCE.** The Rust and TS `build/` layers drift, breaking layout isomorphism. *Guard:* the
  §7.2 structural-equality oracle runs on both; LOCK-6's isomorphism linter is the enduring gate.
- **R-CLOSURE-WINS.** The measured outcome may be that closures beat the staged path on LL(1)/projection
  shapes (the PROVE-11 dispatch-lane precedent: a claimed +10% collapsed to +5% under fresh paired
  windows, VERIFY.md). *This is a legitimate close* (Constitution vii) — report it, do not bury it.
- **R-OVERBUILD.** Adding `Look`/`ApplyFn`/`AssertEof`/`recover`/`mapState` all at once bloats the
  opcode set and the IR beyond the near-bijection's elegance. *Guard:* §4's minimal set — one `Look`,
  one `ApplyFn`, one `AssertEof`; defer `recover`/`mapState` to LOCK-7's lattice.

---

## 10. Summary judgment (for synthesis, not a verdict)

4D is the *cheapest* LOCK-4 family to stand up (the IR, VM, msgpack bus, and half the ownership edge
already exist and are consumer-live) and the one with the *sharpest named wall* (the VM is a
structural recognizer with no value plane — §3). Its strongest form is: **combinators are a `build/`
sugar over `bbnf_ir`; the SmallBox/vtable question dissolves; the AOT terminal gives full projection
today; the interpret terminal reaches projection parity only via a minimal `ApplyFn` opcode + host-fn
table (GAP-1) — and that closure of the value plane is 4D's entire measurable cost.** The two hard
predecessors are LOCK-5 (breaks the Cargo cycle by extracting `Dfa`; rules the regex substrate) and
LOCK-7 (types the host-fn lattice `ApplyFn` dispatches through). The single prototype of §7 —
cycle-break, JSON build oracle, two-terminal paired number, projection-tax measurement, TS msgpack
graft — moves the family decisively or returns its honest negative result.
