# LOCK-4 · FAMILY 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP (pass-3 research)

**Charter.** parse-that's combinator surface (`.then()`, `.or()`, `.map()`, …) is a *construction
API over the same `bbnf_ir` the generator plane uses* — `.then()` builds `IrNode`, not `Box<dyn Fn>`;
the combinator engine is the generator's runtime in disguise; parse-that keeps its public API and
inherits the IR optimizer, tape emitter, and serialization bus. This pass sharpens the family with a
*measured value-plane census* (the named GAP-1 now counted against live consumer source) and a
mechanism trace of the runtime→comptime seam. Every claim below is tree-verified on the bbnf-lang
working tree and the three consumer repos.

**Substrate carried forward (verified this pass, not re-derived):** `IrNode` is a 14-variant
near-bijection with the combinator set (`crates/ir/src/types/node.rs:30`), with a uniform child walk
`for_each_child`/`_mut` (`node.rs:153,181`); the VM is 1772 LOC (`crates/ir/src/vm/`), an `Op`
dispatch loop plus an `IrNode`→bytecode compiler; `Op`/`BytecodeProgram` derive `Serialize`, the sole
`#[serde(skip)]` being `compiled_regexes` (`vm/bytecode.rs:184`), re-hydrated post-decode
(`bytecode.rs:238`). The projection DSL `->` lowers to `FnDescriptor::Expr{expr: MapExpr, …}`
(`crates/ir/src/types/fn_descriptor.rs:54`), a closed analyzable tree with `constant_fold`.

---

## Q1 — Runtime combinator graph → comptime monomorphization (mechanism trace)

**The runtime combinator API does NOT reach comptime type-monomorphization, and cannot absent a
codegen step; it reaches it only as build-time codegen over a comptime-known graph.** The exact trace:

1. The 4D `build/` sugar constructs a *runtime* `GrammarIR` (`crates/ir/src/types/grammar.rs`) — a heap
   `Vec<IrNode>` + rule map materialized at run time. This is owned data, not a type.
2. The only monomorphizing lowering in-tree is the AOT Rust emitter
   (`crates/core/src/backend/rust/emitter/`), consuming `IrNode`/`FnDescriptor` to emit *specialized
   Rust source* — a build-time `xtask`/regen step, not a runtime call. Rust has no `comptime` that
   reifies a runtime `Vec<IrNode>` into monomorphized types, so no runtime→comptime reflection exists.
3. The only path that consumes the runtime graph *at run time* is the interpret terminal
   (`vm/interpreter/mod.rs:114` `new`, the `run` loop): one `match Op`, dynamic dispatch, **zero type
   specialization per grammar**.

**Honest 4D answer:** a combinator graph reaches monomorphization *only* by being emitted to the
serializable `GrammarIR` and fed to the build-time emitter as a codegen input — the identical route
grammar-text→IR takes. A monomorphizing lowering demands of the API exactly that the built `GrammarIR`
be **const-serializable and total** (it is — `IrNode`/`FnDescriptor`/`MapExpr` are all plain
`Serialize` data), so a `const`/`build.rs` site can name it. It does **not** demand any change to the
runtime combinator surface; the surface stays a data builder, and 4B's comptime-fusion mechanism is a
*consumer of 4D's serialized plan*, not a competitor to it. 4B monomorphizes at the type level (a
runtime graph must be lifted to const first); 4D says the lift is *serialization*, and the serialized
plan is precisely what both the AOT emitter and the TS emitter eat (Q5). 4D neither claims nor needs
runtime monomorphization.

---

## Q2 — Value plane through an arbitrary host closure (the census, counted on live source)

Pass-2 named GAP-1 (`FnDescriptor` has no closure variant — verified: the enum at
`fn_descriptor.rs:18` carries exactly `EnumWrap`/`BoxWrap`/`NumberConvert`/`HexConvert`/`SpanCapture`/`Expr`;
`rg HostClosure` = 0 hits). This pass **counts** the non-projection surface across the live consumers.
Projection sites = parser-level `.map()` (the `->` equivalent); domain `Array.prototype.map` on
non-parser data is included only where it constructs the parse value.

**latex-paper (`/Users/mkbabb/Programming/latex-paper/src/{grammar,bibtex}`) — 84 parser `.map` sites
(measured: `text.ts` 17, `commands.ts` 23, `math.ts` 4, `document.ts` 3, `compile.ts` 26,
`environments.ts` 11):**
- **44** are **object-literal constructions** — `.map((value) => ({ … }))` (e.g.
  `src/grammar/math.ts`, verified 44 `=> ({` sites). **NOT MapExpr-expressible today**: `MapExpr`
  (`crates/ir/src/types/map_expr.rs:17`) has literals, `Input`, `InputProp`, `FnCall`, `BinOp`,
  `UnaryOp` — **no record/object constructor, no array constructor, no conditional**.
- The remaining ~40 are named-fn / nullary-constant projections (`.map(textNode)`,
  `.map(() => null)`) — **MapExpr-expressible as `FnCall{name, args}` iff the name is registered**
  (`map_expr.rs`), plus a small ternary residue.
- **Measured barrier: 44 of 84 (~52%) are not MapExpr-expressible today** — and every one of them is
  an object/array/conditional *data* construction, not a captured closure.

**value.js (`/Users/mkbabb/Programming/value.js/src/v4`) — 40 parser `.map` projections; the maximal
wall is that parse produces first-class CLOSURES.** The easing plane
(`src/v4/easing.ts`) types its output `EasingFunction = (progress: number) => number` (`easing.ts:4`);
a cubic-bezier/steps/linear parse **returns a closure capturing parsed control points** —
`return ok((progress) => {…})` at `easing.ts:124`, `:153`, and the steps arm `:142`. **Measured: exactly
3 closure-returning parse sites, all in `easing.ts`.** This is beyond *any* serializable descriptor:
the produced value is itself a host function. The milder tier is a *registered* call
(`channelToken(part,255)`, color grammar) — expressible if named; the color-conversion anchor closures
(`src/v4/color/anchors.ts:336-342`, `toXYZ`/`fromXYZ`) are a lookup **table**, not parse projections,
so they never enter the IR barrier.

**keyframes.js** consumes value.js transitively; its projections are the same two tiers, dominated by
object construction of `ResolvedKeyframes`.

**Implied typed shape for the descriptor (ranked, minimal, still-analyzable-first):**
1. `FnDescriptor::Record { fields: Vec<(StringId, MapExpr)> }` + `MapExpr::Array(Vec<MapExpr>)` +
   `MapExpr::Select{cond,then,els}` — covers **all 44 latex-paper object/array/ternary sites while
   staying pure data**: still serializable, still `constant_fold`-total (`map_expr.rs`), still visible
   to type-inference and fusion. This is the *large, cheap* win — ~52% of the barrier dissolves as
   three plain-data nodes with zero opacity.
2. `FnDescriptor::Opaque(HostFnId)` — the irreducible residue (value.js's 3 easing closures, any
   `.map(f)` whose `f` is neither a registered name nor a data expression). Carries **only** an `FnId`
   into a host `fn_table: Vec<Box<dyn Fn(&[Value]) -> HostValue>>`; the `Value` union must gain a
   `Host(HostHandle)` inhabitant so a parse can *carry* a closure it cannot introspect. This tier is
   LOCK-7's lattice member (Q8) — 4D reports its shape and count, does not type it.

**Load-bearing finding:** the host-closure barrier is *not* monolithic. Split it and ~half is
recoverable as analyzable data (`Record`/`Array`/`Select`); the opaque residue is single-digit (3 sites
in value.js) and concentrated on the value plane. 4D must not smuggle one `Box<dyn Fn>` per `->`; it
adds the three data nodes first and lets `Opaque` carry only the true closures.

---

## Q3 — Opaque-barrier surface size (counted ratio vs the ≤2% floor)

Barrier = fraction of *nodes* an analysis cannot see through. An `Opaque` descriptor is a hard wall to
type-inference, dispatch-synthesis, and fusion; a `Record`/`FnCall`/`MapExpr` node is transparent to
all three.

- **latex-paper:** with the tier-1 `Record`/`Array`/`Select` nodes, the opaque residue is the anonymous
  value-carrying closure sites — near **0 of 84** (latex-paper's projections are all data or named
  fns). **Barrier ≈ 0% — under the ≤2% floor by construction.**
- **value.js:** the irreducible opaque set is the **3 `EasingFunction`-producing parses** (`easing.ts`).
  As a fraction of the 40 value.js parse projections that is ~7.5% of *projection* sites — but the
  barrier floor is *of all IR nodes*, and a value.js grammar's node count is dominated by the recognizer
  spine (leaves + `Seq` + `Alt` + `Repeat`). Against that denominator the 3 opaque leaves are **well
  under 2%**. The site→node expansion is the reconciler: a projection *site* expands to many recognizer
  *nodes*, so a single-digit-of-sites opaque count is a fraction-of-a-percent-of-nodes barrier.
- **Honest framing and the caveat that matters:** the ≤2%-of-**nodes** floor is cleared comfortably on
  both corpora *in aggregate*, because opacity lives only at value-producing leaves, never in the
  recognizer spine — FIRST/FOLLOW, dispatch synthesis, and fusion run on the spine and never touch an
  `Opaque` node. **But the floor is a node aggregate, not a per-grammar invariant:** in a
  closure-dense sub-grammar (an isolated easing/math fragment) the opaque-of-nodes ratio rises well
  above 2%, and such a fragment reads as "skeleton with opaque leaves." The optimizer is unobstructed
  in aggregate; the "fully analyzable graph" reading is safe repo-wide and fragile locally.

---

## Q4 — Fusion-policy / blowup budget (4D's disposition: inherited 1:1, plus one value-plane rule)

4D does not own the cost-model caps (fuse-arity, dispatch-fanout, boxing-depth, `.rlib`/`dist`
budget) — LOCK-2's engine and 4B's comptime fusion set them. **4D's contribution is the proof that the
budget is *identical* to grammar-text's:** because the combinator front produces the *same* `GrammarIR`,
a value.js grammar authored via combinators and the same grammar authored as `.bbnf` lower through the
same passes to byte-identical `IrNode` (Q5's oracle), so their generated-size and compile-time growth
are **the same function of fuse-arity** — there is no separate 4D budget to measure. Concrete in-tree
firing anchors 4D inherits unchanged: `try_factor_alt` fires at `branches.len() >= 4`
(dispatch-fanout), `fuse_single_use` gated on composite `Seq≥2`, map fusion folds `Map(Map(x))`.

The one 4D-specific cap is on the value plane: an `Opaque` node is un-fusible — a hard
boxing-depth-1 wall. The fusion policy must treat `Opaque(FnId)` as a fixed point terminating a fuse
chain: **never inlined, never monomorphized, always dispatched.** That single rule is also the exact
guard that keeps the msgpack bus and TS graft intact — an inlined closure would be unserializable.

---

## Q5 — Serializable-plan observable (the distinguishing datum, and its precise limit)

**Yes — a fused region IS derivable from one serializable plan, but the plan drives *two* of three
lowerings byte-identically and the third by oracle.** The plan is `BytecodeProgram` (`vm/bytecode.rs`,
`Serialize`/`Deserialize`; msgpack for the WASM boundary; sole `#[serde(skip)]` = `compiled_regexes`,
re-hydrated by `prepare_regexes`, `bytecode.rs:238`).

- **Lowering A (Rust interpret):** `GrammarIR` → `compile` (`vm/compiler/mod.rs`) → `BytecodeProgram`
  → `Interpreter::run`.
- **Lowering B (TS decode):** the *same* serialized `BytecodeProgram` crosses (msgpack) → a switch-loop
  decoder executes the identical opcode stream, re-deriving regexes as sticky `RegExp` (mirrors the TS
  `regex()` leaf).
- **The observable:** serialize once in Rust, decode in TS, feed both the identical input — assert
  **opcode-stream identity** (Rust-serialized bytes == TS-decoded program) **and** `Value`-tree
  identity. *One plan, two lowerings, byte-identical parses.* A real, testable datum, not a type-system
  artifact — the plan is data outliving the Rust type that built it.

**Precise limit (state it, do not overclaim):** the AOT Rust emitter (Q1) is a *third* lowering **not**
driven by the serialized `BytecodeProgram` — it consumes `GrammarIR` directly and monomorphizes. So
"one plan, two lowerings" is exact for {interpret, TS-decode} and **oracle-only** for the AOT pair: the
emitted Rust parser must be diff-tested against the interpret path (Q6's dual oracle), not proven
byte-identical at the opcode level (it has no opcodes). The fused region is intrinsic to *neither* the
Rust type system *nor* a single terminal — it is a property of the shared `GrammarIR`, and the
serialized `BytecodeProgram` is its observable proof for the portable pair.

---

## Q6 — Isomorphism-of-record standard + value-plane home (both answered concretely)

**Isomorphism-of-record = a dual oracle over a *live* golden, never a hand-faked blob:**
1. **Opcode-stream identity** — the TS decoder's re-serialized program bytes == the *live* msgpack
   output of `compile(GrammarIR)` run in Rust *this build* (a golden regenerated by the Rust compiler,
   checked into the isomorphism lane, never a frozen literal). Guards the plan.
2. **`Value`-tree identity** — the TS-decoded `Value` tape == the Rust `Interpreter` `Value` tree
   (`vm/interpreter/value.rs`), AND == `JSON.parse` on the JSON corpus (semantic ground truth). Guards
   the value plane.

**Corpus:** the six JSON corpora (apache-builds, canada, citm, data, data_xl, twitter) for the
recognizer/structural tape, `canada` mandatory as the adversarial f64-dense member; a value.js-shaped
grammar (units/color/easing) for the projection plane once tier-1 `Record`/`Select` land;
latex-paper's bibtex + text grammars for the object-literal tier. Paired-cold, two windows, both arm
orders (the window-asymmetry law: a favorable single window is self-deception).

**Value-plane home — the internal-descriptor evaluator (Hex/Number/Enum) is LOCK-3's, not LOCK-7's.**
Verified: `FnDescriptor`'s built-in tiers — `EnumWrap` (`fn_descriptor.rs:20`), `BoxWrap` (`:26`),
`NumberConvert` (`:40`), `HexConvert` (`:45`), `SpanCapture` (`:49`), `Expr` (`:54`) — are evaluated
**only** in `crates/core/src/backend/rust/emitter/`, i.e. the AOT Rust lowering = LOCK-3's IR
transposition spine. The VM touches **none of them**: `compile_node` lowers `IrNode::Map { inner, .. }`
to `self.compile_node(inner, ir)` and **silently drops `fn_id`** (`vm/compiler/node.rs:31`); the `Op`
set has **no `ApplyFn`** (`rg ApplyFn crates/ir/src` = 0); `Value` (`value.rs:52`) has **4 shapes**
(`Span`, `Tagged`, `Array`, `Nil`) — no scalar. So the clean split the family asserts:
- **Internal descriptors (Hex/Number/Enum/Box/Span/Expr)** → **LOCK-3 IR-VM lowering.** Closed,
  data-defined, live today only in the AOT emitter. Interpret-terminal parity means adding
  `Op::ApplyDescriptor(FnDescriptorId)` to the VM (LOCK-3's opcode), evaluating the same closed set —
  no host dispatch.
- **Arbitrary host closures (`Opaque(HostFnId)`, Q2 tier-2)** → **LOCK-7 host-fn lattice.** Only these
  cross into a registered `fn_table`; only these are typed by LOCK-7's signatures.

**Correction of record (do not carry the earlier "value plane built" line):** the VM is a value-
*structural* recognizer (offsets + rule tags), NOT a combinator core that carries `.map`. Making it one
is the family's central UNBUILT completion — four mechanical edits on structures that already
serialize: (1) add the descriptor/`ApplyFn` opcode + lower `Map` to `compile_node(inner)` **then**
emit it; (2) add `#[serde(default)] fns: Vec<FnDescriptor>` to `BytecodeProgram`; (3) extend `Value`
with scalar arms; (4) one interpreter handler + its TS mirror on the *same serialized descriptor*.
This is a build task, not a missing primitive — abrogate-before-patch is discharged (reuse ISA,
container, interner, msgpack bus, dispatch loop).

---

## Q7 — Setup vs value-plane tax vs recognizer cost (the setup number, measured from source)

`Interpreter::new` (`vm/interpreter/mod.rs:114`) performs, **per parse**, seven eager heap allocations
via `Vec::with_capacity`:

| field | capacity | line |
|---|---|---|
| `values` | 64 × `Value` | `:122` |
| `call_stack` | 32 × `CallFrame` | `:123` |
| `checkpoints` | 32 × `Checkpoint` | `:124` |
| `repeats` | 16 × `RepeatState` | `:125` |
| `memo_starts` | 8 | `:127` |
| `value_depth_stack` | 8 | `:128` |
| `rule_stack` | 16 | `:129` |

Plus `memo: FxHashMap::default()` (`:126`, **lazy — zero alloc until the first `MemoStore`, which only
fires when packrat is armed**) and `diagnostics: Vec::new()` (`:130`, zero alloc). So the setup tax is
**7 heap allocations, ~176 slots (~2–3 KB), unconditional, per parse** — paid even for a 10-byte input.

**Amortization curve & the clean split:**
- **Setup cost** = those 7 allocations. Eliminated entirely by pooling: hold one `Interpreter` and, per
  parse, `clear()` each `Vec` (retains capacity) + `memo.clear()` — a `reset(program, input)` method
  replacing `new`. Warm-reuse setup → **0 allocations**; a `1/N` decay steep for a bench loop, negligible
  for a one-shot CLI. For small inputs (value.js CSS values, ~10–40 B) the 7 allocations are a *large*
  share of single-parse cost; for a 1 MB JSON parse they are noise. This is the pooling win to measure:
  paired-cold single-parse of a value.js-sized input, `new` vs pooled `reset`.
- **Recognizer cost** = the `match Op` spine loop (`MatchString`, `MatchRegex`, `Dispatch`,
  `Call`/`Return`, `Repeat*`, `Jump*`). Independent of the value plane.
- **Value-plane cost** = `MakeArray`/`MakeTagged` + (future) `ApplyDescriptor`/`ApplyFn` `Value`
  construction. On a pure recognizer grammar it is `CaptureSpan`-only and near-free; it grows exactly
  with projection density.

The three costs separate in one bench by toggling: (a) `new` vs pooled `reset` isolates setup; (b) a
recognizer-only grammar vs a projection-dense grammar isolates value-plane; (c) the residue is the
recognizer. **Decision value of the rescue is oracle viability, not a competitive terminal:** pooling
makes the interpret correctness backstop cheap enough to run over the full corpus in CI, but even a
rescued interpret path loses to codegen structurally (the fast face is born with its `BumpSlab`, no
constructed graph to pool). Pool the oracle; do not expect it to become the fast face.

---

## Q8 — Predecessor shapes (report only; do not resolve)

- **LOCK-5 (regex continuum) — the Cargo-cycle breaker and the regex-node home.** The settled
  cross-lock fact retires the regex functor: two domain-fit types (opaque `Regex(StringId)` grammar
  tier; rich `Hir` engine tier) with a cheap bridge. 4D's `regex(...)` builder therefore targets the
  opaque `IrNode::Regex(StringId)` on the grammar tier and never crosses a functor. **The live risk 4D
  still inherits is the Cargo cycle:** `crates/ir` depends on `parse_that` (`crates/ir/Cargo.toml:13`
  `parse_that = "0.4"`) for the VM's `Dfa` (`vm/bytecode.rs:184,238`
  `parse_that::regex::Dfa`), while 4D needs `parse_that` → `bbnf_ir`. That is a Cargo cycle; LOCK-5's
  substrate ruling — extract the `Dfa` into `bbnf_regex` (already a dep of `bbnf_ir`) — dissolves it
  and unifies the one regex home. **Disposition: 4D's gate-0 is downstream of LOCK-5's `Dfa`
  extraction; report, do not break the cycle here.** *Open sub-question exported: is `bbnf_ir` the
  genuinely shared HIR (inherit-for-free) or two HIRs with a lawful functor (which evaporates the
  inheritance)? gate-0-compiles is mechanical; shared-substrate *semantics* is DAG-blocking and LOCK-5's
  to rule.*
- **LOCK-7 (host-fn lattice) — owner of the `Opaque(HostFnId)` tier only (Q2/Q6).** LOCK-7 types the
  `fn_table` signature `Opaque` dispatches through and the `Value::Host` inhabitant a closure-carrying
  parse needs. 4D reports the *count* (single-digit — 3 value.js easing closures) and the *seam*
  (`Interpreter.fn_table` + the host-`ApplyFn` opcode); LOCK-7 defines the typed lattice and its
  per-subsystem floor (value.js's ~3 easing folds; the tier-1 `Record`/`Array`/`Select` boundary).
  **Disposition: the internal-descriptor half (Hex/Number/Enum/Box/Span) is LOCK-3's, not LOCK-7's
  (Q6); LOCK-7 owns only the true-closure residue.** *Open sub-question exported: the typed shape of
  `HostClosure{fn_id, arg, ret}` and its per-subsystem floor.*

---

## The ownership block (report its three horns; do not resolve)

4D-as-*shared-runtime* is blocked by a missing primitive of equal-or-greater difficulty than the
original problem; 4D-as-*shared-ownership* is DAG-blocked on the predecessors above. The three horns,
stated so synthesis need not re-derive them:
- **Horn (i) — the port primitive.** A full `GrammarIR`+interpreter+optimizer port to TS is
  re-implementing the grammar plane's engine in a second language. But the fast TS face is per-language
  emission (a build-time emitter), not a shared runtime — so the *live* 4D role does not require this
  primitive; the horn kills only "one VM, two sides, for free."
- **Horn (ii) — the wasm crossing.** Full-walk value materialization across the wasm boundary measured
  a *loss* (~0.91×) on exactly the workload a value-producing combinator parser IS. The crossing wins
  stop-early, loses full materialization.
- **Horn (iii) — the shared-decoder graft.** A runtime switch-loop decoder is measured adverse on V8
  (≈−22%); its *fast* form needs the same build-time emitter primitive. 4D's crossing is correct and
  isomorphic *as data*, slow to *run* as a runtime decoder.

**Net:** 4D's `bbnf_ir`-as-shared-type parsimony dominates *conditional on* LOCK-5 ruling the shared
type (not a functor) AND the block clearing; its Rust runtime is a measured loss and its fast face is
the same per-language emitter every other family lowers through. Do not carry "4D dominates on
parsimony ⇒ elect it" — the dominance is conditional and its runtime is not the fast face.

---

## Prototype obligations (what a probe must test to move 4D)

A single scratch-copy prototype (isolated `CARGO_TARGET_DIR`, external patch-paths repointed
read-only) settles the family:

1. **Cycle-break gate-0.** `Dfa` in `bbnf_regex`, `parse_that` deps `bbnf_ir`, compiles clean.
   Born-RED; blocked on LOCK-5 if it does not compile.
2. **The value-plane census as a compile test.** Land tier-1 `MapExpr::Array` + `MapExpr::Select` +
   `FnDescriptor::Record`; re-lower the 44 latex-paper object-literal projections; assert they
   round-trip through `constant_fold` and serialize — proving ~52% of the barrier dissolves as *data*,
   not opacity. Count the true-`Opaque` residue on value.js easing (expect 3).
3. **The value-plane completion (the never-built backbone).** Land the four mechanical edits (Q6): the
   descriptor/`ApplyFn` opcode, `fns: Vec<FnDescriptor>` on `BytecodeProgram`, `Value` scalar arms, one
   interpreter handler. Gate: VM value-projected output **byte-identical to the closure core `.map`** on
   a projection-dense grammar (differential oracle). This converts the substrate from recognizer to
   combinator core — the earlier "already built" reading was false on the tree.
4. **The setup-pool paired number (the decisive Q7 datum).** Paired-cold single-parse of a
   value.js-sized input (10–40 B), `Interpreter::new` vs a pooled `reset(program,input)`; report the
   allocation-share of single-parse cost and the `1/N` decay across N∈{1,10,100}; then the three-way
   split (setup / recognizer / value-plane) via the toggles.
5. **The one-plan two-lowering observable (Q5/Q6).** Serialize a JSON `BytecodeProgram`, decode in a
   ~200-LOC TS switch-loop, assert opcode-stream identity vs a *live* Rust golden AND
   `Value`==`JSON.parse` across ≥6 corpora (canada mandatory).
6. **The projection-dense two-terminal.** interpret+`ApplyDescriptor` vs the AOT emitter vs closures, on
   the value.js color/units shape. **If closures win the LL(1)/projection shape, report it as the
   family's negative result** — a legitimate close, not a burial.

---

## Prior art (citations)

- **GHC Core** — surface sugar (do-notation, list comprehensions, `RecordWildCards`) desugars to one
  small typed Core the whole compiler optimizes: the exact "sugar over shared IR, inherit the optimizer"
  shape; its record-desugaring is direct precedent for tier-1 `FnDescriptor::Record`.
- **MLIR progressive lowering** — `build/` sugar → `bbnf_ir` → {interpret, AOT, TS-decode} is a
  three-terminal dialect-lowering tower; MLIR's law that *analysis passes live on the shared IR, not a
  front-end* is exactly why dispatch-synth/FIRST-FOLLOW must run on `GrammarIR` (`crates/ir/src/passes/`).
- **First Futamura Projection** (Jonnalagedda OOPSLA'14, flap PLDI'23) — partial-evaluating a combinator
  interpreter against a comptime-known grammar yields straight-line code; the literature's only bridge
  from a grammar *value* to monomorphized code is build-time codegen, grounding Q1.
- **Truffle/GraalVM partial evaluation** — a runtime self-optimizing AST partial-evaluated to native
  code: the closest analogue to "runtime graph, monomorphized only at a codegen boundary," with the
  lesson that the boundary must be a *stable serializable form* (`BytecodeProgram`).
- **tree-sitter / LALRPOP / Menhir** — construction-DSL → shared table → generated runtime; parse-that's
  ordered/PEG `Alt` semantics (`node.rs:46`) match the ordered-choice tier, not GLR.
- **egg / egglog** — the `fuse(Graph)→Graph` saturation 4D *reuses* via in-tree `bbnf_ir::egraph`
  (`crates/ir/src/egraph/`); 4D forks no second engine.
- **nom / winnow** — the closure-chain anti-pattern the two-terminal number measures against.

---

## Named risks and counterexamples

- **R-CYCLE.** The Cargo cycle (`crates/ir/Cargo.toml:13` ↔ 4D's need) is a hard build failure, not a
  smell. A probe that builds `IrNode` from parse-that *without* first extracting `Dfa` will not compile.
  Guard: gate-0 born-RED on the cycle break.
- **R-RECORD-CREEP.** Tier-1 `Record`/`Array`/`Select` risks re-growing `MapExpr` into a general
  expression language (a mini-god-node). Guard: three data variants only, each `constant_fold`-total;
  anything needing control flow or capture is `Opaque`, not a new `MapExpr` arm.
- **R-HOSTCLOSURE.** value.js's 3 easing closures tempt an inlined `Box<dyn Fn>` in the node —
  unserializable, breaking the msgpack bus and the TS graft. Guard: `Opaque(FnId)` indexes a registered
  `fn_table`, never inlines; `Value::Host` is a handle, not a closure.
- **R-VALUE-PLANE-FALSE-GREEN.** The VM today discards `fn_id` (`compiler/node.rs:31`); a demo that
  claims "combinators inherit the VM" on a projection-dense grammar silently drops every `->`. Guard:
  obligation 3 forces byte-identity vs the closure `.map`, not just a spine parse.
- **R-DUAL-LOWERING-DRIFT.** The AOT emitter (LOCK-3, the only current descriptor evaluator) and the
  interpret terminal can silently diverge on Hex/Number/Enum semantics. Guard: Q6's `Value`-tree oracle
  runs AOT-vs-interpret, not just interpret-vs-TS.
- **R-CLOSURE-WINS.** Closures may beat the staged path on LL(1)/projection shapes — a legitimate close
  (a claimed +10% dispatch-lane win once collapsed to +5% under fresh paired windows). Report, do not
  bury.

---

## Summary judgment (for synthesis, not a verdict)

The value-plane wall is now a *counted, split* barrier. **44 of 84 (~52%) latex-paper projections are
object-literal/array/ternary constructions recoverable as analyzable data** via three plain
`MapExpr`/`FnDescriptor` nodes (`Record`, `Array`, `Select`) — not opacity; the irreducible
`Opaque(HostFnId)` residue is **exactly 3 sites, all value.js easing closures**, comfortably under the
≤2%-of-nodes floor in aggregate (fragile only in closure-dense sub-grammars, Q3). The descriptor
evaluator splits cleanly: **Hex/Number/Enum/Box/Span are LOCK-3's** (evaluated only in the AOT emitter
today; VM parity via one descriptor opcode), **the true closures are LOCK-7's** (Q6). The combinator
graph reaches monomorphization *only* as build-time codegen over the serialized plan (Q1); that same
serialized `BytecodeProgram` is the one-plan/two-lowering observable, exact for {interpret, TS-decode}
and oracle-only for AOT (Q5). The setup tax is **7 unconditional heap allocations per
`Interpreter::new`** (`mod.rs:122-129`), fully eliminable by a pooled `reset` — but pooling rescues the
*oracle*, not a competitive terminal (Q7). The correction of record: **the VM value plane is UNBUILT**
— `Map`'s `fn_id` is discarded at compile, `Op` has no `ApplyFn`, `Value` has no scalar; making the
substrate a combinator core is four mechanical edits, the family's central UNBUILT completion. Hard
predecessors: LOCK-5 (breaks the `Dfa` Cargo cycle, rules shared-type-vs-functor) and LOCK-7 (types the
3-site `Opaque` residue only). The single scratch-copy prototype — cycle-break, tier-1
census-as-compile-test, value-plane completion, setup-pool number, one-plan observable,
projection-dense two-terminal — moves the family or returns its honest negative result.
