# LOCK-4 · FAMILY 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP (pass-3 research)

**Charter.** parse-that's combinator surface (`.then()`, `.or()`, `.map()`, …) is a *construction
API over the same `bbnf_ir` the generator plane uses* — `.then()` builds `IrNode`, not `Box<dyn Fn>`;
the combinator engine is the generator's runtime in disguise; parse-that keeps its public API and
inherits the IR optimizer, tape emitter, and serialization bus. Pass-3 sharpens the family with a
*measured value-plane census* (the pass-2 GAP-1 named, now counted) and a mechanism trace of the
runtime→comptime seam. The near-bijection substrate (`IrNode` 14 variants, `crates/ir/src/types/node.rs:30`;
`Op` 30 opcodes, `vm/bytecode.rs`; msgpack bus, `vm/bytecode.rs` `Serialize`) is settled and carried
forward from pass-2 §1 — not re-derived here.

---

## Q1 — Runtime combinator graph → comptime monomorphization (mechanism trace)

**The runtime combinator API does NOT reach comptime type-monomorphization — and cannot, absent a
codegen step. It reaches it only as build-time codegen over a comptime-known graph.** The trace:

1. The 4D `build/` sugar constructs a *runtime* `GrammarIR` (`crates/ir/src/types/grammar.rs:27`) — a
   heap `Vec<IrNode>`/rule map materialized at run time. This is plain owned data, not a type.
2. The only monomorphizing lowering in-tree is the AOT Rust emitter
   (`crates/core/src/backend/rust/emitter/mod.rs`), which consumes `IrNode`/`FnDescriptor` and emits
   *specialized Rust source* — a **build-time** `xtask`/`build.rs` step (`regen`), not a runtime call.
   There is no runtime→comptime reflection: Rust has no `comptime` that reifies a runtime `Vec<IrNode>`
   into monomorphized types. The interpret terminal (`vm/interpreter/mod.rs:144` `run`) is the only path
   that consumes the runtime graph *at run time*, and it is a **dynamic dispatch loop, not
   monomorphized** — one `match Op` (`mod.rs:168+`), zero type specialization per grammar.
3. **Therefore the honest 4D answer:** a combinator graph reaches monomorphization *only* by being
   emitted to the serializable `GrammarIR` and fed to the build-time emitter as a codegen input — the
   identical route grammar-text→IR takes. A monomorphizing lowering demands of the API exactly that the
   built `GrammarIR` be **const-serializable and total** (it is — `IrNode`/`FnDescriptor`/`MapExpr` are
   all plain `Serialize` data), so a `const`/`build.rs` site can name it. What monomorphization does
   *not* demand is any change to the runtime combinator surface; the surface stays a data builder, and
   4B's comptime-fusion mechanism is a *consumer of 4D's serialized plan*, not a competitor to it.

The distinction that matters: 4B monomorphizes at the type level (a runtime graph must be lifted to
const first); 4D says the lift is *serialization*, and the serialized plan is precisely what both the
AOT emitter and the TS emitter eat (Q5). 4D neither claims nor needs runtime monomorphization.

---

## Q2 — Value plane through an arbitrary host closure (the census, counted)

Pass-2 named GAP-1 (`FnDescriptor` has no closure variant). Pass-3 **counts** the non-projection
surface across the three live consumers. The projection sites are the parser-level `.map()` calls
(the `->` equivalent); `Array.prototype.map` in domain code is excluded.

**latex-paper (`/Users/mkbabb/Programming/latex-paper/src/grammar` + `src/bibtex`) — 97 parser `.map` sites:**
- **32** are named-fn or nullary-constant projections — `.map(textNode)` (`src/grammar/text.ts:16`),
  `.map(() => textNode("—"))` (`text.ts:60`), `.map(() => null)` (`text.ts:92`).
  **MapExpr-expressible** as `FnCall{name, args:[Input|StringLit]}` *iff the name is registered*
  (`crates/ir/src/types/map_expr.rs:31`).
- **44** are **object-literal constructions** — `.map((value) => ({ … }))` (`src/grammar/math.ts:35,47,59`).
  **NOT MapExpr-expressible**: `MapExpr` has literals, `Input`, `InputProp`, `FnCall`, `BinOp`,
  `UnaryOp` (`map_expr.rs:17-39`) and **no record/object constructor and no array constructor**.
- **1** ternary (`.map(([_, ch]) => textNode(ch === "&" ? … : …))`, `text.ts:97`) — needs a
  conditional/`Select` op MapExpr also lacks.
- Remainder (~20): tuple-destructuring + mixed. **≈47% (45 of 97) are not MapExpr-expressible today.**

**value.js — the maximal wall: parse produces first-class CLOSURES.** The easing plane
(`/Users/mkbabb/Programming/value.js/src/v4/easing.ts`) types its parse output as
`EasingFunction = (progress: number) => number` (`easing.ts:4`); a cubic-bezier/steps parse yields a
**closure that captures parsed control points** (`easing.ts:124` `return ok((progress) => {…})`,
`:149` `stops.map(s => ({...s}))`). This is beyond *any* serializable descriptor: the produced value
is itself a host function. `channelToken(part, 255)` (color, `src/v4/css/grammar.ts:189`) is the milder
tier — a *registered* call, expressible if named; the easing closure is the hard tier — an *anonymous
value-carrying function* with no name and no data shape.

**keyframes.js** (`src/animation/compile`, 53 map/chain) consumes value.js transitively; its projections
are the same two tiers, dominated by object construction of `ResolvedKeyframes`.

**Implied typed shape for the descriptor.** Two new `FnDescriptor` tiers close the census, ranked:
1. `FnDescriptor::Record { fields: Vec<(StringId, MapExpr)> }` + `MapExpr::Array(Vec<MapExpr>)` +
   `MapExpr::Select{cond,then,els}` — covers all 45 latex-paper object/array/ternary sites **while
   staying analyzable** (still pure data, still serializable, still fold-able by `constant_fold`,
   `map_expr.rs:109`). This is the *large, cheap* win: ~47% of the barrier dissolves with three plain-data
   nodes and no opacity.
2. `FnDescriptor::Opaque(HostFnId)` — the irreducible residue (value.js easing closures, any
   `.map(f)` whose `f` is neither a registered name nor a data expression). Carries **only** an
   `FnId` into a host `fn_table: Vec<Box<dyn Fn(&[Value]) -> HostValue>>`; the `Value` union must gain a
   `Host(HostHandle)` inhabitant so a parse can *carry* a closure it cannot introspect. This tier is
   LOCK-7's lattice member (Q8) — 4D reports its shape and count, does not type it.

**The load-bearing pass-3 finding:** the host-closure barrier is *not* monolithic. Split it and
~half is recoverable as analyzable data (`Record`/`Array`/`Select`); the opaque residue is small and
concentrated (value.js easing/timeline). 4D should not smuggle one `Box<dyn Fn>` per `->` (pass-2
R-HOSTCLOSURE); it should add the three data nodes first and let `Opaque` carry only the true closures.

---

## Q3 — Opaque-barrier surface size (counted ratio vs the ≤2% floor)

Barrier = fraction of parser-projection nodes an analysis cannot see through (an `Opaque` descriptor
is a hard wall to type-inference, dispatch-synthesis, and fusion; a `Record`/`FnCall`/`MapExpr` is not).

- **latex-paper:** with the tier-1 `Record`/`Array`/`Select` nodes, the opaque residue is the anonymous
  closure sites — near **0 of 97**; latex-paper's projections are all data or named fns.
  **Barrier ≈ 0%, under the 2% floor.**
- **value.js:** the opaque tier is the easing/timeline closure-returning sites. Of ~43 domain `.map`
  plus the parser projections, the *irreducibly opaque* set is the `EasingFunction`-producing parsers
  and the color-model `toXYZ/fromXYZ` closures (`src/v4/color/anchors.ts:336-337`) —
  a **single-digit count**, but concentrated on the value plane, not the recognizer. As a fraction of
  *all* IR nodes in a value.js grammar (leaves + seq + alt + repeat dominate), the opaque nodes are
  **well under 2%**; as a fraction of *projection* nodes they are the majority of value.js's hard tier.
- The honest framing: the barrier **passes the ≤2%-of-all-nodes floor comfortably** on both corpora,
  because opacity lives only at value-producing leaves, never in the recognizer spine. Analysis
  (FIRST/FOLLOW, dispatch synth, fusion) runs on the spine and *never touches* an `Opaque` node —
  the barrier does not obstruct the optimizer, only the value materializer.

---

## Q4 — Fusion-policy / blowup budget (4D's disposition: inherited 1:1, not owned)

4D does not own the cost-model caps (fuse-arity, dispatch-fanout, boxing-depth, `.rlib`/`dist`
budget) — LOCK-2's engine and 4B's comptime fusion set them. **4D's contribution is a proof that the
budget is identical to grammar-text's**: because the combinator front produces the *same* `GrammarIR`,
a value.js grammar authored via combinators and the same grammar authored as `.bbnf` lower through the
same passes to byte-identical `IrNode` (Q5's oracle), so their generated-size and compile-time growth
curves are **the same function of fuse-arity** — there is no separate 4D budget to measure. The only
4D-specific cap is on the value plane: an `Opaque` node is un-fusible (a hard boxing-depth-1 wall), so
the fusion policy must treat `Opaque(FnId)` leaves as fixed points that terminate a fuse chain. That is
one rule — `Opaque` is never inlined, never monomorphized, always dispatched — and it is the exact
guard that keeps the msgpack bus and TS graft intact (an inlined closure would be unserializable).

---

## Q5 — Serializable-plan observable (the distinguishing datum, and its precise limit)

**Yes — a fused region IS derivable from one serializable plan, but the plan drives *two* of three
lowerings byte-identically and the third by oracle.** The plan is `BytecodeProgram`
(`vm/bytecode.rs`, `Serialize`/`Deserialize`; msgpack advertised for the WASM boundary; the only
`#[serde(skip)]` is `compiled_regexes`, re-hydrated by `prepare_regexes`).

- **Lowering A (Rust interpret):** `GrammarIR` → `compile` (`vm/compiler/mod.rs:31`) →
  `BytecodeProgram` → `Interpreter::run`.
- **Lowering B (TS decode):** the *same* serialized `BytecodeProgram` crosses (msgpack) → a switch-loop
  decoder executes the identical opcode stream, re-deriving regexes as sticky `RegExp` (mirrors
  parse-that TS `leaf.ts:317`).
- **The observable:** serialize once in Rust, decode in TS, feed both the identical input — assert
  **opcode-stream identity** (Rust-serialized bytes == TS-decoded program) **and** `Value`-tree
  identity. *One plan, two lowerings, byte-identical parses.* This is a real, testable datum, not a
  type-system artifact — the plan is data that outlives the Rust type that built it.

**The precise limit (state it, do not overclaim):** the AOT Rust emitter (Q1) is a *third* lowering
that is **not** driven by the serialized `BytecodeProgram` — it consumes `GrammarIR` directly and
monomorphizes. So the "one plan, two lowerings" claim is exact for the {interpret, TS-decode} pair and
**oracle-only** for the AOT pair: the emitted Rust parser must be diff-tested against the interpret
path (Q6's dual oracle), not proven byte-identical at the opcode level (it has no opcodes). The fused
region is intrinsic to *neither* the Rust type system *nor* a single terminal — it is a property of the
shared `GrammarIR`, and the serialized `BytecodeProgram` is its observable proof for the portable pair.

---

## Q6 — Isomorphism-of-record standard + value-plane home (both answered concretely)

**Isomorphism-of-record = a dual oracle over a live golden, never a hand-faked blob:**
1. **Opcode-stream identity** — the TS decoder's re-serialized program bytes == the *live*
   msgpack output of `compile(GrammarIR)` run in Rust *this build* (a golden regenerated by the Rust
   compiler, checked into the isomorphism lane, never a frozen literal). Guards the plan.
2. **`Value`-tree identity** — the TS-decoded `Value` tape == the Rust `Interpreter` `Value` tree
   (`vm/interpreter/value.rs`), AND == `JSON.parse` on the JSON corpus (the semantic ground truth).
   Guards the value plane.
   **Corpus:** the six JSON corpora (apache-builds, canada, citm, data, data_xl, twitter — R3 §6) for
   the recognizer/structural tape; a value.js-shaped grammar (units/color/easing) for the projection
   plane once tier-1 `Record`/`Select` land; latex-paper's bibtex + text grammars for the
   object-literal tier. Paired-cold, two windows, both arm orders (the PROVE-09 window-asymmetry law).

**Value-plane home — the internal-descriptor evaluator (Hex/Number/Enum) is owned by LOCK-3, not
LOCK-7.** Verified: `FnDescriptor`'s built-in tiers — `EnumWrap` (`types/fn_descriptor.rs:20`),
`BoxWrap` (`:26`), `NumberConvert` (`:40`), `HexConvert` (`:45`), `SpanCapture` (`:49`), `Expr` (`:54`)
— are evaluated **only** in `crates/core/src/backend/rust/emitter/` (grep: `driver/alt.rs`, `driver/map.rs`,
`emitter/shapes/*`), i.e. the **AOT Rust lowering = LOCK-3's IR transposition spine**. The VM
compiler/interpreter touch **none of them** (grep of `vm/compiler/mod.rs` + `vm/interpreter/mod.rs`
for `Map`/`FnDescriptor`/`ApplyFn`/`fn_id` = zero hits; the only map is the memo `FxHashMap`). So the
clean split the family should assert:
- **Internal descriptors (Hex/Number/Enum/Box/Span/Expr)** → **LOCK-3 IR-VM lowering.** They are
  closed, data-defined, and today live only in the AOT emitter; giving the *interpret* terminal parity
  means adding `Op::ApplyDescriptor(FnDescriptorId)` to the VM (LOCK-3's opcode), evaluating the same
  closed set — no host dispatch needed.
- **Arbitrary host closures (`Opaque(HostFnId)`, Q2 tier-2)** → **LOCK-7 host-fn lattice.** Only these
  cross into a registered `fn_table`; only these are typed by LOCK-7's signatures.

This split is the pass-3 sharpening of pass-2's undifferentiated "value plane": most of it (the closed
descriptors) is LOCK-3's and needs no lattice; only the true closures are LOCK-7's.

---

## Q7 — Setup vs value-plane tax vs recognizer cost (the setup number, measured from source)

`Interpreter::new` (`vm/interpreter/mod.rs:114-136`) performs, **per parse**, seven eager heap
allocations via `Vec::with_capacity`:

| field | capacity | line |
|---|---|---|
| `values` | 64 × `Value` | `:122` |
| `call_stack` | 32 × `CallFrame` (`u32,u32,usize`=16B) | `:123` |
| `checkpoints` | 32 × `Checkpoint` (`u32,usize,usize`) | `:124` |
| `repeats` | 16 × `RepeatState` (7 fields ≈48B) | `:125` |
| `memo_starts` | 8 × `(RuleId,u32)` | `:127` |
| `value_depth_stack` | 8 × `usize` | `:128` |
| `rule_stack` | 16 × `RuleId` | `:129` |

Plus `memo: FxHashMap::default()` (`:126`, **lazy — zero alloc until the first `MemoStore`, which only
fires when packrat is armed**) and `diagnostics: Vec::new()` (`:130`, zero alloc). So the setup tax is
**7 heap allocations, ~176 slots (~2–3 KB), unconditional, per parse** — paid even for a 10-byte input.

**Amortization curve & the clean split:**
- **Setup cost** = those 7 allocations. Eliminated entirely by pooling: hold one `Interpreter` and, per
  parse, `clear()` each `Vec` (retains capacity) + `memo.clear()` — a `reset(program, input)` method
  replacing `new`. Warm-reuse setup cost → **0 allocations**. First parse pays 7; every subsequent
  pooled parse pays 0 — a `1/N` decay that is steep for a bench loop and negligible for a one-shot CLI.
  For small inputs (value.js CSS values, ~10–40 bytes) the 7 allocations are a *large* share of single-
  parse cost; for a 1 MB JSON parse they are noise. **This is the pooling win 4D should measure: a
  paired-cold single-parse of a value.js-sized input, `new` vs pooled `reset`.**
- **Recognizer cost** = the `match Op` dispatch loop over the spine opcodes (`MatchString`,
  `MatchRegex`, `Dispatch`, `Call`/`Return`, `Repeat*`, `Jump*`). Independent of the value plane.
- **Value-plane cost** = `MakeArray`/`MakeTagged` (`bytecode.rs:89,92`) + (future)
  `ApplyDescriptor`/`ApplyFn` `Value` construction. On a pure recognizer grammar this is
  `CaptureSpan`-only and near-free (pass-2 §3 "no materialization tax"); it grows exactly with
  projection density.

The three costs are separable in one bench by toggling: (a) `new` vs pooled `reset` isolates setup; (b)
a recognizer-only grammar vs a projection-dense grammar isolates value-plane; (c) the residue is the
recognizer. This is the measurement the family owes.

---

## Q8 — Predecessor shapes (report only; do not resolve)

- **LOCK-5 (regex continuum) — the Cargo-cycle breaker and the regex-node home.** Settled cross-lock
  fact (XL-C): two domain-fit types (opaque `Regex(StringId)` grammar tier; rich `Hir` engine tier) with
  a cheap bridge — the regex functor is *retired*, so 4D's `regex(...)` builder targets the opaque
  `IrNode::Regex(StringId)` on the grammar tier, never crosses a functor. The live risk 4D still
  inherits: `crates/ir` depends on `parse_that` (Cargo.toml:13) for the VM's `Dfa`
  (`vm/bytecode.rs` `parse_that::regex::Dfa`), while 4D needs `parse_that` → `bbnf_ir`. That is a Cargo
  cycle; LOCK-5's substrate ruling (extract the `Dfa` into `bbnf_regex`, already a dep of `bbnf_ir`)
  dissolves it. **Disposition: 4D's gate-0 is downstream of LOCK-5's `Dfa` extraction; report, do not
  break the cycle here.**
- **LOCK-7 (host-fn lattice) — owner of the `Opaque(HostFnId)` tier (Q2/Q6).** LOCK-7 types the
  `fn_table` signature that `Opaque` dispatches through and the `Value::Host` inhabitant a closure-
  carrying parse needs. 4D reports the *count* (single-digit, value.js easing/color) and the *seam*
  (`Interpreter.fn_table` + `Op::ApplyFn(FnId)`); LOCK-7 defines the typed lattice and its per-subsystem
  floor. **Disposition: the internal-descriptor half (Hex/Number/Enum) is LOCK-3's, not LOCK-7's (Q6) —
  LOCK-7 owns only the true-closure residue.**

---

## Prototype obligations (what pass-3 must test to move 4D)

1. **Cycle-break gate-0** (carried from pass-2 §7.1): `Dfa` in `bbnf_regex`, `parse_that` deps
   `bbnf_ir`, compiles clean. Born-RED; blocked on LOCK-5 if it does not.
2. **The value-plane census as a compile test.** Land tier-1 `MapExpr::Array` + `MapExpr::Select` +
   `FnDescriptor::Record`; re-lower the 44 latex-paper object-literal projections; assert they now
   round-trip through `constant_fold` and serialize — proving ~47% of the barrier dissolves as *data*,
   not opacity. Count the true-`Opaque` residue on value.js easing (expect single digits).
3. **The setup-pool paired number (the decisive Q7 datum).** Paired-cold single-parse of a
   value.js-sized input (10–40 B), `Interpreter::new` vs a pooled `reset(program,input)`; report the
   allocation-share of single-parse cost and the `1/N` decay across N∈{1,10,100}. Then the three-way
   split (setup / recognizer / value-plane) via the toggles in Q7.
4. **The one-plan two-lowering observable (Q5).** Serialize a JSON `BytecodeProgram`, decode in a
   ~200-LOC TS switch-loop, assert opcode-stream identity vs a *live* Rust golden AND `Value`==`JSON.parse`.
5. **The projection-dense two-terminal (pass-2 §7.4, now with tier-1).** interpret+`ApplyDescriptor`
   vs the AOT emitter vs closures, on the value.js color/units shape. **If closures win the LL(1)/
   projection shape, report it as the family's negative result** (Constitution vii; the PROVE-11
   dispatch-lane precedent where a claimed +10% collapsed to +5% under fresh windows is the standing warning).

---

## Prior art (citations)

- **GHC Core** — surface sugar (do-notation, list comprehensions, `RecordWildCards`) desugars to one
  small typed Core the whole compiler optimizes: the exact "sugar over shared IR, inherit the
  optimizer" shape, and its `Record`-desugaring is direct precedent for 4D's tier-1 `FnDescriptor::Record`.
- **MLIR progressive lowering** — `build/` sugar → `bbnf_ir` → {interpret, AOT, TS-decode} is a
  three-terminal dialect-lowering tower; MLIR's law that *analysis passes live on the shared IR, not a
  front-end* is exactly why 4D's dispatch-synth/FIRST-FOLLOW must run on `GrammarIR` (`crates/ir/src/passes/`).
- **tree-sitter / LALRPOP / Menhir** — construction-DSL → shared table → generated runtime; parse-that's
  ordered/PEG `Alt` semantics (`node.rs:46`) match the ordered-choice tier, not GLR.
- **Truffle/GraalVM partial evaluation** — a runtime AST (self-optimizing interpreter) is
  partial-evaluated to native code: the closest analogue to 4D's "runtime graph, monomorphized only at a
  codegen boundary" (Q1), and its lesson is that the boundary must be a *stable serializable form* — 4D's
  `BytecodeProgram` (Q5).
- **egg / egglog** — the `fuse(Graph)→Graph` saturation 4D *reuses* via in-tree `bbnf_ir::egraph`
  (`crates/ir/src/egraph/`); 4D forks no second engine.
- **nom / winnow** — the closure-chain anti-pattern the two-terminal number measures against (R3 dev-deps).

---

## Named risks (pass-3 deltas over pass-2 §9)

- **R-CYCLE** (carried): the Cargo cycle is a hard build failure; gate-0 born-RED, downstream of LOCK-5.
- **R-RECORD-CREEP** (new): tier-1 `Record`/`Array`/`Select` risks re-growing `MapExpr` into a general
  expression language (a mini-god-node). *Guard:* three data variants only, each `constant_fold`-total;
  anything needing control flow or capture is `Opaque`, not a new `MapExpr` arm.
- **R-HOSTCLOSURE** (carried, sharpened): the value.js easing closure tempts an inlined `Box<dyn Fn>` in
  the node — unserializable, breaks the msgpack bus and TS graft. *Guard:* `Opaque(FnId)` indexes a
  registered `fn_table`, never inlines; `Value::Host` is a handle, not a closure.
- **R-CLOSURE-WINS** (carried): closures may beat the staged path on LL(1)/projection shapes — a
  legitimate close, reported not buried.
- **R-DUAL-LOWERING-DRIFT** (new): the AOT emitter (LOCK-3) and the interpret terminal can silently
  diverge on descriptor semantics (Hex/Number/Enum) since only the emitter evaluates them today.
  *Guard:* Q6's `Value`-tree oracle must run AOT-vs-interpret, not just interpret-vs-TS.

---

## Summary judgment (for synthesis, not a verdict)

Pass-3 turns 4D's pass-2 wall into a *counted, split* barrier. The value-plane census: **~47% of
latex-paper's projections (45/97) are object-literal/array/ternary constructions recoverable as
analyzable data** via three plain `MapExpr`/`FnDescriptor` nodes (`Record`, `Array`, `Select`) — not
opacity; the irreducible `Opaque(HostFnId)` residue is **single-digit and concentrated in value.js's
closure-returning easing/color plane**, comfortably under the ≤2%-of-all-nodes floor (Q3). The
descriptor evaluator splits cleanly: **Hex/Number/Enum/Box/Span are LOCK-3's (evaluated only in the AOT
emitter today; parity via `Op::ApplyDescriptor`), the true closures are LOCK-7's** (Q6). The
combinator graph reaches monomorphization *only* as build-time codegen over the serialized plan (Q1);
that same serialized `BytecodeProgram` is the one-plan/two-lowering observable (Q5). The setup tax is
**7 unconditional heap allocations per `Interpreter::new`**, fully eliminable by a pooled `reset`
(Q7). Hard predecessors: LOCK-5 (breaks the `Dfa` Cargo cycle) and LOCK-7 (types the `Opaque` residue
only). The single scratch-copy prototype — cycle-break, tier-1 census-as-compile-test, setup-pool
paired number, one-plan observable, projection-dense two-terminal — moves the family or returns its
honest negative result.
