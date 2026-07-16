# LOCK-4 — THE COMBINATOR CORE · pass-1 SYNTHESIS · FAMILY-SPECS

*Synthesis over research-{A,B,C,D,priorart}. One cogent specification per family while the routes
remain incompatible; then the incompatibility map; then the two sharpest prototype briefs. Under
the CONSTITUTION (grammar totality save primitives+combinators; true isomorphism; rust/ts
isomorphism to a measured point; the refutation laws — no warm benches, paired interleaving
mandatory even for obvious wins, x86 dead; type plane costs zero at runtime; no god modules;
consumer-less substrate forbidden). Elegance earns nothing on its own — every family is judged on
a concrete, falsifiable differentiator against parse-that's already-monomorphized, arity-unrolled,
zero-per-parse-alloc closure core; a family stalled at a missing primitive equal in difficulty to
the original problem is marked BLOCKED-candidate.*

---

## The shared substrate every family reforms

parse-that today is a runtime closure graph on both sides: Rust `Parser<'a,O>{ parser_fn:
SmallBox<dyn ParserFn+'a, S32> }` (`rust/parse_that/src/parse.rs:61`) — type-erased boxed closures,
one construction box per combinator, dynamic dispatch on every hot call; TS `Parser<T>` with a
`.parser` function-pointer dispatched megamorphically (`this.parser(state)`, R4 §6c "THE megamorphic
IC of the engine"), hand-specialized at hot leaves by arity-unrolling (`fuseAll` arity-2/3,
`dispatch` `Int8Array(128)`). Neither is monomorphized in the nom/winnow sense. Every family attacks
ONE seam: recover the static grammar structure the closures hid, then specialize it (4B), lower it to
a VM (4C), analyze it as a graph (4A), or re-home it onto bbnf's IR (4D).

**Three binding facts from our own evidence base outrank any external "win":**
1. **The arena ceiling is measured and small.** The `SmallBox<dyn>` boxes are **6 construction
   allocs / 5960 B**, paid once outside the timed loop; the per-parse mass is **5390 allocs** (Vec
   growth in `many`/`sep_by` + string arena), which an arena *cannot touch* and which *no family
   here eliminates* (orthogonal to the box question). A family selling "dissolves the arena" dissolves
   the **construction** box, not the per-parse mass — state it so.
2. **The refutation law (PROVE-09).** The `portable_simd→NEON` shim's claimed 1.34–1.57× was a
   two-arm window artifact; window A ran the *other* way. No throughput claim is admissible without
   cold, paired-interleaved, two-window, opposite-arm measurement. Warm benches are disinformation.
3. **The monadic-bind wall (T2).** Staging / graph analysis / VM lowering cannot see past `chain`
   (value→parser) / `mapState` / `map_with_state` / `map_with_ctx`. Every family's analytic reach is
   bounded by the density of these opaque nodes in the *real* corpus — an empirical quantity
   (PROBE-1), not a theoretical one. Early census signal: value.js has **4 `.chain` against ~575
   applicative/selective sites (~0.7%)** — the wall barely bites in the flagship consumer.

---

## FAMILY 4A — CLOSURE-TO-GRAPH

**Architecture.** The combinator API stops constructing nested closures and instead appends nodes to
a flat, first-class, analyzable **parser graph** — a parse-that-native `Node` arena with integer ids
(`enum Node { Terminal/Seq(NodeRange)/Alt(NodeRange)/Rep{child,min,max,sep}/Map{child,MapId}/Look/
Not/Minus/Bind/Ref(RuleId) }`, held in `ParserGraph{ nodes, terms, maps, binds, rules }`;
`Parser<'a,O> = { graph, root:NodeId }`). Fusion, first-set dispatch, packrat placement, and
diagnostics become **total graph passes** over that arena — derived, not hand-maintained. Runtime is
**extraction to a fused closure tree** (one closure per fused region, not per combinator); the graph
is a build-time IR, closures remain the execution substrate.

**Mechanism.** (i) *Fusion pass* — normalize by combinator laws (flatten Seq/Alt associativity, fuse
Map∘Map, drop empty Alt arms), extract one fused evaluator per maximal static region: reproduces
`seq!`/`alt!` (`macros.rs`, arities 2–8) and `fuseAll` (arities 2–3) as a *theorem*, for arbitrary
arity/nesting. Optionally run this as **equality saturation** over the in-tree `egraph` crate — the
same engine `rust/regex/src/egraph/` (1253 LOC) already runs on regex HIR, giving one e-graph home
(constitution vi). (ii) *First-set dispatch pass* — FIRST sets bottom-up (SCC fixpoint over `Ref`);
disjoint-FIRST `Alt`s lower to a synthesized `Int8Array(128)`/`[u8;256]` LUT, retiring the
hand-filled `dispatch()`. (iii) *Packrat-placement pass* — SCC + shared-node detection marks exactly
the memo points, retiring the manual `memoize()` opt-in. (iv) *Diagnostics pass* — FIRST/FOLLOW give
the static "expected" set per node, complementing runtime furthest-offset.

**The ownership dividend (genuine, distinct from a point-fix).** The `Node` arena is **plain
`'static` data with no `'a` closures inside**; `'a` enters only at extraction and parse. So the
86-site lifetime rewrite that made a `SmallBox` bump-arena *unsound* (`ParserFn<'a>` is
`'a`-parameterized) **does not arise** for the node arena — reification sidesteps the arena question
structurally rather than patching it.

**Integration seams.** `parse.rs:61-77` (Parser core → graph handle + node-append);
`combinators/methods/{sequence,alternation,repetition,sep_by,minus,map,recover}.rs` +
`parser.ts:81-707` (builders → one-line appends); `macros.rs:6-302` + `leaf.ts:39-57,179-273`
(hand fusion → the fusion pass); `leaf.ts:100-151` (hand dispatch → the dispatch pass);
`packrat.ts:156,290` + `lazy.rs:21` (arming → placement pass); `parse.rs:18-21` + `utils.ts:6,28-49`
(expected-set → diagnostics pass); `rust/regex/src/egraph/` (reusable optimizer).

**Gates it would carry.** (G-A1) static-structure census ≥ ~85% static nodes on the real corpus —
**go/no-go for the whole family**. (G-A2) derived fusion ≥ hand `fuseAll`/`seq!` cold-paired on
twitter+citm (if only a tie, close as a *maintainability+generality* win, stated honestly). (G-A3)
module-init time ≤ ~1.2× eager-closure build. (G-A4) derived dispatch LUT byte-identical in effect to
hand `dispatch()`. (G-A5) SCC packrat placement == hand `memoize()` set; passes existing packrat
proofs. (G-A6) `Node` arena round-trips rust↔ts byte-exact (tape-crossing method).

**Exact open questions.** Does e-graph saturation find fusions a linear pass misses at acceptable
build cost, or is a fixed rewrite ordering sufficient? Does extraction-to-closures provably reach the
macro-collapsed single-box shape, or does naive per-node extraction lose? Where exactly does the
rust/ts mirror boundary fall (the Node enum + passes mirrored; extraction idiomatic) — a LOCK-6
dependency, not self-contained.

**Verdict: ADVANCE.** No missing primitive — the egraph engine and SCC fixpoint are in-tree/standard;
the ownership dividend is real. Risk is measurement (extraction ≥ hand-tuned) + a LOCK-6
isomorphism-linter dependency. NOT blocked.

---

## FAMILY 4B — COMPTIME-FUSION

**Architecture.** The combinator tree is known before the first byte; **compile it away** — Rust by
monomorphization, TS by a build-time fuser — into specialized, inlined, allocation-free code with
**no runtime combinator graph**. The vtable/SmallBox question is not answered, it is *dissolved*: a
monomorphic chain has no vtable. Two faces of one staging discipline over the applicative/selective
fragment; only monadic `chain` and recursion stay dynamic.

**Mechanism.** *Rust face* — generic combinator structs owning children by value and implementing
`ParserFn` directly via the blanket-impl hook (`parse.rs:41-53`): `Seq2<P0,P1>` has a concrete type,
its `call` inlines both children — no `dyn`, no `SmallBox`, no heap box, **no `'a`-scoped arena to
thread** (value-owned structs need no allocator). `a.then(b).then(c) : Seq2<Seq2<A,B>,C>`, one
monomorphic `call`, zero indirection — the generalization of `seq!`/`alt!` from arity-capped macros
to transparent types. *TS face* — a build-time fuser (vite plugin) consumes the static parser
description and **emits specialized JS**: arity-N sequence unrolled, choice compiled to a first-byte
`switch`, maps inlined, no `Parser` objects surviving into `dist/`. *The single erasure boundary* —
one `into_dyn`/box per **named rule** at the `lazy` seam (`lazy.rs:20-64`); fusion is *within* a rule,
recursion re-enters through the rule's one box. This is GHC stream-fusion (fuse the pipeline,
materialize at the boundary), which also bounds the monomorphization blowup.

**The reward is a different quantity than the arena.** Not alloc (6 construction allocs — the arena
ceiling) but eliminating the per-combinator indirect **call** and unlocking **cross-combinator
inlining**, which `SmallBox<dyn>` structurally forbids. The gate is therefore cyc/B + a disassembly
`blr`-count, never alloc count.

**Integration seams.** `parse.rs:41-53` (hook), `:61-77` (what fused structs replace on hot chains);
`combinators/methods/sequence.rs:10-45` (return `Seq*`/`impl ParserFn` not `Parser::new(closure)`);
`macros.rs:1-302` (the shape to generalize); `lazy.rs:20-64` (the one dyn box per rule);
`leaf.ts:154-273` (`fuseAll` → arbitrary-arity emitter); `leaf.ts:100-151` (dispatch → compiled
switch); `vite.config.ts` (fuser hook); `test/benchmarks/pt-b3-fusion.bench.ts` (do-not-regress).

**Gates it would carry.** (G-B1) **Rust inlining win** — fused chain shows zero `blr` through the
parser vtable (disasm, hard) AND cold-paired cyc/B beats `SmallBox<dyn>` `.then()` on the six JSON
corpora, two windows, opposite arm order; report compile-time + binary-size delta (the R1 budget).
(G-B2) recursion still parses via one box at `lazy`. (G-B3) TS fuser: emitted region contains **no**
`.parser` indirection; 124/124 semantic parity; honest paired numbers (no dispatch over-claim — the
dispatch lever re-measured +10.2%→~+5%). (G-B4) differential Rust-fused ≡ TS-fused on result, final
offset, furthest_offset/diagnostics. (G-B5) fusion never crosses the memo/diagnostic rule boundary.

**Exact open questions.** Is cross-combinator inlining a *real, large* win over the already-arity-
unrolled closure core, or a marginal one (the PROVE-11 dispatch pattern)? — **the family's make-or-
break, PROBE-2.** What is the fusion *policy* that avoids monomorphization/emit blowup (fuse arity
2–4 + first-byte dispatch; lower long/uniform fan-out to a table)? Where does the rust/ts isomorphism
actually live given Rust uses monomorphized generics (which have *no TS analog*, T1) and TS uses
emitted code — is "isomorphism at the fused form" strong enough for constitution iii?

**Verdict: ADVANCE with the throughput gate.** No missing primitive (nom/winnow prove Rust
monomorphization; the TS fuser generalizes existing code). Weakest *isomorphism* of the four
(divergent Rust/TS mechanisms). Its entire perf case is unproven against the tuned closure core —
PROBE-2 is the gate. NOT blocked.

---

## FAMILY 4C — VM-CORE

**Architecture.** Replace the closure tree with a **flat program** — `Program{ code: Vec<Inst>,
actions, literals }` — run by one interpreter loop whose single dispatch site is a `match`/`switch`
over an integer opcode. The program is *data*: the same byte layout in both languages, so
**isomorphism is by construction** — it crosses rust↔ts the way the offset-tape already does
(PROVE-14, byte-exact), needing neither a linter (6A) nor a spec-generator (6B). Packrat and
diagnostics are VM facilities.

**Mechanism.** Adopt the LPeg parsing-machine ISA (~16 opcodes: `Char/TestChar/Set/TestSet/Span/Str/
Choice/Commit/PartialCommit/BackCommit/Call/Return/Fail/FailTwice/Jmp/End`) plus parse-that
extensions: `Dfa d` (run a compiled regex DFA — the LOCK-5 seam), `Action k` (host semantic action),
`Memo/MemoEnter` (packrat), `Throw/Label` (labeled failure). Combinators become **emitters**
(`a.or(b)` → `Choice L1; ⟦a⟧; Commit L2; L1: ⟦b⟧; L2:`; `many` → a two-instruction `PartialCommit`
loop; named rule → `Call/Return`, dissolving `Box<dyn LazyParserFn>` into a call-table index). The
emitter is **exactly the terminal consumer of 4A's static graph** — 4A's nodes are the pre-linking
program.

**The load-bearing decision — the hybrid action model.** A pure VM *cannot* encode arbitrary user
`.map`/`.chain` closures as bytecode. Resolution: a side-table `actions: Vec<Box<dyn Fn>>` — control
flow is bytecode, semantic actions are host closures reached by `Action k`, **firing on success paths
only, never on backtrack**. Consequences: the megamorphic indirect call is *narrowed from every
combinator step to only action sites*; captures fold on a capture stack (no per-node Vec growth on
the control path); in Rust the actions keep `'a` while the **control code is `'static`**, so the
arena/ownership question *dissolves* (one Program alloc, no per-combinator boxes, the
`smallbox[nightly]` blocker gone). Diagnostics = **one farthest-fail register, zero arrays** on the
default path (retiring the PROVE-11 array-elimination battleground); packrat = a `(pc,sp)` memo table
armed selectively (LPeg deliberately omits blanket memoization; literature: full memoization "always
slower").

**Integration seams.** `parse.rs:61,80,97` + `lazy.rs:21,31` (core → Program + loop + call-table);
`combinators/methods/*.rs` (emit rules); `leaf.rs:182,203,345` (`TakeUntilScan` → `Span`/`Set`);
`parsers/scan/*` + `regex/src/automata/dfa.rs:116` (kernels become instruction bodies — the VM
*orchestrates*, does not replace); `parser.ts:24-32,53,82,88,131` + `leaf.ts:44,49,62,140` (the
megamorphic sites → one `switch(op)`); `packrat.ts` → `Memo`; `utils.ts:6,28-49` → farthest-fail
register; the PROVE-14 tape decoder → the `Program` (de)serializer.

**Gates it would carry.** (G-C1) **Throughput, paired-interleaved cold** VM-JSON vs closure-JSON on
twitter/citm/canada — the honest expectation is **Rust ≈ 0.5–0.9× (loss)**, TS possibly >1 via
monomorphism; **the family must be prepared to report a Rust throughput loss and still advance on the
other axes, or retire.** (G-C2) V8 `--trace-ic`: the VM dispatch site is monomorphic vs the closure
core's megamorphic `this.parser`. (G-C3) byte-exact `Program` crossing rust↔ts (isomorphism proof).
(G-C4) allocation: 1 program + 2 reused stacks vs N boxes. (G-C5) diagnostics alloc-zero default,
per-parse-scoped when armed. (G-C6) bounded left-recursion (grow-the-seed) correctness parity vs
`packrat.ts`.

**Exact open questions.** Can a portable `match`/`switch` dispatch loop (Rust/TS lack computed-goto)
come within an acceptable margin of the arity-unrolled `fuseAll` (~18 ns/op, a hard Rust baseline)?
Is bounded left recursion in a VM placeable without space/time regression? Does the TS megamorphism
kill materialize as throughput, or only as a cleaner IC profile?

**Verdict: ADVANCE only on the non-throughput axes.** No missing primitive (LPeg VM well-understood;
grow-the-seed published). But raw-Rust-throughput **loss is near-certain** (T6: a dispatch loop the
compiler cannot inline across vs inlinable straight-line code) — pest itself *codegens to Rust rather
than interpreting* for exactly this reason. 4C is worth advancing *iff* it argues isomorphism-by-
construction + TS-megamorphism-kill + alloc + one-register-diagnostics + streaming (LOCK-9D), and
concedes the throughput axis to 4B. NOT blocked; differentiator is non-throughput.

---

## FAMILY 4D — GRAMMAR-SUGAR

**Architecture.** Convert the combinator library from a **shallow** embedding (closure that both
denotes and executes) into a **deep** embedding: each constructor builds a **bbnf `IrNode`**
(`crates/ir/src/types/node.rs:30`), preserving the public `Parser<T>` surface byte-for-byte
(tagless-final / finally-tagless — one vocabulary, two interpretations: `RuntimeParser` = today's
closures, `IrBuilder` = IrNode). The combinator program then flows through bbnf's **already-built**
pipeline — e-graph rewrites, CSP strategy solver, prefilter/recognizer derivation, residualizer —
and executes via bbnf's runtime. This is the **first Futamura projection**: `parse ≡
run(specialize(interp, grammar))`; "the combinator engine is the generator's runtime in disguise" is
a literal identity.

**The substrate 90% exists in-tree (verified).** parse-that already path-deps bbnf
(`.cargo/config.toml`: `bbnf → ../../bbnf-lang/crates/core`, `bbnf-ir → …/crates/ir`, shared
`egraph`/`csp-solver`). `IrNode` is near-1:1 with the combinator algebra (`Literal/Regex/Epsilon/
Seq/Alt(+AltDispatch)/Repeat/Ref/Skip/Next/Minus/Negate/Map{fn_id}/OptionalWhitespace/TokenDispatch`
— even the *optimizations* pre-exist: `AltDispatch` == the `dispatch` first-char table). The runtime
is written: `crates/ir/src/vm/interpreter/mod.rs:373 parse_with_ir` (interpret lane),
`crates/ir/src/vm/compiler/mod.rs:31 compile → BytecodeProgram` (a 4C-adjacent bytecode lane over
bbnf's *existing* bytecode), `crates/codegen/` (IR → specialized Rust+NEON, the AOT lane carrying the
SPEC §0 kernels). The `.map` home is written: `FnId`, `FnDescriptor`, `HostFnDecl`, `FnTable`.

**Mechanism / two lanes.** *Staged/AOT (the strong form, the only lane with a perf claim)* — a
build-time step evaluates the combinator expression to `GrammarIR`, runs bbnf's generator, emits
specialized code; parse-that *inherits* prefilter derivation (PROVE-10, 8.8–29.5×), residualized
masks, cost-model kernel selection **without hand-writing any of it**. *Interpreted (the generality
guarantee, NOT the perf claim)* — `parse_with_ir` for runtime-built grammars; predicted to *lose* to
the tuned closure core on LL(1). Regex meets 5D at `IrNode::Regex` (one HIR, not a second engine).

**Integration seams.** `parse.rs:61` (add `IrNode` handle) + `leaf.rs`/`combinators/methods/*`;
`map.rs:10` (`fn` ptr → `FnDescriptor` + `Map{fn_id}`; `map_with_ctx:19`/`map_with_state:32` = the
closure boundary); `lazy.rs:21,31` → `IrNode::Ref`; `.parse()` routes to `parse_with_ir` or a
proc-macro to `crates/codegen`. bbnf side: `crates/ir/src/types/{node,grammar}.rs`, `vm/{interpreter,
compiler}`, `recognizer/mod.rs:{32,86,154,201,255}`, `passes/`, `egraph/`. TS: `parser.ts:24` (add
`#node`); staging = a vite plugin over bbnf codegen compiled to wasm (a LOCK-6 6C verdict).

**Gates it would carry.** (G-D1) **Unification round-trip** — the combinator-built `GrammarIR` is
oracle-equal to the `.bbnf`-text `GrammarIR` (two producers, ONE IR — **or the family is a fork**,
violating viii). (G-D2) **inherit one optimizer pass, measured** — route a combinator parser's scan
site through PROVE-10's `Dfa::find` and show ≥8× on a prefilterable site with zero hand-written
prefilter in parse-that (the flagship datum). (G-D3) public API + hot-path invariance: value.js
surface compiles unchanged, 124/124 + ~400 tests green, steady-state per-parse allocs == baseline.
(G-D4) **the map-opacity / value-model boundary quantified** — what fuses (skeleton/prefilter/
dispatch) vs what does not (across `Map`), the materialization tax, and the *net* staged-vs-closure
ratio paired-cold; **name the negative result** if the closure core already wins on the LL(1) shape.
(G-D5) interpreter vs closure core paired-cold, to establish the honest boundary.

**Exact open questions (the crux).** Does the staged lane's `Map{fn_id}` materialization compose
*cheaply* with bbnf's **tape/offset-value-fabric-first** runtime, or does reconciling arbitrary
user-typed `.map` outputs with the tape model impose a materialization tax that *eats the kernel
win*? (risk 3 — the sharpest.) What fraction of real consumer grammars is build-time-static (AOT-able)
vs runtime-dynamic (interpreter-only)? Where does the TS optimizer run — 6C wasm-core (marshal IrNode
in, specialized parser out) vs 6A TS reimpl of the passes (drift)? — a LOCK-6 dependency.

**Verdict: BLOCKED-candidate on the staged-lane PERF claim; ADVANCE on the architectural axis.** The
architectural win — *one IR, one optimizer, two producers* — is real, near-plumbed, and needs no perf
claim to justify itself (a legitimate close by itself). But the *differentiating* perf win lives
**only** in the staged lane, and that lane hinges on a value-model bridge (arbitrary `.map` values ↔
tape-first codegen) whose difficulty is unquantified and could rival building a new value-
materialization system — the BLOCKED-candidate risk. 4D is also maximally coupled (bbnf-IR maturity,
LOCK-3, LOCK-5, LOCK-6). The interpreter lane works today but is predicted to lose. Advance the
architectural unification; gate the perf claim behind the value-model-bridge probe.

---

## THE INCOMPATIBILITY MAP

The four families are not four rivals but positions on **two orthogonal forks plus one universal
precondition**. Sharing a *mechanism* (reification) does not make two routes one family — they differ
on the load-bearing architectural center (the graph HOME and the runtime SUBSTRATE).

### The universal precondition (below both forks): the monadic-wall density
All four reification routes (4A native graph, 4C bytecode program, 4D bbnf IrNode) — and even 4B's
build-time tree — have reach bounded by the fraction of the corpus that is static/selective vs opaque-
dynamic (`chain`/`mapState`/`map_with_state`/`map_with_ctx`). If the opaque fraction is large or
concentrated in hot rules, **every** family's analytic ceiling collapses simultaneously. This is not
an incompatibility between families; it is a common floor they all stand on. **PROBE-1 measures it.**

### FORK I — the graph HOME: parse-that-native (4A/4B/4C) ⊗ bbnf-shared-IR (4D)
The deepest incompatibility. 4A/4B/4C keep parse-that's **own** combinator representation (borrowing
at most the `egraph` *engine* crate); 4D makes bbnf's **`IrNode` type** the representation and
inherits bbnf's whole optimizer + runtime. **These cannot both be canonical.** A separate parse-that
graph alongside bbnf's IrNode is precisely the *second IR* 4D's own unification gate (viii, "one
IrNode, two producers") forbids — it is a fork, not a unification. Conversely, 4D subordinates
parse-that's combinator core to bbnf-IR maturity and the tape value model, which parse-that primacy
(its own lock set, "not an appendage") and the value-model tax (4D risk 3) resist. Constitution ii
(regex↔bbnf HIR isomorphism), LOCK-3, and LOCK-5 pull toward 4D's one-IR; constitution vi (parsimony)
+ parse-that primacy pull toward the native graph. **This fork is an omega-level either/or —
GESTALT-Ω adjudicates it, not LOCK-4 alone** — but LOCK-4 must state it: you get ONE graph type, not
two.

### FORK II — the runtime SUBSTRATE: closures-optimized (4A) ⊗ straight-line (4B) ⊗ bytecode-VM (4C)
Direct opposition on *what executes*. A fused region is **either** an inlined machine-code chain the
compiler sees through (4B) **or** an interpreted program a dispatch loop cannot be inlined across (4C)
**or** an extracted closure tree (4A). Mutually exclusive as THE single runtime; a multi-lane system
that carries two of them is *possible* but pays double — two differential-oracle surfaces, two code
homes — violating parsimony (vi). The fork also splits the constitution's own priorities against each
other: **4B maximizes throughput but minimizes isomorphism** (Rust monomorphized generics have *no TS
analog*, T1 — isomorphism only at the emitted "fused form", divergent mechanisms); **4C maximizes
isomorphism (physical, program-as-data) but concedes throughput** (dispatch loop, T6). 4A sits between
(optimized closures; mirrored-graph isomorphism via a LOCK-6 linter). **PROBE-2 resolves the
throughput half:** a large 4B inlining win routes Rust throughput to 4B and forces 4C to justify
itself purely on the non-throughput axes; a marginal 4B win (the PROVE-11 pattern) weakens the whole
throughput case and lets isomorphism (4C/4D physical) govern.

### How the pairs actually reduce
- **4A ⊗ 4B, 4A ⊗ 4C:** *compatible as layers, incompatible as canonical products.* 4A's graph is the
  build-time IR; 4B's specializer and 4C's emitter are two *lowerings* of it (4C research states this
  verbatim: "the terminal consumer of family 4A's static parser graph"). They are rivals **only**
  where each insists its own runtime is THE product; they merge where 4A is the front and 4B/4C the
  back. The reconciliation requires 4A to abandon "extraction-to-closures is the product" and 4B to
  abandon "no build-time graph."
- **4B ⊗ 4C:** the irreducible core of Fork II — inlined code vs dispatch loop. Not layerable; one
  must be the runtime (or the system pays for two lanes).
- **4B ⊗ 4D, 4C ⊗ 4D:** reduce to **Fork I**. 4D is *agnostic on the runtime substrate* — its staged
  lane can emit via bbnf's `crates/codegen` (≈ 4B straight-line) OR bbnf's `vm/compiler`
  (≈ 4C bytecode). So 4D can *subsume* 4B's or 4C's lowering **iff** the graph is bbnf's IrNode. Their
  incompatibility with 4D is therefore inherited entirely from the graph-HOME fork, not the runtime
  fork — with one added sub-question: is there ONE bytecode (bbnf's) or TWO (a parse-that-native LPeg
  ISA + bbnf's)? Two bytecodes = fork (viii); one = 4C's VM and 4D's bytecode lane unify.

### The map, compressed
```
                       PROBE-1: monadic-wall density  (universal floor — gates ALL four)
                                        │
        ┌───────────────────────────────┴───────────────────────────────┐
   FORK I: graph HOME                                          the deepest incompatibility
   parse-that-native {4A,4B,4C}   ⊗   bbnf-shared-IR {4D}      (omega-level; primacy vs one-IR)
        │                                          │
   FORK II: runtime SUBSTRATE            4D subsumes a lowering lane
   4B straight-line ⊗ 4C bytecode ⊗ 4A closures       (via crates/codegen OR vm/compiler)
   (throughput ⊗ isomorphism ⊗ middle)
        │
   PROBE-2: 4B inlining win vs closure core  (resolves Fork II throughput half)
```

**BLOCKED-candidate roll-call.** 4A — no. 4B — no (throughput unproven, not blocked). 4C — no
(throughput *loss* near-certain but that is a differentiator choice, not a missing primitive). 4D —
**BLOCKED-candidate on the staged-lane perf claim** (the value-model bridge may be equal-difficulty),
ADVANCE on the architectural unification axis. No family is cleanly blocked; the differentiators are
measurement risk (4B), axis-choice (4C), and the value-model tax (4D).

---

## THE TWO SHARPEST PROTOTYPE BRIEFS (for pass 2)

Chosen because they resolve, respectively, the **universal precondition gating all four families**
(PROBE-1) and the **central runtime-substrate incompatibility** with the disassembly discipline
PROVE-09 demands (PROBE-2). Both are self-contained and runnable; see StructuredOutput for the
canonical forms.

### PROBE-1 (spec-probe) — The monadic-wall census
Every reification family (4A/4B/4C/4D) is bounded by how much of the *real* corpus is analyzable. This
probe measures it, no construction. Question: across value.js, latex-paper, keyframes.js, and
parse-that's own JSON/CSS grammars, what fraction of combinator sites is (a) static skeleton
(`then/or/all/any/many/opt/sepBy/skip/next/wrap/trim/not/negate/minus/dispatch`), (b) typed-leaf
projection (`map` over a pure fn ptr — selective-analyzable), (c) **opaque-dynamic**
(`chain/mapState/map_with_state/map_with_ctx` — the true reanalysis barriers)? Method: ripgrep census
bucketed by combinator token over `/Users/mkbabb/Programming/{value.js,latex-paper,keyframes.js}/src`
and `/Users/mkbabb/Programming/parse-that/typescript/src/parse/parsers` + the Rust JSON/CSS bench
grammars; additionally locate every `.chain`/`mapState` site and mark whether it gates a hot rule
(JSON/CSS value production). Pass/fail: opaque-dynamic ≤ 2% of total sites AND zero hot-path rules
contain `chain`/`mapState` → GREEN reification floor for all four families; else RED with the exact
opaque loci enumerated (the ceiling every family must design around). Early signal already gathered:
value.js = 4 `.chain` / ~575 sites.

### PROBE-2 (pt-scratch) — The Rust runtime-substrate fork
Resolves Fork II's throughput half and the "combinators need not be naive" thesis. Question: does 4B's
cross-combinator inlining beat the current `SmallBox<dyn>` closure core (already arity-unrolled via
`seq!`/`alt!`) cold-paired, with the disasm confirming the vtable `blr` is gone? Method: in a scratch
copy of `/Users/mkbabb/Programming/parse-that/rust/parse_that` (isolated `CARGO_TARGET_DIR`, original
read-only), build generic `Seq2<P0,P1>`/`Seq3`/`Alt2` structs implementing `ParserFn` directly
(`impl Trait` return) via the blanket-impl hook at `parse.rs:41-53`; express the JSON combinator
grammar's hot chains (`benches/parse_that/combinator.rs`) through them; compare vs the current
`.then()`/`seq!` form. Corpora: `data/json/{twitter,citm_catalog,canada}.json`. Commands: divan cold
bench, paired A,B,A,B interleave, N≥200, two load windows, opposite arm order; `otool -tv` on the
fused chain. Pass/fail: fused chain shows **zero `blr` through the parser vtable** (hard disasm gate)
AND cold-paired cyc/B ≥ 1.10× the closure baseline in BOTH windows → 4B throughput win CONFIRMED, Fork
II routes to 4B for Rust. If <1.10× or window-asymmetric (PROVE-09 pattern) → throughput case weak,
Fork II routes to isomorphism (4C/4D). Report compile-time + binary-size delta (R1 budget) and prove
recursion still parses via one box at `lazy`.
