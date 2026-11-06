# LOCK-4 — THE COMBINATOR CORE · PASS-2 FAMILY SPECS

*Synthesis agent, pass 2 (Fable-tier judgment). One cogent spec per family while routes remain
incompatible, then the incompatibility map. Inputs folded: pass-2 research-{A,B,C,D,priorart},
the pass-1 re-folded REGISTRY, PROTO-1 VALUE-PLANE + MONADIC-WALL-CENSUS, PROTO-2-4B-FUSION.
Four code facts re-verified this pass (all GREEN): (1) VM drops `.map` —
`IrNode::Map{inner,..} => self.compile_node(inner, ir)` at `crates/ir/src/vm/compiler/node.rs`;
(2) the Cargo cycle is real — `crates/ir/Cargo.toml:13` declares `parse_that = "0.4"`;
(3) `FnDescriptor` (`crates/ir/src/types/fn_descriptor.rs:18`) has EnumWrap/BoxWrap/NumberConvert/
HexConvert/SpanCapture/Expr{MapExpr} and **no** host-closure variant; (4) the e-graph is consumed,
not cloned — `crates/ir/Cargo.toml:19-20` path-deps `egraph`/`egraph-derive`, `csp-solver = "0.1"`.*

---

## THE PASS-2 REFRAME (the finding that reorganizes the lock)

The lock is four **compilation strategies for one combinator algebra**, decided on two axes:
**Q-runtime** (what survives to runtime: graph 4A | bytecode 4C | none 4B) × **Q-ownership**
(whose representation `.then()` builds: parse-that-private 4A/4B/4C | shared `bbnf_ir` 4D).

Pass-1 resolved the throughput half of Q-runtime (→ 4B for Rust, by inference) and re-cast the
deciding axis as **isomorphism (constitution iii, 0% built)**. Pass-2 research does two new things:

1. **Three of the four families independently reframe away from "terminal runtime" toward roles in
   a single tower whose spine is `bbnf_ir::IrNode`.** research-A concludes 4A is a *source
   representation*, not a terminal ("if the fastest lane is AOT Rust with the graph erased, 4A is
   the source of 4B"). research-B concludes 4B's stage-0 object is *a serializable plan* of which
   Rust-monomorphization and a TS-emitter are two lowerings — i.e. 4B is the *lowering* of the plan,
   which *is* the IR. research-C/D show 4C and 4D share the **same wall and the same fix** (the VM
   is value-*structural*, not value-*producing*; both need `Op::ApplyFn` + a `fns` side table +
   `Value` scalar arms on the *same* `bbnf_ir` VM). The apparent four-way fork collapses to: **4D =
   the ownership decision + value-plane completion; 4A = the analyzable front + fusion passes; 4B =
   the Rust monomorphizing lowering; 4C = the portable-bytecode crossing (iso layer) + the same
   value-plane completion as 4D.** They are not four rivals; they are a tower with two genuine forks
   (below).

2. **The binding constraint — a fast *isomorphic TS* execution face — now carries a MEASURED
   refutation-prior, in this very codebase.** research-priorart §1 surfaces parse-that's own killed
   experiment (`parse-that/docs/future-research.md:71-109`): a `SpanParser` runtime-switch tagged
   union — *the direct V8 analog of a bytecode-VM switch decoder* — measured **10–14% SLOWER** than
   monomorphic closure dispatch on V8, adversarially re-run at −14%, then KILLED, with the recorded
   law: "do not re-attempt the runtime-switch approach on V8 without a build-time-**generated
   specialized dispatcher** — not a recursive runtime `switch`." This is a measured prior against
   *exactly* the ~200-LOC 4C/4D TS switch-decoder graft the pass-1 registry named "most likely to
   move the lock." The graft still crosses byte-identically (isomorphism-as-correctness), but its
   *throughput on V8 is refuted-prior*: the fast isomorphic TS path must be a **build-time emitter**,
   the only survivable V8 form, which converges 4C-TS and 4D-TS *onto 4B-TS*. Constitution iii is
   0% built **and** its two candidate mechanisms are each measured-adverse (switch-decoder −14% V8;
   Rust-monomorphization has no V8 analog, priorart §2.4/research-B TS-face; wasm full-walk 0.91×,
   PROVE-14). The one *un-refuted* TS-fast mechanism named anywhere is the build-time-specialized
   emitter — and no family has built it.

**Consequence for the specs below.** Each family is specified in its strongest pass-2 form *and* in
its reframed tower-role. The incompatibility map is small and sharp: the tower converges the
mechanisms; the two irreducible forks are **Q-ownership (private isomorph vs shared `bbnf_ir`,
gated by the Cargo-cycle break = LOCK-5's Dfa extraction)** and **the TS-fast terminal (build-time
emitter vs any runtime decoder, the latter measured-adverse)**.

---

## 4A — CLOSURE-TO-GRAPH · ADVANCED

**Architecture.** parse-that's combinators stop constructing opaque nested closures and construct a
**static analyzable parser graph** whose nodes' fusion, packrat placement, and diagnostics read as
ordinary graph analyses, and whose shape is one representation across Rust/TS. research-A's
load-bearing discovery: *the graph already exists and runs* — `bbnf_ir::IrNode`
(`crates/ir/src/types/node.rs:30`) is a 14-variant, fully-owned, serializable expression graph in
verified near-bijection with the combinator constructor set, already carrying the three organs 4A
demands. 4A's task is therefore not to invent a graph but to make parse-that *construct* it (or a
private isomorph) in place of closures, and prove the analyses ride cheaply.

**Mechanism (concrete, code-grounded).**
- **Projection-as-data, not projection-as-closure.** `.map()` becomes `Map{inner, fn_id}` →
  `FnDescriptor::Expr{MapExpr}` (`fn_descriptor.rs`), a symbolic value tree IR passes read — the
  resolution of the closure core's opacity. **Bound:** `FnDescriptor` has no host-closure variant
  (code-verified), so arbitrary host closures need an `Opaque(HostFnId)` **barrier** node fusion/
  types do not fold through. 4A's strength = how small that barrier surface is on real grammars.
- **Fusion as a `Graph→Graph` pass — it EXISTS, not hypothetical (pass-2 upgrade over pass-1's
  "keystone UNBUILT").** `passes/transform/{fuse.rs:388 (single-use inline widening Alt FIRST),
  fuse_token/factor.rs:339 (common-prefix → TokenDispatch), pattern_dedup.rs:439 (CSE), inline.rs:331
  (threshold inlining)}` already perform 4A's fusions as graph rewrites. The keystone is no longer
  "build a fusion pass from scratch"; it is "point the *existing* passes at a combinator-constructed
  graph and oracle byte-identity to a hand-fused arena."
- **E-graph CONSUMED, not cloned (LOCK-2 steering answer, code-verified).** `crates/ir/Cargo.toml:19-20`
  path-deps `egraph`; `crates/ir/src/egraph/` is a *client* (`build_egraph.rs:106` translate,
  `write_back.rs:378` extract preserving rule boundaries) of the *same* engine `bbnf-regex` uses —
  one saturation home for regex continuum + combinator graph. 4A's cross-lock double-home hazard is
  *retired* on route (b).
- **All three runtimes already consume `IrNode` and run** (`parse_with_ir` interpret at
  `interpreter/mod.rs:373`; bytecode compile at `vm/compiler/`; AOT Rust emitter). So 4A is the
  **common front** whose runtime fork is *selectable*, and the three-way common-baseline bench is
  directly enabled off one constructed graph.

**Integration seams.** node substrate `node.rs:30`; fusion `passes/transform/`; FIRST/dispatch
`passes/sets/first_sets.rs:431` + `AltDispatch` `node.rs:128`; e-graph client `egraph/{build_egraph,
write_back}.rs`; VM/bytecode `vm/`; msgpack bus `lib.rs:7`; live cross-repo dep
`parse-that/rust/.cargo/config.toml:9` (parse-that already links `bbnf-ir`); closure core replaced
`parse.rs:61`.

**Gates it would carry.** G-A1 `fuse(Graph)→Graph` byte-identical to a hand-fused arena
(structural-equality oracle) + JSON/CSS cold-paired ≥0.98×. G-A2 fused-region size histogram over
JSON/CSS/value.js (regions large enough to amortize per-region box, or the counter-finding).
G-A3 host-closure/state-effecting barrier surface < a named % on value.js + latex-paper.
G-A4 `fuse ≡ saturate` structural equality on one grammar (LOCK-2 evidence). G-A5 projection-dense
tie test — analyzable interpreter vs closure core, name the negative result if closures win LL(1).

**Reframed role (pass-2).** **Source representation, not terminal.** PROTO-2-4B measured 4A's
*charter* runtime (extraction-to-closures) as inferior on the box-tax axis 4B eliminates. Redirect:
4A keeps the *graph front* (the isomorphic, mirror-able part) and adopts **4B monomorphization as
its Rust lowering** — abandon "extraction-to-closures is the product."

**Exact open questions.** (Q-A1) Does the *general* analyzable interpreter tie the tuned closure
core on a projection-dense grammar, or only on structure-dominated work? — UNMEASURED on a common
baseline; PROTO-1's 1.06× was a boxing-retirement artifact vs the wrong rival. (Q-A2) How large is
the `Opaque` barrier surface on value.js/latex-paper? — the number that decides whether the analysis
story holds. (Q-A3) Does the graph *survive to runtime* (4A-terminal) or erase into AOT Rust
(4A-source-of-4B)? — the runtime fork, under-determined, must be measured.

---

## 4B — COMPTIME-FUSION · ADVANCED · terminal-role BANKED → compile-backend

**Architecture.** The combinator algebra stays hand-written and lawful, but its *compilation* is a
staging step (partial evaluation w.r.t. the grammar — the First Futamura projection): at
construction time the tree specializes into a monomorphic, vtable-free region; the per-combinator
indirect call and heap box both vanish; LLVM inlines the structural spine end-to-end to leaf
kernels. No runtime graph survives on fused sites; the specialization *is* the runtime.

**Mechanism (three realizations, two in-tree).** M1 enum-dispatch spine — the `span_parser` tier
(`span_parser/mod.rs:51,58,182`), a 30-arm `SpanKind` with `SpanKind::Boxed` (`mod.rs:131`) as the
single escape, vtable-free, O(1) generated-size; ideal for the *closed generated grammar surface*.
M2 flat comptime fusion — the `seq!`/`alt!` macros (`combinators/macros.rs:6,83`, arity 2–8),
collapsing N−1 boxes into one. M3 generic-struct monomorphization — winnow's shape, `Seq<A,B>`/
`Alt<A,B>`/`Map<P,F>` each `impl Parser`, zero dispatch on the spine at monomorphization-blowup
cost; for the *open hand-written API*. **CONFIRMED (PROTO-2-4B, do not re-litigate):** M3 removes
`SmallBox<dyn>` dispatch → **16–27%** over the boxed closure core, vtable `blr` **provably gone**
(0 fused vs 60 closure, disasm-gated, timing-independent), N=250 A,B,A,B interleaved, two windows,
both orders, recursion proven (0 boxes on fused arm via monomorphic free-fn recursion). Winnow's own
`Rc<dyn Fn>` datum: **3–39× slower** than concrete-type monomorphization (research-B §1).

**The TS face — a *different* mechanism.** V8 has no compile-time monomorphization; the lawful analog
is a **build-time emitter** that generates specialized JS source for a fused region — the arity-unroll
already shipping (`leaf.ts:179 fuseAll`, `:39 any`, `:100 dispatch` LUT), made automatic and
grammar-driven, erasing the megamorphic `this.parser(state)` IC inside the region (members called by
lexical name). This is the *only V8-survivable* form (priorart §1 SpanParser law).

**Integration seams.** `parse.rs:61` (the boxed core specialized away); `combinators/macros.rs`
(M2); `span_parser/mod.rs` (M1); `lazy.rs:21` (the recursion boundary that *stays* boxed — the
depth-cap anchor); `leaf.ts:{179,39,100}` (TS emitter precedents); the crossing plan = `bbnf_ir`
(near-bijection, `parse_with_ir` live, VM + msgpack bus).

**Gates it would carry.** G-B1 production dilution — fused struct vs the **shipped** `json_parser()`
with `dispatch_byte_multi` + SIMD on *both* arms; report the shrink (the 16–27% is an upper bound —
F-3). G-B2 the three-way Rust bench (M3 `Seq2` vs 4A extracted-closures vs 4C VM, same leaf substrate/
grammar) — measure FORK II, don't infer. G-B3 first TS number — emitter region carries **no** `.parser`
indirection, dispatch- isolated from allocation-elimination (PROVE-11 bounds dispatch ~+5%). G-B4
fusion-policy budget — a *pluggable* {inline-monomorphize | enum-node | box-here} cost model with
fuse-arity + dispatch-fanout + boxing-depth caps, `.rlib`/`dist` size + incremental-compile deltas on
CSS **and** value.js against a **named** budget (currently a gate with no threshold).

**Reframed role (pass-2).** **Rust compile-backend, not a cross-language terminal.** The confirmed
win is Rust-monomorphization with no V8 analog; a confirmed win on an un-mirror-able mechanism
*sharpens* the constitution-iii crisis rather than resolving it. Whichever isomorphic terminal wins
(4A-graph-as-data or 4D-IR-as-data) adopts 4B purely as its Rust lowering. Working disposition
(not open): 4B is Parsley's staging *endpoint*, the compile backend.

**Exact open questions.** (Q-B1) Once production leaf kernels reabsorb the cycle budget on both arms,
how far below 1.16–1.27× does the combinator-glue share shrink? — beyond-SOTA is unestablished.
(Q-B2) Is the fused region derivable from a *serializable* plan that also drives the TS emitter (⇒ 4B
is the Rust face of a crossing terminal), or intrinsic to the Rust type system (⇒ standalone)? — the
plan artifact is the deciding evidence. (Q-B3) The fusion-policy value that bounds monomorphization
blowup on the full consumer surface — unset.

---

## 4C — VM-CORE / PORTABLE-PROGRAM ISOMORPHISM · BANKED (terminal) · iso-layer RISEN

**Architecture.** Combinators lower to a compact bytecode program that crosses **as data** —
serialized in Rust, decoded by a small TS switch-loop — so rust/ts isomorphism is *by construction*:
one instruction set, one operand encoding, one linear `Vec<Op>` + string table, nothing recursive to
mirror. The decisive question is not *whether* a VM is possible (one is live in-tree) but *what a
portable value-producing VM must add* and *whether it survives crossing byte-identical*.

**Mechanism (substrate is live; enumerate before inventing).** `bbnf_ir::bytecode::Op`
(`vm/bytecode.rs:45-152`) = 30 opcodes (leaves, control, backtrack state, ws, EBNF `Minus`/`Negate`,
O(1) `Dispatch`/`DispatchToken`, `Repeat`, value construction, `MemoCheck`/`MemoStore` packrat,
`DebugBreak` + `source_map` diagnostics). `BytecodeProgram` (`:154-199`) is `Serialize`/`Deserialize`;
`compiled_regexes` is the only `#[serde(skip)]` field, re-derived on load by `prepare_regexes()` —
so **the regex crossing is already data** (`MatchRegex(sid) → strings[sid] → new RegExp`). Interpreter
`run()` (`interpreter/mod.rs:144-313`) is a single switch-threaded loop with explicit stacks; live
consumers (LSP DAP adapter; `crates/core/benches/json/vm.rs`) satisfy Y.13. Packrat *and* diagnostics
are **already opcodes** — the charter's "VM facilities" clause is true in the substrate, and the
DWARF-like plane crosses to TS for free from the same bytes.

**THE LOAD-BEARING GAP (code-verified this pass).** The VM is value-**structural**, not
value-**producing**: `compiler/node.rs` lowers `IrNode::Map{inner,..} => self.compile_node(inner)` —
`fn_id` **dropped**; there is no projection opcode; `Value` (`interpreter/value.rs:56-71`) has four
shapes (`Span`/`Tagged`/`Array`/`Nil`). It is a recognizer. The value-plane completion (shared
verbatim with 4D): (1) `Op::ApplyFn(FnId)` lowered at `node.rs` Map-site; (2) `fns: Vec<FnDescriptor>`
side table on `BytecodeProgram` (mirroring `dispatch_tables`, `#[serde(default)]`); (3) `Value` scalar
arms (F64/U32/Str/Variant); (4) an `ApplyFn` handler = a small `MapExpr` evaluator both languages
implement over the *same* serialized descriptor. `FnDescriptor` is already `Serialize`, so projection
crosses as data. **Abrogate-before-patch verdict: reuse ISA/container/interner/bus/loop; extend the
value plane with one opcode + one side table. A from-scratch private ISA is unjustified.**

**Integration seams.** ISA `bytecode.rs:45-152`; container `:154-199` (add `fns` here); loop
`interpreter/mod.rs:144-313` (the TS decoder's mirror); Map-drop lowering site `compiler/node.rs`;
value plane `value.rs:56-71`; projection data `fn_descriptor.rs`; bus `lib.rs:7`; baseline
`benches/json/vm.rs`; the TS arm's megamorphic `this.parser(state)` IC (`parser.ts:24`) the switch
replaces.

**Gates it would carry.** G-C1 the ~200-LOC TS switch-loop decoder over a `to_vec_named`-serialized
program, **dual oracle** — (a) decoded `Op[]` structurally == Rust golden; (b) TS `Value` tree
node-for-node == Rust `Interpreter` over the six R3 corpora. G-C2 value-plane completion producing
`.map`-projected output byte-identical to the closure core. G-C3 dependency-homing spike — `Op`/
`BytecodeProgram`/interner extract into a dependency-free crate both `bbnf_ir` and a parse-that scratch
consume with **no** `parse_that → bbnf_ir → parse_that` cycle (`cargo tree`). G-C4 wire-contract
regen-idempotence (encode∘decode∘encode = encode, variant tags pinned). G-C5 the common-baseline runtime
fork, measured on JSON **and** projection-dense — name the negative result.

**Reframed role (pass-2) + BLOCKED-candidate caveat on the terminal.** As a *cross-language runtime
terminal on V8*, 4C is **measured-adverse**: priorart §1 records parse-that's own SpanParser
runtime-switch VM at **−10 to −14%** vs closures on V8, KILLED, with the standing law that only a
build-time-specialized decoder survives. So the 4C *decoder-as-TS-runtime* is a route whose fast face
requires the same primitive as the whole lock (a build-time emitter) — it does **not** supply a novel
fast TS runtime; it converges onto 4B-TS. **What RISES:** the byte-identical *portable-program
isomorphism layer* — the program crosses as data, so isomorphism is a property of the representation,
not a mirrored-source linter. That iso-oracle is the correctness bridge every isomorphic terminal
needs, and it is unbuilt and cheap. **BANKED as terminal-runtime; the iso-layer + value-completion
fold into the LOCK-3/`bbnf_ir` VM line and are the 4D-TS-unblock graft.**

**Exact open questions.** (Q-C1) Does the switch-decoder confirm the −10–14% V8 prior, and by how
much does a build-time-specialized decoder close it? (Q-C2) Does the value-completed VM tie the
closure core on projection-dense grammars, or is the per-`Map` `ApplyFn` dispatch a loss (R2
materialization tax)? (Q-C3) Where does the shared ISA home to break the cycle (LOCK-3/LOCK-6 seam)?

---

## 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · BLOCKED

**Architecture.** parse-that's combinator surface is a *construction API over the same `bbnf_ir` the
generator plane uses*: `.then()` builds `IrNode`, not `Box<dyn Fn>`; the combinator engine is the
generator's runtime in disguise; parse-that keeps its public API and inherits the IR optimizer, tape
emitter, and serialization bus. One IR, one optimizer, two front-ends (grammar text · combinator API),
two terminals (interpret · AOT). The SmallBox/vtable question **dissolves by never arising**.

**Mechanism.** A thin `parse-that/src/build/` sugar layer: each method a pure `IrNode` constructor
(`a.then(b) ≡ Seq(vec![a,b])` with flattening; `p.map(f) ≡ Map{inner, fn_id}`). The built `GrammarIR`
runs the existing passes (dispatch synthesis, FIRST/FOLLOW, TokenDispatch fusion) then either terminal.
The TS face is the 4C decoder (program crosses as msgpack data), plus optionally mirrored builders
returning the same JSON-shaped `IrNode` — layout isomorphism by construction. Parsimony maximum: one
IR, one optimizer (`passes/` + `egraph/`), one bus — all verified to exist, all duplicated by 4A/4C's
private subsystems if the private route is taken.

**The two code-verified walls.**
- **The value plane (shared with 4C).** The interpret terminal cannot run `.map` — the VM drops it
  (`compiler/node.rs`). Split by shape: structure-dominated/recognizer grammars reconcile *for free*
  (the VM emits the exact `Span`/`Tagged`/`Array` tape a tape-first consumer wants — no materialization
  tax because no materialization); projection-dense grammars **cannot run at all** without
  `Op::ApplyFn` + a host-fn table. The AOT terminal has full projection today. Honest claim: "one IR,
  two terminals, but interpret is a recognizer+structural-builder until `ApplyFn` + host-fn table land."
- **The Cargo cycle (gate 0, HARD BUILD FAILURE).** `crates/ir/Cargo.toml:13` declares
  `parse_that = "0.4"` (for `regex::Dfa`). 4D asks `parse_that → bbnf_ir` — a forbidden cycle.
  Resolution #1 (preferred, = LOCK-5's likely verdict): extract the `Dfa` into `bbnf_regex` (already a
  `bbnf_ir` dep); the cycle *dissolves* and the regex home unifies. This is *the first thing* a 4D
  prototype must confront, and it couples 4D (and 4A-route-b, and 4C-reuse) to LOCK-5 as a hard
  predecessor.

**Vocabulary gap (minimal lawful variant set, measured not guessed).** Covered:
`then/all/skip/next/or/many/opt/sepBy/minus/not/negate/wrap/lazy/trim/dispatch` + `map`-as-`MapExpr`.
GAP-1 `map` with arbitrary host closure (no `FnDescriptor` variant) → one `ApplyFn` opcode +
`FnDescriptor::HostClosure(FnId)` + `fn_table` (LOCK-7 owns its typed shape; **never** inline a
`Box<dyn Fn>` into the node — that re-introduces the vtable and breaks the msgpack bus). GAP-2/3
`peek`/`lookAhead` → one `Look{inner, positive, keep_value}` node. GAP-4/5 `recover`/`mapState` →
LOCK-7 host-fn lattice, defer. GAP-6 `eof` → trivial `AssertEof`. Each variant gets a paired
regression lane.

**Integration seams.** node algebra `node.rs:30` + `for_each_child` `:145`; runtime `vm/`; bus
`lib.rs:7`; projection DSL `fn_descriptor.rs:54` / `map_expr.rs:17` (+ `constant_fold` `:109`); the
ownership edge `crates/ir/Cargo.toml:13` (the cycle); AOT emitter `crates/core/src/backend/rust/emitter/`.

**Gates it would carry.** G-D0 the cycle break compiles (`Dfa` in `bbnf_regex`, `parse_that` deps
`bbnf_ir`, no cycle). G-D1 JSON build oracle — combinator front's `IrNode` byte-identical to the
`.bbnf`-generated IR. G-D2 two-terminal paired number over six corpora (interpret vs closure core on
JSON *expect win/tie*; AOT vs hand-tuned parser *report shrink*). G-D3 projection-dense tax —
`ApplyFn`-completed interpret vs closures; **name the negative result** if closures win the
LL(1)/projection shape. G-D4 TS msgpack graft floor (the 4C decoder). G-D5 per-variant regression lanes.

**BLOCKED-candidate assessment.** 4D **is** the tower's ownership decision and the *most*
isomorphism-ready architecture (IR-as-data crosses; 4B's monomorphized structs do not) — its relative
position *strengthened* under the 4B reframe. But it cannot converge inside LOCK-4. Its block is a
TS-execution primitive with three horns, and pass-2 hardens all three: **(i)** full `GrammarIR` +
interpreter + optimizer port to TS = re-implementing the grammar plane's engine in a second language,
a primitive of *equal-or-greater difficulty than the original LOCK-4 problem* → **BLOCKED-candidate by
the rule's own text**; **(ii)** the wasm crossing — PROVE-14 measured full-walk value materialization
at **0.91×, a measured LOSS** on exactly the workload a value-producing combinator parser *is*;
**(iii)** the 4C-decoder graft — now carrying the **−10–14% V8 switch-decoder prior** (priorart §1),
so its *fast* form needs the build-time emitter, i.e. the same unbuilt primitive. **Verdict: BLOCKED.**
The value-plane completion and cycle-break are mechanical (not equal-difficulty) and *should* be
built — they unblock the *Rust* and *correctness* story — but the *fast isomorphic TS* face 4D needs
is the lock's shared unbuilt primitive, and two of its three candidate mechanisms are measured-adverse.

**Exact open questions.** (Q-D1) Does the shared IR's value plane tie closures on projection-dense
grammars, or is the closure core the honest winner on the LL(1) shape? — PROBE-1's census greenlit
*structure* only; the value-plane floor is **untouched** (F-4). (Q-D2) LOCK-5 substrate ruling —
is `bbnf_ir` *genuinely* the shared type (5A), or a lawful functor across separate HIRs (5B, which
evaporates "inherit for free")? (Q-D3) LOCK-7 host-fn lattice shape for GAP-1/4/5.

---

## THE INCOMPATIBILITY MAP

Pass-2 collapses most apparent rivalry into a single tower over `bbnf_ir::IrNode` (4D = ownership +
value-completion, 4A = analyzable front + fusion, 4B = Rust lowering, 4C = portable crossing +
value-completion). What remains genuinely incompatible — routes that **cannot coexist**, and exactly
why:

**INCOMPAT-1 — Q-ownership: private isomorph vs shared `bbnf_ir` (mutually exclusive; gated by a
build failure).** Either parse-that owns a *private* node algebra (4A-route-a, 4B-private, 4C-private
ISA) **or** it builds the shared `bbnf_ir::IrNode` (4D, 4A-route-b, 4C-reuse). They cannot both hold:
the shared route makes the private subsystems a Y.13 *consumer-less duplicate* (a second e-graph, a
second bytecode VM, a second node union — the exact double-homes the gestalt collapses), and the
private route forgoes the "inherit the optimizer/VM/bus for free" that is the shared route's entire
value. **The shared route is a HARD BUILD FAILURE until the Cargo cycle breaks** — `crates/ir` deps
`parse_that = "0.4"` (`Cargo.toml:13`); `parse_that → bbnf_ir` is a forbidden cycle. Resolution =
extract `Dfa` into `bbnf_regex`, which is **LOCK-5's call**. *Therefore the entire Q-ownership axis is
decided by LOCK-5's substrate ruling; LOCK-4 cannot elect a terminal ownership before it.*

**INCOMPAT-2 — the TS-fast terminal: build-time emitter vs any runtime decoder (mutually exclusive as
the fast path; the decoder measured-adverse).** For the *fast* TS execution face, the runtime-switch
decoder (4C/4D graft) and the build-time-specialized emitter (4B-TS) are rivals, and the rivalry is
**already measured**: parse-that's own SpanParser runtime-switch VM lost **10–14%** to closures on V8
and was killed, with the law that only a build-time-generated dispatcher survives (priorart §1). The
Rust-monomorphization mechanism (4B) that *does* win has **no V8 analog**. So a single mechanism cannot
be both the fast Rust path (monomorphization) and the fast TS path (emission) *and* the isomorphic
crossing (bytecode-as-data) — the crossing form is data (slow to interpret on V8), the fast forms are
per-language. **The bytecode decoder survives only as an isomorphism/correctness oracle, not the TS
runtime.** This forces LOCK-6 away from source-mirroring (6A/6B) toward **6C (wasm-core) or 6D
(behavioral/oracle iso, layout free per language)** — a confirmed Rust monomorphization win on a
mechanism with no V8 analog is a standing counterexample to source-level rust/ts isomorphism for the
combinator core. **LOCK-4 ⊗ LOCK-6 are one joint decision; the terminal cannot freeze ahead of it.**

**INCOMPAT-3 — Q-runtime terminal: exactly one of {graph-survives, bytecode-survives, nothing-survives}
is the production runtime per site (they compose as a tower but not as coexisting terminals).** 4A-VM
interpret (graph survives), 4C-VM (bytecode survives), and 4B-AOT (nothing survives) cannot all be *the*
runtime for a given fused site. They are **not** architecturally incompatible — the graph is a lawful
*source* that lowers to bytecode OR monomorphized Rust — so the incompatibility is narrow: "which is the
default production terminal," a *per-shape measured* decision (the fusion policy), not a global choice.
**This fork is currently INFERRED (→ 4B for Rust throughput), never measured three-way on a common
baseline** — the single experiment that would make it a measurement is unbuilt.

**Non-incompatibilities (folded, do NOT re-litigate as rivalries).** 4A-source + 4B-Rust-lowering are
*complementary* (graph front, monomorphized lowering). 4C-value-completion **≡** 4D-value-completion
(the identical `Op::ApplyFn` + `fns` + `Value`-scalar fix on the same `bbnf_ir` VM). 4A-fusion **consumes**
LOCK-2's e-graph (code-verified path-dep, not a clone). 4C's bytecode VM **is** `bbnf_ir`'s live VM
(a private second ISA is an abrogate-before-patch violation). These are convergences, not conflicts.

---

## BLOCKED-CANDIDATE ROSTER

- **4D — BLOCKED (confirmed).** Its fast-isomorphic-TS primitive is the lock's shared unbuilt primitive;
  horn-(i) is equal-difficulty-to-the-original-problem (a second-language engine port) — a
  BLOCKED-candidate by the rule's own text; horns (ii)/(iii) are measured-adverse (wasm 0.91×;
  switch-decoder −14% V8). Its *mechanical* debts (value-plane completion, cycle-break) should still be
  paid — they unblock the Rust + correctness story — but do not lift the block.
- **4C — terminal-runtime BANKED (measured-adverse on V8); iso-layer + value-completion ADVANCE inside
  the `bbnf_ir` VM line.** Not BLOCKED (its unbuilt work is cheap and high-signal), but its
  *terminal-runtime-on-V8* bid is refuted-prior.
- **4A / 4B — ADVANCED, tower-roles assigned** (source-front / Rust-compile-backend). Neither is BLOCKED;
  both are gated by INCOMPAT-1 (ownership/cycle, LOCK-5) and INCOMPAT-2 (LOCK-6 iso verdict) before a
  terminal can be elected.

---

## THE TWO SHARPEST UNCERTAINTIES → PASS-3 PROTOTYPE BRIEFS

The two deciding axes are (A) **the fast isomorphic TS execution face** (constitution iii, 0% built,
now carrying a measured refutation-prior) and (B) **Q-ownership: the cycle-break + whether the shared
IR's value plane ties closures**. The two briefs map one-to-one onto them.

### BRIEF 1 (pt-scratch) — THE TS EXECUTION FACE UNDER THE MEASURED V8 REFUTATION
Confront INCOMPAT-2 with the first honest TS number. On a parse-that scratch copy (rsync-exclude
`target/.git/node_modules`), take ONE small projection-dense region (a value.js-shaped `all()`-of-mapped
color/unit fragment) and build three TS runtimes of it: **(A)** a ~200-LOC switch-loop decoder over a
Rust-serialized `bbnf_ir` bytecode program (`to_vec_named` msgpack; regexes re-derived as sticky
`RegExp`); **(B)** a build-time-emitted specialized closure (generalize `leaf.ts:179 fuseAll` past
arity-3 to a vite/ts-transform that calls members by lexical name, no `.parser` field); **(C)** baseline
= today's `.parser`-IC closure path. Measure paired-cold, sample-interleaved A,B,C,A,B,C in one process,
N≥200, two load windows, both orders, median+dispersion. Instrument: grep the emitted (B) region for
`.parser(` (must be **0**). **Pass/fail:** PASS if (B) achieves **≥1.05×** baseline with **zero**
`.parser` indirection AND (A)'s V8 penalty is pinned to ±2% (confirming or refuting the −10–14%
SpanParser prior). FAIL-and-name-negative if (B) cannot beat baseline (a fast *isomorphic* TS terminal is
refuted — LOCK-4 has no cross-language fast face) OR (A) ties (B) within noise (the SpanParser law is
overturned — re-open 4C-decoder-as-runtime). Either result decides whether any family gets a fast TS
terminal — the constraint no terminal can be named without.

### BRIEF 2 (bbnf-worktree) — CYCLE-BREAK + SHARED-IR VALUE-PLANE TAX (one Rust probe, four families)
Confront INCOMPAT-1's gate-0 and 4D's untouched value-plane floor (F-4). In a bbnf-lang worktree
(isolated `CARGO_TARGET_DIR`): **(gate 0)** move the `parse_that::regex::Dfa` usage out of `crates/ir`
into `bbnf_regex`, delete `parse_that = "0.4"` from `crates/ir/Cargo.toml:13`, add a `parse_that →
bbnf_ir` edge in a parse-that scratch; `cargo tree` must show **no** `parse_that → bbnf_ir → parse_that`
cycle and `cargo build -p bbnf-ir` must compile. **(value completion)** add `Op::ApplyFn(FnId)` at the
Map-drop site `crates/ir/src/vm/compiler/node.rs` (`IrNode::Map{inner,..} => …`), add `fns:
Vec<FnDescriptor>` (`#[serde(default)]`) to `BytecodeProgram` (`vm/bytecode.rs:154`), extend `Value`
(`interpreter/value.rs:56`) with F64/U32/Str/Variant arms, implement the `ApplyFn` handler as a small
`MapExpr` evaluator. **(measure)** on a projection-dense grammar (value.js-shaped color/unit) and JSON,
run `parse_with_ir`-with-`ApplyFn` vs the parse-that closure core: first a byte-identical `Value`-output
oracle, then paired-cold A,B,A,B, N≥200, two windows, both orders, median+dispersion. **Pass/fail:**
PASS if output is byte-identical AND the interpret ratio is **≥0.98×** on JSON (recognizer free-tape
claim) — value-completed shared VM is viable for 4C+4D. FAIL-and-name-negative if the projection-dense
ratio is **<0.98×** (closures win the LL(1)/projection shape — 4D's honest close, F-4). Gate-0 compiling
decides the entire Q-ownership axis; the ratio is the value-plane floor PROBE-1 never touched.
