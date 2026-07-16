# LOCK-4 — THE COMBINATOR CORE · pass-1 SYNTHESIS · FAMILY-SPECS

*Synthesis agent, pass 1. One cogent spec per family while the routes remain incompatible; then
the incompatibility map; then the shared crux and the two prototype briefs. Inputs: PORTFOLIO.md
(LOCK-4 charter + CONSTITUTION), research-{A,B,C,D,priorart}. Elegance earns nothing on its own —
a family stalled at a missing primitive equal in difficulty to the original problem is marked
BLOCKED-candidate.*

---

## 0. The frame the four families actually occupy

The four families are not four features. They are **four compilation strategies for one combinator
algebra**, ordered by how much of the parser survives as a first-class analyzable object, and they
compete for a *single* answer to two questions:

- **Q-runtime — what survives to runtime?** An analyzable representation the parser walks (4A graph,
  4C bytecode), or nothing but monomorphized straight-line code (4B)? 4D inherits *both* modes from
  the grammar plane (interpret ≈ graph, compile ≈ bytecode/codegen).
- **Q-ownership — whose representation is it?** A parse-that-private node union / instruction set
  (4A/4B/4C), or `bbnf_ir` shared with the grammar plane (4D)?

The single most important external finding (priorart §1): **the exact 4A→4B→4C pipeline already
exists, won, and is measured** — Parsley (Haskell): deep-embedded combinator tree (4A) → law-based
rewrites → abstract machine in CPS (4C) → staged partial evaluation to hand-written-quality code
(4B). LPeg (Lua) fixes the 4C instruction-set reference; winnow fixes 4B-Rust as the raw-speed
leader; chumsky supplies the 4B generic-struct + `.boxed()` shape and a live discard-analysis. **We
are not inventing; we are porting a settled design into a dual-language, NEON-era, type-planed
setting.** That reframes LOCK-4 from "which family" to "which *terminal* runtime, given parse-that's
monadic `chain` and the rust/ts isomorphism edict."

**The load-bearing cross-family fault line (priorart §1a, §10; every family's §3).** parse-that ships
a monadic `chain` (TS `parser.ts:124`) and arbitrary-closure `map`/`mapState`/`recover`. Parsley's
theorem: *static grammar structure is exploitable only if you drop the monad* — `chain` generates
dynamic structure no static analysis can see through; the recovery is **selective functors**
(`branch`, statically analyzable data-dependent choice). This is not a family. It is the ONE
decision all four must make identically — opaque escape hatch vs selective reformulation — and it
gates every family's beyond-SOTA claim. It is Brief 2.

---

## 1. FAMILY 4A — CLOSURE-TO-GRAPH

**Architecture.** Defunctionalize the combinator algebra (Reynolds 1972): reify it as a first-class
**data graph** of tagged nodes — `Graph = {rules: Map<RuleId,Node>, entry}`, `Node = StringLit |
Regex(dfa) | Seq([Node]) | Alt([Node]) | Many{inner,lo,hi} | SepBy | Opt | Skip | Next | Not |
LookAhead | Negate | Peek | NonTerminal(RuleId) | Map{inner,f:OpaqueFn} | Boxed(dyn)`. The graph
STAYS at runtime and is tree-walked by an `#[inline(always)]` `match &node` — vtable-free, flat
`Seq/Alt(Vec)` so an N-chain is one loop, not N nested closures. Cleverness that today is
construction-time smart-constructors becomes **passes, each a total `Graph→Graph`**: (1)
flatten/normalize to fixpoint; (2) fusion (`SepBy+trim⇒SepByWs`, `Not(class)⇒TakeUntilAny{1,2,3,Lut,
SIMD}` ladder, adjacent `Map⇒g∘f`, `Alt`-of-literals⇒`AnyOf`); (3) dispatch-table synthesis (disjoint
FIRST sets ⇒ `Dispatch(lut[128])` — Krishnaswami–Yallop); (4) packrat placement (SCC over
`NonTerminal` edges annotates only recursive rules — supersedes the global `PACKRAT_ARMED` latch);
(5) diagnostics annotation (labels as a derived property of graph position).

**Mechanism / substrate — three-quarters built and IN PRODUCTION.** `rust/parse_that/src/
span_parser/` (1437 LOC, 10 files) IS the seed: `SpanKind` (`mod.rs:63`) enumerates every node 4A
needs including the `TakeUntilAny*` fast-path ladder; `call_inner` (`mod.rs:186`) is the enum
dispatch; `then_span`/`or` (`methods.rs:16,29`) already flatten `Seq`/`OneOf`; `SepByWs` is the
hand-fused node. Real consumers satisfy Y.13 by construction: `parsers/json.rs`,
`parsers/css/{declaration,selector,mod}.rs`.

**Integration seams.** Node union to generalize `span_parser/mod.rs:63`; proto-passes to lift out
`methods.rs:16,29` + `constructors.rs`; value plane to fold `parse.rs:61` + `combinators/methods/
{sequence,alternation,repetition,sep_by,map}.rs`; recursion seam `lazy.rs:21` (Rust) /
`lazy.ts:5,19` WeakMap (TS); TS islands to convert to pass outputs `leaf.ts:100` (dispatch LUT),
`:179` (`fuseAll`); rewrite-engine precedent to mirror `rust/regex/src/egraph/` (in-tree
equality-saturation over regex HIR — the *same shape of rewrite system* 4A's fusion pass adopts).

**Gates it carries.** P1 fusion-as-separable-pass: `fuse(Graph)→Graph` produces a byte-identical
optimized graph to the hand-fused constructors (structural-equality oracle) AND JSON/CSS cold-paired
divan ≥0.98×. P2 value plane: add `Map`, re-express `parsers/json.rs` as one graph, retire its
closure tier — typed-output parity on 37 JSON tests AND ≥0.98× vs the closure core head-to-head on
the SAME grammar. P3 packrat-placement: SCC annotation ≥ armed-everywhere on a recursive grammar AND
strictly faster on a non-recursive one. P4 rust/ts iso: `ParserNode` + passes in TS; optimized graph
node-for-node identical across languages on a shared corpus.

**Exact open questions.** (a) **The value plane is the decider** — does a widened `Map`-carrying node
payload push the `match` below the monomorphized closure chain (that gap is 4B's raison d'être)?
(b) The opaque-`Map` ceiling (R1): on action-dominated grammars the graph ≈ closures — 4A wins on
*structure-dominated* work (JSON/CSS scan, `Alt` dispatch, `SepBy` loops) and on *analyzability*
(packrat/diagnostics/iso), not raw speed on projection-heavy grammars; this must be stated, not sold.
(c) Recursion API break (R3): promoting `lazy()`→`NonTerminal(RuleId)` changes how recursive grammars
are authored (value.js/latex-paper write inline `lazy()`); guard = keep `lazy()` as sugar auto-minting
a `RuleId` via existing cache identity (observable sharing fallback). (d) TS megamorphic tag dispatch
(R4) — the dispatch-LUT lever already under-delivered (~+5% vs +10% claimed); the Rust win does not
transfer by assumption.

**Disposition — ADVANCE-candidate.** Substrate ships with real consumers; four falsifiable, ordered,
cheap-first prototypes; no missing primitive. Center of gravity: P1+P2 together — a *single* graph
tier (no parallel closure path) holding JSON/CSS at parity while making fusion a pass. Honest downside
is a *narrowing*, not a block: if the value plane forces the interpreter below closures, 4A becomes
"analyzable graph for packrat+diagnostics+iso, closures retained for hot value assembly" — a real,
smaller result.

---

## 2. FAMILY 4B — COMPTIME-FUSION

**Architecture.** Partial evaluation / staging of combinator *composition*. A static sub-grammar (a
nesting of `then/or/map/opt/many/sep_by` with no recursion crossing it) collapses to ONE straight-line
routine. **No runtime graph at all** (the axis that separates 4B from 4A/4C). Rust: the type system +
LLVM inliner do it (comptime). TS: an explicit build-time fuser emits specialized source
(build-time). Combinators stay lawful hand-written primitives; only their *composition* is specialized
away.

**Mechanism.** *Rust:* replace "erase every node" (`SmallBox<dyn ParserFn,S32>`, `parse.rs:61`) with
"erase only at recursion." Each combinator is a generic ADT struct carrying children by concrete type
— `Then<A,B>`, `Or<A,B>`, `Map<A,F>`, `Many<A>` — impl `Parser<'a>{type Output; fn parse}`, `#[inline]`
throughout, so LLVM folds a static chain into one function with no indirect call and no box. A single
`dyn` erasure re-enters ONLY at recursion via `.boxed()` (chumsky/combine shape), seated exactly at
`lazy.rs:20-45` where a box + `cached_parser` already lives. The public `Parser<'a,O>` is preserved as
a `.boxed()` facade over the monomorphized interior — non-breaking. *TS:* generalize `fuseAll`
(`leaf.ts:154-273`, arity-2/3 unrolled) to any arity + any nesting via a **build-time source
transform** (vite/ts-transform plugin); the runtime interpreter graph remains the sound fallback for
what cannot be statically seen. **`eval`/`new Function` forbidden** (CSP-hostile, deopt-prone) — the
fuser emits source, keeping it isomorphic to the Rust comptime story.

**Shared substrate (iii).** A **fusion algebra**: the shape set `{Then,Or,Map,Opt,Many,SepBy,Boxed}`
+ rewrite rules (how a nested shape collapses; where `Boxed`/recursion halts fusion + memoizes; where
diagnostics thread). Rust realizes it via the monomorphizer+inliner; TS via a codegen pass — *same
shape set, idiomatic realization* (tagless-final + staging). Packrat & diagnostics survive: fusion
halts at `Boxed`, which is exactly where packrat memoizes; diagnostics fold in line as `seq!`/`alt!`
already fold `furthest_offset.max` (`macros.rs:91-97`).

**Integration seams.** In-tree POCs `combinators/macros.rs:1-302` (`seq!`/`alt!` collapse N−1 boxes→1)
and `leaf.ts:154-273` (`fuseAll`); the enum-tier control `span_parser/` (a closed-world realization of
the same anti-vtable thesis — the third bench arm); recursion seam `lazy.rs`; TS fuser home
`typescript/vite.config.ts` (`sideEffects:false` aids tree-shaking).

**Gates it carries.** Rust: build the generic-struct algebra + reimplement the JSON grammar both ways
(vs SmallBox, vs span_parser enum tier); paired cold N≥200 two-windows over six JSON corpora; **`cargo
asm`/`otool -tv` to confirm the `blr` vtable calls are gone** (the mechanism proof, independent of
timing); compile-time + binary-size delta (budget gate); MIRI-clean `Boxed` `'a`-seam (R7). TS: fuser
generalizing `fuseAll` beyond arity-3; certified PROVE-11 method (cold per-op median ns, single-build
paired A,B,A,B, N≥12); **isolate dispatch-elimination from allocation-elimination**; bundle-size; `tsc`
clean; the three consumer suites parity.

**Exact open questions.** (a) **The win is unproven and may be null.** The SmallBox indirect call is a
single fixed-target branch the Apple-M BTB predicts perfectly on the steady loop; inlining across it
may buy nothing — the exact PROVE-09 shape (portable_simd→NEON claimed 1.34–1.57×, REFUTED to
parity-or-slower under paired interleaving; §0/09 is now LAW). (b) PROVE-11 (CONFIRMED_DELTA): the TS
fusion win is **allocation-shaped, not dispatch-shaped** — one flat array vs N nested tuples bought the
double-digits; eliminating the megamorphic edge alone bought only ~+5%. A 4B prototype that does not
isolate the two axes over-claims exactly as the dispatch lane did. (c) Comptime/bundle blowup (R3):
full monomorphization of deep chains explodes rustc codegen + binary size (FastParse is the cautionary
case); the generated-size budget binds; a `.boxed()` cut-point caps depth. (d) TS static recognition
is undecidable in general — "no runtime graph *at all*" is false for dynamic grammars; scope the claim
to fused sites.

**Disposition — ADVANCE-candidate, REWARD-AT-RISK.** No missing primitive — the mechanism is settled
prior art (chumsky/winnow raw-speed leadership; LMS-staged combinators ~2×). But its charter value —
"the vtable/SmallBox question dissolves" — targets an axis (inlining barrier) PROVE-09/PT-2 never
measured, and the honest prior is *possibly NEUTRAL*. **A filed refutation is a valid family outcome**
(PT-2 W4 arena precedent), not a failure to bury. 4B advances only when `cargo asm` shows the calls
folded AND a paired win is real *and inlining-attributable* — or it closes on an honest null.

---

## 3. FAMILY 4C — VM-CORE

**Architecture.** Stop constructing an opaque call tree; **compile the combinator expression to a flat
array of tagged instructions** run by a small dispatch loop over an explicit *position stack* + *choice-
point stack* + *value stack* — the LPeg parsing machine (proven correct: Medeiros & Ierusalimschy).
Instruction core: `Char/TestChar/Set/Dfa/Str/Any/Eof` leaves; `Choice/Commit/PartialCommit` (the
device that makes ordered choice + repetition O(1)-tail without stack growth); `Call/Return/Jump/Fail`
(rule invocation + backtracking); `Span/Capture/Action` (the value plane). Because a flat
operand-encoded stream is *data with a fixed byte layout*, the Rust `Box<[Instr]>` / packed `Vec<u32>`
and the TS `Int32Array`/struct-of-arrays hold **the same program** — the interpreter is the only
per-language artifact, and **isomorphism is a property of the representation, not a mirrored-source
discipline** (the family's strongest claim, and strictly stronger than any LOCK-6 mirror-linter).

**Mechanism / substrate.** The Rust `span_parser/` is a proto-VM *one step short*: `SpanKind` is a
nascent 30-variant instruction set, `call_inner` is the decode/dispatch match — but it is still a
*tree-walker* (holds children behind `Box<SpanParser>`, uses the native call stack + `?` for
backtracking). 4C's move: linearize the tree to a flat program, replace native recursion with the
explicit choice-point stack, `PartialCommit` the `Many` back-edge so it does not grow the stack per
iteration. `CompiledDfa(Arc<Dfa>)` is *already* a compiled machine embedded as a single leaf — half
the leaf vocabulary is already a machine; 4C makes the skeleton compile too. **Packrat** becomes a
first-class `MemoOpen rule,L`/`MemoCommit rule` instruction pair keyed by `(pc,sp)` — structurally
identical to TS `getCijKey = id*OFFSET+offset` (`packrat.ts:71`) but now *the same key arithmetic in
both languages* because `pc`/`sp` are machine registers; placement becomes an analysis over the
program (which `Call` targets re-enter at the same `sp`). **Diagnostics** are registers: `furthest_
offset` bumped on `Fail`, lazy side-tables written only under a `diagnostics` register (the certified
zero-alloc-until-armed discipline, expressed once for both).

**The asymmetry it must confront.** TS has NO enum tier — `*Span` was excised at `1.0.0` and is guarded
by `proof:no-span-surface`. So on Rust 4C *extends* a live proto-VM; on TS it *introduces* one where a
prior span-tier was deliberately deleted. The excision is a caution not a veto: what was removed was a
*parallel public surface*; **the VM is an internal lowering target with NO public combinator surface**,
so it does not re-open that wound. The iso claim is therefore "a *new* shared instruction layer both
cores compile into," not "mirror the two existing cores" (they diverge).

**Integration seams.** Proto-VM to complete `span_parser/mod.rs:56,184` + `combinators.rs:15/30/52`
(the tree-walk arms to linearize into `Seq`-concat / `Choice`+`Commit` / `PartialCommit`); closure
core to measure against `parse.rs:61`; value/action grammar seam = the generator's `->` projections
(residualized on the union path) supply the `Action` table; single-program alloc precedent
`regex/generated.rs:174` (`BumpSlab` — and the standing lesson that a `'static` arena over
`'a`-closures is unsound, so **the program holds action-IDs into an `'a`-scoped side table**, never
smuggled closures); TS interpreter home is new, `leaf.ts:100-151` (`dispatch`'s `Int8Array(128)`) is
the closest monomorphic-table precedent a `switch`-loop generalizes.

**Gates it carries.** (1) Span-tier VM vs the SpanKind tree-walker: compile a recognition skeleton,
paired-interleave N≥200 two-windows cold vs the live `SpanParser` on the SAME grammar — ≥0.98× → VM
throughput-viable, isomorphism free; <0.9× → loses on throughput, justifies itself on facilities
alone (a valid documented negative). (2) Threaded-code recovery: if (1) loses, decode-once into a
function-pointer array (Rust) / confirm V8's jump-table `switch` (TS) and re-measure. (3) The
**byte-identical-program oracle**: serialize the Rust program, decode in TS, assert identical
opcode/operand stream + identical parse over a shared corpus — the headline claim stands or falls here,
independent of throughput. (4) Packrat parity: `MemoOpen/MemoCommit` with the shared `(pc,sp)` key
parses a left-recursive grammar *identically* in both languages (same seed-growth count). (5)
Value-tier honesty: add `Action`/value-stack, wire one `->` grammar, paired-bench value-producing VM vs
the generic closure core — report *separately* from (1).

**Exact open questions.** (a) **Interpreter overhead beats the isomorphism win (the central risk):** a
bytecode loop reintroduces one dispatch branch per instruction + explicit-stack traffic the native
stack did for free; the honest prior is a naïve VM *loses on throughput* (0.7–0.9× on twitter/citm/CSS)
and wins only on iso + facilities, with threaded-code the recovery. (b) **The value/action facility is
where the loss concentrates** — every `Action` is an indirect call the closure core inlined; Span-tier
may hit 0.98× while value-tier hits 0.6×, whose honest verdict is "recognition VM yes, value VM needs
staging → 4B." (c) Instruction granularity must be coarse enough to host the NEON scanners as single
`Set`/`ScanClass` ops — never a per-byte `Char` loop on hot text (the SPEC's certified SIMD-scan wins
must survive). (d) Left-recursion seed-growing parity in a VM is the one genuinely subtle facility.

**Disposition — ADVANCE-candidate, THROUGHPUT-OPEN.** Mechanism substantiated (seed in-tree, prior art
proven-correct, iso claim testable by golden-program oracle); no missing primitive — a recognition VM
is fully buildable and extends `span_parser` directly. Its throughput standing is an open measurement
whose honest prior is *a loss to be recovered*, not a win to be assumed. It must not import a speed
narrative the paired bench has not certified.

---

## 4. FAMILY 4D — GRAMMAR-SUGAR

**Architecture.** One identity: **a parse-that combinator expression IS a `GrammarIR` under
construction.** `a.then(b).or(c).many()` does not close over `move |state|{…}` — it builds
`bbnf_ir::IrNode` (`crates/ir/src/types/node.rs:30`), the *same* IR the grammar plane produces from
`.bbnf` text. The constructed IR flows through the *same* passes (the optimizer) and the *same* runtime
the generator uses. This is the shallow→deep embedding conversion (Gibbons & Wu): each combinator goes
from "a function that does the work" to "a constructor of an inspectable AST a separate
interpreter/compiler consumes." The distinguishing claim over a private analyzable graph (4A/4C): the
graph is **not parse-that-private** — it is `bbnf_ir`, so isomorphism (rust/ts) and the optimizer come
*for free* rather than being rebuilt.

**Mechanism.** The near-bijection already exists: `IrNode`'s variant set is, modulo leaf naming, the
parse-that combinator surface (`Literal/Regex/Seq/Alt/Repeat/Ref/Skip/Next/Minus/Negate/Map/
OptionalWhitespace/TokenDispatch` ↔ `string/regex/then/or/many/ref/skip/next/minus/not/map/trim/
dispatch`). **Front:** a thin builder appends `IrNode` instead of a closure — a second front door onto
the extant node factory (`crates/core/src/lower/expression/mod.rs:157` already emits `IrNode::Seq`;
reuse `string_interner.rs`, `fn_table.rs` verbatim). **Middle (inherited optimizer):** the extant
passes `crates/ir/src/passes/{facts,transform/fuse,transform/fuse_token,csp_strategy}` + `egraph/` with
its cost model — `AltDispatch`/`TokenDispatch` are *populated by the dispatch pass*, so the very
first-char/LUT optimization parse-that hand-codes (`leaf.ts:100`) becomes a *derived output*, a strict
superset of the hand-tuned core. **Runtime (two lawful modes):** *interpret* — `crates/ir/src/vm/
interpreter/mod.rs:373` `parse_with_ir` (runtime grammars, no codegen); *compile* — `vm/compiler/` →
`BytecodeProgram` + backend `crates/core/src/backend/{rust,ts,wasm}` (build-time grammar → AOT parser:
the combinator engine IS the generator). **Isomorphism:** `GrammarIR` is fully-owned, MessagePack-
serializable (`lib.rs:6`) — rust/ts parity is a property of a shared data structure, the strongest form
of clause (iii)… on the Rust side.

**The central wall (host actions vs a declarative IR).** parse-that's combinators admit **arbitrary
host closures** an `IrNode` cannot represent. `FnDescriptor` (`fn_descriptor.rs:18`) has only the
transparent `Expr{MapExpr}` variant (literals/`Input`/`InputProp`/`FnCall`/`BinOp` — analyzable,
constant-folded); there is **no opaque runtime-closure variant**. `.map(|s|…)`, `.chain(fn)`,
`.mapState(fn)`, `.recover(fn)` carry general functions not `MapExpr`-expressible. To keep its public
API, 4D must add `FnDescriptor::HostClosure(HostFnId)` (aligns with LOCK-7's host-fn lattice + the
hybrid-grammar-host law). **Poison risk:** an opaque `HostClosure` inside an `Alt` branch has unknown
FIRST, so `alt_dispatch_admissible` (`facts/mod.rs:161`) returns false and dispatch/LUT *silently
disables* for that alternation — a perf cliff where the closure core is fastest. Guard = the
applicative/selective/monadic split: `map`/`then`/`skip` applicative (structure static); `chain`/`bind`
monadic (opaque, but scoped — map-on-*result* must not defeat FIRST of the branch's *structure*).

**Integration seams.** `node.rs:30` (target); `fn_descriptor.rs:18` (add `HostClosure`);
`map_expr.rs:17` (transparent path); `lower/expression/mod.rs:157` (parallel producer);
`interpreter/mod.rs:373` + `vm/compiler/*` + `bytecode.rs` (runtimes); `passes/*` + `egraph/`
(optimizer); `lib.rs:6` (MessagePack iso bus); fronts to preserve `parse.rs:61` /
`typescript/{parser,leaf}.ts`.

**Gates it carries.** (1) Parity: a Rust combinator front building `GrammarIR` for the JSON grammar,
run via `parse_with_ir` — byte-identical trees vs the hand + closure JSON parsers. (2) The decisive
number: paired cold N≥200 two-windows of interpret AND compile modes vs the closure core over six JSON
corpora — does inheriting the optimizer + compiling *beat* the tuned closures, and does interpret stay
within a stated no-regression floor? (3) Host-closure soundness: route a `.chain(fn)` + a `.map(|s|
custom)`; assert correct results AND that dispatch/FIRST of *sibling* branches still fires (poison does
not trigger). (4) rust/ts iso: serialize the JSON `GrammarIR`, drive from the Rust interpreter AND a
minimal TS interpreter (or the WASM core) — byte-identical trees.

**Exact open questions & the block.** (a) **TS has NO IR/NFA/DFA today (R4 §3).** 4D-in-TS is either
(i) a full port of `GrammarIR` + interpreter + the optimizer passes to TS, or (ii) the WASM-core
crossing (LOCK-6 6C). Horn (i) is a re-implementation of the grammar plane's execution engine in a
second language — a primitive of *equal-or-greater difficulty than the original LOCK-4 problem*. Horn
(ii) routes value-producing parses through wasm, and **PROVE-14 measured full-walk (full value
materialization) at ~0.91× — a LOSS** — precisely on the workload a value-producing combinator parser
IS (the byte-exact tape 4.4× stop-early win does NOT cover full materialization; SPEC §0/14). Both
horns are the "missing primitive equal in difficulty" the constitution flags. (b) Interpret-mode
regression vs the tuned closure core (the shim's evaporated 1.34–1.57× is the standing lesson);
compile-mode is where a genuine win is plausible, interpret-mode is the regression lane. (c)
State-effecting combinators (`peek`/`lookAhead`/`recover`/`mapState`) mutate `ParserState` in ways
`IrNode` does not model — new variants or `HostClosure`-with-state; the minimal lawful set must be
measured. (d) Y.13: the combinator engine as "the generator's runtime in disguise" must retain its
live standalone consumers (value.js/latex-paper/keyframes.js) — preserved by construction if the public
surface is unchanged. (e) 4D is the most cross-lock-entangled family: it needs LOCK-5's verdict that
`bbnf_ir` is *genuinely* the shared substrate (clause i exempts combinators from grammar-totality — 4D
is coherent only if the HIR is the shared substrate, not if it forces combinators to be
grammar-generated) and LOCK-7's host-fn lattice for `HostClosure`.

**Disposition — BLOCKED-candidate.** The Rust-side sub-design is genuinely strong and nearly-free (the
near-bijection is real; the optimizer is inherited, not rebuilt) — on Rust alone it is the most elegant
parsimony story in the lock. But **elegance earns nothing on its own**, and 4D's *constitutional* rust/ts
isomorphism (clause iii, non-negotiable) rests on the TS-execution primitive above, whose two horns are
(i) equal-difficulty re-port and (ii) a measured-loss WASM path on exactly this workload. That is the
BLOCKED-candidate trigger. **Exact gate to unblock:** measure horn (ii) — does the WASM-core crossing
beat the PROVE-14 full-walk non-goal for a *value-producing* combinator parse (not just stop-early)? —
AND/OR obtain LOCK-5's ruling that `bbnf_ir` is the lawful shared substrate. Until one resolves, 4D is a
strong Rust-only design that cannot satisfy the constitution by itself.

---

## 5. THE INCOMPATIBILITY MAP

Two orthogonal axes; the families are terminal choices on them. Parsley proves 4A→4C→4B compose as a
*pipeline* — so the incompatibilities below are about the **terminal runtime / ownership**, not about
ideas-in-a-pipeline. Only one family can be THE combinator core (one-path precept; no orthogonal
codepaths; research-A §2.4.2 names "a parallel second codepath" as the violation to resolve).

**Axis Q-runtime — what survives to runtime:**

- **4A vs 4B — mutually exclusive as terminal.** 4A keeps an analyzable node graph alive at runtime and
  tree-walks it; 4B compiles the graph away to monomorphized straight-line code. Compatible ONLY as
  pipeline stages — 4A's graph as 4B's build-time input (the Parsley fusion) — but that forfeits 4A's
  entire charter value (runtime analyzability, debuggability). 4A-terminal and 4B-terminal cannot
  coexist.
- **4A vs 4C — mutually exclusive as terminal engine; sequential as pipeline.** Both keep a runtime
  representation, but 4A walks a pointer node-tree (enum dispatch, native-stack backtracking) and 4C
  runs a flat bytecode program (explicit choice-point/value stacks, dispatch loop). 4C is the *lawful
  lowering* of 4A's graph — "the graph is the thing 4C lowers" (research-A §0) — so they form one
  pipeline but cannot both be the single terminal runtime. Their isomorphism mechanisms also differ
  (4A = mirrored source union + passes, CI linter; 4C = byte-identical serialized program) and **4C's is
  strictly stronger**.
- **4B vs 4C — mutually exclusive as terminal.** 4B eliminates any runtime program; 4C's entire bet is
  to KEEP the bytecode program at runtime for isomorphism + facilities (packrat, diagnostics,
  incrementality). Parsley resolves the tension by staging the machine *away* (4B wins the terminal) —
  precisely the choice 4C declines.

**Axis Q-ownership — whose representation `.then()` constructs:**

- **4D vs {4A, 4B, 4C} — mutually exclusive answer.** 4A/4B/4C build a parse-that-PRIVATE representation
  (a generalized `SpanKind` / a new instruction set / none). 4D builds NO private representation —
  `.then()` constructs `bbnf_ir::IrNode` shared with the grammar plane. These are two irreconcilable
  answers to "what does a combinator method construct." **Crucially, 4D is NOT incompatible with the
  runtime ENGINES of 4A/4C** — `bbnf_ir`'s interpreter (`parse_with_ir`) is a 4A-shaped tree-walk and
  its compiler is a 4C-shaped bytecode VM / 4B-shaped codegen. The incompatibility is ownership alone.
  Verdict conditionality: if LOCK-5 rules `bbnf_ir` the genuine shared substrate, **4D dominates 4A/4C
  on parsimony** (one IR, one optimizer, one iso bus) — modulo its TS block; if LOCK-5 keeps regex/grammar
  HIRs separate (a lawful functor, not one type), 4A/4C's private representation is the honest choice
  and 4D loses its "for free" claim.

**The non-family fault line (shared, resolved once for all four).** The monadic `chain`/value-plane
decision — opaque escape hatch vs selective-functor reformulation — is NOT a family; it is the identical
choice every family must make, and it gates every family's static-analysis subset. 4A answers it with
`Map{f:OpaqueFn}` + structural passes that never touch `f`; 4B threads it through `Map<A,F>`
monomorphization; 4C with an `Action`-ID side table; 4D with the `MapExpr`(transparent) /
`HostClosure`(opaque) split. All four inherit the *same* poison risk (an opaque action inside an `Alt`
kills that alternation's FIRST/dispatch) and the *same* guard (applicative/selective tiering + scoped
opacity: map-on-result ≠ structure-opaque). This is Brief 2, and it is the pass-2 hinge.

**Net.** The lock's real decision is a **single terminal runtime** chosen on Q-runtime (graph 4A |
bytecode 4C | none 4B) crossed with Q-ownership (private 4A/4B/4C | shared 4D), under a value-plane
policy chosen once. 4D is the parsimony maximum but BLOCKED on TS execution; 4A is the shipping
substrate with the cheapest path to proof; 4C is the strongest isomorphism with an open throughput bet;
4B is the raw-speed leader with a possibly-null reward. Pass-2 cannot pick without the two numbers below.

---

## 6. THE TWO PROTOTYPE BRIEFS (sharpest cross-family uncertainties)

**Brief 1 — VALUE-PLANE-SURVIVAL (pt-scratch).** Decides 4A/4C/4D single-tier viability at once: does a
runtime-analyzable representation survive the VALUE plane at parity with the inlined closure core? (Full
brief in §Structured output.)

**Brief 2 — CHAIN-MONADIC-CENSUS (spec-probe).** The all-family analyzability gate: do the real consumer
grammars use `chain`/`mapState` for genuinely data-dependent parsing, or is every site
applicative/selective-reformulable? (Full brief in §Structured output.)

*(Deferred per-family open question, not a brief: 4B's inlining reality — does removing the SmallBox
indirect-call barrier buy anything given the predicted-branch trap and PROVE-11's allocation-shaped
win? Runnable via generic-struct JSON parser + `cargo asm` + paired bench; single-family, so it yields
to the two cross-family cruxes above.)*
