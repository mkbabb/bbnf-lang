# LOCK-8 pass-3 — family 8A: THE SHAPE-STABLE V8 RUNTIME CORE

The V8-first formulation of the parse-that TS combinator core: every `ParserState` and
combinator object shape-stable and monomorphic by construction, an allocation-zero hot path,
diagnostics and packrat as shape-preserving optional planes — proven by inline-cache / maglev
probes and paired cold harnesses, never asserted. This family owns the *runtime execution face*
of the backbone (the V8 JIT), which is never conflated with the TS *checker* face (8B) nor with
the build-time emitter face (XL-D / 4B-TS). Every measurement names its HEAD: parse-that
`typescript/` @ published tag `v1.0.0` (pristine `60a7ba8`, upstream `ef10d5b`), dist built at
that cut; probes on **Node v26.0.0 / V8** (verified this pass), Apple Silicon, against
`typescript/dist/parse.js`. Consumer HEADs named where cited.

---

## 1. The mechanism made concrete — three shape-stable pillars

**(P1) Fixed-from-birth hidden class — the landed substrate.** `ParserState<T>` (`state.ts:36-53`)
declares its fields in constructor order (`src, value, offset, isError, furthest`) plus the three
diagnostic fields. The governing V8 surface (R4 §6a) is that `value:T` holds
string|array|object|undefined across every parse through one write choke point `unsafeSetValue`
(`state.ts:87-89`) — the sole megamorphic-write site by design; `ok/err/from` all funnel through
it (`state.ts:55-73`). PROVE-11 (GO / CONFIRMED_DELTA) made `suggestions`/`secondarySpans`
**symmetric with the pre-existing lazy `expected?`** (`state.ts:43`): declared native fields,
`undefined` at construction so the map is fixed from birth and never transitions, populated only
under `diagnosticsEnabled` via `(x ??= []).push`. Emitted-JS eager `= []` alloc census (prover
and adversary, exact): **6 → 0**; fused alloc **1.84 → 0.73 B/op (−60.1%)** under `--expose-gc`.
This is the alloc-zero-hot-path pillar, landed-grade (PT-3.W1 productionizes it verbatim).

**(P2) The megamorphic dispatch spine is irreducible — plan around it, do not wish it away.**
Every combinator threads through the `.parser` function-pointer field: `this.parser(state)` /
`p.parser(state)` (parser.ts:53,82,88,131,137; leaf.ts:44,49,62,140). This is THE megamorphic IC
of the engine (R4 §6c) — V8 sees many distinct closure shapes flow through one indirect call.
8A's design center is to keep every *object* monomorphic so the only polymorphism V8 confronts is
this deliberate function-pointer indirection, then to reduce what rides across it (allocation,
tuple materialization), rather than pretend the indirection itself can be monomorphized.

**(P3) Construction-time specialization, never per-parse.** Combinator closures allocate once per
`new Parser(...)` at grammar build (R4 §6e); `createLazyCached` uses a closure-local cache to
avoid function-object-mutation IC pollution (lazy.ts:19-25). The arity unrolls run their arity
branch **once at construction**: `all()` dispatches `parsers.length === 1 ? parsers[0].parser :
fuseAll(parsers)` (leaf.ts:161), and `fuseAll` selects `if (n === 2)` (leaf.ts:183), `if (n === 3)`
(leaf.ts:215), else the general pre-sized for-arm (leaf.ts:253-267, indexed write cursor `out[w++]`).
Each call site therefore yields one closure of one arity with constant-folded `p0/p1/p2` bindings
and no array-index load in the hot loop. `dispatch` builds its `Int8Array(128)` LUT once
(leaf.ts:100-151); `string` len-1 charCode (leaf.ts:282-294) and `regex` sticky-`test()` zero-alloc
(leaf.ts:344-358) are the same construction-time-branch discipline.

---

## 2. The three 8A directive questions — answered with trace + census evidence

### 2A. Does the arity-N specializer INLINE at each arity? — **structural NO; temporal YES; carried SEPARATELY.**

`--trace-maglev-inlining` on `dist/parse.js`, one hot arity per process (Node v26 default
production tier, base-pinned scratch), naming each fused closure by its bytecode size:

| target | bytecode size | maglev verdict (default budget) |
|---|--:|---|
| `fuseAll` arity-2 unroll | **169** | `cannot inline … size (169) >= max-size (100)` |
| `fuseAll` arity-3 unroll | **234** | `cannot inline … size (234) >= max-size (100)` |
| general for-arm (arity ≥ 4) | larger | not inlined |
| `parseStateInner` (the driver) | **162** | `cannot inline … size (162) >= max-size (100)` — at EVERY arity |

**Structural leg (refuted):** the arity-unrolled `fuseAll` bodies are **not inlined into the parse
driver at any arity** — they overflow maglev's default ~100-bytecode inline budget (the per-arm
error/backtrack/`out[w++]` blocks bloat the body). The driver `parseStateInner` (162) is likewise
never inlined into `parse`. So the arity unroll buys **no** call-site inlining. What it *does* buy
— the true, defensible rationale — is (a) closure-body monomorphism (constant-folded positional
bindings, no array load) and (b) elimination of the intermediate 2-tuple cascade that unfused
`a.then(b).then(c)` allocates (N−1 nested tuples collapse to one flat pre-sized array; leaf.ts:171
comment). The *leaves* do inline (`stringParser`, `unsafeSetValue`, `ok`, the `ParserState` ctor
are all small/`== Inlining`), so the leaf/state layer is inline-clean; the **sequencing layer is an
inline boundary by construction**.

**Temporal leg (confirmed, adversarially):** despite the non-inlining, the cold-paired win is real
and reproduces — fused `all(a,b,c)` **+10.1%** (prover) / **+15.6%** (VERIFY), unfused then-chain
**+13.2 / +13.6%**, alloc **−60.1%** exact — because the lever is *allocation elimination*, not
inlining. The two legs MUST be carried apart: the proposition "arity unroll → inlining" is FALSE;
"arity unroll → monomorphic, alloc-free sequencing → measured cold win" is TRUE. Any PT-3.W4 claim
attributing the speed to inlining is the exact §0/09 self-deception (as VERIFY halved the dispatch
lane +10.2% → ~+5% for the same reason: a wide per-round band, min 0.695 / max 1.420, misread at
its top). The honest mechanism is the alloc census; the honest carry for dispatch is ~+5% no-regress.

### 2B. Arity histogram from the ACTUAL live corpus — **the arity-2/3 `all()` unroll has ZERO live external consumers.**

Fresh per-tree census, this pass, HEADs named (2026-07-16):

| consumer | version / HEAD | parse-that dep | combinator usage on tree |
|---|---|---|---|
| **value.js** | **4.0.0**, branch `tranche-u` @ `86a9a17` | **`dependencies:{}` — 0 lockfile hits** | **NONE** — `all/any/dispatch/sepBy/many/chain` = 0; two comments declaring modules "parse-that-FREE" (`src/subpaths/math.ts:2`, `src/subpaths/transform.ts:4`) |
| **keyframes.js** | 6.0.0 | none | NONE — self-declares "parse-that-FREE, 0 parse-that modules" (`src/animation/internal/leaves.ts:9-10`); its `Promise.all` hits are native |
| **latex-paper** | 0.2.1, pins **`^0.7.1`** (installed 0.7.1, pre-1.0.0) | `^0.7.1` | **the ONLY live external combinator consumer**: `any()` at arity-3 (`src/grammar/text.ts`) + variadic `any(...alts)` + a multiline `any(` (`document.ts`); imports `Parser regex string any ParserState` across 5 grammar files (`commands.ts:9 text.ts:5 environments.ts:6 document.ts:5 bibtex/parser.ts:7`); **`all()` = 0**, 6 `any/all` call-sites total |

**This overturns R4's premise and, transitively, PT-3.W1's gate text.** R4 §7 recorded value.js @
**3.1.0** with **59 `all()` sites** and "hot 3-tuples" — the exact justification stamped into the
source (leaf.ts:214-ff, "the hottest value.js shape / calc·rgb·hsl triples"). That corpus **no
longer exists on tree**: value.js divorced parse-that entirely across 3.1.0 → 4.0.0 (`deps:{}`, not
a version bump). The **live external arity histogram** is therefore: `any` arity-3 + variadic (both
hit the general for-arm, NOT the arity-2 unroll), `all` empty. Under Constitution viii
(consumer-less substrate forbidden, Y.13), the arity-2/arity-3 `fuseAll` unroll is presently
justified ONLY by the first-party floor `test/benchmarks/pt-b3-fusion.bench.ts` (R4 §4) — a
legitimate first-party-consumer disposition (an 8D adjudication) — OR it must be re-anchored to
latex-paper's real `any()` shapes. It cannot be re-justified on value.js's dead 59-tuple corpus.
Note further that latex-paper pins 0.7.1 (pre-1.0.0), so it does not even *see* today's `fuseAll`
unroll: the arity-2/3 `all()` unroll currently runs against **no external code at all**. The
specialization decision is thus a keep-as-first-party-bench-consumer vs collapse-to-`any()`-arm
question, not a value.js-driven one.

### 2C. Relationship to the build-time TS emitter (XL-D / 4B-TS) — **distinct mechanisms; a combinator→JS emitter is constructible, NEW, and the only route past the inline ceiling.**

XL-D is RATIFIED (LOCK-4 pass-2, measured): the runtime decoders (4C/4D switch-loops, the 4A
graph-interpreter) are REFUTED as the fast TS face (~−22%, ties naive); the fast face is the
**build-time emitter (4B-TS)**. Cross-lock read of the emitter's state (PT-3.md §2 interlock): the
GN-4.W4 tape decoder seeding the TS backend is a **monomorphic tape decoder that imports no
combinators** (oracle `JSON.parse`) — grammar/tape → JS straight-line code, a *code generator*. The
arity-N `fuseAll` selector, by contrast, is a **runtime closure picker** (a construction-time `if`
choosing among hand-written closures) — it emits no code. **They are different mechanisms.**
Scratch-construction verdict: a build-time combinator→JS fuser (LOCK-4B-TS's "build-time fuser emits
specialized chains") IS constructible on the TS side — discover static-arity `all(...)` / `any(...)`
call sites and emit each as straight-line offset-threading with shared early-returns — and it is the
**only** route that can push a fused sequence body under the ~100-bytecode maglev budget (minimal
per-site straight-line code, not one 234-byte general closure), converting §2A's non-inlining
boundary into an inlined one. But it is a NEW subsystem gated on: (i) a static-arity call-site
discovery pass, (ii) the same build tooling XL-D's emitter uses, (iii) proof that the emitted
per-site bodies actually land < 100 bytecode and inline. Absent that construction, the existing
runtime unroll's non-inlining ceiling is structural and permanent — no maglev flag bump "fixes" it,
because the lever was never inlining.

---

## 3. Plane-wide (answered early)

**CONSUMER LIVENESS.** The live forward-looking consumer of the combinator surface is **latex-paper
alone**, and it is effectively **frozen** at `^0.7.1` — a pre-1.0.0 pin that never saw the 1.0.0
`*Span` excision or `chain()` C-16 cut, so a 1.1.0 bump is an untested 0.7→1.1 jump (PT-3.W0 owns
the migration dry-run). value.js's **on-tree** posture at 4.0.0 (`tranche-u` @ 86a9a17) is a
**total divorce** (`deps:{}`, zero parse-that imports, only "parse-that-FREE" comments) — this is
XL-G P1 surfaced independently. Whether that divorce is *intended* or a `tranche-u` WIP-branch
artifact is the standing owner question (carry BOTH hypotheses per XL-G, non-blocking); but on the
tree of record value.js is **not** a combinator consumer, and the design must not lean on its dead
59-tuple corpus. keyframes.js is parse-that-FREE and consumes nothing (not even transitively at
6.0.0). Net: the external combinator contract is a **single frozen `any()`-shaped consumer**.

**SPEC FIDELITY.** PT-3.md diverges from the measured outcome at three points feeding a spec re-cut:
(i) **W1's gate lane text** carries the R4 "value.js 59 `all()` sites / hot 3-tuples" premise as
live justification — **stale**; the re-cut must state the value.js divorce and re-anchor the
non-regression floor to latex-paper `any()` + the first-party bench. (ii) **W4's monomorph floor**
(pt-monomorph.paired.mjs, arity-2/3 unrolled arms ≥ general arm) is correct as a *first-party*
invariant but must NOT be captioned as protecting a value.js shape — the caption is the drift.
(iii) The **inlining rationale**: nowhere in PT-3.md is the arity unroll claimed to inline, which is
correct — but W4's "preserve the deliberate monomorphizations" language must explicitly disclaim
inlining and cite the §2A structural leg (169/234 > 100), or a later author will re-introduce the
fallacy. All three are documentation re-cuts, not mechanism changes.

---

## 4. Substrate — integration seams (path:line)

- Shape core: `state.ts:36-53` (fields), `state.ts:87-89` (`unsafeSetValue` write choke),
  `state.ts:55-73` (`ok/err/from` funnel), `state.ts:44-45` (PROVE-11 lazy fields — landed lever).
- Dispatch spine: `parser.ts:24-32` (`Parser` shape: `id, state, flags, parser, context`),
  `parser.ts:34-49` / `:51-75` (`parseState` / `parseStateInner` — the 162-byte non-inlinable
  driver), `parser.ts:53,82,88` (the megamorphic `.parser` IC).
- Arity specializers: `leaf.ts:28-79` (`any` arity-2 unroll + general), `leaf.ts:154-273`
  (`all` → `fuseAll`: dispatch `:161`, arity-2 `:183`, arity-3 `:215`, general for-arm `:253-267`),
  `leaf.ts:100-151` (`dispatch` `Int8Array(128)` LUT).
- Heterogeneity proxy (for a build-time selector): every `Parser` carries `context.name` set by
  `createParserContext` (`state.ts:173-189`, registry `parserNames` `state.ts:141-171`) — a faithful
  SharedFunctionInfo proxy (same factory ⇒ same code object ⇒ one call-IC), so
  `new Set(parsers.map(p => p.context.name)).size` computes SFI-distinctness with no AST.
- Deopt watch: `mapState` builds `Object.create(state)` per invocation (parser.ts:177-180) — the one
  prototype-chain object in the core, and (grep of all three consumers + core) it has **zero
  callers** — W4's shape oracle must confirm it is the only prototype-chain object and that a
  fixed-shape struct-view replacement (`{src, value, offset, isError, furthest}` under a shared
  `ReadonlyState<T>` interface, no `Object.create`, no `as unknown as` cast) leaves the map stable.
- Optional planes (must stay shape-preserving): diagnostics global latch `utils.ts:6` (PT-E1 makes
  it per-parse-scoped); packrat `PACKRAT_ARMED` (packrat.ts:156) is a TRUE no-op until first
  `memoize()` (packrat.ts:290); the sole packrat shape divergence is `MemoCell.ans` (`Answer` vs
  `LR`, packrat.ts:125-129), quarantined behind arming — never on the default LL(1) path.
- Certified method: single-build-per-process cold paired interleaving (PROVE-11 `run-paired.sh` +
  `measure-prove11.mjs`), N≥12 rounds, min AND median, loadavg per round — **not** `vitest bench`
  (warm, throughput-amortized, cross-run; load-confounded, PT-3 §1.3 refuses it as evidence).

---

## 5. Prior art (citations)

- **V8 maglev inline budget.** The observed `max-size (100)` bytecode ceiling and `max_inline_depth`
  are maglev heuristics (`v8/src/maglev/maglev-inlining.*`; the `--max-maglev-inlined-bytecode-size*`
  flags). Confirms inlining is **size-gated, not shape-gated** — the direct basis of §2A: a body
  monomorphic and alloc-free can still sit above the inline boundary.
- **Hidden classes / IC monomorphism.** V8 "maps" / transition trees; declaring every field at
  construction (fixed-from-birth) is the canonical shape-stability idiom (V8 team "Fast properties in
  V8"; mrale.ph "What's up with monomorphism?"). PROVE-11's `%HaveSameMap` probe (PT-3.W4) under
  `node --allow-natives-syntax` is the direct oracle; the `mono → poly(≤4) → mega` IC-degree model
  underlies the SFI-heterogeneity selector.
- **Combinator fusion by staged code-gen.** The 4B-TS build-time route mirrors staged / partial-
  evaluation parser generators — Jonnalagedda, Rompf, Odersky, "Staged Parser Combinators for
  Efficient Data Processing" (OOPSLA'14); it is the TS analogue of Rust's comptime monomorphization
  (`nom` / `combine` monomorphized-closure models; XL-E). The runtime arity unroll is a manual
  `#[specialize]`; a build-time emitter is the true staged form.
- **Packrat / arena.** Ford's packrat + the Warth–Douglass–Millstein left-recursion algorithm
  (`Answer`/`LR`/`Head` + `getCijKey` float64 key, packrat.ts:79-127); the alloc-zero-hot-path goal
  parallels tree-sitter's reusable-node reuse and regex-automata's `Cache` amortization.

---

## 6. Named risks & counterexamples

- **R-A1 — the inlining fallacy.** *Counterexample:* a W4 author reads fused +15% and writes "the
  arity unroll lets V8 inline the sequence." §2A refutes this at every arity (169/234/162 > 100). If
  re-attributed to inlining, a maglev-budget flag bump becomes the phantom "fix" and does nothing —
  the lever is alloc elimination. Structural (no-inline) and temporal (alloc-win) legs stay separate
  or the family reasons on a false model.
- **R-A2 — dead-corpus specialization (Constitution viii).** *Counterexample:* PT-3.W4 preserves the
  arity-2/3 unroll citing "value.js's 59 `all()` sites." Those sites are gone (value.js 4.0.0
  `deps:{}`, this-pass census). Keeping the unroll is defensible only as a first-party-bench consumer
  (8D) or re-anchored to latex-paper's `any()` arity-3 + variadic — never on the stale corpus.
- **R-A3 — the `mapState` prototype view deopts callers.** *Counterexample:* `Object.create(state)`
  (parser.ts:177-180) is a slow prototype-chain object; a hot grammar routing through `mapState`
  could deopt the caller. It has zero live callers today, so the fixed-shape-view fix is zero-risk —
  but W4 must probe callers under `%GetOptimizationStatus`, not the object alone.
- **R-A4 — measuring the wrong face.** *Counterexample:* benchmarking the 4C/4D runtime switch-loop
  as "the TS core." XL-D refuted it (~−22%); it survives only as an isomorphism oracle. 8A measures
  the closure runtime; XL-D / 8C own the emitter.
- **R-A5 — load-confounded win.** *Counterexample:* the naive `pt-b3` cross-run (PROVE-11 §3.1:
  pristine loadavg 12, modified loadavg 27) fabricates or erases the delta and halved the dispatch
  claim. Only single-process paired interleaving certifies; every table states armed/unarmed packrat
  (shipping path is unarmed).

---

## 7. What a prototype must test to move family 8A

1. **Per-arity inline verdict, base-pinned (XL-H).** Re-run `--trace-maglev-inlining` at arities 1..6
   on a fresh `v1.0.0` scratch build, HEAD named; confirm the 169/234-byte non-inline boundary, the
   162-byte driver boundary, and record the arity at which the general arm crosses. Structural leg;
   report separately from any ns.
2. **The build-time-emitter counterfactual (the decisive experiment).** Emit straight-line
   `all(a,b,c)` per static-arity call site (scratch, 4B-TS shape), measure the emitted-body bytecode
   size, re-trace: does the per-site body land < 100 and INLINE where the runtime unroll cannot?
   Cold-paired emitted-vs-runtime-unroll closure. The one experiment that dissolves §2A's ceiling —
   and the one that decides whether the runtime unroll should exist at all.
3. **The re-anchored arity histogram → floor.** Rebuild the specialization's non-regression floor on
   latex-paper's actual `any()` shapes (arity-3 + variadic, both on the general for-arm) plus the
   first-party bench, since value.js's `all()` corpus is dead. Decide keep-vs-collapse the arity-2/3
   `fuseAll` unroll and feed the disposition to 8D.
4. **Shape oracle (`%HaveSameMap`).** Two `ParserState`s — one diagnostics-off, one `suggestions`-
   populated — same map TRUE (fixed-from-birth); assert `mapState`'s `Object.create` is the only
   prototype-chain object, its (zero) callers un-deopted, and that the fixed-shape struct-view
   replacement holds the map. Under `node --allow-natives-syntax`.
5. **Alloc-zero certification, default path.** `--expose-gc` heapUsed-delta B/op on fused/dispatch
   with packrat unarmed and diagnostics off; confirm the −60.1% alloc holds and no per-parse
   allocation slips in (the true source of the temporal win, per §2A).
