# LOCK-8 · Family 8A — THE SHAPE-STABLE CORE (pass 1, research)

*The V8-first formulation of the parse-that TypeScript backbone: every parser state and
combinator object shape-stable and monomorphic by construction, an allocation-zero steady path,
diagnostics and packrat as shape-preserving optional planes — and the whole discipline proven
structurally by V8 IC-state probes, not merely by nanoseconds.*

Package root `~/Programming/parse-that/typescript` (READ-ONLY to the bbnf campaign; landed only
by the owner or a parse-that-side session). Build target ES2022 (`tsconfig.json` `"target":
"ES2022"`, `useDefineForClassFields` defaults true → class fields are DEFINED at construction,
native-field semantics), esbuild via vite, Node v26 / V8. All shape claims below are stated as
falsifiable IC-state assertions, not adjectives.

---

## 1. The family thesis, and where it diverges from PROVE-11

PROVE-11 (CONFIRMED_DELTA) is the seed but not the family. It landed ONE lever — the two eager
diagnostic arrays (`ParserState.suggestions`/`secondarySpans`, eager `= []` at `state.ts:44-45`
in the current HEAD tree `ef10d5b`, **not yet productionized upstream**) made lazy. It proved
the *mechanism* by an emitted-JS alloc census (6 eager sites → 0) and by paired timing. It did
**not** prove *shape*: the report asserts "hidden class is fixed from birth" but never probed a
V8 map. Family 8A's distinguishing claim is that shape stability and monomorphism are
**structural invariants provable with `%HaveSameMap` / `%GetOptimizationStatus` / `--trace-ic`**,
elevating the backbone from "measured faster" to "measured monomorphic and deopt-free." The 8A
substrate is therefore a **probe harness plus a shape contract**, of which PROVE-11's lazy-field
patch is the first (and smallest) instance.

The four architectural centers of 8A, in dependency order: **(A)** the shape contract on state
and combinator objects; **(B)** the central megamorphic *call* IC — the one deep V8 problem the
closure core cannot wish away, and the constant-capture specialization that bounds it;
**(C)** the allocation-zero steady path and its honest deliverable boundary; **(D)** the IC-state
probe substrate that makes A/B/C falsifiable. Diagnostics and packrat are treated throughout as
**optional planes that must not perturb the default-path shape** — a shape-preservation obligation,
not a feature.

---

## 2. Center A — the shape contract (state + combinator objects)

**A.1 `ParserState` — the governing surface (R4 §6a).** Class at `state.ts:36-53`: fields in
declaration order `expected?`, `suggestions`, `secondarySpans`, then ctor params `src`, `value:
T=undefined`, `offset=0`, `isError=false`, `furthest=-1`. Under ES2022 `useDefineForClassFields`,
every field — including the optional `expected?` at `state.ts:43` with no initializer — is
**defined at construction** (emitted `expected;` → own-property `undefined`), so the hidden class
(V8 map) is fixed from birth *provided no field is assigned a value of a different elementsKind
representation than its birth value*. The two live shape risks:

- **Eager arrays (the PROVE-11 lever).** `suggestions/secondarySpans = []` allocate two arrays
  per `new ParserState`, and `mergeErrorState` (`utils.ts`) **re-allocates both on every
  furthest-offset advance** — which fires constantly during success-path backtracking though
  diagnostics are OFF. The 8A form generalizes the fix beyond "make these two lazy" to a **rule**:
  no `ParserState` field carries an eager heap initializer; every optional payload is `undefined`
  at birth and lazily `(x ??= []).push(...)` under an explicit gate. The map is then identical for
  a diagnostics-off and a diagnostics-populated state (the field holds `undefined` vs a fresh
  array — **same map**, verified in Center D by `%HaveSameMap`, the probe PROVE-11 omitted).
- **`value: T` representation transitions.** `value` is written by `unsafeSetValue`
  (`state.ts:87-89`) with string | array | object | undefined across parses. Because these are all
  *Tagged* pointers, V8 keeps one map (no Smi→Double→Tagged transition). The one hazard is a
  `value` that is sometimes a Smi (integer) and sometimes an object — a JSON-number parser writing
  a raw `number`. 8A names this: numeric leaf results should be boxed or the field pinned Tagged,
  or the `value` field's map splits polymorphically. Probe: `%DebugPrint(state)` elementsKind.

`offset`/`furthest` are always Smi (integer arithmetic only), `isError` always boolean, `src`
always string — all representation-stable. The `save()` return `{offset, value}` (`state.ts:75`)
is a fresh two-field literal of one shape — monomorphic, fine.

**A.2 `Parser` — one hidden class by construction (R4 §2).** `Parser<T>` at `parser.ts:24-32`:
fields `id:number` (Smi, `PARSER_ID++`), `state`, `flags:number` (Smi), ctor params `parser`
(the function-pointer field) and `context`. Every `new Parser(...)` at every combinator method
runs the same field sequence → **all Parser instances share one map**. This is already correct;
8A's obligation is to *keep* it correct and *prove* it (`%HaveSameMap(then(...), or(...))` TRUE).
The `flags` field is written post-construction in `trim`/`eof`/`call` (FLAG_TRIM_WS/FLAG_EOF) —
but it is declared Smi and written Smi, no transition.

**A.3 The `mapState` prototype-chain view — the one genuine deopt (R4 §6b, HIGH).**
`parser.ts:168-181`: on success, `mapState` builds `const oldView = Object.create(state)` then
sets `oldView.offset`/`oldView.value` and passes `fn(state, oldView)`. This is the single worst
V8 surface in the core: `Object.create(state)` mints a **new map with `state` as its prototype**,
so (i) `oldView` has a *different* hidden class than every real `ParserState`, (ii) any read of
`oldView.offset` after the own-property write is fine but reads of *inherited* fields
(`oldView.src`, `oldView.isError`) walk the prototype chain, and (iii) the user callback `fn`'s IC
now sees **two shapes** for its state argument (real state + the view) → polymorphic at best,
and if `fn` is shared across sites, a step toward megamorphic. 8A's fix: replace
`Object.create(state)` with a **plain monomorphic snapshot of a fixed declared shape** —
either a tiny frozen-shape record `{ src, value, offset, isError, furthest }` built by one
constructor (so all views share one map, distinct from but stable relative to `ParserState`), or a
pooled `ParserState.clone()` (`state.ts:101-109`, already one map) with the two fields overwritten.
The snapshot object must be *the same map on every `mapState` invocation*. This is a real source
change (not verification-only) and belongs in 8A's mechanism, adjacent to but beyond PT-3.W4's
audit-only scope.

---

## 3. Center B — the central megamorphic CALL IC (the deep problem)

**B.1 The site.** Every combinator dispatches through the `.parser` function-pointer field:
`this.parser(state)` / `p.parser(state)` — `parser.ts:53,82,88,131,137,148,172,193,196`;
`leaf.ts:44,49,62,140,193,201,224,232,240,261`. R4 §6c names this "THE megamorphic IC of the
engine: V8 sees many distinct closure shapes flowing through one indirect call." Prior families
of analysis conflate two very different V8 penalties; 8A must separate them precisely, because the
separation *is* the family's central finding:

- A megamorphic **property LOAD** IC (>4 maps) falls back to a global stub-cache hashtable and
  degrades **10–50×** ([thenodebook], [erdem.pl]). This is the scary number — and it does **not**
  apply here. `this.parser` is a load off `Parser`, and **all `Parser` objects share one map**
  (A.2), so the *load* of the `.parser` field is **monomorphic**. No hashtable fallback.
- A megamorphic **CALL** IC (the callee closure varies across >4 targets) does **not** hit the
  property hashtable. Its cost is narrower and specific: **TurboFan stops inlining the callee and
  typically declines to further optimize around that call** ([v8-perf/compiler.md],
  [sujeet.pro]). The call itself remains a plain indirect jump through the JSFunction. So the real
  price of `this.parser(state)` is **lost inlining of the child parser**, not a 10–50× cliff.

**B.2 Why arity-unrolling works, generalized.** V8 separates *feedback* from *code* so it keeps
per-closure type info while sharing optimized code ([v8.dev, feedback separation]); crucially,
**a call whose callee is a closure-captured constant can be inlined** even when the generic
site cannot. That is exactly why PT-B3's arity-2/3 unrolls win: `fuseAll` arity-2/3
(`leaf.ts:183-251`) and `any` arity-2 (`leaf.ts:39-57`) capture `const p0 = parsers[0]; const p1
= parsers[1]` and call `p0.parser(state)` where `p0` is a **captured constant** — TurboFan sees a
known callee and can inline it. The general arm's `parsers[i].parser(state)` (`leaf.ts:61,261`)
loads a *varying* closure from an array → the megamorphic call, no inlining. **The unroll is not a
micro-hack; it is the one lawful route to a monomorphic, inlinable call in a closure-combinator
core.** 8A's thesis: make constant-capture a *discipline*, not three hand-written arities —
a build-time / construction-time specializer that emits an unrolled positional closure for any
static `all`/`any`/`then`-chain up to a measured arity cap, capturing each child as a constant.
This is the V8 face of LOCK-4B COMPTIME-FUSION and shares the graph shape of LOCK-4A; 8A owns the
*V8 justification and the measurement* (does inlining actually fire? — probe in Center D via
`--trace-turbo-inlining` / `%GetOptimizationStatus`), the sibling locks own the graph.

**B.3 The honest boundary (the refutation guard binds here).** PROVE-11's VERIFY halved the
prover's dispatch claim (+10.2% → ~+5%) precisely because `dispatch` (`leaf.ts:134-145`) calls
`parsers[idx].parser(state)` with `idx` varying by input byte — an **irreducibly megamorphic
call**; no lazy-field or unroll touches it, so its win was always small and the +10.2% was window
noise near the top of a wide band (min 0.695 / max 1.420 over 24 rounds). 8A must carry dispatch
as a **~+5% no-regression lane, never a ≥10% gate** (SPEC §0 honest-number law). The deep question
8A poses but does not assume: *can the input-dependent dispatch call be de-megamorphized at all
within a closure core?* The candidate answers — a single monomorphic interpreter loop over a
compact instruction table (LOCK-4C VM-CORE) instead of N closures, or leaving it as an accepted
megamorphic-call residue whose only cost is lost inlining of a leaf — are a genuine 8A/4C seam to
be **measured, not asserted**. The null hypothesis (leave the closures; the megamorphic *call* is
cheap because the *load* is monomorphic) must be defeated by numbers or it wins.

---

## 4. Center C — the allocation-zero steady path, and its true boundary

**C.1 What is already zero-alloc.** `state.ok/err/from` (`state.ts:55-73`) mutate the state in
place through `unsafeSetValue` — no new state per combinator step. `string` len-1 charCode
(`leaf.ts:282-294`), `regex` sticky `test()` (`leaf.ts:344-358`), `trimStateWhitespace`
(`leaf.ts:372-391`) are all allocation-free on the success path. `createLazyCached` uses a
closure-local cache to avoid function-object IC pollution (`lazy.ts:19-25`). Combinator closures
allocate once per `new Parser` at grammar-build, nil per parse (R4 §6e).

**C.2 The residual allocations, classified.** Three remain on the steady path:
1. **`new ParserState(val)` once per top-level `.parse()`** (`parser.ts:52`) — plus a second
   error-view state at `parser.ts:61` **only on error**. One alloc per parse: negligible, and the
   error state is off the success path. *Optional* 8A lever: a pooled/reused top-level state
   (reset in the `parseState` try/finally alongside the packrat epoch, `parser.ts:43-48`) — but
   this risks re-entrancy bugs (the `.map` callback that re-parses, R4/packrat.ts note) and is
   likely not worth it; **name it and probably reject it.**
2. **`then` builds `[value1, state.value]`** (`parser.ts:91`) — one 2-tuple per `then`-node per
   parse. A deep `a.then(b).then(c)` builds N−1 nested tuples (the exact waste `fuseAll` was built
   to avoid, `leaf.ts` header comment). 8A guidance: **prefer `all(...)` over `.then` chains** for
   sequences (one flat array vs N−1 nested tuples) and let the specializer (B.2) fuse it.
3. **`fuseAll`/general `all` build one result array** (`leaf.ts:191,222,257`) — this is the
   **deliverable**, the parse result itself.

**C.3 The honest boundary — "zero-alloc except the deliverable."** A tree-building combinator
parser *cannot* be zero-alloc while its contract is "return a JS value/array/object," because the
returned structure is an allocation by definition. Only a **tape/columnar output contract** (write
byte-offsets to a pre-sized buffer, materialize lazily — the Rust side and PROVE-14's byte-exact
tape) removes deliverable allocation, and that is an **API change** owned by LOCK-6 / the TS
backend seed (GN-4.W4), not by 8A. 8A's precise claim is therefore: **the steady path is
allocation-zero up to the deliverable; the deliverable is one flat array per `all`-node (never
N−1 nested tuples); and going below the deliverable requires the tape contract, which is out of
family.** Stating this boundary crisply is itself a deliverable — it forecloses the fantasy that
the closure core can be made truly zero-alloc without changing its public shape.

---

## 5. Center D — the IC-state probe substrate (the proof method, the true novum)

This is what 8A adds that PROVE-11 (timing-only) and PT-3.W4 (a `%HaveSameMap` sketch) do not
carry to completion: a **structural monomorphism oracle**, so shape claims are *asserted*, not
narrated. Node v26 exposes V8 natives under `--allow-natives-syntax`; the harness is a
`test/v8-shapes.runner.mjs` (a plain node runner, since vitest does not forward the flag) that
asserts:

- **`%HaveSameMap(a, b)`** — a diagnostics-off `ParserState` and a diagnostics-populated one have
  the SAME map (A.1); two different combinators' `Parser` objects have the SAME map (A.2); every
  `mapState` snapshot object has the same map across invocations (A.3). This is the assertion
  PROVE-11's REPORT §2 *claimed in prose* but never executed.
- **`%GetOptimizationStatus(fn)`** on the hot combinator closures after a warmed run — asserts
  **optimized (TurboFan), not deoptimized**; a bit-flag decode (`kTopmostFrameIsTurboFanned`,
  `kIsFunctionOptimized`) turns "is the hot path actually JITed" into a boolean gate.
- **`%DebugPrint(state)`** parsed for `elementsKind` / `map` address — catches the `value`
  representation split (A.1) and any silent transition.
- **`--trace-ic`** over a fixed corpus, post-processed to assert the `this.parser` call site
  reaches at most the intended state (monomorphic where specialized, and — the honest part —
  *megamorphic-but-only-a-call* where dispatch is input-dependent, B.3). **`--trace-opt` /
  `--trace-deopt`** assert **zero deopts** on the steady path (a deopt in the hot loop is a
  correctness-of-shape failure, not a speed nit).
- **`--trace-turbo-inlining`** (or `%GetOptimizationStatus` deltas) to prove B.2: the unrolled
  arm's `p0.parser` **actually inlines** and the general arm's `parsers[i].parser` **does not** —
  turning "unrolling helps V8 inline" from folklore into a checked fact.

The measurement half stays the certified PROVE-11 method (PT-3 §1.2): cold per-op median ns via
`process.hrtime.bigint`, **single build per process, paired A,B,A,B… interleaving**,
median-of-13-batches × ≥300k ops, N≥12 rounds, min AND median, per-round loadavg, **packrat
armed-vs-unarmed stated in every table**, no warm `vitest bench`. The shape oracle (structural) and
the paired harness (temporal) are the **two legs** of every 8A claim; neither alone suffices.

**Optional plane obligations.** *Packrat* (`packrat.ts`) is a true no-op until first `memoize()`
(`PACKRAT_ARMED` false, `packrat.ts:156/290`); its `MemoCell`/`Head`/`LR` records
(`packrat.ts:103-133`) and module-global `Map`s must **not** touch the default-path shape — the
`parseState` epoch swap (`parser.ts:43-48`) is a reference swap, zero-cost unarmed. 8A gate: the
shape oracle is run with packrat unarmed AND armed; the default-path maps are identical.
*Diagnostics* (the PROVE-11 lever + PT-E1 per-parse scoping) must populate the lazy fields under a
scope flag **without** adding a field or changing the state map — the `%HaveSameMap` off-vs-on
assertion is the guard.

---

## 6. Integration seams (path:line, where extant code serves)

- Shape lever (Center A.1): `state.ts:44-45` eager arrays → lazy `undefined`; reset sites
  `utils.ts` `mergeErrorState`/`resetErrorState`; guarded reads `debug.ts:180,185`;
  copy-through `parser.ts:63-64` (error view — off success path). This is PROVE-11 `patch.diff`
  source portion, ~14 net lines.
- `mapState` deopt (A.3): `parser.ts:168-181` — replace `Object.create(state)` with a
  fixed-shape snapshot; the only genuinely *new* source in 8A's mechanism.
- Constant-capture specializer (B.2): generalize `leaf.ts:39-57` (`any` arity-2),
  `leaf.ts:183-251` (`fuseAll` arity-2/3); the general arms `leaf.ts:59-72,255-272` are the
  megamorphic residue to shrink.
- Dispatch residue (B.3): `leaf.ts:134-145` — the input-dependent megamorphic call; carry
  ~+5% no-reg, do not gate.
- Probe substrate (Center D): new `test/v8-shapes.runner.mjs` (natives) + the paired harness
  `test/benchmarks/pt-*.paired.mjs` pattern; the regression floor stays
  `test/benchmarks/pt-b3-fusion.bench.ts` (R4 §4).
- Parser id/flags Smi fields: `parser.ts:25-27`; `call()` flag fast-path `flags===0`
  `parser.ts:438`.

---

## 7. Prior art (citations)

- **V8 IC states & the megamorphic distinction.** Inline caches transition uninitialized →
  monomorphic → polymorphic (≤4) → megamorphic; at megamorphic, inlining stops
  ([v8-perf/compiler.md], [sujeet.pro/v8-engine-architecture]). Megamorphic **property loads**
  fall back to a global stub-cache hashtable, 10–50× ([thenodebook], [erdem.pl]) — the number that
  does **not** apply to our monomorphic `.parser` load.
- **Feedback/code separation → per-closure type info, shared optimized code**, enabling
  constant-callee inlining ([v8.dev inline-caching/feedback]). The basis for B.2.
- **`Object.create` / late prototype changes de-optimize** — objects meant to share a hidden class
  must share a prototype ([v8.dev/blog/fast-properties], [mrale.ph hidden-classes-vs-jsperf]).
  The basis for A.3.
- **Typed inline caching for TS** (Choi et al., PLDI'19 [iacoma pldi19_2]; ACM [3428242]) and
  **basic-block versioning with typed object shapes** (Chevalier-Boisvert, [arxiv 1507.02437]) —
  the academic frame for "shape-stable by construction beats speculation."
- **In-family precedent.** PROVE-11 REPORT+VERIFY (the lazy-field lever, CONFIRMED_DELTA, the
  dispatch honest-number correction); R4 §6 (the V8-sensitive surface map); PT-3.md W4 (the
  shape-audit wave 8A completes and extends).

## 8. Named risks & counterexamples

- **R-A (the dispatch inflation trap).** A landing reads +9% on dispatch in one window and reports
  "near gate." Counter: dispatch is an irreducibly megamorphic call (B.3); its honest carry is
  ~+5%; the two-window paired median is the arbiter. *Inflating it is the exact §0/09
  self-deception the VERIFY already caught once.*
- **R-B (`%HaveSameMap` false-green).** The oracle passes because both probed states happen to be
  freshly constructed and *neither* exercised the transition path. Counter: probe must construct
  one state, run a full parse that populates diagnostics, THEN compare to a virgin state — the map
  must survive population, not just construction.
- **R-C (the specializer perturbs codegen).** Generalizing arity-unroll changes the emitted
  closure and a previously-inlined arm stops inlining. Counter: `--trace-turbo-inlining` before/after
  + the monomorph paired floor (PT-3.W4) — the unrolled arm must still beat the general arm at its
  arity, or the signature reverts.
- **R-D (mapState fix breaks a consumer).** value.js/latex-paper read `oldState` fields the
  snapshot omits. Counter: enumerate `mapState` consumers (R4 §7) before narrowing the view shape;
  the snapshot must carry every field any consumer reads (`src,value,offset,isError,furthest`).
- **R-E (pooled-state re-entrancy).** A pooled top-level `ParserState` (C.2 lever) corrupts a
  nested re-parse (`.map` callback re-parsing mid-grow). Counter: the packrat epoch already handles
  this with a snapshot/restore (`parser.ts:43-48`); either mirror it exactly or **reject the pool**
  — the one-alloc-per-parse cost is negligible and likely not worth the hazard.
- **R-F (numeric `value` split).** A JSON/number leaf writes a raw Smi into `value`, splitting the
  state map polymorphically. Counter: `%DebugPrint` elementsKind assertion in the oracle; box or
  pin the field if the split appears.

## 9. What a prototype must test to move family 8A

1. **Build the shape oracle** `test/v8-shapes.runner.mjs` under `--allow-natives-syntax` and make
   it assert, on the *current* 1.0.0 tree: `%HaveSameMap` for (a) two combinator `Parser`s [expect
   TRUE], (b) diagnostics-off vs diagnostics-on `ParserState` **with PROVE-11 applied** [expect
   TRUE] and **without** [expect the eager-array state still same-map but proving the alloc, not the
   shape, is the issue], (c) `mapState`'s `Object.create` view vs a real state [**expect FALSE** —
   this is the deopt, the prototype's central falsifiable claim].
2. **Prove B.2 mechanically:** `--trace-turbo-inlining` on a warmed `all(a,b,c)` (arity-3 unrolled)
   vs `all(a,b,c,d,e)` (general arm) — assert the child `parser` calls inline in the former and do
   NOT in the latter. If inlining does *not* fire even in the unrolled arm, the entire
   constant-capture thesis is refuted and 8A pivots to 4C (VM-core).
3. **Re-measure dispatch honestly** in ≥2 load windows, paired — confirm ~+5%, not ≥10%.
4. **Land + probe the `mapState` snapshot fix:** replace `Object.create` with a fixed-shape record;
   assert `%HaveSameMap(view_i, view_j)` TRUE across invocations and `%GetOptimizationStatus` on the
   `fn` callee shows no deopt; paired-measure a `mapState`-heavy grammar for no-regression.
5. **Deopt sweep:** `--trace-deopt` over the value.js corpus (the real consumer) — assert **zero**
   steady-path deopts; any deopt names a shape defect to close.

The prototype's single most important output is item 1(c) + item 2: whether the `Object.create`
view is really the shape defect it appears, and whether constant-capture inlining actually fires —
those two measurements decide whether 8A's closure-core-made-monomorphic is *achievable* or must
concede to a VM core.

---

*Sources:* [v8-perf/compiler.md](https://github.com/thlorenz/v8-perf/blob/master/compiler.md) ·
[sujeet.pro/v8-engine-architecture](https://sujeet.pro/articles/v8-engine-architecture) ·
[thenodebook v8-engine-intro](https://www.thenodebook.com/node-arch/v8-engine-intro) ·
[erdem.pl v-8-function-optimization](https://erdem.pl/2019/08/v-8-function-optimization/) ·
[v8.dev/blog/fast-properties](https://v8.dev/blog/fast-properties) ·
[mrale.ph hidden-classes-vs-jsperf](https://mrale.ph/blog/2013/08/14/hidden-classes-vs-jsperf.html) ·
[Choi et al. PLDI'19](https://iacoma.cs.uiuc.edu/iacoma-papers/pldi19_2.pdf) ·
[ACM 3428242](https://dl.acm.org/doi/pdf/10.1145/3428242) ·
[Chevalier-Boisvert arXiv:1507.02437](https://arxiv.org/pdf/1507.02437)
