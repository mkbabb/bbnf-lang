# LOCK-8 pass-3 — family 8A: THE SHAPE-STABLE V8 RUNTIME CORE

The V8-first formulation of the parse-that TS combinator core: every `ParserState` and
combinator object shape-stable and monomorphic by construction, an allocation-zero hot path,
diagnostics and packrat as shape-preserving optional planes — proven by IC/inline-caching
probes and paired cold harnesses, not asserted. This family owns the *runtime execution face*
of the backbone (V8 JIT), never conflated with the TS *checker* face (8B) nor the build-time
emitter (XL-D). Measurements below name their HEAD: parse-that `typescript/` @ `1.0.0`
(published tag `v1.0.0`, dist built at that cut); probes run on **Node v26.0.0 / V8**,
Apple Silicon, against the committed `typescript/dist/parse.js`.

---

## 1. The mechanism made concrete — three shape-stable pillars

**(P1) Fixed-from-birth hidden class.** `ParserState<T>` (`state.ts:36-53`) declares its fields
positionally in the constructor (`src, value, offset, isError, furthest`) plus three
diagnostic fields. The governing V8 surface (R4 §6a) is that `value:T` holds
string|array|object|undefined across every parse through the single write choke point
`unsafeSetValue` (`state.ts:87-89`) — the sole megamorphic-write site by design. PROVE-11
(GO/CONFIRMED_DELTA) made `suggestions`/`secondarySpans` **symmetric with the pre-existing lazy
`expected?`** (`state.ts:43`): declared native fields, `undefined` at construction so the map
is fixed from birth and never transitions, populated only under `diagnosticsEnabled` via
`(x ??= []).push`. Emitted-JS census (both prover and adversary, exact): eager `= []` alloc
sites **6 → 0**; fused alloc **1.84 → 0.73 B/op (−60%)**. This pillar is the *substrate* of 8A —
landed-grade, not proof-grade, and PT-3.W1 productionizes it.

**(P2) The megamorphic dispatch spine is irreducible and must be planned-around, not wished
away.** Every combinator threads through the `.parser` function-pointer field —
`this.parser(state)` / `p.parser(state)` (parser.ts:53,82,88,131,137; leaf.ts:44,49,62,140).
This is THE megamorphic IC of the engine (R4 §6c): V8 sees many distinct closure shapes flow
through one indirect call. 8A's design center is to keep every *object* monomorphic so the only
polymorphism V8 confronts is this deliberate function-pointer indirection — then reduce what
rides across it (alloc, tuple materialization) rather than pretend to monomorphize it.

**(P3) Construction-time specialization, not per-parse.** Combinator closures allocate once per
`new Parser(...)` at grammar-build (R4 §6e); `createLazyCached` uses a closure-local cache to
avoid function-object-mutation IC pollution (lazy.ts:19-25). The arity unrolls
(`any` arity-2 leaf.ts:39-57; `fuseAll` arity-2 leaf.ts:183-212, arity-3 leaf.ts:214-251) run
their `if (n===2/3)` branch **once at construction**, yielding one closure of one arity per
call site — so V8 sees a *monomorphic* body with constant-folded `p0/p1/p2` bindings and no
array-index load in the hot path. `dispatch` builds its `Int8Array(128)` LUT once
(leaf.ts:100-151); `string` len-1 charCode (leaf.ts:282-294) and `regex` sticky-`test()`
zero-alloc (leaf.ts:344-358) are the same construction-time-branch discipline.

---

## 2. The three 8A directive questions — answered with fresh trace evidence

### 2A. Does the arity-N specializer actually INLINE at each arity? — **NO (structural); YES win (temporal); reported separately.**

Direct `--trace-maglev-inlining` on `dist/parse.js`, one hot arity per process (400k parses,
Node v26 default production tier), naming the fused closure by bytecode size:

| arity | fuseAll closure size | maglev verdict (max-size 100) |
|---|--:|---|
| `all(a,b)` unrolled | **169** | `cannot inline … big function, size (169) >= max-size (100)` |
| `all(a,b,c)` unrolled | **234** | `cannot inline … big function, size (234) >= max-size (100)` |
| general arm (arity-4) | larger still | not inlined |
| `parseStateInner` (driver) | **162** | `cannot inline … size (162) >= max-size (100)` — at EVERY arity |

**Structural leg (refuted):** the arity-unrolled `fuseAll` closures are **not inlined into the
parse driver at any arity** — they overflow V8 maglev's 100-bytecode inline budget (169 at
arity-2, 234 at arity-3; the per-arm error/backtrack/`out[w++]` blocks bloat the body). The
top-level `parseStateInner` (162) is likewise never inlined into `parse`. So the arity unroll
does **not** buy call-site inlining. What it *does* buy — the true, defensible rationale — is
(a) closure-body monomorphism (constant-folded positional bindings, no array load) and
(b) elimination of the intermediate 2-tuple cascade that unfused `a.then(b).then(c)` allocates
(N−1 nested tuples → one flat array; leaf.ts:176 comment). Small leaves *do* inline
(`stringParser`, `unsafeSetValue`, `ok`, `ParserState` ctor all `== Inlining`), so the
leaf/state layer is inline-clean; the *sequencing* layer is an inline boundary by construction.

**Temporal leg (confirmed):** despite non-inlining, the cold-paired win is real and reproduces
adversarially — fused `all(a,b,c)` **+10.1%** (prover) / **+15.6%** (VERIFY), unfused then-chain
**+13.2/+13.6%**, alloc **−60%** — because the win is *allocation elimination*, not inlining.
The two legs must be carried separately (directive): "arity unroll → inlining" is FALSE; "arity
unroll → monomorphic, alloc-free sequencing → measured cold win" is TRUE. Any PT-3/W4 claim
attributing the speed to inlining is self-deception; the honest mechanism is the alloc census.

### 2B. Arity histogram from the ACTUAL live corpus — **the arity-2/3 `all()` unroll has ZERO live external consumers today.**

Re-run per-tree census (HEAD-named, 2026-07-16):

| consumer | version / branch | parse-that dep | combinator usage found |
|---|---|---|---|
| **value.js** | **4.0.0** (branch `tranche-u`) | **`dependencies:{}`, `peerDeps:{}`, 0 lockfile hits** | **NONE** — `all/any/dispatch/sepBy/many/chain` = 0; only comments declaring modules "parse-that-FREE" (`src/subpaths/math.ts:2`, `transform.ts:4`) |
| **keyframes.js** | 6.0.0 | none (no package.json dep) | NONE — self-declares "parse-that-FREE, 0 parse-that modules" (`src/animation/internal/leaves.ts:9-10`); the `Promise.all` hits are native |
| **latex-paper** | 0.2.1 (pins `^0.7.1`, installed **0.7.1**) | `^0.7.1` (pre-1.0.0) | **only external combinator consumer**: `any()` at **arity-3** (`src/grammar/text.ts:145`) + **variadic** `any(...alts)` (text.ts:154) + a multiline `any(` (document.ts:81); imports `Parser regex string any ParserState` across 8 grammar files; **`all()` = 0** |

**This overturns R4's premise.** R4 §7 recorded value.js @ **3.1.0** with **59 `all()` sites** and
"hot 3-tuples" — the exact justification stamped into the code (leaf.ts:214 "the hottest value.js
shape (calc/rgb/hsl triples)"). That corpus **no longer exists**: value.js divorced parse-that
entirely across 3.1.0→4.0.0 (`dependencies:{}`, not a version bump). This is XL-G P1 surfaced
independently (intended vs `tranche-u` WIP is the owner question — but on-tree it is a total
divorce). Consequence under Constitution viii (consumer-less substrate forbidden, Y.13): the
arity-2/arity-3 `fuseAll` unroll is presently justified ONLY by the first-party
`test/benchmarks/pt-b3-fusion.bench.ts` floor (R4 §4). The live *external* arity histogram is:
`any` arity-3 + variadic (both hit the **general for-arm**, NOT the arity-2 unroll), `all` empty.
The specialization survives on first-party-consumer grounds (a legitimate disposition — 8D) or
must be re-anchored to latex-paper's real `any()` shapes; it cannot be re-justified on value.js's
dead 59-tuple corpus. Note latex-paper pins 0.7.1 (pre-1.0.0) so it does not even see today's
`fuseAll` unroll — the arity-2/3 `all()` unroll runs against **no external code at all**.

### 2C. Relationship to the build-time TS emitter (XL-D / 4B-TS) — **distinct mechanisms; a combinator→JS emitter is constructible but NEW, and is the only route dissolving the 100-byte inline ceiling.**

XL-D is RATIFIED (CROSS-LOCK.md:27-30, LOCK-4 pass-2, measured): runtime decoders (4C/4D
switch-loops, 4A graph-interpreter) are REFUTED as the fast TS face (−22%, ties naive); the fast
face is the **build-time emitter (4B-TS)**. Cross-lock read of the emitter's state (PT-3.md §2):
the GN-4.W4 tape decoder seeding the TS backend is a **monomorphic tape decoder that imports no
combinators** (oracle `JSON.parse`) — grammar/tape → JS, a *code generator*. The arity-N
`fuseAll` selector is a **runtime closure picker** (a construction-time `if`), *not* a code
generator; they are **different mechanisms**. Scratch-construction verdict: a build-time
combinator→JS fuser (LOCK-4B-TS's "build-time fuser emits specialized chains") IS constructible
on the TS side — emit `all(a,b,c)` as straight-line offset-threading at each static-arity call
site — and it is the **only** route that can push a fused sequence body under the 100-bytecode
maglev budget (minimal per-site straight-line code with shared early-returns rather than one
234-byte general closure), converting §2A's non-inlining boundary into an inlined one. But it is
a NEW subsystem gated on: (i) a static-arity call-site discovery pass, (ii) the build tooling
XL-D's emitter already uses, (iii) proof that emitted per-site bodies land <100 bytecode. Absent
that, the existing runtime unroll's non-inlining ceiling is structural and permanent.

---

## 3. Substrate — integration seams (path:line)

- Shape core: `state.ts:36-53` (fields), `state.ts:87-89` (`unsafeSetValue` write choke),
  `state.ts:55-73` (`ok/err/from` funnel), `state.ts:44-45` (the PROVE-11 lazy fields).
- Dispatch spine: `parser.ts:24-32` (`Parser` shape), `parser.ts:34-49/51-75` (`parseState`/
  `parseStateInner` — the size-162 non-inlinable driver), `parser.ts:53,82,88` (megamorphic IC).
- Arity specializers: `leaf.ts:28-79` (`any` unroll+general), `leaf.ts:154-273`
  (`all`→`fuseAll` unroll+general), `leaf.ts:100-151` (`dispatch` LUT).
- Deopt watch: `mapState` builds `Object.create(state)` per invocation (parser.ts:177-180) — the
  one prototype-chain object; W4's shape oracle must confirm it is the *only* one and that its
  callers are not deopted.
- Optional planes (must be shape-preserving): diagnostics global latch `utils.ts:6` (PT-E1 makes
  per-parse-scoped); packrat `PACKRAT_ARMED` no-op until first `memoize()` (packrat.ts:156,290).
- Non-regress bench floor: `test/benchmarks/pt-b3-fusion.bench.ts`; certified method is
  single-build-per-process cold paired interleaving (PROVE-11 `run-paired.sh`), NOT `vitest bench`
  (warm, cross-run, load-confounded — refused as evidence, PT-3 §1.3).

---

## 4. Prior art (citations)

- **V8 maglev/TurboFan inlining budget**: the observed `max-size (100)` bytecode ceiling and
  `max_inline_depth (1)` are V8 maglev heuristics (`v8/src/maglev/maglev-inlining.*`,
  `--max-maglev-inlined-bytecode-size*` flags); TurboFan gates on call-frequency ≥ threshold +
  size budget. Confirms inlining is size-gated, not shape-gated — matches §2A.
- **Hidden classes / IC monomorphism**: V8 "maps"/transition trees; fixed-from-birth field
  declaration is the canonical shape-stability idiom (V8 team "Fast properties in V8"; mrale.ph
  "What's up with monomorphism"). PROVE-11's `%HaveSameMap` oracle (PT-3.W4) is the direct probe.
- **Combinator fusion by staged code-gen**: the LOCK-4B route mirrors staged/partial-evaluation
  parser generators — Jonnalagedda et al., "Staged Parser Combinators for Efficient Data
  Processing" (OOPSLA'14); the Rust `nom`/`combine` monomorphized-closure models; the build-time
  emitter is the TS analogue of Rust's comptime monomorphization (XL-E).
- **Packrat/arena discipline**: Ford's packrat (packrat.ts opt-in WDM); the alloc-zero hot-path
  goal parallels tree-sitter's reusable-node model and regex-automata's `Cache` amortization.

---

## 5. Named risks & counterexamples

- **R-A1 — the inlining fallacy.** *Counterexample:* a W4 author reads the fused +15% and writes
  "the arity unroll lets V8 inline the sequence." §2A's trace refutes this at every arity (169/234
  > 100). If re-attributed to inlining, a maglev flag bump would be the "fix" — and do nothing,
  because the lever is alloc elimination. Carry structural (no-inline) and temporal (alloc-win)
  legs separately or the family reasons on a false model.
- **R-A2 — dead-corpus specialization.** *Counterexample:* PT-3/W4 preserves the arity-2/3 unroll
  citing "value.js's 59 all() sites." Those sites are gone (value.js 4.0.0 `deps:{}`). Keeping the
  unroll is defensible only as a first-party-bench consumer (8D adjudication) or re-anchored to
  latex-paper's `any()` arity-3/variadic — never on the stale corpus. Constitution viii bites.
- **R-A3 — the `mapState` prototype view deopts callers.** *Counterexample:* `Object.create(state)`
  (parser.ts:177-180) is a slow prototype-chain object; a hot grammar routing through `mapState`
  can deopt the caller. W4's shape oracle must probe callers, not just the object.
- **R-A4 — measuring the wrong face.** *Counterexample:* benchmarking the 4C/4D runtime switch-loop
  as "the TS core." XL-D refuted it (−22%); it survives only as an isomorphism oracle. 8A measures
  the closure runtime; 8C/XL-D own the emitter.
- **R-A5 — load-confounded win.** *Counterexample:* the naive `pt-b3` cross-run (PROVE-11 §3.1:
  pristine loadavg 12, modified loadavg 27) fabricates or erases the delta. Only single-process
  paired interleaving certifies.

---

## 6. What a prototype must test to move 8A

1. **Per-arity inline verdict, base-pinned (XL-H).** Re-run `--trace-maglev-inlining` at arities
   1..6 on a fresh `1.0.0` scratch build, HEAD named; confirm the 169/234-byte non-inline boundary
   and record the arity at which the general arm crosses. Structural leg.
2. **The build-time-emitter counterfactual.** Emit straight-line `all(a,b,c)` per static-arity call
   site (scratch, LOCK-4B-TS shape); measure emitted-body bytecode size and re-trace — does the
   per-site body land <100 and INLINE where the runtime unroll cannot? Cold-paired emitted vs
   runtime-unroll closure. The one experiment that could dissolve §2A's ceiling.
3. **The re-anchored arity histogram → floor.** Rebuild the specialization's non-regression floor
   on latex-paper's *actual* `any()` shapes (arity-3 + variadic) + the first-party bench, since
   value.js's `all()` corpus is dead. Decide keep-vs-collapse the arity-2/3 unroll (feeds 8D).
4. **Shape oracle (`%HaveSameMap`).** Two `ParserState`s — one diagnostics-off, one
   `suggestions`-populated — same map TRUE (fixed-from-birth); assert `mapState` is the only
   prototype-chain object and its callers are not deopted. Under `node --allow-natives-syntax`.
5. **Alloc-zero certification on the default path.** `--expose-gc` heapUsed-delta B/op on the
   fused/dispatch lanes with packrat unarmed and diagnostics off; confirm the −60% alloc holds and
   no per-parse allocation slips in (the true source of the temporal win).
