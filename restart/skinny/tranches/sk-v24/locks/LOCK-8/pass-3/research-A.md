# LOCK-8 · Family 8A — THE V8 RUNTIME PLANE (shape-stable core, arity-N specializer)

*Research report. parse-that/typescript is READ-ONLY; measurements below are censuses over
committed trees, prototypes are scratch-only. Two engines never conflated: the V8 JIT is the
subject here, not the TS checker (8B). Every consumer claim NAMES the tree HEAD it was taken
from.*

---

## 0. The family in one line

8A is the V8-first formulation of the parse-that TS backbone: every `ParserState` and combinator
object shape-stable and monomorphic **by construction**, an allocation-zero hot path, diagnostics
and packrat as shape-preserving optional planes — with the **arity-N build-time specializer**
(the arity-2/arity-3 unrolls of `any`/`all`/`fuseAll`) as its most contested mechanism. This
report develops that mechanism to its strongest **honest** form and reports the one finding that
governs it: **the live external corpus that justified the arity unrolls no longer exists.**

---

## 1. The mechanism made concrete — the arity-N specializer

The specializer is a **runtime, grammar-construction-time** branch, not compile-time codegen. When
a grammar author calls `all(p0, p1)`, `all(p0, p1, p2)`, or `any(...)`, a dispatch on
`parsers.length` selects a pre-built closure once per combinator instance:

- `any` — arity-2 fully unrolled at `parse-that/typescript/src/parse/leaf.ts:39-57`; general
  indexed `for` at `leaf.ts:58-73`; arity-1 degenerates to `parsers[0].parser` (`leaf.ts:76`).
- `all` → `fuseAll` — arity-2 unrolled at `leaf.ts:183-212`, arity-3 unrolled at `leaf.ts:214-251`,
  general pre-sized `for` at `leaf.ts:255-272`; arity-1 degenerates (`leaf.ts:161`).

Each unrolled arm hoists the element parsers into **named captured constants** (`const p0 =
parsers[0]; const p1 = parsers[1];`, `leaf.ts:40-41,184-185,216-218`) and issues N **positional**
indirect calls `p0.parser(state)` / `p1.parser(state)` (`leaf.ts:44,49,193,201,224,232,240`). The
general arm instead loads `parsers[i].parser` through a loop-variable index (`leaf.ts:62,261`).

**What the unroll actually buys, precisely two things (keep them separate — see §3):**
1. **Structural (cold, tier-independent):** the array-index load (`parsers[i]`) and the loop
   induction/bounds-check vanish; each `.parser` call becomes a distinct **monomorphic** call site
   fed by one captured target, rather than one loop-carried site that observes p0 then p1 then p2
   in succession (polymorphic-over-iterations at a single bytecode offset). The result array is one
   flat pre-sized allocation (`out = [undefined, undefined]`, `leaf.ts:191,222,257`) trimmed by
   `out.length = w` (`leaf.ts:209,248,270`) — never N−1 nested 2-tuples.
2. **Temporal (warm, tier-dependent):** because each site is monomorphic with a constant closure
   target, Maglev/Turbofan **may** inline `p0.parser` bodily into the arm. This is a steady-state
   phenomenon that only manifests after tier-up.

These two legs are the whole subtlety of the family and the reason pass-3 demands they be reported
apart (§3.1).

---

## 2. The governing finding — the live corpus that justified the unroll is GONE

The unrolls were authored against a single named justification: *"the value.js hot shapes — 59
`all()` sites"* and *"the hottest value.js shape (calc/rgb/hsl triples)"* (`leaf.ts:176,214`
comments; the same rationale carried in the extant PT-3 baseline). A fresh per-tree import census
refutes that justification at HEAD:

**value.js — HEAD `86a9a17`, version `4.0.0`, branch `tranche-u` (migration also on `master`).**
- `package.json` carries **no `@mkbabb/parse-that` dependency** (grep of the manifest: zero hits
  outside `@typescript-eslint/parser` / `vue-eslint-parser`).
- The only two `parse-that` tokens in `src/` are **negative-space comments**:
  `src/subpaths/math.ts:2` ("parse-that-FREE") and `src/subpaths/transform.ts:4` ("zero parse-that").
- Parsing has moved to a **hand-written, split-based** grammar: `src/v4/css/grammar.ts` (658 LOC),
  built from `splitTopLevel`/`splitValueTokens`/`numberToken`/`channelToken` string routines
  (`grammar.ts:68,94,133,139`), `parseFunctionalColor` (`grammar.ts:180`), `parseCssColor`
  (`grammar.ts:262`). **Zero** `all`/`any`/`dispatch`/`Parser`/`ParserState` imports anywhere in
  `src/`. Not even a char-cursor (`charCodeAt` count = 0 in grammar.ts) — it is `String.split`-driven.
- The migration is **not a WIP artifact**: commit `470818c` ("parser migration") removed the
  `@mkbabb/parse-that` dependency from `package.json`, is dated **2026-02-25**, and is contained in
  `master` plus every active tranche branch (`tranche-o/p/q/t/u`, `tranche-f-handoff`). value.js
  has been combinator-free on its mainline for ~5 months.
- **Answer to P1/P2:** value.js's parse-that-free posture is **intended and settled**, not
  mid-migration. value.js is **no longer a live consumer of the combinator surface.** (parse-that
  `1.0.0` still resolves in value.js's `node_modules` transitively, never imported by `src`.)

**latex-paper — HEAD `c140473`, version `0.2.1`, dep pinned `^0.7.1` (`<0.8.0`), installed
`0.7.1`.**
- The **only** live external combinator consumer. It imports `Parser regex string any ParserState`
  (grep of `src/grammar/*`, `src/bibtex/parser.ts`). It uses **no `all()`** at all (the three
  `.all()` hits at `src/transform/{sections,labels}.ts` are a `Map#all()` method, not the
  combinator).
- Its `any` sites: `src/grammar/text.ts:145` arity-3 (`any(bracedAccent, unbracedAccent,
  namedAccent)`); `src/grammar/text.ts:154` **dynamic spread** `any(...alts)` (arity =
  `names.length`, unknown at build); `src/grammar/document.ts:81` `inlineNode` **arity ≥ 15**
  (comment/paragraphBreak/4 math variants/emDash/…). None of these hits the arity-2 `any` unroll
  (`leaf.ts:39-57`).
- The pin `^0.7.1` **predates the arity unrolls entirely** — the PT-B3 fusion arms shipped in the
  1.0.0 line. latex-paper's last code change is 2026-07-02; the parse-that pin has not moved off
  `<0.8.0`. It is effectively **frozen** against a pre-unroll engine and would need a 0.7→1.1
  migration to ever exercise the specializer.

**keyframes.js — v6.0.0.** No direct combinator dependency; transitive via value.js — and value.js
now imports nothing, so this transitive path to the combinator surface is dead.

### The arity histogram from the ACTUAL live corpus

| arity of `all` | live external sites | arity of `any` | live external sites |
|---|---|---|---|
| 2 (unrolled) | **0** | 2 (unrolled) | **0** |
| 3 (unrolled) | **0** | 3 (unrolled) | 1 (latex, pinned pre-unroll) |
| ≥4 / dynamic (general) | 0 | ≥4 / dynamic (general) | 2 (latex: ≥15, spread) |

The arity-2 and arity-3 `all()` unrolls — the exact code the specializer exists for — have **zero**
live external callers. The only external `any` traffic is arity ≥ 3 and dynamic, on a pin that
cannot even reach the unrolled code. This is the family's central, uncomfortable result and it
must drive the disposition (§6). It ties directly to 8D's consumer-set adjudication: the
specializer is now a **first-party-only** optimization, justified — if at all — solely by
parse-that's own `test/benchmarks/pt-b3-fusion.bench.ts` + `src/parse/parsers/json.ts`, not by any
external contract.

---

## 3. The three pass-3 8A questions, answered

### 3.1 Does the arity-N specializer actually inline at each arity under Node's default tier?

**Structural leg (cold, the honest number).** Node ≥22 ships **Maglev on by default** (Node 22.0.0;
`nodejs/node#50690`); the default production tier ladder is Ignition → Sparkplug → Maglev →
Turbofan. Independent of any tier, the unroll's structural win holds **from birth**: the captured
`p0`/`p1`/`p2` give V8 monomorphic call sites (one target each) and eliminate the `parsers[i]`
keyed load and loop bounds-check. A monomorphic property access lowers to a map-check + constant
offset load in both Maglev and Turbofan; that is a cold, deterministic saving and is the leg the
family may claim under the cold-per-parse / no-warm-benches law.

**Temporal leg (warm, must NOT be laundered).** Whether Maglev **inlines** `p0.parser` into the arm
is a distinct, tier-dependent event. Maglev performs **limited** inlining (small, hot callees, in
the name of compile time); aggressive inlining is Turbofan's job and only after the closure tiers
up. An "inlines at each arity" claim measured at Turbofan steady state is a **warm** claim —
disinformation under the standing law. The pass-3 demand to report the two legs *separately* exists
precisely to stop a warm inlining win from being reported as a cold structural one.

**What the prototype must show (scratch, HEAD-named):** on a scratch parse-that copy at a named
HEAD, run per arity (2, 3, general-4) under
`node --allow-natives-syntax --trace-maglev-inlining --trace-turbo-inlining` a harness that (a)
prints `%GetOptimizationStatus` of each arm to prove which tier it reached, and (b) reports the
**cold per-op** structural delta (unroll vs general at matched arity) *separately* from the
**warm/steady** delta after forced tier-up (`%OptimizeFunctionOnNextCall`). The honest table has
two columns; the temporal column is disclosed, never summed into the cold one. Anticipated result:
the structural leg is a small positive at every arity; the inlining leg fires for arity-2/3 under
Turbofan and is warm-only.

### 3.2 What arity distribution does the live corpus exhibit, and does it justify specialization?

Answered in full at §2: the external distribution is **empty at the unrolled arities.** The
specializer is **not justified by the live external corpus.** It survives only as a first-party
optimization whose sole surviving justification is parse-that's own bench/JSON grammar — which
routes this to 8D (prune vs keep) and to 8C (is the first-party suite a legitimate consumer, Y.13).
The disposition (§6): keep the general indexed-`for` arm (it is the correct, allocation-lean core
and serves latex's real arity ≥3 / dynamic traffic); **demote the arity-2/3 unrolls to a
first-party-bench-gated micro-optimization**, not one defended on a consumer contract that no longer
exists.

### 3.3 Relationship to the build-time TS emitter face (XL-D). One mechanism? Constructible on TS side?

**They are NOT one mechanism today.** The arity specializer is a *hand-written runtime branch*
inside the parse-that library (`leaf.ts` `if (n===2/3)`), selecting a closure per combinator
instance at grammar-construction time. The fast TS execution face — the build-time TS **emitter** —
is a bbnf-side code generator whose current, measured TS output is a **standalone monomorphic tape
decoder that imports no combinators** (its oracle is `JSON.parse`). The emitter emits tape decode;
the specializer specializes combinator sequencing; they share neither code nor codepath.

**Constructible on the TS side — yes, but only as a different family's build.** The unification
route is a build-time **fuser** that emits the specialized arity-N sequencing chains *as generated
TS* (`const p0=…; return state => { …p0.parser(state)… }`), one emitted body per grammar rule — the
arity unroll generalized to a generator product with no runtime `if (n===...)` at all. That collapses
the leaf.ts branch into emitter output and makes the two ONE mechanism. It does **not** exist today
and is properly the province of the combinator-compilation family (a build-time fuser), not of 8A's
runtime shape discipline. 8A's own scope ends at: the runtime specializer is a *separate* mechanism
from the emitter; it is subsumable by a build-time fuser if one is built; and the §2 histogram says
the payoff of building that fuser for the unrolled arities is presently near-zero on live traffic.

---

## 4. The substrate — the shape-stable core beyond the arity question

The specializer is one plane of 8A; the durable substrate is the shape discipline that must be
*proven*, not asserted:

- **`ParserState` hidden-class stability** (`state.ts:36-53`): 8 fields; `value:T` holds
  string|array|object|undefined across every parse; `expected?`, and — after the lazy-alloc lever —
  `suggestions?`/`secondarySpans?` are declared native fields (`undefined` at birth) so the map is
  fixed from construction and never transitions. The eager-alloc census (6 `= []` sites → 0) is the
  proof the lever removed per-parse and per-backtrack array allocations; the fused alloc drop
  (−60.1% B/op, exact) corroborates. This is the allocation-zero hot path made concrete and is the
  substrate the arity unrolls sit on.
- **The megamorphic choke point** (`state.ts:87-99` `unsafeSetValue`/`unsafeCall`/`unsafeCallRaw`;
  every combinator's `this.parser(state)` / `p.parser(state)` indirect call — `parser.ts:53,82,88,
  131,137`, `leaf.ts:44,49,62,140`). This is THE megamorphic IC of the engine: many closure shapes
  flow through one indirect call. The arity unroll is one *local* de-megamorphization (it splits a
  loop-carried site into N monomorphic sites); the family's stronger substrate question is whether
  the global `.parser` indirection can be shape-narrowed at all without a graph/VM reformulation
  (that reformulation is a sibling family's concern; 8A's contribution is to *measure* the IC state,
  not to rebuild the engine).
- **The one deopt-risk object** (`parser.ts:177-180`): `mapState` builds `Object.create(state)` per
  invocation — a prototype-chain view whose property access is slower and can deopt callers. 8A's
  shape oracle must assert this is the **only** prototype-chain object on the hot path.

---

## 5. Prior art (cited)

- **V8 tiering & Maglev default-on:** Node 22 ships Maglev by default; ladder Ignition → Sparkplug →
  Maglev → Turbofan. Maglev is a mid-tier SSA-lite compiler that consumes the FeedbackVector and
  does **limited** inlining for compile-time economy, deferring aggressive inlining to Turbofan.
  ([nodejs/node#50690](https://github.com/nodejs/node/issues/50690);
  [V8 features in Node 22](https://medium.com/@backendbyeli/leveraging-v8s-hidden-gems-in-node-js-2746810d84d9);
  [thenodebook V8 tiers/shapes/deopt](https://www.thenodebook.com/node-arch/v8-engine-intro))
- **Monomorphic call & IC lowering:** monomorphic calls are recorded during unoptimized execution
  then inlined in optimized code; a property access with monomorphic feedback becomes a map-check +
  constant-offset load — the exact structural win the captured `p0`/`p1`/`p2` unlock.
  ([V8 engine architecture](https://sujeet.pro/articles/browser-runtime-internals/js-runtime-internals/v8-engine-architecture))
- **Inlining heuristics (why arity/size gates the temporal leg):** inliners bound by callee size and
  call-site hotness; a survey of the heuristics that decide whether a small monomorphic callee is
  folded. ([Max Bernstein, inlining heuristics](https://bernsteinbear.com/blog/inlining-heuristics/))
- **Speculative inlining + deopt as the temporal-leg cost model** (the same speculate-then-deopt
  discipline the arity sites live under):
  ([V8 wasm speculative optimizations](https://v8.dev/blog/wasm-speculative-optimizations))
- **`--allow-natives-syntax` / `%HaveSameMap` / `%GetOptimizationStatus`** are the standard V8
  intrinsics for a hidden-class + tier oracle; historical baselines and the natives-syntax method
  in [davidmarkclements/v8-perf](https://github.com/davidmarkclements/v8-perf).

---

## 6. Named risks & counterexamples

- **R-A1 (the family's live wire): the arity unroll is a no-consumer perf seam.** *Counterexample:*
  value.js — the sole source of the "59 `all()` sites" justification — left the combinator surface
  on its mainline (commit `470818c`, 2026-02-25); the arity-2/3 unrolls have **zero** live external
  callers. Shipping/defending them on a consumer contract is exactly the no-consumer-perf-seam
  pattern the earlier `dispatch` subTable retraction (`leaf.ts:89-98`) was killed for. Disposition:
  keep the general arm; demote the unrolls to a first-party bench-gated micro-opt, or prune (8D).
- **R-A2: laundering the temporal inlining leg as a cold win.** *Counterexample:* a landing session
  measures the arity-2 arm at Turbofan steady state, reads a large delta, and reports it as the
  cold per-parse number. That is a warm claim under the standing law. Cure: the two-column oracle of
  §3.1 — structural (cold) and temporal (warm) reported apart, HEAD named.
- **R-A3: the R4 consumer table is stale and load-bearing downstream.** *Counterexample:* the extant
  PT-3 W0/W5 gates assume value.js consumes parse-that (3.1.0, `^1.0.0`, 59 `all()` sites) and that
  keyframes.js reaches it transitively through value.js. At HEAD value.js is 4.0.0, imports nothing,
  and the transitive keyframes path is therefore dead. A 1.1.0 "value.js consumer suite green" gate
  is measuring a consumer that no longer links the code under test. **Spec re-cut owed:** the only
  live combinator gate is latex-paper (pinned pre-unroll), and it needs a 0.7→1.1 migration to be a
  gate at all. (Spec-fidelity divergence per the plane-wide directive.)
- **R-A4: dynamic/large arity is the real external shape.** *Counterexample:* latex's `any(...alts)`
  and arity-≥15 `inlineNode` are the actual external `any` traffic; a specializer that only unrolls
  arity-2/3 does nothing for them and the general indexed-`for` arm is what they exercise. The
  correct optimization target for the live corpus is the general arm's steady-state (a
  first-char-dispatch or prefilter routing), not more unrolled arities.
- **R-A5: over-reading `%HaveSameMap` as proof of no churn.** *Counterexample:* same-map TRUE for a
  diagnostics-off vs diagnostics-populated `ParserState` proves the *lazy field* claim, but the
  `mapState` `Object.create` view (`parser.ts:177-180`) is a separate prototype-chain object the
  same-map probe won't catch; the oracle must assert it is the *only* such object, not merely that
  the base state is stable.

---

## 7. Exactly what a prototype must test to move 8A

1. **The two-leg arity oracle (the decisive test).** Scratch parse-that at a NAMED HEAD, Node ≥22
   default tier. For arity ∈ {2, 3, 4-general}: (a) `--trace-maglev-inlining --trace-turbo-inlining`
   + `%GetOptimizationStatus` to record which tier each arm reaches and whether `p_i.parser` inlines;
   (b) **cold** per-op paired median (unroll arm vs general arm, matched arity, single build per
   process, interleaved A,B,A,B…, N≥12, loadavg per round) as the **structural** column; (c)
   **warm/steady** per-op after `%OptimizeFunctionOnNextCall` as the disclosed **temporal** column.
   Interpretation: report the two columns apart; the family's claimable number is the structural
   one. This test decides whether the unroll survives on its own cold merit absent any consumer.
2. **The shape oracle.** `node --allow-natives-syntax`: `%HaveSameMap` TRUE for diagnostics-off vs
   diagnostics-populated `ParserState`; assert `mapState`'s `Object.create` view is the ONLY
   prototype-chain object on the hot path; assert no unrolled arm introduces a new hidden class.
3. **The live-corpus re-census as a committed artifact.** The §2 histogram, machine-generated (grep
   census over value.js@`86a9a17`, latex-paper@`c140473`, keyframes.js@6.0.0), checked in so the
   spec re-cut (R-A3) has a falsifiable before-state — and re-run if the owner confirms a value.js
   or latex-paper migration is forthcoming (P1/P2 owner-gated, non-blocking).
4. **The general-arm target test (the real external shape).** Prototype a first-char/dispatch routing
   over latex's arity-≥15 `inlineNode` `any` and measure cold per-op vs the sequential general arm —
   this is where the *live* corpus's optimization headroom actually sits, not in more unrolled arities.

---

## 8. Disposition summary (for the registry, 8A alone)

- The shape-stable core (lazy `ParserState` fields, fixed-from-birth hidden class, allocation-zero
  default path) is **sound and measured** — the durable substrate; keep and pin with the shape
  oracle.
- The arity-N specializer's **structural** leg is a legitimate cold win; its **temporal** (inlining)
  leg is warm-only and must be reported apart under the standing law.
- The specializer's **consumer justification has evaporated**: value.js migrated off combinators on
  its mainline (2026-02-25), latex-paper is pinned pre-unroll and uses only arity-≥3/dynamic `any`.
  The unrolled arity-2/3 arms are a **first-party-only** micro-optimization; the honest disposition
  is bench-gate-or-prune (routed to 8D), keep the general indexed-`for` arm as the real core.
- The specializer is **not** one mechanism with the build-time TS emitter today; it is subsumable by
  a build-time fuser (a sibling family's build), whose payoff for the unrolled arities is near-zero
  on live traffic per the §2 histogram.
