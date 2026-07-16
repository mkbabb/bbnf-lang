# LOCK-8 · FAMILY 8A — THE SHAPE-STABLE CORE (research, pass 2)

*The V8-first formulation of the parse-that TS combinator core: every state and combinator
object shape-stable and monomorphic by construction, an allocation-zero hot path, diagnostics
and packrat as shape-preserving optional planes. Numbers herein are cold, paired, and
disclose armed/unarmed packrat; the V8 JIT and the TS checker are never conflated. Corpus of
record: value.js at HEAD (tag `tranche-t-close`, commit `86a9a17`), latex-paper `0.2.1`
(`^0.7.1` pin), keyframes.js `6.0.0`.*

---

## 0. The load-bearing discovery: `.mapState` has zero consumers

The pass-2 charter opens by demanding a MECHANICAL enumeration of every field and prototype
method that real `.mapState` consumers read. The mechanical answer, verified by grep across
all three consumer trees and the parse-that core itself, is that **no such consumer exists**:

- `rg mapState` over `value.js/src` at HEAD (`git grep mapState HEAD -- src`) → **0 hits**.
- `rg mapState` over `latex-paper/src` → **0 hits**.
- `rg mapState` over `keyframes.js/src` → **0 hits** (keyframes consumes parse-that only
  transitively through value.js).
- `rg mapState` over `parse-that/typescript/src/parse` → the ONLY hits are the definition
  (`parser.ts:162-187`), its `parserNames` registry entry (`state.ts:167`), and
  `createParserContext("mapState", …)`. **No combinator builds on `mapState` internally**;
  `trim`, `wrap`, `skip`, `next` are all hand-inlined (`parser.ts:189-521`), none routes
  through it.

`mapState` is the sole site in the entire core that constructs a prototype-chain object:
`const oldView = Object.create(state)` (`parser.ts:177`), flagged in the recon as the
combinator-core deopt risk. The finding is that this risk is a **phantom on the real corpus**
— every field and method a hypothetical consumer could read (`.value`, `.offset`, `.src`,
`.isError`, `.furthest`, `.getLineAndColumn`, `.getColumnNumber`, `.toString`) is inherited
through the prototype chain, but nothing reads them because nothing calls `mapState`. It
survives only because `proof:no-dead-combinator` requires a test exercise; it is a
public-API combinator kept for completeness, not a production hot shape.

**Resolution (family 8A's answer, both questions).** Because no consumer exercises the
`Object.create` path, the fix is zero-risk and can be made unconditionally: replace the
prototype-chain view with a **fixed-shape struct view** carrying the same fields in the same
declaration order as `ParserState`, so V8 assigns it a stable, independent hidden class rather
than a slow prototype-walk object. The view need not be a full clone — the two mutated fields
are `offset`/`value` (`parser.ts:178-179`), so a five-field literal
`{ src, value: oldValue, offset: oldOffset, isError, furthest }` suffices for read purposes.

**The typed boundary (no `as unknown as`).** Supply one shared read-surface interface that
both the full `ParserState<T>` and the view satisfy structurally:

```ts
interface ReadonlyState<T> {
    readonly src: string;
    readonly value: T;
    readonly offset: number;
    readonly isError: boolean;
    readonly furthest: number;
}
```

`ParserState<T>` (`state.ts:36-53`) already declares every one of these as a public field, so
`class ParserState<T> implements ReadonlyState<T>` type-checks with no change to runtime shape
and no cast. `mapState`'s callback is retyped `fn(newState: ParserState<T>, oldState:
ReadonlyState<T>)`; the view is constructed as a `ReadonlyState<T>` literal — no `Object.create`,
no `as unknown as ParserState<S>`. This simultaneously answers "collapse the hidden-class
divergence WITHOUT dropping any field" (no field is dropped — the interface enumerates the
complete read surface) and "type the boundary with no any-grade cast" (the interface is the
precise shared supertype). The convergence lever is that there is nothing to preserve: the
enumeration is empty, so a fixed-shape view is a pure win gated only by the `mapState` unit
test, not by any consumer suite.

---

## 1. The real consumer surface (mechanical census, HEAD-committed)

The v1.0.0-floor premise is stale twice over and must be re-pinned:

| consumer | state of record | parse-that surface actually imported | `all()` | `any()` | `dispatch()` | `mapState` |
|---|---|---|--:|--:|--:|--:|
| **value.js** | HEAD `tranche-t-close` (`86a9a17`) | `Parser ParserState all any dispatch regex string whitespace mergeErrorState` | **61** | **93** | **6** | **0** |
| **latex-paper** | `0.2.1`, `^0.7.1` pin | `Parser ParserState regex string any` | 0 | ~3 | 0 | 0 |
| **keyframes.js** | `6.0.0`, transitive via value.js | (none direct) | — | — | — | — |

Counts are `git grep` / `rg` over `src` at the state of record; `any()` at 93 sites is now
the **dominant** fusion surface, exceeding `all()` (61) — the arity-unroll investment weighted
toward `all()` (`fuseAll`, `leaf.ts:179-273`) is mis-proportioned against `any()`
(`leaf.ts:28-79`), which received only an arity-2 unroll.

**Named risk R-8A-1 (the eroding consumer).** value.js's working tree (branch `tranche-u`,
version `4.0.0`, uncommitted) is mid-refactor to a `src/v4/css/grammar.ts` that hand-rolls
parsing over its own `Result`/`ok`/`err` monad and **imports no combinators** — `rg
parse-that value.js/src` in the working tree returns only two "parse-that-FREE" comments. The
61 `all()` / 93 `any()` sites exist ONLY at HEAD. If the v4 cutover lands, the principal
justification for the arity-N fusion specializer collapses to latex-paper (≈3 `any`, 0 `all`)
plus keyframes (transitive). A prototype MUST pin this trajectory before the specializer is
scoped: the fusion work is justified by a consumer that is actively removing itself. This is
the single largest strategic hazard to family 8A and is invisible to a HEAD-only census.

---

## 2. The SFI-heterogeneity predicate (the NEW question, 8A-homed)

**Mechanism.** The general fused arm threads by array index:
`parsers[i].parser(state)` (`leaf.ts:62`, `leaf.ts:261`). This is a single call site whose
callee is loaded from an array element. V8 keys its call inline cache on the callee's
**SharedFunctionInfo** — the syntactic function literal that produced the closure. Every
`string()` closure shares one SFI (`stringParser`, `leaf.ts:284`/`296`), every `regex()`
closure shares `regexParser` (`leaf.ts:326`), every `.map` shares the `map` literal
(`parser.ts:147`), and so on. Therefore:

- A site is **SFI-homogeneous** iff all its argument parsers were built by ONE factory
  (`all(string(","), string(";"))`): the loop's call IC sees one SFI, stays monomorphic, and
  TurboFan/Maglev inline it — **unrolling wins nothing** (it only emits dead bytes).
- A site is **SFI-heterogeneous** iff the arguments mix factories: two-to-four distinct SFIs
  make the IC polymorphic, five-plus make it **megamorphic** (IC gives up, every call is an
  indirect dispatch). Unrolling into positional call sites `p0.parser(state); p1.parser(state)`
  gives each POSITION its own monomorphic IC slot — the win is real and grows with arity.

**The build-time rule (deliverable).** Classify each argument by its constructing factory and
compute `d = |distinct SFIs|`:

```
unroll(site)  ⟺  d ≥ 2   ∧   arity ≤ CEILING
```

`d == 1` ⇒ homogeneous ⇒ skip the unroll (the general loop is already monomorphic). `d ≥ 2`
⇒ heterogeneous ⇒ emit positional closures. `CEILING` is set by the code-size budget (§3).

**Two ways to compute `d`, both available.** (a) **Runtime, at construction:** every `Parser`
already carries `context.name` — the factory tag set by `createParserContext`
(`state.ts:173-189`; the registry `parserNames` at `state.ts:141-171`). So
`new Set(parsers.map(p => p.context.name)).size` is `d` exactly, computable inside `all()`/
`any()` with no AST. This lets the choice be a construction-time branch even without a
build-time codegen. (b) **Build-time, from the grammar AST:** the LOCK-4B fuser reads the
`all(...)`/`any(...)` call arguments and classifies factories statically, emitting the unrolled
closure only for heterogeneous sites — no runtime branch at all.

**Soundness and the false-positive/negative cost.** `context.name` (and the AST factory) is a
FAITHFUL SFI proxy: same factory ⇒ same code object ⇒ the call IC is monomorphic regardless of
captured data. `regex(/a/)` and `regex(/b/)` differ only in closure context, share
`regexParser`'s SFI, and the IC treats them as one — so the predicate has **no false negatives**
(it never calls a truly-megamorphic site homogeneous). A **false positive** — predicting
heterogeneous where V8 would have kept the loop monomorphic — can occur only if two distinct
factories emit closures V8 merges; this does not happen for the parse-that factories (each is a
distinct literal) and its cost is bounded to a wasted positional unroll (extra emitted bytes,
zero perf regression, since the positional form is never slower than the loop for `d ≥ 2`).

**Corpus evidence the predicate has bite.** value.js's real `all()` arguments are almost never
homogeneous: e.g. `all(colorValue.skip(alphaSep), colorValue)`, `all(colorValue, sep,
colorValue, div, colorValue)` — arguments are `.skip`/`.map`/`.trim`/lazy results, each a
distinct SFI (`color.ts:147,151,169,305,314,320,446`). The arity-2/3 unroll (`leaf.ts:183-251`)
catches the short heterogeneous sites; the higher-arity heterogeneous sites (arity ≥ 4) fall to
the megamorphic general loop today. That gap — heterogeneous arity ≥ 4 — is exactly what the
arity-N specializer must cover, and it is where value.js's `all()` mass sits.

---

## 3. The arity-N build-time specializer (shared with LOCK-4B)

**Mechanism.** Generalize the hand-written arity-2 (`leaf.ts:183-212`) and arity-3
(`leaf.ts:214-251`) unrolls to a build step that emits a positional fused closure for any arity
`N` up to `CEILING`, for the heterogeneous sites the §2 predicate selects. Each emitted arm is
the arity-3 body with `N` positional trials — `~8` lines × `N`. The generator is the LOCK-4B
fuser; **coordinate the single TS-execution number with LOCK-4B, do not duplicate the bench**.
The predicate of §2 is the gate that decides which sites the fuser touches, so the two are one
mechanism: LOCK-4B owns the emitter, 8A owns the shape/heterogeneity rule and the V8 evidence.

**TurboFan AND Maglev (Node 26's default tier).** Positional call sites are monomorphic ICs
that BOTH tiers inline; the general-loop indirect call is the megamorphic site neither can
inline once `d ≥ 5`. The prototype must confirm inlining under both tiers explicitly —
`--trace-turbo-inlining` and `--trace-maglev-inlining` (or `%GetOptimizationStatus` bit
inspection) on the emitted closures — because Maglev's inlining heuristics are more
conservative than TurboFan's and a positional site that inlines under TurboFan may not under
Maglev at the same arity. This is the specific measurement the charter demands and that no
prior prover ran.

**The code-size ceiling.** Emitted size is `Σ_sites (8·arity)` bytes-of-source over the
heterogeneous sites. `CEILING` is chosen so this stays within the generated-size budget; the
correct `CEILING` is read from value.js's ACTUAL arity histogram (the sampled sites show arity
2–6; the histogram must be measured, not assumed), not a synthetic guess. Beyond `CEILING`,
sites keep the general loop (accepting megamorphism) so the specializer degrades gracefully.

---

## 4. Structural leg vs temporal leg; the deopt sweep

The certified lever (lazy diagnostic-array allocation: `ParserState.suggestions`/
`secondarySpans` from eager `= []` to lazy `undefined`, `state.ts:44-45`) was verified
CONFIRMED_DELTA — but the verification also proved that the two legs must be reported
SEPARATELY, because they behave differently:

- **Structural leg (deterministic, reproduces exactly):** the removed allocation. Emitted-JS
  eager-alloc census `6 → 0`; fused alloc `1.84 → 0.74 B/op` (`−60.1%`, exact under
  `--expose-gc`). This is an allocation/instruction fact, load-immune, and is the number that
  survived adversarial re-measure unchanged.
- **Temporal/steady-state leg (noisy, load-sensitive):** the ns throughput win. Fused
  `all(a,b,c)` reproduced at `+15.6%`, unfused then-chain `+13.6%` — but the **dispatch lane
  halved on re-measure** (claimed `+10.2%`, reproduced `~+4.9%`), because dispatch's per-round
  paired ratio is a wide band (min 0.695, max 1.420) and the claim sat at the top of it.

**The 8A mandate:** every fusion/shape claim carries BOTH numbers, and the temporal leg is the
paired per-round median over N ≥ 12 rounds in ≥ 2 load windows, never a single window's ns. The
deopt sweep must run against value.js's **actual** `all()`/`any()` sites under `--trace-deopt`
+ `%GetOptimizationStatus`, not synthetic `all(a,b,c)` triples — a synthetic triple is
SFI-homogeneous-friendly and hides exactly the megamorphic behavior the real heterogeneous
sites exhibit. The structural leg is what proves the win is real; the temporal leg is what the
honest-number law forbids inflating.

---

## 5. Packrat homing (plane-wide question)

- **Where memoization attaches.** `makeMemoized` (`packrat.ts:280-283`) wraps a parser and
  returns `Parser<T>` — a memoized parser is **shape-identical** to any other: same class,
  same five fields (`id, state, flags, parser, context`), the memo logic living entirely inside
  the `.parser` closure. No new object shape appears at the parser level, so the closure-core
  hidden-class discipline is preserved by construction. `memoize<T>(p): Parser<T>` and
  `mergeMemos` are the only entry points (`packrat.ts:482-487`). **The type of a memoized
  parser is exactly `Parser<T>`** — the answer the charter asks for.
- **The memo key.** `getCijKey(p, offset) = p.id * 2^32 + offset` (`packrat.ts:79-99`), a
  float64-safe key stored in a module-global `Map<number, MemoCell>` (`packrat.ts:133`).
  Shape-stable (a `number`), fail-loud on overflow (`RangeError`, `MEMO_MAX_ID ≈ 2.1M`).
- **The `MemoCell` shape (the one hazard).** `interface MemoCell { ans: Answer | LR }`
  (`packrat.ts:125-127`). The `.ans` field is **polymorphic** — `Answer {offset, value,
  isError}` (`packrat.ts:103`) vs `LR {seed, parser, head, next}` (`packrat.ts:110`),
  discriminated by `isLR` via `.parser !== undefined` (`packrat.ts:129`). This is the packrat
  plane's sole shape divergence. For shape-stability a tagged single-shape cell
  (`{tag: 0|1, offset, value, isError, seed, parser, head, next}`) would collapse it, but the
  field is cold — reached only on the memoized/left-recursion path, never on the default LL(1)
  hot path — so the divergence is quarantined behind arming.
- **Arming is opt-in and disclosed.** `PACKRAT_ARMED` (`packrat.ts:156`) is a latch that arms
  at the FIRST `makeMemoized` construction (`packrat.ts:290`) and never disarms; until then
  `packratEnter`/`packratExit` are TRUE no-ops and the default path pays nothing. **Every 8A
  perf table states armed/unarmed; the shipping hot path is unarmed; no throughput gate rides
  arming.** value.js, latex-paper, keyframes never construct a memoizer on their default paths,
  so all consumer-suite numbers are unarmed.

---

## 6. Prior art

- **V8 hidden-class / SFI-keyed inline caches.** The homogeneous-vs-heterogeneous distinction
  is the documented V8 IC-degree model (monomorphic → polymorphic ≤ 4 → megamorphic); the
  positional-unroll cure is the standard "split a megamorphic site into monomorphic sites"
  technique (cf. V8's own feedback-vector documentation and the `mono/poly/mega` literature).
- **`Object.create` deopt.** Prototype-chain view objects are a known de-optimization trigger
  (V8 "dictionary mode" / slow-property fallback for objects with non-standard prototypes);
  the fixed-shape-literal cure mirrors V8's "fast properties" guidance.
- **Build-time monomorphization** is the rustc/GHC specialization analogue applied to a JS
  emitter — the arity-unroll is a manual `#[specialize]` and the §2 predicate is a
  cheap-but-sound specialization heuristic akin to GHC's `SPECIALISE` pragma selection.
- **Packrat / left-recursion.** The `Answer`/`LR`/`Head` triple and `getCijKey` implement the
  Warth–Douglass–Millstein left-recursion-in-packrat algorithm; the shape-stability question is
  novel to the V8 setting, not to packrat.

---

## 7. Integration seams (path:line)

- `parser.ts:162-187` — `mapState`; the `Object.create` at `:177` is the fix site.
- `state.ts:36-53` — `ParserState<T>` field declarations; add `implements ReadonlyState<T>`.
- `state.ts:44-45` — the lazy `suggestions`/`secondarySpans` (the certified structural leg).
- `leaf.ts:28-79` — `any()` (arity-2 unroll only; the 93-site value.js surface underserved).
- `leaf.ts:154-273` — `all()`/`fuseAll` (arity-2/3 unroll + general loop `:255-272`).
- `leaf.ts:62`, `leaf.ts:261` — the megamorphic general-loop call `parsers[i].parser(state)`.
- `state.ts:173-189` + `state.ts:141-171` — `context.name`/`parserNames`: the runtime SFI proxy.
- `packrat.ts:79-99` (key), `:103-127` (`Answer`/`LR`/`Head`/`MemoCell`), `:156`/`:290`
  (arming), `:280-283`/`:482-487` (`makeMemoized`/`memoize` return type `Parser<T>`).

---

## 8. What a prototype must test to move family 8A

1. **Fixed-shape `mapState` view.** Replace `Object.create(state)` with a `ReadonlyState<T>`
   literal; assert via `node --allow-natives-syntax` + `%HaveSameMap` that repeated views share
   one hidden class and that a `mapState`-driving microbench shows no caller deopt
   (`%GetOptimizationStatus`). Consumer suites are trivially GREEN (no consumer calls it), so
   the oracle is the shape probe plus the combinator's own unit test — this is the "run an
   actual exercise, not a shape probe alone" requirement met by the one test that exists.
2. **SFI-heterogeneity predicate on the REAL corpus.** Instrument value.js HEAD's actual
   `all()`/`any()` sites, compute `d` per site from `context.name`, bucket homogeneous vs
   heterogeneous, then measure unrolled-vs-loop **paired** ns per bucket + `--trace-deopt` +
   the structural (alloc-B/op, ins/B) leg separately from the temporal (ns) leg. This falsifies
   or confirms that heterogeneous arity ≥ 4 is where the win lives, and quantifies the
   false-positive rate on genuinely homogeneous sites.
3. **Arity-N specializer under BOTH tiers.** Emit positional closures for value.js's measured
   arity histogram; verify inlining under `--trace-turbo-inlining` AND `--trace-maglev-inlining`
   at each arity, and record the emitted-source-size sum against a chosen `CEILING`. Coordinate
   the single TS-execution number with LOCK-4B.
4. **Pin the value.js v4 trajectory (R-8A-1).** Determine whether the parse-that-free `v4/`
   refactor is committing; if it is, re-scope the specializer's justification to the surviving
   consumers before investing in the emitter. A specializer built for a consumer that is
   deleting itself is consumer-less substrate (Y.13).
