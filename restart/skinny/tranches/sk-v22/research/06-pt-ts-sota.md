# 06 — parse-that TypeScript / V8 SOTA audit (R4b)

Scope: the TypeScript tree of `/Users/mkbabb/Programming/parse-that` (`@mkbabb/parse-that@1.0.0`),
audited as a V8 artifact — hidden-class stability, call-site monomorphism, closure/allocation
census, string handling, the packrat latch — plus the two open value.js asks (PT-E1 scoped
diagnostics, PT-E2 the `Parser<any>` leak) re-drawn as redesigns, plus a legacy/deprecated sweep.
Charter correction 4: parse-that is IN scope for uplift; backbone status (value.js, latex-paper,
keyframes) raises the discipline bar (semver; consumer suites are the close gates) but does not
exempt it. This report ends in a TS uplift wave list, each wave carrying its consumer-suite gate.

The tree is small and already hard: `parser.ts` 711, `packrat.ts` 488, `leaf.ts` 399, `debug.ts`
383, `state.ts` 189, `utils.ts` 186, the rest ≤ 58. 2650 LOC total under `src/parse/`. The
perf history (`docs/perf-optimization-ts.md`) already did the big wins — mutable `ParserState`,
zero-alloc save/restore, `test()`+`substring()` regex, FIRST-set `dispatch()`, inlined `wrap()`,
arity-2/3 `all`/`any` unrolls. The uplift here is not a rewrite; it is a set of surgical,
idiomatic transpositions plus a modernization pass. **No quick fixes, no legacy carried.**

---

## 0. The consumer close-gate surface — what semver actually protects

Before any change: the exact surface the three backbone consumers touch, because THAT is what a
1.1.0 must not break. Everything outside it is additive-free-fire.

**Named imports consumed** (grep over `value.js/src`, `latex-paper/src`, `keyframes.js/src`):
`Parser`, `ParserState` (type only), `all`, `any`, `dispatch`, `mergeErrorState`, `regex`,
`string`, `whitespace` — **9 identifiers**. keyframes.js's published tree is parse-that-FREE
(it reaches parse-that transitively through value.js's `^1.0.0` re-pin; only a keyframes worktree
`keyframes-wt-H-W2-verify/src/animation/utils.ts:1` imports `any as parseAny`).

**Parser instance methods consumed** — value.js: `.map` (211), `.trim` (131), `.next` (50),
`.wrap` (35), `.skip` (28), `.opt` (25), `.many` (10), `.sepBy` (7), `.not` (6), `.chain` (4),
`.or` (3), `.eof` (3), `.parse` (2), `.parseState` (2); latex-paper adds `.then` (23). No
consumer calls `.mapState`, `.minus`, `.peek`, `.lookAhead`, or `.recover`.

**ParserState surface consumed** (via the `ParserState` type import): `state.err(undefined, 0)`
(value.js `parsing/index.ts:309,323`, `parsing/utils.ts:151,161,239,503`), `state.getLineAndColumn`
(value.js `parsing/utils.ts:545`), and the fields `src`/`offset`/`value`/`isError`/`furthest`/
`expected` plus `ok()` transitively. Note `state.err()` "has no message parameter" is called out
IN value.js source (`parsing/utils.ts:492`) as the reason three authored messages are silent — that
is the PT-E1 gap seen from the consumer side.

**Confirmed UNCONSUMED by all three backbones** (safe to change/retire): `ParserState.save`/
`restore`/`from`/`getLineNumber`/`getColumnNumber`; `spanToString`/`mergeSpans`/`createParserContext`;
`getLazyParser`/`createLazyCached`/`lazy` decorator; `memoize`/`mergeMemos`/`resetPackrat`;
`containsDelimiter`/`splitBalanced` (bbnf-`toDoc()`-facing, `split.ts:1-4`); `trimStateWhitespace`,
`eof`, the `skipWhitespace`/`skipBlockComments` byte scanners; the `Span`/`ParserContext`/
`Suggestion`/`SecondarySpan`/`Diagnostic` types; the whole `./diagnostics`, `./packrat`, `./utils`
subpath tiers. (The `memoize` hits in value.js are value.js's OWN local LRU `memoize` from
`../utils`, not parse-that's packrat memoize.)

**1.1.0 shape verdict**: PT-E1 and PT-E2 are both achievable as a **MINOR (additive)** release if
they add rather than mutate the consumed 9-identifier / listed-method surface. The V8/legacy
transpositions below are internal and behavior-preserving → also MINOR. The only tripwire that
would force a 2.0.0 is retiring a consumed ParserState method (e.g. deleting `err()` when hardening
the state shape) — the plan below keeps all consumed methods.

---

## 1. The combinators on V8

### 1.1 Hidden-class stability of `Parser` and `ParserState`

**`Parser` (`parser.ts:24-32`).** Field order at construction: the inline class fields
`id` (`= PARSER_ID++`), `state` (`= undefined`), `flags` (`= FLAG_NONE`) initialize first, then the
parameter properties `parser`, `context`. Every `Parser` gets the same five own properties in the
same order → **one hidden class, stable**. The two post-construction mutations are benign:
`this.state` transitions `undefined → ParserState` (`parseStateInner`, `parser.ts:66,71`) — a value
representation change on an existing slot, **not** a shape transition; `flaggedParser.flags = ... `
(`parser.ts:496`) writes number→number on an existing slot. Neither adds/deletes a property. **No
finding on the Parser value shape itself.**

**The real Parser-shape hazard is `context` (`parser.ts:31`, default `= {}`).** `createParserContext`
(`state.ts:179-189`) returns `{name, parser, args}` (3 fields), but the constructor default is the
EMPTY object `{}` (0 fields). Any `new Parser(fn)` with no context — and there are such sites — gives
`parser.context` a DIFFERENT hidden class than the 3-field norm. Reads of `parser.context?.name`
(`parser.ts:488`) and the whole `debug.ts` printer then see a polymorphic `.context`. It is
construction/introspection-frequency, not hot-loop, so the cost is small — but it is a gratuitous
shape split. **Redesign**: make context construction uniform — always mint `{name, parser, args}`
(a `NONE_CONTEXT` singleton for the no-arg case), so `.context` is monomorphic everywhere.

**`ParserState` (`state.ts:36-53`).** Field order: the class fields `expected` (declared
`expected?: string[]` — with `target: ES2022` → `useDefineForClassFields` true, this DOES define an
own `undefined` slot), `suggestions = []`, `secondarySpans = []` initialize first, then the five
constructor params `src`/`value`/`offset`/`isError`/`furthest`. **Eight own properties, same order
every time → stable.** The error-view rebuild (`parser.ts:60-66`) RE-ASSIGNS existing slots
(`errorState.expected = …` etc.) — no shape change. Good.

**One genuine ParserState shape defect: `mapState` (`parser.ts:177-179`).** It builds the old-state
view with `Object.create(state)` then adds `oldView.offset`/`oldView.value` as own props on a
prototype-chained object. The object handed to `fn` is neither a real `ParserState` (different
prototype-chain shape) nor stable across calls — a classic V8 dictionary-mode / megamorphic-read
hazard for any code that reads the old state. `mapState` has **zero consumers** across the three
backbones and zero internal callers. **Disposition: RETIRE `mapState`** (legacy §4), or, if a future
need appears, re-express it as a fixed-shape `{offset, value}` snapshot literal — never
`Object.create`.

### 1.2 Call-site monomorphism — where dispatch goes megamorphic

The structural fact (confirmed by V8 SOTA, §3, and already named in `docs/perf-optimization-ts.md
§5.2`): every combinator stores its parse fn in `Parser.parser`, and each combinator factory mints a
UNIQUE closure shape. A dispatch loop that calls `parsers[i].parser(state)` at ONE shared call site
sees N distinct targets; past 4, V8 abandons the site to generic `CallFunction` (measured 5.9% of CPU
in the historical profile).

- **`any()` general arm (`leaf.ts:59-72`)**: `parsers[i].parser(state)` is the megamorphic site.
  The arity-2 unroll (`leaf.ts:39-57`) splits it into two SEPARATE call sites `p0.parser`/`p1.parser`,
  each closed over one captured binding per `any()` instance — so each site is monomorphic-per-instance.
  This is the correct V8 shape and should be the template.
- **`all()`/`fuseAll` (`leaf.ts:179-273`)**: same treatment — arity-2/3 fully unrolled into positional
  `p0/p1/p2.parser` sites; general arm indexes. Correct.
- **`dispatch()` (`leaf.ts:134-145`)**: `parsers[idx].parser(state)` is a SINGLE shared site indexed
  by an `Int8Array` LUT. It eliminates the sequential-trial WORK but NOT the polymorphism — the one
  site still sees every arm's closure → megamorphic for tables > 4 arms (value.js's CSS function-name
  bucket is exactly this). This is the residual the killed SpanParser tier tried and FAILED to fix
  (§3, `future-research.md §7`: the runtime `switch` ran ~10-14% SLOWER on V8). **The lesson stands:
  do not re-attempt runtime tagged-dispatch on V8.** The only viable lever is reducing the COUNT of
  megamorphic sites, not the polymorphism at one. Two idiomatic options, both additive:
  (a) a specialized 2-arm `dispatch` unroll mirroring `any`'s (the common 2-way keyword split);
  (b) a small-table (≤ 4 arm) form that emits an explicit if-chain closure (polymorphic-but-inlinable)
  instead of the indexed load. Both are perf-gated, not correctness — carry only if `proof:perf`'s
  real-CSS clause (below) shows a win, exactly as PT-Q5 mandated.

### 1.3 Closure-allocation census — per-parse vs per-construction

The discipline to verify: allocation happens at **construction** (grammar build), not at **parse**
(hot loop). Audit result — the combinators are clean:

- Per-construction (once per grammar node): every `new Parser(closure, context)` in the factory
  methods, the `createParserContext` object, the `dispatch` LUT + label. Fine.
- Per-parse, STRUCTURAL (the user's output, unavoidable): `then()` → `[value1, value2]` 2-tuple
  (`parser.ts:90`); `all`/`fuseAll` → one result array (`leaf.ts:191,222,257`); `many`/`sepBy` →
  the matches array, pre-sized when `min > 0` (`parser.ts:526,572`); `Object.fromEntries` in json.
  These are the deliverable — correct.
- Per-parse, INFRASTRUCTURE (should be zero): the mutable single-`ParserState` thread means combinators
  carry inline `savedOffset` locals, no `save()` object. Confirmed across `then`/`or`/`skip`/`next`/
  `opt`/`many`/`sepBy`/`not`/`wrap`/`trim` — all use `const savedOffset = state.offset`. **Zero
  infrastructure allocation on the LL(1) path. This is the crown jewel; keep it inviolate.**
- The ONE avoidable per-parse alloc: `mapState`'s `Object.create` (`parser.ts:177`) — retired anyway.

### 1.4 String handling — offset-carrying vs materializing

- `string()` (`leaf.ts:276-313`): len-1 fast path is a bare `charCodeAt` compare (no substring); the
  general path uses `startsWith(str, offset)` (no slice) and stores the pre-known literal `str` as the
  value. Zero-materialization. Correct.
- `regex()` (`leaf.ts:317-369`): default path is sticky `test()` + `substring(savedOffset, end)` — one
  string, only when the value is actually consumed. `exec()` (allocates a `RegExpMatchArray`) only on
  the `matchFunction` path. Correct and already documented (`perf-optimization-ts.md §5.1`).
- `state.ts` uses `.substring`/`.slice` for line/column (`getLineAndColumn`, cold error path) — fine.
- **Consistency nit**: `spanToString` uses `substring` (`state.ts:14`); `split.ts` uses `slice`;
  `getColumnNumber` uses `lastIndexOf`. All correct, but `getColumnNumber`/`getLineNumber`
  (`state.ts:111-124`) are cold, superseded, and unconsumed → retire (legacy §4), leaving
  `getLineAndColumn` (`state.ts:127`, the only consumed one) as the single line/col API.

### 1.5 The packrat memo cost model and the PACKRAT_ARMED latch — a generalizable pattern

`getCijKey` (`packrat.ts:79-100`) is a float64-safe composite key `id * 2^32 + offset` (whole offset,
no mask), with a fail-loud `RangeError` past the safe-integer ceiling (id ≤ ~2.1M). Sound. The memo
Maps are keyed on that number — as fast as an int32 key, no per-lookup string alloc.

The load-bearing design is **PACKRAT_ARMED** (`packrat.ts:156, 216-250, 262-272`): the packrat epoch
(three Maps opened per top-level parse for cross-input + re-entrancy soundness) is gated behind a
module latch that arms on the FIRST `memoize()`/`mergeMemos()` construction (`packrat.ts:290`) and
never disarms. Until armed, `packratEnter`/`packratExit`/`resetPackrat` are true no-ops — so the LL(1)
constellation (value.js, latex-paper, json/csv) pays **zero** for machinery it never consults. This is
the correct model.

**The generalization — and the bridge to PT-E1.** The latch is a reusable pattern: *an optional tier
costs zero on the default path until a consumer of that tier is constructed.* PT-E1's scoped
diagnostics wants exactly this discipline — diagnostics must be zero-cost-when-off. The redesign
(§2.1) should reuse the SAME latch shape: a diagnostics scope that is inert (no per-parse allocation,
no field seeding) until a consumer opens a diagnostic scope, mirroring `PACKRAT_ARMED`. One pattern,
two tiers — the KISS/DRY win.

### 1.6 Escape-analysis-friendly result shapes — the chain() lineage

The `chain()` falsy-seed fix (`parser.ts:124-144`, C-16, 1.0.0) is the reference: it dropped the
`state.value || chainError` guard that silently skipped the continuation on a falsy-but-valid seed,
and now unconditionally threads `fn(state.value).parser(state)` on success. Clean, monomorphic, no
sentinel object. The broader escape-analysis posture is healthy — the mutable state thread means most
transient objects never escape a single combinator frame. The residual non-escaping churn is the
per-`then` 2-tuple; `all`/`fuseAll` already collapse the `a.then(b).then(c)` nest (N−1 tuples) into
one flat array when the grammar is expressed as `all(...)` — a real win value.js already exploits
(59 `all()` sites per the fusion note, `leaf.ts:174-176`).

---

## 2. The asks as redesign, not patch

### 2.1 PT-E1 — scoped per-parse diagnostics (HIGH)

**The defect, precisely.** `ParseDiagnostic.expected` is structurally dead because the diagnostic
machinery has TWO structural blockers, not one:

1. **The enable toggle is a MODULE GLOBAL, not a scope.** `diagnosticsEnabled`
   (`utils.ts:6-18`) is a process-wide boolean flipped by `enableDiagnostics()`/`disableDiagnostics()`.
   The expected-set is only ever seeded when this global is true (`utils.ts:33,38`). So "scoped
   per-parse diagnostics" is impossible today — a library that enables diagnostics does so for the
   whole process, and two concurrent parses cannot differ.
2. **Even when enabled, the diagnostic never REACHES the caller.** On error, `parseStateInner`
   (`parser.ts:55-69`) builds a rich `errorState` (expected, suggestions, secondarySpans at
   `furthest`), stashes it on `this.state`, and — if the global is on — `console.error`s its
   `toString()`. Then `parse()` (`parser.ts:77-79`) returns `parseState(val).value`, which on error is
   `undefined`. There is **no API that returns a structured diagnostic from a parse.** value.js
   authors correct messages (`state.err()` has no message param — value.js `parsing/utils.ts:492`;
   `mergeErrorState(state, label)` is the only channel) and they vanish into a `console.error` gated
   by a global.

**The redesign (not a patch).** Design the diagnostics surface the way bbnf's generated parsers will
need it — scoped, zero-cost-when-off, structured-return:

- **A scoped result, additively.** Add `parseState`-adjacent surface that RETURNS the diagnostic
  rather than console-logging it. The cleanest additive shape that does not disturb the consumed
  `.parse(): T` / `.parseState(): ParserState` methods: a new `Parser.parseWithDiagnostics(src):
  { value: T | undefined; ok: boolean; diagnostic?: Diagnostic }`. `Diagnostic` already exists
  (`utils.ts:84-93`: offset, furthestOffset, line, column, expected, suggestions, secondarySpans,
  found) and `collectDiagnostic` (`utils.ts:102-128`) already snapshots a state INTO one — the
  producer path is 90% built; it is simply never invoked from the scoped `.parse()` boundary.
- **Zero-cost-when-off via the latch pattern (§1.5).** Replace the global `diagnosticsEnabled` with a
  per-scope flag threaded on the epoch — mirror `PACKRAT_ARMED`: a diagnostics scope opened at the
  `parseWithDiagnostics` boundary (like `packratEnter` at the parseState boundary), so the default
  `.parse()` path seeds NO expected-set, allocates NO suggestion arrays — the `mergeErrorState`
  fast path (`utils.ts:28-34`) already early-returns the label-seeding when disabled; the redesign
  makes "enabled" a scope, not a process mode. `suggestions`/`secondarySpans` init as `[]` on EVERY
  state today (`state.ts:44-45`) — under the scoped design they should be lazily allocated only inside
  a diagnostics scope, removing two array allocations from every ParserState on the default path.
- **The `expected` field lights up.** With the scoped producer wired, `Diagnostic.expected` carries
  value.js's authored labels to the consumer with **zero value.js change** (the letter's promise,
  `VALUEJS-PT-E §the value.js-side half`). value.js keeps authoring; parse-that opens the channel.
- **What bbnf mirrors.** bbnf's generated parsers already thread furthest-offset/expected-set state
  (the same substrate PT-WAVE-1 added on the TS side). When bbnf's Value-API grows an error surface,
  THIS scoped-return `Diagnostic` shape is the design to copy — not the global-flag console path.

**Semver**: additive (new method + internal rewiring of an off-by-default tier). MINOR → 1.1.0.
**Close gate**: a new `test/diagnostics-scoped.test.ts` born-RED against 1.0.0 — asserts
`parseWithDiagnostics` on a failing parse returns a `Diagnostic` with a non-empty `expected`, that two
interleaved scoped parses do not cross-contaminate (re-entrancy, the packrat-reentrant analogue), and
a retained-heap clause that a default `.parse()` on the LL(1) corpus allocates NO suggestion/expected
arrays (the `proof:packrat-armed` flat-probe pattern, reused). value.js's own suite, run against the
1.1.0 tarball, is the outer close gate.

### 2.2 PT-E2 — the `Parser<any>` inference leak (MED)

**The leak, precisely.** Three sites bleed `any` into consumer inference:

1. `any<T extends Array<Parser<any>>>` (`leaf.ts:28`) and `all<T extends Array<Parser<any>>>`
   (`leaf.ts:154`): the element CONSTRAINT is `Parser<any>`. `Result = T[number] extends Parser<infer
   V> ? V : never` recovers `V`, but because the constraint admits `any`, a heterogeneous
   `any(pA, pB)` widens the union toward `any` rather than `A | B`, and the `eslint-disable
   no-explicit-any` at each site is load-bearing, not cosmetic. This is what forces
   `const jsonValue: Parser<any>` (`parsers/json.ts:40`) and propagates into value.js's barrels.
2. `ParserFunction<T = string> = (val: ParserState<any>) => ParserState<any>` (`parser.ts:14-16`):
   the type-erased function-pointer intentionally uses `any` for variance, but it means every
   `.parser` field is `(ParserState<any>) => ParserState<any>` — the erasure is deliberate for the
   internal plumbing but should NOT surface to consumers.
3. `restore(saved: { offset: number; value: any })` (`state.ts:79`) — cold, unconsumed, retire.

**The redesign (TS 5.x gestalt, not a cast-swap).** parse-that pins `typescript@^5.8`
(`package.json`) — the modern inference toolkit is available:

- **Tighten the array constraints** from `Parser<any>` to `readonly Parser<unknown>[]` and recover the
  element type with a mapped/`infer` tuple type. `any` → `unknown` at the constraint flips the leak:
  `unknown` does not widen a union, so `any(pA, pB): Parser<A | B>` instead of `Parser<any>`. The
  internal `.parser(state as ParserState<unknown>)` casts stay (they are the erasure boundary), but the
  PUBLIC return type is precise.
- **`const` type parameters** (TS 5.0) on `dispatch` and the variadic combinators so a table/tuple of
  parsers infers literal-precise element types instead of widening — `dispatch<const T>(table: T)`
  keeps the value types tight through the barrel.
- **`NoInfer<T>`** (TS 5.4) where a combinator has a seed AND a continuation referencing the same param
  (e.g. `chain`, `recover`'s sentinel) to pin inference to the intended position and stop the sentinel
  widening the result.
- The `ParserFunction` erasure stays internal; the fix is that the PUBLIC combinator signatures no
  longer echo `any` outward.

**Semver**: additive/tightening. A tighter public type CAN in principle red a consumer that relied on
the `any` widening — so this is the one that MUST be gated on the consumer suites. Expected impact:
value.js/latex-paper get MORE precise types (they annotate their own parser vars today), so the
tightening should be inbound-compatible; verify with a typecheck of each consumer against the 1.1.0
`.d.ts`. MINOR if the consumer typechecks stay green; hold to a 2.0.0 note only if a consumer's
inference genuinely depended on the leak.
**Close gate**: `test/dist-surface.test.ts` extended with a `.d.ts` inference assertion
(`any(string("a"), string("b"))` resolves to `Parser<string>`, not `Parser<any>`); plus a
cross-repo typecheck of value.js + latex-paper against the candidate `dist/*.d.ts` — green is the gate.

---

## 3. V8 / TS SOTA frontier check (web, July 2026)

**V8 tiering & ICs (2025-26).** V8 now runs a four-tier pipeline: Ignition (interpreter) → Sparkplug
(baseline, no speculation) → **Maglev** (mid-tier optimizer, shipped Chrome M117, builds a simplified
CFG from runtime feedback) → TurboFan (peak). IC states are unchanged and decisive: monomorphic (1
shape) → single map-check fast path; polymorphic (2-4) → linear check chain; **megamorphic (>4) →
optimization abandoned for that site**, and a function whose ICs go megamorphic may not be optimized at
all. This confirms the parse-that posture exactly: keep call sites monomorphic (the arity unrolls),
keep object shapes stable (the Parser/ParserState hidden-class discipline), and accept that the shared
`dispatch` site is the one irreducible megamorphic point — which the FALSIFIED SpanParser experiment
proved cannot be beaten by a runtime `switch` on V8.

**The closures-vs-classes question (v8-users).** The SOTA answer to "replace combinator classes with
closures to avoid megamorphism": closures help ONLY by giving each call site a distinct captured
target (monomorphic-per-instance), which is exactly what the arity unrolls already do; they do NOT
erase polymorphism at a genuinely shared dispatch site. parse-that is already on the right side of
this — no architectural change indicated, and the TypeScript-team's own "monomorphic AST nodes"
work (microsoft/TypeScript#59198) is the same principle applied to their compiler.

**TS 5.x features worth adopting** (all shipped, parse-that pins ^5.8): `const` type parameters (5.0,
literal-precise inference), variance annotations `in`/`out` (5.0, though variance is auto-inferred so
these are documentation more than fix), **`NoInfer<T>`** (5.4, the targeted inference-position fix),
improved higher-order inference (5.9). These are precisely the PT-E2 toolkit. No adoption blocker.

**Parser-combinator libraries that solved megamorphism**: the field consensus (megaparsec/Haskell,
the V8 combinator threads) is that combinator perf is dominated by call overhead and that the ONLY
robust win is a codegen/generated specialized dispatcher (a flat array-of-ops with a generated switch),
NOT a runtime-tagged one — which is what `future-research.md §7` already records as the sole
un-falsified path and which bbnf's codegen (not parse-that's runtime) is positioned to be. No
library offers a runtime trick parse-that is missing.

Sources: [Maglev — V8's Fastest Optimizing JIT](https://v8.dev/blog/maglev),
[V8 Engine Architecture (Jaiswal)](https://sujeet.pro/articles/v8-engine-architecture),
[Ignition & TurboFan pipeline (thlorenz/v8-perf)](https://github.com/thlorenz/v8-perf/blob/master/compiler.md),
[v8-users: closures vs megamorphism](https://www.mail-archive.com/v8-users@googlegroups.com/msg14940.html),
[NoInfer — Total TypeScript](https://www.totaltypescript.com/noinfer),
[TS 5.x features that matter 2026](https://dev.to/synsun/typescript-5x-in-2026-features-that-actually-matter-for-production-code-27kp),
[TS#59198 Monomorphic AST Nodes](https://github.com/microsoft/TypeScript/issues/59198).

---

## 4. Legacy / deprecated audit (TS tree)

Confirmed dead-or-superseded surface, all UNCONSUMED by the three backbones (§0):

- **`ParserState.mapState` + `Parser.mapState`** — `Object.create`-based old-state view
  (`parser.ts:162-187`), the one per-parse shape hazard (§1.1); zero consumers, zero internal callers.
  **RETIRE.**
- **`ParserState.save`/`restore`/`from`** (`state.ts:75-84,69-73`) — superseded by inline
  `savedOffset` locals (`perf-optimization-ts.md §3.2`); 0 internal, 0 consumer hits. `restore` has one
  internal reference only in its own definition. **RETIRE** (`err`/`ok` STAY — `err` is consumed by
  value.js).
- **`ParserState.getLineNumber`/`getColumnNumber`** (`state.ts:111-124`) — superseded by
  `getLineAndColumn` (the only line/col API any consumer or debug.ts calls); 0 hits. **RETIRE**,
  leaving one line/col method.
- **The global `enableDiagnostics`/`disableDiagnostics`/`diagnosticsEnabled`** (`utils.ts:6-18`) —
  the process-global toggle is the PT-E1 defect substrate; **REPLACE** with the scoped design (§2.1),
  not merely retire (a subpath export exists, so this is a coordinated deprecate→replace, gated).
- **`Parser.context = {}` default** (`parser.ts:31`) — the shape-splitting empty default (§1.1);
  **normalize** to a uniform 3-field context.
- **`getLazyParser` + `LAZY_PARSER_CACHE`** (`lazy.ts:5-15`) — a WeakMap-keyed lazy cache used ONLY by
  `debug.ts:318`; `createLazyCached` (the closure-local cache, `lazy.ts:18-24`) is the one Parser.lazy
  uses. `getLazyParser` and the `lazy` method decorator (`lazy.ts:30-43`) have no production consumer.
  **Candidate retire** (verify no bbnf-codegen dependence first; if bbnf emits the decorator, keep).
- **`debug.ts` `PARSER_STRINGS` module Map** (`debug.ts:245`) — unbounded process-lifetime cache keyed
  by `parser.id`; debug-only, but a latent leak in a long-lived process that builds many grammars.
  Bound it or clear on demand. Low priority.
- **No dead combinators remain** — `thenMap`/`fuse`/`subTable`/`*Span`/SpanParser were all already
  excised (0.13.0 PT-Q3/Q5, 1.0.0 S.H2, B.W0). The `proof:no-dead-combinator`/`proof:no-span-surface`
  gates enforce this. The legacy sweep here is the SECOND, finer pass over the state/lazy tiers.

---

## 5. The TS uplift wave list — transpositions with consumer-suite close gates

Ordered; each additive unless noted; each carries its gate. Target release **1.1.0** (all MINOR).

**TW-PT-TS.1 — PT-E2 the inference tightening (the type gestalt).** `Parser<any>`→`Parser<unknown>`
constraints on `any`/`all`/`fuseAll`; `const` type params on `dispatch` + variadics; `NoInfer` on
`chain`/`recover`; retire the outward `any` echo while keeping the internal erasure boundary.
*Gate*: `.d.ts` inference assertion (`any(string("a"),string("b")): Parser<string>`) added to
`dist-surface.test.ts`; **cross-repo typecheck of value.js + latex-paper against the candidate
`dist/*.d.ts` stays green** (the semver tripwire — if a consumer relied on the leak, this reds and the
item escalates to a 2.0.0 note).

**TW-PT-TS.2 — PT-E1 scoped diagnostics (the error surface).** Add
`parseWithDiagnostics(): {value, ok, diagnostic?}` returning the existing `Diagnostic`; thread a scoped
diagnostics flag on the parse epoch (reuse the `PACKRAT_ARMED` latch pattern); lazily allocate
`suggestions`/`secondarySpans` only inside a scope (remove two per-state arrays from the default path);
deprecate-then-replace the global `enableDiagnostics` toggle. *Gate*: new
`test/diagnostics-scoped.test.ts` born-RED — non-empty `expected` reaches the caller; two interleaved
scoped parses do not cross-contaminate; retained-heap flat clause (default `.parse()` allocates no
diagnostic arrays, the `proof:packrat-armed` probe reused). value.js suite green against the 1.1.0
tarball = outer gate.

**TW-PT-TS.3 — the V8 hidden-class normalization.** Uniform `Parser.context` shape (kill the empty-`{}`
default → `NONE_CONTEXT` singleton or always-3-field); retire `mapState` (the `Object.create` shape
hazard); make the default parse path allocate no diagnostic arrays (folds with TW-PT-TS.2).
*Gate*: `proof:perf` extended with a shape-stability clause (all `Parser`/`ParserState`/`context`
instances share one hidden class — assertable via `%HaveSameMap` under `--allow-natives-syntax` in a
Node probe, or a structural own-keys-order check); the existing json-regression clause must not
regress (>15% floor).

**TW-PT-TS.4 — the legacy/state-tier sweep.** Retire `save`/`restore`/`from`/`getLineNumber`/
`getColumnNumber` (all unconsumed, superseded); assess-and-retire `getLazyParser` + the `lazy`
decorator (keep iff bbnf-codegen emits them); bound the `debug.ts` `PARSER_STRINGS` cache.
*Gate*: `dist-surface.test.ts` (dist == source barrel) stays green after the removals; a born-RED
`proof:no-legacy-state-surface` (the `proof:no-span-surface` template) asserting the retired methods
are absent from the built `.d.ts`; consumer typechecks green (they touch none of these).

**TW-PT-TS.5 — the dispatch megamorphism lever (perf, carry-iff-measured).** ONLY if TW-PT-TS.3's
profile shows the shared `dispatch` site dominating: add a 2-arm `dispatch` unroll and/or a ≤4-arm
if-chain form (both closure-based, NO runtime tagged-switch — that path is permanently falsified).
*Gate*: `proof:perf` clause B' (real CSS function-name bucket, value.js's actual shape) must show the
new form ≥ the floor faster than the current single-site `dispatch`; **if no on-path win, DO NOT
CARRY** (the PT-Q5 discipline — no synthetic-corpus perf seams). Deferred/killed if unmeasured.

**Cross-cutting close gate (all waves).** `npm run proof:all` (manifest, subpath, packrat-armed/
cross-input/reentrant/large-offset, no-span, no-dead-combinator, perf) stays green; the built dist
surface equals the source barrel; and the three backbone suites — value.js, latex-paper, keyframes
(transitively) — run against the 1.1.0 candidate before publish. Rust tree is a SEPARATE track (frozen
at 2026-05-02, smallbox[nightly] de-nightly + fork-fold-home + WDM retire per the NO-GO) — not in this
TS wave list.

### Non-goals (recorded, do not re-litigate)

Runtime tagged-union / SpanParser dispatch (falsified −10-14% on V8); zone-splitting `parser.ts` (r6
#6, net-negative — the subpath map IS the zone map); a parse-that-side zero-copy `*Span` surface (r6
#8, delegated to value.js's scanner layer); token streams / incremental / Squirrel LR / Pratt (1.0.0
non-goals + PT-E3 dormant). The WDM/LR tier stays PROVISIONAL pending bbnf's LR-consumer GO/NO-GO — a
bbnf decision, not a parse-that wave.
