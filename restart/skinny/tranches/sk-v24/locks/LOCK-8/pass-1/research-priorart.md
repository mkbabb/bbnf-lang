# LOCK-8 — THE TS BACKBONE PLANE — pass-1 PRIOR-ART RESEARCH

Scope: the parse-that TypeScript combinator backbone as a first-class design problem —
V8 discipline (hidden classes, IC, allocation), the diagnostics plane (PT-E1 per-parse
scoping), TYPE-LEVEL inference (PT-E2 ExtractValue / Parser<any> leaks — TS's own
inference story, distinct from LOCK-1's Rust HM solver), packrat semantics, the API
surface toward 1.1.0, and the consumer contract (value.js, latex-paper, keyframes.js;
semver; migration). No family allegiance — this pass sweeps what EXISTS, what WON and
LOST for this problem class here and in comparable systems, the constraints our own
measured evidence base already imposes, and the traps named in comparable systems.

Citations: `repo/path:line` for code/docs; URLs for external prior art. parse-that paths
are READ-ONLY recon; the SPEC/PT paths are the binding codex.

---

## 0. THE PROBLEM CLASS, STATED

A parser-combinator library on V8 where (a) the hot path is an indirect call through a
per-combinator closure field (`this.parser(state)`) that is *structurally* megamorphic;
(b) the value type carried by state is `string|array|object|undefined` — polymorphic by
nature; (c) the public API must carry precise types through variadic heterogeneous
combinator arrays (`all`, `any`) WITHOUT a runtime cost; (d) diagnostics must be
per-parse-scoped without a hot-path tax; and (e) three live external consumers gate every
change. This is NOT LOCK-1's problem: LOCK-1 is a Rust HM/CSP solver erased to zero
runtime cost; LOCK-8's type plane is TypeScript's *structural* inference over the public
combinator algebra, and its runtime plane is a V8 JIT discipline, not an LLVM one. The two
type stories are governed by opposite engines and must not be conflated (SPEC §0 refutation
09 lesson: what wins in Rust need not transfer to V8 — proven below).

---

## 1. WHAT EXISTS — the parse-that TS backbone at HEAD (1.0.0)

Package `@mkbabb/parse-that` v1.0.0, TS root `parse-that/typescript`, 2650 LOC under
`src/parse/`; multi-entry subpath exports `. / core / diagnostics / packrat / utils`
(R4 §1). The combinator core is `parser.ts` (711 LOC, `Parser<T>` class + all methods);
leaf primitives in `leaf.ts` (399); opt-in WDM packrat + left-recursion memo in
`packrat.ts` (488); diagnostics accumulator + whitespace/block-comment scanners in
`utils.ts` (186 — a GOD MODULE, two unrelated concerns) (R4 §1 table).

The historical arc is documented: `parse-that/docs/perf-optimization-ts.md` — "From 746 to
5,480 ops/s" across three phases. The wins that LANDED and are do-not-regress:
- **Immutable→mutable `ParserState`** (746→~2,500 ops/s) + **zero-alloc save/restore**
  (perf-optimization-ts.md:53-101). The single foundational lever.
- **FIRST-set dispatch tables** — `dispatch()` O(1) `Int8Array(128)` first-char table
  (leaf.ts:100-151); on canada.json this killed 333,000 failed parser invocations
  (perf-optimization-ts.md:118-151).
- **RegExp.exec allocates on every match** → sticky-flag `test()` zero-alloc path
  (leaf.ts:344-358; perf-optimization-ts.md:177-201).
- **Arity unrolling**: `any` arity-2 (leaf.ts:39-57), `fuseAll` arity-2/3
  (leaf.ts:183-251) — value.js's 59 `all()` + hot 3-tuples; general arms use classic
  `for`, never `for…of` iterator objects (R4 §6d).
- **Pre-allocated + trimmed arrays** in `many`/`sepBy` (parser.ts:523-636); numeric
  float64-safe packrat memo keys (packrat.ts:71-100).

parse-that has **NO bespoke TS regex engine** — the TS runtime uses native `RegExp` only
(R4 §3). Any TS-side regex-engine work is greenfield; the bespoke NFA/DFA engine is the
Rust side (LOCK-5's home). This bounds LOCK-8: the TS plane is combinators + native
RegExp, not a second automata engine.

---

## 2. THE COMPARABLE SYSTEMS — typed TS combinators, what won/lost

The typed-TS-combinator design space is well-trodden; the leaders cluster into two camps.

**Runtime combinators with type inference** (parse-that's own camp):
- **parsimmon** (JS, the archetype) — untyped-ish, `.d.ts` bolted on; the "any leaks"
  parse-that is curing (PT-E2) are exactly parsimmon's legacy shape.
- **arcsecond** — Fantasy-Land/monadic; carries value types but famously loses precision
  through `sequenceOf`/`choice` (the heterogeneous-array inference wall PT-E2 hits).
- **gcanti/parser-ts** (https://github.com/gcanti/parser-ts) — fp-ts-based, `Parser<I, A>`
  fully typed; the reference for principled typing but heavier ergonomics.
- **ts-parsec / microsoft/ts-parsec** (https://github.com/microsoft/ts-parsec) — explicit
  goal "TypeScript provides a powerful foundation"; strong on error handling + ambiguity,
  the two areas PT-E1 (diagnostics) touches.
- **parjs, chevrotain, ohm** — parse-that benches against all of these
  (`test/benchmarks/`, R4 §4); chevrotain is the value-building-mode ratio target
  (perf-optimization-ts.md:319-333).

**Type-LEVEL combinators** (the type system IS the parser):
- **sinclairzx81/parsebox** (https://github.com/sinclairzx81/parsebox) — parser
  combinators *in the TS type system*; EBNF encoded as types. The extreme of PT-E2's
  direction. Instructive as a boundary: full type-level parsing is a compile-time cost
  monster (deep conditional-type recursion), NOT what LOCK-8 wants — LOCK-8 wants precise
  inference over a RUNTIME combinator, the middle path parsebox overshoots.
- **anuraghazra/typelevel-parser** — the same limit-pushing experiment; a cautionary tale
  on tsc compile-time blowup.

**The winning mechanism for PT-E2** is **variadic tuple types** (TS 4.0+,
https://github.com/microsoft/TypeScript/pull/39094;
https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-0.html): generic
spreads in tuple syntax + mapped tuple types let `all(pa, pb, pc)` infer
`Parser<[A, B, C]>` from `ReadonlyArray<Parser<unknown>>` via `infer`. This is the exact
tool PT-3.W3 names (types.ts + `infer V` through tightened generics; PT-3.md:305-333). The
known trap: **"Incorrect inference in some situations involving mapped tuple types and
variadic tuple types"** (https://github.com/microsoft/TypeScript/issues/40842) — variadic
elements stay generic while non-variadic map eagerly; the arity-unrolled arms
(leaf.ts:39-57, 183-251) must keep positional inference or fall back to the homogeneous
`Parser<T>[]` case. PT-3.W3's plan to keep the unrolled arms and add precise positional
types is the correct hedge against #40842.

**What LOST, universally, in the typed-combinator space**: chasing full type-level totality
(parsebox extreme) trades runtime win for tsc-compile-time collapse; and the `Parser<any>`
"escape hatch" (parsimmon/arcsecond) is adopted precisely because heterogeneous variadic
inference was historically impossible pre-4.0 — it is now curable but only with the
variadic-tuple discipline, and #40842 says the cure is not free of edge cases.

---

## 3. WHAT LOST HERE — the measured refutations our evidence base imposes

These are LAW for LOCK-8 (Constitution iv; SPEC §0; parse-that native ledger). A design
that relitigates them is dead on arrival.

### 3.1 THE TAGGED-UNION / VM-SWITCH REFUTATION (the big one for 8C/4C)
`parse-that/docs/future-research.md:76-109` — the `SpanParser` tagged-union hypothesis: a
discriminated union mirroring the Rust `SpanParser` enum, `callSpan()` dispatching via a
`switch` on a numeric tag, premised on V8 lowering it to a jump table and escaping the
megamorphic IC. **MEASURED + FALSIFIED (A.W3, `span-dispatch.bench.ts`)**: the tagged
switch-dispatch is **~10-14% SLOWER** than the closure span lane on an 8-arm CSS-value
alt-token scan, reproduced across three workloads, an **independent adversarial re-run
measured −14%** (future-research.md:83-90). "V8's monomorphic-per-call-site closure
dispatch with inlining beats the recursive switch — the OPPOSITE of the Rust
`enum`-vs-`Box<dyn>` regime that motivated this item. The jump-table *speedup* premise does
**not** transfer from Rust to V8/TS." **KILLED** in Tranche B.W0 (P-inv-28,
future-research.md:92-99). Standing lesson recorded verbatim (future-research.md:106-109):
"do NOT re-attempt the runtime-switch SpanParser approach on V8 without a fundamentally
different encoding (e.g., a flat array-of-ops with a generated specialized dispatcher — not
a recursive runtime switch)."

**Direct constraint on LOCK-8C and the LOCK-4C VM-core family**: any "shared bytecode VM /
one instruction set / tagged runtime dispatch shared rust+ts" formulation that lowers to a
runtime `switch` on V8 has ALREADY LOST here by measurement. Layout isomorphism to Rust
that forces a runtime tag-dispatch onto V8 is a *measured regression*, not a mirror. The
only surviving door is a *generated specialized dispatcher* (build-time monomorphized
chains — LOCK-4B's "build-time fuser"), which is a DIFFERENT thing from a runtime VM.

### 3.2 THE MEGAMORPHIC IC IS ARCHITECTURALLY LOAD-BEARING (unfixable-by-tag)
`this.parser(state)` is THE megamorphic IC of the engine (R4 §6c: parser.ts:53,82,88,131;
leaf.ts:44,49,62,140). V8's megamorphic threshold is 4; alternation flows 6+ closure
targets through one call site (perf-optimization-ts.md:201-207). FIRST-set dispatch reduces
the *number* of calls, not the polymorphism (perf-optimization-ts.md:338). The "full fix"
everyone reaches for is the tagged-union — and §3.1 proves it LOSES on V8. So: the
megamorphic IC is a permanent property of the closure-combinator architecture on V8; the
lever is to *avoid the dispatch* (FIRST-set/dispatch LUT, arity unrolling, inlined `wrap`),
never to *tag* it. (External corroboration of the mechanism:
https://www.builder.io/blog/monomorphic-javascript, web.dev/articles/speed-v8 — monomorphic
IC hits the fast path; megamorphic falls off it.)

### 3.3 WARM BENCHES ARE DISINFORMATION; PAIRED-INTERLEAVE IS MANDATORY
SPEC §0 (Constitution iv); PROVE-11 §3.1 — the naive `vitest bench` cross-run is
load-confounded (pristine at loadavg 12, modified at loadavg 27; raw hz dips on light lanes
while json rises). The certified method is **single-build-per-process paired interleaving**
(A,B,A,B…, median-of-13-batches ×≥300k ops, N≥12 rounds, per-round loadavg). PROVE-11
VERIFY halved the prover's dispatch claim (+10.2%→~+5%) using exactly this — the
two-arm-window asymmetry is the classic self-deception (SPEC §0/09). Any LOCK-8 perf claim
that rides `vitest bench` alone is refused as evidence.

### 3.4 THE HONEST-NUMBER LAW, INSTANTIATED
PROVE-11 dispatch lane: prover claimed +10.2%/+11.3%, VERIFY measured **~+5%** across 24
rounds in two windows, misses the ≥10% gate → **CONFIRMED_DELTA** (§0 table row 11;
VERIFY.md:28-46). PT-3 carries dispatch at **~+5% no-regression, NOT gated ≥10%**
everywhere (PT-3.md:66-70). The fused/unfused/alloc lanes reproduced at/above claim
(then-chain +13.6%, fused +15.6%, alloc −60.1% exact). Inflating the dispatch figure back
toward the prover's is the exact self-deception §0/09 outlaws (PT-3.md R1 risk:514-518).

### 3.5 DO NOT ZONE-PARTITION parse-that — the subpath map IS the zone map
future-research.md:206-208 (r6 #6): "the subpath export map (`. / core / diagnostics /
packrat / utils`) IS the zone map; splitting the ~711-LOC `parser.ts` is net-negative and
is not pursued." This CONSTRAINS the colocation edict for LOCK-8: the god-module cure
(utils.ts diagnostics split, PT-3.W2) is legitimate BECAUSE it separates two genuinely
distinct concerns AND targets the existing `./diagnostics` subpath; a blanket "split the
big file" over-reach is refused by measurement (net-negative). The nuance is load-bearing:
`utils.ts` split YES (two concerns), `parser.ts` split NO (one concern, the combinator
algebra).

---

## 4. PACKRAT — the settled adjudication (what won/lost, and the literature)

parse-that's packrat is **OPT-IN**, `PACKRAT_ARMED` latched, a true no-op until the first
`memoize()` construction (packrat.ts:156/290; R4 §6g). The default LL(1)/recursive-descent
path pays NOTHING. This is the CORRECT design and matches the literature exactly:
- Bryan Ford's original (https://bford.info/pub/lang/packrat-icfp02.pdf) is
  simple/powerful/lazy/linear — but **"tends to be slower than many other linear-time
  parsing techniques"** and carries **"significant memory overhead"** (Wikipedia
  Packrat_parser; the IJISAE optimization-strategies paper).
- **"Memoizing all PEs makes performance WORSE"** — selective memoization is the win (the
  Java-grammar result, IJISAE / arXiv 1507.08610). parse-that's arm-on-demand latch IS
  selective memoization at the coarsest grain.
- **GC penalizes a packrat parser more than an LR parser as source size grows** (arXiv
  search corpus) — on V8, where GC is already the enemy (the whole allocation-discipline
  thesis), a default-on packrat would be a catastrophe. The latch is the right call.

The native ledger's HONEST framing (future-research.md:214-220): the WDM/LR
left-recursion tier is free **only for memoize-free processes** (the latch never disarms);
there is **deliberately NO throughput-% gate on the arming** (a "workload-dependent flake
trap"); the retained-heap flat clause is the only born-RED perf oracle. LOCK-8 must
preserve: (a) opt-in latch, (b) armed-vs-unarmed disclosed in EVERY perf table (PT-3.md
§1.2, W4), (c) no throughput gate on arming. The tier's KEEP is provisional pending a
bbnf-lang LR consumer (future-research.md:217-220) — Y.13 consumer-less-substrate applies:
if no LR consumer materializes, a future cut retires the tier.

---

## 5. THE DIAGNOSTICS PLANE (PT-E1) — what exists, the trap

The defect (R4 §5, PT-3.W2): diagnostics gated by a **module-global mutable** flag
`diagnosticsEnabled` (utils.ts:6) + a **module-global collected-diagnostics buffer**
(utils.ts:138-146). Two concurrent/nested parses cross-contaminate — an inner parse that
enables diagnostics leaks its flag+buffer into the outer. `ParseDiagnostic.expected` is
structurally DEAD on the default path (the global flag is off). The cure: per-parse scope
hung off the parse epoch (parseState already opens a packrat epoch in try/finally,
parser.ts:34-49 — the DiagnosticScope opens/closes in the SAME try/finally). The trap named
in the plan (PT-3.md R2:519-523): the scope must NOT add a per-parse allocation on the
diagnostics-off path — a `null`-or-field read, no allocation when off, or it regresses below
the W1 floor. This is the classic "optional plane must be shape-preserving" trap — matches
PROVE-11's own lesson (optional arrays must be `undefined`-at-birth native fields so the
hidden class is fixed from birth, never transitions; PROVE-11 §2). Comparable systems
(ts-parsec's error-handling focus) treat diagnostics as first-class *return* data, not a
global latch — the module-global latch is a legacy JS smell parse-that is the last to carry.

---

## 6. THE V8 DISCIPLINE — the pinned invariants (8A territory)

The governing surfaces (R4 §6):
- **`ParserState` shape** (state.ts:36-53): 8 mutable fields incl. `value:T` (holds
  string|array|object|undefined across every parse) — inherently polymorphic in the value
  slot; the `unsafeSetValue`/`unsafeCall` choke points (state.ts:87-99) are the single
  megamorphic-WRITE sites, deliberately centralized.
- **`mapState`'s `Object.create(state)`** per invocation (parser.ts:177-180) — a
  prototype-chain view object, the ONE documented deopt-risk allocation; PT-3.W4 pins it as
  the *only* prototype-chain object with an oracle.
- **PROVE-11's certified lever**: `suggestions`/`secondarySpans` were eager `= []`
  initializers → 2 array allocs per `new ParserState` + 2 more per `mergeErrorState`
  furthest-advance (fires constantly during backtracking on the SUCCESS path). Made
  symmetric with the pre-existing lazy `expected` (undefined-at-birth native field, `??= []`
  under the diagnostics guard). Emitted-JS census 6→0 eager sites (VERIFY confirmed 6→0
  exactly); fused alloc −60.1% (VERIFY exact). Hidden class identical in both arms —
  `%HaveSameMap` TRUE is the W4 oracle (PT-3.md:360-401). This is the template: **optional
  planes are undefined-at-birth native fields, never eager initializers, never mid-parse
  hidden-class transitions.**

External corroboration of the discipline: V8 unboxes numbers to raw registers when shapes
are stable (web.dev/articles/speed-v8); hidden-class transitions on optional-field
assignment are the documented churn source
(https://medium.com/@yashschandra/hidden-v8-optimizations-hidden-classes-and-inline-caching,
https://dev.to/maxprilutskiy/hidden-classes-...). parse-that already applies this; the
LOCK-8 job is to PIN it with `%HaveSameMap` / `--allow-natives-syntax` oracles (PT-3.W4),
not to re-derive it.

---

## 7. THE CONSUMER CONTRACT (8D territory) — the constraints

Three live gates (R4 §7):
- **value.js** 3.1.0, dep `^1.0.0` (locked 1.0.0); imports `Parser ParserState all any
  dispatch regex string whitespace mergeErrorState` (9 sites); 59 `all()` + `dispatch()` on
  the CSS function-name bucket. Suite `npm test` (vitest, jsdom).
- **latex-paper** 0.2.1, dep **`^0.7.1`** (resolves `<0.8.0`) — **VERSION SKEW**: predates
  the 1.0.0 breaking cut (`*Span` excision S.H2, `chain()` C-16). A 1.1.0 bump is an
  untested 0.7→1.1 jump. Imports only `Parser ParserState regex string any` (7 sites, all
  still exported). PT-3.W0 dry-runs the 0.7→1.0 jump *alone* to isolate which cut owns any
  break before 1.1.0 is blamed (PT-3.md R4:529-532).
- **keyframes.js** 6.0.0 — **no direct dep**, transitive via value.js; PT-E1 downstream
  target is its `ResolvedKeyframes.diagnostics`. Path is `keyframes.js`, not `keyframes`.

The API-surface facts that constrain: `*Span` combinator family EXCISED in 1.0.0
(`proof:no-span-surface` guards reintroduction — R4 §8; do not resurrect). 1.0.0 semver cut
= commits 7eab78c (S.H4) + 043c4d1 (S.H2). Eight proof gates (`proof:all`: manifest,
no-css-surface, subpath, packrat-{cross-input,reentrant,large-offset,armed},
no-span-surface, no-dead-combinator, perf) — node scripts, NOT cargo/vitest (R4 §4). The
`proof:perf` clause-C is a CROSS-MACHINE absolute-ns baseline (rebuild via
`UPDATE_PERF_BASELINE=1`) — VERIFY §3 established it is machine-relative, not a regression;
a LOCK-8 design that treats it as a hard perf gate mis-reads it.

**The 8D thesis** (design backward from consumers) is well-founded: value.js's 59 `all()`
sites are the real inference stress test for PT-E2 (a tightening that breaks them is a
semver escalation minor→major, decided not assumed — PT-3.md R3:524-528); keyframes'
`diagnostics` is the real consumer for PT-E1's per-parse scope (Y.13 — a diagnostics scope
with no consumer is forbidden substrate).

---

## 8. TRAPS NAMED (for the synthesis fleet)

1. **The Rust→V8 transfer fallacy** (the cardinal trap). Enum/jump-table/VM-dispatch wins
   in Rust; it LOSES on V8 by −14% measured (§3.1). Any 8C "isomorphic mirror" or 4C
   "shared VM" that lowers to runtime tag-dispatch on V8 is a measured regression. Isomorphism
   to a reasonable point (Constitution iii) ENDS where V8's inlining beats a tag — and that
   boundary is measured, already, at the SpanParser death. The lawful mirror is
   *build-time-generated specialized dispatchers* (LOCK-4B), not a runtime VM (LOCK-4C on V8).
2. **The type-level overshoot**. parsebox proves full type-level parsing is possible and
   proves it is a tsc compile-time monster. PT-E2 wants precise inference over a RUNTIME
   combinator (variadic tuples + `infer`), not a type-level parser. Stay in the middle.
3. **Variadic-tuple inference edge cases** (TS #40842) — mapped+variadic tuple interaction
   is buggy at the margins; keep the arity-unrolled arms as the precise-positional path,
   general arm as homogeneous fallback.
4. **The optional-plane hidden-class churn**. Any new optional field (diagnostics scope,
   packrat cell, span) MUST be undefined-at-birth native field, never eager `= []`, never
   assigned-first mid-parse (PROVE-11 §2; §6). A per-parse scope object allocated on the
   diagnostics-OFF path regresses the hot path (PT-3.md R2).
5. **The zone-partition over-reach**. Splitting `parser.ts` is net-negative (r6 #6, §3.5);
   the god-module cure is bounded to utils.ts's two-concern split, riding the wave that
   already edits diagnostics. Colocation edict ≠ "split every big file."
6. **The warm-bench / single-window self-deception** (§3.3-3.4). Every number paired,
   in-process, N≥12, two windows; dispatch stays honest at ~+5%; `vitest bench` alone is not
   evidence.
7. **The default-on packrat catastrophe**. Memoizing-all hurts (literature §4); GC penalty
   grows with input on V8; the opt-in latch with no throughput gate is settled — do not
   re-open with a "smart auto-arm."
8. **The latex-paper skew mislabel** (§7). A 1.0.0 break surfacing at the 1.1.0 bump gets
   mis-blamed on 1.1.0; W0 must dry-run 0.7→1.0 alone first.

---

## 9. SUMMARY OF CONSTRAINTS THE EVIDENCE BASE ALREADY IMPOSES

| # | Constraint | Source |
|---|---|---|
| C1 | Runtime tag/switch/VM dispatch LOSES on V8 (−14% measured); only build-time-generated specialized dispatchers are lawful | future-research.md:76-109 (SpanParser KILL) |
| C2 | Megamorphic IC is permanent; avoid the dispatch (FIRST-set/unroll), never tag it | perf-optimization-ts.md:201-207,338; R4 §6c |
| C3 | Optional planes = undefined-at-birth native fields; no eager `=[]`, no mid-parse transition | PROVE-11 §2; VERIFY §2 |
| C4 | Dispatch lane is ~+5% honest, NOT ≥10%; every number paired-in-process, N≥12, 2 windows | PROVE-11 VERIFY; SPEC §0/11; PT-3 §1.3 |
| C5 | Packrat opt-in latch, no throughput gate, armed/unarmed disclosed everywhere; KEEP is provisional (LR consumer) | R4 §6g; future-research.md:214-220 |
| C6 | PT-E2 inference via variadic tuples + `infer`, zero runtime cost (dist byte-identical); mind TS #40842 | PT-3.W3; TS 4.0 release notes |
| C7 | Don't split `parser.ts` (net-negative); god-module cure bounded to utils.ts two-concern split | future-research.md:206-208; PT-3.W2 |
| C8 | `*Span` stays excised (proof:no-span-surface); 8 proof gates are node scripts | R4 §8; R4 §4 |
| C9 | latex-paper `^0.7.1` skew: 1.1.0 is an untested 0.7→1.1 jump; dry-run 0.7→1.0 first | R4 §7; PT-3.W0 |
| C10 | Type plane costs zero at runtime (Constitution v) — TS inference is compile-time, mirrors the Rust zero-cost law | Constitution v; PT-3.W3 |

---

## 10. SOURCES

Internal (binding): PORTFOLIO.md (Constitution + LOCK-8 task); SPEC.md §0/§1/§2b; INDEX.md;
R4-parse-that-ts.md; PROVE-11 REPORT.md + VERIFY.md; PT-3.md.
parse-that native (READ-ONLY prior art): `docs/future-research.md` (SpanParser
falsification+KILL, r6 constraints, packrat framing); `docs/perf-optimization-ts.md`
(the 746→5480 arc, megamorphic-IC finding, FIRST-set dispatch); `docs/tranches/A/`
(VALUEJS-PT-E letter).
External: sinclairzx81/parsebox (github); microsoft/ts-parsec (github); gcanti/parser-ts
(github); TS 4.0 variadic tuple types (typescriptlang.org release notes; microsoft/TypeScript
PR#39094); TS issue #40842 (mapped+variadic inference bug); Ford, Packrat Parsing ICFP02
(bford.info); Wikipedia Packrat_parser; IJISAE packrat-optimization; web.dev/articles/speed-v8;
builder.io/blog/monomorphic-javascript; V8 hidden-class/IC explainers (yashschandra, maxprilutskiy).
