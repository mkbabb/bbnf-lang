# LOCK-8 — THE TS BACKBONE PLANE — PASS-2 PRIOR-ART RESEARCH

*Prior-art sweep (web + codebase) for the parse-that/typescript backbone: what exists, what
won and lost elsewhere for this problem class, the constraints our own evidence base already
imposes, and the traps named in comparable systems. Citations: `path:line` for code,
inline URLs for web. Constitution-bound; no family allegiance. Read-only against parse-that.*

---

## 0. The problem class, decomposed

LOCK-8 spans four sub-problems, each with a distinct prior-art literature:
1. **V8 hot-shape discipline** (8A) — hidden classes, inline caches, megamorphic dispatch,
   allocation, monomorphization, and the Node-26 Maglev/TurboFan tiering the numbers ride.
2. **Type-level inference** (8B) — TS's own inference story: `ExtractValue`/tuple inference,
   the `Parser<any>` leaks, and TS2589 instantiation budgets. A CHECKER problem, not a JIT one.
3. **Correspondence** (8C) — the TS face of the rust/ts isomorphism (defers to LOCK-6).
4. **Consumer contract** (8D) — per-parse diagnostics scoping, semver, the three-repo suites.

The single most load-bearing prior-art fact for LOCK-8 lives in parse-that's own record and
is a MEASURED REFUTATION (§2 below): the "escape the megamorphic IC via a tagged-union +
runtime switch" move — the obvious 8A/8C/4C answer — was implemented, measured, and killed on
V8 as *slower*. That refutation is law and constrains every route.

---

## 1. V8 hot-shape prior art (8A) — external + our measured record

### 1.1 The IC threshold facts (external, settled)
V8's inline caches are monomorphic (1 shape → map-check + direct offset load), polymorphic
(2–4 shapes → linear check chain), or **megamorphic (>4 shapes → global hashtable fallback,
TurboFan refuses to optimize the containing function; property/call degrades ~10–50×)**.
Sources: [An Inline Caching Design for TypeScript Performance
(ACM)](https://dl.acm.org/doi/pdf/10.1145/3428242),
[v8.dev/blog/maglev](https://v8.dev/blog/maglev),
[thenodebook V8 tiers/shapes](https://www.thenodebook.com/node-arch/v8-engine-intro). The
4-shape cliff is the governing constant for the closure-per-combinator pattern.

### 1.2 Our own engine already hit and documented the megamorphic wall
parse-that's published optimization narrative names it exactly:
`/Users/mkbabb/Programming/parse-that/docs/perf-optimization-ts.md:201-207` — *"Every
`Parser` stores its parse function in `.parser`. Each combinator creates a unique closure
shape… `any()` sees 6+ targets… V8's megamorphic threshold is 4… `Builtin:
CallFunction_ReceiverIsNotNullOrUndefined` appeared at 5.9% of total CPU… No inlining, just a
hashtable lookup on every call. This is structural to the closure-based combinator pattern.
The FIRST-set dispatch table mitigates it (fewer calls, not fewer targets), but doesn't solve
it."* This is R4 §6c's "central megamorphic call site" (`this.parser(state)`,
`parser.ts:53,82,88`; `leaf.ts:44,49,62,140`) with a measured CPU share attached. **8A cannot
claim to eliminate this IC by re-monomorphizing `.parser`** — the closure-shape diversity is
inherent; the historical wins reduced call COUNT (FIRST-set/`dispatch()` `Int8Array(128)`,
`leaf.ts:100-151`), never the target set.

### 1.3 The allocation lever (PROVE-11) — the substrate LOCK-8 W1 lands
The certified §0/11 lever is lazy diagnostic-array allocation: eager `= []` field initializers
churned 6 alloc sites (2 ctor + 2 `mergeErrorState` per-backtrack + 2 `resetErrorState`) →
0; fields declared `undefined` so **the hidden class is fixed from birth and never
transitions** (PROVE-11 REPORT §2, census 6→0; VERIFY confirmed exact). This is the canonical
V8 idiom (declare-at-construction, populate-lazily) and it is already how `expected?` worked
(`state.ts:43`) — PROVE-11 made `suggestions`/`secondarySpans` symmetric. Directive 8A's
"same-map probe" is the right oracle (`%HaveSameMap`); the prior art says it will pass, because
`undefined` and a later-assigned array share a map only if the field is declared at
construction — which is the whole point of the patch.

### 1.4 The prototype-chain deopt surface (8A, un-eliminated)
`mapState` builds `Object.create(state)` per invocation (`parser.ts:177-180`, R4 §6b) — the
**only** prototype-chain object in the engine. Prototype-chain objects have slower property
access and can deopt callers ([Hidden classes / IC
overview](https://medium.com/@yashschandra/hidden-v8-optimizations-hidden-classes-and-inline-caching-736a09c2e9eb)).
Directive 8A is right to demand the mechanical read-surface enumeration across value.js /
latex-paper / keyframes (not inspection) before proposing a fixed-shape view: the risk is that
a flat view drops a field a real `.mapState` consumer reads. No prior art removes this object
for free; a fixed struct view must be proven behavior-preserving against a RUN consumer.

### 1.5 Node 26 tiering — the tier every number rides (8A)
V8's tiers: Ignition → Sparkplug → **Maglev (mid-tier, M117; SSA CFG, FeedbackVector-driven,
LIMITED inlining of small hot callees)** → TurboFan (top, Sea-of-Nodes).
[v8.dev/blog/maglev](https://v8.dev/blog/maglev),
[Turbolev (Maglev IR becomes TF's frontend)](https://blog.seokho.dev/development/2025/07/15/V8-Expanding-To-Turbolev.html),
[Profile-Guided Tiering](https://community.intel.com/t5/Blogs/Tech-Innovation/Client/Profile-Guided-Tiering-in-the-V8-JavaScript-Engine/post/1679340).
**Consequence for the arity-unroll (`leaf.ts:39-57,183-251`):** Maglev's inlining is weaker
than TurboFan's, and short-lived consumer test processes (value.js `vitest`) spend proportionally
more time at Maglev/Sparkplug than a long benchmark loop that reaches TurboFan. So the unrolled
arms plausibly matter MORE at the tier the real consumer suites actually run — but that must be
MEASURED at the tier, not asserted. Directive 8A's "inline under BOTH TurboFan AND Maglev"
demand is exactly the right falsifier; the honest report must disclose which tier each figure
was captured at (the standing profiling law's TS instantiation).

---

## 2. THE REFUTATION THAT IS LAW: runtime tag+switch dispatch loses on V8 (8A/8C/4C)

The obvious escape from §1.2's megamorphic IC — replace closure dispatch with a tagged union +
`switch` (the Rust `enum`-beats-`Box<dyn>` move) — was **implemented, measured, falsified, and
KILLED** in parse-that. This is the settled adjudication most binding on LOCK-8:

`/Users/mkbabb/Programming/parse-that/docs/future-research.md:71-109` — the `SpanParser`
tagged-union with `callSpan()` switch-dispatch was **~10–14% SLOWER** than the closure span
lane on an 8-arm CSS-value alt-token scan, reproduced across three workloads, with an
independent adversarial re-run at −14%. *"V8's monomorphic-per-call-site closure dispatch with
inlining beats the recursive switch — the OPPOSITE of the Rust `enum`-vs-`Box<dyn>` regime that
motivated this item. The jump-table speedup premise does not transfer from Rust to V8/TS."*
KILLED at Tranche B.W0 (P-inv-28) with zero in-realm consumers. perf-optimization-ts.md:207
had floated exactly this ("a future architecture using tagged unions + switch dispatch could
eliminate it entirely") — §7 then falsified it.

**The surviving carve-out (verbatim, future-research.md:107-109):** do NOT re-attempt the
runtime-switch approach on V8 *"without a fundamentally different encoding (e.g., a flat
array-of-ops with a generated specialized dispatcher — not a recursive runtime switch)."*

Implications the gestalt must carry:
- Any LOCK-8 8A/8C proposal to "escape the megamorphic IC via a runtime discriminant" is
  **dead by measured refutation** unless it is a *build-time-generated specialized* dispatcher
  (which is LOCK-4B's build-time fuser / a generated flat op-array, not a runtime `switch`).
- This directly constrains **LOCK-4C (VM-core)** on its TS face: a bytecode VM interpreted by a
  runtime `switch(op)` is the exact shape §7 refuted. LOCK-4C survives on TS only if the
  instruction dispatch is generated-and-specialized per grammar, not a single interpreter loop.
- It reinforces the constitution's family split: the win is in the COMPILER (fewer/monomorphic
  call sites, FIRST-set), never in a cleverer runtime tag.

---

## 3. Type-level inference prior art (8B) — the CHECKER problem

### 3.1 The field, and where PT-E2 sits within it
Type-level TS parsing/inference is a mature niche: [microsoft/ts-parsec](https://github.com/microsoft/ts-parsec)
(inferred AST types, error-handling focus), [sinclairzx81/parsebox](https://github.com/sinclairzx81/parsebox)
(parser combinators *in the type system* — dual runtime + static generic environments),
[spakhm/ts-parsec](https://www.spakhm.com/ts-parsec), [parjs](https://github.com/GregRos/parjs),
and the TS type-magic canon ["Into the Chamber of
Secrets"](https://herringtondarkholme.github.io/2023/04/30/typescript-magic/). **Critical
scoping distinction the gestalt must hold:** parsebox et al. do full type-level PARSING (parse a
string literal type into an AST type) — an enormous instantiation budget. **PT-E2 is the far
lighter problem: INPUT-side inference tightening** — make `all(a,b)` infer `Parser<[A,B]>` and
kill the `Parser<any>` INPUT leaks (`parser.ts:14-16`, `leaf.ts:28,154,179`, `state.ts:79`;
R4 §5). It is `infer`-through-a-tuple, not a type-level interpreter. Do not import parsebox's
budget concerns wholesale; the risk profile is an order of magnitude smaller.

### 3.2 The TS2589 wall (external, exact)
TS2589 fires at instantiation limits: **instantiationDepth** (~mapped-type recursion) and
**instantiationCount (hard cap 5,000,000)**; recursive-descent-shaped types blow it easily.
Sources: [Chamber of Secrets](https://medium.com/@hchan_nvim/into-the-chamber-of-secrets-break-through-the-limits-of-typescript-3532c6dd080),
the TS2589 discussions above. Mitigations the prior art proves: **tail-recursion elimination in
conditional types (TS 4.5+)**, tuple accumulation over deep nesting, and **reference-based
recursion** (parsebox's string `Ref()`) to break circular type dependencies. For PT-E2 the
governing empirical question (directive 8B) is: does `Parser<DropUndefined<Values<T>>>` stay
under TS2589 across **value.js's 59 real `all()` sites** — measured as TS2589 count +
instantiationCount + check-time on THAT exact signature, disclosed. The prior art says the
tuple-`infer` form is cheap; the `DropUndefined` filter is the marginal cost to bound.

### 3.3 The `all(a, opt(b), c)` variadic-length trap (8B)
An `opt(b)` inside `all` makes the runtime tuple length vary with `b`'s presence — a genuine
soundness fork. parsebox's prior art is directly on point: its `Optional` combinator returns
**single-element-tuple | empty-tuple** for clean type-level discrimination without null/undefined
([parsebox](https://github.com/sinclairzx81/parsebox)). That is the "type-level filter +
documented widening" branch of directive 8B's trichotomy. The decision must be a RUNTIME
DIFFERENTIAL against value.js's actual opt-in-`all` sites (confirm they exist first) — not an
a-priori choice — exactly as the directive demands.

### 3.4 The zero-cost invariant (constitution v) — inference is checker-only
Any leak that closes only by inserting a runtime cast or altering emitted JS is a defect (PT-3
W3 failure semantics). Prior art / method: prove emit-identity via a `dist/` diff, and prove it
on **parse-that's real `dist/` toolchain built both ways** (directive 8B), not a single-file
transpile proxy — the two-engine caution (JIT vs checker) is a real trap: a checker change that
looks free can perturb emit if it touches a value-level default.

---

## 4. Per-parse scope / reentrancy prior art (8D, W2) — the module-global sin

### 4.1 The established answer exists in comparable libraries
The defect (R4 §5): diagnostics gated by a **module-global** `diagnosticsEnabled`
(`utils.ts:6`) + a module-global collected buffer (`utils.ts:138-146`) — nested/concurrent
parses cross-contaminate. The canonical prior-art cure is well-trodden:
- **parjs** creates a *unique mutable user-state object every `.parse()` invocation*, propagated
  through the parse ([parjs](https://github.com/GregRos/parjs)) — scope hangs off the parse
  epoch, never a module global. This is precisely PT-3 W2's design (scope on the parse epoch's
  try/finally, `parser.ts:34-49`).
- **arcsecond** `.run`/`.fork` return per-invocation result records
  ([arcsecond](https://github.com/francisrstokes/arcsecond)); **parsimmon** threads state through
  the parse ([parsimmon](https://github.com/jneen/parsimmon)). None keep a mutable module-global
  flag governing observable output.
Haskell's parsec-lineage guarantee — *"the parse stream is used by only one thread"* — is the
single-threaded-assumption that JS shares, but that assumption does NOT excuse a module-global
that leaks across *nested* parses in the same thread; nesting is the W2 born-RED oracle.

### 4.2 We already fixed the sibling module-global (packrat) — precedent
parse-that's packrat re-entrancy was the analogous defect and is CLOSED: DQ-1 / PT-Q1,
`proof:packrat-reentrant`, verified landed in 0.13.0 (future-research.md:222). So the
per-parse-scope discipline is already partly established in the engine (`packratEnter`/
`packratExit` epoch save/restore, `packrat.ts:216-250`); **diagnostics is the last surviving
module-global**, and the packrat epoch is the exact seam W2 rides. Prior art internal to the
repo says the shape works.

### 4.3 The zero-cost-when-off trap (directive 8D, PT-3 R2)
The named counterexample: W2 threads a scope object allocated per `.parse()` and regresses the
diagnostics-off path below the W1 floor. Cure is prior-art-clean: a `null`-or-single-field read
on the default path, no allocation when off — and re-run the W1 paired harness to hold the floor.

---

## 5. Constraints our own evidence base already imposes (SPEC §0 + PROVE-11)

### 5.1 Which §0 laws bind TS (PT-3 §1.3, verified)
TS is NEON-free → the digit-scan-SIMD / block-scheduling / tape-navigation / shim-PORT laws do
NOT bind by subject. The laws that DO bind LOCK-8, verbatim:
- **The honest-number law (no shim perf claims).** PROVE-11's dispatch claim was HALVED by the
  verify fleet: +10.2%/+11.3% → **~+5%** across 24 rounds in two windows (VERIFY §1). The
  dispatch lane is carried at ~+5% *no-regression*, NOT a ≥10% gate. Inflating it is the exact
  §0/09 self-deception. (SPEC §0 row 11: CONFIRMED_DELTA; then-chain +13.6%, fused +15.6%
  CONFIRMED, alloc −60.1% exact — those two DO clear ≥10%.)
- **Warm benches forbidden.** The naive `vitest bench` cross-run is load-confounded (PROVE-11
  §3.1: pristine at loadavg 12, modified at 27) and is refused as evidence. Single-build-per-
  process paired interleaving (A,B,A,B…) is the certified method.
- **Paired interleaving mandatory even for "obvious" wins** (§0/09 standing lesson, the
  portable_simd shim refutation): *"two-arm window asymmetry is the classic self-deception."*
  Every LOCK-8 ratio is paired-in-one-process.

### 5.2 The cross-language tape number is fenced (8C, §0 row 14 / PROVE-14)
Stop-early tape get in TS is 4.4–4.6× vs `JSON.parse`; **full-walk ~0.91× — the 2× is
honestly unmet, native parse is near-optimal for full materialization, a MEASURED non-goal**
(SPEC §0/14). Directive 8C's guard is correct: keep the 4.4× stop-early figure OUT of any
wasm-boundary claim until export/boundary-crossing cost is measured *separately*. The tape
decoder (GN-4.W4 seed) imports no combinators — it is complementary to, not gating on, the
combinator backbone (PT-3 §2).

### 5.3 The armed/unarmed disclosure law (PLANE-WIDE, §0 method)
`PACKRAT_ARMED` is a permanent latch, opt-in, true no-op until first `memoize()`
(`packrat.ts:156,290`). Recorded decision (future-research.md:214-220): **NO throughput-% gate
on arming** (a workload-dependent flake trap); the retained-heap flat clause is the only born-RED
perf oracle. Every LOCK-8 perf table MUST state armed vs unarmed. The numeric memo key
(`id<<20 | offset&0xFFFFF`, later `getCijKey` float64-safe `id*MEMO_OFFSET_SPAN+offset`,
`packrat.ts:71-100`) is the V8-fit shape-stable form — prior-art-clean (avoids per-lookup string
alloc).

---

## 6. Traps named in comparable systems (the negative canon)

1. **The tagged-union switch (§2)** — the single biggest trap; falsified on V8, KILLED. Any
   VM/discriminant route inherits its refutation unless build-time-generated-specialized.
2. **Fair-benchmark fraud** (perf-optimization-ts.md:19-27) — Chevrotain measured 2× too fast in
   recognizer-only mode (`outputCst:false`) and used `.slice(1,-1)` instead of `JSON.parse`
   (wrong on escaped strings). LAW: every competitor must produce output `deepEqual` to the
   oracle before timing. Any LOCK-8 competitor-matrix claim (json-comprehensive.bench.ts) must
   carry this.
3. **`RegExp.exec()` allocation** (perf-optimization-ts.md:178-199) — `exec()` returns a
   `RegExpMatchArray` (1,380 allocs/parse, ~30% of allocation); cured by sticky `test()` +
   `substring()`. Already solved (`leaf.ts:344-358`); do-not-regress.
4. **Identity-map allocation** (perf-optimization-ts.md:264-273) — a no-op `.map(([k,v])=>[k,v])`
   cost 720 array allocs/parse. The general trap: value-preserving codegen must never emit
   round-trip re-construction.
5. **Nested inline-storage blowup** (future-research.md:261-271) — SmallVec-in-SmallVec → ~4.4KB
   stack `CssNode`, 5× regression. A Rust lesson, but the colocation caution transfers: don't
   nest per-element specialization without measuring the aggregate object size.
6. **Zero-progress infinite loop** (perf-optimization-ts.md:274-283) — `many`/`sepBy` OOM when
   the inner parser succeeds without consuming; the `offset===savedOffset ⇒ break` guard is PEG's
   `*` semantics. Any packrat/left-recursion tier work must preserve it.
7. **Checked-in bundle false-green** (directive 8D) — value.js BUNDLES parse-that
   (`libraryExternal=[]`); a crater harness over the committed bundle proves nothing. Mandatory
   `dist/` rebuild step before the consumer suite is trusted.
8. **The latex-paper `^0.7.1` version-skew silent break** (PT-3 R4, R4 §7) — pinned pre-1.0.0,
   so a 1.1.0 bump surfaces a 1.0.0 break (`*Span` excision S.H2, `chain()` C-16) mislabeled as
   a 1.1.0 one. Cure: the `0.7→1.0→1.1` three-step attribution matrix (directive 8D) isolates
   which cut owns each break before 1.1.0 is blamed.

---

## 7. Adjacencies & ownership boundaries (scope-limiting the residue)

- **8C ⇄ LOCK-6.** The single-spec→two-artifact generator, the behavioral differential oracle,
  and the isomorphism linter are LOCK-6 mechanisms (6A/6B/6D). 8C's genuine parse-that residue is
  the *three named structural divergences* (directive 8C): SpanParser has no twin (it was killed
  both sides — §2 + S.H2), regex is Rust-bespoke-DFA vs native `RegExp` (R4 §3: **no NFA/DFA in
  `typescript/src`; the TS runtime uses native `RegExp` only** — greenfield if a bespoke TS regex
  is ever wanted), and diagnostics is behavioral-vs-mechanism. Scope 8C to only that residue;
  don't re-home LOCK-6's linter.
- **8A ⇄ LOCK-4B/4C.** The build-time fusion specializer is LOCK-4B's fuser; coordinate the
  TS-execution number, don't duplicate. The §2 refutation is the shared constraint on 4C's TS face.
- **8B ⇄ LOCK-1.** Distinct engines: LOCK-1 is the Rust HM/CSP solver; 8B is TS's own checker
  inference. Never conflate a runtime (JIT) measurement with a checker one (directive preamble).

---

## 8. One-line verdicts the gestalt can bank

- The megamorphic `.parser` IC is structural and measured (5.9% CPU); it is NOT eliminable by
  runtime re-monomorphization — only mitigated by fewer/FIRST-set call sites (compiler-side).
- The tagged-union/runtime-switch escape is **LAW-refuted on V8 (−10–14%)**; a VM/discriminant
  survives only as a build-time-generated specialized dispatcher.
- PROVE-11 lands as substrate; dispatch carried at the honest ~+5%, then/fused/alloc at their
  ≥10%/−60% CONFIRMED figures; all paired-interleaved, armed/unarmed disclosed.
- Per-parse diagnostics scoping is prior-art-standard (parjs) and the packrat epoch is its seam;
  the module-global is deleted, not flagged; zero-alloc-when-off is the born-RED floor.
- PT-E2 is INPUT-inference tightening, an order of magnitude under parsebox's type-level-parse
  budget; measure TS2589/instantiationCount on the exact `DropUndefined<Values<T>>` signature
  across value.js's 59 `all()` sites; `opt`-in-`all` resolved by runtime differential, not fiat.
- The tape 4.4× is stop-early-only and fenced from any wasm-boundary claim; full-walk is a
  measured non-goal.

## Sources (web)
- [An Inline Caching Design for TypeScript Performance (ACM)](https://dl.acm.org/doi/pdf/10.1145/3428242)
- [Maglev — V8's Fastest Optimizing JIT (v8.dev)](https://v8.dev/blog/maglev)
- [Turbolev / Maglev IR as TF frontend](https://blog.seokho.dev/development/2025/07/15/V8-Expanding-To-Turbolev.html)
- [Profile-Guided Tiering in V8 (Intel)](https://community.intel.com/t5/Blogs/Tech-Innovation/Client/Profile-Guided-Tiering-in-the-V8-JavaScript-Engine/post/1679340)
- [V8 tiers/shapes/deopt (thenodebook)](https://www.thenodebook.com/node-arch/v8-engine-intro)
- [Hidden classes & inline caching](https://medium.com/@yashschandra/hidden-v8-optimizations-hidden-classes-and-inline-caching-736a09c2e9eb)
- [microsoft/ts-parsec](https://github.com/microsoft/ts-parsec) · [sinclairzx81/parsebox](https://github.com/sinclairzx81/parsebox) · [spakhm/ts-parsec](https://www.spakhm.com/ts-parsec) · [GregRos/parjs](https://github.com/GregRos/parjs)
- [Chamber of Secrets — TS type limits / TS2589](https://herringtondarkholme.github.io/2023/04/30/typescript-magic/)
- [jneen/parsimmon](https://github.com/jneen/parsimmon) · [francisrstokes/arcsecond](https://github.com/francisrstokes/arcsecond)
</content>
</invoke>
