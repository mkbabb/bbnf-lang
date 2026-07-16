# LOCK-8 — THE TS BACKBONE PLANE — PRIOR-ART RESEARCH (pass 3)

Researcher: prior-art, pass 3. No family allegiance; registry/favored-approach withheld by
protocol. Scope = parse-that/typescript backbone: V8 discipline (8A), TYPE-LEVEL inference
(8B), Rust/TS isomorphic mirror (8C), consumer-contract-first (8D). Citations are
`path:line`, web URLs, or SPEC/PROVE artifacts.

---

## 0. What this problem class IS (framing)

The backbone is a **runtime-compiled, scannerless combinator core**: a BBNF grammar string
builds a tree of `Parser<T>` closures at construction time; the hot path threads one mutable
`ParserState` through nested `.parser(state)` indirect calls. This shape is the source of
every win AND every ceiling below. The extant parse-that campaign already took this core from
**746 → 5,480 ops/s (6.4×)** on data.json, beating Chevrotain/Peggy/Parsimmon/Nearley in
value-building mode (`parse-that/docs/perf-optimization-ts.md:304-329`). LOCK-8 is not
greenfield — it is a productionization of a measured, near-SOTA-for-its-class artifact whose
remaining headroom is well-characterized.

---

## 1. FAMILY 8A — THE SHAPE-STABLE (V8-discipline) CORE

**Prior art — settled techniques (all already applied in-tree; the "do-not-regress" floor):**
- **Mutable single-threaded `ParserState`** — the single biggest win (~4,000 heap objects/parse
  eliminated); immutable-state combinators are elegant but allocate per call
  (`perf-optimization-ts.md:53-98,335`). This is the canonical combinator-perf lesson (mirrors
  nom/chumsky's in-place cursor in Rust).
- **Zero-alloc save/restore** via inline offset locals, not `{offset,value}` objects
  (`perf-optimization-ts.md:83-98`).
- **`RegExp.test()`+`substring()` over `.exec()`** — `exec()` allocates a `RegExpMatchArray`
  per match (1,380/parse, ~30% of allocation); sticky `test()` is zero-alloc
  (`perf-optimization-ts.md:178-201`; R4 §6d leaf.ts:344-358).
- **FIRST-set `dispatch()` via `Int8Array(128)`** — turns O(n) sequential alternation into O(1)
  LL(1) lookahead; the workhorse win on number-heavy corpora
  (`perf-optimization-ts.md:118-151,210-227`; R4 §6d leaf.ts:100-151).
- **Arity-unrolled `any`/`fuseAll` (arity-2/3)** monomorphizations; classic `for` (never
  `for…of` iterator objects) on general arms (R4 §6d, leaf.ts:39-57,183-251).

**Prior art — the STRUCTURAL CEILING (what LOST, measured):** the **megamorphic IC at
alternation is structural to the closure-per-combinator pattern**. Each combinator is a
distinct closure shape; `any()` dispatching `parser.parser(state)` over 6+ alternatives blows
V8's megamorphic threshold of 4 → generic `CallFunction_ReceiverIsNotNullOrUndefined` at
**5.9% of total CPU** in profiles, no inlining
(`perf-optimization-ts.md:202-207`). The campaign's own verdict: *"FIRST-set dispatch reduces
the number of calls, not the polymorphism… A full fix would require a tagged-union +switch
architecture — a future project"* (`perf-optimization-ts.md:207,338`). This is the pivotal
prior-art finding: **8A's "monomorphic by construction / allocation-zero hot path" cannot
fully hold under the closure core**; the only measured route past it is a tagged-union/bytecode
dispatcher — which is **LOCK-4C (VM-CORE) / 4A (CLOSURE-TO-GRAPH)** territory, not a shape
tweak. 8A must either (a) scope its claim to allocation/hidden-class stability (achievable) and
concede the IC ceiling as named, or (b) explicitly reach for the VM/graph lowering, which
crosses into LOCK-4's family and must be adjudicated there, not double-homed.

**Prior art — hidden-class discipline (external, corroborating):** V8 shares one hidden class
across instances only when key-set AND insertion order match; >4 cached maps at a site = megamorphic
([V8 hidden classes/IC](https://medium.com/@yashschandra/hidden-v8-optimizations-hidden-classes-and-inline-caching-736a09c2e9eb)).
Academic prior art **STSC / "An Inline Caching Design for TypeScript Performance"** (ACM
[dl.acm.org/10.1145/3428242](https://dl.acm.org/doi/pdf/10.1145/3428242)) statically generates
non-empty hidden classes from TS static types as constructor initial maps — a direct precedent
for 8B×8A fusion (types informing shape) but requires an engine fork, so it is a *lesson*, not
a shippable lever.

**Traps (named counterexamples):**
- **Eager `= []` field initializers** cause per-parse array alloc AND are the anti-pattern
  PROVE-11 cured: making `suggestions`/`secondarySpans` **declared-undefined native fields**
  (fixed hidden class from birth, symmetric with the pre-existing lazy `expected?`) removed
  6→0 eager-alloc sites, −60% fused B/op (`PROVE-11/REPORT.md:26-71`; VERIFY CONFIRMED_DELTA).
  The trap is the *opposite* mistake — reintroducing eager init or a mid-parse optional-field
  assignment that triggers a hidden-class transition.
- **`mapState`'s `Object.create(state)` per-invocation prototype-chain view** (parser.ts:177-180,
  R4 §6b) — a deopt risk for callers; the one sanctioned prototype object. Any new
  `Object.create` on the hot path is a regression.
- **Bench self-deception:** SPEC §0/09 law — warm `vitest bench` cross-runs are load-confounded
  (PROVE-11 §3.1: pristine loadavg 12 vs modified loadavg 27 flipped light lanes). **Single
  build per process, paired A,B,A,B interleaving** is the only certified method; the VERIFY
  fleet *halved* the prover's dispatch claim (+10.2% → ~+5%) precisely by this discipline
  (`PROVE-11/VERIFY.md:28-46`). Any 8A perf number must carry the honest figure.

---

## 2. FAMILY 8B — TYPE-LEVEL TOTALITY (ExtractValue / no `Parser<any>`)

**Prior art — the landscape (what exists):**
- **sinclairzx81/parsebox** ([github](https://github.com/sinclairzx81/parsebox)) — the state of
  the art: EBNF encoded as TS types, **runtime AND type-level combinators** that parse both at
  runtime and via static inference. Most relevant precedent — proves value-typed combinators
  with precise `infer` are viable, and hints at grammar-in-the-type-system (touches the
  constitution's grammar-totality clause on the TS side).
- **gcanti/parser-ts** (fp-ts family, [github](https://github.com/gcanti/parser-ts)),
  **typescript-parsec**, **johtela/parzec**, **anuraghazra/typelevel-parser** (full type-level
  tokenizer/parser/interpreter) — the design space for typed combinators.
- The `infer` keyword in conditional types is the core mechanism for precise generic extraction
  ([realcoding](https://realcoding.blog/en/2026/03/14/typescript-infer-extract-generic-type/)).

**Prior art — what LOSES / the community warning:** typed combinators are notorious for making
developers *"spend more time dealing with weird type errors than working on their grammar"*
(search synthesis; [HN: TS-types parser](https://news.ycombinator.com/item?id=44781523)). The
lesson: **tighten inference without degrading editor/compile ergonomics or the consumer's
call-site experience** — value.js's 59 `all()` sites and latex-paper's grammar are the real
falsifier (PT-3 §7 R3).

**Constraints our evidence imposes:**
- **Zero runtime cost is a VERIFIED INVARIANT** (Constitution v; SPEC §0/08). 8B inference is
  compile-time only; the emitted JS must be **byte-identical** (prove via empty `dist/` semantic
  diff — PT-3 W3 gate). A leak closed by a runtime cast is a **defect**, not a fix.
- The **enumerated leak sites** are fixed and small: `ParserFunction<T>`→`ParserState<any>`
  (parser.ts:14-16), `any`/`all`/`fuseAll` `Array<Parser<any>>` (leaf.ts:28,154,179),
  `restore(value:any)` (state.ts:79), duplicated `ExtractValue` (parser.ts:9-11 ≡ leaf.ts:155-157)
  (R4 §5, PT-E2). This is a bounded DRY+`infer` job, not open-ended.

**Traps:**
- **The variance leak is partly LEGITIMATE.** `ParserFunction<T>` erasing to `ParserState<any>`
  at the megamorphic-IC implementation boundary is a *required* variance point (R4 §6c), NOT a
  leak to close — only the *public* combinator signatures must stop propagating it (PT-3 W3
  correctly carves this out). Closing it naïvely re-monomorphizes the IC and can perturb 8A's
  codegen. Function-parameter **bivariance** is the subtle TS pitfall here.
- **Type-instantiation depth.** TS caps basic recursion at **50**, tail-recursive conditional
  types at **~1000** (TS 4.5+), with a hard 1000 tail limit; deep grammars hit **TS2589
  "Type instantiation is excessively deep and possibly infinite"**
  ([oneuptime](https://oneuptime.com/blog/post/2026-01-24-fix-type-instantiation-excessively-deep-typescript/view),
  [TS#49459](https://github.com/microsoft/TypeScript/issues/49459)). Mitigations that are prior
  art: unroll recursion (2× depth), **defer instantiation via object/array properties**
  ([dev.to/susisu](https://dev.to/susisu/how-to-create-deep-recursive-types-5fgg)), counter-reset
  at 500. For a bounded-arity combinator algebra (not full type-level *parsing* of the input),
  8B stays well under these — but a tuple-fold over N-ary `all()` must be written tail-recursively
  or it will TS2589 on large grammars. This is the trap that sinks ambitious type-level ports.

---

## 3. FAMILY 8C — THE ISOMORPHIC MIRROR (Rust core ⇄ TS)

**Prior art — the decisive negative result (what LOST, freshly measured 2026):** **WASM-core
for a parser is a boundary-tax trap.** OpenUI *rewrote their Rust+WASM parser in pure TypeScript
and got 2.2×–4.6× faster per call* — the copy/serialize cost across the JS↔WASM boundary
exceeded Rust's compute win; `serde-wasm-bindgen` made it **30% slower** via fine-grained
crossings
([dev.to](https://dev.to/arkacoc13/we-rewrote-our-rust-wasm-parser-in-typescript-and-it-got-faster-a-deep-dive-into-javascript-5dhp),
[openui.com](https://www.openui.com/blog/rust-wasm-parser),
[kunalganglani](https://www.kunalganglani.com/blog/rust-wasm-vs-typescript-performance)). The
rule: **native JS wins for high-frequency, small-input, JS-integrated calls; WASM wins only for
self-contained, CPU-bound bulk tasks with rare boundary crossings.** This is exactly a
combinator hot path (millions of tiny `.parser(state)` calls) → WASM-core loses for the
backbone.

**Our own evidence agrees precisely:** PROVE-14 (SPEC §0/14, CONFIRMED) — the TS offset-tape
decoder is byte-exact and **4.4–4.6× on stop-early get**, but **full-walk is ~0.91×** — *"2×
honestly unmet; native parse is near-optimal for full materialization."* The single place WASM
could pay is the **self-contained bulk tape crossing** (offsets+arena, one coarse boundary), NOT
the combinator engine. So LOCK-6's **6C WASM-CORE is refuted for the backbone**; the surviving
isomorphism forms are **6D ORACLE-ISOMORPHISM** (behavioral/differential conformance corpus,
layout free) and **6A MIRROR-TREE** (a mirrored module tree + name map lint). 8C should consume
LOCK-6's verdict and **not** claim a shared WASM implementation for the hot path.

**Constraints:** the TS regex runtime is **native `RegExp` only** — there is NO NFA/DFA engine
in `typescript/src/`; the bespoke engine is Rust-side (R4 §3). So 8C isomorphism is about the
combinator/state layout and the tape, not a shared automata engine. Non-clobber law binds:
bbnf agents never write into `~/Programming/parse-that`; the mirror is coordinated via letter
BBNF-PT-G (PT-3 §2, SPEC §2b).

**Trap:** claiming layout isomorphism where V8 laws and LLVM laws diverge. The PROVE-14 full-walk
lesson IS the proof that per-language idiom boundaries exist and must be measured, not asserted
(Constitution iii: "reasonable point… measured, not asserted"). Forcing a Rust-shaped tagged
union into TS where a closure/`Int8Array` dispatch already wins is the mirror-for-its-own-sake
trap.

---

## 4. FAMILY 8D — CONSUMER-CONTRACT-FIRST (semver / migration as artifact)

**Prior art (in-tree, the working model):** parse-that already runs a **consumer-ask-letter**
discipline — `VALUEJS-PT-E-2026-07-05.md` is value.js's 1.1.0+ ask letter, gating no wave but
enumerating PT-E1/E2/E3 by consumer need (R4 §5). The consumer contract is concrete: value.js
3.1.0 (`^1.0.0`), latex-paper 0.2.1 (**`^0.7.1`** — version SKEW, predates the 1.0.0 breaking
cut), keyframes.js 6.0.0 (transitive via value.js) — with the exact imported symbol surfaces
enumerated (R4 §7). Semver-as-artifact and migration-dry-run are already specified (PT-3 W0/W5).

**Constraints our evidence imposes:**
- **Y.13 (consumer-less substrate forbidden):** PROVE-11's lazy fields (W1 substrate) MUST land
  with their per-parse-scoped-diagnostics consumer (W2) in the same tranche
  (`PROVE-11/REPORT.md` + PT-3 §1.2 #8/#40). A shape lever with no live reader is banked, not
  shipped.
- **The `latex-paper ^0.7.1` skew is a real migration risk**, not a formality: a 1.1.0 bump is
  an untested 0.7→1.1 jump that could surface a *1.0.0* break (`*Span` excision, `chain()` C-16)
  mislabeled as 1.1.0 (PT-3 §7 R4). W0 must isolate the 0.7→1.0 jump first.
- **Deleting the module-global `enableDiagnostics()`** (PT-E1) could be **breaking** if any
  consumer calls it → forces a 2.0.0 reconsideration (PT-3 W5 semver). The consumer suites are
  the terminal falsifier, decided not assumed.

**Trap:** designing the API from the library inward rather than from value.js/latex-paper/
keyframes usage outward. The 8D formulation's own thesis — perf gates ride THEIR suites, semver
+ migration are first-class deliverables — is the guard against a "clean" API that breaks a real
consumer.

---

## 5. CROSS-FAMILY VERDICT (the load-bearing constraints for synthesis)

1. **8A's strongest claim (allocation-zero, hidden-class-stable) is REAL and largely banked
   (PROVE-11); its maximal claim (monomorphic-by-construction hot path) is REFUTED by the
   structural megamorphic-IC ceiling** — the only measured cure (tagged-union/VM) is LOCK-4C
   territory. The lock must either scope down honestly or route the IC fix through LOCK-4, never
   double-home it.
2. **8C WASM-CORE is refuted for the combinator hot path** by two independent bodies of evidence
   (external OpenUI rewrite 2.2–4.6×; our PROVE-14 full-walk 0.91×). Surviving isomorphism =
   oracle/behavioral (6D) + mirror-tree (6A); WASM pays only at the coarse tape boundary.
3. **8B is a bounded, tractable win** (enumerated leak sites, `infer`+DRY) provided it (a) keeps
   the legitimate implementation-boundary variance `any`, (b) stays under TS2589 depth via
   tail-recursive tuple folds, (c) proves zero-runtime-cost by empty `dist/` diff.
4. **8D is the framing that de-risks all three** — every lever gates on the three consumer
   suites; the latex-paper skew and the `enableDiagnostics` deletion are the two named semver
   hazards.
5. **SPEC §0 profiling law binds every TS number**: single-build paired interleaving, honest
   figures (dispatch ~+5% carried, NOT ≥10%), packrat armed/unarmed disclosed in every table.

---

## Sources
- In-tree: `SPEC.md §0/§1/§2b`, `PORTFOLIO.md` (Constitution + LOCK-8), `R4-parse-that-ts.md`,
  `PROVE-11/{REPORT.md,VERIFY.md}`, `docs/tranches/sk-v24/PT-3.md`, `parse-that/docs/perf-optimization-ts.md`,
  `parse-that/docs/tranches/A/VALUEJS-PT-E-2026-07-05.md`.
- Web: [V8 hidden classes/IC](https://medium.com/@yashschandra/hidden-v8-optimizations-hidden-classes-and-inline-caching-736a09c2e9eb) ·
  [STSC inline caching for TS (ACM)](https://dl.acm.org/doi/pdf/10.1145/3428242) ·
  [parsebox](https://github.com/sinclairzx81/parsebox) ·
  [parser-ts](https://github.com/gcanti/parser-ts) ·
  [typelevel-parser](https://github.com/anuraghazra/typelevel-parser) ·
  [TS2589 fix](https://oneuptime.com/blog/post/2026-01-24-fix-type-instantiation-excessively-deep-typescript/view) ·
  [TS tail-recursion #49459](https://github.com/microsoft/TypeScript/issues/49459) ·
  [deep recursive types](https://dev.to/susisu/how-to-create-deep-recursive-types-5fgg) ·
  [OpenUI Rust-WASM→TS 3×](https://www.openui.com/blog/rust-wasm-parser) ·
  [WASM boundary tax](https://dev.to/arkacoc13/we-rewrote-our-rust-wasm-parser-in-typescript-and-it-got-faster-a-deep-dive-into-javascript-5dhp) ·
  [Rust-WASM vs TS](https://www.kunalganglani.com/blog/rust-wasm-vs-typescript-performance).
