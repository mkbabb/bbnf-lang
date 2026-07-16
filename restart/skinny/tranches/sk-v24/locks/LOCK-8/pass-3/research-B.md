# LOCK-8 / FAMILY 8B — THE TYPE PLANE — PASS-3 RESEARCH

*The TS type system carries the grammar: full `ExtractValue` inference, a typed
combinator algebra with no `Parser<any>` at any public boundary, erasures either
justified as lawful variance or removed, mirroring the zero-runtime-cost law. All
`parse-that/typescript` paths are READ-ONLY; prototyping is on scratch copies; the
type plane costs zero at runtime by construction (it is erased before emission).*

---

## 0. Substrate — the exact type seams at HEAD (`ef10d5b`, `@mkbabb/parse-that@1.0.0`)

| seam | path:line | current type | verdict |
|---|---|---|---|
| `ExtractValue` (canonical) | `src/parse/parser.ts:9-11` | `{[K in keyof T]: T[K] extends Parser<infer V> ? V : never}` over `T extends ReadonlyArray<Parser<unknown>>` | **already tight** — homomorphic mapped type, preserves tuple + labels |
| `ExtractValue` (duplicate) | `src/parse/leaf.ts:155-157` | byte-identical re-definition, local to `all()` | DRY defect — consolidate to one exported utility |
| `ParserFunction<T>` | `src/parse/parser.ts:14-16` | `(val: ParserState<any>) => ParserState<any>` | erases `T` entirely; the megamorphic-IC variance boundary (R4 §6c) |
| `any(...)` bound | `src/parse/leaf.ts:28` | `<T extends Array<Parser<any>>>`; result `T[number] extends Parser<infer V> ? V : never` | **entry bound leaks `any`**; result derivation (distributive) is correct |
| `all(...)` bound | `src/parse/leaf.ts:154` | `<T extends Array<Parser<any>>>` | **entry bound leaks `any`**; body maps via local `ExtractValue` |
| `all()` return erasure | `src/parse/leaf.ts:160-163` | `makeParser(...) as Parser<Result>` | see §1 — lawful for arity≥2, **UNSOUND at arity-1** |
| `fuseAll(...)` param | `src/parse/leaf.ts:179` | `(parsers: Array<Parser<any>>): ParserFunction<Result>` | `any` param leak; `Result` is a free generic default `unknown[]` |
| `skip<S>` input | `src/parse/parser.ts:189` | `skip<S>(parser: Parser<T \| S>)` | **input-type leak** — value discarded, union is fiction (§2) |
| `sepBy<S>` input | `src/parse/parser.ts:569` | `sepBy<S>(sep: Parser<S \| T>, …)` | **input-type leak** — separator value discarded (§2) |
| `many`/`sepBy` return | `src/parse/parser.ts:556,629` | `return state as unknown as ParserState<T[]>` | lawful value-reuse variance (§1) |
| `trim` default + return | `src/parse/parser.ts:481,520` | `whitespace as unknown as Parser<S>` / `this.wrap(…) as unknown as Parser<T>` | lawful value-discard variance (§1) |
| `state.ok/err/from` | `src/parse/state.ts:59,66,72` | `return this as unknown as ParserState<S>` | lawful — `.value` is `any`-internal, reused in place (§1) |
| `restore` value | `src/parse/state.ts:79` | `restore(saved: {…; value: any})` | `any` leak; should be the state's own `T` |

**Key correction to the standing brief:** `ExtractValue` is *already* in its tight
homomorphic form (`ReadonlyArray<Parser<unknown>>`, `[K in keyof T]`). The `Parser<any>`
leaks are **not** in the mapped type — they are in the **entry generic bounds**
(`leaf.ts:28,154,179`) and the `ParserFunction`/`restore` variance points. The mapped
type is the model the bounds should be rewritten toward, not the defect.

---

## 1. Q1 — audit the `as unknown as` / `as Parser<Result>` erasures

The erasures partition into two classes.

**(a) Lawful value-reuse / value-discard variance — DOCUMENTABLE, keep with a comment.**
`many`/`sepBy` (`parser.ts:556,629`) reuse the same `ParserState` object and retype its
`.value` field: `state as unknown as ParserState<T[]>`. Runtime-sound because the state's
`value` is written through the internal `any`-typed setter (`unsafeSetValue`, `state.ts`)
and genuinely holds the built `T[]` at the cast point; no fresh object, no field
transition — this is exactly the V8 shape-stability discipline the plane wants. Likewise
`state.ok/err/from` (`state.ts:59-72`), `trim` (`parser.ts:481,520`), and the `skip`/`next`
discard-side casts. **Disposition: LAWFUL-ERASURE — annotate each `// variance: value
reused in place / value discarded` and leave.** These are not soundness leaks; they are the
cost of a single mutable state threading a heterogeneous value column, which is the design.

**(b) A genuine soundness leak hiding under `all()`'s `as Parser<Result>` — the arity-1
fold.** `all()` (`leaf.ts:160-163`) special-cases one parser:
`parsers.length === 1 ? parsers[0].parser : fuseAll<Result>(parsers)`. For arity≥2,
`fuseAll<Result>` genuinely emits an array, so `as Parser<Result>` is a lawful narrowing of
`makeParser`'s `any`-inferred `T` (the `any` arm coming from `parsers[0].parser:
ParserFunction<any>` polluting the ternary). **But at arity-1 the runtime returns the inner
parser's function unchanged — it yields the bare value `V`** — while the declared type is
`ExtractValue<[Parser<V>]> = [V]`. `all(string("a"))` is typed `Parser<[string]>` and
delivers `"a"`. The `as` cast is precisely what conceals this divergence. **Disposition:
GENUINE LEAK (arity-1 only).** Two lawful cures, adjudicated in §4/§5: (i) delete the
arity-1 fast-path and let `fuseAll` build a 1-array so type and runtime agree (a *runtime*
change — breaks byte-identity, §4); or (ii) type arity-1 as `Parser<V>` to match runtime
(a *type* change — byte-empty, §4). The prototype must pick one on measured cost.

---

## 2. Q4 — the `skip` / `sepBy` INPUT-type leaks

Both discard the value of the parser passed to them, yet constrain its *type* against the
receiver's `T`:

- `skip<S>(parser: Parser<T | S>)` (`parser.ts:189`) — the skipped parser's value is thrown
  away (`skip` returns `value1`, the receiver's value). The union `T | S` is a fiction that
  forces callers to reconcile an irrelevant type; it also drags a phantom generic `S` that
  never appears in the return. **Tighten to `skip(parser: Parser<unknown>): Parser<T>`** —
  input in a discarding position is contravariantly `unknown`; the phantom `S` is deleted.
- `sepBy<S>(sep: Parser<S | T>, min=0, max=Infinity)` (`parser.ts:569`) — the separator's
  value is never collected (only elements enter `matches`). **Tighten to
  `sepBy(sep: Parser<unknown>, min?, max?): Parser<T[]>`** — delete `S`.
- `next<S>(parser: Parser<S>)` (`parser.ts:212`) is the *dual* and is already correct — it
  keeps the second value and returns `Parser<S>`; leave it.

Both tightenings *remove* generics and narrow inputs to `unknown`. **TS2589 risk: none** —
TS2589 ("excessively deep / possibly infinite") fires only on recursive conditional/mapped
instantiation; deleting a generic and widening an input strictly *reduces* the instantiation
graph. The no-new-TS2589 check is satisfied by construction; the prototype confirms with
`tsc --noEmit` on the type-test project.

---

## 3. The tightened typed algebra — concrete signatures (the mechanism made concrete)

One shared `src/parse/types.ts` exporting the single `ExtractValue`; `parser.ts:9` and
`leaf.ts:155` both import it (kills the duplicate). Then the entry bounds:

```ts
// leaf.ts — the const-tuple form; TS 5.0 `const` type parameters (pkg pins typescript ^5.8)
export function all<const T extends ReadonlyArray<Parser<unknown>>>(
    ...parsers: T
): Parser<ExtractValue<T>> { … }               // no `Parser<any>`; no `as` cast if §1(b)(ii)

export function any<const T extends ReadonlyArray<Parser<unknown>>>(
    ...parsers: T
): Parser<T[number] extends Parser<infer V> ? V : never> { … }

function fuseAll<const T extends ReadonlyArray<Parser<unknown>>>(
    parsers: T,
): ParserFunction<ExtractValue<T>> { … }        // param `any` → `unknown`, result typed
```

**Why `const T`.** Without it, rest-parameter inference on `(...parsers: T)` where
`T extends ReadonlyArray<…>` widens `all(a, b)` toward `Parser<…>[]` and `ExtractValue`
degrades from the positional tuple `[Va, Vb]` to `Vx[]`. The `const` modifier
(TS 5.0+, TS-5.8 available) freezes the literal tuple so the homomorphic map indexes
positionally. This is the single enabling feature; it is compile-time only (erased).

`ParserFunction` (`parser.ts:14-16`) **stays `ParserState<any>`** at the implementation
boundary — it is the deliberate variance seam through which every distinct closure shape
flows one indirect call (`this.parser(state)`, R4 §6c). It is documented as an
implementation-boundary `any`, NOT counted as a public leak, and the *public* combinator
signatures above stop threading it into user-facing return types. `restore` (`state.ts:79`)
retypes `value: any → T`.

**TS2589 for the whole algebra:** the mapped type is single-level (O(N) in arity, not
recursive) — no accumulator recursion, no tail-recursive conditional. TS2589 is therefore
structurally impossible for `all`/`any`/`fuseAll` at any realistic arity; the failure mode
would only appear if someone folded nested `.then` 2-tuples type-side (the emitted runtime
already flattens to one array — do not mirror the nesting in the type).

---

## 4. Q2 — does the tightened algebra emit a BYTE-IDENTICAL `dist/`?

**Scope the claim: `.js` chunks only.** The `.d.ts` outputs MUST change — that is the entire
deliverable of the tightening — so byte-identity applies to the emitted JavaScript
(`dist/*.{js,cjs}`), never to declarations. The two-step isolation on the **real
vite + vite-plugin-dts toolchain** (an esbuild-only proxy reorders/renames and would forge a
false delta):

1. **Version-migration delta (i):** build 1.0.0 pristine `dist/`, bump the toolchain to the
   1.1.0 devDeps (vite ^7, vitest ^3.2, typescript ^5.8, vite-plugin-dts) with **source
   unchanged**, rebuild. Record the `.js` churn this alone produces (esbuild transform
   version, banner, sourcemap). This is the toolchain floor, attributed and set aside.
2. **Tightening delta (ii):** apply ONLY the §2/§3 type edits (signatures, `const T`, the
   shared `types.ts`, `unknown` inputs) — **no runtime line touched** — rebuild. This `.js`
   delta **MUST be byte-empty** against step (i)'s output. A non-empty `.js` delta means a
   type edit leaked into runtime (e.g. a mis-placed value-level default) — a defect that
   halts.

**The tension the prototype must resolve:** §1(b) — the arity-1 `all` soundness leak. Cure
(i) (build a 1-array) is a *runtime* change and would make step-(ii) `.js` **non-empty** —
so it CANNOT ride the byte-empty tightening; it is a separate, semver-visible runtime item.
Cure (ii) (type arity-1 as `Parser<V>`) keeps step-(ii) byte-empty and is the type-plane-pure
choice. **Recommended: cure (ii)** — the type plane tightens to match the runtime that
ships, and the arity-1-array behavior (if wanted) becomes an explicit future runtime
proposal, not a smuggled edit. The byte-empty `.js` diff is then the *proof of the
zero-runtime-cost invariant* — the whole tightening is erasure.

---

## 5. Q3 — the instantiation-budget figure and its corpus (gated on P2)

**Mechanism:** `tsc --noEmit --extendedDiagnostics` reports `Instantiations`,
`Types`, and check time; the budget figure is the `Instantiations` delta between the
`any`-bound baseline and the tightened `const`-tuple algebra over a fixed corpus, disclosed
with **tsc version + the exact signatures + the corpus manifest** (per SPEC §0 fencing).

**Corpus — resolved by P2 (§6):** value.js's production posture (branch `master`, 3.1.0,
`@mkbabb/parse-that: ^1.0.0`) carries **59 `all()` sites** (R4 §7) and `^1.0.0` resolves
1.1.0 — so master value.js WILL adopt the tightened types and is a **live native corpus**.
The tranche-`u`/4.0.0 divest (§6) is NOT the corpus (it imports nothing). Therefore:

- **Native leg:** run `--extendedDiagnostics` on master value.js @ 3.1.0 with the candidate
  1.1.0 tarball installed — the real 59 `all()` + `dispatch()` sites. This is runnable
  (P2-H2 holds for master), disclose the number.
- **Self-use leg:** run it on the first-party `test/types/inference.test-d.ts` + the
  `test/*.test.ts` suite under the tightened signature, as the always-available floor.
- Because the tightened form only *adds* one single-level mapped instantiation per `all`
  call (no recursion), the expected `Instantiations` delta is **linear in call-site count
  and small**; a superlinear blow-up would signal an accidental recursive form (halt, §3).

---

## 6. PLANE-WIDE — CONSUMER LIVENESS (P2), decided on evidence

**The value.js import posture is a WIP-branch artifact, not the production contract —
carry BOTH hypotheses (XL-G, non-blocking).**

| branch | value.js version | parse-that dep | src imports | posture |
|---|---|---|---|---|
| `master` (+`tranche-t`) | **3.1.0** | `@mkbabb/parse-that: ^1.0.0` (installed 1.0.0; `^` resolves 1.1.0) | 9 sites, 59 `all()` (R4 §7) | **LIVE consumer** |
| `tranche-u` (HEAD 2026-07-15, checked out) | **4.0.0** | **absent** — `dependencies: {}`, no dev/peer dep | **0** imports of `@mkbabb/parse-that` | **fully DIVESTED** |

Evidence: `value.js` on `tranche-u` has `dependencies: {}`, zero `@mkbabb/parse-that` in
`package.json`, and `rg -c "@mkbabb/parse-that" src/` = 0. `master:package.json` still
declares `^1.0.0` at 3.1.0. **Answer to P2:** value.js 4.0.0's zero-import posture is a
mid-migration `tranche-u` artifact; the production consumer of record remains master @
3.1.0. **Directional risk (name it):** if `tranche-u` merges to master, value.js *ceases to
be a parse-that consumer entirely* — which would strand the LOCK-8 consumer-contract
rationale on latex-paper (frozen `^0.7.1`, R4 §7) + keyframes (transitive via value.js →
also severed) + the first-party suite alone. **This is owner-confirmation-pending and
NON-blocking**; the tightening and byte-identity work do not depend on it, but the Q3 native
corpus and any "consumer suites green" gate MUST pin **master**, not the checked-out
`tranche-u`, or they measure an empty consumer.

## 6b. SPEC FIDELITY — where family text diverges from measured HEAD

- **"kill the `Parser<any>` leaks" locates the leak in `ExtractValue`** (implied by PT-E2
  framing) — DIVERGES: `ExtractValue` is already tight (`parser.ts:9`); the leaks are in the
  **entry bounds** `leaf.ts:28,154,179` + `ParserFunction`/`restore`. Re-cut the spec to
  name the bounds, not the mapped type.
- **"the `as unknown as` inside `all`"** — DIVERGES: `all` uses a single `as Parser<Result>`
  (`leaf.ts:163`), not `as unknown as`; the `as unknown as` family lives in
  `many`/`sepBy`/`state.ok` and is **lawful variance** (§1a). The real `all` finding is the
  **arity-1 soundness leak** (§1b), which the spec does not yet name — add it.
- **"empty `dist/` diff proves zero runtime cost"** (PT-3 W3) — REFINE: scope to `.js`;
  `.d.ts` legitimately changes; and the arity-1 cure choice governs whether the `.js` diff
  can be empty at all (§4). The spec's byte-empty claim is only achievable under §1(b) cure
  (ii).
- **value.js "59 `all()` sites, 3.1.0" as the consumer floor** — HOLDS for master, VOID for
  the checked-out tranche-`u`; the spec must pin the branch.

---

## 7. Prior art (with citations)

- **TS variadic tuple types + homomorphic mapped types over tuples** — the `[K in keyof T]`
  form preserving tuple arity and labels (TS 4.0 "Variadic Tuple Types"; TS 4.1 labeled
  tuples). `ExtractValue` is the textbook application; `all`/`zip`-style combinators in
  fp-ts, ts-toolbelt, and type-fest use exactly this shape.
- **`const` type parameters** (TS 5.0) — the enabler for positional tuple capture from rest
  args without an `as const` at every call site; without it rest-arg inference widens to an
  array. This is the crux of §3.
- **TS2589 recursion budget** — TS caps type instantiation depth (~50) and total
  instantiations (~5,000,000) to guarantee termination. Deep libraries (arktype, zod's
  inference, ts-toolbelt) mitigate via **tail-recursive conditional types** (TS 4.5 removed
  the tail-recursion stack cost) and accumulator-tuple patterns. 8B's algebra is
  *non-recursive* (single-level map) so it sits far under the budget — the SOTA lesson is
  the negative one: **do not fold nested `.then` 2-tuples type-side** (that IS the recursive
  form that trips TS2589); mirror the flattened runtime array, which is O(N) flat.
- **Type-level test harnesses** — `expect-type`, `tsd`, and bare `@ts-expect-error` under
  `tsc --noEmit` are the SOTA for asserting inference both ways (reject over-widening AND
  catch true mismatches); `@ts-expect-error` on a genuinely-wrong shape is the two-sided
  proof the gate needs.
- **`--extendedDiagnostics` `Instantiations`** — the accepted instantiation-budget metric
  (used by DefinitelyTyped's perf CI and `tsc --generateTrace` analysis) — the correct
  instrument for Q3.

---

## 8. Named risks & counterexamples

- **R-A (arity-1 soundness, live).** `all(p)` typed `Parser<[V]>`, yields `V`. A consumer
  destructuring `const [x] = all(p).parse(s).value` compiles and crashes (`x` is a char, not
  the array). Cure (ii) (type as `Parser<V>`) keeps byte-identity; cure (i) fixes runtime but
  breaks §4. The prototype must pick and record.
- **R-B (`const T` regressions inference at call sites).** `all(...someArray)` where the arg
  is a widened `Parser<X>[]` (not a literal tuple) now yields `Parser<X[]>` — correct, but a
  consumer relying on the old `any` widening to pass a heterogeneous non-tuple array reddens.
  Caught only by full `tsc` at consumer sites (master value.js), never by unit tests.
- **R-C (measuring the empty consumer).** Running Q3/consumer-suite gates against the
  checked-out `tranche-u` value.js measures zero `all()` sites and falsely reports "green."
  Pin `master`.
- **R-D (`.d.ts` byte-diff false alarm).** Treating `.d.ts` churn as a byte-identity failure
  would reject the correct outcome — the declarations MUST change. Fence the diff to `.js`.
- **R-E (`ParserFunction` over-tightening).** Replacing `ParserState<any>` with a generic
  would re-introduce the variance friction at the one megamorphic IC and could perturb the
  emitted closures the V8 plane pins (R4 §6c) — leave it, document it, exclude it from the
  leak count.

---

## 9. What a prototype MUST test to move this family

1. **The tightened algebra compiles and infers** — `types.ts` shared `ExtractValue`; `all`/
   `any`/`fuseAll` on `<const T extends ReadonlyArray<Parser<unknown>>>`; `skip`/`sepBy`
   inputs → `Parser<unknown>`; `restore` → `T`. Prove with `test/types/inference.test-d.ts`:
   `all(string("a"), regex(/\d/))` ⟹ `Parser<[string, string]>`; `any(...)` ⟹ union;
   `@ts-expect-error` on a wrong shape still fires; `skip`/`sepBy` accept any element type.
2. **No new TS2589** — `tsc --noEmit` clean on the type-test project and on master value.js's
   59 sites with the candidate installed.
3. **Byte-identical `.js`** — the §4 two-step (real vite+dts): version-migration delta
   recorded, tightening delta byte-empty on `dist/*.{js,cjs}` (under §1(b) cure (ii)).
4. **Instantiation budget** — `--extendedDiagnostics Instantiations` delta, baseline vs
   tightened, native (master value.js) + self-use legs, disclosed with tsc version +
   signatures + corpus manifest; assert linear-in-call-sites, not superlinear.
5. **Arity-1 adjudication** — demonstrate the current `all(p)` type/runtime divergence, then
   the chosen cure, and show which of {byte-empty, runtime-changing} it is.
6. **Consumer red-check** — full `tsc`/`vue-tsc` at master value.js's `all()`/`any()`/
   `dispatch()` sites under the tightened tarball: does any site redden (R-B)? A break is a
   real semver signal, decided not assumed.
