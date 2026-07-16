# LOCK-8 / FAMILY 8B — THE TYPE PLANE — PASS-3 RESEARCH

*The TS type system carries the grammar: full value inference, a typed combinator
algebra with no `Parser<any>` at any public boundary, every erasure either removed by a
sharper signature or justified as lawful in-place variance, mirroring the zero-runtime-cost
law. All `parse-that/typescript` paths are READ-ONLY; prototyping is on scratch copies; the
type plane is erased before emission and costs zero at runtime by construction.*

---

## 0. Substrate — the exact type seams at HEAD (`@mkbabb/parse-that@1.0.0`, `src/parse/`)

| seam | path:line | current type | verdict |
|---|---|---|---|
| `ExtractValue` (canonical) | `parser.ts:9-11` | `{[K in keyof T]: T[K] extends Parser<infer V> ? V : never}` over `T extends ReadonlyArray<Parser<unknown>>` | **already tight** — homomorphic mapped type; preserves tuple arity + labels |
| `ExtractValue` (duplicate) | `leaf.ts:155-157` | byte-identical re-definition local to `all()` | DRY defect — consolidate to one exported utility (`types.ts`) |
| `ParserFunction<T>` | `parser.ts:13-16` | `(val: ParserState<any>) => ParserState<any>` | erases `T`; the megamorphic-IC variance boundary (R4 §6c) — **lawful, keep** |
| `any(...)` bound | `leaf.ts:28` | `<T extends Array<Parser<any>>>`; result `T[number] extends Parser<infer V> ? V : never` | **entry bound leaks `any`**; result derivation (distributive union) is correct |
| `all(...)` bound | `leaf.ts:154` | `<T extends Array<Parser<any>>>` | **entry bound leaks `any`**; body maps via the duplicate `ExtractValue` |
| `all()` return cast | `leaf.ts:160-163` | `makeParser(...) as Parser<Result>` | lawful for arity≥2; **UNSOUND at arity-1** (§1c) |
| `fuseAll(...)` param | `leaf.ts:179` | `(parsers: Array<Parser<any>>): ParserFunction<Result>` | `any` param leak; `Result` a free generic default `unknown[]`; **reads-only body** (confirmed leaf.ts:184-272 — no write to `parsers`) |
| `many`/`sepBy` return | `parser.ts:556,629` | `return state as unknown as ParserState<T[]>` | **lawful** value-reuse variance (§1a) |
| `trim` default+return | `parser.ts:481,520` | `whitespace as unknown as Parser<S>` / `wrap(…) as unknown as Parser<T>` | **lawful** value-discard variance (§1a) |
| `state.ok/err/from` | `state.ts:59,66,72` | `return this as unknown as ParserState<S>` | **lawful** — `.value` written through `unsafeSetValue` (`state.ts:87-89`), reused in place (§1a) |
| `then/or/chain` input | `parser.ts:81,105,124` | `Parser<S \| T>` | **input-type leaks** — the `\| T` is a fiction; PROTO-1 tightened to `Parser<S>` (§2) |
| `skip<S>` input | `parser.ts:189` | `skip<S>(parser: Parser<T \| S>)` | **input-type leak** — value discarded, `S` phantom (§2) |
| `sepBy<S>` input | `parser.ts:569` | `sepBy<S>(sep: Parser<S \| T>, …)` | **input-type leak** — separator value discarded, `S` phantom (§2) |
| `restore` value | `state.ts:79` | `restore(saved: {…; value: any})` | `any` leak → the state's own `T` (§3) |

**Key correction to the standing brief.** The `Parser<any>` leaks are **not** in
`ExtractValue` — that mapped type is already in tight homomorphic form. They are in the
**entry generic bounds** (`leaf.ts:28,154,179`), the **input unions** on
`then/or/chain/skip/sepBy`, and the `restore` variance point. The mapped type is the model
the bounds should be rewritten *toward*, not the defect. Any spec text locating the leak in
`ExtractValue` is a fidelity miss (§6b).

---

## 1. Q1 — audit the `as unknown as` erasure inside the TIGHTENED `all` signature

The directive's target is precise and is **not** the shipped `1.0.0` return cast. When the
algebra is tightened, the prototype's `all` body reads:

```ts
parsers.length === 1 ? parsers[0].parser : fuseAll<Result>(parsers as unknown as Parser<unknown>[])
```

i.e. a fresh `as unknown as Parser<unknown>[]` **laundering the tightened tuple/readonly
`parsers` into `fuseAll`'s stale `Array<Parser<any>>` param**. Three distinct classes must
be separated; only the first is the directive's subject.

**(a) The flagged cast — REMOVABLE, therefore NOT lawful; it must be deleted, not
documented.** The cast exists because the tightening stopped one call short: `all` was given
a readonly/tuple bound but `fuseAll` still declares `parsers: Array<Parser<any>>`, so the
tightened `all` cannot pass its own `parsers` into `fuseAll` without laundering
readonly→mutable and tuple→`Parser<unknown>[]`. This is exactly the "tightening smuggles an
un-audited erasure into its own thesis" charge. **A `readonly`-variant signature removes it
with zero new TS2589:** propagate the same bound into `fuseAll` —

```ts
function fuseAll<const T extends readonly Parser<unknown>[]>(
    parsers: T,
): ParserFunction<AllValue<T>> { … }        // body byte-identical: reads parsers[i]/.length only

// all() then calls, with NO cast:
parsers.length === 1 ? parsers[0].parser : fuseAll(parsers)
```

The cure is **sound** because `fuseAll` never mutates `parsers` (verified: only
`parsers[0..2]`, `parsers[i]`, `parsers.length` reads at `leaf.ts:184-272`; the sole
`parsers.push` in the file is `dispatch`'s *local* intern array at `leaf.ts:108`, a different
variable). A `readonly` param is therefore a strictly-more-honest declaration of what the
body already does, and readonly-tuple → readonly-tuple is exact, no variance gap. **The
emitted `.js` is unchanged** (fuseAll body untouched) — so this cure rides the byte-empty
tightening (§4). **Disposition: DELETE the cast via the readonly-tuple `fuseAll` signature;
do NOT keep-and-document it.** Documenting a removable cast as "lawful variance" would be the
self-deception the plane forbids.

**(b) Lawful value-reuse / value-discard variance — the `as unknown as` family that STAYS,
documented.** `many`/`sepBy` (`parser.ts:556,629`), `state.ok/err/from` (`state.ts:59-72`),
and `trim` (`parser.ts:481,520`) reuse the *same* `ParserState` object and retype its
`.value` field, written through the `any`-internal `unsafeSetValue` choke point
(`state.ts:87-89`). No fresh object, no hidden-class transition — this IS the V8
shape-stability discipline the plane wants. **Disposition: LAWFUL-ERASURE** — annotate each
`// variance: value reused in place / discarded` and leave. These are not soundness leaks;
they are the cost of one mutable state threading a heterogeneous value column, which is the
design (they are also, unlike (a), NOT removable without allocating a fresh typed state per
combinator — the exact allocation the hot path forbids).

**(c) A separate genuine soundness leak under `all`'s RETURN cast — the arity-1 fold.**
`all()` special-cases one parser (`leaf.ts:161`): at arity-1 the runtime returns the inner
parser's function unchanged and yields the bare value `V`, while the declared type is
`ExtractValue<[Parser<V>]> = [V]`. `all(string("a"))` is typed `Parser<[string]>` and
delivers `"a"`. The `as Parser<Result>` return cast conceals this. **Disposition: GENUINE
LEAK (arity-1 only), distinct from (a).** Two lawful cures, adjudicated in §4: (i) delete the
arity-1 fast-path so `fuseAll` builds a 1-array (a *runtime* change — breaks byte-identity);
or (ii) type arity-1 as `Parser<V>` to match the runtime that ships (a *type* change —
byte-empty). **Recommended: (ii)** — the type plane tightens to match shipping runtime; a
1-array runtime becomes an explicit, semver-visible future proposal, never a smuggled edit.

---

## 2. Q4 — the INPUT-type leaks: `skip`/`sepBy` (and `then`/`or`/`chain`)

Every combinator that discards the value of a parser it is *passed*, yet constrains that
parser's type against the receiver's `T`, leaks a phantom generic and a fictional union:

- `skip<S>(parser: Parser<T | S>)` (`parser.ts:189`) — the skipped parser's value is thrown
  away; `skip` returns `value1` (the receiver's value). `T | S` forces callers to reconcile
  an irrelevant type and drags a phantom `S` that never surfaces in the return. **Tighten to
  `skip(parser: Parser<unknown>): Parser<T>`** — input in a discarding position is
  contravariantly `unknown`; delete `S`.
- `sepBy<S>(sep: Parser<S | T>, min=0, max=Infinity)` (`parser.ts:569`) — the separator value
  never enters `matches` (`parser.ts:584-617` collect only the element). **Tighten to
  `sepBy(sep: Parser<unknown>, min?, max?): Parser<T[]>`** — delete `S`.
- `then<S>(next: Parser<S | T>)`, `or<S>(other: Parser<S | T>)`, `chain<S>(fn: … Parser<S | T>)`
  (`parser.ts:81,105,124`) — PROTO-1 tightened all three from `Parser<S | T>` to `Parser<S>`
  (PROTO-1.diff:21-43). `then` returns `[T, S]`, `or` returns `T | S`, `chain` returns `S`;
  the incoming `| T` is spurious in each (the receiver already carries `T`). **Tighten the
  input unions to `Parser<S>`.**
- `next<S>(parser: Parser<S>)` (`parser.ts:212`) is the *dual* and already correct — it keeps
  the second value and returns `Parser<S>`. Leave it (it is the positive control the
  type-test uses to prove the tightening does not over-reach).

All tightenings **remove** generics/unions and widen inputs to `unknown` or drop a spurious
arm. **TS2589 risk: none** — TS2589 ("excessively deep / possibly infinite") fires only on
recursive conditional/mapped instantiation; deleting a generic and widening an input strictly
*reduces* the instantiation graph. Confirmed structurally; the prototype verifies with
`tsc --noEmit` on the type-test project.

---

## 3. The tightened algebra — concrete signatures (mechanism made concrete)

One shared `src/parse/types.ts` (the 8B type home) homes the single value-tuple utility;
`parser.ts:9` and `leaf.ts:155` both import it, killing the duplicate. The home also decides
the drop-undefined question (§3a).

```ts
// types.ts
import type { Parser } from "./parser.js";
export type Values<Ps extends readonly Parser<unknown>[]> =
    { [K in keyof Ps]: Ps[K] extends Parser<infer V> ? V : never };
export type AnyValue<Ps extends readonly Parser<unknown>[]> =
    Ps[number] extends Parser<infer V> ? V : never;         // ordered-choice union

// leaf.ts — const-tuple form; TS 5.0 `const` type params (pkg pins typescript ^5.8)
export function all<const T extends readonly Parser<unknown>[]>(
    ...parsers: T
): Parser<AllValue<T>> { … }                                  // §1(a) cure: fuseAll(parsers) — no cast
export function any<const T extends readonly Parser<unknown>[]>(
    ...parsers: T
): Parser<AnyValue<T>> { … }
function fuseAll<const T extends readonly Parser<unknown>[]>(
    parsers: T,
): ParserFunction<AllValue<T>> { … }                         // param `any` → readonly tuple, reads-only body
```

- `restore` (`state.ts:79`) retypes `value: any → T`.
- `ParserFunction` (`parser.ts:13-16`) **stays `ParserState<any>`** at the implementation
  boundary — the deliberate variance seam through which every distinct closure shape flows
  one indirect call (`this.parser(state)`, R4 §6c). Documented as an implementation-boundary
  `any`, **not** counted as a public leak; the public signatures above stop threading it into
  user-facing return types. Over-tightening it to a generic re-introduces friction at the one
  megamorphic IC and can perturb the closures the V8 plane pins (risk R-E).

**Why `const T`.** Without it, rest-parameter inference on `(...parsers: T)` where
`T extends readonly Parser<unknown>[]` widens `all(a, b)` toward `Parser<…>[]` and the map
degrades from the positional tuple `[Va, Vb]` to `Vx[]`. The `const` modifier (TS 5.0+,
available at the `^5.8` pin) freezes the literal tuple so the homomorphic map indexes
positionally. This is the single enabling feature; it is compile-time only (erased).

**TS2589 for the whole algebra.** The map is single-level (O(N) in arity, non-recursive) — no
accumulator recursion, no tail-recursive conditional — so TS2589 is structurally impossible at
any realistic arity. The failure mode would appear only if someone folded nested `.then`
2-tuples type-side; the emitted runtime already flattens to one flat array, so the type must
mirror the flat array, never the nesting (§7).

### 3a. The DropUndefined subtlety — a documented widening, named honestly

`all()` **drops `undefined` values at runtime** (`leaf.ts:199,207,246,267`:
`if (state.value !== undefined) out[w++] = …`, then `out.length = w`). So
`all(a, opt(b), c)` where `opt(b)` yields `undefined` produces a **length-2, position-shifted**
array at runtime. **No static tuple type can express this length-variance.** Two candidate
public result types, both imperfect:

- shipped `Values<T>` = `[Va, Vb|undefined, Vc]` — keeps `undefined` in the type, but the
  runtime has *removed* that slot and shifted `Vc` left. Unsound on position.
- PROTO-1's `AllValue<T> = DropUndefined<Values<T>>` (`{[K]: Exclude<T[K], undefined>}`,
  PROTO-1 types.ts:120-122) — strips `undefined` from each arm, so
  `[Va, Vb, Vc]` — claims `Vb` present when it may be absent-and-shifted. Also unsound on
  position, in the opposite direction.

Neither is fully sound; the honest disposition is **DOCUMENT the length-variance limitation**
and gate shippability on "no surviving consumer exercises the length-variable `all()`" — which
**holds** on current evidence: value.js is divested (§6, zero `all()`), latex-paper has zero
`all()` and its 11 `.opt()` are all outside `all`. `DropUndefined` is the marginally better
default (a present-value tuple is the common case and the shape consumers destructure), but it
must ship with the caveat comment PROTO-1's own `types.ts` already carries, and the self-test
differential records the widening. **This is inherited, not introduced** — the shipped
`ExtractValue` already mis-models the drop; the tightening does not create the hazard, it makes
the choice explicit.

---

## 4. Q2 — does the tightened algebra emit a BYTE-IDENTICAL `dist/`?

**Fence the claim to `.js`/`.cjs` chunks.** The `.d.ts` outputs MUST change — that is the
entire deliverable — so byte-identity applies to emitted JavaScript (`dist/*.{js,cjs}`), never
to declarations (risk R-D). The two-step isolation on the **real vite + vite-plugin-dts
toolchain** (an esbuild-only proxy reorders/renames and forges a false delta):

1. **Version-migration delta (i):** build `1.0.0` pristine `dist/`; bump the toolchain to the
   `1.1.0` devDeps (`typescript ^5.8`, `vite ^7`, `vitest ^3.2`, `vite-plugin-dts ^4.5` —
   confirmed pins) with **source unchanged**; rebuild. Record the `.js` churn this alone
   produces (esbuild transform version, banner, sourcemap). Toolchain floor, attributed, set
   aside.
2. **Tightening delta (ii):** apply ONLY the §2/§3 type edits (entry bounds, `const T`, shared
   `types.ts`, `unknown` inputs, `restore: T`, the §1(a) readonly `fuseAll`) — **no runtime
   line touched** — rebuild. This `.js` delta **MUST be byte-empty** against step-(i) output. A
   non-empty `.js` delta means a type edit leaked into runtime (a mis-placed value-level
   default) — a defect that halts.

**The tension the prototype must resolve.** §1(c) — the arity-1 `all` divergence. Cure (i)
(build a 1-array) is a *runtime* change and makes step-(ii) `.js` **non-empty**, so it CANNOT
ride the byte-empty tightening; it is a separate, semver-visible runtime item. Cure (ii) (type
arity-1 as `Parser<V>`) keeps step-(ii) byte-empty and is type-plane-pure. **Recommended: cure
(ii).** The §1(a) readonly-`fuseAll` cure is likewise byte-empty (body unchanged). The empty
`.js` diff is then the *proof of the zero-runtime-cost invariant* — the whole tightening is
erasure, exactly the SPEC law mirrored from the Rust type plane.

---

## 5. Q3 — the instantiation-budget figure and its corpus (gated on P2)

**Mechanism.** `tsc --noEmit --extendedDiagnostics` reports `Instantiations`, `Types`, and
check time; the budget figure is the `Instantiations` delta between the `any`-bound baseline
and the tightened `const`-tuple algebra over a fixed corpus, disclosed with **tsc version + the
exact signatures + the corpus manifest** (SPEC §0 fencing).

**Corpus — the native leg is UN-RUNNABLE on the value.js tree as it currently stands.** The
checked-out value.js is `tranche-u` @ **4.0.0**, `dependencies` carries **no
`@mkbabb/parse-that`**, and `rg -c "@mkbabb/parse-that" src/` = 0, `all(` sites = **0**
(verified). There is no native `all()` corpus to measure on the current tree. The 59-`all()`
figure belongs to value.js `master` @ 3.1.0 (`^1.0.0`), which resolves 1.1.0 — but `master` is
NOT the checked-out state, and the freshest signal (tranche-u HEAD, 2026-07-15) is *divestment*
(§6). Therefore:

- **Native leg: record UN-RUNNABLE on the current tree.** It is runnable *only if* value.js
  `master` is confirmed the consumer of record AND does not merge tranche-u — an
  owner-confirmation-pending, non-blocking condition (XL-G). Do not silently measure the
  checked-out empty tree and report "native" (risk R-C).
- **Self-use leg (always-available floor):** run `--extendedDiagnostics` on the first-party
  `test/types/inference.test-d.ts` + the `test/*.test.ts` suite under the EXACT tightened
  signatures; disclose tsc version + signatures + corpus. This is the number that ships.
- Because the tightened form only *adds* one single-level mapped instantiation per `all`/`any`
  call (no recursion, §3), the expected `Instantiations` delta is **linear in call-site count
  and small**; a superlinear blow-up signals an accidental recursive form (halt). The pass-2
  prototype's self-test measurement of the full 4-combinator algebra + `DropUndefined` was
  **+2.9% instantiations / 0 new TS2589 / precision both ways** — small and linear, consistent
  with this prediction; carry it as the self-use anchor, re-measured under the §1(a)-cured
  signatures.

---

## 6. PLANE-WIDE — CONSUMER LIVENESS (P2), decided on fresh evidence

**value.js's zero-import posture is the CURRENT checked-out reality, not a hypothesis — and it
is DIVESTMENT, freshest as of 2026-07-15. Carry both, but weight the fresh signal.**

| branch | value.js version | parse-that dep | src imports / `all()` | posture |
|---|---|---|---|---|
| `tranche-u` (**checked out**, HEAD 2026-07-15) | **4.0.0** | **absent** (`dependencies` has no parse-that) | **0 / 0** (verified `rg`) | **fully DIVESTED** |
| `master` (per prior recon) | 3.1.0 | `@mkbabb/parse-that: ^1.0.0` (resolves 1.1.0) | 9 sites / 59 `all()` (R4 §7) | LIVE — but not the checked-out state |

- **latex-paper** (verified): `master`, 0.2.1, dep `^0.7.1`, installed **0.7.1** — predates the
  1.0.0 breaking cut; a 1.1.0 bump is an untested 0.7→1.1 jump. Imports only
  `Parser/ParserState/regex/string/any` (R4 §7) — **zero `all()`**, so it is NOT an
  instantiation-budget corpus for the `all` tightening; it is only a migration/type-check
  crater subject.
- **keyframes.js**: transitive via value.js → if value.js divests, keyframes' parse-that link
  is severed too.

**Answer to P2.** value.js 4.0.0's zero-import posture is **not** merely a WIP artifact to be
discounted — it is the currently-committed tree and the freshest direction of travel
(divestment). The production consumer *of record* may still be `master` @ 3.1.0, but LOCK-8's
consumer-contract rationale is **materially at risk**: if tranche-u merges, value.js ceases to
be a parse-that consumer entirely, stranding the contract on latex-paper (frozen `^0.7.1`,
zero `all()`) + keyframes (transitive, also severed) + the first-party suite. **This is
owner-confirmation-pending and NON-blocking**; the tightening and byte-identity work do not
depend on it. But every "consumer suites green" / native-instantiation gate MUST pin `master`
explicitly or record un-runnable — measuring the checked-out `tranche-u` tree measures an empty
consumer and would falsely report green (risk R-C).

### 6b. SPEC FIDELITY — where family text diverges from measured HEAD

- **"kill the `Parser<any>` leaks" implying the leak is in `ExtractValue`** — DIVERGES:
  `ExtractValue` is already tight (`parser.ts:9`); the leaks are the **entry bounds**
  (`leaf.ts:28,154,179`), the **input unions** (`then/or/chain/skip/sepBy`), and `restore`.
  Re-cut to name the bounds/unions, not the mapped type.
- **"the `as unknown as` inside `all`"** — REFINE: it is NOT the shipped `1.0.0` code (which
  uses a single `as Parser<Result>` return cast at `leaf.ts:163`). It is the **tightened
  prototype's** `fuseAll<Result>(parsers as unknown as Parser<unknown>[])` — a REMOVABLE cast
  laundering the tightened tuple into `fuseAll`'s stale `any` param, cured by a readonly-tuple
  `fuseAll` signature (§1a). Separately, the shipped `all` carries an **arity-1 return
  soundness leak** (§1c) the spec does not yet name — add it.
- **"empty `dist/` diff proves zero runtime cost"** — REFINE: fence to `.js`/`.cjs`; `.d.ts`
  legitimately changes; and the arity-1 cure choice governs whether the `.js` diff can be empty
  (§4). Byte-empty is achievable only under §1(c) cure (ii) + §1(a) readonly-`fuseAll`.
- **"value.js 59 `all()` sites, 3.1.0 as the consumer floor"** — VOID for the checked-out
  `tranche-u` (4.0.0, divested); the spec must pin the branch and treat the native
  instantiation leg as conditional on `master` being the consumer of record.

---

## 7. Prior art (with citations)

- **Variadic tuple types + homomorphic mapped types over tuples** (TS 4.0 Variadic Tuple
  Types; TS 4.1 labeled tuples) — the `[K in keyof T]` form preserving tuple arity/labels.
  `Values<T>` is the textbook application; `all`/`zip`-style combinators in fp-ts, ts-toolbelt,
  and type-fest use exactly this shape.
- **`const` type parameters** (TS 5.0) — the enabler for positional tuple capture from rest
  args without an `as const` at every call site; without it, rest-arg inference widens to an
  array. This is the crux of §3.
- **TS2589 recursion budget** — TS caps instantiation depth (~50) and total instantiations
  (~5M) to guarantee termination. Deep libraries (arktype, zod, ts-toolbelt) mitigate via
  tail-recursive conditional types (TS 4.5 removed the tail-recursion stack cost) and
  accumulator-tuple patterns. 8B's algebra is *non-recursive* (single-level map), far under
  budget — the SOTA lesson is the negative one: do NOT fold nested `.then` 2-tuples type-side
  (that IS the recursive form that trips TS2589); mirror the flattened runtime array (O(N)
  flat).
- **`readonly` array params as variance discipline** — the canonical fix for a "smuggled cast
  into a helper with a mutable param that only reads" is to declare the param `readonly`, which
  is both safer and strictly more permissive on inputs; this is the §1(a) cure and matches the
  guidance in the TS handbook (ReadonlyArray) and DefinitelyTyped review conventions.
- **Type-level test harnesses** — `expect-type`, `tsd`, and bare `@ts-expect-error` under
  `tsc --noEmit` are SOTA for asserting inference both ways (reject over-widening AND catch true
  mismatches). `@ts-expect-error` on a genuinely-wrong shape is the two-sided proof the gate
  needs.
- **`--extendedDiagnostics` `Instantiations`** (and `--generateTrace`) — the accepted
  instantiation-budget metric, used by DefinitelyTyped perf CI — the correct instrument for Q3.

---

## 8. Named risks & counterexamples

- **R-A (the flagged cast is removable, not lawful).** Keeping `parsers as unknown as
  Parser<unknown>[]` and documenting it as "variance" would be the self-deception the plane
  forbids — it dissolves entirely under a readonly-tuple `fuseAll` signature (§1a). The
  prototype MUST show the cast gone, not annotated.
- **R-B (arity-1 soundness, live).** `all(p)` typed `Parser<[V]>`, yields `V`; a consumer
  writing `const [x] = all(p).parse(s).value` compiles and crashes. Cure (ii) (type as
  `Parser<V>`) keeps byte-identity; cure (i) fixes runtime but breaks §4. Pick and record.
- **R-C (measuring the empty consumer).** Running Q3 / consumer-suite gates against the
  checked-out `tranche-u` value.js measures zero `all()` sites and falsely reports "green."
  Pin `master` or record native un-runnable.
- **R-D (`.d.ts` byte-diff false alarm).** Treating `.d.ts` churn as a byte-identity failure
  rejects the correct outcome — declarations MUST change. Fence the diff to `.js`/`.cjs`.
- **R-E (`ParserFunction` over-tightening).** Replacing `ParserState<any>` with a generic
  re-introduces variance friction at the one megamorphic IC (R4 §6c) and can perturb the
  emitted closures the V8 plane pins. Leave it, document it, exclude it from the leak count.
- **R-F (`const T` reddens a non-tuple call site).** `all(...someArray)` where the arg is a
  widened `Parser<X>[]` (not a literal tuple) now yields `Parser<X[]>` — correct, but a
  consumer relying on the old `any` widening to pass a heterogeneous non-tuple array reddens.
  Caught only by full `tsc` at consumer sites, never by unit tests (would land on `master`
  value.js if it is the consumer of record).
- **R-G (DropUndefined position unsoundness).** `AllValue = DropUndefined<Values<T>>` claims a
  present value at a position an absent `opt()` arm shifts away at runtime (§3a). Sound only
  when every `opt()` member is present; ship with the caveat comment + self-test differential;
  safe today because no surviving consumer exercises length-variable `all()`.

---

## 9. What a prototype MUST test to move this family

1. **The flagged cast is GONE.** The tightened `all` calls `fuseAll(parsers)` with no cast,
   `fuseAll` declared `<const T extends readonly Parser<unknown>[]>(parsers: T)`, body
   unchanged; `tsc --noEmit` clean. (Directly discharges Q1.)
2. **The tightened algebra infers precisely.** `types.ts` shared `Values`/`AnyValue`/`AllValue`;
   `all`/`any`/`fuseAll` on `<const T extends readonly Parser<unknown>[]>`;
   `then/or/chain/skip/sepBy` inputs tightened (`Parser<S>` / `Parser<unknown>`); `restore → T`.
   Prove with `test/types/inference.test-d.ts`: `all(string("a"), regex(/\d/))` ⟹
   `Parser<[string, string]>`; `any(...)` ⟹ union; `skip`/`sepBy` accept any element type;
   `@ts-expect-error` on a wrong shape still fires; `next<S>` unchanged (positive control).
3. **No new TS2589** — `tsc --noEmit` clean on the type-test project (and on `master` value.js's
   59 sites with the candidate installed, IF master is confirmed the corpus).
4. **Byte-identical `.js`/`.cjs`** — the §4 two-step (real vite + vite-plugin-dts):
   version-migration delta recorded, tightening delta byte-empty on `dist/*.{js,cjs}` under
   §1(c) cure (ii) + §1(a) readonly `fuseAll`.
5. **Instantiation budget** — `--extendedDiagnostics Instantiations` delta, baseline vs
   tightened, self-use leg (always) + native leg (only if `master` confirmed), disclosed with
   tsc version + signatures + corpus manifest; assert linear-in-call-sites (anchor: prototype's
   +2.9% self-test, re-measured post-cure).
6. **Arity-1 adjudication** — demonstrate the current `all(p)` type/runtime divergence, apply
   cure (ii), show it is byte-empty.
7. **DropUndefined disclosure** — a self-test differential recording the length-variance
   widening honestly (§3a), with the caveat comment landed.
8. **Consumer red-check (conditional on P2)** — full `tsc`/`vue-tsc` at `master` value.js's
   `all`/`any`/`dispatch` sites under the tightened tarball: does any site redden (R-F)? A break
   is a real semver signal, decided not assumed — but ONLY meaningful if `master` is the
   consumer of record; against the checked-out `tranche-u` tree the check is vacuous.
