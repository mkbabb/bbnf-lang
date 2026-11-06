# LOCK-8 family 8B — TYPE-LEVEL TOTALITY (research, pass 1)

*The TS type system carries the grammar: full `ExtractValue` inference, a typed combinator algebra
with no `Parser<any>` anywhere, diagnostics typed by position, compile-time inference mirroring the
zero-runtime-cost law.* This is parse-that's OWN inference story — distinct from the Rust HM/CSP
solver (LOCK-1). The plane is `type`/generic-signature only; the emitted JS is byte-identical, so
the constitution's clause-v zero-runtime-cost invariant holds by construction and is *proven*, not
asserted, by an empty `dist/` semantic diff.

---

## 1. The root finding — the phantom-`T` boundary severs all inference

`ParserFunction<T>` is declared `(val: ParserState<any>) => ParserState<any>` at
`typescript/src/parse/parser.ts:14-16` — a structural type that **discards its `T` parameter
entirely** (T is phantom; every `ParserFunction<A>` is identical to every `ParserFunction<B>`).
Because the class is `Parser<T = string>` with constructor `parser: ParserFunction<T>`
(`parser.ts:24-32`), the pervasive combinator idiom `new Parser(fn as ParserFunction<[T,S]>, ctx)`
**cannot propagate `[T,S]` into `Parser<[T,S]>`**: TS unifies two structurally-identical
`ParserFunction`s, finds no inference candidate, and falls to the default `string`. The
`as ParserFunction<[T,S]>` casts (parser.ts:100,119,141,157; leaf.ts:57,72) are cosmetic. The only
reason `all()` types at all is the *trailing force-cast* `... as Parser<Result>` (leaf.ts:163) that
re-asserts the computed type after the phantom boundary has already dropped it.

Consequence enumerated: `then` (parser.ts:81), `or` (105), `chain` (124), `map` (146), `opt` (234,
returns `Parser<string>` not `Parser<T|undefined>`), `skip`, `next`, `wrap`, `trim`, `many`,
`sepBy` carry NO precise return annotation — their inferred return type is the `string` default,
not the true value type. This is a deeper defect than PT-3/PT-E2's enumerated `Parser<any>` leaks:
those close the *inputs*; 8B must close the *outputs*, and the output leak is systemic, rooted in
one phantom type.

**The lawful cure (the mechanism):** keep `ParserFunction<T>` phantom-and-erased at the
implementation boundary — the megamorphic `.parser(state)` IC (R4 §6c) genuinely needs one
invariant call shape, and this is the single legitimate bounded `any` (documented, NOT counted a
leak) — but give **every public combinator an explicit precise return annotation**, so inference
flows through the *signatures* while the *runtime pointer* stays monomorphic. Types and codegen are
decoupled: the type plane is a parallel, erasable structure over the same closures.

## 2. The mechanism made concrete — the typed combinator algebra

### 2a. One type-utility home (`src/parse/types.ts`, new, tiny — DRY)

`ExtractValue<T>` is defined twice today (identically): `parser.ts:9-11` and re-declared *inside*
`all()` at `leaf.ts:155-157`. Consolidate to one exported module consumed by both:

- `ValueOf<P> = P extends Parser<infer V> ? V : never`
- `Values<Ps extends readonly Parser<unknown>[]> = { [K in keyof Ps]: ValueOf<Ps[K]> }` — a
  **variadic mapped tuple** (TS 4.0 variadic tuple types) that preserves BOTH positional value
  types AND tuple length. This is the enabling primitive: `all(a,b,c) → Parser<[A,B,C]>`.
- `Union<Ps> = ValueOf<Ps[number]>` for `any`/`dispatch`.
- `DropUndefined<Tup>` — a tail-recursive accumulator filter (see §5 counterexample R1).

### 2b. Precise signatures — the full algebra

- `all<const Ps extends readonly Parser<unknown>[]>(...ps: Ps): Parser<DropUndefined<Values<Ps>>>`
  (leaf.ts:154) — variadic tuple result; drop-undefined mirrored (§5 R1).
- `any<const Ps extends readonly Parser<unknown>[]>(...ps: Ps): Parser<Union<Ps>>` (leaf.ts:28).
- `fuseAll(parsers: readonly Parser<unknown>[])` (leaf.ts:179) — internal, `unknown` not `any`.
- `then<S>(next: Parser<S>): Parser<[T, S]>` — DROP the `S | T` widening (parser.ts:81); the union
  defeats positional inference and is unsound (`then` returns a 2-tuple, never `S|T`).
- `or<S>(other: Parser<S>): Parser<T | S>` (parser.ts:105).
- `chain<S>(fn: (value: T) => Parser<S>): Parser<S>` (parser.ts:124) — drop `S | T`.
- `map<S>(fn: (v: T) => S): Parser<S>`; `opt(): Parser<T | undefined>` (parser.ts:234);
  `many(): Parser<T[]>`; `sepBy<S>(sep: Parser<S>): Parser<T[]>`; `skip<S>(p: Parser<S>): Parser<T>`;
  `next<S>(p: Parser<S>): Parser<S>`; `wrap<L,R>(l,r): Parser<T>`; `trim(): Parser<T>`.
- `restore(saved: { offset: number; value: T }): this` (state.ts:79) — typed on the state's own
  `T`, not `any`.

### 2c. Literal precision — the grammar's alphabet enters the types (TS 5.0 `const` params)

`string<const S extends string>(s: S): Parser<S>` (leaf.ts:276) so `string("+")` infers
`Parser<"+">`, not `Parser<string>`. `dispatch<const T extends Record<string, Parser<unknown>>>`
(leaf.ts:100) → union of the table's precise value types. This is literally "the type system
carries the grammar": `all(string("rgb"), string("("), num, string(")"))` yields
`Parser<["rgb", "(", number, ")"]>` — the literal skeleton is checkable at compile time, enabling
exhaustiveness on value.js's function-name buckets. A non-`const` defaulting path is retained for
`string(dynamicVar)` (§5 R4).

### 2d. Diagnostics typed by position — the tractable form

The charter's frontier clause. Two readings, and the pass-1 verdict:

- **Result-union typing (ADOPT).** Type the parse *outcome* as a discriminated union carrying
  success-by-position: `ParseResult<T> = { ok: true; value: T; offset: number } | { ok: false;
  diagnostics: readonly Diagnostic[] }`. Consumers must narrow on `.ok` — failure and its
  diagnostics are *in the type*, positioned in the union, not a runtime-only surprise. This is the
  direct type-level consumer of PT-E1/W2's per-parse-scoped diagnostics (PT-3.md W2): the scoped
  result IS the typed surface. `Diagnostic.offset`/`SecondarySpan.offset` (state.ts:31-34) stay
  `number`, optionally branded `Offset = number & { __brand: "offset" }` for API hygiene.
- **Expected-label typing `Parser<T, E>` (BANK — likely negative result).** A second type param
  carrying the union of literal `expected` labels a parser can emit (`string("a") → E = '"a"'`).
  Elegant but (i) explodes every type into two exponential dimensions, (ii) has thin consumer value
  (value.js/kf read diagnostics at runtime, not compile time), (iii) is the prime TS2589 blowup
  candidate (§5 R2). Pass-1 recommendation: prototype it to *measure* the instantiation cost, then
  bank it as a named negative result unless a consumer demands it (Y.13 — consumer-less substrate
  forbidden). The honest default is the single-`T` `Parser<T>` + result-union.

## 3. Integration seams (path:line, READ-ONLY parse-that; land via letter BBNF-PT-G only)

| seam | file:line | change |
|---|---|---|
| duplicated `ExtractValue` | parser.ts:9-11 · leaf.ts:155-157 | → import from new `src/parse/types.ts` |
| phantom `ParserFunction` | parser.ts:14-16 | keep erased+documented; add precise method annotations |
| `S\|T` widenings | parser.ts:81,105,124 | drop union; `then→[T,S]`, `or→T\|S`, `chain→S` |
| `opt` untyped | parser.ts:234-249 | `: Parser<T \| undefined>` |
| `any`/`all` generics | leaf.ts:28,154 | `Array<Parser<any>>` → `readonly Parser<unknown>[]` + `const` |
| `fuseAll` | leaf.ts:179 | `Array<Parser<any>>` → `readonly Parser<unknown>[]` |
| `string`/`dispatch` literals | leaf.ts:276,100 | `const` type params for literal precision |
| `restore` `value:any` | state.ts:79 | typed on `T` |
| arity-unrolled arms | leaf.ts:39-57,183-251 | runtime shape UNTOUCHED (V8 monomorph, W4-guarded); gain positional types only |

New: `src/parse/types.ts` (utility home) and `test/types/inference.test-d.ts` (the type-level
suite). No runtime file changes beyond signature edits — the closures are inert to types.

## 4. Prior art (citations)

- **TS 4.0 variadic tuple types** — the enabler for `all(...) → tuple` and length-preserving
  `Values<Ps>` (TS 4.0 release notes; fettblog "Variadic Tuple Types Preview"; microsoft/TypeScript
  #5453 "Variadic Kinds"). Without it, N-ary sequencing cannot be typed positionally.
- **TS 4.1 template-literal + recursive conditional types** — type-level string manipulation; the
  substrate for literal-precise `string<const S>` composition.
- **TS 4.5 tail-recursion elimination in conditional types** — raises the recursion ceiling for
  accumulator-style type programs; the mitigation for §5 R2. **Hard cap = 1000 tail-recursive
  instantiations** (microsoft/TypeScript #49459; oneuptime "excessively deep TS2589"); non-tail
  nesting caps far lower (~50). Accumulator + chunking extends practical tuple work to ~2000
  elements (esveo "workaround max recursion depth"; DEV "deep recursive types"). This is the
  governing budget for 8B's ambition.
- **TS 5.0 `const` type parameters** — literal inference at call sites without `as const` at every
  use; the mechanism for §2c.
- **ArkType** — the SOTA proof that a full parser (its own type syntax) runs in the TS type system;
  its lesson is exactly 8B's risk surface: hand tail-recursive type-level state machines to dodge
  TS2589, and obsess over editor/hover perf. It is the existence proof AND the cautionary tale.
- **Zod (`z.infer`), io-ts (`t.TypeOf`), Effect Schema** — schema-to-type inference; Zod v4's
  rewrite for *type-checker* performance (fewer instantiations) is the direct precedent for 8B's
  instantiation-budget gate.
- **HotScript (Gabriel Vergnaud)** — defunctionalized higher-order type functions (`Fn`/`Pipe`/
  `Call`); the technique for a *composable* type-level combinator algebra if the naive signature
  approach hits limits. Paired with **Yallop & White, "Lightweight higher-kinded polymorphism"**
  (FLOPS 2014) — the `URItoKind` defunctionalization fp-ts/Effect use — should a truly HKT-abstract
  `Parser` constructor be wanted (pass-2 question; likely over-engineering for value.js).
- **Parsimmon/arcsecond/chevrotain** — the baseline: loosely-typed (`Parser<any>`-grade) or
  class-based; parse-that at 8B strength would exceed all of them on inference precision.

## 5. Named risks & counterexamples (the falsifiers)

- **R1 — the `opt()`-inside-`all()` parity hole (8B's hardest correctness problem).** `all` drops
  `undefined` values at runtime (leaf.ts:199,207,230,246,267). So `all(a, opt(b), c)` returns
  `[A,C]` when b fails but `[A,B,C]` when b matches — a **length-variable runtime tuple the static
  type cannot know**. `Parser<[A, B|undefined, C]>` LIES; `Parser<DropUndefined<[A,B|undefined,C]>>
  = Parser<[A,C]>` mistypes the b-present case. Three routes, one must be proven: (a) type-level
  `DropUndefined` filter + document `opt`-in-`all` as the one lawful widening; (b) redefine `all`
  to NOT drop undefined (breaking — value.js's 61 `all()` sites rely on the drop); (c) a distinct
  `allOptional` combinator. Pass-1 leans (a) with a measured audit of which value.js sites actually
  place `opt` inside `all`.
- **R2 — TS2589 recursion blowup on real grammars.** latex-paper's recursive document grammar and
  a left-recursive math grammar, instantiated through precise variadic tuples nested in
  `many`/`sepBy`, can exceed the 1000-instantiation cap. Non-negotiable that 8B does not turn a
  compiling consumer into an uncompilable one. Mitigation: tail-recursive accumulators, chunking,
  depth-limited `DropUndefined`; the `Parser<T,E>` frontier (§2d) is the prime offender and the
  reason it is banked.
- **R3 — editor-perf / hover explosion.** `Parser<[["rgb","("],number,")"]]>`-scale tuples make
  hover unreadable and IntelliSense slow (the ArkType/Zod-v4 lesson). Mitigate with a display-alias
  and `const`-opt-in, not mandatory literal precision everywhere.
- **R4 — `const` over-narrowing.** `string<const S>` breaks `string(runtimeVar: string)` if it
  over-narrows; a non-`const` default path is required.
- **R5 — `S\|T` removal + tightening is a semver signal.** Dropping the widening in then/or/chain,
  and precise `all`/`any`, is source-compatible for *well-typed* consumers (precise types are a
  superset of `any` for correct sites) but can reject a site that leaned on the slop. value.js's 61
  `all()` + latex-paper grammar are the falsifier; a real break escalates minor→major (PT-3.md R3).
- **R6 — non-clobber.** parse-that is READ-ONLY to bbnf; prototype on scratch copies; land only via
  letter BBNF-PT-G (owner / parse-that-side), never a bbnf-agent write (PT-3.md §2b, R6).

## 6. What a prototype MUST test (to move family 8B)

1. **P1 — precision + zero-cost + budget on real consumers.** Apply the tightened plane to scratch
   value.js (61 `all()`, dispatch c-bucket) + latex-paper grammars; assert (a) strictly more
   precise inferred types, (b) `vite build` `dist/` semantic diff EMPTY (zero runtime cost proven),
   (c) `tsc --noEmit` clean, (d) `tsc --extendedDiagnostics`/`--generateTrace` instantiation count
   ≤ a named ceiling (e.g. ≤1.5× baseline) and check-time ≤ named ms. **The instantiation-budget
   gate is 8B's signature falsifier** — type-level totality that blows the checker is a failed 8B.
2. **P2 — the R1 parity oracle.** On real value.js `opt`-in-`all` sites, does the chosen
   `DropUndefined` typing match runtime output for both b-present and b-absent, or produce a
   lying-type counterexample? A runtime differential test decides.
3. **P3 — the `.test-d.ts` suite** (`expect-type` + `@ts-expect-error`): `all(string("a"),
   regex(/\d/)) → Parser<[string,string]>` (or `["a", string]` with literals); `any(...)→union`;
   a `@ts-expect-error` on a genuinely wrong shape STILL errors (precision proven both ways —
   rejects over-widening AND catches true mismatches). RED on 1.0.0, GREEN after.
4. **P4 — the frontier probe.** Prototype `Parser<T,E>` expected-label typing on the value.js
   grammar; measure TS2589 distance and instantiation cost; bank it as a named negative result with
   the number unless a consumer (kf `ResolvedKeyframes.diagnostics`) is shown to want compile-time
   labels. The result-union typing (§2d ADOPT) is proven live against W2's scoped diagnostics.

## 7. Consumer contract (Y.13 — no consumer-less substrate)

The type plane's consumers are concrete: value.js's 61 `all()` sites + `dispatch`/`any`/`map`
grammar, latex-paper's 7 grammar modules, keyframes.js (transitive), and the `.test-d.ts` suite
itself. The tightening must be a **superset** for well-typed sites; the falsifier is the three
consumer suites green against a packed candidate (PT-3.md W5). This family is the *strongest form*
of PT-3's W3 (PT-E2): W3 tightens inputs and kills the enumerated `Parser<any>`; 8B additionally
closes the systemic OUTPUT leak at the phantom-`ParserFunction` boundary, threads literal precision,
and types the diagnostic result — carrying the grammar in the type system while the emitted JS, and
the V8 hot shapes (leaf.ts:39-57,183-251), stay byte-for-byte unchanged.
