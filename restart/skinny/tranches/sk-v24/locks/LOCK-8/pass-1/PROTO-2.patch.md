# LOCK-8 8B tightened type-plane — patch
# (parse-that/typescript is READ-ONLY; applied in a scratch copy per sk-v24 R4)

## NEW FILE: src/parse/types.ts
```typescript
// LOCK-8 8B — the tightened type plane.
//
// Pure type-level helpers shared by the leaf/combinator surface. NO runtime
// value lives here; every symbol is a `type`, so the module contributes ZERO
// bytes to the emitted bundle (the zero-cost invariant). The combinators route
// their return annotations through these names instead of re-spelling the same
// conditional/mapped types at each call site, so precision is defined once.

import type { Parser } from "./parser.js";

// The value a parser yields.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type ValueOf<P> = P extends Parser<infer V> ? V : never;

// Variadic mapped tuple: map a positional list of parsers to the tuple of the
// values they yield, preserving arity and order (the `all()` result shape).
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type Values<Ps extends readonly Parser<any>[]> = {
    [K in keyof Ps]: ValueOf<Ps[K]>;
};

// The union of the values a positional list of parsers may yield (the `any()`
// result shape). `Ps[number]` distributes the conditional in `ValueOf`.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type Union<Ps extends readonly Parser<any>[]> = ValueOf<Ps[number]>;

// Tail-recursive accumulator that drops `undefined` positions from a tuple —
// the type-level mirror of `all()`'s runtime drop-`undefined` compaction, so the
// static result shape matches the values actually pushed at runtime.
export type DropUndefined<
    T extends readonly unknown[],
    Acc extends readonly unknown[] = [],
> = T extends readonly [infer Head, ...infer Tail]
    ? [undefined] extends [Head]
        ? DropUndefined<Tail, Acc>
        : DropUndefined<Tail, [...Acc, Head]>
    : Acc;
```

## src/parse/leaf.ts (dispatch const-K reverted — see PROTO-2.md finding)
```diff
--- a/src/parse/leaf.ts	2026-07-03 16:45:35
+++ b/src/parse/leaf.ts	2026-07-16 04:14:54
@@ -1,5 +1,6 @@
 import { Parser } from "./parser.js";
 import type { ParserFunction } from "./parser.js";
+import type { Values, Union } from "./types.js";
 import type { ParserState, ParserContext, Span } from "./state.js";
 import { createParserContext } from "./state.js";
 import { mergeErrorState } from "./utils.js";
@@ -25,8 +26,8 @@
 }
 
 // eslint-disable-next-line @typescript-eslint/no-explicit-any
-export function any<T extends Array<Parser<any>>>(...parsers: T) {
-    type Result = T[number] extends Parser<infer V> ? V : never;
+export function any<const T extends Array<Parser<any>>>(...parsers: T): Parser<Union<T>> {
+    type Result = Union<T>;
     const n = parsers.length;
 
     // PT-B3 fusion (semantics-preserving): indexed `for` (no `for…of` iterator
@@ -151,11 +152,8 @@
 }
 
 // eslint-disable-next-line @typescript-eslint/no-explicit-any
-export function all<T extends Array<Parser<any>>>(...parsers: T) {
-    type ExtractValue<T extends ReadonlyArray<Parser<unknown>>> = {
-        [K in keyof T]: T[K] extends Parser<infer V> ? V : never;
-    };
-    type Result = ExtractValue<T>;
+export function all<const T extends Array<Parser<any>>>(...parsers: T): Parser<Values<T>> {
+    type Result = Values<T>;
 
     return makeParser(
         parsers.length === 1 ? parsers[0].parser : fuseAll<Result>(parsers),
@@ -273,7 +271,7 @@
 }
 
 // Step 2: string() with startsWith + single-char charCodeAt fast path
-export function string(str: string) {
+export function string<const S extends string>(str: S) {
     const len = str.length;
     const label = `"${str}"`;
 
@@ -309,7 +307,7 @@
     return makeParser(
         stringParser,
         createParserContext("string", undefined, str),
-    );
+    ) as Parser<S>;
 }
 
 // regex() with test()+substring() for zero-alloc default path,
```

## test-types/inference.test-d.ts (consumer-side inference assertions)
```typescript
// LOCK-8 8B — type-level inference assertions, evaluated from the CONSUMER side
// (imports the published `@mkbabb/parse-that` surface exactly as value.js does).
//
// GREEN under the tightened (candidate) types; RED under the 1.0.0 (baseline)
// types. The RED arm is the falsifier's positive control: it proves the
// assertions actually discriminate precision rather than passing vacuously.

import { all, any, string, regex, type Parser } from "@mkbabb/parse-that";

// Invariant type-equality (the standard `(<T>() => …)` trick).
type Equal<A, B> =
    (<T>() => T extends A ? 1 : 2) extends (<T>() => T extends B ? 1 : 2)
        ? true
        : false;
type Expect<T extends true> = T;

// (1) all(string("a"), regex(/\d/)) — sequence tuple, literal head.
//     candidate: Parser<["a", string]>   baseline: Parser<[string, string]>
const seq = all(string("a"), regex(/\d/));
type _Seq = Expect<Equal<typeof seq, Parser<["a", string]>>>;

// (2) any(string("a"), string("b")) — alternation union of literals.
//     candidate: Parser<"a" | "b">        baseline: Parser<string>
const alt = any(string("a"), string("b"));
type _Alt = Expect<Equal<typeof alt, Parser<"a" | "b">>>;

// (3) literal preservation on a lone leaf.
//     candidate: Parser<"px">             baseline: Parser<string>
const lit = string("px");
type _Lit = Expect<Equal<typeof lit, Parser<"px">>>;

// (4) wrong shape STILL rejects under both type planes (a non-Parser argument
//     violates `T extends Array<Parser<any>>`). @ts-expect-error passes iff the
//     error is present — the const-param tightening must not silently admit it.
// @ts-expect-error — a bare string is not a Parser
const _bad = all("not a parser", regex(/\d/));

export { seq, alt, lit };
```
