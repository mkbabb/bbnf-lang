/**
 * Typed-algebra utilities (LOCK-8 8B-G3 home).
 *
 * Homes the type machinery the combinator algebra shares, killing the two
 * duplicate `ExtractValue` definitions (`parser.ts:9-11`, `leaf.ts:155-157`).
 *
 * The two erasures that LAWFULLY STAY are NOT here (research-B §3):
 *   - `ParserFunction<T>` — the invariant IC function-pointer (`parser.ts`);
 *   - `MemoCell.value: unknown` — the heterogeneous packrat memo table.
 * Everything else that carried `any` on the INPUT side is tightened to `unknown`.
 */
import type { Parser } from "./parser.js";

/** The sound value-tuple of a static parser list (the single `ExtractValue`). */
export type Values<Ps extends readonly Parser<unknown>[]> = {
    [K in keyof Ps]: Ps[K] extends Parser<infer V> ? V : never;
};

/**
 * Strip `undefined` from each member of a value tuple.
 *
 * `all()` DROPS undefined VALUES at runtime (`leaf.ts` fuseAll:
 * `if (state.value !== undefined) out[w++] = …; out.length = w`), so
 * `all(a, opt(b), c)` yields a length-VARIABLE array. A static tuple type cannot
 * express runtime length-variance, so the algebra ADOPTS the documented widening
 * (8B-G2 resolution): keep every position, strip `undefined` from the arm type.
 *
 * SOUND when every `opt()` member is present; a DOCUMENTED WIDENING when an
 * `opt()` arm is absent (the position shifts at runtime). Shippable precisely
 * because no surviving consumer exercises the length-variable case (research-B §2;
 * latex has zero `all`, its 11 `.opt()` are all outside `all`). The parse-that-side
 * self-test differential (gate 8B-#5) records the widening honestly.
 */
export type DropUndefined<T extends readonly unknown[]> = {
    [K in keyof T]: Exclude<T[K], undefined>;
};

/** The tightened public `all()` result type. */
export type AllValue<Ps extends readonly Parser<unknown>[]> =
    DropUndefined<Values<Ps>>;

/** The tightened public `any()` result type (ordered-choice union). */
export type AnyValue<Ps extends readonly Parser<unknown>[]> =
    Ps[number] extends Parser<infer V> ? V : never;
