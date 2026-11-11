import type { Parser } from "./parser.js";

/**
 * Tuple value-extraction for a static parser list: maps a
 * `ReadonlyArray<Parser<unknown>>` to the tuple of its element value types.
 * Lifted here (PT-E2 combinator-inference tightening) so `all()` (leaf.ts) and
 * the combinator core (parser.ts) share ONE canonical definition instead of the
 * two duplicated copies.
 */
export type ExtractValue<T extends ReadonlyArray<Parser<unknown>>> = {
    [K in keyof T]: T[K] extends Parser<infer V> ? V : never;
};
