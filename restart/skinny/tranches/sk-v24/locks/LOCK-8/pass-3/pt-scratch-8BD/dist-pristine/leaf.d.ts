import { Parser } from './parser.js';
import { ParserState } from './state.js';
export declare function eof<T>(): Parser<unknown>;
export declare function any<T extends Array<Parser<any>>>(...parsers: T): Parser<T[number] extends Parser<infer V> ? V : never>;
/**
 * O(1) first-character dispatch for alternation.
 * Maps ASCII characters to parsers for instant lookup instead of
 * sequential trial-and-error like any().
 *
 * @param table - Maps characters (or char ranges) to parsers.
 *   Keys can be single chars ("a"), ranges ("0-9"), or multi-char ("tf" = 't' or 'f").
 *
 * PT-Q5 RETRACT note: a speculative 2nd-byte `subTable` widening shipped in
 * 0.12.0 to flatten the residual megamorphism of a deep first-char bucket
 * (value.js's `c`-bucket: calc/clamp/cos/conic/cubic). It had ZERO production
 * consumers — value.js's only `dispatch()` calls pass NO subTable — so the
 * widening was a no-consumer perf seam gated against a SYNTHETIC corpus no
 * consumer runs. Per the terminal-or-KILL disposition it is RETRACTED in 0.13.0:
 * the seam is a localized revert (no consumer passed the 2nd arg, so removing it
 * breaks no published contract). If value.js's coordinated Q session measures an
 * on-path win, the widening is re-introduced as the CONSUME upgrade with the perf
 * gate re-anchored to value.js's real `c`-bucket grammar — not before.
 */
export declare function dispatch<T>(table: Record<string, Parser<T>>): Parser<T>;
export declare function all<T extends Array<Parser<any>>>(...parsers: T): Parser<{ [K in keyof T]: T[K] extends Parser<infer V> ? V : never; }>;
export declare function string(str: string): Parser<string>;
export declare function regex(r: RegExp, matchFunction?: (match: RegExpMatchArray | null) => string | null): Parser<string>;
export declare const trimStateWhitespace: <T>(state: ParserState<T>) => ParserState<T>;
export declare let whitespace: ReturnType<typeof regex>;
export declare function _initWhitespace(): void;
//# sourceMappingURL=leaf.d.ts.map