import { Parser } from './parser.js';
export declare function getCijKey(parser: Parser<unknown>, offset: number): number;
/** A finished parse result snapshot at a memo position. */
interface Answer {
    offset: number;
    value: unknown;
    isError: boolean;
}
/** In-progress left-recursion marker held in a memo cell during evaluation. */
interface LR {
    seed: Answer;
    parser: Parser<unknown>;
    head: Head | undefined;
    next: LR | undefined;
}
/** Head record for the rule heading a left-recursion at a given position. */
interface Head {
    parser: Parser<unknown>;
    involvedSet: Set<number>;
    evalSet: Set<number>;
}
/** A memo cell holds either a finished Answer or an in-progress LR marker. */
interface MemoCell {
    ans: Answer | LR;
}
/** A frozen snapshot of the full module-global packrat state. */
interface PackratEpoch {
    memo: Map<number, MemoCell>;
    heads: Map<number, Head>;
    growing: Map<number, number>;
    lrStack: LR | undefined;
    currentSrc: string | undefined;
}
/**
 * packratEnter — open a fresh packrat epoch at the parseState entry boundary.
 * Returns the OUTER epoch's snapshot, which the caller hands back to
 * packratExit() to restore the parent. Installs empty tables so the child parse
 * starts clean (cross-input sound) and cannot see the parent's in-progress cells.
 *
 * NO-OP UNTIL ARMED (S.H1): when no memoize()/mergeMemos() has been constructed
 * the packrat machinery is inert, so entering an epoch would allocate three Maps
 * for nothing. In that state packratEnter returns `null` and touches no globals;
 * packratExit(null) is likewise a no-op. Once the latch arms (first makeMemoized)
 * this is the byte-identical epoch open it always was.
 */
export declare function packratEnter(): PackratEpoch | null;
/**
 * packratExit — close the current epoch and restore the parent's snapshot. Called
 * from a `finally` so the parent's tables are restored even if the child parse
 * threw mid-grow (the try/finally unwind hardening — the LR machinery the child
 * dirtied is discarded wholesale with its epoch's tables, never left dangling on
 * the parent's stacks).
 *
 * NULL-GUARD (S.H1): a `null` saved value means the matching packratEnter ran
 * unarmed (no epoch was opened), so there is nothing to restore — return early.
 */
export declare function packratExit(saved: PackratEpoch | null): void;
export declare function resetPackrat(): void;
export declare function memoize<T>(parser: Parser<T>): Parser<T>;
export declare function mergeMemos<T>(parser: Parser<T>): Parser<T>;
export {};
//# sourceMappingURL=packrat.d.ts.map