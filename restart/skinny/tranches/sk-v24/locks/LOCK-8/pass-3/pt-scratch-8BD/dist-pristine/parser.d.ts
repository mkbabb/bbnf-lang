import { ParserState, ParserContext } from './state.js';
export type ParserFunction<T = string> = (val: ParserState<any>) => ParserState<any>;
export declare class Parser<T = string> {
    parser: ParserFunction<T>;
    context: ParserContext;
    id: number;
    state: ParserState<T> | undefined;
    flags: number;
    constructor(parser: ParserFunction<T>, context?: ParserContext);
    parseState(val: string): ParserState<T>;
    private parseStateInner;
    parse(val: string): T;
    then<S>(next: Parser<S | T>): Parser<[T, S]>;
    or<S>(other: Parser<S | T>): Parser<T | S>;
    chain<S>(fn: (value: T) => Parser<S | T>): Parser<S>;
    map<S>(fn: (value: T) => S, mapError?: boolean): Parser<S>;
    mapState<S extends T>(fn: (newState: ParserState<T>, oldState: ParserState<T>) => ParserState<S>): Parser<S>;
    skip<S>(parser: Parser<T | S>): Parser<T>;
    next<S>(parser: Parser<S>): Parser<S>;
    opt(): Parser<T | undefined>;
    not<S extends T>(parser?: Parser<S | T>): Parser<string>;
    /**
     * Set difference: match `this` only if `excluded` would NOT match at the
     * same starting position. Used for EBNF/BNF exception (`-`) semantics.
     */
    minus<S>(excluded: Parser<S>): Parser<T>;
    /**
     * Zero-width positive assertion: succeeds with `this`'s value when
     * `this` matches, but does NOT consume any input. The dual of
     * `not()` (no argument): where `not()` is zero-width negative
     * assertion, `peek()` is zero-width positive assertion.
     */
    peek(): Parser<T>;
    /**
     * Consuming positive lookahead: parse `this`, then check that
     * `lookahead` matches at the resulting position without consuming it.
     * Returns `this`'s value; the lookahead is zero-width.
     * Mirrors Rust's `look_ahead()`.
     */
    lookAhead<S>(lookahead: Parser<S>): Parser<T>;
    wrap<L, R>(start: Parser<L>, end: Parser<R>, discard?: boolean): Parser<T>;
    /**
     * Call the parser with flag-based pre/post processing.
     * Fast path: flags === 0 just calls parser directly.
     */
    call(state: ParserState<T>): ParserState<T>;
    trim<S>(parser?: Parser<S>, discard?: boolean): Parser<T>;
    many(min?: number, max?: number): Parser<T[]>;
    /**
     * Strictly interleaving: `elem (sep elem)*`. Never accepts a trailing
     * separator — trailing sep acceptance is a grammar concern.
     */
    sepBy<S>(sep: Parser<S | T>, min?: number, max?: number): Parser<T[]>;
    eof(): Parser<T>;
    /**
     * Error recovery combinator. On success, returns the result normally.
     * On failure, snapshots the current diagnostic into the collected
     * diagnostics list, then runs `sync` to skip past the bad content
     * and returns `sentinel`.
     *
     * This enables `many()` / `sepBy()` loops to keep going — each failed
     * element produces a diagnostic but doesn't halt the overall parse.
     */
    recover(sync: Parser<unknown>, sentinel: T): Parser<T>;
    debug(name?: string, recursivePrint?: boolean, logger?: (...s: unknown[]) => void): Parser<T>;
    toString(): string;
    static lazy<T>(fn: () => Parser<T>): Parser<T>;
}
//# sourceMappingURL=parser.d.ts.map