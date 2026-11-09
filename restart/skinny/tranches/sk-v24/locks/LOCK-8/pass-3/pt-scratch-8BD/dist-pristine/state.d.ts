import { Parser } from './parser.js';
/**
 * Zero-copy span: stores start/end offsets into the source string.
 * Call toString(src) for lazy materialization of the substring.
 */
export interface Span {
    start: number;
    end: number;
}
export declare function spanToString(span: Span, src: string): string;
export declare function mergeSpans(a: Span, b: Span): Span;
export interface Suggestion {
    kind: "unclosed-delimiter" | "trailing-content";
    message: string;
    openOffset?: number;
}
export interface SecondarySpan {
    offset: number;
    label: string;
}
export declare class ParserState<T = unknown> {
    src: string;
    value: T;
    offset: number;
    isError: boolean;
    furthest: number;
    /**
     * Furthest-offset error tracking, threaded per-parse (the Rust port's
     * `state.furthest_offset` model). `expected` is the accumulated label set
     * at `furthest`; `suggestions`/`secondarySpans` are the diagnostic extras
     * collected at `furthest` when diagnostics are enabled.
     */
    expected?: string[];
    suggestions: Suggestion[];
    secondarySpans: SecondarySpan[];
    constructor(src: string, value?: T, offset?: number, isError?: boolean, furthest?: number);
    ok<S>(value: S, offset?: number): ParserState<S>;
    err<S>(value?: S, offset?: number): ParserState<S>;
    from<S>(value: S, offset?: number): ParserState<S>;
    save(): {
        offset: number;
        value: T;
    };
    restore(saved: {
        offset: number;
        value: any;
    }): this;
    /** Type-erased value setter — single choke point for the mutable-state cast pattern. */
    unsafeSetValue(value: unknown): void;
    /** Type-erased parser invocation via .call() — single choke point for combinator type casts. */
    unsafeCall(parser: Parser<unknown>): void;
    /** Type-erased raw parser invocation via .parser() — for internal combinator plumbing. */
    unsafeCallRaw(parser: Parser<unknown>): void;
    clone(): ParserState<T>;
    getColumnNumber(): number;
    getLineNumber(): number;
    /** Returns 1-based line and 0-based column for any offset. */
    getLineAndColumn(offset?: number): {
        line: number;
        column: number;
    };
    toString(): string;
}
export declare const parserNames: readonly ["string", "regex", "then", "or", "chain", "map", "many", "lazy", "memoize", "mergeMemo", "not", "minus", "skip", "next", "trim", "trimWhitespace", "whitespace", "wrap", "sepBy", "any", "all", "opt", "eof", "dispatch", "debug", "mapState", "recover", "peek", "lookAhead"];
export type ParserContext = {
    name?: (typeof parserNames)[number];
    parser?: Parser<unknown>;
    args?: unknown[];
};
export declare function createParserContext(name: (typeof parserNames)[number], parser: Parser<unknown> | undefined, ...args: unknown[]): {
    name: "string" | "regex" | "then" | "or" | "chain" | "map" | "many" | "lazy" | "memoize" | "mergeMemo" | "not" | "minus" | "skip" | "next" | "trim" | "trimWhitespace" | "whitespace" | "wrap" | "sepBy" | "any" | "all" | "opt" | "eof" | "dispatch" | "debug" | "mapState" | "recover" | "peek" | "lookAhead";
    parser: Parser<unknown> | undefined;
    args: unknown[];
};
//# sourceMappingURL=state.d.ts.map