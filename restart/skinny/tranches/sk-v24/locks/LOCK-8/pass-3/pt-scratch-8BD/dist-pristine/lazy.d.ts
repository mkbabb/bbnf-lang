import { Parser } from './parser.js';
import { ParserState } from './state.js';
export declare function getLazyParser<T>(fn: () => T): T;
export declare function createLazyCached<T>(fn: () => Parser<T>): (state: ParserState<T>) => ParserState<T>;
/**
 * Method decorator that wraps a parser-returning method in a lazy parser.
 * Defers parser construction until first invocation, then caches.
 */
export declare function lazy<T>(target: unknown, _propertyName: string, descriptor: TypedPropertyDescriptor<() => any>): void;
//# sourceMappingURL=lazy.d.ts.map