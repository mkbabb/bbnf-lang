export { Parser, type ParserFunction } from './parser.js';
export { ParserState, createParserContext, spanToString, mergeSpans, } from './state.js';
export type { ParserContext, Span } from './state.js';
export { getLazyParser, createLazyCached, lazy } from './lazy.js';
export { eof, any, dispatch, all, string, regex, trimStateWhitespace, whitespace, } from './leaf.js';
export { containsDelimiter, splitBalanced } from './split.js';
//# sourceMappingURL=core.d.ts.map