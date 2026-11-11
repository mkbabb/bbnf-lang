import { Parser } from '../index.js';
/** Parse escaped characters: backslash followed by common escape chars or \\uXXXX. */
export declare function escapedString(): Parser<string>;
/** Parse a quoted string with escape handling. */
export declare function quotedString(quote?: string): Parser<string>;
/** Parse a number (integer or decimal with optional exponent). */
export declare function numberParser(): Parser<number>;
//# sourceMappingURL=utils.d.ts.map