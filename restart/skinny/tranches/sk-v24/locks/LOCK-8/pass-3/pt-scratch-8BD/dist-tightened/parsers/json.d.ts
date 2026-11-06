import { Parser } from '../index.js';
export type JsonValue = string | number | boolean | null | JsonValue[] | {
    [key: string]: JsonValue;
};
/** Combinator JSON parser with leading/trailing whitespace trimming. */
export declare const jsonParser: Parser<any>;
//# sourceMappingURL=json.d.ts.map