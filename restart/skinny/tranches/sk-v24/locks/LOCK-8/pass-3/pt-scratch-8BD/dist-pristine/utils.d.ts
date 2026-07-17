import { ParserState, Suggestion, SecondarySpan } from './state.js';
export type { Suggestion, SecondarySpan } from './state.js';
export declare function enableDiagnostics(): void;
export declare function disableDiagnostics(): void;
export declare function isDiagnosticsEnabled(): boolean;
export declare function mergeErrorState(state: ParserState<unknown>, label?: string): ParserState<unknown>;
export declare function addSuggestion(state: ParserState<unknown>, suggestion: Suggestion): void;
export declare function addSecondarySpan(state: ParserState<unknown>, offset: number, label: string): void;
/**
 * Report an unclosed delimiter diagnostic. Used by wrap().
 */
export declare function reportUnclosedDelimiter(state: ParserState<unknown>, openText: string, openOffset: number): void;
export interface Diagnostic {
    offset: number;
    furthestOffset: number;
    line: number;
    column: number;
    expected: string[];
    suggestions: Suggestion[];
    secondarySpans: SecondarySpan[];
    found: string;
}
/**
 * Snapshot the parse state's current error tracking into a Diagnostic object,
 * push it to the collection, then reset the state's error tracking so the next
 * error starts fresh.
 */
export declare function collectDiagnostic(state: ParserState<unknown>, errorOffset: number): void;
/** Reset the per-parse error tracking on a state. */
export declare function resetErrorState(state: ParserState<unknown>): void;
export declare function getCollectedDiagnostics(): readonly Diagnostic[];
export declare function clearCollectedDiagnostics(): void;
export declare function popLastDiagnostic(): Diagnostic | undefined;
/** Skip ASCII whitespace (charCode <= 32) in place. No comment handling. */
export declare function skipWhitespace(state: ParserState<unknown>): void;
/** Skip whitespace and CSS block comments in place (memchr-style closing-token
 *  scan via indexOf). An unterminated comment stops the scan at its start. */
export declare function skipBlockComments(state: ParserState<unknown>): void;
//# sourceMappingURL=utils.d.ts.map