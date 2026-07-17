import { ParserState } from './state.js';
import { Parser } from './parser.js';
import { Diagnostic } from './utils.js';
export declare function summarizeLine(line: string, columnNum?: number): string;
export declare function formatExpected(expected: readonly string[]): string;
export declare function addCursor(state: ParserState<unknown>, cursor?: string, error?: boolean): string;
export declare function statePrint(state: ParserState<unknown>, name?: string, parserString?: string): string;
/**
 * Format a single collected Diagnostic into compiler-style output,
 * using the same visual style as statePrint but from a snapshot.
 */
export declare function formatDiagnostic(d: Diagnostic, src: string): string;
/**
 * Format all collected diagnostics with blank-line separators
 * and a summary line.
 */
export declare function formatAllDiagnostics(diagnostics: readonly Diagnostic[], src: string): string;
export declare function parserPrint(parser: Parser<unknown>): string;
export declare function parserDebug<T>(parser: Parser<T>, name?: string, recursivePrint?: boolean, logger?: (...s: unknown[]) => void): Parser<T>;
//# sourceMappingURL=debug.d.ts.map