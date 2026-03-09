/* tslint:disable */
/* eslint-disable */

export function analyze_grammar(text: string): any;

export function code_actions(text: string, start_offset: number, end_offset: number): any;

export function code_lens(text: string): any;

export function completions(text: string): any;

export function document_symbols(text: string): any;

export function folding_ranges(text: string): any;

export function format_bbnf(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;

export function format_bnf(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;

export function format_css(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;

export function format_ebnf(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;

export function format_json(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;

export function goto_definition(text: string, offset: number): any;

export function hover_at_offset(text: string, offset: number): any;

export function inlay_hints(text: string, start_line: number, end_line: number): any;

export function selection_ranges(text: string, offsets: Uint32Array): any;

export function semantic_tokens_full(text: string): any;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
    readonly memory: WebAssembly.Memory;
    readonly format_json: (a: number, b: number, c: number, d: number, e: number) => [number, number];
    readonly format_css: (a: number, b: number, c: number, d: number, e: number) => [number, number];
    readonly format_bnf: (a: number, b: number, c: number, d: number, e: number) => [number, number];
    readonly format_ebnf: (a: number, b: number, c: number, d: number, e: number) => [number, number];
    readonly format_bbnf: (a: number, b: number, c: number, d: number, e: number) => [number, number];
    readonly analyze_grammar: (a: number, b: number) => any;
    readonly hover_at_offset: (a: number, b: number, c: number) => any;
    readonly completions: (a: number, b: number) => any;
    readonly semantic_tokens_full: (a: number, b: number) => any;
    readonly inlay_hints: (a: number, b: number, c: number, d: number) => any;
    readonly goto_definition: (a: number, b: number, c: number) => any;
    readonly document_symbols: (a: number, b: number) => any;
    readonly folding_ranges: (a: number, b: number) => any;
    readonly selection_ranges: (a: number, b: number, c: number, d: number) => any;
    readonly code_actions: (a: number, b: number, c: number, d: number) => any;
    readonly code_lens: (a: number, b: number) => any;
    readonly __wbindgen_malloc: (a: number, b: number) => number;
    readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
    readonly __wbindgen_externrefs: WebAssembly.Table;
    readonly __wbindgen_free: (a: number, b: number, c: number) => void;
    readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;

/**
 * Instantiates the given `module`, which can either be bytes or
 * a precompiled `WebAssembly.Module`.
 *
 * @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
 *
 * @returns {InitOutput}
 */
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
 * If `module_or_path` is {RequestInfo} or {URL}, makes a request and
 * for everything else, calls `WebAssembly.instantiate` directly.
 *
 * @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
 *
 * @returns {Promise<InitOutput>}
 */
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;
