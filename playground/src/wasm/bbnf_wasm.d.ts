/* tslint:disable */
/* eslint-disable */

export function analyze_grammar(text: string): any;

export function code_actions(text: string, start_offset: number, end_offset: number): any;

export function code_lens(text: string): any;

/**
 * Compile a BBNF grammar string into a bytecode program.
 * Returns a numeric handle for use with `parse_with_grammar` and `free_grammar`.
 *
 * If `entry_rule` is provided and non-empty, it overrides the default entry rule
 * (which is the last rule in source order).
 */
export function compile_grammar(grammar: string, entry_rule?: string | null): number;

export function completions(text: string): any;

export function document_symbols(text: string): any;

export function find_references(text: string, offset: number): any;

export function folding_ranges(text: string): any;

export function format_bbnf(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;

export function format_bnf(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;

export function format_css(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;

export function format_document(text: string): any;

export function format_ebnf(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;

export function format_json(input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;

export function format_range(text: string, start_offset: number, end_offset: number): any;

/**
 * Format input using a previously compiled grammar's @pretty hints.
 * Returns the formatted string, or null if parsing fails or no pretty hints are defined.
 */
export function format_with_grammar(handle: number, input: string, max_width: number, indent: number, use_tabs: boolean): string | undefined;

/**
 * Free a compiled grammar, releasing its memory.
 */
export function free_grammar(handle: number): void;

export function full_sync(text: string): any;

export function goto_definition(text: string, offset: number): any;

export function hover_at_offset(text: string, offset: number): any;

/**
 * Initialize panic hook for better error messages in WASM.
 */
export function init_panic_hook(): void;

export function inlay_hints(text: string, start_line: number, end_line: number): any;

export function on_type_format(text: string, offset: number): any;

/**
 * Parse input, returning only success and offset — no tree serialization.
 * Use this when you only need to validate or measure raw parse throughput.
 */
export function parse_check(handle: number, input: string): any;

/**
 * Parse input using a previously compiled grammar.
 * Returns a JSON-serializable parse result.
 */
export function parse_with_grammar(handle: number, input: string): any;

export function prepare_rename(text: string, offset: number): any;

export function rename_symbol(text: string, offset: number, new_name: string): any;

export function selection_ranges(text: string, offsets: Uint32Array): any;

export function semantic_tokens_full(text: string): any;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
    readonly memory: WebAssembly.Memory;
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
    readonly find_references: (a: number, b: number, c: number) => any;
    readonly prepare_rename: (a: number, b: number, c: number) => any;
    readonly rename_symbol: (a: number, b: number, c: number, d: number, e: number) => any;
    readonly format_document: (a: number, b: number) => any;
    readonly format_range: (a: number, b: number, c: number, d: number) => any;
    readonly on_type_format: (a: number, b: number, c: number) => any;
    readonly full_sync: (a: number, b: number) => any;
    readonly init_panic_hook: () => void;
    readonly format_json: (a: number, b: number, c: number, d: number, e: number) => [number, number];
    readonly format_css: (a: number, b: number, c: number, d: number, e: number) => [number, number];
    readonly format_bnf: (a: number, b: number, c: number, d: number, e: number) => [number, number];
    readonly format_ebnf: (a: number, b: number, c: number, d: number, e: number) => [number, number];
    readonly format_bbnf: (a: number, b: number, c: number, d: number, e: number) => [number, number];
    readonly compile_grammar: (a: number, b: number, c: number, d: number) => [number, number, number];
    readonly parse_with_grammar: (a: number, b: number, c: number) => [number, number, number];
    readonly parse_check: (a: number, b: number, c: number) => [number, number, number];
    readonly format_with_grammar: (a: number, b: number, c: number, d: number, e: number, f: number) => [number, number];
    readonly free_grammar: (a: number) => void;
    readonly analyze_grammar: (a: number, b: number) => any;
    readonly __wbindgen_malloc: (a: number, b: number) => number;
    readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
    readonly __wbindgen_free: (a: number, b: number, c: number) => void;
    readonly __wbindgen_exn_store: (a: number) => void;
    readonly __externref_table_alloc: () => number;
    readonly __wbindgen_externrefs: WebAssembly.Table;
    readonly __externref_table_dealloc: (a: number) => void;
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
