/**
 * WASM-accelerated Grammar API.
 *
 * Wraps the WASM bytecode VM exports into a clean Grammar class:
 *
 * ```ts
 * import { Grammar, initWasm } from "@mkbabb/bbnf-lang";
 * import init, * as wasm from "bbnf-wasm";
 *
 * await init();                         // load WASM module
 * initWasm(wasm);                       // register bindings
 *
 * const grammar = Grammar.compile(`
 *   value = string | number | array | object ;
 *   ...
 * `);
 * const tree = grammar.parse('{"key": "value"}');
 * grammar.free();
 * ```
 */

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/** A parse tree value returned by the bytecode VM. */
export type ParseValue =
    | { type: "Span"; start: number; end: number }
    | {
          type: "Tagged";
          tag: string;
          start: number;
          end: number;
          children: ParseValue[];
      }
    | { type: "Array"; items: ParseValue[] }
    | { type: "Nil" };

/** Result of parsing input with a compiled grammar. */
export interface ParseResult {
    success: boolean;
    offset: number;
    value: ParseValue | null;
}

/** Options for formatting with a compiled grammar. */
export interface FormatOptions {
    /** Maximum line width before breaking. Default: 80. */
    maxWidth?: number;
    /** Indentation width (spaces or tab stops). Default: 4. */
    indent?: number;
    /** Use tabs instead of spaces. Default: false. */
    useTabs?: boolean;
}

/**
 * The shape of the WASM bindings that must be provided to `initWasm()`.
 * This matches the exports from the `bbnf-wasm` package.
 */
export interface WasmBindings {
    compile_grammar(grammar: string): number;
    parse_with_grammar(handle: number, input: string): ParseResult;
    format_with_grammar(
        handle: number,
        input: string,
        max_width: number,
        indent: number,
        use_tabs: boolean,
    ): string | null;
    free_grammar(handle: number): void;
}

// ---------------------------------------------------------------------------
// Singleton bindings store
// ---------------------------------------------------------------------------

let wasmBindings: WasmBindings | null = null;

/**
 * Register the WASM bindings for use by `Grammar`.
 *
 * The consumer is responsible for loading and initializing the WASM module.
 * Pass the module's exports object (which must include `compile_grammar`,
 * `parse_with_grammar`, and `free_grammar`).
 *
 * ```ts
 * import init, * as wasm from "bbnf-wasm";
 * await init();
 * initWasm(wasm);
 * ```
 */
export function initWasm(bindings: WasmBindings): void {
    wasmBindings = bindings;
}

/** Check whether WASM bindings have been registered. */
export function isWasmReady(): boolean {
    return wasmBindings !== null;
}

function getBindings(): WasmBindings {
    if (!wasmBindings) {
        throw new Error(
            "WASM bindings not registered — call initWasm(wasmModule) first",
        );
    }
    return wasmBindings;
}

// ---------------------------------------------------------------------------
// Grammar class
// ---------------------------------------------------------------------------

/**
 * A compiled BBNF grammar backed by the WASM bytecode VM.
 *
 * Use `Grammar.compile(source)` to create an instance, then call `.parse(input)`
 * to run the parser. Call `.free()` when done to release WASM-side memory.
 *
 * Implements `Symbol.dispose` for `using` syntax (TC39 Explicit Resource Management).
 */
export class Grammar {
    #handle: number;
    #freed = false;

    private constructor(handle: number) {
        this.#handle = handle;
    }

    /** Compile a BBNF grammar source string into a bytecode program. */
    static compile(source: string): Grammar {
        const handle = getBindings().compile_grammar(source);
        return new Grammar(handle);
    }

    /** Parse input text, returning the parse tree. */
    parse(input: string): ParseResult {
        if (this.#freed) {
            throw new Error("Grammar has been freed");
        }
        return getBindings().parse_with_grammar(this.#handle, input);
    }

    /** Format input text using the grammar's @pretty directives. */
    format(input: string, options?: FormatOptions): string | null {
        if (this.#freed) {
            throw new Error("Grammar has been freed");
        }
        const maxWidth = options?.maxWidth ?? 80;
        const indent = options?.indent ?? 4;
        const useTabs = options?.useTabs ?? false;
        return getBindings().format_with_grammar(
            this.#handle,
            input,
            maxWidth,
            indent,
            useTabs,
        );
    }

    /** Release WASM-side memory. Safe to call multiple times. */
    free(): void {
        if (this.#freed) return;
        this.#freed = true;
        getBindings().free_grammar(this.#handle);
    }

    /** Whether this grammar has been freed. */
    get isFreed(): boolean {
        return this.#freed;
    }

    /** The numeric handle for advanced usage. */
    get handle(): number {
        return this.#handle;
    }

    [Symbol.dispose](): void {
        this.free();
    }
}
