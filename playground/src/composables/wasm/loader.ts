type WasmModule = typeof import("../../wasm/bbnf_wasm");

let wasmModule: WasmModule | null = null;
let wasmInitPromise: Promise<WasmModule> | null = null;

/** Ensure the WASM module is loaded (singleton, lazy). */
export async function ensureWasmLoaded(): Promise<WasmModule> {
    if (wasmModule) return wasmModule;
    if (wasmInitPromise) return wasmInitPromise;

    wasmInitPromise = (async () => {
        const mod = await import("../../wasm/bbnf_wasm");
        await mod.default();
        wasmModule = mod;
        return mod;
    })();

    return wasmInitPromise;
}

/** Get the already-loaded WASM module (throws if not yet loaded). */
export function getWasmModule(): WasmModule {
    if (!wasmModule) {
        throw new Error("WASM module not loaded — call ensureWasmLoaded() first");
    }
    return wasmModule;
}
