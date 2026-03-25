import { ref, type Ref } from "vue";
import { useWasm } from "./wasm";
import type { WasmDebugSnapshot } from "./wasm/types";

export type { WasmDebugSnapshot as DebugSnapshot };

export interface UseDebugSessionOptions {
    grammarText: Ref<string>;
    inputText: Ref<string>;
    entryRuleOverride: Ref<string>;
}

export interface UseDebugSessionReturn {
    active: Ref<boolean>;
    snapshot: Ref<WasmDebugSnapshot | null>;
    breakpoints: Ref<Set<string>>;
    isRunning: Ref<boolean>;

    start(): Promise<void>;
    stop(): void;
    continueExec(): Promise<void>;
    stepRule(): Promise<void>;
    stepNode(): Promise<void>;
    toggleBreakpoint(ruleName: string): void;
}

export function useDebugSession(opts: UseDebugSessionOptions): UseDebugSessionReturn {
    const wasm = useWasm();

    const active = ref(false);
    const snapshot = ref<WasmDebugSnapshot | null>(null);
    const breakpoints = ref<Set<string>>(new Set());
    const isRunning = ref(false);

    let debugHandle: number | null = null;

    async function start() {
        stop();
        if (!opts.grammarText.value.trim() || !opts.inputText.value.trim()) return;

        try {
            const entryRule = opts.entryRuleOverride.value || undefined;
            debugHandle = await wasm.compileGrammarDebug(opts.grammarText.value, entryRule);
            active.value = true;
            snapshot.value = null;

            // Reset step index for the new session.
            await wasm.debugStep(debugHandle, opts.inputText.value, "reset", "[]");

            // Run to first breakpoint or step.
            await step("stepRule");
        } catch (e) {
            console.error("Debug session start failed:", e);
            stop();
        }
    }

    function stop() {
        if (debugHandle != null) {
            wasm.freeGrammar(debugHandle);
            debugHandle = null;
        }
        active.value = false;
        snapshot.value = null;
        isRunning.value = false;
    }

    async function step(mode: string) {
        if (debugHandle == null) return;
        isRunning.value = true;
        try {
            const bpJson = JSON.stringify([...breakpoints.value]);
            const result = await wasm.debugStep(
                debugHandle,
                opts.inputText.value,
                mode,
                bpJson,
            );
            snapshot.value = result;
            if (result.completed) {
                // Parse finished — session ends naturally.
                active.value = false;
            }
        } catch (e) {
            console.error("Debug step failed:", e);
        } finally {
            isRunning.value = false;
        }
    }

    async function continueExec() {
        await step("continue");
    }

    async function stepRule() {
        await step("stepRule");
    }

    async function stepNode() {
        await step("stepNode");
    }

    function toggleBreakpoint(ruleName: string) {
        const bp = new Set(breakpoints.value);
        if (bp.has(ruleName)) {
            bp.delete(ruleName);
        } else {
            bp.add(ruleName);
        }
        breakpoints.value = bp;
    }

    return {
        active,
        snapshot,
        breakpoints,
        isRunning,
        start,
        stop,
        continueExec,
        stepRule,
        stepNode,
        toggleBreakpoint,
    };
}
