import { watch, type Ref } from "vue";
import { useRoute, useRouter, type LocationQuery, type LocationQueryValue } from "vue-router";
import { useDebounceFn } from "@vueuse/core";
import { getDemoById } from "@/demos";
import type { useWalkthrough } from "@/composables/useWalkthrough";

const LOCAL_STORAGE_KEY = "bbnf-playground-state";

function normalizeQueryValue(value: LocationQueryValue | LocationQueryValue[] | undefined) {
    if (Array.isArray(value)) return value[0] ?? undefined;
    return value ?? undefined;
}

function parseBooleanQuery(value: string | undefined) {
    if (value == null) return undefined;
    if (value === "1" || value === "true") return true;
    if (value === "0" || value === "false") return false;
    return undefined;
}

function parseNumberQuery(value: string | undefined) {
    if (value == null) return undefined;
    const parsed = Number.parseInt(value, 10);
    return Number.isFinite(parsed) ? parsed : undefined;
}

async function compressState(state: object): Promise<string> {
    const json = JSON.stringify(state);
    const stream = new Blob([json]).stream().pipeThrough(new CompressionStream("deflate-raw"));
    const compressed = await new Response(stream).arrayBuffer();
    const bytes = new Uint8Array(compressed);
    let b64 = btoa(String.fromCharCode(...bytes));
    return b64.replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

async function decompressState(hash: string): Promise<Record<string, string>> {
    let b64 = hash.replace(/-/g, "+").replace(/_/g, "/");
    while (b64.length % 4) b64 += "=";
    const binary = atob(b64);
    const bytes = Uint8Array.from(binary, (c) => c.charCodeAt(0));
    const stream = new Blob([bytes]).stream().pipeThrough(new DecompressionStream("deflate-raw"));
    const json = await new Response(stream).text();
    return JSON.parse(json);
}

export interface PlaygroundQueryOptions {
    grammarText: Ref<string>;
    inputText: Ref<string>;
    entryRuleOverride: Ref<string>;
    printerConfig: { maxWidth: number; indent: number; useTabs: boolean };
    walkthrough: ReturnType<typeof useWalkthrough>;
    leftTab?: Ref<string>;
    rightTab?: Ref<string>;
    exampleName?: Ref<string>;
    onHydrated: () => void;
}

export function usePlaygroundQuery(options: PlaygroundQueryOptions) {
    const route = useRoute();
    const router = useRouter();
    let lastDemoId: string | null = null;

    function saveToLocalStorage() {
        try {
            const state: Record<string, string> = {
                grammar: options.grammarText.value,
                input: options.inputText.value,
                entry: options.entryRuleOverride.value || "auto",
                width: String(options.printerConfig.maxWidth),
                indent: String(options.printerConfig.indent),
                tabs: options.printerConfig.useTabs ? "1" : "0",
            };
            if (options.leftTab) state.leftTab = options.leftTab.value;
            if (options.rightTab) state.rightTab = options.rightTab.value;
            if (options.exampleName) state.exampleName = options.exampleName.value;
            localStorage.setItem(LOCAL_STORAGE_KEY, JSON.stringify(state));
        } catch {
            // localStorage may be unavailable
        }
    }

    function loadFromLocalStorage(): Record<string, string> | null {
        try {
            const raw = localStorage.getItem(LOCAL_STORAGE_KEY);
            if (!raw) return null;
            return JSON.parse(raw);
        } catch {
            return null;
        }
    }

    const debouncedSave = useDebounceFn(saveToLocalStorage, 500);

    function applyState(state: Record<string, string | undefined>) {
        if (state.grammar != null) options.grammarText.value = state.grammar;
        if (state.input != null) options.inputText.value = state.input;
        if (state.entry != null) options.entryRuleOverride.value = state.entry === "auto" ? "" : state.entry;

        const qTabs = parseBooleanQuery(state.tabs);
        const qWidth = parseNumberQuery(state.width);
        const qIndent = parseNumberQuery(state.indent);

        if (qTabs != null) options.printerConfig.useTabs = qTabs;
        if (qWidth != null) options.printerConfig.maxWidth = Math.min(120, Math.max(40, qWidth));
        if (qIndent != null) options.printerConfig.indent = Math.max(1, qIndent);

        if (state.leftTab && options.leftTab) options.leftTab.value = state.leftTab;
        if (state.rightTab && options.rightTab) options.rightTab.value = state.rightTab;
    }

    function hydrateFromQuery(query: LocationQuery) {
        const demoId = normalizeQueryValue(query.demo) ?? null;
        if (demoId && demoId !== lastDemoId) {
            const demo = getDemoById(demoId);
            if (demo) {
                options.walkthrough.startDemo(demo);
                lastDemoId = demoId;
            }
        }

        // Check for compressed hash fragment first
        const hash = route.hash;
        if (hash.startsWith("#z=")) {
            decompressState(hash.slice(3))
                .then((state) => {
                    applyState(state);
                    options.onHydrated();
                })
                .catch(() => {
                    // Fall through to query params on decompression failure
                    applyFromQueryParams(query);
                    options.onHydrated();
                });
            return;
        }

        // Backward compat: read from query params
        const hasQueryParams = query.grammar || query.input || query.entry;
        if (hasQueryParams) {
            applyFromQueryParams(query);
            options.onHydrated();
            return;
        }

        // Try localStorage as fallback
        const saved = loadFromLocalStorage();
        if (saved) {
            applyState(saved);
            options.onHydrated();
            return;
        }

        options.onHydrated();
    }

    function applyFromQueryParams(query: LocationQuery) {
        applyState({
            grammar: normalizeQueryValue(query.grammar),
            input: normalizeQueryValue(query.input),
            entry: normalizeQueryValue(query.entry),
            width: normalizeQueryValue(query.width),
            indent: normalizeQueryValue(query.indent),
            tabs: normalizeQueryValue(query.tabs),
        });
    }

    async function buildShareUrl() {
        const state: Record<string, string> = {
            grammar: options.grammarText.value,
            input: options.inputText.value,
            entry: options.entryRuleOverride.value || "auto",
            width: String(options.printerConfig.maxWidth),
            indent: String(options.printerConfig.indent),
            tabs: options.printerConfig.useTabs ? "1" : "0",
        };
        if (options.leftTab) state.leftTab = options.leftTab.value;
        if (options.rightTab) state.rightTab = options.rightTab.value;
        if (options.exampleName) state.exampleName = options.exampleName.value;

        const compressed = await compressState(state);
        const url = new URL("/playground", window.location.origin);
        url.hash = `z=${compressed}`;
        return url.toString();
    }

    watch(
        () => route.query,
        (query) => { hydrateFromQuery(query); },
        { immediate: true },
    );

    // Persist state to localStorage on changes
    watch(
        [
            options.grammarText,
            options.inputText,
            options.entryRuleOverride,
            () => options.printerConfig.maxWidth,
            () => options.printerConfig.indent,
            () => options.printerConfig.useTabs,
            ...(options.leftTab ? [options.leftTab] : []),
            ...(options.rightTab ? [options.rightTab] : []),
            ...(options.exampleName ? [options.exampleName] : []),
        ],
        () => { debouncedSave(); },
    );

    return { hydrateFromQuery, buildShareUrl };
}
