import { watch, type Ref } from "vue";
import { useRoute, useRouter, type LocationQuery, type LocationQueryValue } from "vue-router";
import { getDemoById } from "@/demos";
import type { useWalkthrough } from "@/composables/useWalkthrough";

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

export interface PlaygroundQueryOptions {
    grammarText: Ref<string>;
    inputText: Ref<string>;
    entryRuleOverride: Ref<string>;
    printerConfig: { maxWidth: number; indent: number; useTabs: boolean };
    walkthrough: ReturnType<typeof useWalkthrough>;
    onHydrated: () => void;
}

export function usePlaygroundQuery(options: PlaygroundQueryOptions) {
    const route = useRoute();
    const router = useRouter();
    let lastDemoId: string | null = null;

    function hydrateFromQuery(query: LocationQuery) {
        const demoId = normalizeQueryValue(query.demo) ?? null;
        if (demoId && demoId !== lastDemoId) {
            const demo = getDemoById(demoId);
            if (demo) {
                options.walkthrough.startDemo(demo);
                lastDemoId = demoId;
            }
        }

        const qGrammar = normalizeQueryValue(query.grammar);
        const qInput = normalizeQueryValue(query.input);
        const qEntry = normalizeQueryValue(query.entry);
        const qWidth = parseNumberQuery(normalizeQueryValue(query.width));
        const qIndent = parseNumberQuery(normalizeQueryValue(query.indent));
        const qTabs = parseBooleanQuery(normalizeQueryValue(query.tabs));

        if (qGrammar != null) options.grammarText.value = qGrammar;
        if (qInput != null) options.inputText.value = qInput;
        if (qEntry != null) options.entryRuleOverride.value = qEntry === "auto" ? "" : qEntry;

        if (qTabs != null) {
            options.printerConfig.useTabs = qTabs;
        }

        if (qWidth != null) {
            options.printerConfig.maxWidth = Math.min(120, Math.max(40, qWidth));
        }

        if (qIndent != null) {
            options.printerConfig.indent = Math.max(1, qIndent);
        }

        options.onHydrated();
    }

    async function buildShareUrl() {
        const activeEntry = options.entryRuleOverride.value || "auto";
        const resolved = router.resolve({
            path: "/playground",
            query: {
                grammar: options.grammarText.value,
                input: options.inputText.value,
                entry: activeEntry,
                width: String(options.printerConfig.maxWidth),
                indent: String(options.printerConfig.indent),
                tabs: options.printerConfig.useTabs ? "1" : "0",
            },
        });

        return new URL(resolved.href, window.location.origin).toString();
    }

    watch(
        () => route.query,
        (query) => { hydrateFromQuery(query); },
        { immediate: true },
    );

    return { hydrateFromQuery, buildShareUrl };
}
