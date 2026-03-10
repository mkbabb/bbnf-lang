<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount, nextTick } from "vue";
import { useRoute, useRouter, type LocationQuery, type LocationQueryValue } from "vue-router";
import * as monaco from "monaco-editor";
import { useDebounceFn, useMediaQuery, useStorage } from "@vueuse/core";
import { toast } from "vue-sonner";
import { Card } from "@/components/ui/card";
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";
import {
    HoverCardRoot,
    HoverCardTrigger,
    HoverCardPortal,
    HoverCardContent,
} from "reka-ui";
import MonacoEditor from "@/components/editors/MonacoEditor.vue";
import ControlsBar from "@/components/layout/ControlsBar.vue";
import { useExamples } from "@/composables/useExamples";
import { usePipeline, type PipelineError } from "@/composables/usePipeline";
import { registerBBNFLanguage } from "@/components/editors/bbnfMonarch";
import { registerBBNFLanguageProvider, updateGrammarDiagnostics } from "@/composables/useLanguageProvider";
import { useWalkthrough } from "@/composables/useWalkthrough";
import { getDemoById } from "@/demos";
import WalkthroughOverlay from "@/components/walkthrough/WalkthroughOverlay.vue";
import WalkthroughControls from "@/components/walkthrough/WalkthroughControls.vue";
import { BbnfLogo } from "@/components/custom/bbnf-logo";
import InlineRichText from "@/components/ui/InlineRichText.vue";
import { ExternalLink, GripHorizontal, GripVertical, Redo2 } from "lucide-vue-next";
import "@/lib/monacoWorkers";

type LeftTab = "grammar" | "input";
type RightTab = "ast" | "format";

const DESKTOP_SPLIT_KEY = "bbnf-playground-split-desktop";
const MOBILE_SPLIT_KEY = "bbnf-playground-split-mobile";
const DEFAULT_SPLIT_RATIO = 0.5;
const DESKTOP_MIN_PANE_PX = 280;
const MOBILE_MIN_PANE_PX = 160;
const SPLIT_STEP = 0.04;

registerBBNFLanguage();

// Register WASM-powered language features — fire-and-forget
let langProviderDisposable: { dispose(): void } | null = null;
registerBBNFLanguageProvider().then((d) => { langProviderDisposable = d; }).catch(() => {});
onBeforeUnmount(() => { langProviderDisposable?.dispose(); });

const route = useRoute();
const router = useRouter();
const walkthrough = useWalkthrough();

const isDesktop = useMediaQuery("(min-width: 768px)");
const desktopSplitRatio = useStorage(DESKTOP_SPLIT_KEY, DEFAULT_SPLIT_RATIO);
const mobileSplitRatio = useStorage(MOBILE_SPLIT_KEY, DEFAULT_SPLIT_RATIO);

const { examples, currentExample, selectExample } = useExamples();
const {
    grammarText,
    inputText,
    entryRuleOverride,
    availableEntryRules,
    printerConfig,
    astJson,
    formatted,
    errors,
    isProcessing,
    formattedLanguage,
    formattedBy,
    telemetry,
} = usePipeline();

const leftTab = ref<LeftTab>("grammar");
const rightTab = ref<RightTab>("format");

const splitContainerRef = ref<HTMLElement | null>(null);
const splitAxisSize = ref(0);
const isDraggingDivider = ref(false);

const grammarEditorRef = ref<InstanceType<typeof MonacoEditor> | null>(null);
const inputEditorRef = ref<InstanceType<typeof MonacoEditor> | null>(null);
const astEditorRef = ref<InstanceType<typeof MonacoEditor> | null>(null);
const formattedEditorRef = ref<InstanceType<typeof MonacoEditor> | null>(null);

let splitResizeObserver: ResizeObserver | null = null;
let lastDemoId: string | null = null;
let dividerDragStart: { x: number; y: number } | null = null;
let dividerDidDrag = false;

const langIcons: Record<string, string> = {
    json: "/img/json.svg",
    css: "/img/css.svg",
    javascript: "/img/js.svg",
    typescript: "/img/typescript.svg",
    html: "/img/html.svg",
    markdown: "/img/markdown.svg",
    bbnf: "/img/bbnf.png",
    plaintext: "/img/text.svg",
};

const panelConfig = {
    grammar: {
        label: "Grammar",
        color: "pastel-green",
        description: "`BBNF` grammar definition — rules, directives, and `@pretty` hints",
    },
    input: {
        label: "Input",
        color: "pastel-blue",
        description: "Source text to parse using the grammar above",
    },
    ast: {
        label: "Parsed AST",
        color: "pastel-purple",
        description: "Abstract syntax tree produced by the parser (`JSON`)",
    },
    format: {
        label: "Formatted",
        color: "pastel-amber",
        description: "Pretty-printed output driven by `@pretty` directives. Uses `gorgeous` (`WASM`) for built-in languages, `TS interpreter` for custom grammars.",
    },
} as const;

const activeEntryRule = computed(() => {
    if (entryRuleOverride.value && availableEntryRules.value.includes(entryRuleOverride.value)) {
        return entryRuleOverride.value;
    }
    return "";
});

const activeResultText = computed(() => rightTab.value === "ast" ? astJson.value : formatted.value);
const activeResultLabel = computed(() => panelConfig[rightTab.value].label);
const canCopyResult = computed(() => activeResultText.value.length > 0);

const splitRatio = computed({
    get: () => clampSplitRatio(isDesktop.value ? desktopSplitRatio.value : mobileSplitRatio.value),
    set: (value: number) => {
        const clamped = clampSplitRatio(value);
        if (isDesktop.value) {
            desktopSplitRatio.value = clamped;
        } else {
            mobileSplitRatio.value = clamped;
        }
    },
});

const primaryPaneStyle = computed(() => {
    const basis = `${(splitRatio.value * 100).toFixed(3)}%`;
    return isDesktop.value
        ? {
              flex: `0 0 ${basis}`,
              minWidth: `${DESKTOP_MIN_PANE_PX}px`,
          }
        : {
              flex: `0 0 ${basis}`,
              minHeight: `${MOBILE_MIN_PANE_PX}px`,
          };
});

const secondaryPaneStyle = computed(() => {
    return isDesktop.value
        ? {
              minWidth: `${DESKTOP_MIN_PANE_PX}px`,
          }
        : {
              minHeight: `${MOBILE_MIN_PANE_PX}px`,
          };
});

applyCurrentExample();

function applyCurrentExample() {
    grammarText.value = currentExample.value.grammar;
    inputText.value = currentExample.value.input;
    entryRuleOverride.value = currentExample.value.entryRule ?? "";
}

function onSelectExample(name: string) {
    selectExample(name);
    applyCurrentExample();
    leftTab.value = "grammar";
    rightTab.value = "format";
    scheduleEditorRelayout();
}

function onSelectEntryRule(value: string) {
    entryRuleOverride.value = value;
    scheduleEditorRelayout();
}

function toggleLeftTab() {
    leftTab.value = leftTab.value === "grammar" ? "input" : "grammar";
}

function toggleRightTab() {
    rightTab.value = rightTab.value === "ast" ? "format" : "ast";
}

function clampSplitRatio(value: number, axisSize = splitAxisSize.value) {
    if (!Number.isFinite(value)) return DEFAULT_SPLIT_RATIO;

    const minPane = isDesktop.value ? DESKTOP_MIN_PANE_PX : MOBILE_MIN_PANE_PX;
    if (!axisSize || axisSize <= minPane * 2) {
        return Math.min(0.75, Math.max(0.25, value));
    }

    const minRatio = minPane / axisSize;
    const maxRatio = 1 - minRatio;
    if (minRatio >= maxRatio) return DEFAULT_SPLIT_RATIO;

    return Math.min(maxRatio, Math.max(minRatio, value));
}

function measureSplitAxis() {
    if (!splitContainerRef.value) return;
    const rect = splitContainerRef.value.getBoundingClientRect();
    splitAxisSize.value = isDesktop.value ? rect.width : rect.height;
}

function updateSplitFromPointer(clientX: number, clientY: number) {
    if (!splitContainerRef.value) return;

    const rect = splitContainerRef.value.getBoundingClientRect();
    const axisSize = isDesktop.value ? rect.width : rect.height;
    if (axisSize <= 0) return;

    splitAxisSize.value = axisSize;
    const offset = isDesktop.value ? clientX - rect.left : clientY - rect.top;
    splitRatio.value = offset / axisSize;
}

function stopDividerDrag() {
    if (!isDraggingDivider.value) return;
    isDraggingDivider.value = false;
    dividerDragStart = null;
    dividerDidDrag = false;
    document.body.style.userSelect = "";
    window.removeEventListener("pointermove", onWindowPointerMove);
    window.removeEventListener("pointerup", onWindowPointerUp);
}

function onDividerPointerDown(event: PointerEvent) {
    event.preventDefault();
    isDraggingDivider.value = true;
    dividerDragStart = { x: event.clientX, y: event.clientY };
    dividerDidDrag = false;
    document.body.style.userSelect = "none";
    (event.currentTarget as HTMLElement | null)?.focus();
    window.addEventListener("pointermove", onWindowPointerMove);
    window.addEventListener("pointerup", onWindowPointerUp);
}

function onWindowPointerMove(event: PointerEvent) {
    if (!isDraggingDivider.value) return;
    if (!dividerDidDrag) {
        const start = dividerDragStart;
        if (!start) return;

        const delta = isDesktop.value ? Math.abs(event.clientX - start.x) : Math.abs(event.clientY - start.y);
        if (delta < 3) return;

        dividerDidDrag = true;
    }

    updateSplitFromPointer(event.clientX, event.clientY);
}

function onWindowPointerUp(event: PointerEvent) {
    if (dividerDidDrag) {
        updateSplitFromPointer(event.clientX, event.clientY);
    }
    stopDividerDrag();
    if (dividerDidDrag) {
        scheduleEditorRelayout();
    }
}

function resetSplitForCurrentMode() {
    splitRatio.value = DEFAULT_SPLIT_RATIO;
    scheduleEditorRelayout();
}

function onDividerKeyDown(event: KeyboardEvent) {
    const isHorizontal = isDesktop.value;
    let nextValue: number | null = null;

    if (event.key === "Home") nextValue = 0;
    if (event.key === "End") nextValue = 1;
    if (isHorizontal && event.key === "ArrowLeft") nextValue = splitRatio.value - SPLIT_STEP;
    if (isHorizontal && event.key === "ArrowRight") nextValue = splitRatio.value + SPLIT_STEP;
    if (!isHorizontal && event.key === "ArrowUp") nextValue = splitRatio.value - SPLIT_STEP;
    if (!isHorizontal && event.key === "ArrowDown") nextValue = splitRatio.value + SPLIT_STEP;

    if (nextValue == null) return;

    event.preventDefault();
    splitRatio.value = nextValue;
    scheduleEditorRelayout();
}

const scheduleEditorRelayout = useDebounceFn(() => {
    nextTick(() => {
        requestAnimationFrame(() => {
            grammarEditorRef.value?.layout();
            inputEditorRef.value?.layout();
            astEditorRef.value?.layout();
            formattedEditorRef.value?.layout();
        });
    });
}, 16);

function focusEditorAfterSwitch(editorRef: typeof grammarEditorRef, line = 1, column = 1) {
    nextTick(() => {
        requestAnimationFrame(() => {
            editorRef.value?.layout();
            editorRef.value?.focusPosition(line, column);
        });
    });
}

async function copyToClipboard(text: string, successMessage: string) {
    try {
        await navigator.clipboard.writeText(text);
        toast.success(successMessage);
    } catch (error) {
        console.error(error);
        toast.error("Clipboard access failed");
    }
}

async function onCopyResult() {
    if (!canCopyResult.value) return;
    await copyToClipboard(activeResultText.value, `${activeResultLabel.value} copied`);
}

async function onShareLink() {
    const resolved = router.resolve({
        path: "/playground",
        query: {
            grammar: grammarText.value,
            input: inputText.value,
            entry: activeEntryRule.value || "auto",
            width: String(printerConfig.maxWidth),
            indent: String(printerConfig.indent),
            tabs: printerConfig.useTabs ? "1" : "0",
        },
    });

    const url = new URL(resolved.href, window.location.origin).toString();
    await copyToClipboard(url, "Playground link copied");
}

function onResetPlayground() {
    applyCurrentExample();
    printerConfig.maxWidth = 80;
    printerConfig.indent = 2;
    printerConfig.useTabs = false;
    leftTab.value = "grammar";
    rightTab.value = "format";
    resetSplitForCurrentMode();
    toast.success("Playground reset");
}

function onJumpToError(error: PipelineError) {
    if (error.source === "grammar") {
        leftTab.value = "grammar";
        focusEditorAfterSwitch(grammarEditorRef, error.line ?? 1, error.column ?? 1);
        return;
    }

    if (error.source === "parse") {
        leftTab.value = "input";
        focusEditorAfterSwitch(inputEditorRef, error.line ?? 1, error.column ?? 1);
        return;
    }

    if (error.source === "format") {
        rightTab.value = "format";
        nextTick(() => {
            requestAnimationFrame(() => {
                formattedEditorRef.value?.layout();
                formattedEditorRef.value?.focus();
            });
        });
        return;
    }

    toast.error("This error has no editor location");
}

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

function hydrateFromQuery(query: LocationQuery) {
    const demoId = normalizeQueryValue(query.demo) ?? null;
    if (demoId && demoId !== lastDemoId) {
        const demo = getDemoById(demoId);
        if (demo) {
            walkthrough.startDemo(demo);
            lastDemoId = demoId;
        }
    }

    const qGrammar = normalizeQueryValue(query.grammar);
    const qInput = normalizeQueryValue(query.input);
    const qEntry = normalizeQueryValue(query.entry);
    const qWidth = parseNumberQuery(normalizeQueryValue(query.width));
    const qIndent = parseNumberQuery(normalizeQueryValue(query.indent));
    const qTabs = parseBooleanQuery(normalizeQueryValue(query.tabs));

    if (qGrammar != null) grammarText.value = qGrammar;
    if (qInput != null) inputText.value = qInput;
    if (qEntry != null) entryRuleOverride.value = qEntry === "auto" ? "" : qEntry;

    if (qTabs != null) {
        printerConfig.useTabs = qTabs;
    }

    if (qWidth != null) {
        printerConfig.maxWidth = Math.min(120, Math.max(40, qWidth));
    }

    if (qIndent != null) {
        printerConfig.indent = Math.max(1, qIndent);
    }

    scheduleEditorRelayout();
}

// WASM diagnostics: debounced analysis on grammar text change
const debouncedWasmDiagnostics = useDebounceFn(() => {
    const model = grammarEditorRef.value?.editor?.getModel();
    if (model) updateGrammarDiagnostics(model);
}, 500);

watch(grammarText, () => {
    debouncedWasmDiagnostics();
});

const grammarMarkers = computed<monaco.editor.IMarkerData[]>(() => {
    return errors.value
        .filter((e) => e.source === "grammar")
        .map((e) => ({
            severity: monaco.MarkerSeverity.Error,
            message: e.message,
            startLineNumber: e.line ?? 1,
            startColumn: e.column ?? 1,
            endLineNumber: e.line ?? 1,
            endColumn: (e.column ?? 1) + 20,
        }));
});

const inputMarkers = computed<monaco.editor.IMarkerData[]>(() => {
    return errors.value
        .filter((e) => e.source === "parse")
        .map((e) => ({
            severity: monaco.MarkerSeverity.Error,
            message: e.message,
            startLineNumber: e.line ?? 1,
            startColumn: e.column ?? 1,
            endLineNumber: e.line ?? 1,
            endColumn: (e.column ?? 1) + 20,
        }));
});

watch(
    () => walkthrough.currentStep.value,
    (step) => {
        if (!step) return;
        if (step.grammar != null) grammarText.value = step.grammar;
        if (step.input != null) inputText.value = step.input;
        if (step.entryRule != null) entryRuleOverride.value = step.entryRule;
        scheduleEditorRelayout();
    },
);

watch(
    () => route.query,
    (query) => {
        hydrateFromQuery(query);
    },
    { immediate: true },
);

watch(availableEntryRules, (rules) => {
    if (entryRuleOverride.value && rules.length > 0 && !rules.includes(entryRuleOverride.value)) {
        entryRuleOverride.value = "";
    }
});

watch([leftTab, rightTab], () => {
    scheduleEditorRelayout();
});

watch(isDesktop, () => {
    nextTick(() => {
        measureSplitAxis();
        splitRatio.value = splitRatio.value;
        scheduleEditorRelayout();
    });
});

watch(splitRatio, () => {
    scheduleEditorRelayout();
});

onMounted(() => {
    nextTick(() => {
        measureSplitAxis();
        splitResizeObserver = new ResizeObserver(() => {
            measureSplitAxis();
            splitRatio.value = splitRatio.value;
            scheduleEditorRelayout();
        });

        if (splitContainerRef.value) {
            splitResizeObserver.observe(splitContainerRef.value);
        }

        scheduleEditorRelayout();
    });
});

onBeforeUnmount(() => {
    stopDividerDrag();
    splitResizeObserver?.disconnect();
});
</script>

<template>
    <div class="relative mt-14 grid h-[calc(100dvh-3.5rem)] max-h-[calc(100dvh-3.5rem)] w-full grid-rows-[1fr_auto] overflow-hidden">
        <div class="min-h-0 min-w-0 overflow-hidden p-1 pb-1 sm:p-4 sm:pb-2">
            <div
                ref="splitContainerRef"
                class="flex h-full min-h-0 min-w-0 overflow-hidden"
                :class="isDesktop ? 'flex-row items-stretch' : 'flex-col items-stretch'"
            >
                <div class="min-h-0 min-w-0" :style="primaryPaneStyle">
                    <Card class="relative flex h-full min-h-0 min-w-0 flex-col overflow-hidden">
                        <div class="flex items-center justify-between px-3 py-1 backdrop-blur-md bg-card/40 border-b border-border/30">
                            <Tooltip>
                                <TooltipTrigger as-child>
                                    <span
                                        class="instrument-serif text-2xl tracking-tight cursor-default"
                                        :style="{ color: `var(--color-${panelConfig[leftTab].color})` }"
                                    >
                                        {{ panelConfig[leftTab].label }}
                                    </span>
                                </TooltipTrigger>
                                <TooltipContent side="bottom" :side-offset="8" class="max-w-xs border-border/40 bg-card/90 backdrop-blur-xl">
                                    <p class="instrument-serif text-base leading-relaxed">
                                        <InlineRichText :text="panelConfig[leftTab].description" />
                                    </p>
                                </TooltipContent>
                            </Tooltip>
                            <button
                                type="button"
                                class="flex items-center gap-1.5 rounded-md px-2 py-0.5 instrument-serif text-sm transition-colors hover:bg-muted/50"
                                :style="{ color: `var(--color-${panelConfig[leftTab === 'grammar' ? 'input' : 'grammar'].color})` }"
                                @click="toggleLeftTab"
                            >
                                <Redo2 class="h-4.5 w-4.5" />
                                <span>{{ panelConfig[leftTab === "grammar" ? "input" : "grammar"].label }}</span>
                            </button>
                        </div>
                        <div class="relative flex-1 min-h-0 min-w-0 overflow-hidden">
                            <div
                                class="absolute inset-0 min-h-0 min-w-0 transition-opacity duration-150"
                                :class="leftTab === 'grammar' ? 'z-10 opacity-100' : 'pointer-events-none z-0 opacity-0'"
                            >
                                <MonacoEditor
                                    ref="grammarEditorRef"
                                    v-model="grammarText"
                                    language="bbnf"
                                    :markers="grammarMarkers"
                                />
                            </div>
                            <div
                                class="absolute inset-0 min-h-0 min-w-0 transition-opacity duration-150"
                                :class="leftTab === 'input' ? 'z-10 opacity-100' : 'pointer-events-none z-0 opacity-0'"
                            >
                                <MonacoEditor
                                    ref="inputEditorRef"
                                    v-model="inputText"
                                    :language="formattedLanguage"
                                    :markers="inputMarkers"
                                />
                            </div>

                            <Transition name="hover-card">
                                <div
                                    v-if="leftTab === 'grammar'"
                                    class="absolute bottom-2 right-3 z-20 flex items-center gap-1.5 rounded-md border border-border/30 bg-card/60 px-2 py-0.5 text-[11px] text-muted-foreground backdrop-blur-sm font-mono"
                                >
                                    <BbnfLogo size="sm" shimmer />
                                </div>
                                <div
                                    v-else-if="formattedLanguage !== 'plaintext'"
                                    class="absolute bottom-2 right-3 z-20 flex items-center gap-1.5 rounded-md border border-border/30 bg-card/60 px-2 py-0.5 text-[11px] text-muted-foreground backdrop-blur-sm font-mono"
                                >
                                    <BbnfLogo v-if="formattedLanguage === 'bbnf'" size="sm" shimmer />
                                    <template v-else>
                                        <img :src="langIcons[formattedLanguage] ?? langIcons.plaintext" alt="" class="h-3.5 w-3.5" />
                                        <span>{{ formattedLanguage }}</span>
                                    </template>
                                </div>
                            </Transition>
                        </div>
                    </Card>
                </div>

                <Tooltip>
                    <TooltipTrigger as-child>
                        <button
                            type="button"
                            class="group relative shrink-0 rounded-full border border-border/35 bg-card/35 text-muted-foreground transition-all hover:text-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring/50"
                            :class="isDesktop
                                ? 'mx-1 my-10 w-3 cursor-col-resize'
                                : 'mx-10 my-1 h-3 cursor-row-resize'"
                            :aria-label="isDesktop ? 'Resize playground panes horizontally' : 'Resize playground panes vertically'"
                            @pointerdown="onDividerPointerDown"
                            @keydown="onDividerKeyDown"
                            @dblclick="resetSplitForCurrentMode"
                        >
                            <span class="absolute inset-[1px] rounded-full bg-card/70 backdrop-blur-xl" />
                            <span class="relative flex h-full w-full items-center justify-center">
                                <GripVertical v-if="isDesktop" class="h-3.5 w-3.5" />
                                <GripHorizontal v-else class="h-3.5 w-3.5" />
                            </span>
                        </button>
                    </TooltipTrigger>
                    <TooltipContent side="top" :side-offset="8" class="max-w-xs border-border/40 bg-card/90 backdrop-blur-xl">
                        <p class="text-xs sm:text-sm">
                            Drag to resize the panes. Use arrow keys for fine adjustments and double-click to reset.
                        </p>
                    </TooltipContent>
                </Tooltip>

                <div class="min-h-0 min-w-0 flex-1" :style="secondaryPaneStyle">
                    <Card class="relative flex h-full min-h-0 min-w-0 flex-col overflow-hidden">
                        <div class="flex items-center justify-between px-3 py-1 backdrop-blur-md bg-card/40 border-b border-border/30">
                            <Tooltip>
                                <TooltipTrigger as-child>
                                    <span
                                        class="instrument-serif text-2xl tracking-tight cursor-default"
                                        :style="{ color: `var(--color-${panelConfig[rightTab].color})` }"
                                    >
                                        {{ panelConfig[rightTab].label }}
                                    </span>
                                </TooltipTrigger>
                                <TooltipContent side="bottom" :side-offset="8" class="max-w-xs border-border/40 bg-card/90 backdrop-blur-xl">
                                    <p class="instrument-serif text-base leading-relaxed">
                                        <InlineRichText :text="panelConfig[rightTab].description" />
                                    </p>
                                </TooltipContent>
                            </Tooltip>
                            <button
                                type="button"
                                class="flex items-center gap-1.5 rounded-md px-2 py-0.5 instrument-serif text-sm transition-colors hover:bg-muted/50"
                                :style="{ color: `var(--color-${panelConfig[rightTab === 'ast' ? 'format' : 'ast'].color})` }"
                                @click="toggleRightTab"
                            >
                                <Redo2 class="h-4.5 w-4.5" />
                                <span>{{ panelConfig[rightTab === "ast" ? "format" : "ast"].label }}</span>
                            </button>
                        </div>
                        <div class="relative flex-1 min-h-0 min-w-0 overflow-hidden">
                            <div
                                class="absolute inset-0 min-h-0 min-w-0 transition-opacity duration-150"
                                :class="rightTab === 'ast' ? 'z-10 opacity-100' : 'pointer-events-none z-0 opacity-0'"
                            >
                                <MonacoEditor
                                    ref="astEditorRef"
                                    :model-value="astJson"
                                    language="json"
                                    :readonly="true"
                                />
                            </div>
                            <div
                                class="absolute inset-0 min-h-0 min-w-0 transition-opacity duration-150"
                                :class="rightTab === 'format' ? 'z-10 opacity-100' : 'pointer-events-none z-0 opacity-0'"
                            >
                                <MonacoEditor
                                    ref="formattedEditorRef"
                                    :model-value="formatted"
                                    :language="formattedLanguage"
                                    :readonly="true"
                                />
                            </div>

                            <Transition name="hover-card">
                                <div
                                    v-if="rightTab === 'format' && formattedLanguage !== 'plaintext'"
                                    class="absolute bottom-2 right-3 z-20 flex items-center gap-1.5 rounded-md border border-border/30 bg-card/60 px-2 py-0.5 text-[11px] text-muted-foreground backdrop-blur-sm font-mono"
                                >
                                    <BbnfLogo v-if="formattedLanguage === 'bbnf'" size="sm" shimmer />
                                    <template v-else>
                                        <img :src="langIcons[formattedLanguage] ?? langIcons.plaintext" alt="" class="h-3.5 w-3.5" />
                                        <span>{{ formattedLanguage }}</span>
                                    </template>
                                </div>
                            </Transition>

                            <HoverCardRoot :open-delay="300">
                                <HoverCardTrigger as-child>
                                    <Transition name="hover-card">
                                        <span
                                            v-if="rightTab === 'format' && formattedBy"
                                            class="absolute top-2 right-4 z-20 cursor-default rounded-md border px-2 py-0.5 text-xs font-mono backdrop-blur-sm"
                                            :class="formattedBy === 'gorgeous'
                                                ? 'bg-emerald-500/10 text-emerald-600 border-emerald-500/20 dark:text-emerald-400'
                                                : 'bg-amber-500/10 text-amber-600 border-amber-500/20 dark:text-amber-400'"
                                        >
                                            {{ formattedBy === 'gorgeous' ? 'gorgeous (WASM)' : 'interpreter' }}
                                        </span>
                                    </Transition>
                                </HoverCardTrigger>
                                <HoverCardPortal>
                                    <HoverCardContent
                                        side="bottom"
                                        :side-offset="8"
                                        class="z-50 w-72 rounded-xl border border-border/40 bg-card/90 p-4 shadow-lg backdrop-blur-xl"
                                    >
                                        <div class="mb-2 flex items-center gap-2">
                                            <span class="instrument-serif text-base">
                                                {{ formattedBy === 'gorgeous' ? 'gorgeous' : 'TS Interpreter' }}
                                            </span>
                                            <span class="rounded-full bg-muted/40 px-2 py-0.5 text-[10px] text-muted-foreground">
                                                {{ formattedBy === 'gorgeous' ? 'WASM' : 'JS' }}
                                            </span>
                                        </div>
                                        <div class="mb-3 grid grid-cols-2 gap-y-1 text-xs text-muted-foreground font-mono">
                                            <span>Parse</span>  <span class="text-right">{{ telemetry.parseMs }}ms</span>
                                            <span>Format</span> <span class="text-right">{{ telemetry.formatMs }}ms</span>
                                            <span>Total</span>  <span class="text-right">{{ telemetry.totalMs }}ms</span>
                                            <span>Input</span>  <span class="text-right">{{ telemetry.inputBytes }} B</span>
                                        </div>
                                        <a
                                            v-if="formattedBy === 'gorgeous'"
                                            href="https://github.com/mkbabb/bbnf-lang"
                                            target="_blank"
                                            rel="noopener"
                                            class="flex items-center gap-1 text-xs text-pastel-blue hover:underline"
                                        >
                                            <ExternalLink class="h-3 w-3" /> github.com/mkbabb/bbnf-lang
                                        </a>
                                    </HoverCardContent>
                                </HoverCardPortal>
                            </HoverCardRoot>
                        </div>
                    </Card>
                </div>
            </div>
        </div>

        <ControlsBar
            :examples="examples"
            :current-example="currentExample"
            :printer-config="printerConfig"
            :errors="errors"
            :is-processing="isProcessing"
            :entry-rule="activeEntryRule"
            :available-entry-rules="availableEntryRules"
            :active-result-label="activeResultLabel"
            :can-copy-result="canCopyResult"
            @select-example="onSelectExample"
            @select-entry-rule="onSelectEntryRule"
            @copy-result="onCopyResult"
            @share-link="onShareLink"
            @reset-playground="onResetPlayground"
            @jump-to-error="onJumpToError"
        />

        <WalkthroughOverlay v-if="walkthrough.isActive.value" :walkthrough="walkthrough" />
        <WalkthroughControls v-if="walkthrough.isActive.value" :walkthrough="walkthrough" />
    </div>
</template>

<style scoped>
.hover-card-enter-active,
.hover-card-leave-active {
    transition: opacity 150ms cubic-bezier(0.4, 0, 0.2, 1), transform 150ms cubic-bezier(0.4, 0, 0.2, 1);
}
.hover-card-enter-from,
.hover-card-leave-to {
    opacity: 0;
    transform: scale(0.92) translateY(6px);
}
</style>
