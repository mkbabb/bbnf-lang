<script setup lang="ts">
import { ref, computed, watch, onBeforeUnmount, nextTick, type Ref } from "vue";
import { useRoute } from "vue-router";
import * as monaco from "monaco-editor";
import { useDebounceFn } from "@vueuse/core";
import { toast } from "vue-sonner";
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";
import {
    HoverCardRoot,
    HoverCardTrigger,
    HoverCardPortal,
    HoverCardContent,
} from "reka-ui";
import MonacoEditor from "@/components/editors/MonacoEditor.vue";
import EditorPanel from "@/components/editors/EditorPanel.vue";
import type { PanelTab } from "@/components/editors/EditorPanel.vue";
import ControlsBar from "@/components/layout/ControlsBar.vue";
import { useExamples } from "@/composables/useExamples";
import { usePipeline, type PipelineError } from "@/composables/usePipeline";
import { useSplitPane } from "@/composables/useSplitPane";
import { usePlaygroundQuery } from "@/composables/usePlaygroundQuery";
import { registerBBNFLanguage } from "@/components/editors/bbnfMonarch";
import { registerBBNFLanguageProvider, updateGrammarDiagnostics } from "@/lib/languageProvider";
import { useWalkthrough } from "@/composables/useWalkthrough";
import { useDebugSession } from "@/composables/useDebugSession";
import DebugPanel from "@/components/debug/DebugPanel.vue";
import { BbnfLogo } from "@/components/custom/bbnf-logo";
import { ExternalLink, GripVertical } from "lucide-vue-next";
import "@/lib/monacoWorkers";

type LeftTab = "grammar" | "input";
type RightTab = "ast" | "format" | "debug";

registerBBNFLanguage();

// Register WASM-powered language features — fire-and-forget
let langProviderDisposable: { dispose(): void } | null = null;
registerBBNFLanguageProvider().then((d) => { langProviderDisposable = d; }).catch(() => {});
onBeforeUnmount(() => { langProviderDisposable?.dispose(); });

const route = useRoute();
const walkthrough = useWalkthrough();

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
    needsFullTree,
} = usePipeline();

const debugSession = useDebugSession({
    grammarText,
    inputText,
    entryRuleOverride,
});

// Single-pass rule definition map: line (1-based) ↔ rule name.
// Computed once from grammar text; all breakpoint logic derives from this.
const ruleLineMap = computed(() => {
    const lineToName = new Map<number, string>();
    const nameToLine = new Map<string, number>();
    const lineArr = grammarText.value.split("\n");
    for (let i = 0; i < lineArr.length; i++) {
        const m = lineArr[i]!.match(/^\s*([a-zA-Z_][\w-]*)\s*=/);
        if (m) {
            const line = i + 1;
            lineToName.set(line, m[1]!);
            nameToLine.set(m[1]!, line);
        }
    }
    return { lineToName, nameToLine };
});

const ruleDefinitionLines = computed(() => new Set(ruleLineMap.value.lineToName.keys()));

const breakpointLines = computed(() => {
    const lines = new Set<number>();
    for (const name of debugSession.breakpoints.value) {
        const line = ruleLineMap.value.nameToLine.get(name);
        if (line) lines.add(line);
    }
    return lines;
});

function onToggleBreakpointLine(line: number) {
    const name = ruleLineMap.value.lineToName.get(line);
    if (name) debugSession.toggleBreakpoint(name);
}

// Consumed offset for the input editor decoration.
const debugConsumedOffset = computed(() => {
    if (!debugSession.active.value || !debugSession.snapshot.value) return 0;
    return debugSession.snapshot.value.offset;
});

const leftTab = ref<LeftTab>("grammar");
const rightTab = ref<RightTab>("format");
const mobilePaneIndex = ref<0 | 1>(0);

const grammarEditorRef = ref<InstanceType<typeof MonacoEditor> | null>(null);
const inputEditorRef = ref<InstanceType<typeof MonacoEditor> | null>(null);
const astEditorRef = ref<InstanceType<typeof MonacoEditor> | null>(null);
const formattedEditorRef = ref<InstanceType<typeof MonacoEditor> | null>(null);

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

const leftTabs: PanelTab[] = [
    { key: "grammar", label: "Grammar", color: "pastel-green", description: "`BBNF` grammar definition — rules, directives, and `@pretty` hints" },
    { key: "input", label: "Input", color: "pastel-blue", description: "Source text to parse using the grammar above" },
];

const rightTabs: PanelTab[] = [
    { key: "ast", label: "Parsed AST", color: "pastel-purple", description: "Abstract syntax tree produced by the parser (`JSON`)" },
    { key: "format", label: "Formatted", color: "pastel-amber", description: "Pretty-printed output driven by `@pretty` directives, powered by `gorgeous` (`WASM`)" },
    { key: "debug", label: "Debug", color: "pastel-pink", description: "Step through parse execution — set breakpoints, inspect call stack and parse state" },
];

const activeEntryRule = computed(() => {
    if (entryRuleOverride.value && availableEntryRules.value.includes(entryRuleOverride.value)) {
        return entryRuleOverride.value;
    }
    return "";
});

const activeResultText = computed(() => rightTab.value === "ast" ? astJson.value : formatted.value);
const activeResultLabel = computed(() => rightTab.value === "ast" ? "Parsed AST" : "Formatted");
const canCopyResult = computed(() => activeResultText.value.length > 0);

// Split pane management
function relayoutAllEditors() {
    grammarEditorRef.value?.layout();
    inputEditorRef.value?.layout();
    astEditorRef.value?.layout();
    formattedEditorRef.value?.layout();
}

const {
    splitContainerRef,
    isDesktop,
    primaryPaneStyle,
    secondaryPaneStyle,
    onDividerPointerDown,
    onDividerKeyDown,
    resetSplitForCurrentMode,
    scheduleEditorRelayout,
} = useSplitPane(relayoutAllEditors);

// Drive parse_check fast path: only serialize the full AST when the tab is visible.
watch([rightTab, mobilePaneIndex], () => {
    needsFullTree.value = rightTab.value === "ast" && (isDesktop.value || mobilePaneIndex.value === 1);
}, { immediate: true });

// Relayout editors when mobile pane switches
watch(mobilePaneIndex, () => { scheduleEditorRelayout(); });

const currentExampleName = computed(() => currentExample.value.name);

// Query string hydration
const { buildShareUrl } = usePlaygroundQuery({
    grammarText,
    inputText,
    entryRuleOverride,
    printerConfig,
    walkthrough,
    leftTab: leftTab as Ref<string>,
    rightTab: rightTab as Ref<string>,
    exampleName: currentExampleName,
    onHydrated: () => {
        // If localStorage had an exampleName, restore it
        const saved = localStorage.getItem("bbnf-playground-state");
        if (saved) {
            try {
                const state = JSON.parse(saved);
                if (state.exampleName && state.exampleName !== currentExample.value.name) {
                    selectExample(state.exampleName);
                }
            } catch {}
        }
        scheduleEditorRelayout();
    },
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

function selectRightTab(key: string) {
    rightTab.value = key as RightTab;
}

function focusEditorAfterSwitch(editorRef: typeof grammarEditorRef, line = 1, column = 1) {
    requestAnimationFrame(() => {
        editorRef.value?.layout();
        editorRef.value?.focusPosition(line, column);
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
    const url = await buildShareUrl();
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
        if (!isDesktop.value) mobilePaneIndex.value = 0;
        focusEditorAfterSwitch(grammarEditorRef, error.line ?? 1, error.column ?? 1);
        return;
    }

    if (error.source === "parse") {
        leftTab.value = "input";
        if (!isDesktop.value) mobilePaneIndex.value = 0;
        focusEditorAfterSwitch(inputEditorRef, error.line ?? 1, error.column ?? 1);
        return;
    }

    if (error.source === "format") {
        rightTab.value = "format";
        if (!isDesktop.value) mobilePaneIndex.value = 1;
        requestAnimationFrame(() => {
            formattedEditorRef.value?.layout();
            formattedEditorRef.value?.focus();
        });
        return;
    }

    toast.error("This error has no editor location");
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

watch(availableEntryRules, (rules) => {
    if (entryRuleOverride.value && rules.length > 0 && !rules.includes(entryRuleOverride.value)) {
        entryRuleOverride.value = "";
    }
});

watch([leftTab, rightTab], () => {
    scheduleEditorRelayout();
});
</script>

<template>
    <div
        class="relative mt-14 w-full overflow-hidden h-[calc(100dvh-var(--spacing-navbar))] max-h-[calc(100dvh-var(--spacing-navbar))]"
    >
        <div class="absolute inset-0 overflow-hidden p-1 pb-12 sm:p-4 sm:pb-12 flex flex-col">
            <!-- Desktop: split pane with divider -->
            <div
                v-if="isDesktop"
                ref="splitContainerRef"
                class="flex h-full min-h-0 min-w-0 overflow-hidden flex-row items-stretch"
            >
                <!-- Left pane: Grammar / Input -->
                <div class="min-h-0 min-w-0" :style="primaryPaneStyle">
                    <EditorPanel
                        :active-tab="leftTab"
                        :tabs="leftTabs"
                        :lang-icons="langIcons"
                        :badge-language="leftTab === 'input' ? formattedLanguage : undefined"
                        :show-bbnf-badge="leftTab === 'grammar'"
                        @select-tab="(key: string) => { leftTab = key as LeftTab; }"
                    >
                        <template #grammar>
                            <MonacoEditor
                                ref="grammarEditorRef"
                                v-model="grammarText"
                                language="bbnf"
                                :markers="grammarMarkers"
                                :breakpoint-lines="breakpointLines"
                                :rule-definition-lines="ruleDefinitionLines"
                                @toggle-breakpoint-line="onToggleBreakpointLine"
                            />
                        </template>
                        <template #input>
                            <MonacoEditor
                                ref="inputEditorRef"
                                v-model="inputText"
                                :language="formattedLanguage"
                                :markers="inputMarkers"
                                :consumed-offset="debugConsumedOffset"
                            />
                        </template>
                    </EditorPanel>
                </div>

                <!-- Divider -->
                <Tooltip>
                    <TooltipTrigger as-child>
                        <button
                            type="button"
                            style="touch-action: none"
                            class="group relative shrink-0 rounded-full border border-border/50 bg-card/35 text-muted-foreground transition-all hover:text-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring/50 mx-1 my-6 w-3 cursor-col-resize"
                            aria-label="Resize playground panes horizontally"
                            @pointerdown="onDividerPointerDown"
                            @keydown="onDividerKeyDown"
                            @dblclick="resetSplitForCurrentMode"
                        >
                            <span class="absolute inset-[1px] rounded-full bg-card/70 backdrop-blur-xl" />
                            <span class="relative flex h-full w-full items-center justify-center">
                                <GripVertical class="h-3.5 w-3.5" />
                            </span>
                        </button>
                    </TooltipTrigger>
                    <TooltipContent side="top" :side-offset="8" class="max-w-xs">
                        <p class="text-sm sm:text-base">
                            Drag to resize the panes. Use arrow keys for fine adjustments and double-click to reset.
                        </p>
                    </TooltipContent>
                </Tooltip>

                <!-- Right pane: AST / Formatted -->
                <div class="min-h-0 min-w-0 flex-1" :style="secondaryPaneStyle">
                    <EditorPanel
                        :active-tab="rightTab"
                        :tabs="rightTabs"
                        :lang-icons="langIcons"
                        :badge-language="rightTab === 'format' ? formattedLanguage : undefined"
                        @select-tab="selectRightTab"
                    >
                        <template #ast>
                            <MonacoEditor
                                ref="astEditorRef"
                                :model-value="astJson"
                                language="json"
                                :readonly="true"
                            />
                        </template>
                        <template #format>
                            <MonacoEditor
                                ref="formattedEditorRef"
                                :model-value="formatted"
                                :language="formattedLanguage"
                                :readonly="true"
                            />
                        </template>
                        <template #debug>
                            <DebugPanel
                                :session="debugSession"
                                :input-text="inputText"
                                @jump-to-rule="(name) => {
                                    leftTab = 'grammar';
                                    // Future: jump to rule definition line
                                }"
                            />
                        </template>
                        <template #overlay>
                            <HoverCardRoot :open-delay="300">
                                <HoverCardTrigger as-child>
                                    <Transition name="hover-card">
                                        <span
                                            v-if="rightTab === 'format' && formattedBy"
                                            class="absolute top-2 right-4 z-20 cursor-default rounded-md border px-2 py-0.5 text-xs font-mono backdrop-blur-sm bg-emerald-500/10 text-emerald-600 border-emerald-500/20 dark:text-emerald-400"
                                        >
                                            gorgeous (WASM)
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
                                            <span class="instrument-serif text-base">gorgeous</span>
                                            <span class="rounded-full bg-muted/40 px-2 py-0.5 text-[0.625rem] text-muted-foreground">WASM</span>
                                        </div>
                                        <div class="mb-3 grid grid-cols-2 gap-y-1 text-xs text-muted-foreground font-mono">
                                            <span>Parse</span>  <span class="text-right">{{ telemetry.parseMs }}ms</span>
                                            <span>Format</span> <span class="text-right">{{ telemetry.formatMs }}ms</span>
                                            <span>Total</span>  <span class="text-right">{{ telemetry.totalMs }}ms</span>
                                            <span>Input</span>  <span class="text-right">{{ telemetry.inputBytes }} B</span>
                                        </div>
                                        <a
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
                        </template>
                    </EditorPanel>
                </div>
            </div>

            <!-- Mobile: single pane, toggled by mobilePaneIndex -->
            <template v-else>
                <Transition name="mobile-pane" mode="out-in">
                    <div v-if="mobilePaneIndex === 0" key="left" class="flex-1 min-h-0 min-w-0 overflow-hidden">
                        <EditorPanel
                            :active-tab="leftTab"
                            :tabs="leftTabs"
                            :lang-icons="langIcons"
                            :badge-language="leftTab === 'input' ? formattedLanguage : undefined"
                            :show-bbnf-badge="leftTab === 'grammar'"
                            mobile-pane-label="Output"
                            mobile-pane-color="pastel-amber"
                            @select-tab="(key: string) => { leftTab = key as LeftTab; }"
                            @switch-pane="mobilePaneIndex = 1"
                        >
                            <template #grammar>
                                <MonacoEditor
                                    ref="grammarEditorRef"
                                    v-model="grammarText"
                                    language="bbnf"
                                    :markers="grammarMarkers"
                                />
                            </template>
                            <template #input>
                                <MonacoEditor
                                    ref="inputEditorRef"
                                    v-model="inputText"
                                    :language="formattedLanguage"
                                    :markers="inputMarkers"
                                />
                            </template>
                        </EditorPanel>
                    </div>
                    <div v-else key="right" class="flex-1 min-h-0 min-w-0 overflow-hidden">
                        <EditorPanel
                            :active-tab="rightTab"
                            :tabs="rightTabs"
                            :lang-icons="langIcons"
                            :badge-language="rightTab === 'format' ? formattedLanguage : undefined"
                            mobile-pane-label="Editor"
                            mobile-pane-color="pastel-green"
                            @select-tab="selectRightTab"
                            @switch-pane="mobilePaneIndex = 0"
                        >
                            <template #ast>
                                <MonacoEditor
                                    ref="astEditorRef"
                                    :model-value="astJson"
                                    language="json"
                                    :readonly="true"
                                />
                            </template>
                            <template #format>
                                <MonacoEditor
                                    ref="formattedEditorRef"
                                    :model-value="formatted"
                                    :language="formattedLanguage"
                                    :readonly="true"
                                />
                            </template>
                            <template #debug>
                                <DebugPanel
                                    :session="debugSession"
                                    :input-text="inputText"
                                    @jump-to-rule="(name) => {
                                        leftTab = 'grammar';
                                        if (!isDesktop) mobilePaneIndex = 0;
                                    }"
                                />
                            </template>
                            <template #overlay>
                                <HoverCardRoot :open-delay="300">
                                    <HoverCardTrigger as-child>
                                        <Transition name="hover-card">
                                            <span
                                                v-if="rightTab === 'format' && formattedBy"
                                                class="absolute top-2 right-4 z-20 cursor-default rounded-md border px-2 py-0.5 text-xs font-mono backdrop-blur-sm bg-emerald-500/10 text-emerald-600 border-emerald-500/20 dark:text-emerald-400"
                                            >
                                                gorgeous (WASM)
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
                                                <span class="instrument-serif text-base">gorgeous</span>
                                                <span class="rounded-full bg-muted/40 px-2 py-0.5 text-[0.625rem] text-muted-foreground">WASM</span>
                                            </div>
                                            <div class="mb-3 grid grid-cols-2 gap-y-1 text-xs text-muted-foreground font-mono">
                                                <span>Parse</span>  <span class="text-right">{{ telemetry.parseMs }}ms</span>
                                                <span>Format</span> <span class="text-right">{{ telemetry.formatMs }}ms</span>
                                                <span>Total</span>  <span class="text-right">{{ telemetry.totalMs }}ms</span>
                                                <span>Input</span>  <span class="text-right">{{ telemetry.inputBytes }} B</span>
                                            </div>
                                            <a
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
                            </template>
                        </EditorPanel>
                    </div>
                </Transition>
            </template>
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

<script lang="ts">
import WalkthroughOverlay from "@/components/walkthrough/WalkthroughOverlay.vue";
import WalkthroughControls from "@/components/walkthrough/WalkthroughControls.vue";
</script>

