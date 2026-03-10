<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount } from "vue";
import { useRoute } from "vue-router";
import * as monaco from "monaco-editor";
import { useDebounceFn } from "@vueuse/core";
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
import { usePipeline } from "@/composables/usePipeline";
import { registerBBNFLanguage } from "@/components/editors/bbnfMonarch";
import { registerBBNFLanguageProvider, updateGrammarDiagnostics } from "@/composables/useLanguageProvider";
import { useWalkthrough } from "@/composables/useWalkthrough";
import { getDemoById } from "@/demos";
import WalkthroughOverlay from "@/components/walkthrough/WalkthroughOverlay.vue";
import WalkthroughControls from "@/components/walkthrough/WalkthroughControls.vue";
import { BbnfLogo } from "@/components/custom/bbnf-logo";
import { Redo2, ExternalLink } from "lucide-vue-next";

// Lazy-load Monaco workers only when playground mounts
onMounted(() => {
    import("@/lib/monacoWorkers");
});

registerBBNFLanguage();

// Register WASM-powered language features — fire-and-forget
let langProviderDisposable: { dispose(): void } | null = null;
registerBBNFLanguageProvider().then((d) => { langProviderDisposable = d; }).catch(() => {});
onBeforeUnmount(() => { langProviderDisposable?.dispose(); });

const { examples, currentExample, selectExample } = useExamples();
const {
    grammarText, inputText, entryRuleOverride, printerConfig,
    astJson, formatted, errors, isProcessing,
    formattedLanguage, formattedBy, telemetry,
} = usePipeline();

grammarText.value = currentExample.value.grammar;
inputText.value = currentExample.value.input;
entryRuleOverride.value = currentExample.value.entryRule ?? "";

function onSelectExample(name: string) {
    selectExample(name);
    grammarText.value = currentExample.value.grammar;
    inputText.value = currentExample.value.input;
    entryRuleOverride.value = currentExample.value.entryRule ?? "";
}

// Tab state for left/right panes
const leftTab = ref<"grammar" | "input">("grammar");
const rightTab = ref<"ast" | "format">("format");

function toggleLeftTab() {
    leftTab.value = leftTab.value === "grammar" ? "input" : "grammar";
}
function toggleRightTab() {
    rightTab.value = rightTab.value === "ast" ? "format" : "ast";
}

// Language badge config
const langIcons: Record<string, string> = {
    json: "/icons/json.svg",
    css: "/icons/css.svg",
    javascript: "/icons/js.svg",
    typescript: "/icons/typescript.svg",
    html: "/icons/html.svg",
    markdown: "/icons/markdown.svg",
    bbnf: "/icons/bbnf.png",
    plaintext: "/icons/text.svg",
};

const panelConfig = {
    grammar: {
        label: "Grammar",
        color: "pastel-green",
        description: "BBNF grammar definition — rules, directives, and @pretty hints",
    },
    input: {
        label: "Input",
        color: "pastel-blue",
        description: "Source text to parse using the grammar above",
    },
    ast: {
        label: "Parsed AST",
        color: "pastel-purple",
        description: "Abstract syntax tree produced by the parser (JSON)",
    },
    format: {
        label: "Formatted",
        color: "pastel-amber",
        description: "Pretty-printed output driven by @pretty directives. Uses gorgeous (WASM) for built-in languages, TS interpreter for custom grammars.",
    },
} as const;

// Template ref for grammar editor — used for WASM diagnostics
const grammarEditorRef = ref<InstanceType<typeof MonacoEditor>>();

// WASM diagnostics: debounced analysis on grammar text change
const debouncedWasmDiagnostics = useDebounceFn(() => {
    const ed = grammarEditorRef.value?.editor;
    const model = ed?.getModel();
    if (model) updateGrammarDiagnostics(model);
}, 500);

watch(grammarText, () => { debouncedWasmDiagnostics(); });

// Inline diagnostic markers for grammar errors
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

// Inline diagnostic markers for parse errors
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

// Walkthrough demos
const walkthrough = useWalkthrough();

const route = useRoute();
onMounted(() => {
    const demoId = route.query.demo as string | undefined;
    if (demoId) {
        const demo = getDemoById(demoId);
        if (demo) walkthrough.startDemo(demo);
    }

    // Accept grammar/input from query params (e.g. from "Open in Playground" on landing)
    const qGrammar = route.query.grammar as string | undefined;
    const qInput = route.query.input as string | undefined;
    if (qGrammar != null) grammarText.value = qGrammar;
    if (qInput != null) inputText.value = qInput;
});

// Sync walkthrough step changes into editor state
watch(() => walkthrough.currentStep.value, (step) => {
    if (!step) return;
    if (step.grammar != null) grammarText.value = step.grammar;
    if (step.input != null) inputText.value = step.input;
    if (step.entryRule != null) entryRuleOverride.value = step.entryRule;
});
</script>

<template>
    <div class="relative grid h-[calc(100dvh-3.5rem)] max-h-[calc(100dvh-3.5rem)] w-full grid-rows-[1fr_auto] overflow-hidden mt-14">
        <!-- Main content: two panes -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-1.5 sm:gap-3 p-1 sm:p-4 pb-1 sm:pb-2 overflow-hidden">
            <!-- Left pane -->
            <Card class="flex flex-col overflow-hidden relative">
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
                        <TooltipContent side="bottom" :side-offset="8" class="max-w-xs">
                            <p class="instrument-serif text-base">{{ panelConfig[leftTab].description }}</p>
                        </TooltipContent>
                    </Tooltip>
                    <button @click="toggleLeftTab"
                            class="flex items-center gap-1.5 instrument-serif text-sm transition-colors rounded-md px-2 py-0.5 hover:bg-muted/50"
                            :style="{ color: `var(--color-${panelConfig[leftTab === 'grammar' ? 'input' : 'grammar'].color})` }">
                        <Redo2 class="h-4.5 w-4.5" />
                        <span>{{ panelConfig[leftTab === "grammar" ? "input" : "grammar"].label }}</span>
                    </button>
                </div>
                <div class="flex-1 overflow-hidden relative">
                    <MonacoEditor
                        ref="grammarEditorRef"
                        v-show="leftTab === 'grammar'"
                        v-model="grammarText"
                        language="bbnf"
                        :markers="grammarMarkers"
                    />
                    <MonacoEditor
                        v-show="leftTab === 'input'"
                        v-model="inputText"
                        :language="formattedLanguage"
                        :markers="inputMarkers"
                    />
                    <!-- Language badge -->
                    <Transition name="hover-card">
                        <div
                            v-if="leftTab === 'grammar'"
                            class="absolute bottom-2 right-3 z-10 flex items-center gap-1.5 rounded-md px-2 py-0.5 bg-card/60 backdrop-blur-sm border border-border/30 text-[11px] text-muted-foreground font-mono"
                        >
                            <BbnfLogo size="sm" shimmer />
                        </div>
                        <div
                            v-else-if="formattedLanguage !== 'plaintext'"
                            class="absolute bottom-2 right-3 z-10 flex items-center gap-1.5 rounded-md px-2 py-0.5 bg-card/60 backdrop-blur-sm border border-border/30 text-[11px] text-muted-foreground font-mono"
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

            <!-- Right pane -->
            <Card class="flex flex-col overflow-hidden relative">
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
                        <TooltipContent side="bottom" :side-offset="8" class="max-w-xs">
                            <p class="instrument-serif text-base">{{ panelConfig[rightTab].description }}</p>
                        </TooltipContent>
                    </Tooltip>
                    <button @click="toggleRightTab"
                            class="flex items-center gap-1.5 instrument-serif text-sm transition-colors rounded-md px-2 py-0.5 hover:bg-muted/50"
                            :style="{ color: `var(--color-${panelConfig[rightTab === 'ast' ? 'format' : 'ast'].color})` }">
                        <Redo2 class="h-4.5 w-4.5" />
                        <span>{{ panelConfig[rightTab === "ast" ? "format" : "ast"].label }}</span>
                    </button>
                </div>
                <div class="flex-1 overflow-hidden">
                    <MonacoEditor
                        v-show="rightTab === 'ast'"
                        :model-value="astJson"
                        language="json"
                        :readonly="true"
                    />
                    <div v-show="rightTab === 'format'" class="relative h-full">
                        <MonacoEditor
                            :model-value="formatted"
                            :language="formattedLanguage"
                            :readonly="true"
                        />
                        <!-- Language badge (bottom-right) -->
                        <Transition name="hover-card">
                            <div
                                v-if="formattedLanguage !== 'plaintext'"
                                class="absolute bottom-2 right-3 z-10 flex items-center gap-1.5 rounded-md px-2 py-0.5 bg-card/60 backdrop-blur-sm border border-border/30 text-[11px] text-muted-foreground font-mono"
                            >
                                <BbnfLogo v-if="formattedLanguage === 'bbnf'" size="sm" shimmer />
                                <template v-else>
                                    <img :src="langIcons[formattedLanguage] ?? langIcons.plaintext" alt="" class="h-3.5 w-3.5" />
                                    <span>{{ formattedLanguage }}</span>
                                </template>
                            </div>
                        </Transition>
                        <!-- Formatter badge with telemetry hover card -->
                        <HoverCardRoot :open-delay="300">
                            <HoverCardTrigger as-child>
                                <Transition name="hover-card">
                                    <span
                                        v-if="formattedBy"
                                        class="absolute top-2 right-4 z-10 rounded-md px-2 py-0.5 text-xs font-mono backdrop-blur-sm border cursor-default"
                                        :class="formattedBy === 'gorgeous'
                                            ? 'bg-emerald-500/10 text-emerald-600 dark:text-emerald-400 border-emerald-500/20'
                                            : 'bg-amber-500/10 text-amber-600 dark:text-amber-400 border-amber-500/20'"
                                    >
                                        {{ formattedBy === 'gorgeous' ? 'gorgeous (WASM)' : 'interpreter' }}
                                    </span>
                                </Transition>
                            </HoverCardTrigger>
                            <HoverCardPortal>
                                <HoverCardContent
                                    side="bottom"
                                    :side-offset="8"
                                    class="w-72 rounded-xl border border-border/40 bg-card/90 backdrop-blur-xl p-4 shadow-lg z-50"
                                >
                                    <div class="flex items-center gap-2 mb-2">
                                        <span class="instrument-serif text-base">
                                            {{ formattedBy === 'gorgeous' ? 'gorgeous' : 'TS Interpreter' }}
                                        </span>
                                        <span class="rounded-full bg-muted/40 px-2 py-0.5 text-[10px] text-muted-foreground">
                                            {{ formattedBy === 'gorgeous' ? 'WASM' : 'JS' }}
                                        </span>
                                    </div>
                                    <div class="grid grid-cols-2 gap-y-1 text-xs text-muted-foreground font-mono mb-3">
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
                </div>
            </Card>
        </div>

        <!-- Bottom controls bar -->
        <ControlsBar
            :examples="examples"
            :current-example="currentExample"
            :printer-config="printerConfig"
            :errors="errors"
            :is-processing="isProcessing"
            @select-example="onSelectExample"
        />

        <!-- Walkthrough overlay -->
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
