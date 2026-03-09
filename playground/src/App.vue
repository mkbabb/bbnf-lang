<script setup lang="ts">
import { ref, computed, watch, onBeforeUnmount } from "vue";
import * as monaco from "monaco-editor";
import { useDebounceFn } from "@vueuse/core";
import { HeaderRibbon } from "@/components/custom/header-ribbon";
import { DarkModeToggle } from "@/components/custom/dark-mode-toggle";
import { Card } from "@/components/ui/card";
import { Tooltip, TooltipTrigger, TooltipContent, TooltipProvider } from "@/components/ui/tooltip";
import MonacoEditor from "@/components/editors/MonacoEditor.vue";
import ControlsBar from "@/components/layout/ControlsBar.vue";
import { useExamples } from "@/composables/useExamples";
import { usePipeline } from "@/composables/usePipeline";
import { registerBBNFLanguage } from "@/components/editors/bbnfMonarch";
import { registerBBNFLanguageProvider, updateGrammarDiagnostics } from "@/composables/useLanguageProvider";
import { Redo2, ExternalLink } from "lucide-vue-next";

registerBBNFLanguage();

// Register WASM-powered language features (hover, completion) — fire-and-forget
let langProviderDisposable: { dispose(): void } | null = null;
registerBBNFLanguageProvider().then((d) => { langProviderDisposable = d; }).catch(() => {});
onBeforeUnmount(() => { langProviderDisposable?.dispose(); });

const { examples, currentExample, selectExample } = useExamples();
const { grammarText, inputText, entryRuleOverride, printerConfig, astJson, formatted, errors, isProcessing, formattedLanguage, formattedBy } = usePipeline();

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
const rightTab = ref<"ast" | "format">("ast");

function toggleLeftTab() {
    leftTab.value = leftTab.value === "grammar" ? "input" : "grammar";
}
function toggleRightTab() {
    rightTab.value = rightTab.value === "ast" ? "format" : "ast";
}

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

// BBNF logo hover card
const logoHoverOpen = ref(false);
let logoHoverTimeout: ReturnType<typeof setTimeout> | undefined;

function onLogoEnter() {
    clearTimeout(logoHoverTimeout);
    logoHoverOpen.value = true;
}
function onLogoLeave() {
    logoHoverTimeout = setTimeout(() => { logoHoverOpen.value = false; }, 150);
}

// Template ref for grammar editor — used for WASM diagnostics
const grammarEditorRef = ref<InstanceType<typeof MonacoEditor>>();

// WASM diagnostics: debounced analysis on grammar text change
const debouncedWasmDiagnostics = useDebounceFn(() => {
    const ed = grammarEditorRef.value?.editor;
    const model = ed?.getModel();
    if (model) updateGrammarDiagnostics(model);
}, 500);

watch(grammarText, () => { debouncedWasmDiagnostics(); });

// Inline diagnostic markers for grammar errors — always include at least line 1
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
</script>

<template>
    <TooltipProvider :delay-duration="300">
    <div class="relative grid h-dvh max-h-dvh w-dvw grid-rows-[1fr_auto] overflow-hidden bg-background text-foreground">
        <!-- Grid background -->
        <div
            class="pointer-events-none absolute inset-0 opacity-[0.03]"
            style="
                background-image: url(&quot;data:image/svg+xml,%3Csvg width='40' height='40' xmlns='http://www.w3.org/2000/svg'%3E%3Cpath d='M0 0h40v40H0z' fill='none' stroke='%23888' stroke-width='0.5'/%3E%3C/svg%3E&quot;);
            "
        />

        <!-- Top-left: BBNF logo with hover card -->
        <div
            class="fixed top-0 left-0 z-40 px-5 pt-2 pb-6"
            @mouseenter="onLogoEnter"
            @mouseleave="onLogoLeave"
        >
            <div class="flex items-center gap-0.5 select-none cursor-default">
                <img src="/bbnf-icon.png" alt="" class="h-8 w-8 object-contain -mr-0.5" />
                <span class="instrument-serif text-4xl tracking-tight text-foreground">BNF</span>
            </div>

            <!-- Hover card -->
            <Transition name="hover-card">
                <div
                    v-if="logoHoverOpen"
                    class="absolute left-5 top-full mt-1 w-56 rounded-lg border border-border/50 bg-card/90 backdrop-blur-xl p-3 shadow-lg"
                    @mouseenter="onLogoEnter"
                    @mouseleave="onLogoLeave"
                >
                    <p class="instrument-serif text-base text-muted-foreground">
                        Grammar-driven parser &amp; formatter playground
                    </p>
                    <hr class="my-2 border-border/40" />
                    <a
                        href="https://github.com/mkbabb/bbnf-lang"
                        target="_blank"
                        rel="noopener"
                        class="flex items-center gap-1.5 instrument-serif text-base text-foreground/80 hover:text-foreground transition-colors"
                    >
                        <ExternalLink class="h-3.5 w-3.5" />
                        <span>View on GitHub</span>
                    </a>
                </div>
            </Transition>
        </div>

        <!-- Top-right ribbon with dark mode toggle -->
        <HeaderRibbon position="right">
            <template #items>
                <DarkModeToggle class="h-6 w-6" />
            </template>
            <template #anchor>
                <div class="flex items-center cursor-pointer px-2 py-1 rounded-md hover:bg-muted/50 transition-colors">
                    <span class="instrument-serif text-lg text-muted-foreground hover:text-foreground transition-colors">@mbabb</span>
                </div>
            </template>
        </HeaderRibbon>

        <!-- Main content: two panes -->
        <div class="grid grid-cols-2 gap-3 p-4 pt-14 pb-2 overflow-hidden">
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
                <div class="flex-1 overflow-hidden">
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
                        language="plaintext"
                        :markers="inputMarkers"
                    />
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
                        <Transition name="hover-card">
                            <span
                                v-if="formattedBy"
                                class="absolute top-2 right-4 z-10 rounded-md px-2 py-0.5 text-xs font-mono backdrop-blur-sm border"
                                :class="formattedBy === 'gorgeous'
                                    ? 'bg-emerald-500/10 text-emerald-600 dark:text-emerald-400 border-emerald-500/20'
                                    : 'bg-amber-500/10 text-amber-600 dark:text-amber-400 border-amber-500/20'"
                            >
                                {{ formattedBy === 'gorgeous' ? 'gorgeous (WASM)' : 'interpreter' }}
                            </span>
                        </Transition>
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
    </div>
    </TooltipProvider>
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
