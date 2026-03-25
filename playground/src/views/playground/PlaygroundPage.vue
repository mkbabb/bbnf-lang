<script setup lang="ts">
import { ref, computed, watch, onBeforeUnmount, type Ref } from "vue";
import { useDebounceFn } from "@vueuse/core";
import { toast } from "vue-sonner";
import { Tooltip, TooltipTrigger, TooltipContent } from "@mkbabb/glass-ui";
import { GripVertical } from "lucide-vue-next";
import { useExamples } from "@/composables/useExamples";
import { usePipeline, type PipelineError } from "@/composables/usePipeline";
import { useSplitPane } from "@/composables/useSplitPane";
import { usePlaygroundQuery } from "@/composables/usePlaygroundQuery";
import { registerBBNFLanguage } from "@/components/editors/bbnfMonarch";
import { registerBBNFLanguageProvider, updateGrammarDiagnostics } from "@/lib/languageProvider";
import { useWalkthrough } from "@/composables/useWalkthrough";
import { useDebugSession } from "@/components/debug/useDebugSession";
import { usePlaygroundEditors } from "./usePlaygroundEditors";
import { usePlaygroundDebug } from "./usePlaygroundDebug";
import LeftPane from "./LeftPane.vue";
import RightPane from "./RightPane.vue";
import PlaygroundControls from "./PlaygroundControls.vue";
import "@/lib/monacoWorkers";

type LeftTab = "grammar" | "input";
type RightTab = "ast" | "format" | "debug";

registerBBNFLanguage();

// Register WASM-powered language features — fire-and-forget
let langProviderDisposable: { dispose(): void } | null = null;
registerBBNFLanguageProvider().then((d) => { langProviderDisposable = d; }).catch(() => {});
onBeforeUnmount(() => { langProviderDisposable?.dispose(); });

const walkthrough = useWalkthrough();
const { examples, currentExample, selectExample } = useExamples();
const pipeline = usePipeline();
const { grammarText, inputText, entryRuleOverride, availableEntryRules, printerConfig, astJson, formatted, errors, isProcessing, formattedLanguage, formattedBy, telemetry, needsFullTree } = pipeline;

const debugSession = useDebugSession({ grammarText, inputText, entryRuleOverride });

const { grammarEditorRef, inputEditorRef, astEditorRef, formattedEditorRef, relayoutAllEditors, grammarMarkers, inputMarkers, focusEditorAfterSwitch } = usePlaygroundEditors(errors);
const { ruleDefinitionLines, breakpointLines, onToggleBreakpointLine, debugConsumedOffset } = usePlaygroundDebug(grammarText, debugSession);

const leftTab = ref<LeftTab>("grammar");
const rightTab = ref<RightTab>("format");
const mobilePaneIndex = ref<0 | 1>(0);

const langIcons: Record<string, string> = {
    json: "/img/json.svg", css: "/img/css.svg", javascript: "/img/js.svg",
    typescript: "/img/typescript.svg", html: "/img/html.svg", markdown: "/img/markdown.svg",
    bbnf: "/img/bbnf.png", plaintext: "/img/text.svg",
};

const { splitContainerRef, isDesktop, primaryPaneStyle, secondaryPaneStyle, onDividerPointerDown, onDividerKeyDown, resetSplitForCurrentMode, scheduleEditorRelayout } = useSplitPane(relayoutAllEditors);

// Drive parse_check fast path
watch([rightTab, mobilePaneIndex], () => {
    needsFullTree.value = rightTab.value === "ast" && (isDesktop.value || mobilePaneIndex.value === 1);
}, { immediate: true });

watch(mobilePaneIndex, () => { scheduleEditorRelayout(); });

const currentExampleName = computed(() => currentExample.value.name);
const activeEntryRule = computed(() => entryRuleOverride.value && availableEntryRules.value.includes(entryRuleOverride.value) ? entryRuleOverride.value : "");
const activeResultText = computed(() => rightTab.value === "ast" ? astJson.value : formatted.value);
const activeResultLabel = computed(() => rightTab.value === "ast" ? "Parsed AST" : "Formatted");
const canCopyResult = computed(() => activeResultText.value.length > 0);

// Query string hydration
const { buildShareUrl } = usePlaygroundQuery({
    grammarText, inputText, entryRuleOverride, printerConfig, walkthrough,
    leftTab: leftTab as Ref<string>, rightTab: rightTab as Ref<string>,
    exampleName: currentExampleName,
    onHydrated: () => {
        const saved = localStorage.getItem("bbnf-playground-state");
        if (saved) {
            try {
                const state = JSON.parse(saved);
                if (state.exampleName && state.exampleName !== currentExample.value.name) selectExample(state.exampleName);
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

function onSelectExample(name: string) { selectExample(name); applyCurrentExample(); leftTab.value = "grammar"; rightTab.value = "format"; scheduleEditorRelayout(); }
function onSelectEntryRule(value: string) { entryRuleOverride.value = value; scheduleEditorRelayout(); }

async function copyToClipboard(text: string, msg: string) { try { await navigator.clipboard.writeText(text); toast.success(msg); } catch { toast.error("Clipboard access failed"); } }
async function onCopyResult() { if (canCopyResult.value) await copyToClipboard(activeResultText.value, `${activeResultLabel.value} copied`); }
async function onShareLink() { await copyToClipboard(await buildShareUrl(), "Playground link copied"); }

function onResetPlayground() { applyCurrentExample(); printerConfig.maxWidth = 80; printerConfig.indent = 2; printerConfig.useTabs = false; leftTab.value = "grammar"; rightTab.value = "format"; resetSplitForCurrentMode(); toast.success("Playground reset"); }
function onJumpToError(error: PipelineError) {
    if (error.source === "grammar") { leftTab.value = "grammar"; if (!isDesktop.value) mobilePaneIndex.value = 0; focusEditorAfterSwitch(grammarEditorRef, error.line ?? 1, error.column ?? 1); return; }
    if (error.source === "parse") { leftTab.value = "input"; if (!isDesktop.value) mobilePaneIndex.value = 0; focusEditorAfterSwitch(inputEditorRef, error.line ?? 1, error.column ?? 1); return; }
    if (error.source === "format") { rightTab.value = "format"; if (!isDesktop.value) mobilePaneIndex.value = 1; requestAnimationFrame(() => { formattedEditorRef.value?.layout(); formattedEditorRef.value?.focus(); }); return; }
    toast.error("This error has no editor location");
}
function onJumpToRule(name: string) { leftTab.value = "grammar"; if (!isDesktop.value) mobilePaneIndex.value = 0; }

// WASM diagnostics: debounced analysis on grammar text change
const debouncedWasmDiagnostics = useDebounceFn(() => { const model = grammarEditorRef.value?.editor?.getModel(); if (model) updateGrammarDiagnostics(model); }, 500);
watch(grammarText, () => { debouncedWasmDiagnostics(); });

watch(() => walkthrough.currentStep.value, (step) => { if (!step) return; if (step.grammar != null) grammarText.value = step.grammar; if (step.input != null) inputText.value = step.input; if (step.entryRule != null) entryRuleOverride.value = step.entryRule; scheduleEditorRelayout(); });
watch(availableEntryRules, (rules) => { if (entryRuleOverride.value && rules.length > 0 && !rules.includes(entryRuleOverride.value)) entryRuleOverride.value = ""; });
watch([leftTab, rightTab], () => { scheduleEditorRelayout(); });
</script>

<template>
    <div class="relative mt-14 w-full overflow-hidden h-[calc(100dvh-var(--spacing-navbar))] max-h-[calc(100dvh-var(--spacing-navbar))]">
        <div class="absolute inset-0 overflow-hidden p-1 pb-12 sm:p-4 sm:pb-12 flex flex-col">
            <!-- Desktop: split pane with divider -->
            <div v-if="isDesktop" ref="splitContainerRef" class="flex h-full min-h-0 min-w-0 overflow-hidden flex-row items-stretch">
                <div class="min-h-0 min-w-0" :style="primaryPaneStyle">
                    <LeftPane v-model:grammar-text="grammarText" v-model:input-text="inputText" v-model:left-tab="leftTab"
                        :grammar-markers="grammarMarkers" :input-markers="inputMarkers"
                        :breakpoint-lines="breakpointLines" :rule-definition-lines="ruleDefinitionLines"
                        :debug-consumed-offset="debugConsumedOffset"
                        :formatted-language="formattedLanguage" :lang-icons="langIcons" :show-bbnf-badge="leftTab === 'grammar'"
                        @toggle-breakpoint-line="onToggleBreakpointLine"
                    />
                </div>

                <!-- Divider -->
                <Tooltip>
                    <TooltipTrigger as-child>
                        <button type="button" style="touch-action: none"
                            class="group relative shrink-0 rounded-full border border-border/50 bg-card/35 text-muted-foreground transition-all hover:text-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring/50 mx-1 my-6 w-3 cursor-col-resize"
                            aria-label="Resize playground panes horizontally"
                            @pointerdown="onDividerPointerDown" @keydown="onDividerKeyDown" @dblclick="resetSplitForCurrentMode"
                        >
                            <span class="absolute inset-[1px] rounded-full bg-card/70 backdrop-blur-xl" />
                            <span class="relative flex h-full w-full items-center justify-center"><GripVertical class="h-3.5 w-3.5" /></span>
                        </button>
                    </TooltipTrigger>
                    <TooltipContent side="top" :side-offset="8" class="max-w-xs">
                        <p class="text-sm sm:text-base">Drag to resize the panes. Use arrow keys for fine adjustments and double-click to reset.</p>
                    </TooltipContent>
                </Tooltip>

                <div class="min-h-0 min-w-0 flex-1" :style="secondaryPaneStyle">
                    <RightPane v-model:right-tab="rightTab"
                        :ast-json="astJson" :formatted="formatted" :formatted-language="formattedLanguage"
                        :debug-session="debugSession" :input-text="inputText"
                        :telemetry="telemetry" :formatted-by="formattedBy" :lang-icons="langIcons"
                        @jump-to-rule="onJumpToRule"
                    />
                </div>
            </div>

            <!-- Mobile: single pane, toggled by mobilePaneIndex -->
            <template v-else>
                <Transition name="mobile-pane" mode="out-in">
                    <div v-if="mobilePaneIndex === 0" key="left" class="flex-1 min-h-0 min-w-0 overflow-hidden">
                        <LeftPane v-model:grammar-text="grammarText" v-model:input-text="inputText" v-model:left-tab="leftTab"
                            :grammar-markers="grammarMarkers" :input-markers="inputMarkers"
                            :formatted-language="formattedLanguage" :lang-icons="langIcons" :show-bbnf-badge="leftTab === 'grammar'"
                            mobile-pane-label="Output" mobile-pane-color="pastel-amber"
                            @switch-pane="mobilePaneIndex = 1"
                        />
                    </div>
                    <div v-else key="right" class="flex-1 min-h-0 min-w-0 overflow-hidden">
                        <RightPane v-model:right-tab="rightTab"
                            :ast-json="astJson" :formatted="formatted" :formatted-language="formattedLanguage"
                            :debug-session="debugSession" :input-text="inputText"
                            :telemetry="telemetry" :formatted-by="formattedBy" :lang-icons="langIcons"
                            mobile-pane-label="Editor" mobile-pane-color="pastel-green"
                            @jump-to-rule="onJumpToRule" @switch-pane="mobilePaneIndex = 0"
                        />
                    </div>
                </Transition>
            </template>
        </div>

        <PlaygroundControls
            :examples="examples" :current-example="currentExample" :printer-config="printerConfig"
            :errors="errors" :is-processing="isProcessing" :entry-rule="activeEntryRule"
            :available-entry-rules="availableEntryRules" :active-result-label="activeResultLabel" :can-copy-result="canCopyResult"
            @select-example="onSelectExample" @select-entry-rule="onSelectEntryRule"
            @copy-result="onCopyResult" @share-link="onShareLink"
            @reset-playground="onResetPlayground" @jump-to-error="onJumpToError"
        />

        <WalkthroughOverlay v-if="walkthrough.isActive.value" :walkthrough="walkthrough" />
        <WalkthroughControls v-if="walkthrough.isActive.value" :walkthrough="walkthrough" />
    </div>
</template>

<script lang="ts">
import WalkthroughOverlay from "@/components/walkthrough/WalkthroughOverlay.vue";
import WalkthroughControls from "@/components/walkthrough/WalkthroughControls.vue";
</script>
