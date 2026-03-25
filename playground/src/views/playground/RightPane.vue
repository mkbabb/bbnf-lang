<script setup lang="ts">
import MonacoEditor from "@/components/editors/MonacoEditor.vue";
import EditorPanel from "@/components/editors/EditorPanel.vue";
import type { PanelTab } from "@/components/editors/EditorPanel.vue";
import DebugPanel from "@/components/debug/DebugPanel.vue";
import type { UseDebugSessionReturn } from "@/components/debug/useDebugSession";
import type { Telemetry } from "@/composables/usePipeline";
import { ExternalLink } from "lucide-vue-next";
import {
    HoverCardRoot,
    HoverCardTrigger,
    HoverCardPortal,
    HoverCardContent,
} from "reka-ui";

type RightTab = "ast" | "format" | "debug";

defineProps<{
    rightTab: RightTab;
    astJson: string;
    formatted: string;
    formattedLanguage: string;
    debugSession: UseDebugSessionReturn;
    inputText: string;
    telemetry: Telemetry;
    formattedBy: "gorgeous" | "";
    langIcons: Record<string, string>;
    mobilePaneLabel?: string;
    mobilePaneColor?: string;
}>();

const emit = defineEmits<{
    "update:rightTab": [value: RightTab];
    "jump-to-rule": [name: string];
    "switch-pane": [];
}>();

const rightTabs: PanelTab[] = [
    { key: "ast", label: "Parsed AST", color: "pastel-purple", description: "Abstract syntax tree produced by the parser (`JSON`)" },
    { key: "format", label: "Formatted", color: "pastel-amber", description: "Pretty-printed output driven by `@pretty` directives, powered by `gorgeous` (`WASM`)" },
    { key: "debug", label: "Debug", color: "pastel-pink", description: "Step through parse execution — set breakpoints, inspect call stack and parse state" },
];

const astEditorRef = defineModel<InstanceType<typeof MonacoEditor> | null>("astEditorRef");
const formattedEditorRef = defineModel<InstanceType<typeof MonacoEditor> | null>("formattedEditorRef");
</script>

<template>
    <EditorPanel
        :active-tab="rightTab"
        :tabs="rightTabs"
        :lang-icons="langIcons"
        :badge-language="rightTab === 'format' ? formattedLanguage : undefined"
        :mobile-pane-label="mobilePaneLabel"
        :mobile-pane-color="mobilePaneColor"
        @select-tab="(key: string) => { emit('update:rightTab', key as RightTab); }"
        @switch-pane="emit('switch-pane')"
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
                @jump-to-rule="(name) => emit('jump-to-rule', name)"
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
</template>
