<script setup lang="ts">
import MonacoEditor from "@/components/editors/MonacoEditor.vue";
import EditorPanel from "@/components/editors/EditorPanel.vue";
import type { PanelTab } from "@/components/editors/EditorPanel.vue";
import type * as monaco from "monaco-editor";

type LeftTab = "grammar" | "input";

const props = defineProps<{
    grammarText: string;
    inputText: string;
    leftTab: LeftTab;
    grammarMarkers: monaco.editor.IMarkerData[];
    inputMarkers: monaco.editor.IMarkerData[];
    breakpointLines?: Set<number>;
    ruleDefinitionLines?: Set<number>;
    debugConsumedOffset?: number;
    formattedLanguage: string;
    langIcons: Record<string, string>;
    showBbnfBadge: boolean;
    mobilePaneLabel?: string;
    mobilePaneColor?: string;
}>();

const emit = defineEmits<{
    "update:grammarText": [value: string];
    "update:inputText": [value: string];
    "update:leftTab": [value: LeftTab];
    "toggle-breakpoint-line": [line: number];
    "switch-pane": [];
}>();

const leftTabs: PanelTab[] = [
    { key: "grammar", label: "Grammar", color: "pastel-green", description: "`BBNF` grammar definition — rules, directives, and `@pretty` hints" },
    { key: "input", label: "Input", color: "pastel-blue", description: "Source text to parse using the grammar above" },
];

const grammarEditorRef = defineModel<InstanceType<typeof MonacoEditor> | null>("grammarEditorRef");
const inputEditorRef = defineModel<InstanceType<typeof MonacoEditor> | null>("inputEditorRef");
</script>

<template>
    <EditorPanel
        :active-tab="leftTab"
        :tabs="leftTabs"
        :lang-icons="langIcons"
        :badge-language="leftTab === 'input' ? formattedLanguage : undefined"
        :show-bbnf-badge="leftTab === 'grammar'"
        :mobile-pane-label="mobilePaneLabel"
        :mobile-pane-color="mobilePaneColor"
        @select-tab="(key: string) => { emit('update:leftTab', key as LeftTab); }"
        @switch-pane="emit('switch-pane')"
    >
        <template #grammar>
            <MonacoEditor
                ref="grammarEditorRef"
                :model-value="grammarText"
                language="bbnf"
                :markers="grammarMarkers"
                :breakpoint-lines="breakpointLines"
                :rule-definition-lines="ruleDefinitionLines"
                @update:model-value="emit('update:grammarText', $event)"
                @toggle-breakpoint-line="emit('toggle-breakpoint-line', $event)"
            />
        </template>
        <template #input>
            <MonacoEditor
                ref="inputEditorRef"
                :model-value="inputText"
                :language="formattedLanguage"
                :markers="inputMarkers"
                :consumed-offset="debugConsumedOffset"
                @update:model-value="emit('update:inputText', $event)"
            />
        </template>
    </EditorPanel>
</template>
