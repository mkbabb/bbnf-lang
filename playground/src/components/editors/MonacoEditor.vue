<script setup lang="ts">
import { ref, watch, onMounted, onBeforeUnmount, shallowRef } from "vue";
import * as monaco from "monaco-editor";
import DarkTheme from "monaco-themes/themes/Dracula.json";
import LightTheme from "monaco-themes/themes/GitHub.json";
import { useGlobalDark } from "@/components/custom/dark-mode-toggle";

// Register themes once
monaco.editor.defineTheme("dark-theme", DarkTheme as any);
monaco.editor.defineTheme("light-theme", LightTheme as any);

const props = withDefaults(
    defineProps<{
        modelValue?: string;
        language?: string;
        readonly?: boolean;
        markers?: monaco.editor.IMarkerData[];
    }>(),
    {
        modelValue: "",
        language: "plaintext",
        readonly: false,
    },
);

const emit = defineEmits<{
    "update:modelValue": [value: string];
}>();

const { isDark } = useGlobalDark();
const containerRef = ref<HTMLElement | null>(null);
const editor = shallowRef<monaco.editor.IStandaloneCodeEditor>();
let resizeObserver: ResizeObserver | null = null;
let isSettingValue = false;
let layoutFrame = 0;

function layout() {
    if (!editor.value || !containerRef.value) return;

    const { width, height } = containerRef.value.getBoundingClientRect();
    if (width <= 0 || height <= 0) return;

    cancelAnimationFrame(layoutFrame);
    layoutFrame = requestAnimationFrame(() => {
        editor.value?.layout({ width, height });
    });
}

function initEditor() {
    if (!containerRef.value || editor.value) return;

    const { width, height } = containerRef.value.getBoundingClientRect();
    if (width <= 0 || height <= 0) return;

    const ed = monaco.editor.create(containerRef.value, {
        value: props.modelValue,
        language: props.language,
        theme: isDark.value ? "dark-theme" : "light-theme",
        readOnly: props.readonly,
        fontFamily: "Fira Code, monospace",
        fontSize: 14,
        fontLigatures: true,
        minimap: { enabled: false },
        wordWrap: "on",
        padding: { top: 12, bottom: 12 },
        automaticLayout: false,
        scrollBeyondLastLine: false,
        lineNumbers: props.readonly ? "off" : "on",
        renderLineHighlight: "none",
        overviewRulerLanes: 0,
        hideCursorInOverviewRuler: true,
        scrollbar: {
            vertical: "auto",
            horizontal: "auto",
            verticalScrollbarSize: 8,
            horizontalScrollbarSize: 8,
        },
    });

    ed.onDidChangeModelContent(() => {
        if (isSettingValue) return;
        const value = ed.getValue();
        if (value !== props.modelValue) {
            emit("update:modelValue", value);
        }
    });

    editor.value = ed;

    // Apply any initial markers
    if (props.markers?.length) {
        const model = ed.getModel();
        if (model) {
            monaco.editor.setModelMarkers(model, "pipeline", props.markers);
        }
    }

    layout();
}

function ensureEditor() {
    if (editor.value) {
        layout();
        return;
    }
    initEditor();
}

function focus() {
    ensureEditor();
    editor.value?.focus();
}

function focusPosition(line = 1, column = 1) {
    ensureEditor();
    if (!editor.value) return;

    const position = { lineNumber: Math.max(1, line), column: Math.max(1, column) };
    editor.value.focus();
    editor.value.setPosition(position);
    editor.value.revealPositionInCenter(position);
}

onMounted(() => {
    const el = containerRef.value;
    if (!el) return;

    resizeObserver = new ResizeObserver((entries) => {
        const entry = entries[0];
        if (!entry) return;

        if (!editor.value) {
            if (entry.contentRect.width > 0 && entry.contentRect.height > 0) {
                initEditor();
            }
            return;
        }

        if (entry.contentRect.width > 0 && entry.contentRect.height > 0) {
            layout();
        }
    });

    resizeObserver.observe(el);
    ensureEditor();
});

watch(
    () => props.readonly,
    (readonly) => {
        if (!editor.value) return;
        editor.value.updateOptions({
            readOnly: readonly,
            lineNumbers: readonly ? "off" : "on",
        });
        layout();
    },
);

watch(
    () => props.modelValue,
    (newVal) => {
        if (!editor.value) {
            ensureEditor();
            return;
        }

        if (editor.value.getValue() !== newVal) {
            const pos = editor.value.getPosition();
            isSettingValue = true;
            editor.value.setValue(newVal);
            if (pos) editor.value.setPosition(pos);
            isSettingValue = false;
        }
    },
);

// Language sync — update model language when prop changes
watch(
    () => props.language,
    (lang) => {
        if (!editor.value) return;
        const model = editor.value.getModel();
        if (model) {
            monaco.editor.setModelLanguage(model, lang);
        }
        layout();
    },
);

// Theme sync — isDark drives Monaco global theme
watch(isDark, (dark) => {
    monaco.editor.setTheme(dark ? "dark-theme" : "light-theme");
    layout();
});

// Inline diagnostic markers — deep watch since computed array contents change
watch(
    () => props.markers,
    (markers) => {
        if (!editor.value) {
            ensureEditor();
            return;
        }

        const model = editor.value.getModel();
        if (!model) return;
        monaco.editor.setModelMarkers(model, "pipeline", markers ?? []);
    },
    { deep: true },
);

onBeforeUnmount(() => {
    cancelAnimationFrame(layoutFrame);
    resizeObserver?.disconnect();
    editor.value?.dispose();
});

defineExpose({ editor, layout, focus, focusPosition });
</script>

<template>
    <div ref="containerRef" class="h-full w-full min-h-0 min-w-0" />
</template>
