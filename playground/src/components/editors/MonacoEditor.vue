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
const containerRef = ref<HTMLElement>();
const editor = shallowRef<monaco.editor.IStandaloneCodeEditor>();
let resizeObserver: ResizeObserver | null = null;
let isSettingValue = false;

function initEditor() {
    if (!containerRef.value) return;

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
        automaticLayout: true,
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
}

onMounted(() => {
    const el = containerRef.value!;
    if (el.offsetWidth > 0 && el.offsetHeight > 0) {
        initEditor();
    } else {
        resizeObserver = new ResizeObserver((entries) => {
            const entry = entries[0];
            if (entry && entry.contentRect.width > 0 && entry.contentRect.height > 0) {
                resizeObserver?.disconnect();
                resizeObserver = null;
                initEditor();
            }
        });
        resizeObserver.observe(el);
    }
});

watch(
    () => props.modelValue,
    (newVal) => {
        if (editor.value && editor.value.getValue() !== newVal) {
            const pos = editor.value.getPosition();
            isSettingValue = true;
            editor.value.setValue(newVal);
            if (pos) editor.value.setPosition(pos);
            isSettingValue = false;
        }
    },
);

// Theme sync — isDark drives Monaco global theme
watch(isDark, (dark) => {
    monaco.editor.setTheme(dark ? "dark-theme" : "light-theme");
});

// Inline diagnostic markers — deep watch since computed array contents change
watch(
    () => props.markers,
    (markers) => {
        if (!editor.value) return;
        const model = editor.value.getModel();
        if (!model) return;
        monaco.editor.setModelMarkers(model, "pipeline", markers ?? []);
    },
    { deep: true },
);

onBeforeUnmount(() => {
    resizeObserver?.disconnect();
    editor.value?.dispose();
});

defineExpose({ editor });
</script>

<template>
    <div ref="containerRef" class="h-full w-full" />
</template>
