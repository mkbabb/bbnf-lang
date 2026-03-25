import { ref, computed, type Ref } from "vue";
import * as monaco from "monaco-editor";
import type MonacoEditor from "@/components/editors/MonacoEditor.vue";
import type { PipelineError } from "@/composables/usePipeline";

type EditorRef = Ref<InstanceType<typeof MonacoEditor> | null>;

export function usePlaygroundEditors(errors: Ref<PipelineError[]>) {
    const grammarEditorRef = ref<InstanceType<typeof MonacoEditor> | null>(null) as EditorRef;
    const inputEditorRef = ref<InstanceType<typeof MonacoEditor> | null>(null) as EditorRef;
    const astEditorRef = ref<InstanceType<typeof MonacoEditor> | null>(null) as EditorRef;
    const formattedEditorRef = ref<InstanceType<typeof MonacoEditor> | null>(null) as EditorRef;

    function relayoutAllEditors() {
        grammarEditorRef.value?.layout();
        inputEditorRef.value?.layout();
        astEditorRef.value?.layout();
        formattedEditorRef.value?.layout();
    }

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

    function focusEditorAfterSwitch(editorRef: EditorRef, line = 1, column = 1) {
        requestAnimationFrame(() => {
            editorRef.value?.layout();
            editorRef.value?.focusPosition(line, column);
        });
    }

    return {
        grammarEditorRef,
        inputEditorRef,
        astEditorRef,
        formattedEditorRef,
        relayoutAllEditors,
        grammarMarkers,
        inputMarkers,
        focusEditorAfterSwitch,
    };
}
