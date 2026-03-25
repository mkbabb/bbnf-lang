<script setup lang="ts">
import ControlsBar from "@/components/layout/ControlsBar.vue";
import type { PipelineError } from "@/composables/usePipeline";
import type { Example } from "@/composables/useExamples";

defineProps<{
    examples: Example[];
    currentExample: Example;
    printerConfig: { maxWidth: number; indent: number; useTabs: boolean };
    errors: PipelineError[];
    isProcessing: boolean;
    activeResultLabel: string;
    canCopyResult: boolean;
}>();

const emit = defineEmits<{
    "select-example": [name: string];
    "copy-result": [];
    "share-link": [];
    "reset-playground": [];
    "jump-to-error": [error: PipelineError];
}>();
</script>

<template>
    <ControlsBar
        :examples="examples"
        :current-example="currentExample"
        :printer-config="printerConfig"
        :errors="errors"
        :is-processing="isProcessing"
        :active-result-label="activeResultLabel"
        :can-copy-result="canCopyResult"
        @select-example="emit('select-example', $event)"
        @copy-result="emit('copy-result')"
        @share-link="emit('share-link')"
        @reset-playground="emit('reset-playground')"
        @jump-to-error="emit('jump-to-error', $event)"
    />
</template>
