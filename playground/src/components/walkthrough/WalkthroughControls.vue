<script setup lang="ts">
import { computed } from "vue";

const props = defineProps<{
    walkthrough: ReturnType<typeof import("@/composables/useWalkthrough").useWalkthrough>;
}>();

const demoTitle = computed(() => props.walkthrough.currentDemo.value?.title ?? "Demo");
</script>

<template>
    <div class="absolute top-2 left-1/2 -translate-x-1/2 z-50">
        <div class="flex items-center gap-3 rounded-xl border border-border/40 bg-card/80 backdrop-blur-xl px-4 py-2 shadow-md">
            <span class="instrument-serif text-sm text-pastel-green">{{ demoTitle }}</span>
            <span class="text-xs text-muted-foreground">
                Step {{ walkthrough.currentStepIndex.value + 1 }} of {{ walkthrough.totalSteps.value }}
            </span>
            <button
                class="text-xs text-muted-foreground hover:text-foreground transition-colors ml-2"
                @click="walkthrough.exitDemo()"
            >
                Exit
            </button>
        </div>
    </div>
</template>
