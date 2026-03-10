<script setup lang="ts">
import { computed } from "vue";
import { ChevronLeft, ChevronRight, X } from "lucide-vue-next";

const props = defineProps<{
    walkthrough: ReturnType<typeof import("@/composables/useWalkthrough").useWalkthrough>;
}>();

const step = computed(() => props.walkthrough.currentStep.value);
const stepNum = computed(() => props.walkthrough.currentStepIndex.value + 1);
const total = computed(() => props.walkthrough.totalSteps.value);
const isFirst = computed(() => props.walkthrough.currentStepIndex.value === 0);
const isLast = computed(() => props.walkthrough.currentStepIndex.value === total.value - 1);
</script>

<template>
    <!-- Progress bar -->
    <div class="absolute top-0 left-0 right-0 z-50 h-0.5 bg-muted/30">
        <div
            class="h-full bg-pastel-green transition-all duration-500 ease-out"
            :style="{ width: `${walkthrough.progress.value * 100}%` }"
        />
    </div>

    <!-- Annotation card -->
    <div
        v-if="step"
        class="absolute bottom-20 left-1/2 -translate-x-1/2 z-50 w-[32rem] max-w-[calc(100vw-2rem)]"
    >
        <div class="rounded-xl border border-border/50 bg-card/90 backdrop-blur-xl shadow-lg p-5">
            <div class="flex items-start gap-3">
                <!-- Step badge -->
                <div class="shrink-0 h-7 w-7 rounded-full bg-pastel-green/20 border border-pastel-green/40 flex items-center justify-center">
                    <span class="instrument-serif text-sm text-pastel-green">{{ stepNum }}</span>
                </div>

                <!-- Annotation text -->
                <div class="flex-1 min-w-0">
                    <p class="text-sm text-foreground/90 leading-relaxed">{{ step.annotation }}</p>
                </div>

                <!-- Close button -->
                <button
                    class="shrink-0 p-1 rounded-md hover:bg-muted/50 text-muted-foreground hover:text-foreground transition-colors"
                    @click="walkthrough.exitDemo()"
                >
                    <X class="h-4 w-4" />
                </button>
            </div>

            <!-- Navigation -->
            <div class="flex items-center justify-between mt-4 pt-3 border-t border-border/30">
                <button
                    class="flex items-center gap-1 text-xs text-muted-foreground hover:text-foreground transition-colors disabled:opacity-30 disabled:cursor-not-allowed"
                    :disabled="isFirst"
                    @click="walkthrough.prevStep()"
                >
                    <ChevronLeft class="h-3.5 w-3.5" /> Previous
                </button>

                <div class="flex items-center gap-1.5">
                    <div
                        v-for="i in total"
                        :key="i"
                        class="h-1 rounded-full transition-all duration-300"
                        :class="i <= stepNum ? 'w-2.5 bg-pastel-green' : 'w-1 bg-muted-foreground/30'"
                    />
                </div>

                <button
                    v-if="!isLast"
                    class="flex items-center gap-1 text-xs text-foreground font-medium hover:text-pastel-green transition-colors"
                    @click="walkthrough.nextStep()"
                >
                    Next <ChevronRight class="h-3.5 w-3.5" />
                </button>
                <button
                    v-else
                    class="flex items-center gap-1 text-xs text-pastel-green font-medium hover:text-pastel-green/80 transition-colors"
                    @click="walkthrough.exitDemo()"
                >
                    Finish
                </button>
            </div>
        </div>
    </div>
</template>
