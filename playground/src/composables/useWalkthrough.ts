import { ref, computed } from "vue";
import type { Demo, DemoStep } from "@/demos/types";

export function useWalkthrough() {
    const currentDemo = ref<Demo | null>(null);
    const currentStepIndex = ref(0);

    const isActive = computed(() => currentDemo.value !== null);
    const totalSteps = computed(() => currentDemo.value?.steps.length ?? 0);
    const progress = computed(() => totalSteps.value > 0 ? (currentStepIndex.value + 1) / totalSteps.value : 0);

    const currentStep = computed<DemoStep | null>(() => {
        if (!currentDemo.value) return null;
        return currentDemo.value.steps[currentStepIndex.value] ?? null;
    });

    function startDemo(demo: Demo) {
        currentDemo.value = demo;
        currentStepIndex.value = 0;
    }

    function nextStep() {
        if (currentStepIndex.value < totalSteps.value - 1) {
            currentStepIndex.value++;
        }
    }

    function prevStep() {
        if (currentStepIndex.value > 0) {
            currentStepIndex.value--;
        }
    }

    function exitDemo() {
        currentDemo.value = null;
        currentStepIndex.value = 0;
    }

    return {
        currentDemo,
        currentStepIndex,
        isActive,
        totalSteps,
        progress,
        currentStep,
        startDemo,
        nextStep,
        prevStep,
        exitDemo,
    };
}
