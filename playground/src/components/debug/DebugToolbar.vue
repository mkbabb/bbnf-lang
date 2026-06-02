<script setup lang="ts">
import { Tooltip, TooltipTrigger, TooltipContent } from "@mkbabb/glass-ui/tooltip";
import { DockIconButton } from "@mkbabb/glass-ui/dock";
import { Play, StepForward, ArrowDownToLine, Square } from "lucide-vue-next";
import type { UseDebugSessionReturn } from "@/components/debug/useDebugSession";

defineProps<{
    session: UseDebugSessionReturn;
}>();
</script>

<template>
    <div class="flex items-center gap-0.5">
        <Tooltip>
            <TooltipTrigger as-child>
                <DockIconButton
                    compact
                    class="debug-btn"
                    :disabled="session.isRunning.value"
                    @click="session.active.value ? session.continueExec() : session.start()"
                >
                    <Play class="h-3.5 w-3.5 sm:h-4 sm:w-4" />
                </DockIconButton>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8">
                {{ session.active.value ? "Continue" : "Start" }}
            </TooltipContent>
        </Tooltip>

        <Tooltip>
            <TooltipTrigger as-child>
                <DockIconButton
                    compact
                    class="debug-btn"
                    :disabled="!session.active.value || session.isRunning.value"
                    @click="session.stepRule()"
                >
                    <StepForward class="h-3.5 w-3.5 sm:h-4 sm:w-4" />
                </DockIconButton>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8">Step Rule</TooltipContent>
        </Tooltip>

        <Tooltip>
            <TooltipTrigger as-child>
                <DockIconButton
                    compact
                    class="debug-btn"
                    :disabled="!session.active.value || session.isRunning.value"
                    @click="session.stepNode()"
                >
                    <ArrowDownToLine class="h-3.5 w-3.5 sm:h-4 sm:w-4" />
                </DockIconButton>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8">Step Node</TooltipContent>
        </Tooltip>

        <div class="dock-separator" />

        <Tooltip>
            <TooltipTrigger as-child>
                <DockIconButton
                    compact
                    class="debug-btn"
                    :disabled="!session.active.value"
                    @click="session.stop()"
                >
                    <Square class="h-3 w-3 sm:h-3.5 sm:w-3.5" />
                </DockIconButton>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8">Stop</TooltipContent>
        </Tooltip>
    </div>
</template>

<style scoped>
.debug-btn {
    width: 1.75rem;
    height: 1.75rem;
}
@media (min-width: 640px) {
    .debug-btn {
        width: 2rem;
        height: 2rem;
    }
}
</style>
