<script setup lang="ts">
import { Tooltip, TooltipTrigger, TooltipContent } from "@mkbabb/glass-ui";
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
                <button
                    class="dock-icon-btn !w-7 !h-7 sm:!w-8 sm:!h-8"
                    :disabled="session.isRunning.value"
                    @click="session.active.value ? session.continueExec() : session.start()"
                >
                    <Play class="h-3.5 w-3.5 sm:h-4 sm:w-4" />
                </button>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8">
                {{ session.active.value ? "Continue" : "Start" }}
            </TooltipContent>
        </Tooltip>

        <Tooltip>
            <TooltipTrigger as-child>
                <button
                    class="dock-icon-btn !w-7 !h-7 sm:!w-8 sm:!h-8"
                    :disabled="!session.active.value || session.isRunning.value"
                    @click="session.stepRule()"
                >
                    <StepForward class="h-3.5 w-3.5 sm:h-4 sm:w-4" />
                </button>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8">Step Rule</TooltipContent>
        </Tooltip>

        <Tooltip>
            <TooltipTrigger as-child>
                <button
                    class="dock-icon-btn !w-7 !h-7 sm:!w-8 sm:!h-8"
                    :disabled="!session.active.value || session.isRunning.value"
                    @click="session.stepNode()"
                >
                    <ArrowDownToLine class="h-3.5 w-3.5 sm:h-4 sm:w-4" />
                </button>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8">Step Node</TooltipContent>
        </Tooltip>

        <div class="dock-separator" />

        <Tooltip>
            <TooltipTrigger as-child>
                <button
                    class="dock-icon-btn !w-7 !h-7 sm:!w-8 sm:!h-8"
                    :disabled="!session.active.value"
                    @click="session.stop()"
                >
                    <Square class="h-3 w-3 sm:h-3.5 sm:w-3.5" />
                </button>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8">Stop</TooltipContent>
        </Tooltip>
    </div>
</template>
