<script setup lang="ts">
import { computed } from "vue";
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";
import FormatterSettings from "@/components/layout/FormatterSettings.vue";
import { Copy, Link2, RotateCcw } from "lucide-vue-next";

const props = defineProps<{
    canCopyResult: boolean;
    activeResultLabel: string;
    printerConfig: { maxWidth: number; indent: number; useTabs: boolean };
}>();

const emit = defineEmits<{
    copyResult: [];
    shareLink: [];
    resetPlayground: [];
}>();

const copyLabel = computed(() => props.activeResultLabel === "Parsed AST" ? "Copy AST" : "Copy Formatted");
</script>

<template>
    <div class="flex items-center gap-0.5">
        <Tooltip>
            <TooltipTrigger as-child>
                <button
                    type="button"
                    :disabled="!canCopyResult"
                    class="dock-icon-btn"
                    @click="emit('copyResult')"
                >
                    <Copy class="h-4 w-4" />
                </button>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8" class="max-w-xs">
                <p class="text-sm sm:text-base">{{ copyLabel }}</p>
            </TooltipContent>
        </Tooltip>

        <Tooltip>
            <TooltipTrigger as-child>
                <button
                    type="button"
                    class="dock-icon-btn"
                    @click="emit('shareLink')"
                >
                    <Link2 class="h-4 w-4" />
                </button>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8" class="max-w-xs">
                <p class="text-sm sm:text-base">Share Link</p>
            </TooltipContent>
        </Tooltip>

        <Tooltip>
            <TooltipTrigger as-child>
                <button
                    type="button"
                    class="dock-icon-btn"
                    @click="emit('resetPlayground')"
                >
                    <RotateCcw class="h-4 w-4" />
                </button>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8" class="max-w-xs">
                <p class="text-sm sm:text-base">Reset playground</p>
            </TooltipContent>
        </Tooltip>

        <FormatterSettings :printer-config="printerConfig" />
    </div>
</template>
