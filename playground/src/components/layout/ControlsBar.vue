<script setup lang="ts">
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";
import GlassDock from "@/components/ui/GlassDock.vue";
import ErrorDialog from "@/components/layout/ErrorDialog.vue";
import ExampleSelector from "@/components/layout/ExampleSelector.vue";
import EntryRuleSelector from "@/components/layout/EntryRuleSelector.vue";
import ActionButtons from "@/components/layout/ActionButtons.vue";
import { Loader2, AlertCircle, Ellipsis } from "lucide-vue-next";
import { exampleIcons, shimmerClass } from "@/lib/toneMaps";
import type { PipelineError } from "@/composables/usePipeline";
import type { Example } from "@/composables/useExamples";

const props = defineProps<{
    examples: Example[];
    currentExample: Example;
    printerConfig: { maxWidth: number; indent: number; useTabs: boolean };
    errors: PipelineError[];
    isProcessing: boolean;
    entryRule: string;
    availableEntryRules: string[];
    activeResultLabel: string;
    canCopyResult: boolean;
}>();

const emit = defineEmits<{
    selectExample: [name: string];
    selectEntryRule: [value: string];
    copyResult: [];
    shareLink: [];
    resetPlayground: [];
    jumpToError: [error: PipelineError];
}>();
</script>

<template>
    <div class="absolute bottom-0 left-0 right-0 z-10 flex justify-center px-1 sm:px-4 pb-0.5 sm:pb-2 pt-1 sm:pt-1">
        <GlassDock :collapse-delay="2000" :start-collapsed="true" :fit-content="true">
            <!-- Expanded state -->
            <div class="flex items-center gap-1 sm:gap-1.5">
                <ExampleSelector
                    :examples="examples"
                    :current-example="currentExample"
                    @select-example="(name) => emit('selectExample', name)"
                />

                <EntryRuleSelector
                    class="hidden sm:flex"
                    :entry-rule="entryRule"
                    :available-entry-rules="availableEntryRules"
                    @select-entry-rule="(value) => emit('selectEntryRule', value)"
                />

                <div class="dock-separator hidden sm:block" />

                <div class="flex items-center justify-center">
                    <Loader2 v-if="isProcessing" class="h-4 w-4 animate-spin text-muted-foreground sm:h-5 sm:w-5" />

                    <ErrorDialog
                        v-else-if="errors.length > 0"
                        :errors="errors"
                        @jump-to-error="(err) => emit('jumpToError', err)"
                    />

                    <Tooltip v-else>
                        <TooltipTrigger as-child>
                            <span class="rounded-full border border-pastel-green/20 bg-pastel-green/10 px-2 py-0.5 sm:px-2.5 sm:py-1 instrument-serif text-xs sm:text-sm text-pastel-green">
                                OK
                            </span>
                        </TooltipTrigger>
                        <TooltipContent side="top" :side-offset="8">
                            <p class="text-sm sm:text-base">No grammar, parse, or format errors.</p>
                        </TooltipContent>
                    </Tooltip>
                </div>

                <div class="dock-separator" />

                <ActionButtons
                    :can-copy-result="canCopyResult"
                    :active-result-label="activeResultLabel"
                    :printer-config="printerConfig"
                    @copy-result="emit('copyResult')"
                    @share-link="emit('shareLink')"
                    @reset-playground="emit('resetPlayground')"
                />
            </div>

            <!-- Collapsed state -->
            <template #collapsed>
                <div class="flex items-center gap-2">
                    <img :src="exampleIcons[currentExample.name] ?? '/img/text.svg'" alt="" class="h-4 w-4 shrink-0" />
                    <span class="instrument-serif text-sm max-w-[120px] truncate" :class="shimmerClass(currentExample.name)">{{ currentExample.name }}</span>

                    <div class="dock-separator" />

                    <Loader2 v-if="isProcessing" class="h-3.5 w-3.5 animate-spin text-muted-foreground" />
                    <span v-else-if="errors.length > 0" class="flex items-center gap-1 text-destructive">
                        <AlertCircle class="h-3.5 w-3.5" />
                        <span class="font-mono text-xs">{{ errors.length }}</span>
                    </span>
                    <span v-else class="h-2 w-2 rounded-full bg-pastel-green" />

                    <Ellipsis class="h-3.5 w-3.5 text-muted-foreground/50" />
                </div>
            </template>
        </GlassDock>
    </div>
</template>
