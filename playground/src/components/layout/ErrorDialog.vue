<script setup lang="ts">
import { ref, watch, inject } from "vue";
import { Dialog, DialogContent, DialogTitle, DialogDescription, DialogTrigger } from "@/components/ui/dialog";
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";
import { AlertCircle } from "lucide-vue-next";
import type { PipelineError } from "@/composables/usePipeline";

defineProps<{
    errors: PipelineError[];
}>();

const emit = defineEmits<{
    jumpToError: [error: PipelineError];
}>();

const errorDialogOpen = ref(false);

const dockKeepOpen = inject<(() => void) | null>("dockKeepOpen", null);
const dockRelease = inject<(() => void) | null>("dockRelease", null);

watch(errorDialogOpen, (open) => {
    if (open) dockKeepOpen?.();
    else dockRelease?.();
});

const sourceLabels: Record<string, string> = {
    grammar: "Grammar",
    parse: "Parse",
    format: "Format",
    import: "Import",
};

function onJumpToError(error: PipelineError) {
    errorDialogOpen.value = false;
    emit("jumpToError", error);
}
</script>

<template>
    <Dialog v-model:open="errorDialogOpen">
        <Tooltip>
            <TooltipTrigger as-child>
                <DialogTrigger as-child>
                    <button class="flex items-center gap-1 rounded-xl px-2 py-1 text-sm text-destructive transition-colors hover:bg-destructive/8 hover:text-destructive/80 sm:text-base">
                        <AlertCircle class="h-3.5 w-3.5 shrink-0 sm:h-4.5 sm:w-4.5" />
                        <span class="font-mono">{{ errors.length }}</span>
                    </button>
                </DialogTrigger>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8">
                <p class="text-sm">Inspect errors and jump to the failing editor span.</p>
            </TooltipContent>
        </Tooltip>
        <DialogContent class="max-w-lg">
            <DialogTitle class="instrument-serif text-xl text-destructive">Errors</DialogTitle>
            <DialogDescription class="sr-only">Pipeline errors</DialogDescription>
            <div class="mt-4 space-y-3 max-h-96 overflow-y-auto">
                <button
                    v-for="(err, i) in errors"
                    :key="err.message + (err.line ?? i)"
                    type="button"
                    class="w-full rounded-xl border border-destructive/20 bg-destructive/5 p-3 text-left transition-colors hover:bg-destructive/10"
                    @click="onJumpToError(err)"
                >
                    <div class="mb-1 flex items-center gap-2">
                        <span class="text-xs font-medium uppercase tracking-wider text-destructive/70">{{ sourceLabels[err.source] ?? err.source }}</span>
                        <span v-if="err.line" class="font-mono text-xs text-muted-foreground">line {{ err.line }}<template v-if="err.column">:{{ err.column }}</template></span>
                    </div>
                    <p class="text-sm font-mono text-foreground/90 break-words">{{ err.message }}</p>
                </button>
            </div>
        </DialogContent>
    </Dialog>
</template>
