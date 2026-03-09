<script setup lang="ts">
import { ref, computed } from "vue";
import { Select, SelectTrigger, SelectValue, SelectContent, SelectItem } from "@/components/ui/select";
import { Slider } from "@/components/ui/slider";
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";
import { Dialog, DialogContent, DialogTitle, DialogDescription, DialogTrigger } from "@/components/ui/dialog";
import { AlertCircle, Loader2 } from "lucide-vue-next";
import type { PipelineError } from "@/composables/usePipeline";
import type { Example } from "@/composables/useExamples";

const props = defineProps<{
    examples: Example[];
    currentExample: Example;
    printerConfig: { maxWidth: number; indent: number; useTabs: boolean };
    errors: PipelineError[];
    isProcessing: boolean;
}>();

const emit = defineEmits<{
    selectExample: [name: string];
}>();

const errorDialogOpen = ref(false);

const widthModel = computed({
    get: () => [props.printerConfig.maxWidth],
    set: (val: number[]) => { props.printerConfig.maxWidth = val[0]; },
});

const indentLabel = computed(() => {
    if (props.printerConfig.useTabs) return "Tab";
    return String(props.printerConfig.indent);
});

const indentModel = computed({
    get: () => props.printerConfig.useTabs ? "tab" : String(props.printerConfig.indent),
    set: (val: string) => {
        if (val === "tab") {
            props.printerConfig.useTabs = true;
            props.printerConfig.indent = 1;
        } else {
            props.printerConfig.useTabs = false;
            props.printerConfig.indent = parseInt(val, 10);
        }
    },
});

/** Grammar names that get the gold shimmer treatment. */
const shimmerNames = new Set(["JSON"]);

const sourceLabels: Record<string, string> = {
    grammar: "Grammar",
    parse: "Parse",
    format: "Format",
    import: "Import",
};
</script>

<template>
    <div class="flex justify-center px-4 pb-3 pt-1">
        <div class="flex items-center gap-5 rounded-2xl border border-border/40 bg-card/60 backdrop-blur-2xl backdrop-saturate-150 px-6 py-3">
            <!-- Example selector — no label, Instrument Serif, no bg trigger -->
            <Select
                :model-value="currentExample.name"
                @update:model-value="(v: string) => emit('selectExample', v)"
            >
                <SelectTrigger class="w-auto h-9 border-0 bg-transparent shadow-none px-1 gap-1.5 instrument-serif text-base text-foreground">
                    <SelectValue placeholder="Example" />
                </SelectTrigger>
                <SelectContent align="center" :side-offset="8">
                    <SelectItem v-for="ex in examples" :key="ex.name" :value="ex.name">
                        <span :class="shimmerNames.has(ex.name) ? 'gold-shimmer' : ''">{{ ex.name }}</span>
                    </SelectItem>
                </SelectContent>
            </Select>

            <!-- Divider -->
            <div class="h-6 w-px bg-border/30" />

            <!-- Width slider -->
            <div class="flex items-center gap-2.5">
                <label class="instrument-serif text-base text-muted-foreground whitespace-nowrap">Width</label>
                <Slider v-model="widthModel" :min="40" :max="120" :step="1" class="w-24" />
                <span class="w-7 text-center tabular-nums text-sm text-foreground">{{ printerConfig.maxWidth }}</span>
            </div>

            <!-- Divider -->
            <div class="h-6 w-px bg-border/30" />

            <!-- Indent selector — compact, no bg on trigger -->
            <div class="flex items-center gap-2">
                <label class="instrument-serif text-base text-muted-foreground whitespace-nowrap">Indent</label>
                <Select v-model="indentModel">
                    <SelectTrigger class="w-14 h-8 text-sm border-0 bg-transparent shadow-none px-1 justify-center gap-1">
                        <span class="tabular-nums">{{ indentLabel }}</span>
                    </SelectTrigger>
                    <SelectContent align="center" :side-offset="8">
                        <SelectItem value="2">2</SelectItem>
                        <SelectItem value="4">4</SelectItem>
                        <SelectItem value="tab">Tab</SelectItem>
                    </SelectContent>
                </Select>
            </div>

            <!-- Divider -->
            <div class="h-6 w-px bg-border/30" />

            <!-- Status — fixed width to prevent layout shift -->
            <div class="flex items-center justify-center w-20">
                <Loader2 v-if="isProcessing" class="h-4 w-4 animate-spin text-muted-foreground" />

                <template v-else-if="errors.length > 0">
                    <Dialog v-model:open="errorDialogOpen">
                        <Tooltip>
                            <TooltipTrigger as-child>
                                <DialogTrigger as-child>
                                    <button class="flex items-center gap-1 text-sm text-destructive hover:text-destructive/80 transition-colors cursor-pointer">
                                        <AlertCircle class="h-4 w-4 shrink-0" />
                                        <span>{{ errors.length }} error{{ errors.length > 1 ? "s" : "" }}</span>
                                    </button>
                                </DialogTrigger>
                            </TooltipTrigger>
                            <TooltipContent side="top" :side-offset="8">
                                <p class="text-xs">Click to view details</p>
                            </TooltipContent>
                        </Tooltip>
                        <DialogContent>
                            <DialogTitle class="instrument-serif text-xl text-destructive">Errors</DialogTitle>
                            <DialogDescription class="sr-only">Pipeline errors</DialogDescription>
                            <div class="mt-3 space-y-3 max-h-80 overflow-y-auto">
                                <div v-for="(err, i) in errors" :key="i" class="rounded-lg border border-destructive/20 bg-destructive/5 p-3">
                                    <div class="flex items-center gap-2 mb-1">
                                        <span class="text-xs font-medium uppercase tracking-wider text-destructive/70">{{ sourceLabels[err.source] ?? err.source }}</span>
                                        <span v-if="err.line" class="text-xs text-muted-foreground">line {{ err.line }}<template v-if="err.column">:{{ err.column }}</template></span>
                                    </div>
                                    <p class="text-sm font-mono text-foreground/90 break-words">{{ err.message }}</p>
                                </div>
                            </div>
                        </DialogContent>
                    </Dialog>
                </template>

                <span v-else class="instrument-serif text-base text-pastel-green">OK</span>
            </div>
        </div>
    </div>
</template>

<style scoped>
.gold-shimmer {
    background: linear-gradient(90deg, #b8860b, #ffd700, #daa520, #ffd700, #b8860b);
    background-size: 200% 100%;
    background-clip: text;
    -webkit-background-clip: text;
    color: transparent;
    animation: shimmer 3s linear infinite;
}

@keyframes shimmer {
    0% { background-position: 200% 0; }
    100% { background-position: -200% 0; }
}
</style>
