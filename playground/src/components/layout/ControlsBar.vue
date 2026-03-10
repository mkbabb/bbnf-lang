<script setup lang="ts">
import { ref, computed } from "vue";
import { Select, SelectTrigger, SelectValue, SelectContent, SelectItem } from "@/components/ui/select";
import { Slider } from "@/components/ui/slider";
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";
import { Dialog, DialogContent, DialogTitle, DialogDescription, DialogTrigger } from "@/components/ui/dialog";
import {
    HoverCardRoot,
    HoverCardTrigger,
    HoverCardPortal,
    HoverCardContent,
} from "reka-ui";
import { AlertCircle, Loader2, Settings2 } from "lucide-vue-next";
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
const settingsOpen = ref(false);

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

/** Per-language shimmer color map. */
const shimmerMap: Record<string, "gold" | "blue"> = {
    BBNF: "gold",
    CSS: "blue",
};

function shimmerClass(name: string): string {
    const color = shimmerMap[name];
    if (color === "gold") return "gold-shimmer";
    if (color === "blue") return "blue-shimmer";
    return "";
}

const sourceLabels: Record<string, string> = {
    grammar: "Grammar",
    parse: "Parse",
    format: "Format",
    import: "Import",
};
</script>

<template>
    <div class="flex items-center justify-between px-2 sm:px-4 pb-2 pt-1 gap-2">
        <!-- Left: Example selector -->
        <Select
            :model-value="currentExample.name"
            @update:model-value="(v: string) => emit('selectExample', v)"
        >
            <SelectTrigger class="w-auto h-8 border border-border/30 bg-card/60 backdrop-blur-sm shadow-none px-2 gap-1.5 instrument-serif text-sm text-foreground rounded-lg">
                <SelectValue placeholder="Example" />
            </SelectTrigger>
            <SelectContent align="start" :side-offset="8">
                <SelectItem v-for="ex in examples" :key="ex.name" :value="ex.name">
                    <HoverCardRoot :open-delay="400">
                        <HoverCardTrigger as-child>
                            <span :class="shimmerClass(ex.name)">{{ ex.name }}</span>
                        </HoverCardTrigger>
                        <HoverCardPortal>
                            <HoverCardContent
                                side="right"
                                :side-offset="12"
                                class="w-64 rounded-xl border border-border/40 bg-card/90 backdrop-blur-xl p-3 shadow-lg z-[100]"
                            >
                                <p class="instrument-serif text-base mb-1">{{ ex.name }}</p>
                                <p class="text-xs text-muted-foreground mb-2">{{ ex.description }}</p>
                                <div v-if="ex.tags?.length" class="flex flex-wrap gap-1">
                                    <span
                                        v-for="tag in ex.tags"
                                        :key="tag"
                                        class="rounded-full bg-muted/40 px-2 py-0.5 text-[10px] text-muted-foreground"
                                    >
                                        {{ tag }}
                                    </span>
                                </div>
                            </HoverCardContent>
                        </HoverCardPortal>
                    </HoverCardRoot>
                </SelectItem>
            </SelectContent>
        </Select>

        <!-- Center: Status -->
        <div class="flex items-center justify-center">
            <Loader2 v-if="isProcessing" class="h-4 w-4 animate-spin text-muted-foreground" />

            <template v-else-if="errors.length > 0">
                <Dialog v-model:open="errorDialogOpen">
                    <Tooltip>
                        <TooltipTrigger as-child>
                            <DialogTrigger as-child>
                                <button class="flex items-center gap-1 text-xs text-destructive hover:text-destructive/80 transition-colors cursor-pointer">
                                    <AlertCircle class="h-3.5 w-3.5 shrink-0" />
                                    <span>{{ errors.length }}</span>
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

            <span v-else class="instrument-serif text-sm text-pastel-green">OK</span>
        </div>

        <!-- Right: Settings button -->
        <Dialog v-model:open="settingsOpen">
            <DialogTrigger as-child>
                <button class="flex items-center gap-1.5 h-8 px-2 rounded-lg border border-border/30 bg-card/60 backdrop-blur-sm text-muted-foreground hover:text-foreground transition-colors">
                    <Settings2 class="h-3.5 w-3.5" />
                    <span class="text-xs font-mono hidden sm:inline">{{ printerConfig.maxWidth }}w · {{ indentLabel }}</span>
                </button>
            </DialogTrigger>
            <DialogContent class="max-w-sm">
                <DialogTitle class="instrument-serif text-xl">Formatter Settings</DialogTitle>
                <DialogDescription class="sr-only">Configure formatter width and indentation</DialogDescription>
                <div class="mt-4 space-y-5">
                    <!-- Width slider -->
                    <div class="space-y-2">
                        <div class="flex items-center justify-between">
                            <label class="instrument-serif text-base text-muted-foreground">Print Width</label>
                            <span class="tabular-nums text-sm text-foreground font-mono">{{ printerConfig.maxWidth }}</span>
                        </div>
                        <Slider v-model="widthModel" :min="40" :max="120" :step="1" />
                    </div>

                    <!-- Indent selector -->
                    <div class="space-y-2">
                        <label class="instrument-serif text-base text-muted-foreground">Indentation</label>
                        <Select v-model="indentModel">
                            <SelectTrigger class="w-full h-9">
                                <span class="tabular-nums">{{ indentLabel === "Tab" ? "Tab" : `${indentLabel} spaces` }}</span>
                            </SelectTrigger>
                            <SelectContent>
                                <SelectItem value="2">2 spaces</SelectItem>
                                <SelectItem value="4">4 spaces</SelectItem>
                                <SelectItem value="tab">Tab</SelectItem>
                            </SelectContent>
                        </Select>
                    </div>
                </div>
            </DialogContent>
        </Dialog>
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

.blue-shimmer {
    background: linear-gradient(90deg, #1e40af, #60a5fa, #3b82f6, #60a5fa, #1e40af);
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
