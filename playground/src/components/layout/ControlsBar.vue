<script setup lang="ts">
import { ref, computed } from "vue";
import { Select, SelectTrigger, SelectValue, SelectContent, SelectItem } from "@/components/ui/select";
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";
import InlineRichText from "@/components/ui/InlineRichText.vue";
import ErrorDialog from "@/components/layout/ErrorDialog.vue";
import FormatterSettings from "@/components/layout/FormatterSettings.vue";
import { Copy, GitBranch, Link2, Loader2, RotateCcw } from "lucide-vue-next";
import { exampleIcons, exampleToneClass, shimmerClass, tagToneStyle } from "@/lib/toneMaps";
import type { PipelineError } from "@/composables/usePipeline";
import type { Example } from "@/composables/useExamples";

const AUTO_ENTRY_VALUE = "__auto__";

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

const descriptionScrollDistances = ref<Record<string, number>>({});

const entryRuleModel = computed({
    get: () => props.entryRule || AUTO_ENTRY_VALUE,
    set: (value: string) => {
        emit("selectEntryRule", value === AUTO_ENTRY_VALUE ? "" : value);
    },
});

const copyLabel = computed(() => props.activeResultLabel === "Parsed AST" ? "Copy AST" : "Copy Formatted");
const firstEntryRule = computed(() => props.availableEntryRules[0] ?? "");
const entryRuleOptions = computed(() => {
    const autoDetail = firstEntryRule.value
        ? `Uses "${firstEntryRule.value}" as the compiled default.`
        : "Uses the first compiled rule in the grammar.";

    return [
        {
            value: AUTO_ENTRY_VALUE,
            label: "Auto",
            detail: autoDetail,
            toneClass: "text-pastel-blue bg-pastel-blue/12 border-pastel-blue/25",
        },
        ...props.availableEntryRules.map((rule, index) => ({
            value: rule,
            label: rule,
            detail: index === 0 ? "Current first compiled rule." : "",
            toneClass: index === 0
                ? "text-pastel-green bg-pastel-green/12 border-pastel-green/25"
                : "text-pastel-purple bg-pastel-purple/12 border-pastel-purple/25",
        })),
    ];
});

const selectedEntryRuleOption = computed(() => {
    return entryRuleOptions.value.find((option) => option.value === entryRuleModel.value) ?? entryRuleOptions.value[0];
});

const entryRuleTooltip = computed(() => {
    if (selectedEntryRuleOption.value?.value === AUTO_ENTRY_VALUE) {
        return firstEntryRule.value
            ? `\`Auto\` starts from "${firstEntryRule.value}". Pick another rule here to change the parser entry.`
            : "`Auto` starts from the first compiled rule. Pick another rule here to change the parser entry.";
    }

    return `Parsing currently starts from "${selectedEntryRuleOption.value?.label}". Switch this to change the parser entry.`;
});

function measureDescriptionScroll(name: string, event: Event) {
    const item = event.currentTarget as HTMLElement | null;
    if (!item) return;

    const viewport = item.querySelector("[data-example-description-viewport]") as HTMLElement | null;
    const track = item.querySelector("[data-example-description-track]") as HTMLElement | null;
    if (!viewport || !track) return;

    descriptionScrollDistances.value[name] = Math.max(0, track.scrollWidth - viewport.clientWidth);
}

function descriptionScrollStyle(name: string) {
    const distance = descriptionScrollDistances.value[name] ?? 0;
    return { "--description-scroll-distance": `${distance}px` };
}

function hasDescriptionOverflow(name: string) {
    return (descriptionScrollDistances.value[name] ?? 0) > 8;
}
</script>

<template>
    <div class="flex justify-center px-2 sm:px-4 pb-2 pt-1">
        <div class="flex w-full max-w-[78rem] flex-wrap items-center justify-center gap-2 rounded-[1.75rem] border border-border/30 bg-card/60 px-3 py-2 backdrop-blur-xl shadow-lg">
            <Tooltip>
                <TooltipTrigger as-child>
                    <div>
                        <Select
                            :model-value="currentExample.name"
                            @update:model-value="(v: string) => emit('selectExample', v)"
                        >
                            <SelectTrigger class="h-9 w-auto max-w-full border-none bg-transparent px-2 shadow-none instrument-serif text-sm text-foreground rounded-xl sm:h-10 sm:text-base [&>span]:flex [&>span]:items-center [&>span]:gap-2">
                                <SelectValue>
                                    <img :src="exampleIcons[currentExample.name] ?? '/img/text.svg'" alt="" class="h-4 w-4 shrink-0 sm:h-5 sm:w-5" />
                                    <span :class="shimmerClass(currentExample.name)">{{ currentExample.name }}</span>
                                </SelectValue>
                            </SelectTrigger>
                            <SelectContent align="start" :side-offset="10" class="min-w-[18rem] max-w-[23rem]">
                                <SelectItem
                                    v-for="ex in examples"
                                    :key="ex.name"
                                    :value="ex.name"
                                    class="group/example rounded-xl border bg-card/35 py-3 pl-9 pr-3 transition-all duration-200 hover:-translate-y-0.5"
                                    :class="exampleToneClass(ex.name)"
                                    @pointerenter="measureDescriptionScroll(ex.name, $event)"
                                    @focusin="measureDescriptionScroll(ex.name, $event)"
                                >
                                    <div class="flex items-start gap-3">
                                        <img :src="exampleIcons[ex.name] ?? '/img/text.svg'" alt="" class="mt-0.5 h-5 w-5 shrink-0" />
                                        <div class="min-w-0">
                                            <div class="flex items-center gap-2">
                                                <span class="instrument-serif text-base" :class="shimmerClass(ex.name)">{{ ex.name }}</span>
                                            </div>
                                            <div
                                                data-example-description-viewport
                                                class="mt-1 overflow-hidden"
                                            >
                                                <p
                                                    data-example-description-track
                                                    class="example-description-track text-sm text-muted-foreground whitespace-nowrap"
                                                    :data-overflow="hasDescriptionOverflow(ex.name) ? 'true' : undefined"
                                                    :style="descriptionScrollStyle(ex.name)"
                                                >
                                                    <InlineRichText :text="ex.description" />
                                                </p>
                                            </div>
                                            <div v-if="ex.tags?.length" class="mt-2 flex flex-wrap gap-1">
                                                <span
                                                    v-for="tag in ex.tags"
                                                    :key="tag"
                                                    class="inline-flex items-center rounded-full px-2 py-0.5 font-mono text-[length:var(--font-size-label)] tracking-[0.04em] backdrop-blur-sm"
                                                    :style="tagToneStyle(tag)"
                                                >
                                                    {{ tag }}
                                                </span>
                                            </div>
                                        </div>
                                    </div>
                                </SelectItem>
                            </SelectContent>
                        </Select>
                    </div>
                </TooltipTrigger>
                <TooltipContent side="top" :side-offset="8" class="max-w-xs border-border/40 bg-card/90 backdrop-blur-xl">
                    <p class="text-xs sm:text-sm">Swap between showcase grammars.</p>
                </TooltipContent>
            </Tooltip>

            <Tooltip>
                <TooltipTrigger as-child>
                    <div>
                        <Select v-model="entryRuleModel">
                            <SelectTrigger class="h-9 min-w-[9.25rem] max-w-full border-border/35 bg-background/35 px-2.5 shadow-none rounded-xl sm:h-10 sm:min-w-[11rem]">
                                <div class="flex items-center gap-2 overflow-hidden">
                                    <GitBranch class="h-4 w-4 shrink-0 text-pastel-blue" />
                                    <span class="truncate font-mono text-xs sm:text-sm">
                                        {{ selectedEntryRuleOption?.label === "Auto" ? "Auto (first rule)" : selectedEntryRuleOption?.label }}
                                    </span>
                                </div>
                            </SelectTrigger>
                            <SelectContent align="start" :side-offset="10" class="min-w-[18rem] max-w-[22rem]">
                                <SelectItem
                                    v-for="option in entryRuleOptions"
                                    :key="option.value"
                                    :value="option.value"
                                    class="rounded-xl border border-border/35 bg-card/35 py-3 pl-9 pr-3"
                                >
                                    <div class="min-w-0">
                                        <div class="flex items-center gap-2">
                                            <span
                                                class="rounded border px-1.5 py-0.5 font-mono text-xs"
                                                :class="option.toneClass"
                                            >
                                                {{ option.label === "Auto" ? "default" : "entry" }}
                                            </span>
                                            <span class="font-mono text-sm text-foreground">{{ option.label === "Auto" ? "Auto (first rule)" : option.label }}</span>
                                        </div>
                                        <p v-if="option.detail" class="mt-1 text-xs text-muted-foreground">{{ option.detail }}</p>
                                    </div>
                                </SelectItem>
                            </SelectContent>
                        </Select>
                    </div>
                </TooltipTrigger>
                <TooltipContent side="top" :side-offset="8" class="max-w-xs border-border/40 bg-card/90 backdrop-blur-xl">
                    <p class="text-xs sm:text-sm">
                        <InlineRichText :text="entryRuleTooltip" />
                    </p>
                </TooltipContent>
            </Tooltip>

            <div class="mx-1 hidden h-6 w-px bg-border/35 md:block" />

            <div class="flex items-center justify-center">
                <Loader2 v-if="isProcessing" class="h-4 w-4 animate-spin text-muted-foreground sm:h-5 sm:w-5" />

                <ErrorDialog
                    v-else-if="errors.length > 0"
                    :errors="errors"
                    @jump-to-error="(err) => emit('jumpToError', err)"
                />

                <Tooltip v-else>
                    <TooltipTrigger as-child>
                        <span class="rounded-full border border-pastel-green/20 bg-pastel-green/10 px-2.5 py-1 instrument-serif text-sm text-pastel-green">
                            OK
                        </span>
                    </TooltipTrigger>
                    <TooltipContent side="top" :side-offset="8" class="border-border/40 bg-card/90 backdrop-blur-xl">
                        <p class="text-xs sm:text-sm">No grammar, parse, or format errors.</p>
                    </TooltipContent>
                </Tooltip>
            </div>

            <div class="mx-1 hidden h-6 w-px bg-border/35 lg:block" />

            <div class="flex flex-wrap items-center justify-center gap-1.5">
                <Tooltip>
                    <TooltipTrigger as-child>
                        <button
                            type="button"
                            :disabled="!canCopyResult"
                            class="flex h-9 items-center gap-1.5 rounded-xl border border-border/35 bg-background/30 px-2.5 text-muted-foreground transition-all hover:border-border/55 hover:bg-background/45 hover:text-foreground disabled:pointer-events-none disabled:opacity-40 sm:h-10"
                            @click="emit('copyResult')"
                        >
                            <Copy class="h-4 w-4 shrink-0" />
                            <span class="hidden font-mono text-xs sm:inline">{{ copyLabel }}</span>
                        </button>
                    </TooltipTrigger>
                    <TooltipContent side="top" :side-offset="8" class="max-w-xs border-border/40 bg-card/90 backdrop-blur-xl">
                        <p class="text-xs sm:text-sm">Copy the current {{ activeResultLabel.toLowerCase() }} pane.</p>
                    </TooltipContent>
                </Tooltip>

                <Tooltip>
                    <TooltipTrigger as-child>
                        <button
                            type="button"
                            class="flex h-9 items-center gap-1.5 rounded-xl border border-border/35 bg-background/30 px-2.5 text-muted-foreground transition-all hover:border-border/55 hover:bg-background/45 hover:text-foreground sm:h-10"
                            @click="emit('shareLink')"
                        >
                            <Link2 class="h-4 w-4 shrink-0" />
                            <span class="hidden font-mono text-xs sm:inline">Share Link</span>
                        </button>
                    </TooltipTrigger>
                    <TooltipContent side="top" :side-offset="8" class="max-w-xs border-border/40 bg-card/90 backdrop-blur-xl">
                        <p class="text-xs sm:text-sm">Copy a playground URL with grammar, input, entry rule, and formatter settings.</p>
                    </TooltipContent>
                </Tooltip>

                <Tooltip>
                    <TooltipTrigger as-child>
                        <button
                            type="button"
                            class="flex h-9 items-center gap-1.5 rounded-xl border border-border/35 bg-background/30 px-2.5 text-muted-foreground transition-all hover:border-border/55 hover:bg-background/45 hover:text-foreground sm:h-10"
                            @click="emit('resetPlayground')"
                        >
                            <RotateCcw class="h-4 w-4 shrink-0" />
                            <span class="hidden font-mono text-xs sm:inline">Reset</span>
                        </button>
                    </TooltipTrigger>
                    <TooltipContent side="top" :side-offset="8" class="max-w-xs border-border/40 bg-card/90 backdrop-blur-xl">
                        <p class="text-xs sm:text-sm">Restore the selected example, default tabs, formatter settings, and the current split balance.</p>
                    </TooltipContent>
                </Tooltip>

                <FormatterSettings :printer-config="printerConfig" />
            </div>
        </div>
    </div>
</template>

<style scoped>
.example-description-track {
    display: inline-block;
    min-width: 100%;
    padding-right: 1.5rem;
}

.group\/example:hover .example-description-track[data-overflow="true"],
.group\/example:focus-within .example-description-track[data-overflow="true"] {
    animation: description-marquee 5.5s var(--ease-smooth) infinite alternate;
}

@keyframes description-marquee {
    0%, 18% { transform: translateX(0); }
    82%, 100% { transform: translateX(calc(-1 * var(--description-scroll-distance, 0px))); }
}

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
