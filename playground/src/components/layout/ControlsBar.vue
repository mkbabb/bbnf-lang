<script setup lang="ts">
import { ref, computed } from "vue";
import { Select, SelectTrigger, SelectValue, SelectContent, SelectItem } from "@/components/ui/select";
import { Slider } from "@/components/ui/slider";
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";
import { Dialog, DialogContent, DialogTitle, DialogDescription, DialogTrigger } from "@/components/ui/dialog";
import InlineRichText from "@/components/ui/InlineRichText.vue";
import { AlertCircle, Copy, GitBranch, Link2, Loader2, RotateCcw, Settings2 } from "lucide-vue-next";
import type { PipelineError } from "@/composables/usePipeline";
import type { Example } from "@/composables/useExamples";

const AUTO_ENTRY_VALUE = "__auto__";

const exampleIcons: Record<string, string> = {
    JSON: "/img/json.svg",
    CSS: "/img/css.svg",
    BBNF: "/img/bbnf.png",
    Math: "/img/math.svg",
    Hello: "/img/text.svg",
};

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

const errorDialogOpen = ref(false);
const settingsOpen = ref(false);
const descriptionScrollDistances = ref<Record<string, number>>({});

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
        ? `Uses “${firstEntryRule.value}” as the compiled default.`
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
            ? `\`Auto\` starts from “${firstEntryRule.value}”. Pick another rule here to change the parser entry.`
            : "`Auto` starts from the first compiled rule. Pick another rule here to change the parser entry.";
    }

    return `Parsing currently starts from “${selectedEntryRuleOption.value?.label}”. Switch this to change the parser entry.`;
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

const exampleToneMap: Record<string, string> = {
    JSON: "border-pastel-green/25 hover:border-pastel-green/45 hover:bg-pastel-green/5",
    CSS: "border-pastel-blue/25 hover:border-pastel-blue/45 hover:bg-pastel-blue/5",
    BBNF: "border-pastel-amber/25 hover:border-pastel-amber/45 hover:bg-pastel-amber/5",
    Math: "border-pastel-purple/25 hover:border-pastel-purple/45 hover:bg-pastel-purple/5",
    Hello: "border-pastel-green/25 hover:border-pastel-green/45 hover:bg-pastel-green/5",
};

const tagToneMap: Record<string, string> = {
    "@pretty": "pastel-pink",
    "@recover": "pastel-blue",
    "error recovery": "pastel-blue",
    recursive: "pastel-green",
    nesting: "pastel-purple",
    precedence: "pastel-amber",
    "self-hosting": "pastel-amber",
    meta: "pastel-purple",
    beginner: "pastel-green",
};

function exampleToneClass(name: string): string {
    return exampleToneMap[name] ?? "border-border/35 hover:border-border/60 hover:bg-accent/40";
}

function tagToneColor(tag: string): string {
    return tagToneMap[tag.toLowerCase()] ?? "muted-foreground";
}

function tagToneStyle(tag: string) {
    const color = tagToneColor(tag);
    return {
        color: `var(--color-${color})`,
        background: `color-mix(in srgb, var(--color-${color}) 12%, transparent)`,
        border: `1px solid color-mix(in srgb, var(--color-${color}) 24%, transparent)`,
        boxShadow: `inset 0 1px 0 color-mix(in srgb, var(--color-${color}) 10%, transparent)`,
    };
}

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
    return {
        "--description-scroll-distance": `${distance}px`,
    };
}

function hasDescriptionOverflow(name: string) {
    return (descriptionScrollDistances.value[name] ?? 0) > 8;
}

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
                                                    class="inline-flex items-center rounded-full px-2 py-0.5 font-mono text-[10px] tracking-[0.04em] backdrop-blur-sm"
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
                                                class="rounded border px-1.5 py-0.5 font-mono text-[11px]"
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

                <template v-else-if="errors.length > 0">
                    <Dialog v-model:open="errorDialogOpen">
                        <Tooltip>
                            <TooltipTrigger as-child>
                                <DialogTrigger as-child>
                                    <button class="flex items-center gap-1 rounded-xl px-2 py-1 text-xs text-destructive transition-colors hover:bg-destructive/8 hover:text-destructive/80 sm:text-sm">
                                        <AlertCircle class="h-3.5 w-3.5 shrink-0 sm:h-4.5 sm:w-4.5" />
                                        <span class="font-mono">{{ errors.length }}</span>
                                    </button>
                                </DialogTrigger>
                            </TooltipTrigger>
                            <TooltipContent side="top" :side-offset="8">
                                <p class="text-xs">Inspect errors and jump to the failing editor span.</p>
                            </TooltipContent>
                        </Tooltip>
                        <DialogContent class="max-w-lg">
                            <DialogTitle class="instrument-serif text-xl text-destructive">Errors</DialogTitle>
                            <DialogDescription class="sr-only">Pipeline errors</DialogDescription>
                            <div class="mt-4 space-y-3 max-h-96 overflow-y-auto">
                                <button
                                    v-for="(err, i) in errors"
                                    :key="i"
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

                <Dialog v-model:open="settingsOpen">
                    <Tooltip>
                        <TooltipTrigger as-child>
                            <DialogTrigger as-child>
                                <button class="flex h-9 items-center gap-1.5 rounded-xl border border-border/35 bg-background/30 px-2.5 text-muted-foreground transition-all hover:border-border/55 hover:bg-background/45 hover:text-foreground sm:h-10">
                                    <Settings2 class="h-4 w-4 shrink-0" />
                                    <span class="hidden font-mono text-xs sm:inline">{{ printerConfig.maxWidth }}w · {{ indentLabel }}</span>
                                </button>
                            </DialogTrigger>
                        </TooltipTrigger>
                        <TooltipContent side="top" :side-offset="8" class="max-w-xs border-border/40 bg-card/90 backdrop-blur-xl">
                            <p class="text-xs sm:text-sm">Tune print width and indentation for the formatter output.</p>
                        </TooltipContent>
                    </Tooltip>
                    <DialogContent class="max-w-sm">
                        <DialogTitle class="instrument-serif text-xl">Formatter Settings</DialogTitle>
                        <DialogDescription class="sr-only">Configure formatter width and indentation</DialogDescription>
                        <div class="mt-4 space-y-5">
                            <div class="space-y-2">
                                <div class="flex items-center justify-between">
                                    <label class="instrument-serif text-base text-muted-foreground">Print Width</label>
                                    <span class="tabular-nums text-sm text-foreground font-mono">{{ printerConfig.maxWidth }}</span>
                                </div>
                                <Slider v-model="widthModel" :min="40" :max="120" :step="1" />
                            </div>

                            <div class="space-y-2">
                                <label class="instrument-serif text-base text-muted-foreground">Indentation</label>
                                <Select v-model="indentModel">
                                    <SelectTrigger class="h-9 w-full">
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
    animation: description-marquee 5.5s cubic-bezier(0.4, 0, 0.2, 1) infinite alternate;
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
