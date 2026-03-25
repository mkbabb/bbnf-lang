<script setup lang="ts">
import { computed, inject } from "vue";
import { Select, SelectTrigger, SelectContent, SelectItem, Tooltip, TooltipTrigger, TooltipContent } from "@mkbabb/glass-ui";
import InlineRichText from "@/components/ui/InlineRichText.vue";
import { GitBranch } from "lucide-vue-next";

const AUTO_ENTRY_VALUE = "__auto__";

const props = defineProps<{
    entryRule: string;
    availableEntryRules: string[];
}>();

const emit = defineEmits<{
    selectEntryRule: [value: string];
}>();

const dockKeepOpen = inject<(() => void) | null>("dockKeepOpen", null);
const dockRelease = inject<(() => void) | null>("dockRelease", null);

function onSelectOpenChange(open: boolean) {
    if (open) dockKeepOpen?.();
    else dockRelease?.();
}

const entryRuleModel = computed({
    get: () => props.entryRule || AUTO_ENTRY_VALUE,
    set: (value: string) => {
        emit("selectEntryRule", value === AUTO_ENTRY_VALUE ? "" : value);
    },
});

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
</script>

<template>
    <Tooltip>
        <TooltipTrigger as-child>
            <div>
                <Select v-model="entryRuleModel" @update:open="onSelectOpenChange">
                    <SelectTrigger class="h-9 min-w-36 max-w-full border-border/35 bg-background/35 px-2.5 shadow-none rounded-xl sm:h-10 sm:min-w-44">
                        <div class="flex items-center gap-2 overflow-hidden">
                            <GitBranch class="h-4 w-4 shrink-0 text-pastel-blue" />
                            <span class="truncate font-mono text-sm sm:text-base">
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
                                <p v-if="option.detail" class="mt-1 text-sm text-muted-foreground">{{ option.detail }}</p>
                            </div>
                        </SelectItem>
                    </SelectContent>
                </Select>
            </div>
        </TooltipTrigger>
        <TooltipContent side="top" :side-offset="8" class="max-w-xs">
            <p class="text-sm sm:text-base">
                <InlineRichText :text="entryRuleTooltip" />
            </p>
        </TooltipContent>
    </Tooltip>
</template>
