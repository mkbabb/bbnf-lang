<script setup lang="ts">
import { ref, onMounted } from "vue";
import DocCard from "./DocCard.vue";
import TabBar from "./TabBar.vue";
import FloatingTooltip from "./FloatingTooltip.vue";
import { PARSER_DESCRIPTIONS } from "@/lib/parserDescriptions";
import { useChartData, type ChartData } from "@/composables/useChartData";

const props = defineProps<{
    data: ChartData;
}>();

const mounted = ref(false);

onMounted(() => {
    requestAnimationFrame(() => {
        requestAnimationFrame(() => {
            mounted.value = true;
        });
    });
});

const {
    activeDataset,
    datasetTabs,
    activeSeries,
    activeLabels,
    isOurs,
    lowerIsBetter,
    maxValue,
    isWinner,
    formatValue,
    isSingleSeries,
    getBarColor,
    getColor,
    barDelay,
} = useChartData(props.data, mounted);

// Hover state
const hoveredBar = ref<{ si: number; vi: number; x: number; y: number } | null>(null);

function onBarEnter(event: MouseEvent, si: number, vi: number) {
    const rect = (event.currentTarget as HTMLElement).getBoundingClientRect();
    hoveredBar.value = { si, vi, x: rect.left + rect.width / 2, y: rect.top };
}

function onBarLeave() {
    hoveredBar.value = null;
}

function hoveredLabel(): string {
    if (!hoveredBar.value) return "";
    const { vi } = hoveredBar.value;
    return activeLabels.value?.[vi] ?? `#${vi + 1}`;
}

function hoveredValue(): number {
    if (!hoveredBar.value) return 0;
    const { si, vi } = hoveredBar.value;
    return activeSeries.value[si]?.values[vi] ?? 0;
}

function hoveredPercent(): string {
    if (!hoveredBar.value) return "0";
    return ((hoveredValue() / maxValue.value) * 100).toFixed(1);
}

function hoveredDescription(): string {
    const label = hoveredLabel();
    return props.data.descriptions?.[label] ?? PARSER_DESCRIPTIONS[label] ?? "";
}

function hoveredIsOurs(): boolean {
    return isOurs(hoveredLabel());
}
</script>

<template>
    <DocCard :title="data.title" class="relative">
        <template #header>
            <TabBar
                v-if="data.datasets?.length"
                :tabs="datasetTabs"
                v-model="activeDataset"
            />
        </template>

        <div class="space-y-1">
            <div v-for="(series, si) in activeSeries" :key="series.label">
                <!-- Series header for multi-series -->
                <div v-if="!isSingleSeries" class="flex items-center gap-2 mb-2 mt-3 first:mt-0">
                    <span
                        class="w-2.5 h-2.5 rounded-full shrink-0"
                        :style="{ background: getColor(si, series.color) }"
                    />
                    <span class="text-xs font-semibold font-mono text-muted-foreground">{{ series.label }}</span>
                </div>

                <!-- Bars: full-width vertical stack -->
                <div class="flex flex-col gap-1.5">
                    <div
                        v-for="(val, vi) in series.values"
                        :key="vi"
                        class="group flex items-center gap-3 transition-opacity duration-300"
                        :style="{
                            opacity: mounted ? 1 : 0,
                            transitionDelay: barDelay(si, vi),
                        }"
                        @mouseenter="onBarEnter($event, si, vi)"
                        @mouseleave="onBarLeave"
                    >
                        <!-- Label -->
                        <span
                            v-if="activeLabels?.[vi]"
                            class="flex items-center justify-end gap-1.5 text-xs font-mono shrink-0 text-right whitespace-nowrap overflow-hidden text-ellipsis w-[9.5rem]"
                            :class="[
                                isOurs(activeLabels[vi]!) ? 'font-bold' : '',
                                isWinner(si, vi) ? 'text-foreground font-bold' : isOurs(activeLabels[vi]!) ? 'text-foreground' : 'text-muted-foreground',
                            ]"
                        >
                            <span
                                v-if="isSingleSeries"
                                class="w-2 h-2 rounded-full shrink-0"
                                :style="{ background: getBarColor(si, vi, series.color) }"
                            />
                            {{ activeLabels[vi] }}
                        </span>

                        <!-- Bar track -->
                        <div class="flex-1 h-7 bg-muted/15 rounded-md overflow-hidden relative">
                            <!-- Bar fill -->
                            <div
                                class="h-full rounded-md transition-all duration-700 ease-out"
                                :style="{
                                    width: mounted ? `${Math.max((val / maxValue) * 100, 0.5)}%` : '0%',
                                    background: getBarColor(si, vi, series.color),
                                    opacity: isOurs(activeLabels?.[vi] ?? '') ? 0.95 : isWinner(si, vi) ? 0.85 : 0.55,
                                    transitionDelay: barDelay(si, vi),
                                }"
                            />
                            <!-- Value label -->
                            <span
                                class="absolute right-2 top-1/2 -translate-y-1/2 text-xs font-mono tabular-nums"
                                :class="isWinner(si, vi) || isOurs(activeLabels?.[vi] ?? '') ? 'text-foreground font-semibold' : 'text-foreground/70'"
                            >
                                {{ formatValue(val) }}
                                <span class="text-muted-foreground text-[0.6875rem] ml-0.5">{{ data.unit }}</span>
                            </span>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <FloatingTooltip
            :visible="!!hoveredBar"
            :x="hoveredBar?.x ?? 0"
            :y="hoveredBar?.y ?? 0"
        >
            <div class="flex items-center gap-1.5">
                <span class="text-foreground font-semibold">{{ hoveredLabel() }}</span>
                <span
                    v-if="hoveredIsOurs()"
                    class="text-[0.5625rem] font-bold uppercase tracking-wider px-1.5 py-0.5 rounded-full"
                    style="background: hsl(45 90% 55% / 0.2); color: hsl(45 90% 55%)"
                >BBNF</span>
            </div>
            <div class="text-muted-foreground">{{ formatValue(hoveredValue()) }} {{ data.unit }}</div>
            <div class="text-muted-foreground/70">{{ hoveredPercent() }}% of max</div>
            <div v-if="hoveredDescription()" class="text-muted-foreground/60 text-[0.6875rem] mt-0.5 max-w-[220px]">{{ hoveredDescription() }}</div>
        </FloatingTooltip>
    </DocCard>
</template>
