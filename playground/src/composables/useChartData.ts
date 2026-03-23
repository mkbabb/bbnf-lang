import { computed, ref, watch, type Ref } from "vue";
import { getLanguageIcon } from "@/lib/languageIcons";

export interface Series {
    label: string;
    values: number[];
    color?: string;
}

export interface Dataset {
    name: string;
    icon?: string;
    labels?: string[];
    series: Series[];
}

export interface ChartData {
    title: string;
    unit: string;
    labels?: string[];
    series: Series[];
    datasets?: Dataset[];
    /** Names that should always be highlighted (gold accent) */
    highlight?: string[];
    /** When true, lower values are better (latency charts) */
    lowerIsBetter?: boolean;
    /** Optional per-label description overrides (merged with PARSER_DESCRIPTIONS) */
    descriptions?: Record<string, string>;
}

// Gold color for "our" implementations, standard palette for others
const GOLD = "hsl(45 90% 55%)";

const colors = [
    "var(--color-pastel-cyan)",
    "var(--color-pastel-blue)",
    "var(--color-pastel-green)",
    "var(--color-pastel-amber)",
    "var(--color-pastel-purple)",
    "hsl(350 70% 65%)",
    "hsl(200 70% 65%)",
    "hsl(160 60% 55%)",
    "hsl(30 80% 60%)",
    "hsl(270 60% 65%)",
];

// "Our" implementations — always get gold accent
const OUR_NAMES = new Set([
    "bbnf aot", "bbnf-generated", "parse-that", "parse_that",
    "gorgeous", "gorgeous (cached)", "aot",
    "bbnf span", "bbnf borrow", "bbnf owned",
    "bbnf fast", "bbnf pretty",
    "bbnf aot (borrow)",
]);

export function useChartData(data: ChartData, mounted: Ref<boolean>) {
    // Dataset tabs
    const activeDataset = ref(data.datasets?.[0]?.name ?? "");

    watch(activeDataset, () => {
        mounted.value = false;
        requestAnimationFrame(() => {
            requestAnimationFrame(() => {
                mounted.value = true;
            });
        });
    });

    const datasetTabs = computed(() =>
        (data.datasets ?? []).map((d) => ({
            key: d.name,
            label: d.name,
            icon: d.icon ? getLanguageIcon(d.icon) : undefined,
        }))
    );

    const activeSeries = computed<Series[]>(() => {
        if (data.datasets?.length) {
            const ds = data.datasets.find((d) => d.name === activeDataset.value);
            return ds?.series ?? data.datasets[0]?.series ?? [];
        }
        return data.series;
    });

    const activeLabels = computed<string[] | undefined>(() => {
        if (data.datasets?.length) {
            const ds = data.datasets.find((d) => d.name === activeDataset.value);
            return ds?.labels ?? data.datasets[0]?.labels;
        }
        return data.labels;
    });

    // --- Highlight logic ---

    const highlightSet = computed(() => {
        const s = new Set(OUR_NAMES);
        if (data.highlight) {
            for (const h of data.highlight) s.add(h.toLowerCase());
        }
        return s;
    });

    function isOurs(label: string): boolean {
        return highlightSet.value.has(label.toLowerCase());
    }

    // Latency-aware: "ns" and "ms" units mean lower is better
    const lowerIsBetter = computed(() => {
        if (data.lowerIsBetter !== undefined) return data.lowerIsBetter;
        const u = data.unit.toLowerCase();
        return u === "ns" || u === "ms" || u === "µs";
    });

    const maxValue = computed(() => {
        let max = 0;
        for (const s of activeSeries.value) {
            for (const v of s.values) {
                if (v > max) max = v;
            }
        }
        return max || 1;
    });

    // Winner = best value (max for throughput, min for latency)
    const winnerIndices = computed(() => {
        return activeSeries.value.map((s) => {
            let bestIdx = 0;
            let bestVal = lowerIsBetter.value ? Infinity : -Infinity;
            for (let i = 0; i < s.values.length; i++) {
                const v = s.values[i]!;
                if (v === 0) continue; // skip zero (missing data)
                if (lowerIsBetter.value ? v < bestVal : v > bestVal) {
                    bestVal = v;
                    bestIdx = i;
                }
            }
            return bestIdx;
        });
    });

    function isWinner(si: number, vi: number): boolean {
        return winnerIndices.value[si] === vi;
    }

    function formatValue(val: number): { value: string; unit: string } {
        const baseUnit = data.unit;
        // Auto-scale throughput units
        if (baseUnit === "MB/s" && val >= 1000) {
            return { value: (val / 1000).toFixed(2), unit: "GB/s" };
        }
        if (baseUnit === "ns" && val >= 1_000_000) {
            return { value: (val / 1_000_000).toFixed(2), unit: "ms" };
        }
        if (baseUnit === "ns" && val >= 1000) {
            return { value: (val / 1000).toFixed(1), unit: "µs" };
        }
        // Default: thousands separator
        const formatted = val >= 1000 ? val.toLocaleString() : String(val);
        return { value: formatted, unit: baseUnit };
    }

    // Track which non-highlighted color index we're at per render
    let _colorIdx = 0;
    const barColorCache = new Map<string, string>();

    const isSingleSeries = computed(() => activeSeries.value.length === 1);

    function getBarColor(si: number, vi: number, custom?: string): string {
        if (custom) return custom;
        const label = activeLabels.value?.[vi] ?? "";

        // "Our" implementations always get gold
        if (isSingleSeries.value && isOurs(label)) return GOLD;

        // Multi-series: color by series index
        if (!isSingleSeries.value) return colors[si % colors.length]!;

        // Single series: stable per-label color from palette
        if (barColorCache.has(label)) return barColorCache.get(label)!;
        const c = colors[_colorIdx % colors.length]!;
        _colorIdx++;
        barColorCache.set(label, c);
        return c;
    }

    // Reset color cache when dataset changes
    watch([activeSeries, activeLabels], () => {
        _colorIdx = 0;
        barColorCache.clear();
    }, { immediate: true });

    function getColor(index: number, custom?: string): string {
        return custom ?? colors[index % colors.length]!;
    }

    function barDelay(si: number, vi: number): string {
        const idx = isSingleSeries.value ? vi : si * (activeLabels.value?.length ?? 1) + vi;
        return `${idx * 40}ms`;
    }

    return {
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
    };
}
