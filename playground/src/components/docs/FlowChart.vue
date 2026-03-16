<script setup lang="ts">
import { ref, onMounted, computed } from "vue";
import DocCard from "./DocCard.vue";
import FloatingTooltip from "./FloatingTooltip.vue";

interface FlowNode {
    label: string;
    detail?: string;
    color?: string;
    href?: string;
}

interface FlowChartData {
    title?: string;
    nodes: FlowNode[];
}

const props = defineProps<{
    data: FlowChartData;
}>();

const mounted = ref(false);
const hoveredNode = ref<{ index: number; x: number; y: number } | null>(null);

onMounted(() => {
    requestAnimationFrame(() => {
        mounted.value = true;
    });
});

const colorVars: Record<string, string> = {
    green: "var(--color-pastel-green)",
    blue: "var(--color-pastel-blue)",
    purple: "var(--color-pastel-purple)",
    cyan: "var(--color-pastel-cyan)",
    amber: "var(--color-pastel-amber)",
};

const defaultColors = ["var(--color-pastel-cyan)", "var(--color-pastel-blue)", "var(--color-pastel-green)", "var(--color-pastel-amber)", "var(--color-pastel-purple)"];

function getNodeColor(node: FlowNode, index: number): string {
    if (node.color && colorVars[node.color]) return colorVars[node.color];
    if (node.color) return node.color;
    return defaultColors[index % defaultColors.length]!;
}

function onNodeEnter(event: MouseEvent, index: number) {
    const rect = (event.currentTarget as HTMLElement).getBoundingClientRect();
    hoveredNode.value = { index, x: rect.left + rect.width / 2, y: rect.top };
}

function onNodeLeave() {
    hoveredNode.value = null;
}

const hoveredDetail = computed(() => {
    if (!hoveredNode.value) return "";
    return props.data.nodes[hoveredNode.value.index]?.detail ?? "";
});

function onNodeClick(node: FlowNode) {
    if (!node.href) return;
    const id = node.href.replace(/^#/, '');
    const el = document.getElementById(id);
    if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' });
}
</script>

<template>
    <DocCard :title="data.title">
        <div class="flex flex-col items-center gap-0">
            <template v-for="(node, i) in data.nodes" :key="i">
                <!-- Arrow connector -->
                <div
                    v-if="i > 0"
                    class="flex flex-col items-center transition-opacity duration-300"
                    :style="{
                        opacity: mounted ? 1 : 0,
                        transitionDelay: `${i * 100 - 50}ms`,
                    }"
                >
                    <svg width="2" height="20" class="text-border/50">
                        <line x1="1" y1="0" x2="1" y2="20" stroke="currentColor" stroke-width="1.5" />
                    </svg>
                    <svg width="10" height="8" class="text-border/50 -mt-px">
                        <polygon points="5,8 0,0 10,0" fill="currentColor" />
                    </svg>
                </div>

                <!-- Node pill -->
                <div
                    class="relative flex items-center gap-3 rounded-lg border border-border/30 bg-muted/10 backdrop-blur-sm px-4 py-2.5 w-full max-w-xl transition-all duration-300"
                    :class="node.href ? 'cursor-pointer hover:ring-1 hover:ring-foreground/20' : 'cursor-default'"
                    :style="{
                        opacity: mounted ? 1 : 0,
                        transform: mounted ? 'translateY(0)' : 'translateY(8px)',
                        transitionDelay: `${i * 100}ms`,
                    }"
                    @mouseenter="onNodeEnter($event, i)"
                    @mouseleave="onNodeLeave"
                    @click="onNodeClick(node)"
                >
                    <!-- Left accent bar -->
                    <div
                        class="absolute left-0 top-2 bottom-2 w-1 rounded-full"
                        :style="{ background: getNodeColor(node, i) }"
                    />
                    <div class="pl-2 flex-1 flex items-center justify-between gap-2">
                        <span class="text-sm font-mono font-medium text-foreground">{{ node.label }}</span>
                        <span v-if="node.detail" class="text-[10px] font-mono text-muted-foreground">
                            {{ node.detail }}
                        </span>
                    </div>
                </div>
            </template>
        </div>

        <FloatingTooltip
            :visible="!!hoveredNode && !!hoveredDetail"
            :x="hoveredNode?.x ?? 0"
            :y="hoveredNode?.y ?? 0"
        >
            <div class="text-foreground">{{ data.nodes[hoveredNode?.index ?? 0]?.label }}</div>
            <div class="text-muted-foreground">{{ hoveredDetail }}</div>
        </FloatingTooltip>
    </DocCard>
</template>
