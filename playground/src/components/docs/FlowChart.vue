<script setup lang="ts">
import { ref, onMounted } from "vue";
import DocCard from "./DocCard.vue";

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

function onNodeClick(node: FlowNode) {
    if (!node.href) return;
    const id = node.href.replace(/^#/, "");
    const el = document.getElementById(id);
    if (el) el.scrollIntoView({ behavior: "smooth", block: "start" });
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
                    class="flow-node relative flex items-center gap-3 rounded-lg border border-border/30 bg-muted/10 backdrop-blur-sm px-4 py-2.5 w-full max-w-xl transition-all duration-200"
                    :class="[
                        node.href ? 'flow-node--clickable cursor-pointer' : 'cursor-default',
                        node.detail ? 'flow-node--has-detail' : '',
                    ]"
                    :style="{
                        opacity: mounted ? 1 : 0,
                        transform: mounted ? 'translateY(0)' : 'translateY(8px)',
                        transitionDelay: `${i * 100}ms`,
                    }"
                    :data-tooltip="node.detail || undefined"
                    @click="onNodeClick(node)"
                >
                    <!-- Left accent bar -->
                    <div
                        class="absolute left-0 top-2 bottom-2 w-1 rounded-full"
                        :style="{ background: getNodeColor(node, i) }"
                    />
                    <div class="pl-2 flex-1 flex items-center justify-between gap-2">
                        <span class="text-sm font-mono font-medium text-foreground">{{ node.label }}</span>
                        <span v-if="node.detail" class="text-[10px] font-mono text-muted-foreground truncate max-w-[180px]">
                            {{ node.detail }}
                        </span>
                    </div>

                    <!-- Tooltip -->
                    <div
                        v-if="node.detail"
                        class="flow-tooltip pointer-events-none absolute left-1/2 bottom-full mb-2 -translate-x-1/2 z-50 rounded-md border border-border/40 bg-popover/95 backdrop-blur-md shadow-lg px-3 py-2 text-xs font-mono text-popover-foreground max-w-[300px] w-max opacity-0 transition-opacity duration-150"
                    >
                        <div class="font-medium text-foreground mb-0.5">{{ node.label }}</div>
                        <div class="text-muted-foreground leading-relaxed">{{ node.detail }}</div>
                    </div>
                </div>
            </template>
        </div>
    </DocCard>
</template>

<style scoped>
/* Clickable node hover effects */
.flow-node--clickable:hover {
    filter: brightness(1.1);
    transform: scale(1.01) translateY(0) !important;
    border-color: hsl(var(--border) / 0.5);
    box-shadow: 0 0 0 1px hsl(var(--foreground) / 0.08);
}

.flow-node--clickable:active {
    transform: scale(0.995) translateY(0) !important;
}

/* Show tooltip on hover */
.flow-node--has-detail:hover .flow-tooltip {
    opacity: 1;
}
</style>
