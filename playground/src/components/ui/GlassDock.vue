<script setup lang="ts">
import { ref, onMounted, onUnmounted, useTemplateRef, provide } from "vue";

const props = withDefaults(
    defineProps<{
        collapseDelay?: number;
        startCollapsed?: boolean;
        fitContent?: boolean;
    }>(),
    {
        collapseDelay: 2000,
        startCollapsed: true,
        fitContent: false,
    },
);

const expanded = ref(!props.startCollapsed);
const mounted = ref(false);
let collapseTimer: ReturnType<typeof setTimeout> | null = null;
let ignoreEvents = true;

const dockEl = useTemplateRef<HTMLElement>("dockEl");

// Child components (e.g. Select, Dialog) can hold the dock open
let keepOpenCount = 0;

provide("dockKeepOpen", () => {
    keepOpenCount++;
    clearTimer();
});

provide("dockRelease", () => {
    keepOpenCount = Math.max(0, keepOpenCount - 1);
    if (keepOpenCount === 0) scheduleCollapse();
});

onMounted(() => {
    // Suppress transitions on initial render
    requestAnimationFrame(() => {
        mounted.value = true;
    });
    setTimeout(() => {
        ignoreEvents = false;
    }, 600);
});

function clearTimer() {
    if (collapseTimer) {
        clearTimeout(collapseTimer);
        collapseTimer = null;
    }
}

function scheduleCollapse() {
    if (keepOpenCount > 0) return;
    clearTimer();
    collapseTimer = setTimeout(() => {
        expanded.value = false;
    }, props.collapseDelay);
}

function onEnter() {
    if (ignoreEvents) return;
    clearTimer();
    expanded.value = true;
}

function onLeave() {
    scheduleCollapse();
}

function onFocusOut(e: FocusEvent) {
    const root = e.currentTarget as HTMLElement;
    if (e.relatedTarget && root.contains(e.relatedTarget as Node)) return;
    scheduleCollapse();
}

function onClickSummary() {
    clearTimer();
    expanded.value = true;
    scheduleCollapse();
}

defineExpose({ expanded, expand: onEnter, collapse: () => { expanded.value = false; } });
onUnmounted(clearTimer);
</script>

<template>
    <div
        ref="dockEl"
        class="glass-dock"
        :class="{ expanded, collapsed: !expanded, 'fit-content': fitContent, 'no-transition': !mounted }"
        @mouseenter="onEnter"
        @mouseleave="onLeave"
        @focusin="onEnter"
        @focusout="onFocusOut"
    >
        <div class="dock-layers">
            <div :class="['dock-layer dock-layer--full', { 'layer-active': expanded }]">
                <slot />
            </div>
            <div :class="['dock-layer dock-layer--summary', { 'layer-active': !expanded }]" @click="onClickSummary">
                <slot name="collapsed" />
            </div>
        </div>
    </div>
</template>

<style scoped>
.glass-dock {
    display: inline-flex;
    align-items: center;
    padding: 0.375rem 0.75rem;
    border-radius: var(--radius-pill);
    background: var(--glass-bg);
    backdrop-filter: var(--glass-blur);
    -webkit-backdrop-filter: var(--glass-blur);
    border: 1px solid var(--glass-border);
    box-shadow: var(--glass-shadow);
    overflow: hidden;
    white-space: nowrap;
    transition:
        max-width 0.4s var(--ease-smooth),
        padding 0.35s var(--ease-smooth),
        box-shadow var(--duration-normal) var(--ease-standard),
        background var(--duration-normal) var(--ease-standard),
        border-color var(--duration-normal) var(--ease-standard);
}

/* Suppress transitions on initial render */
.glass-dock.no-transition {
    transition: none !important;
}

/* Expanded: allow full intrinsic width */
.glass-dock.expanded {
    max-width: 100vw;
}

/* ── Collapsed: compact pill ── */
.glass-dock.collapsed {
    cursor: pointer;
    max-width: 14rem;
    padding: 0.375rem 0.75rem;
    background: hsl(var(--card) / 0.92);
    border-color: hsl(var(--border) / 0.7);
    box-shadow:
        0 2px 8px hsl(var(--foreground) / 0.06),
        0 0 0 1px hsl(var(--foreground) / 0.06);
}

.glass-dock.collapsed:hover {
    background: hsl(var(--card) / 0.96);
    border-color: hsl(var(--border));
    box-shadow:
        0 4px 20px hsl(var(--foreground) / 0.18),
        0 0 0 1px hsl(var(--foreground) / 0.1);
}

/* ── Layer stacking via grid ── */
.dock-layers {
    display: grid;
}

.dock-layer {
    grid-area: 1 / 1;
    display: flex;
    align-items: center;
    gap: 0.5rem;
    min-height: 2rem;
    white-space: nowrap;
    transition: opacity var(--duration-normal) var(--ease-standard);
}

.dock-layer.layer-active {
    opacity: 1;
    pointer-events: auto;
}

.dock-layer:not(.layer-active) {
    opacity: 0;
    pointer-events: none;
    position: absolute;
    visibility: hidden;
}

</style>
