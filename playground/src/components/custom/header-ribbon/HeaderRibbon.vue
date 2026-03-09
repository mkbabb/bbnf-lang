<template>
    <div
        :class="[
            'fixed z-40 pointer-events-none w-fit flex items-center px-4 pt-4 pb-2',
            position === 'left' ? 'top-0 left-0' : 'top-0 right-0',
        ]"
        @mouseleave="onGroupMouseLeave"
    >
        <!-- Left extra slot (e.g. ppmycota logo in keyframes.js) -->
        <div v-if="$slots.left" class="pointer-events-auto shrink-0 mr-2">
            <slot name="left"></slot>
        </div>

        <div
            class="pointer-events-auto flex items-center h-6"
            @mouseenter="onRibbonMouseEnter"
        >
            <!-- When position=left: anchor first, then items expand right -->
            <!-- When position=right: items first (expand left), then anchor -->
            <template v-if="position === 'right'">
                <div
                    :class="[
                        'header-items-wrapper header-items-right flex items-center gap-3',
                        isVisible ? '' : 'header-collapsed-right',
                    ]"
                >
                    <slot name="items"></slot>
                </div>
            </template>

            <div class="shrink-0" @click="onAnchorClick">
                <slot name="anchor" :pinned="isPinned" :toggled="isToggled"></slot>
            </div>

            <template v-if="position === 'left'">
                <div
                    :class="[
                        'header-items-wrapper header-items-left flex items-center gap-3',
                        isVisible ? '' : 'header-collapsed-left',
                    ]"
                >
                    <slot name="items"></slot>
                </div>
            </template>
        </div>
    </div>
</template>

<script setup lang="ts">
import { ref, computed, onBeforeUnmount } from "vue";

const props = withDefaults(
    defineProps<{
        position?: "left" | "right";
        hideTimeoutMs?: number;
    }>(),
    {
        position: "left",
        hideTimeoutMs: 2000,
    },
);

const isExpanded = ref(false);
const isPinned = ref(false);
const isToggled = ref(false);

let hoverTimeout: ReturnType<typeof setTimeout> | undefined;

const isVisible = computed(() => isExpanded.value || isPinned.value);

function clearHoverTimeout() {
    if (hoverTimeout != null) {
        clearTimeout(hoverTimeout);
        hoverTimeout = undefined;
    }
}

function startHideTimeout() {
    clearHoverTimeout();
    hoverTimeout = setTimeout(() => {
        isExpanded.value = false;
    }, props.hideTimeoutMs);
}

function onRibbonMouseEnter() {
    clearHoverTimeout();
    isExpanded.value = true;
}

function onGroupMouseLeave() {
    if (!isPinned.value) {
        startHideTimeout();
    }
}

function onAnchorClick() {
    if (isPinned.value) {
        isPinned.value = false;
        isToggled.value = false;
        startHideTimeout();
    } else {
        isPinned.value = true;
        isExpanded.value = true;
        isToggled.value = true;
        clearHoverTimeout();
    }
}

onBeforeUnmount(() => {
    clearHoverTimeout();
});

defineExpose({ isPinned, isExpanded, isVisible, isToggled });
</script>

<style scoped>
/* Shared base */
.header-items-wrapper {
    max-width: 500px;
    opacity: 1;
    overflow: visible;
}

/* Left-aligned: items expand to the right of anchor */
.header-items-left {
    margin-left: 0.75rem;
    transition:
        max-width 0.35s cubic-bezier(0.4, 0, 0.2, 1),
        margin-left 0.35s cubic-bezier(0.4, 0, 0.2, 1),
        opacity 0.25s ease-out;
}
.header-collapsed-left {
    max-width: 0;
    margin-left: 0;
    opacity: 0;
    pointer-events: none;
    overflow: hidden;
}

/* Right-aligned: items expand to the left of anchor */
.header-items-right {
    margin-right: 0.75rem;
    transition:
        max-width 0.35s cubic-bezier(0.4, 0, 0.2, 1),
        margin-right 0.35s cubic-bezier(0.4, 0, 0.2, 1),
        opacity 0.25s ease-out;
}
.header-collapsed-right {
    max-width: 0;
    margin-right: 0;
    opacity: 0;
    pointer-events: none;
    overflow: hidden;
}
</style>
