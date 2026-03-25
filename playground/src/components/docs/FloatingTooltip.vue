<script setup lang="ts">
defineProps<{
    visible: boolean;
    x: number;
    y: number;
}>();
</script>

<template>
    <Teleport to="body">
        <div
            v-if="visible"
            class="fixed z-[var(--z-hovercard)] pointer-events-none rounded-lg border border-border/40 bg-popover/95 backdrop-blur-md shadow-lg px-3 py-2 text-xs font-mono space-y-0.5"
            :style="{
                left: `${x}px`,
                top: `${y - 8}px`,
                transform: 'translate(-50%, -100%)',
            }"
        >
            <slot />
        </div>
    </Teleport>
</template>
