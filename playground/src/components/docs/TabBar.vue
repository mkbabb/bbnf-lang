<script setup lang="ts">
interface Tab {
    key: string;
    label: string;
    icon?: { svg: string; color: string };
}

defineProps<{
    tabs: Tab[];
    modelValue: string;
}>();

defineEmits<{
    "update:modelValue": [key: string];
}>();
</script>

<template>
    <div class="flex items-center gap-0 border-b border-border/20 overflow-x-auto scrollbar-hidden">
        <button
            v-for="tab in tabs"
            :key="tab.key"
            class="flex items-center gap-1.5 px-3 py-1.5 text-sm font-mono transition-colors relative whitespace-nowrap shrink-0"
            :class="tab.key === modelValue
                ? 'text-foreground'
                : 'text-muted-foreground/60 hover:text-muted-foreground'"
            @click="$emit('update:modelValue', tab.key)"
        >
            <span
                v-if="tab.icon"
                class="inline-flex h-4 w-4 shrink-0 items-center justify-center [&>svg]:w-full [&>svg]:h-full"
                :style="{ color: tab.icon.color }"
                v-html="tab.icon.svg"
            />
            {{ tab.label }}
            <span
                v-if="tab.key === modelValue"
                class="absolute bottom-0 left-1 right-1 h-0.5 rounded-full"
                :style="{ background: tab.icon?.color ?? 'currentColor' }"
            />
        </button>
    </div>
</template>
