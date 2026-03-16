<script setup lang="ts">
defineProps<{
    title?: string;
    class?: string;
}>();
</script>

<template>
    <div class="rounded-xl border border-border/30 bg-card/60 backdrop-blur-sm p-4 sm:p-5 my-4" :class="$props.class">
        <div v-if="title || $slots.header" class="mb-3">
            <h4 v-if="title" class="text-sm font-semibold" :class="{ 'mb-2': $slots.header }">{{ title }}</h4>
            <slot name="header" />
        </div>
        <slot />
    </div>
</template>
