<script setup lang="ts">
import type { HTMLAttributes } from "vue";
import { cn } from "@/lib/utils";

const props = defineProps<{
    class?: HTMLAttributes["class"];
}>();
</script>

<template>
    <div
        :class="
            cn(
                'scrollbar-hidden rounded-xl border border-border/60 bg-card/80 text-card-foreground backdrop-blur-sm shadow-card',
                props.class,
            )
        "
    >
        <slot />
    </div>
</template>
