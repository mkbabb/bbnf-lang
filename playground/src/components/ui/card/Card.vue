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
                'scrollbar-hidden rounded-xl border border-border/60 bg-card/80 text-card-foreground backdrop-blur-sm shadow-[3px_3px_0px_0px_rgba(0,0,0,0.08)] dark:shadow-[3px_3px_0px_0px_rgba(200,200,255,0.06)]',
                props.class,
            )
        "
    >
        <slot />
    </div>
</template>
