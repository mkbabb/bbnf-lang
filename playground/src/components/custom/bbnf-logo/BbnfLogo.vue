<script setup lang="ts">
withDefaults(defineProps<{
    size?: "xs" | "sm" | "md" | "lg" | "xl" | "2xl";
    brand?: boolean;
    shimmer?: boolean;
}>(), {
    size: "md",
    brand: false,
    shimmer: false,
});

const sizeClasses = {
    xs: { icon: "h-4 w-4", text: "text-lg" },
    sm: { icon: "h-5 w-5", text: "text-xl" },
    md: { icon: "h-7 w-7", text: "text-3xl" },
    lg: { icon: "h-10 w-10", text: "text-5xl" },
    xl: { icon: "h-14 w-14", text: "text-7xl" },
    "2xl": { icon: "h-20 w-20", text: "text-9xl" },
};
</script>

<template>
    <span class="inline-flex items-center gap-0.5 select-none">
        <img src="/bbnf-icon.png" alt="" :class="[sizeClasses[size].icon, 'object-contain -mr-0.5']" />
        <span
            :class="[
                'instrument-serif tracking-tight',
                sizeClasses[size].text,
                shimmer ? 'bbnf-shimmer' : brand ? 'text-pastel-green' : 'text-foreground',
            ]"
        >BNF</span>
    </span>
</template>

<style scoped>
.bbnf-shimmer {
    background: linear-gradient(90deg, #b8860b, #ffd700, #daa520, #ffd700, #b8860b);
    background-size: 200% 100%;
    background-clip: text;
    -webkit-background-clip: text;
    color: transparent;
    animation: shimmer 3s linear infinite;
}
@keyframes shimmer {
    0% { background-position: 200% 0; }
    100% { background-position: -200% 0; }
}
</style>
