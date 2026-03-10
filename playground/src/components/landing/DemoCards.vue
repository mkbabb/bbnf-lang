<script setup lang="ts">
import { BookOpen, ShieldAlert, Palette, FileCode, ArrowRight } from "lucide-vue-next";

const demos = [
    {
        id: "json-parser",
        icon: BookOpen,
        title: "Build a JSON parser",
        description: "Start from scratch: define value, string, number, array, and object rules step by step.",
        iconClass: "text-pastel-green",
        borderClass: "border-pastel-green/40",
        shimmerClass: "shimmer-green",
    },
    {
        id: "bbnf-grammar",
        icon: FileCode,
        title: "Write BBNF in BBNF",
        description: "Explore the self-hosting grammar — BBNF defined in its own notation.",
        iconClass: "text-pastel-amber",
        borderClass: "border-pastel-amber/40",
        shimmerClass: "shimmer-amber",
    },
    {
        id: "error-recovery",
        icon: ShieldAlert,
        title: "Add error recovery",
        description: "Use @recover directives to parse past errors and collect multiple diagnostics.",
        iconClass: "text-pastel-blue",
        borderClass: "border-pastel-blue/40",
        shimmerClass: "shimmer-blue",
    },
    {
        id: "pretty-printing",
        icon: Palette,
        title: "Format with @pretty",
        description: "Annotate rules with group, indent, and sep to build a grammar-driven formatter.",
        iconClass: "text-pastel-pink",
        borderClass: "border-pastel-pink/40",
        shimmerClass: "shimmer-pink",
    },
];
</script>

<template>
    <section class="max-w-5xl mx-auto px-4 sm:px-8 py-10 sm:py-16">
        <h2 class="instrument-serif text-2xl sm:text-3xl text-center mb-2 sm:mb-3">Guided walkthroughs</h2>
        <p class="text-sm text-muted-foreground text-center mb-6 sm:mb-8">Step-by-step demos in the playground — click to begin.</p>

        <div class="grid grid-cols-2 lg:grid-cols-4 gap-3 sm:gap-6">
            <router-link
                v-for="demo in demos"
                :key="demo.id"
                :to="{ path: '/playground', query: { demo: demo.id } }"
                class="group block"
            >
                <div
                    class="demo-card relative overflow-hidden rounded-xl border bg-card/80 backdrop-blur-sm p-3 sm:p-5 h-full transition-all duration-300
                           hover:-translate-y-1 cursor-pointer
                           shadow-[3px_3px_0px_0px_rgba(0,0,0,0.08)] dark:shadow-[3px_3px_0px_0px_rgba(200,200,255,0.06)]
                           hover:shadow-[5px_7px_0_rgba(0,0,0,0.12)] dark:hover:shadow-[5px_7px_0_rgba(200,200,255,0.08)]"
                    :class="[demo.borderClass, demo.shimmerClass]"
                >
                    <component :is="demo.icon" class="h-5 w-5 sm:h-6 sm:w-6 mb-2 sm:mb-3" :class="demo.iconClass" />
                    <h3 class="instrument-serif text-base sm:text-lg mb-1 sm:mb-2">{{ demo.title }}</h3>
                    <p class="text-[11px] sm:text-xs text-muted-foreground leading-relaxed mb-3">{{ demo.description }}</p>
                    <span class="flex items-center gap-1 text-[11px] sm:text-xs font-mono text-muted-foreground/60 group-hover:text-foreground/80 transition-colors">
                        Start
                        <ArrowRight class="h-3 w-3 transition-transform group-hover:translate-x-0.5" />
                    </span>
                </div>
            </router-link>
        </div>
    </section>
</template>

<style scoped>
/* Shimmer sweep — a gentle highlight that glides across the card surface */
.demo-card::before {
    content: "";
    position: absolute;
    inset: 0;
    pointer-events: none;
}

/* Per-color tint — subtle opacity, wide gradient band, slow sweep */
.shimmer-green::before {
    background: linear-gradient(105deg, transparent 35%, hsl(152 50% 42% / 0.04) 43%, hsl(152 50% 42% / 0.08) 50%, hsl(152 50% 42% / 0.04) 57%, transparent 65%);
    background-size: 300% 100%;
    animation: sweep 12s ease-in-out infinite;
}
.shimmer-amber::before {
    background: linear-gradient(105deg, transparent 35%, hsl(30 70% 50% / 0.04) 43%, hsl(30 70% 50% / 0.08) 50%, hsl(30 70% 50% / 0.04) 57%, transparent 65%);
    background-size: 300% 100%;
    animation: sweep 12s 3s ease-in-out infinite;
}
.shimmer-blue::before {
    background: linear-gradient(105deg, transparent 35%, hsl(215 55% 50% / 0.04) 43%, hsl(215 55% 50% / 0.08) 50%, hsl(215 55% 50% / 0.04) 57%, transparent 65%);
    background-size: 300% 100%;
    animation: sweep 12s 6s ease-in-out infinite;
}
.shimmer-pink::before {
    background: linear-gradient(105deg, transparent 35%, hsl(330 65% 55% / 0.04) 43%, hsl(330 65% 55% / 0.08) 50%, hsl(330 65% 55% / 0.04) 57%, transparent 65%);
    background-size: 300% 100%;
    animation: sweep 12s 9s ease-in-out infinite;
}

@keyframes sweep {
    0%, 100% { background-position: 250% 0; }
    50% { background-position: -50% 0; }
}
</style>
