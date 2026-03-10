<script setup lang="ts">
import { BookOpen, ShieldAlert, Palette, FileCode } from "lucide-vue-next";

const demos = [
    {
        id: "json-parser",
        icon: BookOpen,
        title: "Build a JSON parser",
        description: "Start from scratch: define value, string, number, array, and object rules step by step.",
        iconClass: "text-pastel-green",
        borderClass: "border-pastel-green/40",
    },
    {
        id: "bbnf-grammar",
        icon: FileCode,
        title: "Write BBNF in BBNF",
        description: "Explore the self-hosting grammar — BBNF defined in its own notation.",
        iconClass: "text-pastel-amber",
        borderClass: "border-pastel-amber/40",
    },
    {
        id: "error-recovery",
        icon: ShieldAlert,
        title: "Add error recovery",
        description: "Use @recover directives to parse past errors and collect multiple diagnostics.",
        iconClass: "text-pastel-blue",
        borderClass: "border-pastel-blue/40",
    },
    {
        id: "pretty-printing",
        icon: Palette,
        title: "Format with @pretty",
        description: "Annotate rules with group, indent, and sep to build a grammar-driven formatter.",
        iconClass: "text-pastel-pink",
        borderClass: "border-pastel-pink/40",
    },
];
</script>

<template>
    <section class="max-w-5xl mx-auto px-4 sm:px-8 py-10 sm:py-16">
        <h2 class="instrument-serif text-2xl sm:text-3xl text-center mb-6 sm:mb-8">Guided walkthroughs</h2>

        <div class="grid grid-cols-2 lg:grid-cols-4 gap-3 sm:gap-6">
            <router-link
                v-for="demo in demos"
                :key="demo.id"
                :to="{ path: '/playground', query: { demo: demo.id } }"
                class="block"
            >
                <div
                    class="rounded-xl border bg-card/80 backdrop-blur-sm p-3 sm:p-5 h-full transition-all duration-300
                           hover:-translate-y-0.5
                           shadow-[3px_3px_0px_0px_rgba(0,0,0,0.08)] dark:shadow-[3px_3px_0px_0px_rgba(200,200,255,0.06)]
                           hover:shadow-[5px_7px_0_rgba(0,0,0,0.12)] dark:hover:shadow-[5px_7px_0_rgba(200,200,255,0.08)]"
                    :class="demo.borderClass"
                >
                    <component :is="demo.icon" class="h-5 w-5 sm:h-6 sm:w-6 mb-2 sm:mb-3" :class="demo.iconClass" />
                    <h3 class="instrument-serif text-base sm:text-lg mb-1 sm:mb-2">{{ demo.title }}</h3>
                    <p class="text-[11px] sm:text-xs text-muted-foreground leading-relaxed">{{ demo.description }}</p>
                </div>
            </router-link>
        </div>
    </section>
</template>
