<script setup lang="ts">
import { Blocks, ShieldCheck, Paintbrush, Zap } from "lucide-vue-next";

const features = [
    {
        icon: Blocks,
        title: "Parser Combinators",
        color: "pastel-green",
        parts: [
            { text: "Compose parsers from simple building blocks. String literals, regex, alternation, repetition — all in a clean grammar syntax." },
        ],
    },
    {
        icon: ShieldCheck,
        title: "Error Recovery",
        color: "pastel-blue",
        parts: [
            { text: "Parse past errors with " },
            { text: "@recover", code: true, color: "pastel-blue" },
            { text: " directives. Collect multiple diagnostics in a single pass, just like a real compiler." },
        ],
    },
    {
        icon: Paintbrush,
        title: "Pretty-Printing",
        color: "pastel-pink",
        parts: [
            { text: "Annotate rules with " },
            { text: "@pretty", code: true, color: "pastel-pink" },
            { text: " directives to generate a formatter. " },
            { text: "group", code: true },
            { text: ", " },
            { text: "indent", code: true },
            { text: ", and line-breaking — all grammar-driven." },
        ],
    },
    {
        icon: Zap,
        title: "WASM Performance",
        color: "pastel-amber",
        parts: [
            { text: "Built-in languages run through " },
            { text: "gorgeous", code: true, color: "pastel-amber" },
            { text: " (Rust → WASM) for native-speed formatting. Custom grammars use the TS interpreter." },
        ],
    },
];
</script>

<template>
    <section class="max-w-6xl mx-auto px-4 sm:px-8 py-10 sm:py-16">
        <div class="grid grid-cols-2 lg:grid-cols-4 gap-3 sm:gap-6">
            <div
                v-for="feature in features"
                :key="feature.title"
                class="rounded-xl bg-card/80 backdrop-blur-sm p-3 sm:p-5 transition-all duration-300
                       hover:-translate-y-0.5 cursor-default
                       shadow-[3px_3px_0px_0px_rgba(0,0,0,0.08)] dark:shadow-[3px_3px_0px_0px_rgba(200,200,255,0.06)]
                       hover:shadow-[5px_7px_0_rgba(0,0,0,0.12)] dark:hover:shadow-[5px_7px_0_rgba(200,200,255,0.08)]"
                :style="{ border: `1px solid color-mix(in srgb, var(--color-${feature.color}) 40%, transparent)` }"
            >
                <component :is="feature.icon" class="h-5 w-5 sm:h-6 sm:w-6 mb-2 sm:mb-3" :style="{ color: `var(--color-${feature.color})` }" />
                <h3 class="instrument-serif text-base sm:text-lg mb-1 sm:mb-2">{{ feature.title }}</h3>
                <p class="text-[11px] sm:text-xs text-muted-foreground leading-relaxed">
                    <template v-for="(part, i) in feature.parts" :key="i">
                        <code
                            v-if="part.code"
                            class="font-mono text-[11px] rounded px-1.5 py-0.5"
                            :style="{
                                color: part.color ? `var(--color-${part.color})` : undefined,
                                background: part.color
                                    ? `color-mix(in srgb, var(--color-${part.color}) 12%, transparent)`
                                    : 'hsl(var(--muted) / 0.5)',
                                border: `1px solid ${part.color ? `color-mix(in srgb, var(--color-${part.color}) 25%, transparent)` : 'hsl(var(--border) / 0.3)'}`,
                            }"
                        >{{ part.text }}</code>
                        <template v-else>{{ part.text }}</template>
                    </template>
                </p>
            </div>
        </div>
    </section>
</template>
