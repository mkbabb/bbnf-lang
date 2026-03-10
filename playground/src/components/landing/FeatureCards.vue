<script setup lang="ts">
import { Blocks, ShieldCheck, Paintbrush, Zap } from "lucide-vue-next";

const features = [
    {
        icon: Blocks,
        title: "Parser Combinators",
        iconClass: "text-pastel-green",
        borderClass: "border-pastel-green/40",
        parts: [
            { text: "Compose parsers from simple building blocks. String literals, regex, alternation, repetition — all in a clean grammar syntax." },
        ],
    },
    {
        icon: ShieldCheck,
        title: "Error Recovery",
        iconClass: "text-pastel-blue",
        borderClass: "border-pastel-blue/40",
        parts: [
            { text: "Parse past errors with " },
            { text: "@recover", code: true, codeClass: "text-pastel-blue bg-pastel-blue/12 border-pastel-blue/25" },
            { text: " directives. Collect multiple diagnostics in a single pass, just like a real compiler." },
        ],
    },
    {
        icon: Paintbrush,
        title: "Pretty-Printing",
        iconClass: "text-pastel-pink",
        borderClass: "border-pastel-pink/40",
        parts: [
            { text: "Annotate rules with " },
            { text: "@pretty", code: true, codeClass: "text-pastel-pink bg-pastel-pink/12 border-pastel-pink/25" },
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
        iconClass: "text-pastel-amber",
        borderClass: "border-pastel-amber/40",
        parts: [
            { text: "Built-in languages run through " },
            { text: "gorgeous", code: true, codeClass: "text-pastel-amber bg-pastel-amber/12 border-pastel-amber/25" },
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
                class="rounded-xl border bg-card/80 backdrop-blur-sm p-3 sm:p-5 transition-all duration-300
                       hover:-translate-y-0.5 cursor-default
                       shadow-[3px_3px_0px_0px_rgba(0,0,0,0.08)] dark:shadow-[3px_3px_0px_0px_rgba(200,200,255,0.06)]
                       hover:shadow-[5px_7px_0_rgba(0,0,0,0.12)] dark:hover:shadow-[5px_7px_0_rgba(200,200,255,0.08)]"
                :class="feature.borderClass"
            >
                <component :is="feature.icon" class="h-5 w-5 sm:h-6 sm:w-6 mb-2 sm:mb-3" :class="feature.iconClass" />
                <h3 class="instrument-serif text-base sm:text-lg mb-1 sm:mb-2">{{ feature.title }}</h3>
                <p class="text-[11px] sm:text-xs text-muted-foreground leading-relaxed">
                    <template v-for="(part, i) in feature.parts" :key="i">
                        <code
                            v-if="part.code"
                            class="font-mono text-[11px] rounded border px-1.5 py-0.5"
                            :class="part.codeClass ?? 'bg-muted/50 border-border/30'"
                        >{{ part.text }}</code>
                        <template v-else>{{ part.text }}</template>
                    </template>
                </p>
            </div>
        </div>
    </section>
</template>
