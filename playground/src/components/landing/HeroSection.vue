<script setup lang="ts">
import { ref, onMounted } from "vue";
import { useTypewriter, type TypewriterWord } from "@/components/landing/useTypewriter";
import { useHeroSequence } from "@/components/landing/useHeroSequence";
import { BbnfLogo } from "@/components/custom/bbnf-logo";
import TypewriterText from "./TypewriterText.vue";
import CodeCardFan from "./CodeCardFan.vue";
import CodeCardGrid from "./CodeCardGrid.vue";
import { codeCards, expandConfigs } from "@/lib/heroCards";

const words: TypewriterWord[] = [
    { text: "that",     className: "tw-rainbow", isCode: false },
    { text: "BBNF",     className: "tw-golden",  isCode: false },
    { text: "JSON",     className: "tw-green",   isCode: true },
    { text: "CSS",      className: "tw-blue",    isCode: true },
    { text: "HTML",     className: "tw-purple",  isCode: true },
    { text: "TOML",     className: "tw-amber",   isCode: true },
];

// Typewriter with external control
const typewriter = useTypewriter(words);

// Morph marker + element refs
const morphMarker = ref<HTMLElement | null>(null);
const morphElement = ref<HTMLElement | null>(null);

const visible = ref(false);
/** True once the entrance transition is done — clears the translate so no
 *  ancestor `transform` exists to break `position: fixed` for the morph. */
const entranceDone = ref(false);
onMounted(() => {
    requestAnimationFrame(() => { visible.value = true; });
    setTimeout(() => { entranceDone.value = true; }, 750);
});

// Hero sequence: scroll-driven typewriter → morph orchestration
const { morphElementOpacity, typewriterOpacity, cursorOpacity } = useHeroSequence(
    typewriter,
    words,
    morphMarker,
    morphElement,
    "[data-navbar-logo]",
    { scrollThreshold: 0.35, entranceDone },
);
</script>

<template>
    <section class="min-h-[calc(100dvh-3.5rem)] flex flex-col items-center justify-center px-4 sm:px-6 py-12 sm:py-20 gap-8 sm:gap-12">
        <!-- Text content -->
        <div
            class="text-center max-w-3xl relative z-50"
            :class="[
                entranceDone ? 'transition-opacity' : 'transition-all duration-700',
                visible ? 'opacity-100' : 'opacity-0',
                !visible ? 'translate-y-6' : '',
                visible && !entranceDone ? 'translate-y-0' : '',
            ]"
        >
            <h1 class="text-display-2 sm:text-display-3 text-foreground mb-4">
                Grammar-driven<br />parser &amp; formatter
            </h1>

            <!-- Typewriter -->
            <TypewriterText
                :display-text="typewriter.displayText.value"
                :current-word="typewriter.currentWord.value"
                :word-opacity="typewriterOpacity"
                :cursor-opacity="cursorOpacity"
            />

            <!--
                Morph origin block: always rendered for measurement.
                The inner container sizes itself to BbnfLogo (via the invisible sizer).
                Marker (absolute inset-0) measures that exact rect.
                Morph element overlays the same space, controlled by morphElementOpacity.
                No ancestor transforms — safe for position:fixed.

                The absolute overlay on the typewriter row ensures the morph starts
                from the typewriter's vertical position without affecting layout.
            -->
            <div class="absolute left-0 right-0 flex items-center justify-center pointer-events-none">
                <div class="relative inline-block">
                    <!-- Invisible sizer: establishes container dimensions = BbnfLogo natural size -->
                    <span class="inline-block invisible" aria-hidden="true">
                        <BbnfLogo size="xl" />
                    </span>
                    <!-- Marker: measures this correct BbnfLogo-sized rect -->
                    <span ref="morphMarker" class="absolute inset-0 pointer-events-none" aria-hidden="true" />
                    <!-- Morph element: fades in during Phase C, goes position:fixed for flight -->
                    <span
                        ref="morphElement"
                        class="inline-block absolute inset-0 z-[60]"
                        :style="{ opacity: morphElementOpacity }"
                    >
                        <BbnfLogo size="xl" shimmer />
                    </span>
                </div>
            </div>

            <p class="text-lg text-muted-foreground max-w-xl mx-auto mb-10">
                Define a grammar in BBNF. Get a parser, error recovery, and a pretty-printer—all in one.
            </p>

            <!-- CTA pair -->
            <div class="flex flex-wrap justify-center gap-4">
                <router-link
                    to="/playground"
                    class="group btn-cta bg-pastel-green/10 border border-pastel-green/30"
                >
                    <span class="relative z-10">Try the Playground</span>
                    <div class="absolute inset-0 rounded-xl bg-gradient-to-r from-transparent via-pastel-green/10 to-transparent
                                bg-[length:200%_100%] opacity-0 group-hover:opacity-100 group-hover:animate-[shimmer_3s_linear_infinite] transition-opacity" />
                </router-link>
                <router-link
                    to="/docs"
                    class="group btn-cta bg-pastel-blue/10 border border-pastel-blue/30"
                >
                    <span class="relative z-10">Read the Docs</span>
                    <div class="absolute inset-0 rounded-xl bg-gradient-to-r from-transparent via-pastel-blue/10 to-transparent
                                bg-[length:200%_100%] opacity-0 group-hover:opacity-100 group-hover:animate-[shimmer_3s_linear_infinite] transition-opacity" />
                </router-link>
            </div>
        </div>

        <div class="tapered-rule w-full max-w-5xl" />

        <!-- Code cards: vertical stack on mobile, 3D fan on md+ -->
        <CodeCardGrid :cards="codeCards" :visible="visible" />
        <CodeCardFan :cards="codeCards" :expand-configs="expandConfigs" :visible="visible" />
    </section>
</template>
