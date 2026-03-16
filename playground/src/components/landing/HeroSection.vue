<script setup lang="ts">
import { ref, watch, onMounted, onBeforeUnmount } from "vue";
import { useScrollMorph } from "@/composables/useScrollMorph";
import { useHeroState } from "@/composables/useHeroState";
import { BbnfLogo } from "@/components/custom/bbnf-logo";
import TypewriterText from "./TypewriterText.vue";
import CodeCardFan from "./CodeCardFan.vue";
import CodeCardGrid from "./CodeCardGrid.vue";
import { codeCards, expandConfigs } from "@/lib/heroCards";

// Scroll-morph: hero logo animates toward the navbar logo.
// Uses a separate marker element (untransformed) for accurate position measurement.
// elementRef is written to directly in rAF — no Vue reactivity lag.
const heroLogoMarker = ref<HTMLElement | null>(null);
const heroLogoElement = ref<HTMLElement | null>(null);
const { progress: morphProgressLocal } = useScrollMorph(heroLogoMarker, heroLogoElement, "[data-navbar-logo]", { scrollThreshold: 0.35 });

// Share morph progress with NavBar
const { morphProgress } = useHeroState();
watch(morphProgressLocal, (p) => { morphProgress.value = p; }, { immediate: true });
onBeforeUnmount(() => { morphProgress.value = 1; });

const visible = ref(false);
onMounted(() => {
    requestAnimationFrame(() => { visible.value = true; });
});
</script>

<template>
    <section class="min-h-[calc(100dvh-3.5rem)] flex flex-col items-center justify-center px-4 sm:px-6 py-12 sm:py-20 gap-8 sm:gap-12">
        <!-- Text content -->
        <div
            class="text-center max-w-3xl transition-all duration-700 relative z-50"
            :class="visible ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-6'"
        >
            <!-- BBNF Logo — above heading, morphs into navbar on scroll -->
            <div class="relative mb-6 inline-block">
                <!-- Invisible marker at logo's natural position (no morph transform) -->
                <span ref="heroLogoMarker" class="absolute inset-0 pointer-events-none" aria-hidden="true" />
                <!-- Logo with morph transform — hidden once morph completes (navbar logo takes over) -->
                <span
                    ref="heroLogoElement"
                    class="inline-block relative z-[60] transition-opacity duration-150"
                    :class="morphProgressLocal >= 1 ? 'opacity-0 pointer-events-none' : 'opacity-100'"
                >
                    <BbnfLogo size="xl" shimmer />
                </span>
            </div>

            <h1 class="instrument-serif text-4xl sm:text-5xl md:text-7xl lg:text-8xl tracking-tight text-foreground mb-4">
                Grammar-driven<br />parser &amp; formatter
            </h1>

            <TypewriterText />

            <p class="text-lg text-muted-foreground max-w-xl mx-auto mb-10">
                Define a grammar in BBNF. Get a parser, error recovery, and a pretty-printer — all from a single source of truth.
            </p>

            <!-- CTA pair -->
            <div class="flex flex-wrap justify-center gap-4">
                <router-link
                    to="/playground"
                    class="group relative px-6 py-3 rounded-xl bg-pastel-green/10 border border-pastel-green/30 backdrop-blur-sm
                           instrument-serif text-lg text-foreground transition-all duration-300
                           hover:scale-[1.02] hover:-translate-y-0.5
                           shadow-card hover:shadow-card-hover"
                >
                    <span class="relative z-10">Try the Playground</span>
                    <div class="absolute inset-0 rounded-xl bg-gradient-to-r from-transparent via-pastel-green/10 to-transparent
                                bg-[length:200%_100%] opacity-0 group-hover:opacity-100 group-hover:animate-[shimmer_3s_linear_infinite] transition-opacity" />
                </router-link>
                <router-link
                    to="/docs"
                    class="group relative px-6 py-3 rounded-xl bg-pastel-blue/10 border border-pastel-blue/30 backdrop-blur-sm
                           instrument-serif text-lg text-foreground transition-all duration-300
                           hover:scale-[1.02] hover:-translate-y-0.5
                           shadow-card hover:shadow-card-hover"
                >
                    <span class="relative z-10">Read the Docs</span>
                    <div class="absolute inset-0 rounded-xl bg-gradient-to-r from-transparent via-pastel-blue/10 to-transparent
                                bg-[length:200%_100%] opacity-0 group-hover:opacity-100 group-hover:animate-[shimmer_3s_linear_infinite] transition-opacity" />
                </router-link>
            </div>
        </div>

        <hr class="border-border/60 border-t-2 w-full max-w-5xl" />

        <!-- Code cards: vertical stack on mobile, 3D fan on md+ -->
        <CodeCardGrid :cards="codeCards" :visible="visible" />
        <CodeCardFan :cards="codeCards" :expand-configs="expandConfigs" :visible="visible" />
    </section>
</template>
