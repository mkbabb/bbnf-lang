<script setup lang="ts">
import { ref, watch, onMounted, onBeforeUnmount } from "vue";
import { useMouseParallax } from "@/composables/useMouseParallax";
import { useScrollMorph } from "@/composables/useScrollMorph";
import { useHeroState } from "@/composables/useHeroState";
import { BbnfLogo } from "@/components/custom/bbnf-logo";
import TypewriterText from "./TypewriterText.vue";

const perspectiveRef = ref<HTMLElement | null>(null);
const { rotateX, rotateY } = useMouseParallax(perspectiveRef, 5);

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

// Syntax-highlighted card content
interface CardLine {
    spans: { text: string; cls?: string }[];
}
interface CodeCard {
    title: string;
    color: string;
    lines: CardLine[];
}

const codeCards: CodeCard[] = [
    {
        title: "Grammar",
        color: "pastel-green",
        lines: [
            { spans: [
                { text: "value", cls: "hl-type" }, { text: " = object " }, { text: "|", cls: "hl-operator" },
                { text: " array " }, { text: "|", cls: "hl-operator" }, { text: " string " },
                { text: "|", cls: "hl-operator" }, { text: " number " }, { text: ";", cls: "hl-operator" },
            ] },
            { spans: [
                { text: "object", cls: "hl-type" }, { text: " = " },
                { text: '"{" ', cls: "hl-string" }, { text: ", ", cls: "hl-operator" },
                { text: "members", cls: "hl-type" }, { text: "?", cls: "hl-operator" },
                { text: " , ", cls: "hl-operator" }, { text: '"}"', cls: "hl-string" },
                { text: " ;", cls: "hl-operator" },
            ] },
            { spans: [
                { text: "@pretty", cls: "hl-decorator" }, { text: " object " },
                { text: "group", cls: "hl-builtin" }, { text: " " },
                { text: "indent", cls: "hl-builtin" }, { text: " " },
                { text: "sep", cls: "hl-builtin" }, { text: "(" },
                { text: '", "', cls: "hl-string" }, { text: ")" },
                { text: " ;", cls: "hl-operator" },
            ] },
        ],
    },
    {
        title: "Input",
        color: "pastel-blue",
        lines: [
            { spans: [
                { text: "{" }, { text: '"name"', cls: "hl-string" }, { text: ": " },
                { text: '"BBNF"', cls: "hl-string" }, { text: ", " },
                { text: '"version"', cls: "hl-string" }, { text: ": " },
                { text: "1", cls: "hl-number" }, { text: "," },
            ] },
            { spans: [
                { text: ' ' }, { text: '"items"', cls: "hl-string" }, { text: ": [" },
                { text: "1", cls: "hl-number" }, { text: ", " },
                { text: "2", cls: "hl-number" }, { text: ", " },
                { text: "3", cls: "hl-number" }, { text: "]}" },
            ] },
        ],
    },
    {
        title: "Parsed AST",
        color: "pastel-purple",
        lines: [
            { spans: [
                { text: "{ " }, { text: '"type"', cls: "hl-string" }, { text: ": " },
                { text: '"object"', cls: "hl-string" }, { text: "," },
            ] },
            { spans: [
                { text: '  ' }, { text: '"members"', cls: "hl-string" }, { text: ": [" },
            ] },
            { spans: [
                { text: '    { ' }, { text: '"key"', cls: "hl-string" }, { text: ": " },
                { text: '"name"', cls: "hl-string" }, { text: " }," },
            ] },
            { spans: [{ text: "    ..." }, { text: " ]}" }] },
        ],
    },
    {
        title: "Formatted",
        color: "pastel-amber",
        lines: [
            { spans: [{ text: "{" }] },
            { spans: [
                { text: '  ' }, { text: '"name"', cls: "hl-string" }, { text: ": " },
                { text: '"BBNF"', cls: "hl-string" }, { text: "," },
            ] },
            { spans: [
                { text: '  ' }, { text: '"version"', cls: "hl-string" }, { text: ": " },
                { text: "1", cls: "hl-number" }, { text: "," },
            ] },
            { spans: [
                { text: '  ' }, { text: '"items"', cls: "hl-string" }, { text: ": [" },
                { text: "1", cls: "hl-number" }, { text: ", " },
                { text: "2", cls: "hl-number" }, { text: ", " },
                { text: "3", cls: "hl-number" }, { text: "]" },
            ] },
            { spans: [{ text: "}" }] },
        ],
    },
];

// Per-card irregular fan-out transforms — angled downward, wide spread
const expandConfigs = [
    { tx: -380, ty: -20, tz: 100, ry: -30, scale: 1.08 },
    { tx: -120, ty: -55, tz: 150, ry: -10, scale: 1.12 },
    { tx: 160,  ty: -45, tz: 160, ry: 12,  scale: 1.12 },
    { tx: 400,  ty: 15,  tz: 90,  ry: 28,  scale: 1.06 },
];

const expanded = ref(false);

function cardTransform(i: number): string {
    const center = (codeCards.length - 1) / 2;
    const base = `translate(-50%, -50%) translateZ(${i * 35}px) translateX(${(i - center) * 25}px) rotateY(${(i - center) * 1.5}deg)`;
    if (!expanded.value) return base;
    const e = expandConfigs[i]!;
    return `translate(-50%, -50%) translateZ(${e.tz}px) translateX(${e.tx}px) translateY(${e.ty}px) rotateY(${e.ry}deg) scale(${e.scale})`;
}
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
                           shadow-[3px_3px_0_rgba(0,0,0,0.08)] hover:shadow-[5px_7px_0_rgba(0,0,0,0.12)]"
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
                           shadow-[3px_3px_0_rgba(0,0,0,0.08)] hover:shadow-[5px_7px_0_rgba(0,0,0,0.12)]"
                >
                    <span class="relative z-10">Read the Docs</span>
                    <div class="absolute inset-0 rounded-xl bg-gradient-to-r from-transparent via-pastel-blue/10 to-transparent
                                bg-[length:200%_100%] opacity-0 group-hover:opacity-100 group-hover:animate-[shimmer_3s_linear_infinite] transition-opacity" />
                </router-link>
            </div>
        </div>

        <hr class="border-border/60 border-t-2 w-full max-w-5xl" />

        <!-- Code cards: vertical stack on mobile, 3D fan on md+ -->
        <!-- Mobile: vertical grid -->
        <div
            class="md:hidden grid grid-cols-2 gap-3 w-full max-w-lg px-2 transition-all duration-700 delay-200"
            :class="visible ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-6'"
        >
            <div
                v-for="card in codeCards"
                :key="card.title"
                class="rounded-xl bg-card/80 backdrop-blur-xl p-3 shadow-lg"
                :style="{
                    border: `1px solid color-mix(in srgb, var(--color-${card.color}) 40%, transparent)`,
                }"
            >
                <div class="flex items-center gap-1.5 mb-1.5">
                    <div class="h-1.5 w-1.5 rounded-full" :style="{ background: `var(--color-${card.color})` }" />
                    <span class="instrument-serif text-xs" :style="{ color: `var(--color-${card.color})` }">{{ card.title }}</span>
                </div>
                <pre class="text-[10px] leading-relaxed text-foreground/80 overflow-hidden"><code><template v-for="(line, j) in card.lines" :key="j"><template v-for="(span, k) in line.spans" :key="k"><span v-if="span.cls" :class="span.cls">{{ span.text }}</span><template v-else>{{ span.text }}</template></template>
</template></code></pre>
            </div>
        </div>

        <!-- Desktop: 3D floating fan -->
        <div
            ref="perspectiveRef"
            class="hidden md:block relative w-full max-w-5xl h-80 transition-all duration-700 delay-200 cursor-pointer overflow-visible"
            :class="visible ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-6'"
            style="perspective: 1200px"
            @mouseenter="expanded = true"
            @mouseleave="expanded = false"
            @click="expanded = !expanded"
        >
            <div
                class="relative w-full h-full"
                style="transform-style: preserve-3d"
                :style="{
                    transform: `rotateX(${-3 + rotateX}deg) rotateY(${-10 + rotateY}deg)`,
                }"
            >
                <div
                    v-for="(card, i) in codeCards"
                    :key="card.title"
                    class="absolute left-1/2 top-1/2 w-72 rounded-xl bg-card/80 backdrop-blur-xl p-4 shadow-lg card-fan"
                    :style="{
                        border: `1px solid var(--color-${card.color})`,
                        borderColor: `color-mix(in srgb, var(--color-${card.color}) 40%, transparent)`,
                        transform: cardTransform(i),
                        transitionDelay: `${i * 60}ms`,
                        animation: expanded ? 'none' : `float 4s ease-in-out ${i * 0.5}s infinite alternate`,
                    }"
                >
                    <div class="flex items-center gap-2 mb-2">
                        <div class="h-2 w-2 rounded-full" :style="{ background: `var(--color-${card.color})` }" />
                        <span class="instrument-serif text-sm" :style="{ color: `var(--color-${card.color})` }">{{ card.title }}</span>
                    </div>
                    <pre class="text-xs leading-relaxed text-foreground/80 overflow-hidden"><code><template v-for="(line, j) in card.lines" :key="j"><template v-for="(span, k) in line.spans" :key="k"><span v-if="span.cls" :class="span.cls">{{ span.text }}</span><template v-else>{{ span.text }}</template></template>
</template></code></pre>
                </div>
            </div>
        </div>
    </section>
</template>

<style scoped>
.card-fan {
    transition: transform 0.6s cubic-bezier(0.34, 1.56, 0.64, 1);
}

@keyframes float {
    from { transform: translate(-50%, -50%) translateZ(var(--tz, 0)) translateX(var(--tx, 0)) rotateY(var(--ry, 0)) translateY(0); }
    to { transform: translate(-50%, -50%) translateZ(var(--tz, 0)) translateX(var(--tx, 0)) rotateY(var(--ry, 0)) translateY(8px); }
}
@keyframes shimmer {
    0% { background-position: 200% 0; }
    100% { background-position: -200% 0; }
}
</style>
