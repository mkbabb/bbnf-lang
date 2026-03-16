<script setup lang="ts">
import { ref } from "vue";
import { useMouseParallax } from "@/composables/useMouseParallax";
import type { CodeCard } from "@/lib/heroCards";

const props = defineProps<{
    cards: CodeCard[];
    expandConfigs: { tx: number; ty: number; tz: number; ry: number; scale: number }[];
    visible: boolean;
}>();

const perspectiveRef = ref<HTMLElement | null>(null);
const { rotateX, rotateY } = useMouseParallax(perspectiveRef, 5);

const expanded = ref(false);

function cardTransform(i: number): string {
    const center = (props.cards.length - 1) / 2;
    const base = `translate(-50%, -50%) translateZ(${i * 35}px) translateX(${(i - center) * 25}px) rotateY(${(i - center) * 1.5}deg)`;
    if (!expanded.value) return base;
    const e = props.expandConfigs[i]!;
    return `translate(-50%, -50%) translateZ(${e.tz}px) translateX(${e.tx}px) translateY(${e.ty}px) rotateY(${e.ry}deg) scale(${e.scale})`;
}
</script>

<template>
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
                v-for="(card, i) in cards"
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
                <pre class="text-sm leading-relaxed text-foreground/80 overflow-hidden"><code><template v-for="(line, j) in card.lines" :key="j"><template v-for="(span, k) in line.spans" :key="k"><span v-if="span.cls" :class="span.cls">{{ span.text }}</span><template v-else>{{ span.text }}</template></template>
</template></code></pre>
            </div>
        </div>
    </div>
</template>

<style scoped>
.card-fan {
    transition: transform 0.6s var(--ease-spring);
}

@keyframes float {
    from { transform: translate(-50%, -50%) translateZ(var(--tz, 0)) translateX(var(--tx, 0)) rotateY(var(--ry, 0)) translateY(0); }
    to { transform: translate(-50%, -50%) translateZ(var(--tz, 0)) translateX(var(--tx, 0)) rotateY(var(--ry, 0)) translateY(8px); }
}
</style>
