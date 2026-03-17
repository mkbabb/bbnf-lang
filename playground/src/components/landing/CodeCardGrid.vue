<script setup lang="ts">
import type { CodeCard } from "@/lib/heroCards";

defineProps<{
    cards: CodeCard[];
    visible: boolean;
}>();
</script>

<template>
    <div
        class="md:hidden grid grid-cols-2 gap-3 w-full max-w-lg px-2 transition-all duration-700 delay-200"
        :class="visible ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-6'"
    >
        <div
            v-for="card in cards"
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
            <pre class="text-[0.625rem] leading-relaxed text-foreground/80 overflow-hidden"><code><template v-for="(line, j) in card.lines" :key="j"><template v-for="(span, k) in line.spans" :key="k"><span v-if="span.cls" :class="span.cls">{{ span.text }}</span><template v-else>{{ span.text }}</template></template>
</template></code></pre>
        </div>
    </div>
</template>
