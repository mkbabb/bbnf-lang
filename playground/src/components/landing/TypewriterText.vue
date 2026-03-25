<script setup lang="ts">
import { ref } from "vue";
import type { TypewriterWord } from "@/composables/useTypewriter";

withDefaults(defineProps<{
    displayText: string;
    currentWord: TypewriterWord;
    wordOpacity?: number;
    cursorOpacity?: number;
}>(), {
    wordOpacity: 1,
    cursorOpacity: 1,
});

const wordRef = ref<HTMLElement | null>(null);
defineExpose({ wordRef });
</script>

<template>
    <div class="flex items-baseline justify-center mb-6 typewriter-row">
        <span class="instrument-serif text-5xl sm:text-6xl md:text-7xl lg:text-8xl text-muted-foreground whitespace-nowrap">to parse:&nbsp;</span>
        <span
            ref="wordRef"
            :class="[
                currentWord.isCode ? 'font-mono code-word' : 'instrument-serif',
                currentWord.className,
            ]"
            class="inline-block min-w-[3ch] text-5xl sm:text-6xl md:text-7xl lg:text-8xl leading-none overflow-visible"
            :style="{ opacity: wordOpacity }"
        >{{ displayText }}</span><span
            class="tw-cursor text-5xl sm:text-6xl md:text-7xl lg:text-8xl"
            :style="{ opacity: cursorOpacity }"
        >|</span>
    </div>
</template>

<style scoped>
.tw-cursor {
    font-weight: 100;
    color: var(--color-foreground);
    animation: blink 1.06s step-end infinite;
}
@keyframes blink {
    50% { opacity: 0; }
}

.tw-rainbow {
    background: linear-gradient(90deg, #ff6b6b, #ffd93d, #6bff6b, #6bc5ff, #c56bff, #ff6b6b);
    background-size: 300% 100%;
    background-clip: text;
    -webkit-background-clip: text;
    color: transparent;
    animation: rainbow-shift 4s linear infinite;
    padding-bottom: 0.15em;
}
@keyframes rainbow-shift {
    0% { background-position: 0% 50%; }
    100% { background-position: 300% 50%; }
}

.tw-golden {
    background: linear-gradient(90deg, #c49a2e, #e8c84a, #d4a832, #e8c84a, #c49a2e);
    background-size: 250% 100%;
    background-clip: text;
    -webkit-background-clip: text;
    color: transparent;
    animation: shimmer 5s linear infinite;
    padding-bottom: 0.15em;
}
@keyframes shimmer {
    0% { background-position: 250% 0; }
    100% { background-position: -250% 0; }
}

.tw-green { color: var(--color-pastel-green); }
.tw-blue { color: var(--color-pastel-blue); }
.tw-purple { color: var(--color-pastel-purple); }
.tw-amber { color: var(--color-pastel-amber); }

/* Fixed height prevents reflow when switching between serif and code words. */
.typewriter-row {
    height: clamp(3.5rem, 9vw, 7.5rem);
}

.code-word {
    background: hsl(var(--muted) / 0.4);
    border: 1px solid hsl(var(--border) / 0.5);
    border-radius: 0.25em;
    padding: 0 0.15em;
    box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04);
}
</style>
