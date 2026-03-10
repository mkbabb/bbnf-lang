<script setup lang="ts">
import { useTypewriter, type TypewriterWord } from "@/composables/useTypewriter";

const words: TypewriterWord[] = [
    { text: "that",     className: "tw-rainbow", isCode: false },
    { text: "BBNF",     className: "tw-golden",  isCode: false },
    { text: "JSON",     className: "tw-green",   isCode: true },
    { text: "CSS",      className: "tw-blue",    isCode: true },
    { text: "HTML",     className: "tw-purple",  isCode: true },
    { text: "TOML",     className: "tw-amber",   isCode: true },
];

const { displayText, currentWord } = useTypewriter(words);
</script>

<template>
    <div class="flex items-baseline justify-center mb-6 typewriter-row">
        <span class="instrument-serif text-5xl sm:text-6xl md:text-7xl lg:text-8xl text-muted-foreground whitespace-nowrap">to parse:&nbsp;</span>
        <span
            :class="[
                currentWord.isCode ? 'font-mono code-word' : 'instrument-serif',
                currentWord.className,
            ]"
            class="inline-block min-w-[3ch] text-5xl sm:text-6xl md:text-7xl lg:text-8xl leading-none overflow-visible"
        >{{ displayText }}</span><span class="tw-cursor text-5xl sm:text-6xl md:text-7xl lg:text-8xl">|</span>
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
    background: linear-gradient(90deg, #b8860b, #ffd700, #daa520, #ffd700, #b8860b);
    background-size: 200% 100%;
    background-clip: text;
    -webkit-background-clip: text;
    color: transparent;
    animation: shimmer 3s linear infinite;
    padding-bottom: 0.15em;
}
@keyframes shimmer {
    0% { background-position: 200% 0; }
    100% { background-position: -200% 0; }
}

.tw-green { color: var(--color-pastel-green); }
.tw-blue { color: var(--color-pastel-blue); }
.tw-purple { color: var(--color-pastel-purple); }
.tw-amber { color: var(--color-pastel-amber); }

/* Fixed height prevents reflow when switching between serif and code words.
   Height matches the tallest variant (code-word with border/padding at lg:text-8xl). */
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
