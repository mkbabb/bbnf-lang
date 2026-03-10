<script setup lang="ts">
import { computed } from "vue";
import { BbnfLogo } from "@/components/custom/bbnf-logo";

type RichTextPart = {
    code: boolean;
    text: string;
};

const props = defineProps<{
    text: string;
}>();

const tokenToneMap: Record<string, string> = {
    "@pretty": "pastel-pink",
    "@recover": "pastel-blue",
    "@media": "pastel-blue",
    "@supports": "pastel-blue",
    "error recovery": "pastel-blue",
    "JSON": "pastel-green",
    "CSS L1.75": "pastel-blue",
    "Auto": "pastel-blue",
    "gorgeous": "pastel-amber",
    "WASM": "pastel-amber",
    "TS interpreter": "pastel-purple",
};

const parts = computed<RichTextPart[]>(() => {
    const output: RichTextPart[] = [];
    const pattern = /`([^`]+)`/g;
    let lastIndex = 0;

    for (const match of props.text.matchAll(pattern)) {
        const matchIndex = match.index ?? 0;
        if (matchIndex > lastIndex) {
            output.push({
                code: false,
                text: props.text.slice(lastIndex, matchIndex),
            });
        }

        output.push({
            code: true,
            text: match[1] ?? "",
        });

        lastIndex = matchIndex + match[0].length;
    }

    if (lastIndex < props.text.length) {
        output.push({
            code: false,
            text: props.text.slice(lastIndex),
        });
    }

    if (output.length === 0) {
        output.push({
            code: false,
            text: props.text,
        });
    }

    return output;
});

function chipStyle(token: string) {
    const tone = tokenToneMap[token];
    if (!tone) {
        return {
            color: "var(--color-foreground)",
            background: "hsl(var(--muted) / 0.55)",
            border: "1px solid hsl(var(--border) / 0.45)",
            boxShadow: "inset 0 1px 0 hsl(var(--background) / 0.2)",
        };
    }

    return {
        color: `var(--color-${tone})`,
        background: `color-mix(in srgb, var(--color-${tone}) 12%, transparent)`,
        border: `1px solid color-mix(in srgb, var(--color-${tone}) 24%, transparent)`,
        boxShadow: `inset 0 1px 0 color-mix(in srgb, var(--color-${tone}) 10%, transparent)`,
    };
}

function isBbnfToken(token: string) {
    return token === "BBNF";
}

function bbnfBadgeStyle() {
    return {
        background: "linear-gradient(180deg, color-mix(in srgb, var(--color-pastel-blue) 10%, hsl(var(--card)) 90%), color-mix(in srgb, var(--color-pastel-green) 10%, hsl(var(--card)) 90%))",
        border: "1px solid color-mix(in srgb, var(--color-pastel-green) 28%, transparent)",
        boxShadow: "inset 0 1px 0 color-mix(in srgb, var(--color-pastel-blue) 18%, transparent), 0 0 0 1px color-mix(in srgb, var(--color-pastel-blue) 8%, transparent)",
    };
}
</script>

<template>
    <span class="inline">
        <template v-for="(part, index) in parts" :key="`${part.text}-${index}`">
            <span
                v-if="part.code && isBbnfToken(part.text)"
                class="mx-[0.08rem] inline-flex items-center rounded-full px-2 py-0.5 align-[0.05em] backdrop-blur-sm"
                :style="bbnfBadgeStyle()"
            >
                <BbnfLogo size="xs" brand />
            </span>
            <code
                v-else-if="part.code"
                class="mx-[0.05rem] inline-flex items-center rounded-full px-1.5 py-0.5 align-[0.05em] font-mono text-[0.82em] leading-none backdrop-blur-sm"
                :style="chipStyle(part.text)"
            >
                {{ part.text }}
            </code>
            <template v-else>{{ part.text }}</template>
        </template>
    </span>
</template>
