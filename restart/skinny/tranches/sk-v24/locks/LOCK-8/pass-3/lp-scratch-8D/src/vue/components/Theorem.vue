<script setup lang="ts">
import { inject } from "vue";
import { PAPER_CONTEXT } from "../context";

defineProps<{
    type:
        | "theorem"
        | "definition"
        | "lemma"
        | "proposition"
        | "corollary"
        | "aside"
        | "example";
    name?: string;
    number?: string;
    label?: string;
}>();

const ctx = inject(PAPER_CONTEXT)!;

const labels: Record<string, string> = {
    theorem: "Theorem",
    definition: "Definition",
    lemma: "Lemma",
    proposition: "Proposition",
    corollary: "Corollary",
    aside: "Aside",
    example: "Example",
};
</script>

<template>
    <div
        :id="label ? label.replace(/:/g, '-') : undefined"
        class="theorem-block"
        :class="`theorem-block--${type}`"
    >
        <p class="theorem-label">
            <span class="theorem-type">{{ labels[type] }}</span>
            <span v-if="number" class="theorem-number">&nbsp;{{ number }}</span>
            <template v-if="name">
                —
                <em
                    class="theorem-name"
                    v-html="ctx.renderTitle(name)"
                />
            </template>
        </p>
        <div
            class="theorem-body"
            :class="{
                'theorem-body--italic':
                    type === 'theorem' ||
                    type === 'lemma' ||
                    type === 'proposition' ||
                    type === 'corollary',
            }"
        >
            <slot />
        </div>
    </div>
</template>
