<script setup lang="ts">
import { inject } from "vue";
import { PAPER_CONTEXT } from "../context";

const props = defineProps<{
    tex: string;
    id?: string;
    number?: string;
    numbered?: boolean;
}>();
const ctx = inject(PAPER_CONTEXT)!;
const html = ctx.renderDisplay(props.tex);
</script>

<template>
    <div class="math-block" :id="id">
        <div class="math-block__equation" v-once v-html="html" />
        <div v-if="numbered && number" class="math-block__number fira-code">
            ({{ number }})
        </div>
    </div>
</template>
