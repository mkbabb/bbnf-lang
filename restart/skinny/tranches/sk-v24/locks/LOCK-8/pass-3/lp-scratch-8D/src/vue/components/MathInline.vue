<script setup lang="ts">
import { inject } from "vue";
import { PAPER_CONTEXT } from "../context";

const props = defineProps<{ tex: string }>();
const ctx = inject(PAPER_CONTEXT)!;
const html = ctx.renderInline(props.tex);
</script>

<template>
    <span class="math-inline" v-once v-html="html" />
</template>
