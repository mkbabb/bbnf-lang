<script setup lang="ts">
import { computed } from "vue";
import { useCodeHighlight } from "../composables/useCodeHighlight";
import { PAPER_CONTEXT } from "../context";
import { inject } from "vue";

const props = defineProps<{
    code: string;
    caption?: string;
    language?: string;
}>();

const ctx = inject(PAPER_CONTEXT)!;
const { highlight } = useCodeHighlight();

const highlighted = computed(() => highlight(props.code, props.language));
</script>

<template>
    <figure class="paper-code-block">
        <figcaption
            v-if="caption"
            class="paper-code-caption"
            v-html="ctx.renderTitle(caption)"
        />
        <pre
            class="paper-code-pre hljs"
            :data-language="highlighted.language || language || undefined"
        ><code v-html="highlighted.html" /></pre>
    </figure>
</template>
