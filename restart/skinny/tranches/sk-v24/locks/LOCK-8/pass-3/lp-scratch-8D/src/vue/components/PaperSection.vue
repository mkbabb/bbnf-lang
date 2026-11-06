<script setup lang="ts">
import { inject } from "vue";
import { PAPER_CONTEXT } from "../context";

defineProps<{
    id: string;
    number: string;
    title: string;
    depth?: number;
    sectionIndex?: number;
}>();

const ctx = inject(PAPER_CONTEXT)!;
</script>

<template>
    <section :id="id" class="paper-section" :style="sectionIndex != null ? { '--_section-color': `var(--section-color-${sectionIndex})` } : undefined">
        <div
            class="section-header"
            :class="{
                'section-header--chapter': (depth ?? 0) === 0,
                'section-header--sub': (depth ?? 0) > 0,
            }"
        >
            <component
                :is="(depth ?? 0) > 0 ? 'h3' : 'h2'"
                class="section-heading"
            >
                <span v-if="number" class="section-number">{{ number }}.</span>
                <span class="section-title" v-html="ctx.renderTitle(title)" />
            </component>
            <div v-if="(depth ?? 0) === 0" class="section-divider" />
        </div>
        <div class="section-body">
            <slot />
        </div>
    </section>
</template>
