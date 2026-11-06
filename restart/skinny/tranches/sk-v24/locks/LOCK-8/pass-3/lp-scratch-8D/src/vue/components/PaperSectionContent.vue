<script setup lang="ts">
import { inject, provide, useSlots } from "vue";
import type { PaperSectionData } from "../../types/output";
import { CONTENT_SLOTS } from "./paperSectionSlots";
import PaperSectionBlocks from "./PaperSectionBlocks.vue";
import PaperSection from "./PaperSection.vue";

const props = defineProps<{
    section: PaperSectionData;
    depth: number;
    sectionIndex: number;
}>();

const ownSlots = useSlots();
// Root instance captures consumer slots; children inherit via inject.
const parentSlots = inject(CONTENT_SLOTS, null);
const effectiveSlots = parentSlots ?? ownSlots;
provide(CONTENT_SLOTS, effectiveSlots);
</script>

<template>
    <PaperSection
        :id="section.id"
        :number="section.number"
        :title="section.title"
        :depth="depth"
        :section-index="sectionIndex"
    >
        <PaperSectionBlocks :section="section" />

        <!-- Recursive subsections -->
        <template v-if="section.subsections">
            <PaperSectionContent
                v-for="sub in section.subsections"
                :key="sub.id"
                :section="sub"
                :depth="depth + 1"
                :section-index="sectionIndex"
            />
        </template>
    </PaperSection>
</template>
