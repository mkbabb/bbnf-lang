<script setup lang="ts">
import { ref, inject } from "vue";
import { Select, SelectTrigger, SelectValue, SelectContent, SelectItem } from "@/components/ui/select";
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";
import InlineRichText from "@/components/ui/InlineRichText.vue";
import { exampleIcons, exampleToneClass, shimmerClass, tagToneStyle } from "@/lib/toneMaps";
import type { Example } from "@/composables/useExamples";

defineProps<{
    examples: Example[];
    currentExample: Example;
}>();

const emit = defineEmits<{
    selectExample: [name: string];
}>();

const dockKeepOpen = inject<(() => void) | null>("dockKeepOpen", null);
const dockRelease = inject<(() => void) | null>("dockRelease", null);

function onSelectOpenChange(open: boolean) {
    if (open) dockKeepOpen?.();
    else dockRelease?.();
}

const descriptionScrollDistances = ref<Record<string, number>>({});

function measureDescriptionScroll(name: string, event: Event) {
    const item = event.currentTarget as HTMLElement | null;
    if (!item) return;

    const viewport = item.querySelector("[data-example-description-viewport]") as HTMLElement | null;
    const track = item.querySelector("[data-example-description-track]") as HTMLElement | null;
    if (!viewport || !track) return;

    descriptionScrollDistances.value[name] = Math.max(0, track.scrollWidth - viewport.clientWidth);
}

function descriptionScrollStyle(name: string) {
    const distance = descriptionScrollDistances.value[name] ?? 0;
    return { "--description-scroll-distance": `${distance}px` };
}

function hasDescriptionOverflow(name: string) {
    return (descriptionScrollDistances.value[name] ?? 0) > 8;
}
</script>

<template>
    <Tooltip>
        <TooltipTrigger as-child>
            <div>
                <Select
                    :model-value="currentExample.name"
                    @update:model-value="(v) => emit('selectExample', v as string)"
                    @update:open="onSelectOpenChange"
                >
                    <SelectTrigger class="h-9 w-auto max-w-full border-none bg-transparent px-2 shadow-none instrument-serif text-sm text-foreground rounded-xl sm:h-10 sm:text-base [&>span]:flex [&>span]:items-center [&>span]:gap-2">
                        <SelectValue>
                            <img :src="exampleIcons[currentExample.name] ?? '/img/text.svg'" alt="" class="h-4 w-4 shrink-0 sm:h-5 sm:w-5" />
                            <span :class="shimmerClass(currentExample.name)">{{ currentExample.name }}</span>
                        </SelectValue>
                    </SelectTrigger>
                    <SelectContent align="start" :side-offset="10" class="min-w-[18rem] max-w-[23rem] overflow-hidden">
                        <SelectItem
                            v-for="ex in examples"
                            :key="ex.name"
                            :value="ex.name"
                            class="group/example overflow-hidden rounded-xl border bg-card/35 py-2 pl-3 pr-3 transition-all duration-200 hover:-translate-y-0.5 data-[state=checked]:-translate-y-0.5 data-[state=checked]:bg-card/60 data-[state=checked]:shadow-md data-[state=checked]:border-accent/40"
                            :class="exampleToneClass(ex.name)"
                            @pointerenter="measureDescriptionScroll(ex.name, $event)"
                            @focusin="measureDescriptionScroll(ex.name, $event)"
                        >
                            <div class="flex items-start gap-3">
                                <img :src="exampleIcons[ex.name] ?? '/img/text.svg'" alt="" class="mt-0.5 h-5 w-5 shrink-0" />
                                <div class="min-w-0">
                                    <div class="flex items-center gap-2">
                                        <span class="instrument-serif text-base" :class="shimmerClass(ex.name)">{{ ex.name }}</span>
                                    </div>
                                    <div
                                        data-example-description-viewport
                                        class="mt-1 overflow-hidden"
                                    >
                                        <p
                                            data-example-description-track
                                            class="example-description-track text-sm text-muted-foreground whitespace-nowrap"
                                            :data-overflow="hasDescriptionOverflow(ex.name) ? 'true' : undefined"
                                            :style="descriptionScrollStyle(ex.name)"
                                        >
                                            <InlineRichText :text="ex.description" />
                                        </p>
                                    </div>
                                    <div v-if="ex.tags?.length" class="mt-2 flex flex-wrap gap-1">
                                        <span
                                            v-for="tag in ex.tags"
                                            :key="tag"
                                            class="inline-flex items-center rounded-full px-2 py-0.5 font-mono text-[length:var(--font-size-label)] tracking-[0.04em] backdrop-blur-sm"
                                            :style="tagToneStyle(tag)"
                                        >
                                            {{ tag }}
                                        </span>
                                    </div>
                                </div>
                            </div>
                        </SelectItem>
                    </SelectContent>
                </Select>
            </div>
        </TooltipTrigger>
        <TooltipContent side="top" :side-offset="8" class="max-w-xs border-border/40 bg-card/90 backdrop-blur-xl">
            <p class="text-sm sm:text-base">Swap between showcase grammars.</p>
        </TooltipContent>
    </Tooltip>
</template>

<style scoped>
.example-description-track {
    display: inline-block;
    min-width: 100%;
    padding-right: 1.5rem;
}

.group\/example:hover .example-description-track[data-overflow="true"],
.group\/example:focus-within .example-description-track[data-overflow="true"] {
    animation: description-marquee 5.5s var(--ease-smooth) infinite alternate;
}

@keyframes description-marquee {
    0%, 18% { transform: translateX(0); }
    82%, 100% { transform: translateX(calc(-1 * var(--description-scroll-distance, 0px))); }
}
</style>
