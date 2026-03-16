<script setup lang="ts">
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";
import InlineRichText from "@/components/ui/InlineRichText.vue";
import { BbnfLogo } from "@/components/custom/bbnf-logo";
import { Redo2 } from "lucide-vue-next";
import { Card } from "@/components/ui/card";

export interface PanelTab {
    key: string;
    label: string;
    color: string;
    description: string;
}

defineProps<{
    activeTab: string;
    tabs: [PanelTab, PanelTab];
    /** Language icon map for badge overlays. */
    langIcons?: Record<string, string>;
    /** Current language for badge display (e.g. "json", "bbnf"). */
    badgeLanguage?: string;
    /** Whether to show the BBNF logo badge instead of a language icon. */
    showBbnfBadge?: boolean;
}>();

const emit = defineEmits<{
    toggleTab: [];
}>();
</script>

<template>
    <Card class="relative flex h-full min-h-0 min-w-0 flex-col overflow-hidden">
        <div class="flex items-center justify-between px-3 py-1 backdrop-blur-md bg-card/40 border-b border-border/50">
            <Tooltip>
                <TooltipTrigger as-child>
                    <span
                        class="instrument-serif text-3xl tracking-tight cursor-default"
                        :style="{ color: `var(--color-${tabs.find(t => t.key === activeTab)?.color})` }"
                    >
                        {{ tabs.find(t => t.key === activeTab)?.label }}
                    </span>
                </TooltipTrigger>
                <TooltipContent side="bottom" :side-offset="8" class="max-w-xs border-border/40 bg-card/90 backdrop-blur-xl">
                    <p class="instrument-serif text-base leading-relaxed">
                        <InlineRichText :text="tabs.find(t => t.key === activeTab)?.description ?? ''" />
                    </p>
                </TooltipContent>
            </Tooltip>
            <button
                type="button"
                class="flex items-center gap-1.5 rounded-md px-2 py-0.5 instrument-serif text-sm transition-colors hover:bg-muted/50"
                :style="{ color: `var(--color-${tabs.find(t => t.key !== activeTab)?.color})` }"
                @click="emit('toggleTab')"
            >
                <Redo2 class="h-4.5 w-4.5" />
                <span>{{ tabs.find(t => t.key !== activeTab)?.label }}</span>
            </button>
        </div>
        <div class="relative flex-1 min-h-0 min-w-0">
            <!-- Tab content slots: first tab -->
            <div
                class="absolute inset-0 min-h-0 min-w-0 transition-opacity duration-fast"
                :class="activeTab === tabs[0].key ? 'z-10 opacity-100' : 'pointer-events-none z-0 opacity-0'"
            >
                <slot :name="tabs[0].key" />
            </div>
            <!-- Tab content slots: second tab -->
            <div
                class="absolute inset-0 min-h-0 min-w-0 transition-opacity duration-fast"
                :class="activeTab === tabs[1].key ? 'z-10 opacity-100' : 'pointer-events-none z-0 opacity-0'"
            >
                <slot :name="tabs[1].key" />
            </div>

            <!-- Language badge overlay -->
            <Transition name="hover-card">
                <div
                    v-if="showBbnfBadge"
                    class="absolute bottom-2 right-3 z-20 flex items-center gap-1.5 rounded-md border border-border/30 bg-card/60 px-2 py-0.5 text-xs text-muted-foreground backdrop-blur-sm font-mono"
                >
                    <BbnfLogo size="sm" shimmer />
                </div>
                <div
                    v-else-if="badgeLanguage && badgeLanguage !== 'plaintext' && langIcons"
                    class="absolute bottom-2 right-3 z-20 flex items-center gap-1.5 rounded-md border border-border/30 bg-card/60 px-2 py-0.5 text-xs text-muted-foreground backdrop-blur-sm font-mono"
                >
                    <BbnfLogo v-if="badgeLanguage === 'bbnf'" size="sm" shimmer />
                    <template v-else>
                        <img :src="langIcons[badgeLanguage] ?? langIcons.plaintext" alt="" class="h-3.5 w-3.5" />
                        <span>{{ badgeLanguage }}</span>
                    </template>
                </div>
            </Transition>

            <!-- Extra overlay slot (e.g. gorgeous badge, hover card) -->
            <slot name="overlay" />
        </div>
    </Card>
</template>

<style scoped>
.hover-card-enter-active,
.hover-card-leave-active {
    transition: opacity var(--duration-fast) var(--ease-smooth), transform var(--duration-fast) var(--ease-smooth);
}
.hover-card-enter-from,
.hover-card-leave-to {
    opacity: 0;
    transform: scale(0.92) translateY(6px);
}
</style>
