<script setup lang="ts">
import { ref, computed, watch, nextTick } from "vue";
import { PanelLeftClose } from "lucide-vue-next";
import { FuzzySearch, useFuzzySearch } from "@mkbabb/glass-ui";
import type { SearchableItem } from "@mkbabb/glass-ui";
import { useDocs } from "@/composables/useDocs";
import { getSectionTheme } from "@/lib/sectionTheme";
import { useRouter } from "vue-router";

const props = defineProps<{
    currentSlug?: string;
    showClose?: boolean;
    showCollapse?: boolean;
}>();

const emit = defineEmits<{
    close: [];
    collapse: [];
}>();

const router = useRouter();

// JS-based height transition hooks
function onBeforeEnter(el: Element) {
    const htmlEl = el as HTMLElement;
    htmlEl.style.maxHeight = "0";
    htmlEl.style.overflow = "hidden";
}
function onEnter(el: Element) {
    const htmlEl = el as HTMLElement;
    nextTick(() => {
        htmlEl.style.maxHeight = htmlEl.scrollHeight + "px";
    });
}
function onAfterEnter(el: Element) {
    const htmlEl = el as HTMLElement;
    htmlEl.style.maxHeight = "none";
    htmlEl.style.overflow = "";
}
function onBeforeLeave(el: Element) {
    const htmlEl = el as HTMLElement;
    htmlEl.style.maxHeight = htmlEl.scrollHeight + "px";
    htmlEl.style.overflow = "hidden";
}
function onLeave(el: Element) {
    const htmlEl = el as HTMLElement;
    requestAnimationFrame(() => {
        htmlEl.style.maxHeight = "0";
    });
}
function onAfterLeave(el: Element) {
    const htmlEl = el as HTMLElement;
    htmlEl.style.maxHeight = "none";
    htmlEl.style.overflow = "";
}

const { sections } = useDocs();
// Expand only the section containing the current page (BBNF by default)
const expandedSections = ref<Set<string>>(new Set((() => {
    if (props.currentSlug) {
        const match = sections.value.find((s) => s.docs.some((d) => d.slug === props.currentSlug));
        if (match) return [match.name];
    }
    return ["BBNF"];
})()));

// Auto-expand section when navigating to a new page
watch(() => props.currentSlug, (slug) => {
    if (!slug) return;
    const match = sections.value.find((s) => s.docs.some((d) => d.slug === slug));
    if (match && !expandedSections.value.has(match.name)) {
        expandedSections.value.add(match.name);
    }
});

function toggleSection(name: string) {
    if (expandedSections.value.has(name)) {
        expandedSections.value.delete(name);
    } else {
        expandedSections.value.add(name);
    }
}

// Fuzzy search over all docs
const searchItems = computed<SearchableItem[]>(() =>
    sections.value.flatMap((s) =>
        s.docs.map((d) => ({
            id: d.slug,
            label: d.title,
            text: d.content,
            type: s.name,
        }))
    )
);

const searchState = useFuzzySearch({
    items: () => searchItems.value,
    debounceMs: 150,
    maxResults: 20,
    onSelect: (result) => {
        router.push(`/docs/${result.item.id}`);
    },
});

// Filter sections based on active search query
const filteredSections = computed(() => {
    const q = searchState.query.value.toLowerCase().trim();
    if (!q) return sections.value;
    return sections.value
        .map((section) => ({
            ...section,
            docs: section.docs.filter((doc) =>
                doc.title.toLowerCase().includes(q) ||
                doc.content.toLowerCase().includes(q)
            ),
        }))
        .filter((section) => section.docs.length > 0);
});
</script>

<template>
    <aside class="flex h-full w-64 shrink-0 flex-col overflow-y-auto border-r border-border/30 bg-card/40 backdrop-blur-xl scrollbar-hidden">
        <!-- Search bar + collapse toggle -->
        <div class="px-2 py-1.5 border-b border-border/20">
            <div class="flex items-center gap-1.5">
                <div class="flex-1 min-w-0">
                    <FuzzySearch
                        :state="searchState"
                        variant="sidebar"
                        placeholder="Search docs..."
                        :type-label="(item: SearchableItem) => item.type ?? ''"
                    />
                </div>
                <button
                    v-if="showCollapse"
                    class="shrink-0 p-1.5 rounded-md hover:bg-muted/50 active:scale-90 transition-[colors,transform] text-muted-foreground"
                    title="Collapse sidebar"
                    @click="emit('collapse')"
                >
                    <PanelLeftClose class="h-4 w-4" />
                </button>
            </div>
        </div>

        <!-- Navigation -->
        <nav class="flex-1 overflow-y-auto px-0 py-1 scrollbar-hidden">
            <div v-for="section in filteredSections" :key="section.name" class="mb-4">
                <button
                    class="flex items-center gap-2 w-full px-2 py-1.5 group active:scale-[0.98] transition-transform"
                    @click="toggleSection(section.name)"
                >
                    <!-- Section icon -->
                    <img
                        v-if="getSectionTheme(section.name).iconSrc"
                        :src="getSectionTheme(section.name).iconSrc"
                        alt=""
                        class="h-3.5 w-3.5 shrink-0"
                    />
                    <svg
                        v-else
                        class="h-3.5 w-3.5 shrink-0 transition-colors"
                        :style="{ color: `var(--color-${getSectionTheme(section.name).color})` }"
                        viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"
                    >
                        <path :d="getSectionTheme(section.name).iconPath" />
                        <path v-if="getSectionTheme(section.name).iconPath2" :d="getSectionTheme(section.name).iconPath2" />
                    </svg>
                    <h3
                        class="instrument-serif text-xs uppercase tracking-wider flex-1 text-left transition-colors"
                        :style="{ color: `var(--color-${getSectionTheme(section.name).color})` }"
                    >
                        {{ section.name }}
                    </h3>
                    <svg
                        class="h-3 w-3 text-muted-foreground/40 transition-transform duration-200"
                        :class="expandedSections.has(section.name) ? 'rotate-90' : ''"
                        viewBox="0 0 12 12" fill="none" stroke="currentColor" stroke-width="2"
                    >
                        <path d="M4 2l4 4-4 4" />
                    </svg>
                </button>
                <Transition
                    @before-enter="onBeforeEnter"
                    @enter="onEnter"
                    @after-enter="onAfterEnter"
                    @before-leave="onBeforeLeave"
                    @leave="onLeave"
                    @after-leave="onAfterLeave"
                >
                    <ul v-show="expandedSections.has(section.name)" class="space-y-0.5 mt-1 section-expand-list">
                        <li v-for="doc in section.docs" :key="doc.slug">
                            <router-link
                                :to="`/docs/${doc.slug}`"
                                class="block px-3 py-1.5 text-sm transition-all"
                                :class="currentSlug === doc.slug
                                    ? 'sidebar-active text-foreground'
                                    : 'text-muted-foreground hover:text-foreground hover:pl-4'"
                                :style="currentSlug === doc.slug
                                    ? `border-left: 2px solid var(--color-${getSectionTheme(section.name).color})`
                                    : 'border-left: 2px solid transparent'"
                            >
                                {{ doc.title }}
                            </router-link>
                        </li>
                    </ul>
                </Transition>
            </div>
            <p v-if="filteredSections.length === 0" class="text-xs text-muted-foreground/50 px-2 italic">
                No results
            </p>
        </nav>
    </aside>
</template>

<style scoped>
.section-expand-list {
    transition: max-height 0.2s ease;
}
</style>
