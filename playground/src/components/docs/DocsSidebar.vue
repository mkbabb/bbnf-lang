<script setup lang="ts">
import { ref, computed, watch, nextTick } from "vue";
import { PanelLeftClose, ChevronRight } from "lucide-vue-next";
import { FuzzySearch, useFuzzySearch } from "@mkbabb/glass-ui/search";
import type { SearchableItem } from "@mkbabb/glass-ui/search";
import { buildTreeIndex, useScrollTracker } from "@mkbabb/glass-ui/sidebar";
import type { SidebarSection } from "@mkbabb/glass-ui/sidebar";
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
const navRef = ref<HTMLElement | null>(null);

// ── Height transition hooks ──────────────────────────────────────────────────

function onBeforeEnter(el: Element) {
    (el as HTMLElement).style.maxHeight = "0";
    (el as HTMLElement).style.overflow = "hidden";
}
function onEnter(el: Element) {
    nextTick(() => { (el as HTMLElement).style.maxHeight = el.scrollHeight + "px"; });
}
function onAfterEnter(el: Element) {
    (el as HTMLElement).style.maxHeight = "none";
    (el as HTMLElement).style.overflow = "";
}
function onBeforeLeave(el: Element) {
    (el as HTMLElement).style.maxHeight = el.scrollHeight + "px";
    (el as HTMLElement).style.overflow = "hidden";
}
function onLeave(el: Element) {
    requestAnimationFrame(() => { (el as HTMLElement).style.maxHeight = "0"; });
}
function onAfterLeave(el: Element) {
    (el as HTMLElement).style.maxHeight = "none";
    (el as HTMLElement).style.overflow = "";
}

// ── Section state ────────────────────────────────────────────────────────────

const { sections } = useDocs();

// Expand only the section containing the current page (BBNF by default)
const expandedSections = ref<Set<string>>(new Set((() => {
    if (props.currentSlug) {
        const match = sections.value.find((s) => s.docs.some((d) => d.slug === props.currentSlug));
        if (match) return [match.name];
    }
    return ["BBNF"];
})()));

// Track whether subsections are collapsed per doc (default: expanded for active)
const collapsedSubsections = ref<Set<string>>(new Set());

function toggleSubsections(slug: string) {
    if (collapsedSubsections.value.has(slug)) {
        collapsedSubsections.value.delete(slug);
    } else {
        collapsedSubsections.value.add(slug);
    }
}

// Which section contains the current page?
const currentSectionName = computed(() => {
    if (!props.currentSlug) return null;
    return sections.value.find((s) => s.docs.some((d) => d.slug === props.currentSlug))?.name ?? null;
});

// Auto-expand section when navigating to a new page
watch(() => props.currentSlug, (slug) => {
    if (!slug) return;
    const match = sections.value.find((s) => s.docs.some((d) => d.slug === slug));
    if (match && !expandedSections.value.has(match.name)) {
        expandedSections.value.add(match.name);
    }
    // Reset subsection collapse for the new doc
    collapsedSubsections.value.delete(slug);
});

function toggleSection(name: string) {
    const wasExpanded = expandedSections.value.has(name);
    if (wasExpanded) {
        expandedSections.value.delete(name);
    } else {
        expandedSections.value.add(name);
        // Auto-scroll the section header into view after expanding
        nextTick(() => {
            const el = navRef.value?.querySelector(`[data-section="${name}"]`);
            el?.scrollIntoView({ block: "nearest", behavior: "smooth" });
        });
    }
}

// ── Search ───────────────────────────────────────────────────────────────────

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

// ── Scroll-based active heading tracking ─────────────────────────────────────

// Build a flat SidebarSection[] from the current doc's headings for the scroll tracker.
// These IDs match the rendered heading elements' id attributes in the markdown.
const currentDoc = computed(() => {
    if (!props.currentSlug) return null;
    for (const s of sections.value) {
        const doc = s.docs.find((d) => d.slug === props.currentSlug);
        if (doc) return doc;
    }
    return null;
});

const headingSections = computed<SidebarSection[]>(() => {
    const doc = currentDoc.value;
    if (!doc || doc.headings.length === 0) return [];
    // Build a tree: ## headings are roots, ### are children of the preceding ##
    const roots: SidebarSection[] = [];
    for (const h of doc.headings) {
        if (h.level === 2) {
            roots.push({ id: h.id, title: h.text, children: [] });
        } else if (h.level === 3 && roots.length > 0) {
            roots[roots.length - 1]!.children!.push({ id: h.id, title: h.text });
        }
    }
    return roots;
});

const headingIndex = computed(() => buildTreeIndex(headingSections.value));

const { activeId: activeHeadingId } = useScrollTracker(headingSections, headingIndex);

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
    <aside class="flex h-full w-64 shrink-0 flex-col overflow-hidden border-r border-border/30 bg-card/40 backdrop-blur-xl rounded-r-xl">
        <!-- Search + controls -->
        <div class="px-2 py-1.5 border-b border-border/20 flex items-center gap-1.5">
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
            <button
                v-if="showClose"
                class="shrink-0 p-1.5 rounded-md hover:bg-muted/50 active:scale-90 transition-[colors,transform] text-muted-foreground"
                title="Close sidebar"
                @click="emit('close')"
            >
                <PanelLeftClose class="h-4 w-4" />
            </button>
        </div>

        <!-- Navigation -->
        <nav ref="navRef" class="flex-1 overflow-y-auto px-0 py-1 scrollbar-hidden">
            <div v-for="section in filteredSections" :key="section.name" class="mb-2" :data-section="section.name">
                <!-- Section header -->
                <button
                    class="flex items-center gap-2 w-full px-2 py-1.5 rounded-md transition-all active:scale-[0.98]"
                    :class="currentSectionName === section.name
                        ? 'bg-muted/40'
                        : 'hover:bg-muted/20'"
                    @click="toggleSection(section.name)"
                >
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
                    <ChevronRight
                        class="h-3 w-3 text-muted-foreground/40 transition-transform duration-200"
                        :class="expandedSections.has(section.name) ? 'rotate-90' : ''"
                    />
                </button>

                <!-- Doc list -->
                <Transition
                    @before-enter="onBeforeEnter" @enter="onEnter" @after-enter="onAfterEnter"
                    @before-leave="onBeforeLeave" @leave="onLeave" @after-leave="onAfterLeave"
                >
                    <ul v-show="expandedSections.has(section.name)" class="mt-0.5 section-expand-list">
                        <li v-for="doc in section.docs" :key="doc.slug">
                            <!-- Doc title -->
                            <router-link
                                :to="`/docs/${doc.slug}`"
                                class="block px-3 py-1 text-sm transition-all"
                                :class="currentSlug === doc.slug
                                    ? 'text-foreground font-medium'
                                    : 'text-muted-foreground hover:text-foreground hover:pl-4'"
                                :style="currentSlug === doc.slug
                                    ? `border-left: 2px solid var(--color-${getSectionTheme(section.name).color})`
                                    : 'border-left: 2px solid transparent'"
                            >
                                {{ doc.title }}
                            </router-link>

                            <!-- Subsection headings (collapsible, shown when doc is active) -->
                            <div v-if="currentSlug === doc.slug && doc.headings.length > 0">
                                <button
                                    class="flex items-center gap-1 px-3 py-0.5 text-[0.625rem] text-muted-foreground/50 hover:text-muted-foreground transition-colors"
                                    :style="`border-left: 2px solid color-mix(in srgb, var(--color-${getSectionTheme(section.name).color}) 20%, transparent)`"
                                    @click="toggleSubsections(doc.slug)"
                                >
                                    <ChevronRight
                                        class="h-2.5 w-2.5 transition-transform duration-150"
                                        :class="!collapsedSubsections.has(doc.slug) ? 'rotate-90' : ''"
                                    />
                                    <span>{{ doc.headings.length }} sections</span>
                                </button>
                                <Transition
                                    @before-enter="onBeforeEnter" @enter="onEnter" @after-enter="onAfterEnter"
                                    @before-leave="onBeforeLeave" @leave="onLeave" @after-leave="onAfterLeave"
                                >
                                    <ul v-show="!collapsedSubsections.has(doc.slug)" class="section-expand-list">
                                        <li v-for="h in doc.headings" :key="h.id">
                                            <a
                                                :href="`#${h.id}`"
                                                class="block py-0.5 text-xs transition-colors truncate"
                                                :class="[
                                                    h.level === 3 ? 'pl-8' : 'pl-5',
                                                    activeHeadingId === h.id
                                                        ? 'text-foreground font-medium'
                                                        : 'text-muted-foreground/60 hover:text-foreground',
                                                ]"
                                                :style="activeHeadingId === h.id
                                                    ? `border-left: 2px solid var(--color-${getSectionTheme(section.name).color})`
                                                    : `border-left: 2px solid color-mix(in srgb, var(--color-${getSectionTheme(section.name).color}) 20%, transparent)`"
                                            >
                                                {{ h.text }}
                                            </a>
                                        </li>
                                    </ul>
                                </Transition>
                            </div>
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
