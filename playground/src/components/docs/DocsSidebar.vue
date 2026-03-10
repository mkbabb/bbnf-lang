<script setup lang="ts">
import { ref, computed } from "vue";
import { Search } from "lucide-vue-next";
import { useDocs } from "@/composables/useDocs";
import { getSectionTheme } from "@/composables/useSectionTheme";

defineProps<{
    currentSlug?: string;
}>();

const { sections } = useDocs();
const searchQuery = ref("");
const expandedSections = ref<Set<string>>(new Set(sections.value.map((s) => s.name)));

function toggleSection(name: string) {
    if (expandedSections.value.has(name)) {
        expandedSections.value.delete(name);
    } else {
        expandedSections.value.add(name);
    }
}

const filteredSections = computed(() => {
    const q = searchQuery.value.toLowerCase().trim();
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
        <!-- Search bar -->
        <div class="p-3 border-b border-border/20">
            <div class="relative">
                <Search class="absolute left-2.5 top-1/2 -translate-y-1/2 h-3.5 w-3.5 text-muted-foreground/50" />
                <input
                    v-model="searchQuery"
                    type="text"
                    placeholder="Search docs..."
                    class="w-full pl-8 pr-3 py-1.5 rounded-md bg-muted/20 border border-border/30 text-sm text-foreground placeholder:text-muted-foreground/40 outline-none focus:border-pastel-green/50 transition-colors"
                />
            </div>
        </div>

        <!-- Navigation -->
        <nav class="flex-1 overflow-y-auto p-3 scrollbar-hidden">
            <div v-for="section in filteredSections" :key="section.name" class="mb-5">
                <button
                    class="flex items-center gap-2 w-full px-2 py-1.5 group"
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
                <Transition name="section-expand">
                    <ul v-show="expandedSections.has(section.name)" class="space-y-0.5 mt-1">
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
.section-expand-enter-active,
.section-expand-leave-active {
    transition: all 0.2s ease;
    overflow: hidden;
}
.section-expand-enter-from,
.section-expand-leave-to {
    opacity: 0;
    max-height: 0;
}
</style>
