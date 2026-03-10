<script setup lang="ts">
import { ref, computed, watch } from "vue";
import { useRouter } from "vue-router";
import DocsSidebar from "@/components/docs/DocsSidebar.vue";
import { useDocs } from "@/composables/useDocs";
import { useMarkdown } from "@/composables/useMarkdown";
import { getSectionTheme } from "@/composables/useSectionTheme";
import { PanelLeftClose, PanelLeftOpen, Menu, X } from "lucide-vue-next";

const props = defineProps<{
    slug?: string;
}>();

const router = useRouter();
const { allDocs, getDoc } = useDocs();
const { renderMarkdown } = useMarkdown();

// Sidebar state
const sidebarOpen = ref(true);       // desktop: collapsed/expanded
const mobileDrawer = ref(false);     // mobile: drawer open/closed

// Redirect to first doc if no slug
watch(
    () => props.slug,
    (slug) => {
        if (!slug && allDocs.length > 0) {
            router.replace(`/docs/${allDocs[0]!.slug}`);
        }
    },
    { immediate: true },
);

// Close mobile drawer on navigation
watch(() => props.slug, () => { mobileDrawer.value = false; });

const currentDoc = computed(() => props.slug ? getDoc(props.slug) : undefined);
const rendered = computed(() => currentDoc.value ? renderMarkdown(currentDoc.value.content) : "");
const sectionTheme = computed(() => currentDoc.value ? getSectionTheme(currentDoc.value.section) : null);
</script>

<template>
    <div class="mt-14 flex min-h-[calc(100dvh-3.5rem)]">
        <!-- Desktop sidebar — collapsible -->
        <div
            class="sticky top-14 hidden h-[calc(100dvh-3.5rem)] shrink-0 self-start overflow-hidden transition-[width] duration-200 md:block"
            :style="{ width: sidebarOpen ? '16rem' : '0' }"
        >
            <DocsSidebar :current-slug="slug" />
        </div>

        <!-- Mobile drawer overlay -->
        <Transition name="mobile-drawer">
            <div
                v-if="mobileDrawer"
                class="fixed inset-0 z-50 md:hidden bg-black/20"
                @click.self="mobileDrawer = false"
            >
                <div class="absolute top-14 left-0 w-72 h-[calc(100dvh-3.5rem)] bg-card/95 backdrop-blur-xl border-r border-border/30 shadow-lg flex flex-col">
                    <div class="flex items-center justify-between px-4 py-2 border-b border-border/20 shrink-0">
                        <span class="instrument-serif text-sm text-muted-foreground">Navigation</span>
                        <button
                            class="p-1.5 rounded-md hover:bg-muted/50 transition-colors text-muted-foreground"
                            @click="mobileDrawer = false"
                        >
                            <X class="h-5 w-5 transition-transform duration-200" />
                        </button>
                    </div>
                    <div class="flex-1 overflow-y-auto">
                        <DocsSidebar :current-slug="slug" />
                    </div>
                </div>
            </div>
        </Transition>

        <!-- Main content -->
        <main class="flex-1 flex min-h-[calc(100dvh-3.5rem)] flex-col px-4 py-4 sm:px-8 sm:py-6 min-w-0">
            <div v-if="currentDoc" class="mx-auto flex w-full max-w-4xl flex-1 flex-col">
                <!-- Card-styled article container -->
                <div
                    class="rounded-xl border bg-card/80 backdrop-blur-sm p-5 sm:p-8 md:p-10 flex-1 relative"
                    :style="{
                        borderColor: sectionTheme
                            ? `color-mix(in srgb, var(--color-${sectionTheme.color}) 25%, hsl(var(--border) / 0.4))`
                            : undefined,
                        boxShadow: '3px 3px 0px 0px rgba(0,0,0,0.08)',
                    }"
                >
                    <!-- Sidebar toggle — top-right of card, sticky -->
                    <div class="float-right sticky top-4 z-20 ml-3 mb-2">
                        <!-- Mobile: menu button -->
                        <button
                            class="md:hidden p-1.5 rounded-md hover:bg-muted/50 transition-colors text-muted-foreground"
                            @click="mobileDrawer = !mobileDrawer"
                        >
                            <Menu class="h-4 w-4" />
                        </button>
                        <!-- Desktop: collapse/expand -->
                        <button
                            class="hidden md:flex items-center gap-1.5 p-1.5 rounded-md hover:bg-muted/50 transition-colors text-muted-foreground"
                            @click="sidebarOpen = !sidebarOpen"
                        >
                            <component :is="sidebarOpen ? PanelLeftClose : PanelLeftOpen" class="h-4 w-4" />
                        </button>
                    </div>

                    <!-- Section badge -->
                    <div
                        v-if="sectionTheme"
                        class="inline-flex items-center gap-1.5 rounded-full px-2.5 py-0.5 text-[10px] font-mono uppercase tracking-wider mb-4"
                        :style="{
                            color: `var(--color-${sectionTheme.color})`,
                            background: `color-mix(in srgb, var(--color-${sectionTheme.color}) 10%, transparent)`,
                            border: `1px solid color-mix(in srgb, var(--color-${sectionTheme.color}) 20%, transparent)`,
                        }"
                    >
                        <img
                            v-if="sectionTheme.iconSrc"
                            :src="sectionTheme.iconSrc"
                            alt=""
                            class="h-3 w-3"
                        />
                        <svg v-else class="h-3 w-3" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                            <path :d="sectionTheme.iconPath" />
                            <path v-if="sectionTheme.iconPath2" :d="sectionTheme.iconPath2" />
                        </svg>
                        {{ currentDoc.section }}
                    </div>
                    <article class="prose max-w-none" v-html="rendered" />
                </div>
            </div>
            <div v-else class="text-center text-muted-foreground pt-20">
                <p class="instrument-serif text-xl">Select a topic from the sidebar</p>
            </div>
        </main>
    </div>
</template>

<style scoped>
.dark div[style*="boxShadow"] {
    box-shadow: 3px 3px 0px 0px rgba(200, 200, 255, 0.06) !important;
}

.mobile-drawer-enter-active,
.mobile-drawer-leave-active {
    transition: opacity 0.2s ease;
}
.mobile-drawer-enter-active > div,
.mobile-drawer-leave-active > div {
    transition: transform 0.25s cubic-bezier(0.4, 0, 0.2, 1);
}
.mobile-drawer-enter-from,
.mobile-drawer-leave-to {
    opacity: 0;
}
.mobile-drawer-enter-from > div,
.mobile-drawer-leave-to > div {
    transform: translateX(-100%);
}
</style>
