<script setup lang="ts">
import { ref, computed, onMounted, onBeforeUnmount } from "vue";
import { useRoute } from "vue-router";
import { HeaderRibbon } from "@/components/custom/header-ribbon";
import { DarkModeToggle } from "@/components/custom/dark-mode-toggle";
import { BbnfLogo } from "@/components/custom/bbnf-logo";
import { navIcons } from "@/composables/useSectionTheme";
import { useHeroState } from "@/composables/useHeroState";

const route = useRoute();
const scrollY = ref(0);

function onScroll() {
    scrollY.value = window.scrollY;
}

onMounted(() => {
    window.addEventListener("scroll", onScroll, { passive: true });
});
onBeforeUnmount(() => {
    window.removeEventListener("scroll", onScroll);
});

const isLanding = computed(() => route.path === "/");
const navOpaque = computed(() => !isLanding.value || scrollY.value > 40);

const { morphProgress } = useHeroState();
const hideNavLogo = computed(() => isLanding.value && morphProgress.value < 1);

/**
 * When the logo is hidden (landing, pre-scroll), pull it out of flow
 * so the nav links sit flush at the navbar's left padding — matching
 * the @mbabb right-side inset.  The logo element stays positioned for
 * scroll-morph measurement via `absolute`.
 */
const logoHiddenClass = computed(() =>
    hideNavLogo.value ? 'absolute opacity-0 pointer-events-none' : 'relative opacity-100',
);

const navLinks = [
    { to: "/playground", label: "Playground", icon: navIcons.playground },
    { to: "/docs", label: "Docs", icon: navIcons.docs },
];

function isActive(to: string) {
    return route.path.startsWith(to);
}
</script>

<template>
    <nav
        class="fixed top-0 left-0 right-0 z-50 h-14 flex items-center transition-[padding,background-color,border-color] duration-300 border-b"
        :class="[
            navOpaque
                ? 'backdrop-blur-xl bg-background/95 border-border/30 shadow-sm'
                : 'bg-transparent border-transparent',
            hideNavLogo ? 'px-4 sm:px-3' : 'px-3 sm:px-5',
        ]"
    >
        <!-- Left: Logo + nav links — always visible, no hamburger -->
        <div class="relative flex items-center gap-0">
            <router-link
                to="/"
                class="shrink-0 transition-opacity duration-300"
                :class="logoHiddenClass"
            >
                <span data-navbar-logo class="inline-block">
                    <BbnfLogo size="md" />
                </span>
            </router-link>

            <!-- Vertical separator — hidden when logo is out of flow -->
            <div
                v-if="!hideNavLogo"
                class="h-6 w-px bg-border/40 mx-2 sm:mx-4 transition-opacity duration-300"
            />

            <!-- Nav links — horizontally scrollable with edge fade -->
            <div class="nav-links-mask overflow-x-auto scrollbar-hidden">
                <div class="flex items-center gap-0.5 sm:gap-1 whitespace-nowrap">
                    <router-link
                        v-for="link in navLinks"
                        :key="link.to"
                        :to="link.to"
                        class="flex items-center gap-1 sm:gap-1.5 instrument-serif text-sm sm:text-base px-2 sm:px-3 py-1.5 rounded-md transition-colors shrink-0"
                        :class="isActive(link.to)
                            ? 'text-foreground'
                            : 'text-muted-foreground hover:text-foreground'"
                    >
                        <svg class="h-4 w-4 shrink-0" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                            <path :d="link.icon.iconPath" />
                            <path v-if="link.icon.iconPath2" :d="link.icon.iconPath2" />
                        </svg>
                        {{ link.label }}
                    </router-link>
                </div>
            </div>
        </div>

        <!-- Spacer -->
        <div class="flex-1" />
    </nav>

    <!-- Top-right ribbon with dark mode toggle -->
    <HeaderRibbon position="right" ref="ribbonRef">
        <template #items>
            <DarkModeToggle class="h-6 w-6" />
        </template>
        <template #anchor="{ toggled }">
            <div class="flex items-center cursor-pointer px-2 py-1 group">
                <span
                    class="instrument-serif text-lg text-muted-foreground transition-all duration-200 group-hover:text-foreground"
                    :class="toggled
                        ? 'text-foreground underline underline-offset-4 decoration-1 decoration-foreground/40'
                        : 'group-hover:underline group-hover:underline-offset-4 group-hover:decoration-1 group-hover:decoration-muted-foreground/40'"
                >@mbabb</span>
            </div>
        </template>
    </HeaderRibbon>
</template>

<style scoped>
.nav-links-mask {
    mask-image: linear-gradient(
        to right,
        transparent 0%,
        black 8px,
        black calc(100% - 8px),
        transparent 100%
    );
    -webkit-mask-image: linear-gradient(
        to right,
        transparent 0%,
        black 8px,
        black calc(100% - 8px),
        transparent 100%
    );
}
</style>
