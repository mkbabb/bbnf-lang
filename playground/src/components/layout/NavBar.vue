<script setup lang="ts">
import { ref, computed, onMounted, onBeforeUnmount, useTemplateRef } from "vue";
import { useRoute, useRouter } from "vue-router";
import { onClickOutside } from "@vueuse/core";
import { HeaderRibbon } from "@/components/custom/header-ribbon";
import { DarkModeToggle } from "@/components/custom/dark-mode-toggle";
import { BbnfLogo } from "@/components/custom/bbnf-logo";
import { ChevronDown, Home } from "lucide-vue-next";
import { navIcons } from "@/lib/sectionTheme";
import { useHeroState } from "@/composables/useHeroState";

const route = useRoute();
const router = useRouter();
const scrollY = ref(0);
const hoverCardOpen = ref(false);
const navDropdownOpen = ref(false);
const dropdownRef = useTemplateRef<HTMLElement>("dropdownRef");
let hoverTimer: ReturnType<typeof setTimeout> | null = null;

onClickOutside(dropdownRef, () => { navDropdownOpen.value = false; });

function onHoverEnter() {
    if (hoverTimer) { clearTimeout(hoverTimer); hoverTimer = null; }
    hoverCardOpen.value = true;
}
function onHoverLeave() {
    hoverTimer = setTimeout(() => { hoverCardOpen.value = false; }, 200);
}

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
/** True when the logo is not visually present in the navbar. */
const hideNavLogo = computed(() => isLanding.value && morphProgress.value < 0.5);

/**
 * Inline style for the logo+separator wrapper.
 * On landing: width is driven by morphProgress (clip-reveals the slot as scroll
 * progresses), pushing the dropdown right. The logo INSIDE stays invisible —
 * it's only a morph measurement target. The hero logo IS the visual element
 * that flies in. Once progress hits 1, the hero fades out and the navbar logo
 * fades in via navLogoOpacity.
 * On other pages: no constraint (natural width), logo fully visible.
 */
const NAV_LOGO_SLOT_REM = 5.75;
const logoSlotStyle = computed(() => {
    if (!isLanding.value) return {};
    return { width: `${morphProgress.value * NAV_LOGO_SLOT_REM}rem` };
});

/** Navbar logo opacity — invisible on landing (hero logo covers it), visible elsewhere. */
const navLogoOpacity = computed(() => {
    return isLanding.value ? 0 : 1;
});

const allNavLinks = [
    { to: "/", label: "Home", isHome: true },
    { to: "/playground", label: "Playground", icon: navIcons.playground },
    { to: "/docs", label: "Docs", icon: navIcons.docs },
];

const activeNavLink = computed(() => {
    if (route.path === "/") return allNavLinks[0]!;
    for (const link of allNavLinks) {
        if (link.to !== "/" && route.path.startsWith(link.to)) return link;
    }
    return allNavLinks[0]!;
});

function isActiveLink(link: typeof allNavLinks[0]) {
    if (link.to === "/") return route.path === "/";
    return route.path.startsWith(link.to);
}
</script>

<template>
    <nav
        class="fixed top-0 left-0 right-0 z-50 h-14 flex items-center transition-[padding,background-color,border-color] duration-300 border-b"
        :class="[
            navOpaque
                ? 'backdrop-blur-xl bg-background/95 border-border/50 shadow-sm'
                : 'bg-transparent border-transparent',
            hideNavLogo ? 'px-4 sm:px-3' : 'px-3 sm:px-5',
        ]"
    >
        <!-- Left: Logo + nav dropdown -->
        <div class="relative flex items-center gap-0">
            <!--
              Single logo+separator wrapper — always the same DOM.
              On landing: width driven by morphProgress (clip-reveals as scroll progresses).
              On other pages: unconstrained, takes natural width.
            -->
            <div
                class="shrink-0 flex items-center overflow-hidden"
                :style="logoSlotStyle"
            >
                <router-link
                    to="/"
                    class="shrink-0 transition-opacity duration-150"
                    :style="{ opacity: navLogoOpacity }"
                >
                    <span data-navbar-logo class="inline-block">
                        <BbnfLogo size="md" />
                    </span>
                </router-link>
                <div class="h-6 w-px bg-border mx-2 sm:mx-4 shrink-0" />
            </div>

            <!-- Nav dropdown -->
            <div ref="dropdownRef" class="relative">
                <button
                    class="flex items-center gap-1 sm:gap-1.5 instrument-serif text-sm sm:text-base px-2 sm:px-3 py-1.5 rounded-md transition-colors"
                    :class="navDropdownOpen
                        ? 'text-foreground bg-muted/30'
                        : 'text-muted-foreground hover:text-foreground'"
                    @click="navDropdownOpen = !navDropdownOpen"
                >
                    <template v-if="activeNavLink?.isHome">
                        <Home class="h-4 w-4 shrink-0" />
                        Home
                    </template>
                    <template v-else-if="activeNavLink?.icon">
                        <svg class="h-4 w-4 shrink-0" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                            <path :d="activeNavLink.icon.iconPath" />
                            <path v-if="activeNavLink.icon.iconPath2" :d="activeNavLink.icon.iconPath2" />
                        </svg>
                        {{ activeNavLink.label }}
                    </template>
                    <ChevronDown class="h-3.5 w-3.5 transition-transform duration-200" :class="navDropdownOpen ? 'rotate-180' : ''" />
                </button>

                <Transition name="nav-dropdown">
                    <div
                        v-if="navDropdownOpen"
                        class="absolute top-full left-0 mt-1.5 min-w-[11rem] rounded-lg border border-border/60 bg-card/95 backdrop-blur-xl shadow-lg p-1 z-50"
                    >
                        <router-link
                            v-for="link in allNavLinks"
                            :key="link.to"
                            :to="link.to"
                            class="flex items-center gap-2 px-3 py-2 rounded-md transition-colors instrument-serif text-sm"
                            :class="isActiveLink(link)
                                ? 'text-pastel-amber font-medium bg-pastel-amber/8'
                                : 'text-muted-foreground hover:text-foreground hover:bg-muted/30'"
                            @click="navDropdownOpen = false"
                        >
                            <Home v-if="link.isHome" class="h-4 w-4 shrink-0" />
                            <svg v-else-if="link.icon" class="h-4 w-4 shrink-0" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                <path :d="link.icon.iconPath" />
                                <path v-if="link.icon.iconPath2" :d="link.icon.iconPath2" />
                            </svg>
                            {{ link.label }}
                        </router-link>
                    </div>
                </Transition>
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
            <div
                class="relative flex items-center cursor-pointer px-2 py-1 group"
                @mouseenter="onHoverEnter"
                @mouseleave="onHoverLeave"
            >
                <span
                    class="instrument-serif text-lg text-muted-foreground transition-all duration-200 group-hover:text-foreground"
                    :class="toggled
                        ? 'text-foreground underline underline-offset-4 decoration-1 decoration-foreground/40'
                        : 'group-hover:underline group-hover:underline-offset-4 group-hover:decoration-1 group-hover:decoration-muted-foreground/40'"
                >@mbabb</span>

                <!-- Attribution hover card -->
                <div
                    class="attribution-card"
                    :class="{ 'is-open': hoverCardOpen }"
                    @mouseenter="onHoverEnter"
                    @mouseleave="onHoverLeave"
                >
                    <div class="flex items-center gap-3">
                        <img
                            src="https://avatars.githubusercontent.com/u/2848617?v=4"
                            alt="mkbabb"
                            class="h-10 w-10 rounded-full"
                        />
                        <div class="flex-1">
                            <a
                                href="https://github.com/mkbabb"
                                target="_blank"
                                rel="noopener noreferrer"
                                class="font-mono text-sm font-semibold text-foreground hover:underline"
                            >@mbabb</a>
                            <p class="mt-0.5 text-xs italic text-muted-foreground">Grammar-driven parsers &amp; formatters</p>
                        </div>
                    </div>
                    <hr class="my-2 border-border/50" />
                    <a
                        href="https://github.com/mkbabb/bbnf-lang"
                        target="_blank"
                        rel="noopener noreferrer"
                        class="block text-sm text-foreground hover:underline"
                    >View the project on GitHub 🎉</a>
                </div>
            </div>
        </template>
    </HeaderRibbon>
</template>

<style scoped>
.attribution-card {
    position: absolute;
    top: 100%;
    right: 0;
    margin-top: 0.25rem;
    padding: 1rem;
    background: color-mix(in srgb, hsl(var(--popover)) 80%, transparent);
    border: 1px solid hsl(var(--border) / 0.3);
    border-radius: 0.75rem;
    opacity: 0;
    pointer-events: none;
    transform: scale(0.92) translateY(6px);
    transition: all 150ms cubic-bezier(0.4, 0, 0.2, 1);
    z-index: 50;
    min-width: 16rem;
    white-space: normal;
}

.attribution-card::before {
    content: '';
    position: absolute;
    top: -0.5rem;
    left: 0;
    right: 0;
    height: 0.5rem;
}

.attribution-card.is-open {
    opacity: 1;
    pointer-events: auto;
    transform: scale(1) translateY(0);
    backdrop-filter: blur(12px);
    -webkit-backdrop-filter: blur(12px);
    box-shadow: 0 8px 32px hsl(var(--foreground) / 0.1);
}

.nav-dropdown-enter-active,
.nav-dropdown-leave-active {
    transition: opacity 0.15s ease, transform 0.15s ease;
}
.nav-dropdown-enter-from,
.nav-dropdown-leave-to {
    opacity: 0;
    transform: translateY(-4px) scale(0.95);
}
</style>
