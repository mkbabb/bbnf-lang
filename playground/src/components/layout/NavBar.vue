<script setup lang="ts">
import { ref, computed, onMounted, onBeforeUnmount } from "vue";
import { useRoute } from "vue-router";
import { HeaderRibbon } from "@/components/custom/header-ribbon";
import { DarkModeToggle } from "@/components/custom/dark-mode-toggle";
import { BbnfLogo } from "@/components/custom/bbnf-logo";
import { navIcons } from "@/lib/sectionTheme";
import { useHeroState } from "@/composables/useHeroState";

const route = useRoute();
const scrollY = ref(0);
const hoverCardOpen = ref(false);
let hoverTimer: ReturnType<typeof setTimeout> | null = null;

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

.nav-links-mask {
    --edge-fade: 0.5rem;
    mask-image: linear-gradient(
        to right,
        transparent 0%,
        black var(--edge-fade),
        black calc(100% - var(--edge-fade)),
        transparent 100%
    );
    -webkit-mask-image: linear-gradient(
        to right,
        transparent 0%,
        black var(--edge-fade),
        black calc(100% - var(--edge-fade)),
        transparent 100%
    );
}
</style>
