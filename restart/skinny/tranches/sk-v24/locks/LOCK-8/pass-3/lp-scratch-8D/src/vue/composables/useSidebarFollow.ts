import { nextTick, onMounted, onUnmounted, watch } from "vue";
import type { Ref } from "vue";

export interface SidebarFollowOptions {
    sidebarEl: Ref<HTMLElement | null>;
    activeId: Ref<string | null>;
    activeRootId?: Ref<string | null>;
    scrollSource?: Ref<HTMLElement | null>;
    damping?: number;
}

export function useSidebarFollow(options: SidebarFollowOptions) {
    const damping = options.damping ?? 0.22;
    let followRaf = 0;
    let syncRaf = 0;
    let currentScrollSource: HTMLElement | null = null;
    let currentSidebar: HTMLElement | null = null;
    let targetScrollTop: number | null = null;
    let manualOverride = false;
    let programmaticScrollDepth = 0;

    function suspendForManualInteraction() {
        manualOverride = true;
        targetScrollTop = null;
        if (followRaf) {
            cancelAnimationFrame(followRaf);
            followRaf = 0;
        }
        if (syncRaf) {
            cancelAnimationFrame(syncRaf);
            syncRaf = 0;
        }
    }

    function escapeSelector(value: string): string {
        if (typeof CSS !== "undefined" && typeof CSS.escape === "function") {
            return CSS.escape(value);
        }
        return value.replace(/["\\]/g, "\\$&");
    }

    function getActiveElement(): HTMLElement | null {
        const nav = options.sidebarEl.value;
        const id = options.activeId.value ?? options.activeRootId?.value ?? null;
        if (!nav || !id) return null;
        return nav.querySelector(
            `[data-toc-id="${escapeSelector(id)}"]`,
        ) as HTMLElement | null;
    }

    function resolveTarget(nav: HTMLElement, activeEl: HTMLElement): number {
        const navHeight = nav.clientHeight;
        const elementCenter = activeEl.offsetTop + activeEl.offsetHeight / 2;
        const maxScrollTop = Math.max(0, nav.scrollHeight - navHeight);
        const deadzone = navHeight * 0.18;
        const currentCenter = nav.scrollTop + navHeight / 2;
        if (Math.abs(elementCenter - currentCenter) <= deadzone) {
            return nav.scrollTop;
        }
        return Math.max(0, Math.min(maxScrollTop, elementCenter - navHeight / 2));
    }

    function withProgrammaticScroll(fn: () => void) {
        programmaticScrollDepth += 1;
        try {
            fn();
        } finally {
            requestAnimationFrame(() => {
                programmaticScrollDepth = Math.max(0, programmaticScrollDepth - 1);
            });
        }
    }

    function follow() {
        followRaf = 0;
        if (manualOverride) return;
        const nav = options.sidebarEl.value;
        const target = targetScrollTop;
        if (!nav || target == null) return;

        const delta = target - nav.scrollTop;
        if (Math.abs(delta) < 1) {
            withProgrammaticScroll(() => {
                nav.scrollTop = target;
            });
            targetScrollTop = null;
            return;
        }

        withProgrammaticScroll(() => {
            nav.scrollTop += delta * damping;
        });
        followRaf = requestAnimationFrame(follow);
    }

    function queue(immediate = false) {
        if (!immediate && manualOverride) return;
        const nav = options.sidebarEl.value;
        const activeEl = getActiveElement();
        if (!nav || !activeEl) return;

        targetScrollTop = resolveTarget(nav, activeEl);

        if (immediate) {
            if (followRaf) {
                cancelAnimationFrame(followRaf);
                followRaf = 0;
            }
            withProgrammaticScroll(() => {
                nav.scrollTop = targetScrollTop!;
            });
            targetScrollTop = null;
            return;
        }

        if (!followRaf) {
            followRaf = requestAnimationFrame(follow);
        }
    }

    function scheduleFromScroll() {
        if (manualOverride) {
            manualOverride = false;
            targetScrollTop = null;
        }
        if (syncRaf) return;
        syncRaf = requestAnimationFrame(() => {
            syncRaf = 0;
            queue();
        });
    }

    function handleSidebarWheel() {
        suspendForManualInteraction();
    }

    function handleSidebarTouch() {
        suspendForManualInteraction();
    }

    function handleSidebarPointer(event: PointerEvent) {
        const target = event.target as HTMLElement | null;
        if (target?.closest("[data-toc-id], .sidebar-top-btn")) return;
        suspendForManualInteraction();
    }

    function handleSidebarKeydown(event: KeyboardEvent) {
        if (
            event.key === "ArrowUp" ||
            event.key === "ArrowDown" ||
            event.key === "PageUp" ||
            event.key === "PageDown" ||
            event.key === "Home" ||
            event.key === "End" ||
            event.key === " "
        ) {
            suspendForManualInteraction();
        }
    }

    function handleSidebarScroll() {
        if (programmaticScrollDepth > 0) return;
        suspendForManualInteraction();
    }

    function bindSidebar(nav: HTMLElement | null) {
        if (currentSidebar === nav) return;
        currentSidebar?.removeEventListener("wheel", handleSidebarWheel);
        currentSidebar?.removeEventListener("scroll", handleSidebarScroll);
        currentSidebar?.removeEventListener("touchstart", handleSidebarTouch);
        currentSidebar?.removeEventListener("pointerdown", handleSidebarPointer);
        currentSidebar?.removeEventListener("keydown", handleSidebarKeydown);
        currentSidebar = nav;
        currentSidebar?.addEventListener("wheel", handleSidebarWheel, { passive: true });
        currentSidebar?.addEventListener("scroll", handleSidebarScroll, { passive: true });
        currentSidebar?.addEventListener("touchstart", handleSidebarTouch, { passive: true });
        currentSidebar?.addEventListener("pointerdown", handleSidebarPointer, { passive: true });
        currentSidebar?.addEventListener("keydown", handleSidebarKeydown);
    }

    function bindScrollSource(source: HTMLElement | null) {
        if (currentScrollSource === source) return;
        currentScrollSource?.removeEventListener("scroll", scheduleFromScroll);
        currentScrollSource = source;
        currentScrollSource?.addEventListener("scroll", scheduleFromScroll, {
            passive: true,
        });
    }

    onMounted(() => {
        bindScrollSource(options.scrollSource?.value ?? null);
        bindSidebar(options.sidebarEl.value ?? null);
        nextTick(() => queue(true));
        window.addEventListener("resize", scheduleFromScroll);
    });

    watch(
        [options.activeId, options.activeRootId ?? { value: null }],
        () => {
            nextTick(() => queue());
        },
        { flush: "post" },
    );

    watch(
        options.sidebarEl,
        (sidebar) => {
            bindSidebar(sidebar);
        },
        { immediate: true },
    );

    watch(
        () => options.scrollSource?.value ?? null,
        (source) => {
            bindScrollSource(source);
        },
        { immediate: true },
    );

    onUnmounted(() => {
        if (followRaf) cancelAnimationFrame(followRaf);
        if (syncRaf) cancelAnimationFrame(syncRaf);
        currentScrollSource?.removeEventListener("scroll", scheduleFromScroll);
        currentSidebar?.removeEventListener("wheel", handleSidebarWheel);
        currentSidebar?.removeEventListener("scroll", handleSidebarScroll);
        currentSidebar?.removeEventListener("touchstart", handleSidebarTouch);
        currentSidebar?.removeEventListener("pointerdown", handleSidebarPointer);
        currentSidebar?.removeEventListener("keydown", handleSidebarKeydown);
        window.removeEventListener("resize", scheduleFromScroll);
    });

    return { queueSidebarFollow: queue };
}
