import { ref, watch, onMounted, onUnmounted, nextTick } from "vue";
import type { Ref } from "vue";
import type { LazyLoaderOptions } from "./types";

/**
 * Progressive content loading: mounts items in batches as the user scrolls.
 * Uses IntersectionObserver for normal scrolling, plus a scroll-event fallback
 * for fast scrollbar drags that skip past the sentinel.
 */
export function useLazyLoader(
    totalCount: number,
    options?: LazyLoaderOptions & {
        scrollContainer?: Ref<HTMLElement | null>;
    },
) {
    const batchSize = options?.batchSize ?? 2;
    const rootMargin = options?.rootMargin ?? "0px 0px 600px 0px";

    const visibleCount = ref(batchSize);
    const loadSentinel = ref<HTMLElement | null>(null);
    let observer: IntersectionObserver | null = null;
    let rafId = 0;

    function loadMore() {
        if (visibleCount.value < totalCount) {
            visibleCount.value = Math.min(
                visibleCount.value + batchSize,
                totalCount,
            );
        }
    }

    /**
     * Scroll-event fallback: if the sentinel is above the viewport
     * (user scrolled past it), force-load the next batch.
     */
    function onScroll() {
        if (rafId || visibleCount.value >= totalCount) return;
        rafId = requestAnimationFrame(() => {
            rafId = 0;
            if (!loadSentinel.value || visibleCount.value >= totalCount) return;
            const container = options?.scrollContainer?.value;
            const viewportBottom = container
                ? container.getBoundingClientRect().bottom
                : window.innerHeight;
            const sentinelRect = loadSentinel.value.getBoundingClientRect();
            // If sentinel is above the bottom of the viewport, we scrolled past it
            if (sentinelRect.top < viewportBottom) {
                loadMore();
            }
        });
    }

    onMounted(() => {
        const container = options?.scrollContainer?.value;
        observer = new IntersectionObserver(
            (entries) => {
                for (const entry of entries) {
                    if (entry.isIntersecting) loadMore();
                }
            },
            { root: container ?? undefined, rootMargin },
        );
        nextTick(() => {
            if (loadSentinel.value) observer!.observe(loadSentinel.value);
        });

        // Scroll fallback
        const scrollTarget = container ?? document;
        scrollTarget.addEventListener("scroll", onScroll, { passive: true });
    });

    onUnmounted(() => {
        observer?.disconnect();
        if (rafId) cancelAnimationFrame(rafId);
        const container = options?.scrollContainer?.value;
        const scrollTarget = container ?? document;
        scrollTarget.removeEventListener("scroll", onScroll);
    });

    // Re-observe the sentinel when it moves (after new items mount)
    watch(visibleCount, () => {
        if (!observer) return;
        observer.disconnect();
        nextTick(() => {
            if (loadSentinel.value) observer!.observe(loadSentinel.value);
        });
    });

    return { visibleCount, loadSentinel };
}
