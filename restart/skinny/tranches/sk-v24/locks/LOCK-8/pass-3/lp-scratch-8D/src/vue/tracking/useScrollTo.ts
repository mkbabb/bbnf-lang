import { nextTick } from "vue";
import type { ScrollToOptions } from "./types";

/**
 * Scroll-to-element with rAF retry.
 * When a treeIndex is provided, loads only up to the target section's root index + buffer
 * instead of force-loading all content.
 */
export function useScrollTo(options: ScrollToOptions) {
    const { scrollContainer, totalCount, visibleCount } = options;
    const scrollOffset = options.scrollOffset ?? 16;
    const maxAttempts = options.maxAttempts ?? 60;
    const treeIndex = options.treeIndex;

    function ensureTargetLoaded(id: string) {
        if (treeIndex) {
            const entry = treeIndex.get(id);
            if (entry) {
                // Load up to the target's root section + 1 buffer
                const needed = entry.rootIndex + 2;
                visibleCount.value = Math.max(
                    visibleCount.value,
                    Math.min(needed, totalCount),
                );
                return;
            }
        }
        // Fallback: load everything
        visibleCount.value = totalCount;
    }

    function scrollTo(id: string) {
        ensureTargetLoaded(id);

        let attempts = 0;
        let lastY = -1;

        function tryScroll() {
            const el = document.getElementById(id);
            const scroller = scrollContainer.value;
            if (!el || !scroller) {
                if (attempts++ < maxAttempts) requestAnimationFrame(tryScroll);
                return;
            }

            const scrollerRect = scroller.getBoundingClientRect();
            const elRect = el.getBoundingClientRect();
            const absoluteTop =
                elRect.top - scrollerRect.top + scroller.scrollTop;

            if (Math.abs(absoluteTop - lastY) < 1 && attempts > 2) {
                scroller.scrollTo({
                    top: Math.max(0, absoluteTop - scrollOffset),
                    behavior: "smooth",
                });
                return;
            }
            lastY = absoluteTop;
            attempts++;
            requestAnimationFrame(tryScroll);
        }
        nextTick(() => requestAnimationFrame(tryScroll));
    }

    return { scrollTo };
}
