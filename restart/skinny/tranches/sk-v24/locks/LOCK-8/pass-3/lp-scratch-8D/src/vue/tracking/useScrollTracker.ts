import { ref, computed, watch, onMounted, onUnmounted, nextTick } from "vue";
import type { Ref } from "vue";
import type { TreeNode, TreeIndexEntry, ScrollTrackerOptions } from "./types";

/**
 * Tracks which tree node is currently visible via IntersectionObserver,
 * with a scroll-event fallback for fast scrollbar drags.
 * Deepest visible node wins.
 */
export function useScrollTracker<T extends TreeNode>(
    roots: T[],
    index: Map<string, TreeIndexEntry<T>>,
    visibleCount: Ref<number>,
    options?: ScrollTrackerOptions & {
        getChildren?: (node: T) => T[] | undefined;
        /** Scroll container for the scroll-event fallback. Falls back to document. */
        scrollContainer?: Ref<HTMLElement | null>;
        /** Sidebar element for auto-scrolling active TOC item. Uses data-toc-id attribute. */
        sidebarEl?: Ref<HTMLElement | null>;
    },
) {
    const getChildren =
        options?.getChildren ?? ((n: T) => n.children as T[] | undefined);
    const rootMargin = options?.rootMargin ?? "-20% 0px -60% 0px";
    const threshold = options?.threshold ?? 0;

    const activeId = ref<string | null>(roots[0]?.id ?? null);
    const sectionVisibility = new Map<string, boolean>();
    let observer: IntersectionObserver | null = null;
    const observedIds = new Set<string>();
    let locked = false;

    function lockTracking() {
        locked = true;
    }
    function unlockTracking() {
        locked = false;
    }

    const activeRootId = computed(() => {
        if (!activeId.value) return null;
        return index.get(activeId.value)?.parentId ?? null;
    });

    /** Walk the tree bottom-up: the deepest visible section wins. */
    function findDeepestVisible(list: T[]): string | null {
        for (const node of list) {
            const children = getChildren(node);
            if (children) {
                const deep = findDeepestVisible(children);
                if (deep) return deep;
            }
            if (sectionVisibility.get(node.id)) return node.id;
        }
        return null;
    }

    function updateActive() {
        const found = findDeepestVisible(roots);
        if (found) activeId.value = found;
    }

    /** Collect all node IDs in tree order (depth-first). Cached. */
    let cachedIds: string[] | null = null;
    function collectIds(list: T[]): string[] {
        if (cachedIds) return cachedIds;
        const out: string[] = [];
        function walk(nodes: T[]) {
            for (const node of nodes) {
                out.push(node.id);
                const children = getChildren(node);
                if (children) walk(children);
            }
        }
        walk(list);
        cachedIds = out;
        return out;
    }

    function invalidateIdCache() {
        cachedIds = null;
    }

    /**
     * Scroll-event fallback: find the section closest to the top of the
     * viewport. Runs on every scroll event (throttled via rAF) to catch
     * fast scrollbar drags that the IntersectionObserver misses.
     */
    let rafId = 0;
    function onScroll() {
        if (locked || rafId) return;
        rafId = requestAnimationFrame(() => {
            rafId = 0;
            const container = options?.scrollContainer?.value;
            // Parse rootMargin top percentage for the "active zone" offset
            const topPct = parseFloat(rootMargin.split(" ")[0]) / 100;
            const viewportH = container
                ? container.clientHeight
                : window.innerHeight;
            const activeZoneTop = Math.abs(topPct) * viewportH;
            const containerTop = container
                ? container.getBoundingClientRect().top
                : 0;

            const allIds = collectIds(roots);
            let bestId: string | null = null;
            let bestDist = Infinity;
            // Fallback: closest section below the active zone
            let closestBelowId: string | null = null;
            let closestBelowDist = Infinity;

            for (const id of allIds) {
                const el = document.getElementById(id);
                if (!el) continue;
                const rect = el.getBoundingClientRect();
                // Distance from top of element to the active zone line
                const dist = rect.top - containerTop - activeZoneTop;
                // Pick the section whose top is closest above or at the active zone
                if (dist <= 0 && Math.abs(dist) < bestDist) {
                    bestDist = Math.abs(dist);
                    bestId = id;
                }
                // Track closest section below the active zone as fallback
                if (dist > 0 && dist < closestBelowDist) {
                    closestBelowDist = dist;
                    closestBelowId = id;
                }
            }

            // If no section is above the active zone, use the closest one below
            const resolvedId = bestId ?? closestBelowId;
            if (resolvedId && resolvedId !== activeId.value) {
                sectionVisibility.clear();
                sectionVisibility.set(resolvedId, true);
                activeId.value = resolvedId;
            }
        });
    }

    function observeTree(list: T[]) {
        for (const node of list) {
            if (!observedIds.has(node.id)) {
                const el = document.getElementById(node.id);
                if (el) {
                    observer!.observe(el);
                    observedIds.add(node.id);
                }
            }
            const children = getChildren(node);
            if (children) observeTree(children);
        }
    }

    onMounted(() => {
        const container = options?.scrollContainer?.value;
        observer = new IntersectionObserver(
            (entries) => {
                if (locked) return;
                for (const entry of entries) {
                    sectionVisibility.set(
                        (entry.target as HTMLElement).id,
                        entry.isIntersecting,
                    );
                }
                updateActive();
            },
            { root: container ?? undefined, rootMargin, threshold },
        );
        nextTick(() => observeTree(roots));

        // Scroll-event fallback for fast scrollbar drags
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

    // Re-observe when new items mount from progressive loading
    watch(visibleCount, () => {
        invalidateIdCache();
        if (!observer) return;
        nextTick(() => observeTree(roots));
    });

    // Auto-scroll sidebar to keep active item visible
    if (options?.sidebarEl) {
        const sidebarEl = options.sidebarEl;
        watch(activeId, (id) => {
            if (!id || !sidebarEl.value) return;
            nextTick(() => {
                const el = sidebarEl.value?.querySelector(
                    `[data-toc-id="${id}"]`,
                ) as HTMLElement | null;
                if (el) {
                    el.scrollIntoView({ behavior: "smooth", block: "nearest" });
                }
            });
        });
    }

    /**
     * Clear stale visibility state and force position recalculation.
     * Runs the position-finding logic **synchronously** so that activeId
     * updates immediately (no rAF deferral).
     */
    function forceRecalculate() {
        sectionVisibility.clear();
        if (rafId) cancelAnimationFrame(rafId);
        rafId = 0;

        // Inline the same logic as onScroll, but synchronously
        const container = options?.scrollContainer?.value;
        const topPct = parseFloat(rootMargin.split(" ")[0]) / 100;
        const viewportH = container
            ? container.clientHeight
            : window.innerHeight;
        const activeZoneTop = Math.abs(topPct) * viewportH;
        const containerTop = container
            ? container.getBoundingClientRect().top
            : 0;

        const allIds = collectIds(roots);
        let bestId: string | null = null;
        let bestDist = Infinity;
        let closestBelowId: string | null = null;
        let closestBelowDist = Infinity;

        for (const id of allIds) {
            const el = document.getElementById(id);
            if (!el) continue;
            const rect = el.getBoundingClientRect();
            const dist = rect.top - containerTop - activeZoneTop;
            if (dist <= 0 && Math.abs(dist) < bestDist) {
                bestDist = Math.abs(dist);
                bestId = id;
            }
            if (dist > 0 && dist < closestBelowDist) {
                closestBelowDist = dist;
                closestBelowId = id;
            }
        }

        const resolvedId = bestId ?? closestBelowId;
        if (resolvedId) {
            sectionVisibility.set(resolvedId, true);
            activeId.value = resolvedId;
        }
    }

    return { activeId, activeRootId, forceRecalculate, lockTracking, unlockTracking };
}
