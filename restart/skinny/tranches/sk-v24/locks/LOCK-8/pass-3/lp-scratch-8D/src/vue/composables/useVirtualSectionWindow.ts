import {
    computed,
    onUnmounted,
    ref,
    shallowRef,
    toValue,
    watch,
    type MaybeRefOrGetter,
    type Ref,
} from "vue";
import type { FlatPaperSection } from "../../paper/flattenPaperSections";
import {
    buildSectionLayout,
    findSectionOffset,
    resolveActiveSection,
    resolveSectionWindow,
    type ForcedSectionWindowRange,
    type SectionLayout,
    type SectionWindowRange,
} from "./virtualSectionLayout";

export interface VirtualSectionWindowOptions<T extends FlatPaperSection = FlatPaperSection> {
    items: MaybeRefOrGetter<readonly T[]>;
    scrollContainer: Ref<HTMLElement | null>;
    overscanBeforePx?: number;
    overscanAfterPx?: number;
    warmTargetBefore?: number;
    warmTargetAfter?: number;
    leadingOffsetPx?: MaybeRefOrGetter<number>;
}

const SESSION_HEIGHT_CACHE = new Map<string, number>();

export function useVirtualSectionWindow<T extends FlatPaperSection>(
    options: VirtualSectionWindowOptions<T>,
) {
    const items = computed(() => Array.from(toValue(options.items)));
    const measuredHeights = new Map<string, number>();
    const itemIndex = new Map<string, number>();
    const layout = shallowRef<SectionLayout<T>>({
        entries: [],
        totalHeight: 0,
    });
    const range = ref<SectionWindowRange>({
        startIndex: 0,
        endIndex: -1,
        topSpacerPx: 0,
        bottomSpacerPx: 0,
    });
    const activeItem = ref<T | null>(null);
    const warmRange = ref<ForcedSectionWindowRange | null>(null);

    const elementMap = new Map<string, HTMLElement>();
    let scrollRaf = 0;
    let recalcRaf = 0;
    let warmTimer = 0;
    let containerResizeObserver: ResizeObserver | null = null;

    function getViewportHeight(): number {
        return Math.max(
            1,
            options.scrollContainer.value?.clientHeight ?? window.innerHeight ?? 900,
        );
    }

    function getLeadingOffset(): number {
        return Math.max(0, toValue(options.leadingOffsetPx) ?? 0);
    }

    function getHeight(item: T): number {
        return (
            measuredHeights.get(item.id) ??
            SESSION_HEIGHT_CACHE.get(item.id) ??
            item.estimatedHeight
        );
    }

    function rebuildLayout() {
        layout.value = buildSectionLayout(items.value, getHeight);
    }

    function computeWindowState() {
        const container = options.scrollContainer.value;
        const viewportHeight = getViewportHeight();
        const overscanBeforePx = options.overscanBeforePx ?? viewportHeight;
        const overscanAfterPx = options.overscanAfterPx ?? viewportHeight * 2;
        const normalizedScrollTop = Math.max(
            0,
            (container?.scrollTop ?? 0) - getLeadingOffset(),
        );

        range.value = resolveSectionWindow(
            layout.value,
            normalizedScrollTop,
            viewportHeight,
            overscanBeforePx,
            overscanAfterPx,
            warmRange.value,
        );
        activeItem.value = resolveActiveSection(
            layout.value,
            normalizedScrollTop + viewportHeight * 0.2,
        );
    }

    function recalculate() {
        rebuildLayout();
        computeWindowState();
    }

    function scheduleRecalculate() {
        if (recalcRaf) return;
        recalcRaf = requestAnimationFrame(() => {
            recalcRaf = 0;
            recalculate();
        });
    }

    function scheduleWarmRangeRelease() {
        if (warmTimer) window.clearTimeout(warmTimer);
        warmTimer = window.setTimeout(() => {
            warmRange.value = null;
            scheduleRecalculate();
        }, 320);
    }

    function syncMeasuredHeight(id: string, height: number) {
        const normalized = Math.max(1, Math.round(height));
        if (measuredHeights.get(id) === normalized) return;
        measuredHeights.set(id, normalized);
        SESSION_HEIGHT_CACHE.set(id, normalized);
        scheduleWarmRangeRelease();
        scheduleRecalculate();
    }

    function disconnectSection(id: string) {
        elementMap.delete(id);
    }

    function measureSection(id: string, el: HTMLElement | null) {
        if (!el) {
            disconnectSection(id);
            return;
        }

        const current = elementMap.get(id);
        if (current === el) {
            syncMeasuredHeight(id, el.offsetHeight);
            return;
        }

        disconnectSection(id);
        elementMap.set(id, el);
        requestAnimationFrame(() => {
            const target = elementMap.get(id);
            if (target) syncMeasuredHeight(id, target.offsetHeight);
        });
    }

    function ensureTargetWindow(id: string) {
        const index = itemIndex.get(id);
        if (index == null) return;
        const warmBefore = options.warmTargetBefore ?? 2;
        const warmAfter = options.warmTargetAfter ?? 3;
        warmRange.value = {
            startIndex: Math.max(0, index - warmBefore),
            endIndex: Math.min(items.value.length - 1, index + warmAfter),
        };
        scheduleWarmRangeRelease();
        recalculate();
    }

    function getOffsetFor(id: string): number | null {
        return findSectionOffset(layout.value, id);
    }

    function attachContainerObserver(container: HTMLElement | null) {
        containerResizeObserver?.disconnect();
        containerResizeObserver = null;
        if (!container || typeof ResizeObserver === "undefined") return;
        containerResizeObserver = new ResizeObserver(() => {
            scheduleRecalculate();
        });
        containerResizeObserver.observe(container);
    }

    function handleScroll() {
        if (scrollRaf) return;
        scrollRaf = requestAnimationFrame(() => {
            scrollRaf = 0;
            computeWindowState();
        });
    }

    let currentContainer: HTMLElement | null = null;
    function bindContainer(container: HTMLElement | null) {
        if (currentContainer === container) return;
        currentContainer?.removeEventListener("scroll", handleScroll);
        currentContainer = container;
        currentContainer?.addEventListener("scroll", handleScroll, { passive: true });
        attachContainerObserver(container);
        scheduleRecalculate();
    }

    watch(
        items,
        (nextItems) => {
            itemIndex.clear();
            for (const item of nextItems) {
                itemIndex.set(item.id, item.index);
                const cached = SESSION_HEIGHT_CACHE.get(item.id);
                if (cached != null) measuredHeights.set(item.id, cached);
            }
            for (const id of [...measuredHeights.keys()]) {
                if (!itemIndex.has(id)) measuredHeights.delete(id);
            }
            recalculate();
        },
        { immediate: true },
    );

    watch(
        options.scrollContainer,
        (container) => bindContainer(container),
        { immediate: true },
    );

    watch(
        () => toValue(options.leadingOffsetPx),
        () => scheduleRecalculate(),
    );

    onUnmounted(() => {
        if (scrollRaf) cancelAnimationFrame(scrollRaf);
        if (recalcRaf) cancelAnimationFrame(recalcRaf);
        if (warmTimer) window.clearTimeout(warmTimer);
        currentContainer?.removeEventListener("scroll", handleScroll);
        containerResizeObserver?.disconnect();
        elementMap.clear();
    });

    const visibleItems = computed(() => {
        if (range.value.endIndex < range.value.startIndex) return [] as T[];
        return items.value.slice(range.value.startIndex, range.value.endIndex + 1);
    });

    const activeId = computed(() => activeItem.value?.id ?? null);
    const activeRootId = computed(() => activeItem.value?.rootId ?? null);

    return {
        visibleItems,
        topSpacerPx: computed(() => range.value.topSpacerPx),
        bottomSpacerPx: computed(() => range.value.bottomSpacerPx),
        measureSection,
        ensureTargetWindow,
        getOffsetFor,
        activeId,
        activeRootId,
        recalculate,
    };
}
