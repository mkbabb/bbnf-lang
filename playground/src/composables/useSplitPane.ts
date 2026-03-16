import { ref, computed, watch, onMounted, onBeforeUnmount, nextTick, type Ref, type ComputedRef } from "vue";
import { useMediaQuery, useStorage, useDebounceFn } from "@vueuse/core";

const DESKTOP_SPLIT_KEY = "bbnf-playground-split-desktop";
const MOBILE_SPLIT_KEY = "bbnf-playground-split-mobile";
const DEFAULT_SPLIT_RATIO = 0.5;
const DESKTOP_MIN_PANE_PX = 280;
const MOBILE_MIN_PANE_PX = 160;
const SPLIT_STEP = 0.04;

export interface SplitPaneReturn {
    splitContainerRef: Ref<HTMLElement | null>;
    isDesktop: Ref<boolean>;
    isDraggingDivider: Ref<boolean>;
    primaryPaneStyle: ComputedRef<Record<string, string>>;
    secondaryPaneStyle: ComputedRef<Record<string, string>>;
    splitRatio: ComputedRef<number>;
    onDividerPointerDown: (event: PointerEvent) => void;
    onDividerKeyDown: (event: KeyboardEvent) => void;
    resetSplitForCurrentMode: () => void;
    scheduleEditorRelayout: () => void;
}

export function useSplitPane(onRelayout: () => void): SplitPaneReturn {
    const isDesktop = useMediaQuery("(min-width: 768px)");
    const desktopSplitRatio = useStorage(DESKTOP_SPLIT_KEY, DEFAULT_SPLIT_RATIO);
    const mobileSplitRatio = useStorage(MOBILE_SPLIT_KEY, DEFAULT_SPLIT_RATIO);

    const splitContainerRef = ref<HTMLElement | null>(null);
    const splitAxisSize = ref(0);
    const isDraggingDivider = ref(false);
    let splitResizeObserver: ResizeObserver | null = null;
    let dividerDragStart: { x: number; y: number } | null = null;
    let dividerDidDrag = false;

    function clampSplitRatio(value: number, axisSize = splitAxisSize.value) {
        if (!Number.isFinite(value)) return DEFAULT_SPLIT_RATIO;

        const minPane = isDesktop.value ? DESKTOP_MIN_PANE_PX : MOBILE_MIN_PANE_PX;
        if (!axisSize || axisSize <= minPane * 2) {
            return Math.min(0.75, Math.max(0.25, value));
        }

        const minRatio = minPane / axisSize;
        const maxRatio = 1 - minRatio;
        if (minRatio >= maxRatio) return DEFAULT_SPLIT_RATIO;

        return Math.min(maxRatio, Math.max(minRatio, value));
    }

    const splitRatio = computed({
        get: () => clampSplitRatio(isDesktop.value ? desktopSplitRatio.value : mobileSplitRatio.value),
        set: (value: number) => {
            const clamped = clampSplitRatio(value);
            if (isDesktop.value) {
                desktopSplitRatio.value = clamped;
            } else {
                mobileSplitRatio.value = clamped;
            }
        },
    });

    const primaryPaneStyle = computed(() => {
        const basis = `${(splitRatio.value * 100).toFixed(3)}%`;
        return isDesktop.value
            ? { flex: `0 0 ${basis}`, minWidth: `${DESKTOP_MIN_PANE_PX}px` }
            : { flex: `0 0 ${basis}`, minHeight: `${MOBILE_MIN_PANE_PX}px` };
    });

    const secondaryPaneStyle = computed(() => {
        return isDesktop.value
            ? { minWidth: `${DESKTOP_MIN_PANE_PX}px` }
            : { minHeight: `${MOBILE_MIN_PANE_PX}px` };
    });

    function measureSplitAxis() {
        if (!splitContainerRef.value) return;
        const rect = splitContainerRef.value.getBoundingClientRect();
        splitAxisSize.value = isDesktop.value ? rect.width : rect.height;
    }

    function updateSplitFromPointer(clientX: number, clientY: number) {
        if (!splitContainerRef.value) return;
        const rect = splitContainerRef.value.getBoundingClientRect();
        const axisSize = isDesktop.value ? rect.width : rect.height;
        if (axisSize <= 0) return;
        splitAxisSize.value = axisSize;
        const offset = isDesktop.value ? clientX - rect.left : clientY - rect.top;
        splitRatio.value = offset / axisSize;
    }

    const scheduleEditorRelayout = useDebounceFn(() => {
        requestAnimationFrame(() => { onRelayout(); });
    }, 16);

    function stopDividerDrag() {
        if (!isDraggingDivider.value) return;
        isDraggingDivider.value = false;
        dividerDragStart = null;
        dividerDidDrag = false;
        document.body.style.userSelect = "";
        window.removeEventListener("pointermove", onWindowPointerMove);
        window.removeEventListener("pointerup", onWindowPointerUp);
    }

    function onDividerPointerDown(event: PointerEvent) {
        event.preventDefault();
        isDraggingDivider.value = true;
        dividerDragStart = { x: event.clientX, y: event.clientY };
        dividerDidDrag = false;
        document.body.style.userSelect = "none";
        (event.currentTarget as HTMLElement | null)?.focus();
        window.addEventListener("pointermove", onWindowPointerMove);
        window.addEventListener("pointerup", onWindowPointerUp);
    }

    function onWindowPointerMove(event: PointerEvent) {
        if (!isDraggingDivider.value) return;
        if (!dividerDidDrag) {
            const start = dividerDragStart;
            if (!start) return;
            const delta = isDesktop.value ? Math.abs(event.clientX - start.x) : Math.abs(event.clientY - start.y);
            if (delta < 3) return;
            dividerDidDrag = true;
        }
        updateSplitFromPointer(event.clientX, event.clientY);
    }

    function onWindowPointerUp(event: PointerEvent) {
        if (dividerDidDrag) {
            updateSplitFromPointer(event.clientX, event.clientY);
        }
        stopDividerDrag();
        if (dividerDidDrag) {
            scheduleEditorRelayout();
        }
    }

    function resetSplitForCurrentMode() {
        splitRatio.value = DEFAULT_SPLIT_RATIO;
        scheduleEditorRelayout();
    }

    function onDividerKeyDown(event: KeyboardEvent) {
        const isHorizontal = isDesktop.value;
        let nextValue: number | null = null;

        if (event.key === "Home") nextValue = 0;
        if (event.key === "End") nextValue = 1;
        if (isHorizontal && event.key === "ArrowLeft") nextValue = splitRatio.value - SPLIT_STEP;
        if (isHorizontal && event.key === "ArrowRight") nextValue = splitRatio.value + SPLIT_STEP;
        if (!isHorizontal && event.key === "ArrowUp") nextValue = splitRatio.value - SPLIT_STEP;
        if (!isHorizontal && event.key === "ArrowDown") nextValue = splitRatio.value + SPLIT_STEP;

        if (nextValue == null) return;
        event.preventDefault();
        splitRatio.value = nextValue;
        scheduleEditorRelayout();
    }

    watch(isDesktop, () => {
        nextTick(() => {
            measureSplitAxis();
            splitRatio.value = splitRatio.value;
            scheduleEditorRelayout();
        });
    });

    watch(splitRatio, () => {
        scheduleEditorRelayout();
    });

    onMounted(() => {
        nextTick(() => {
            measureSplitAxis();
            splitResizeObserver = new ResizeObserver(() => {
                measureSplitAxis();
                splitRatio.value = splitRatio.value;
                scheduleEditorRelayout();
            });

            if (splitContainerRef.value) {
                splitResizeObserver.observe(splitContainerRef.value);
            }

            scheduleEditorRelayout();
        });
    });

    onBeforeUnmount(() => {
        stopDividerDrag();
        splitResizeObserver?.disconnect();
    });

    return {
        splitContainerRef,
        isDesktop,
        isDraggingDivider,
        primaryPaneStyle,
        secondaryPaneStyle,
        splitRatio,
        onDividerPointerDown,
        onDividerKeyDown,
        resetSplitForCurrentMode,
        scheduleEditorRelayout,
    };
}
