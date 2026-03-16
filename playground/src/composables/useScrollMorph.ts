import { ref, onMounted, onBeforeUnmount, type Ref } from "vue";
import { ScrollTimeline, easeOutCubic } from "@mkbabb/keyframes.js";

const snap = (v: number) => Math.round(v * 2) / 2;

/**
 * Scroll-driven morph: moves `elementRef` from its natural document position
 * (measured via `markerRef`) to the fixed-position `toSelector` target.
 *
 * Once progress > 0 the element becomes `position: fixed` so the browser's
 * scroll paint can't fight the JS-driven transform.  The parent container's
 * dimensions are locked to prevent layout shift.
 *
 * IMPORTANT: no ancestor of `elementRef` may have a CSS `transform` set
 * while the morph is active — that changes the containing block for
 * `position: fixed` from the viewport to the transformed ancestor.
 */
export function useScrollMorph(
    markerRef: Ref<HTMLElement | null>,
    elementRef: Ref<HTMLElement | null>,
    toSelector: string,
    options: {
        scrollThreshold?: number;
        damping?: number;
    } = {},
) {
    const progress = ref(0);
    let rafId = 0;
    let lastP = -1;
    let parentLocked = false;

    const timeline = new ScrollTimeline({
        threshold: options.scrollThreshold ?? 0.35,
        easing: easeOutCubic,
        boundaryEpsilon: 0.006,
        smoothing: {
            damping: options.damping ?? 0.2,
            snapThreshold: 0.008,
            targetEpsilon: 0.002,
        },
    });

    function lockParent(el: HTMLElement) {
        const parent = el.parentElement;
        if (!parent || parentLocked) return;
        const rect = parent.getBoundingClientRect();
        parent.style.minWidth = `${rect.width}px`;
        parent.style.minHeight = `${rect.height}px`;
        parentLocked = true;
    }

    function unlockParent(el: HTMLElement) {
        const parent = el.parentElement;
        if (!parent || !parentLocked) return;
        parent.style.minWidth = "";
        parent.style.minHeight = "";
        parentLocked = false;
    }

    function clearStyles(el: HTMLElement) {
        el.style.position = "";
        el.style.left = "";
        el.style.top = "";
        el.style.width = "";
        el.style.height = "";
        el.style.transform = "";
        el.style.transformOrigin = "";
        el.style.willChange = "";
        el.style.pointerEvents = "";
        el.style.zIndex = "";
    }

    function update() {
        const marker = markerRef.value;
        const element = elementRef.value;
        const target = document.querySelector(toSelector) as HTMLElement | null;

        if (!marker || !element || !target) {
            rafId = requestAnimationFrame(update);
            return;
        }

        const p = timeline.tick();
        progress.value = p;

        if (p <= 0) {
            if (lastP !== 0) {
                clearStyles(element);
                unlockParent(element);
                lastP = 0;
            }
        } else if (p >= 1 && lastP >= 1) {
            // Locked — no updates
        } else {
            if (!parentLocked) lockParent(element);

            // Both rects are in viewport coordinates — same space as
            // position:fixed when no ancestor has a transform.
            const m = marker.getBoundingClientRect();
            const t = target.getBoundingClientRect();

            const x = snap(m.x) * (1 - p) + snap(t.x) * p;
            const y = snap(m.y) * (1 - p) + snap(t.y) * p;
            const sx = m.width > 0 ? 1 + (t.width / m.width - 1) * p : 1;
            const sy = m.height > 0 ? 1 + (t.height / m.height - 1) * p : 1;

            element.style.position = "fixed";
            element.style.left = "0px";
            element.style.top = "0px";
            element.style.width = `${m.width}px`;
            element.style.height = `${m.height}px`;
            element.style.zIndex = "60";
            element.style.transform =
                `translate(${x}px,${y}px) scale(${sx},${sy})`;
            element.style.transformOrigin = "top left";
            element.style.willChange = p >= 1 ? "auto" : "transform";
            element.style.pointerEvents = p > 0.5 ? "none" : "";
            lastP = p;
        }

        rafId = requestAnimationFrame(update);
    }

    onMounted(() => {
        rafId = requestAnimationFrame(update);
    });

    onBeforeUnmount(() => {
        if (elementRef.value) {
            clearStyles(elementRef.value);
            unlockParent(elementRef.value);
        }
        cancelAnimationFrame(rafId);
    });

    return { progress };
}
