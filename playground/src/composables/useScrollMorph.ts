import { ref, onMounted, onBeforeUnmount, type Ref } from "vue";
import { easeOutCubic } from "@mkbabb/keyframes.js";

/**
 * Scroll-driven morph animation between two DOM elements.
 *
 * Measures `markerRef` and `toSelector` each frame, interpolates position/scale
 * between them based on scroll progress. Writes transforms directly to `elementRef`
 * in the rAF callback — zero Vue reactivity lag, zero jitter.
 *
 * When scroll exceeds the threshold, snaps immediately to the target position.
 */
export function useScrollMorph(
    markerRef: Ref<HTMLElement | null>,
    elementRef: Ref<HTMLElement | null>,
    toSelector: string,
    options: {
        scrollThreshold?: number;
        /** Lerp damping factor — lower = smoother/slower (0–1). Default 0.15 */
        damping?: number;
    } = {},
) {
    const scrollThreshold = options.scrollThreshold ?? 0.35;
    const damping = options.damping ?? 0.15;

    const progress = ref(0);

    let smoothProgress = 0;
    let rafId = 0;

    function update() {
        const marker = markerRef.value;
        const element = elementRef.value;
        const target = document.querySelector(toSelector);

        if (!marker || !element || !target) {
            rafId = requestAnimationFrame(update);
            return;
        }

        // Compute raw scroll progress and apply easing
        const threshold = window.innerHeight * scrollThreshold;
        const raw = Math.min(Math.max(window.scrollY / threshold, 0), 1);
        const targetProgress = easeOutCubic(raw);

        // Snap immediately past threshold; lerp within
        if (raw >= 1) {
            smoothProgress = 1;
        } else if (raw <= 0) {
            smoothProgress = 0;
        } else {
            smoothProgress += (targetProgress - smoothProgress) * damping;
            if (Math.abs(smoothProgress - targetProgress) < 0.002) {
                smoothProgress = targetProgress;
            }
        }

        const p = smoothProgress;
        progress.value = p;

        // Measure source and target
        const sr = marker.getBoundingClientRect();
        const tr = target.getBoundingClientRect();

        if (p <= 0) {
            element.style.transform = "";
            element.style.willChange = "transform";
            element.style.pointerEvents = "";
            rafId = requestAnimationFrame(update);
            return;
        }

        // Compute interpolated transform and write directly to DOM — no reactivity lag
        const dx = (tr.left - sr.left) * p;
        const dy = (tr.top - sr.top) * p;
        const scaleX = 1 + (tr.width / sr.width - 1) * p;
        const scaleY = 1 + (tr.height / sr.height - 1) * p;

        element.style.transform = `translate(${dx}px, ${dy}px) scale(${scaleX}, ${scaleY})`;
        element.style.transformOrigin = "top left";
        element.style.willChange = p >= 1 ? "auto" : "transform";
        element.style.pointerEvents = p > 0.5 ? "none" : "";

        rafId = requestAnimationFrame(update);
    }

    onMounted(() => {
        rafId = requestAnimationFrame(update);
    });

    onBeforeUnmount(() => {
        cancelAnimationFrame(rafId);
    });

    return { progress };
}
