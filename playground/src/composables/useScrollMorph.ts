import { ref, onMounted, onBeforeUnmount, type Ref } from "vue";
import { ScrollTimeline, ElementMorph, easeOutCubic } from "@mkbabb/keyframes.js";

/**
 * Scroll-driven morph animation between two DOM elements.
 *
 * Uses ScrollTimeline for progress and ElementMorph for interpolation.
 * Writes transforms directly in rAF — zero Vue reactivity lag, zero jitter.
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
    const progress = ref(0);
    let rafId = 0;
    let morph: ElementMorph | null = null;

    const timeline = new ScrollTimeline({
        threshold: options.scrollThreshold ?? 0.35,
        easing: easeOutCubic,
        smoothing: { damping: options.damping ?? 0.15, snapThreshold: 0.002 },
    });

    function update() {
        const marker = markerRef.value;
        const element = elementRef.value;
        const target = document.querySelector(toSelector);

        if (!marker || !element || !target) {
            rafId = requestAnimationFrame(update);
            return;
        }

        const p = timeline.tick();
        progress.value = p;

        if (p <= 0) {
            element.style.transform = "";
            element.style.willChange = "transform";
            element.style.pointerEvents = "";
        } else {
            if (!morph) {
                morph = new ElementMorph(marker, target as HTMLElement);
            } else {
                morph.measure(marker, target as HTMLElement);
            }
            morph.apply(element, p);
            element.style.willChange = p >= 1 ? "auto" : "transform";
            element.style.pointerEvents = p > 0.5 ? "none" : "";
        }

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
