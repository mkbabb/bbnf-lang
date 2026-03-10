import { ref, onMounted, onBeforeUnmount, type Ref } from "vue";
import { easeOutCubic } from "@mkbabb/keyframes.js";

/**
 * Scroll-driven entrance animation: element fades in + translates up as it scrolls into view.
 * Uses IntersectionObserver to trigger, then tracks scroll progress via rAF.
 */
export function useScrollTimeline(
    elementRef: Ref<HTMLElement | null>,
    options?: { translateY?: number; threshold?: number },
) {
    const translateY = options?.translateY ?? 40;
    const threshold = options?.threshold ?? 0.15;
    const progress = ref(0);

    let observer: IntersectionObserver | null = null;
    let rafId = 0;
    let tracking = false;
    let done = false;

    function update() {
        const el = elementRef.value;
        if (!el || done) return;

        const rect = el.getBoundingClientRect();
        const viewportH = window.innerHeight;

        // Progress: 0 when element top is at bottom of viewport, 1 when fully in view
        const raw = Math.min(
            Math.max((viewportH - rect.top) / (viewportH * threshold + rect.height * 0.3), 0),
            1,
        );
        const eased = easeOutCubic(raw);
        progress.value = eased;

        // Apply transform directly for performance
        el.style.opacity = String(eased);
        el.style.transform = `translateY(${(1 - eased) * translateY}px)`;

        if (eased >= 1) {
            done = true;
            tracking = false;
            // Clean up inline styles once complete
            el.style.willChange = "";
            return;
        }

        rafId = requestAnimationFrame(update);
    }

    function startTracking() {
        if (tracking || done) return;
        tracking = true;
        const el = elementRef.value;
        if (el) {
            el.style.willChange = "transform, opacity";
        }
        rafId = requestAnimationFrame(update);
    }

    onMounted(() => {
        const el = elementRef.value;
        if (!el) return;

        // Initial state: hidden
        el.style.opacity = "0";
        el.style.transform = `translateY(${translateY}px)`;

        observer = new IntersectionObserver(
            (entries) => {
                for (const entry of entries) {
                    if (entry.isIntersecting) {
                        startTracking();
                    } else if (!done) {
                        tracking = false;
                        cancelAnimationFrame(rafId);
                    }
                }
            },
            { threshold: 0.05 },
        );

        observer.observe(el);
    });

    onBeforeUnmount(() => {
        cancelAnimationFrame(rafId);
        observer?.disconnect();
    });

    return { progress };
}
