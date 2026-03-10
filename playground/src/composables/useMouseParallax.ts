import { ref, onMounted, onBeforeUnmount, type Ref } from "vue";

export function useMouseParallax(containerRef: Ref<HTMLElement | null>, intensity = 5) {
    const rotateX = ref(0);
    const rotateY = ref(0);

    let targetX = 0;
    let targetY = 0;
    let rafId = 0;

    function onPointerMove(e: PointerEvent) {
        const el = containerRef.value;
        if (!el) return;
        const rect = el.getBoundingClientRect();
        const cx = rect.left + rect.width / 2;
        const cy = rect.top + rect.height / 2;
        targetX = -((e.clientY - cy) / rect.height) * intensity;
        targetY = ((e.clientX - cx) / rect.width) * intensity;
    }

    function tick() {
        rotateX.value += (targetX - rotateX.value) * 0.08;
        rotateY.value += (targetY - rotateY.value) * 0.08;
        rafId = requestAnimationFrame(tick);
    }

    onMounted(() => {
        window.addEventListener("pointermove", onPointerMove, { passive: true });
        rafId = requestAnimationFrame(tick);
    });

    onBeforeUnmount(() => {
        window.removeEventListener("pointermove", onPointerMove);
        cancelAnimationFrame(rafId);
    });

    return { rotateX, rotateY };
}
