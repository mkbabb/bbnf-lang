import { ref } from "vue";

/** Module-level shared state — 0 = hero logo fully visible, 1 = navbar logo visible */
const morphProgress = ref(1);

export function useHeroState() {
    return { morphProgress };
}
