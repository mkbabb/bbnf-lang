import { onMounted, onUnmounted } from "vue";
import type { ClickDelegateOptions } from "./types";

/**
 * Delegated click handling with configurable selector and attribute.
 */
export function useClickDelegate(options: ClickDelegateOptions) {
    const selector = options.selector ?? "[data-scroll-target]";
    const attribute = options.attribute ?? "data-scroll-target";

    function handleClick(e: MouseEvent) {
        const target = (e.target as HTMLElement).closest<HTMLElement>(selector);
        if (!target) return;
        e.preventDefault();
        const value = target.getAttribute(attribute);
        if (!value) return;
        const id = options.resolve(value);
        if (id) options.scrollTo(id);
    }

    onMounted(() => {
        const el = options.container.value;
        if (el) el.addEventListener("click", handleClick);
    });

    onUnmounted(() => {
        const el = options.container.value;
        if (el) el.removeEventListener("click", handleClick);
    });

    return { handleClick };
}
