export { default as DarkModeToggle } from "./DarkModeToggle.vue";

import { createGlobalState, useDark, useToggle } from "@vueuse/core";
import { watch } from "vue";

/** Single shared dark mode instance -- avoids multiple useDark() watchers racing on classList. */
export const useGlobalDark = createGlobalState(() => {
    const isDark = useDark({ disableTransition: false });
    const toggleDark = useToggle(isDark);

    // Safari: force style recalculation after .dark class toggle.
    watch(
        isDark,
        (dark) => {
            document.documentElement.style.colorScheme = dark ? "dark" : "light";
        },
        { immediate: true },
    );

    return { isDark, toggleDark };
});
