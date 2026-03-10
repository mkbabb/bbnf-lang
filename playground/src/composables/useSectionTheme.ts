/**
 * Centralized section theming — colors, icons, and labels for docs/sidebar/nav.
 * Single source of truth for all section-related styling across the app.
 */

export interface SectionTheme {
    color: string;
    /** SVG path data for a 24x24 viewBox artistic icon */
    iconPath: string;
    /** Optional second SVG path for compound icons */
    iconPath2?: string;
    /** Optional image src (e.g. PNG) — takes priority over SVG paths when present */
    iconSrc?: string;
}

/**
 * Section name → theme mapping.
 * Colors reference CSS custom properties (e.g. "pastel-green" → var(--color-pastel-green)).
 */
export const sectionThemes: Record<string, SectionTheme> = {
    BBNF: {
        color: "pastel-green",
        // Quill pen — writing/grammar (fallback)
        iconPath: "M20.24 12.24a6 6 0 0 0-8.49-8.49L5 10.5V19h8.5z",
        iconPath2: "M16 8L2 22 M17.5 15H9",
        iconSrc: "/bbnf-icon.png",
    },
    "parse-that": {
        color: "pastel-blue",
        // Tree/branching — parser combinators
        iconPath: "M12 3v6m0 0l-4 4m4-4l4 4M8 13v4a2 2 0 0 0 2 2h4a2 2 0 0 0 2-2v-4",
        iconPath2: "M5 17a2 2 0 1 0 0 4 2 2 0 0 0 0-4zm14 0a2 2 0 1 0 0 4 2 2 0 0 0 0-4z",
    },
    pprint: {
        color: "pastel-purple",
        // Flowing text lines — pretty-printing
        iconPath: "M4 6h16M4 12h10M4 18h14",
        iconPath2: "M18 9l3 3-3 3",
    },
    gorgeous: {
        color: "pastel-amber",
        // Diamond/gem — polished output
        iconPath: "M6 3h12l4 6-10 13L2 9z",
        iconPath2: "M2 9h20M12 22L6 9l6-6 6 6z",
    },
};

/** Nav link icons for Playground and Docs. */
export const navIcons = {
    playground: {
        // Terminal/console
        iconPath: "M4 17l6-6-6-6",
        iconPath2: "M12 19h8",
    },
    docs: {
        // Open book
        iconPath: "M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2z",
        iconPath2: "M22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z",
    },
};

export function getSectionTheme(sectionName: string): SectionTheme {
    return sectionThemes[sectionName] ?? { color: "muted-foreground", iconPath: "M12 12m-10 0a10 10 0 1 0 20 0 10 10 0 1 0-20 0" };
}
