/** Shared color/tone map constants used by ControlsBar, InlineRichText, and other components. */

export const exampleIcons: Record<string, string> = {
    JSON: "/img/json.svg",
    CSS: "/img/css.svg",
    BBNF: "/img/bbnf.png",
    Math: "/img/math.svg",
    Hello: "/img/text.svg",
    "Google Sheets": "/img/sheets.svg",
};

export const exampleToneMap: Record<string, string> = {
    JSON: "border-pastel-green/25 hover:border-pastel-green/45 hover:bg-pastel-green/5",
    CSS: "border-pastel-blue/25 hover:border-pastel-blue/45 hover:bg-pastel-blue/5",
    BBNF: "border-pastel-amber/25 hover:border-pastel-amber/45 hover:bg-pastel-amber/5",
    Math: "border-pastel-purple/25 hover:border-pastel-purple/45 hover:bg-pastel-purple/5",
    Hello: "border-pastel-green/25 hover:border-pastel-green/45 hover:bg-pastel-green/5",
    "Google Sheets": "border-pastel-green/25 hover:border-pastel-green/45 hover:bg-pastel-green/5",
};

export const tagToneMap: Record<string, string> = {
    "@pretty": "pastel-pink",
    "@recover": "pastel-blue",
    "error recovery": "pastel-blue",
    recursive: "pastel-green",
    nesting: "pastel-purple",
    precedence: "pastel-amber",
    "self-hosting": "pastel-amber",
    meta: "pastel-purple",
    beginner: "pastel-green",
    operators: "pastel-amber",
};

/** Per-language shimmer color map. */
export const shimmerMap: Record<string, "gold" | "blue"> = {
    BBNF: "gold",
    CSS: "blue",
};

export function shimmerClass(name: string): string {
    const color = shimmerMap[name];
    if (color === "gold") return "gold-shimmer";
    if (color === "blue") return "blue-shimmer";
    return "";
}

export function exampleToneClass(name: string): string {
    return exampleToneMap[name] ?? "border-border/35 hover:border-border/60 hover:bg-accent/40";
}

function tagToneColor(tag: string): string {
    return tagToneMap[tag.toLowerCase()] ?? "muted-foreground";
}

export function tagToneStyle(tag: string) {
    const color = tagToneColor(tag);
    return {
        color: `var(--color-${color})`,
        background: `color-mix(in srgb, var(--color-${color}) 12%, transparent)`,
        border: `1px solid color-mix(in srgb, var(--color-${color}) 24%, transparent)`,
        boxShadow: `inset 0 1px 0 color-mix(in srgb, var(--color-${color}) 10%, transparent)`,
    };
}

/** Token tone map used by InlineRichText for inline code chip styling. */
export const tokenToneMap: Record<string, string> = {
    "@pretty": "pastel-pink",
    "@recover": "pastel-blue",
    "@media": "pastel-blue",
    "@supports": "pastel-blue",
    "error recovery": "pastel-blue",
    "JSON": "pastel-green",
    "CSS L1.75": "pastel-blue",
    "Auto": "pastel-blue",
    "gorgeous": "pastel-amber",
    "WASM": "pastel-amber",
    "TS interpreter": "pastel-purple",
};
