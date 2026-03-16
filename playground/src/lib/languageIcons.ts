export interface LanguageIcon {
    /** Display label */
    label: string;
    /** SVG path data for a compact icon (viewBox 0 0 24 24 or similar) */
    svg: string;
    /** Color for the icon */
    color: string;
}

export const languageIcons: Record<string, LanguageIcon> = {
    rust: {
        label: "Rust",
        svg: `<svg viewBox="0 0 24 24" fill="currentColor"><path d="M23.8 14.1l-1.3-.8c0-.2.1-.5.1-.7v-.7l1.3-.8c.2-.1.3-.4.2-.6l-.5-1.2-.3-.6c-.1-.2-.4-.3-.6-.2l-1.4.5c-.3-.4-.6-.7-1-1l.5-1.4c.1-.2 0-.5-.2-.6l-.6-.3-1.2-.5c-.2-.1-.5 0-.6.2l-.8 1.3c-.5-.1-.9-.1-1.4 0l-.8-1.3c-.1-.2-.4-.3-.6-.2l-1.2.5-.6.3c-.2.1-.3.4-.2.6l.5 1.4c-.4.3-.7.6-1 1l-1.4-.5c-.2-.1-.5 0-.6.2l-.3.6-.5 1.2c-.1.2 0 .5.2.6l1.3.8v1.4l-1.3.8c-.2.1-.3.4-.2.6l.5 1.2.3.6c.1.2.4.3.6.2l1.4-.5c.3.4.6.7 1 1l-.5 1.4c-.1.2 0 .5.2.6l.6.3 1.2.5c.2.1.5 0 .6-.2l.8-1.3c.5.1.9.1 1.4 0l.8 1.3c.1.2.4.3.6.2l1.2-.5.6-.3c.2-.1.3-.4.2-.6l-.5-1.4c.4-.3.7-.6 1-1l1.4.5c.2.1.5 0 .6-.2l.3-.6.5-1.2c.1-.2 0-.5-.2-.6zM12 16c-2.2 0-4-1.8-4-4s1.8-4 4-4 4 1.8 4 4-1.8 4-4 4z"/></svg>`,
        color: "#CE422B",
    },
    typescript: {
        label: "TypeScript",
        svg: `<svg viewBox="0 0 24 24" fill="currentColor"><rect width="24" height="24" rx="3" fill="#3178C6"/><path d="M5.7 14.2v1.3h3v8h1.8v-8h3v-1.3H5.7zm8.6 1.5c0 .6.2 1.1.6 1.5.4.4 1 .7 1.9 1 .4.1.7.3.9.4.2.2.3.4.3.6 0 .3-.1.5-.3.7-.2.2-.5.3-.9.3-.7 0-1.3-.3-1.8-.9l-1.1.9c.3.5.7.8 1.2 1.1.5.2 1.1.4 1.7.4.9 0 1.6-.2 2.1-.7.5-.4.8-1 .8-1.8 0-.6-.2-1.1-.6-1.5-.4-.4-1-.7-1.9-1-.4-.1-.7-.3-.9-.5-.2-.2-.2-.3-.2-.5 0-.2.1-.4.3-.6.2-.2.5-.2.8-.2.6 0 1.1.2 1.6.7l1-1c-.7-.7-1.5-1.1-2.6-1.1-.8 0-1.5.2-2 .7-.5.4-.7 1-.7 1.7z" fill="white"/></svg>`,
        color: "#3178C6",
    },
    ts: {
        label: "TypeScript",
        svg: `<svg viewBox="0 0 24 24" fill="currentColor"><rect width="24" height="24" rx="3" fill="#3178C6"/><path d="M5.7 14.2v1.3h3v8h1.8v-8h3v-1.3H5.7zm8.6 1.5c0 .6.2 1.1.6 1.5.4.4 1 .7 1.9 1 .4.1.7.3.9.4.2.2.3.4.3.6 0 .3-.1.5-.3.7-.2.2-.5.3-.9.3-.7 0-1.3-.3-1.8-.9l-1.1.9c.3.5.7.8 1.2 1.1.5.2 1.1.4 1.7.4.9 0 1.6-.2 2.1-.7.5-.4.8-1 .8-1.8 0-.6-.2-1.1-.6-1.5-.4-.4-1-.7-1.9-1-.4-.1-.7-.3-.9-.5-.2-.2-.2-.3-.2-.5 0-.2.1-.4.3-.6.2-.2.5-.2.8-.2.6 0 1.1.2 1.6.7l1-1c-.7-.7-1.5-1.1-2.6-1.1-.8 0-1.5.2-2 .7-.5.4-.7 1-.7 1.7z" fill="white"/></svg>`,
        color: "#3178C6",
    },
    javascript: {
        label: "JavaScript",
        svg: `<svg viewBox="0 0 24 24" fill="currentColor"><rect width="24" height="24" rx="3" fill="#F7DF1E"/><path d="M6.3 19.7l1.5-0.9c.3.5.5.9 1.1.9.6 0 .9-.2.9-.9v-5h1.9v5c0 1.6-.9 2.3-2.3 2.3-1.2 0-1.9-.6-2.3-1.4h.2zm5.2-.2l1.5-.9c.4.6.8 1.1 1.7 1.1.7 0 1.1-.4 1.1-.8 0-.6-.5-.8-1.2-1.1l-.4-.2c-1.2-.5-2-1.2-2-2.5 0-1.3 1-2.2 2.5-2.2 1.1 0 1.8.4 2.4 1.3l-1.3.9c-.3-.5-.6-.7-1.1-.7-.5 0-.8.3-.8.7 0 .5.3.7 1 1l.4.2c1.4.6 2.2 1.2 2.2 2.6 0 1.5-1.2 2.3-2.7 2.3-1.5 0-2.5-.7-3-1.7h.2z" fill="black"/></svg>`,
        color: "#F7DF1E",
    },
    js: {
        label: "JavaScript",
        svg: `<svg viewBox="0 0 24 24" fill="currentColor"><rect width="24" height="24" rx="3" fill="#F7DF1E"/><path d="M6.3 19.7l1.5-0.9c.3.5.5.9 1.1.9.6 0 .9-.2.9-.9v-5h1.9v5c0 1.6-.9 2.3-2.3 2.3-1.2 0-1.9-.6-2.3-1.4h.2zm5.2-.2l1.5-.9c.4.6.8 1.1 1.7 1.1.7 0 1.1-.4 1.1-.8 0-.6-.5-.8-1.2-1.1l-.4-.2c-1.2-.5-2-1.2-2-2.5 0-1.3 1-2.2 2.5-2.2 1.1 0 1.8.4 2.4 1.3l-1.3.9c-.3-.5-.6-.7-1.1-.7-.5 0-.8.3-.8.7 0 .5.3.7 1 1l.4.2c1.4.6 2.2 1.2 2.2 2.6 0 1.5-1.2 2.3-2.7 2.3-1.5 0-2.5-.7-3-1.7h.2z" fill="black"/></svg>`,
        color: "#F7DF1E",
    },
    wasm: {
        label: "WASM",
        svg: `<svg viewBox="0 0 24 24" fill="currentColor"><path d="M1 11.5l3.3-6.5h15.4l3.3 6.5-3.3 6.5H4.3L1 11.5z" fill="#654FF0"/><path d="M7.5 14.5h1.2l.5-2.5.7 2.5h1.2l1-4h-1l-.6 2.5-.7-2.5H9l-.7 2.5-.5-2.5H6.5l1 4zm6.2 0h1.1l.3-1h1.6l.3 1h1.2l-1.6-4h-1.3l-1.6 4zm1.7-1.9l.5-1.5.5 1.5h-1z" fill="white"/></svg>`,
        color: "#654FF0",
    },
    bbnf: {
        label: "BBNF",
        svg: `<img src="/img/bbnf.png" alt="BBNF" style="width:100%;height:100%;object-fit:contain" />`,
        color: "#4ade80",
    },
    json: {
        label: "JSON",
        svg: `<svg viewBox="0 0 24 24" fill="currentColor"><path d="M5 3h2v2H5v5a2 2 0 0 1-2 2 2 2 0 0 1 2 2v5h2v2H5c-1.07-.27-2-.9-2-2v-4a2 2 0 0 0-2-2H0v-2h1a2 2 0 0 0 2-2V5a2 2 0 0 1 2-2m14 0a2 2 0 0 1 2 2v4a2 2 0 0 0 2 2h1v2h-1a2 2 0 0 0-2 2v4a2 2 0 0 1-2 2h-2v-2h2v-5a2 2 0 0 1 2-2 2 2 0 0 1-2-2V5h-2V3h2z"/></svg>`,
        color: "#FFA500",
    },
    css: {
        label: "CSS",
        svg: `<svg viewBox="0 0 24 24" fill="currentColor"><path d="M4.2 3l1.7 17L12 22l6.1-2L19.8 3H4.2zM16 7H8.3l.2 2H15.8l-.6 7-3.2.9-3.2-.9-.2-2.5h2l.1 1.3 1.3.3 1.3-.3.1-1.6H8l-.5-5.7H16.3L16 7z" fill="#264de4"/></svg>`,
        color: "#264de4",
    },
    bash: {
        label: "Bash",
        svg: `<svg viewBox="0 0 24 24" fill="currentColor"><path d="M4 17l6-6-6-6"/><path d="M12 19h8"/></svg>`,
        color: "#4EAA25",
    },
    toml: {
        label: "TOML",
        svg: `<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M14 3v4a1 1 0 0 0 1 1h4"/><path d="M17 21H7a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h7l5 5v11a2 2 0 0 1-2 2z"/></svg>`,
        color: "#9C4221",
    },
};

export function getLanguageIcon(lang: string): LanguageIcon | undefined {
    return languageIcons[lang.toLowerCase()];
}
