/**
 * Raw LaTeX text cleanup utilities.
 *
 * Text nodes from braceBalanced() contain raw LaTeX (accents, dashes,
 * nested commands, etc.). cleanRawLatex() processes these patterns,
 * mirroring the old regex-based parser's cleanProseSegment().
 */

import { ACCENT_MAPS, SYMBOL_MAP } from "../utils/accents";

/**
 * Apply accent command replacements to raw text.
 * Handles braced (\"{a}) and unbraced (\"a) forms.
 */
function replaceAccents(text: string): string {
    for (const [cmd, map] of Object.entries(ACCENT_MAPS)) {
        if (Object.keys(map).length === 0) continue;
        const escaped = cmd.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
        // Braced form: \cmd{char}
        text = text.replace(
            new RegExp(`\\\\${escaped}\\{([a-zA-Z])\\}`, "g"),
            (_, ch: string) => map[ch] ?? ch,
        );
        // Unbraced form: \cmd char
        text = text.replace(
            new RegExp(`\\\\${escaped}([a-zA-Z])`, "g"),
            (_, ch: string) => map[ch] ?? ch,
        );
    }
    return text;
}

/**
 * Replace symbol commands (\infty, \implies, etc.) in raw text.
 */
function replaceSymbols(text: string): string {
    // Sort by length descending to match longest first
    const names = Object.keys(SYMBOL_MAP).sort((a, b) => b.length - a.length);
    for (const name of names) {
        const escaped = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
        text = text.replace(
            new RegExp(`\\\\${escaped}(?![a-zA-Z])`, "g"),
            SYMBOL_MAP[name],
        );
    }
    return text;
}

/**
 * Clean residual LaTeX patterns from a prose (non-math) text segment.
 *
 * Most patterns are now handled by the combinator parser (braceContent).
 * This only processes patterns that may still appear in text nodes:
 * - Escaped specials (\&, \#, etc.) from the plainText parser
 * - Residual braces from brace-balanced content
 * - Tilde → space (if not caught by tilde parser)
 * - Spacing commands that might appear in raw text
 */
function cleanProseSegment(
    text: string,
    labelResolver?: (key: string) => string | undefined,
): string {
    // Accents → Unicode (fallback for any unprocessed accents)
    text = replaceAccents(text);

    // Dashes (fallback — parser handles these, but raw text may still have them)
    text = text.replace(/---/g, "\u2014");
    text = text.replace(/--/g, "\u2013");

    // Smart quotes (fallback)
    text = text.replace(/``/g, "\u201C");
    text = text.replace(/''/g, "\u201D");

    // Symbols → Unicode (fallback)
    text = replaceSymbols(text);

    // Refs: resolve \ref{key} via label registry, producing clickable links
    const resolveRef = (key: string) => labelResolver?.(key) ?? "";
    const refLink = (key: string, display: string) =>
        `<a class="paper-ref" data-ref="${key}">${display}</a>`;

    text = text.replace(
        /(Chapters?|Sections?|Theorem|Figure|Lemma|Definition|Proposition|Corollary)[~\s]+\\ref\{([^}]*)\}/g,
        (_, prefix: string, key: string) => {
            const num = resolveRef(key);
            return num ? refLink(key, `${prefix} ${num}`) : prefix;
        },
    );
    text = text.replace(/\\S\s*\\ref\{([^}]*)\}/g, (_, key: string) => {
        const num = resolveRef(key);
        return num ? refLink(key, `\u00a7${num}`) : "\u00a7";
    });
    text = text.replace(/\\eqref\{([^}]*)\}/g, (_, key: string) => {
        const num = resolveRef(key);
        return num ? refLink(key, `(${num})`) : "";
    });
    text = text.replace(
        /\\hyperref\[([^\]]*)\]\{([^}]*)\}/g,
        (_, key: string, display: string) => refLink(key, display),
    );
    text = text.replace(/\\ref\{([^}]*)\}/g, (_, key: string) => {
        const num = resolveRef(key);
        return num ? refLink(key, num) : "";
    });
    text = text.replace(/\\label\{[^}]*\}/g, "");

    // Tilde → non-breaking space
    text = text.replace(/~/g, " ");

    // Spacing commands → space
    text = text.replace(/\\[,;:!]/g, " ");
    text = text.replace(/\\q?quad/g, " ");
    text = text.replace(/\\\\/g, "<br />");
    text = text.replace(/\\newline(?![a-zA-Z])/g, "<br />");
    text = text.replace(/\\thinspace(?![a-zA-Z])/g, " ");

    // Skip/strip commands
    text = text.replace(/\\(?:noindent|hfill|centering)\s*/g, "");
    text = text.replace(/\\(?:medskip|smallskip|bigskip|vfill)\s*/g, "");
    text = text.replace(/\\vspace\*?\{[^}]*\}/g, "");

    // Escaped specials
    text = text.replace(/\\@/g, "");
    text = text.replace(/\\&/g, "&amp;");
    text = text.replace(/\\([#$%_{}])/g, "$1");

    // Strip remaining braces (after all command processing)
    text = text.replace(/[{}]/g, "");

    return text;
}

/**
 * Clean residual LaTeX patterns from text node content.
 *
 * Math segments ($...$) are preserved verbatim.
 *
 * @param labelResolver Optional function to resolve \ref{key} → number string.
 */
export function cleanRawLatex(
    text: string,
    labelResolver?: (key: string) => string | undefined,
): string {
    // Split on $...$ boundaries. Odd-indexed segments are math.
    const parts = text.split(/(\$[^$]*\$)/g);
    for (let i = 0; i < parts.length; i++) {
        if (i % 2 === 1) continue; // skip math segments
        parts[i] = cleanProseSegment(parts[i], labelResolver);
    }
    return parts.join("").replace(/  +/g, " ");
}

/** Slugify a string for HTML id generation. */
export function slugify(text: string): string {
    return text
        .toLowerCase()
        .replace(/[^a-z0-9]+/g, "-")
        .replace(/^-|-$/g, "");
}
