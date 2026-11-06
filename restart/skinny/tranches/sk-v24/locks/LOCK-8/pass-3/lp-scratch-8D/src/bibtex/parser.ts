/**
 * BibTeX parser: parseBibString() and parseBibFile().
 * Uses hand-written combinators (the BBNF grammar is available as documentation,
 * but the combinator approach is more robust for real-world .bib files).
 */

import { Parser, regex, string, any } from "@mkbabb/parse-that";
import type { BibEntry } from "../types/bibtex";
import { ACCENT_MAPS } from "../utils/accents";

// ── Accent cleaning ─────────────────────────────────────────────────

function cleanAccents(text: string): string {
    // Braced accents: \"{a} → ä
    text = text.replace(
        /\\(['"`^~])\{(\w)\}/g,
        (_, cmd: string, ch: string) => ACCENT_MAPS[cmd]?.[ch] ?? ch,
    );
    // Unbraced accents: \"a → ä
    text = text.replace(
        /\\(['"`^~])(\w)/g,
        (_, cmd: string, ch: string) => ACCENT_MAPS[cmd]?.[ch] ?? ch,
    );
    // Named accents: \c{c} → ç
    text = text.replace(
        /\\([Hcuv])\{(\w)\}/g,
        (_, cmd: string, ch: string) => ACCENT_MAPS[cmd]?.[ch] ?? ch,
    );
    // Strip remaining braces
    text = text.replace(/[{}]/g, "");
    return text.trim();
}

// ── Short author extraction ─────────────────────────────────────────

function extractShortAuthor(author: string): string {
    const multiAuthor = author.includes(" and ");
    let short = author;

    if (multiAuthor) {
        short = author.split(" and ")[0].trim();
    }

    // Get last name
    if (short.includes(",")) {
        short = short.split(",")[0].trim();
    } else {
        const parts = short.split(/\s+/);
        const suffixes = new Set(["Jr.", "Sr.", "Jr", "Sr", "II", "III", "IV"]);
        let lastIdx = parts.length - 1;
        while (lastIdx > 0 && suffixes.has(parts[lastIdx])) lastIdx--;
        short = parts[lastIdx];
    }

    if (multiAuthor) {
        short += " et al.";
    }

    return short;
}

// ── Regex-based parser (reliable) ───────────────────────────────────

/**
 * Parse a BibTeX string into an array of BibEntry objects.
 * Uses regex extraction — robust for real-world .bib files.
 */
export function parseBibString(source: string): BibEntry[] {
    const entries: BibEntry[] = [];
    const entryRe = /@(\w+)\{\s*([^\s,]+)\s*,([\s\S]*?)(?=\n@|\n*$)/g;
    let m: RegExpExecArray | null;

    while ((m = entryRe.exec(source)) !== null) {
        const type = m[1].toLowerCase();
        const key = m[2];
        const body = m[3];

        const fields: Record<string, string> = {};

        // Extract fields with brace-balanced values
        const fieldRe =
            /(\w+)\s*=\s*\{([^{}]*(?:\{[^}]*\}[^{}]*)*)\}/gi;
        let fm: RegExpExecArray | null;
        while ((fm = fieldRe.exec(body)) !== null) {
            fields[fm[1].toLowerCase()] = cleanAccents(fm[2]);
        }

        const author = fields.author ?? "";
        const shortAuthor = extractShortAuthor(author);

        entries.push({
            key,
            type,
            author,
            shortAuthor,
            year: fields.year ?? "",
            title: fields.title ?? "",
            fields,
        });
    }

    return entries;
}

/**
 * Parse a BibTeX string and return a Map keyed by citation key.
 */
export function parseBibToMap(source: string): Map<string, BibEntry> {
    const entries = parseBibString(source);
    const map = new Map<string, BibEntry>();
    for (const entry of entries) {
        map.set(entry.key, entry);
    }
    return map;
}
