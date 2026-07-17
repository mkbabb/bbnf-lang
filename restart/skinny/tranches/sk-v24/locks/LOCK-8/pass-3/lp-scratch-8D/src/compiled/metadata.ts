import type { SectionNode } from "../types/ast";
import type { PaperLabelInfo } from "../types/output";

export interface CompiledTocEntry {
    level: SectionNode["level"];
    number: string;
    title: string;
    page: number;
    anchor: string;
}

export interface CompiledLabelEntry {
    key: string;
    number: string;
    title: string;
    page: number;
    anchor: string;
    type?: PaperLabelInfo["type"];
}

export interface CompiledBibliographyItem {
    key: string;
    body: string;
}

export interface TheoremCounterConfig {
    envName: string;
    counterName: string;
    resetWithin: "section" | "chapter" | "subsection" | "none";
}

export interface CompiledPaperMetadata {
    tocEntries: CompiledTocEntry[];
    labels: Map<string, CompiledLabelEntry>;
    bibliography: CompiledBibliographyItem[];
    theoremCounters: Map<string, TheoremCounterConfig>;
    totalPages: number;
}

function isWhitespace(ch: string | undefined): boolean {
    return ch === " " || ch === "\t" || ch === "\n" || ch === "\r";
}

function skipWhitespace(source: string, offset: number): number {
    let cursor = offset;
    while (cursor < source.length && isWhitespace(source[cursor])) cursor += 1;
    return cursor;
}

function readBalanced(
    source: string,
    offset: number,
    open: string,
    close: string,
): { value: string; end: number } | null {
    if (source[offset] !== open) return null;
    let depth = 1;
    let cursor = offset + 1;

    while (cursor < source.length && depth > 0) {
        const ch = source[cursor];
        if (ch === "\\") {
            cursor += 2;
            continue;
        }
        if (ch === open) depth += 1;
        else if (ch === close) depth -= 1;
        cursor += 1;
    }

    if (depth !== 0) return null;

    return {
        value: source.slice(offset + 1, cursor - 1),
        end: cursor,
    };
}

function splitTopLevelBraceGroups(source: string): string[] {
    const groups: string[] = [];
    let cursor = 0;

    while (cursor < source.length) {
        cursor = skipWhitespace(source, cursor);
        const next = readBalanced(source, cursor, "{", "}");
        if (!next) break;
        groups.push(next.value);
        cursor = next.end;
    }

    return groups;
}

function readDigits(source: string, offset: number): { value: string; end: number } {
    let cursor = offset;
    while (cursor < source.length) {
        const ch = source[cursor];
        if (ch < "0" || ch > "9") break;
        cursor += 1;
    }
    return {
        value: source.slice(offset, cursor),
        end: cursor,
    };
}

function trimCompiledText(value: string): string {
    let text = value.trim();
    const ignorespaces = "\\ignorespaces";
    if (text.startsWith(ignorespaces)) {
        text = text.slice(ignorespaces.length).trimStart();
    }
    if (text.endsWith("\\relax")) {
        text = text.slice(0, -6).trimEnd();
    }
    return text;
}

function mapContentsLevel(value: string): SectionNode["level"] | null {
    if (value === "chapter") return "chapter";
    if (value === "section") return "section";
    if (value === "subsection") return "subsection";
    if (value === "subsubsection") return "subsubsection";
    return null;
}

function parseNumberedTitle(payload: string): { number: string; title: string } {
    const trimmed = trimCompiledText(payload);
    if (!trimmed.startsWith("\\numberline")) {
        return {
            number: "",
            title: trimmed,
        };
    }

    let cursor = "\\numberline".length;
    cursor = skipWhitespace(trimmed, cursor);
    const numberGroup = readBalanced(trimmed, cursor, "{", "}");
    if (!numberGroup) {
        return {
            number: "",
            title: trimmed,
        };
    }

    return {
        number: trimCompiledText(numberGroup.value),
        title: trimCompiledText(trimmed.slice(numberGroup.end)),
    };
}

function normalizeCounterReset(value: string): TheoremCounterConfig["resetWithin"] {
    if (value === "chapter") return "chapter";
    if (value === "section") return "section";
    if (value === "subsection") return "subsection";
    return "none";
}

export function parseLatexLogTotalPages(source: string): number | null {
    const marker = " pages";
    let cursor = 0;

    while (cursor < source.length) {
        const outputIndex = source.indexOf("Output written on ", cursor);
        if (outputIndex === -1) break;
        const parenIndex = source.indexOf("(", outputIndex);
        if (parenIndex === -1) break;
        const digits = readDigits(source, parenIndex + 1);
        if (digits.value && source.startsWith(marker, digits.end)) {
            return Number.parseInt(digits.value, 10);
        }
        cursor = outputIndex + 1;
    }

    return null;
}

export function parseLatexTocEntries(source: string): CompiledTocEntry[] {
    const entries: CompiledTocEntry[] = [];
    let cursor = 0;

    while (cursor < source.length) {
        const index = source.indexOf("\\contentsline", cursor);
        if (index === -1) break;

        let next = skipWhitespace(source, index + "\\contentsline".length);
        const levelGroup = readBalanced(source, next, "{", "}");
        if (!levelGroup) break;
        next = skipWhitespace(source, levelGroup.end);

        const titleGroup = readBalanced(source, next, "{", "}");
        if (!titleGroup) break;
        next = skipWhitespace(source, titleGroup.end);

        const pageGroup = readBalanced(source, next, "{", "}");
        if (!pageGroup) break;
        next = skipWhitespace(source, pageGroup.end);

        const anchorGroup = readBalanced(source, next, "{", "}");
        if (!anchorGroup) break;

        const mappedLevel = mapContentsLevel(trimCompiledText(levelGroup.value));
        if (mappedLevel) {
            const numberedTitle = parseNumberedTitle(titleGroup.value);
            const page = Number.parseInt(trimCompiledText(pageGroup.value), 10);
            entries.push({
                level: mappedLevel,
                number: numberedTitle.number,
                title: numberedTitle.title,
                page: Number.isFinite(page) ? page : 0,
                anchor: trimCompiledText(anchorGroup.value),
            });
        }

        cursor = anchorGroup.end;
    }

    return entries;
}

export function parseLatexAuxLabels(source: string): Map<string, CompiledLabelEntry> {
    const labels = new Map<string, CompiledLabelEntry>();
    let cursor = 0;

    while (cursor < source.length) {
        const index = source.indexOf("\\newlabel", cursor);
        if (index === -1) break;

        let next = skipWhitespace(source, index + "\\newlabel".length);
        const keyGroup = readBalanced(source, next, "{", "}");
        if (!keyGroup) break;
        next = skipWhitespace(source, keyGroup.end);

        const payloadGroup = readBalanced(source, next, "{", "}");
        if (!payloadGroup) break;
        const groups = splitTopLevelBraceGroups(payloadGroup.value);

        labels.set(trimCompiledText(keyGroup.value), {
            key: trimCompiledText(keyGroup.value),
            number: trimCompiledText(groups[0] ?? ""),
            page: Number.parseInt(trimCompiledText(groups[1] ?? "0"), 10) || 0,
            title: trimCompiledText(groups[2] ?? ""),
            anchor: trimCompiledText(groups[3] ?? ""),
        });

        cursor = payloadGroup.end;
    }

    return labels;
}

export function parseBibliographyItems(source: string): CompiledBibliographyItem[] {
    const items: CompiledBibliographyItem[] = [];
    const endMarker = "\\end{thebibliography}";
    let cursor = 0;

    while (cursor < source.length) {
        const itemIndex = source.indexOf("\\bibitem", cursor);
        if (itemIndex === -1) break;

        let next = skipWhitespace(source, itemIndex + "\\bibitem".length);
        if (source[next] === "[") {
            const opt = readBalanced(source, next, "[", "]");
            if (!opt) break;
            next = skipWhitespace(source, opt.end);
        }

        const keyGroup = readBalanced(source, next, "{", "}");
        if (!keyGroup) break;
        const bodyStart = keyGroup.end;

        const nextItemIndex = source.indexOf("\\bibitem", bodyStart);
        const endIndex = source.indexOf(endMarker, bodyStart);
        const bodyEnd =
            nextItemIndex === -1
                ? endIndex === -1
                    ? source.length
                    : endIndex
                : endIndex !== -1 && endIndex < nextItemIndex
                    ? endIndex
                    : nextItemIndex;

        items.push({
            key: trimCompiledText(keyGroup.value),
            body: source.slice(bodyStart, bodyEnd).trim(),
        });

        cursor = bodyEnd;
    }

    return items;
}

export function parseTheoremCounterConfigs(source: string): Map<string, TheoremCounterConfig> {
    const configs = new Map<string, TheoremCounterConfig>();
    let cursor = 0;

    while (cursor < source.length) {
        const index = source.indexOf("\\newtheorem", cursor);
        if (index === -1) break;

        let next = skipWhitespace(source, index + "\\newtheorem".length);
        const envGroup = readBalanced(source, next, "{", "}");
        if (!envGroup) break;
        next = skipWhitespace(source, envGroup.end);

        let sharedCounter: string | null = null;
        let resetWithin: TheoremCounterConfig["resetWithin"] = "none";

        const initialBracket = readBalanced(source, next, "[", "]");
        if (initialBracket) {
            sharedCounter = trimCompiledText(initialBracket.value) || null;
            next = skipWhitespace(source, initialBracket.end);
        }

        const titleGroup = readBalanced(source, next, "{", "}");
        if (!titleGroup) break;
        next = skipWhitespace(source, titleGroup.end);

        const trailingBracket = readBalanced(source, next, "[", "]");
        if (trailingBracket) {
            const trailingValue = trimCompiledText(trailingBracket.value);
            if (configs.has(trailingValue)) {
                sharedCounter = trailingValue;
            } else {
                resetWithin = normalizeCounterReset(trailingValue);
            }
            next = trailingBracket.end;
        }

        const envName = trimCompiledText(envGroup.value);
        const counterName = sharedCounter ?? envName;
        if (sharedCounter && configs.has(sharedCounter)) {
            resetWithin = configs.get(sharedCounter)?.resetWithin ?? resetWithin;
        }

        configs.set(envName, {
            envName,
            counterName,
            resetWithin,
        });

        cursor = next;
    }

    return configs;
}

export function createCompiledPaperMetadata(input: {
    tocSource: string;
    auxSource: string;
    bblSource: string;
    texSource: string;
    logSource: string;
}): CompiledPaperMetadata {
    return {
        tocEntries: parseLatexTocEntries(input.tocSource),
        labels: parseLatexAuxLabels(input.auxSource),
        bibliography: parseBibliographyItems(input.bblSource),
        theoremCounters: parseTheoremCounterConfigs(input.texSource),
        totalPages: parseLatexLogTotalPages(input.logSource) ?? 0,
    };
}
