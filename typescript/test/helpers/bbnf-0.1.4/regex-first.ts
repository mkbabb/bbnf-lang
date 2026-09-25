import { CharSet } from "./charset.js";

/**
 * Extract possible first characters from a regex source. Returns null if
 * the pattern is too complex to analyze conservatively.
 *
 * Handles: [abc], [a-z], [^...], literal chars, alternation (a|b),
 * optional quantifiers (?, *).
 */
export function regexFirstChars(re: RegExp): CharSet | null {
    const src = re.source;
    if (!src) return null;

    const result = new CharSet();
    // Split top-level alternation branches (unescaped | outside brackets)
    const branches = splitTopLevelAlternation(src);

    for (const branch of branches) {
        const branchChars = extractBranchFirstChars(branch);
        if (!branchChars) return null;
        result.union(branchChars);
    }

    return result.isEmpty() ? null : result;
}

function splitTopLevelAlternation(src: string): string[] {
    const branches: string[] = [];
    let depth = 0; // parens
    let inBracket = false;
    let start = 0;

    for (let i = 0; i < src.length; i++) {
        const ch = src[i];
        if (ch === "\\" && i + 1 < src.length) {
            i++; // skip escaped char
            continue;
        }
        if (inBracket) {
            if (ch === "]") inBracket = false;
            continue;
        }
        if (ch === "[") {
            inBracket = true;
        } else if (ch === "(") {
            depth++;
        } else if (ch === ")") {
            depth--;
        } else if (ch === "|" && depth === 0) {
            branches.push(src.slice(start, i));
            start = i + 1;
        }
    }
    branches.push(src.slice(start));
    return branches;
}

/**
 * Extract first chars from a single alternation branch.
 * If the first atom is optional (?, *), continue to next atom.
 */
function extractBranchFirstChars(branch: string): CharSet | null {
    const result = new CharSet();
    let i = 0;

    while (i < branch.length) {
        const atomResult = extractAtomFirstChars(branch, i);
        if (!atomResult) return null;

        const { chars, end } = atomResult;
        result.union(chars);

        // Check if this atom has an optional quantifier (?, *, {0,...})
        if (end < branch.length) {
            const qch = branch[end];
            if (qch === "?" || qch === "*") {
                // Atom is optional -- its chars are possible, continue to next atom
                i = end + 1;
                // Skip lazy modifier
                if (i < branch.length && branch[i] === "?") i++;
                continue;
            }
        }

        // Atom is mandatory -- we're done
        break;
    }

    return result;
}

/**
 * Parse one atom at position i, return its possible chars and the end position.
 */
function extractAtomFirstChars(
    src: string,
    i: number,
): { chars: CharSet; end: number } | null {
    if (i >= src.length) return null;
    const ch = src[i];

    // Anchors -- skip them (zero-width)
    if (ch === "^" || ch === "$") {
        return extractAtomFirstChars(src, i + 1);
    }

    // Character class [...]
    if (ch === "[") {
        const end = findClosingBracket(src, i);
        if (end < 0) return null;
        const chars = parseCharClass(src.slice(i + 1, end));
        if (!chars) return null;
        return { chars, end: end + 1 };
    }

    // Group (...)
    if (ch === "(") {
        const end = findClosingParen(src, i);
        if (end < 0) return null;
        let inner = src.slice(i + 1, end);
        // Strip non-capturing group prefix
        if (inner.startsWith("?:")) inner = inner.slice(2);
        else if (inner.startsWith("?=") || inner.startsWith("?!")) {
            // Lookahead -- zero-width, skip
            return { chars: new CharSet(), end: end + 1 };
        }
        const chars = regexFirstCharsFromSource(inner);
        if (!chars) return null;
        return { chars, end: end + 1 };
    }

    // Escape sequences
    if (ch === "\\") {
        if (i + 1 >= src.length) return null;
        const next = src[i + 1];
        const chars = new CharSet();

        if (next === "d") {
            chars.addRange(48, 57); // 0-9
        } else if (next === "w") {
            chars.addRange(48, 57);
            chars.addRange(65, 90);
            chars.addRange(97, 122);
            chars.add(95); // _
        } else if (next === "s") {
            chars.add(9);
            chars.add(10);
            chars.add(13);
            chars.add(32);
        } else if (next === "D" || next === "W" || next === "S") {
            return null; // negated classes are complex
        } else {
            // Escaped literal
            chars.add(next.charCodeAt(0));
        }
        return { chars, end: i + 2 };
    }

    // Dot -- matches anything
    if (ch === ".") return null;

    // Literal character
    const chars = new CharSet();
    chars.add(ch.charCodeAt(0));
    return { chars, end: i + 1 };
}

function regexFirstCharsFromSource(src: string): CharSet | null {
    const branches = splitTopLevelAlternation(src);
    const result = new CharSet();
    for (const branch of branches) {
        const chars = extractBranchFirstChars(branch);
        if (!chars) return null;
        result.union(chars);
    }
    return result;
}

function findClosingBracket(src: string, start: number): number {
    for (let i = start + 1; i < src.length; i++) {
        if (src[i] === "\\" && i + 1 < src.length) {
            i++;
            continue;
        }
        if (src[i] === "]") return i;
    }
    return -1;
}

function findClosingParen(src: string, start: number): number {
    let depth = 0;
    for (let i = start; i < src.length; i++) {
        if (src[i] === "\\" && i + 1 < src.length) {
            i++;
            continue;
        }
        if (src[i] === "(") depth++;
        else if (src[i] === ")") {
            depth--;
            if (depth === 0) return i;
        }
    }
    return -1;
}

function parseCharClass(inner: string): CharSet | null {
    const negated = inner.startsWith("^");
    if (negated) inner = inner.slice(1);

    const chars = new CharSet();
    let i = 0;

    while (i < inner.length) {
        let code: number;
        if (inner[i] === "\\" && i + 1 < inner.length) {
            const next = inner[i + 1];
            if (next === "d") {
                chars.addRange(48, 57);
                i += 2;
                continue;
            } else if (next === "w") {
                chars.addRange(48, 57);
                chars.addRange(65, 90);
                chars.addRange(97, 122);
                chars.add(95);
                i += 2;
                continue;
            } else if (next === "s") {
                chars.add(9);
                chars.add(10);
                chars.add(13);
                chars.add(32);
                i += 2;
                continue;
            }
            // Map common escape sequences to their actual char codes
            const escapeMap: Record<string, number> = {
                n: 10,
                r: 13,
                t: 9,
                f: 12,
                v: 11,
                "0": 0,
            };
            code = escapeMap[next] ?? next.charCodeAt(0);
            i += 2;
        } else {
            code = inner.charCodeAt(i);
            i++;
        }

        // Check for range a-z
        if (i < inner.length - 1 && inner[i] === "-" && inner[i + 1] !== "]") {
            let endCode: number;
            if (inner[i + 1] === "\\" && i + 2 < inner.length) {
                endCode = inner.charCodeAt(i + 2);
                i += 3;
            } else {
                endCode = inner.charCodeAt(i + 1);
                i += 2;
            }
            chars.addRange(code, endCode);
        } else {
            chars.add(code);
        }
    }

    if (negated) {
        // Invert within ASCII printable range (32-126)
        const inverted = new CharSet();
        for (let c = 0; c < 128; c++) {
            if (!chars.has(c)) inverted.add(c);
        }
        return inverted;
    }

    return chars;
}
