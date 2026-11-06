/**
 * Output validation — scan transformed PaperSectionData for
 * suspicious unprocessed LaTeX patterns.
 */

import type { PaperNestedBlock, PaperSectionData } from "../types/output";

/** Patterns that indicate unprocessed LaTeX in output. */
const SUSPICIOUS_PATTERNS: Array<{ pattern: RegExp; description: string }> = [
    { pattern: /\\['"`^~]\{[a-zA-Z]\}/, description: "Unprocessed braced accent" },
    { pattern: /\\['"`^~][a-zA-Z]/, description: "Unprocessed unbraced accent" },
    { pattern: /\\c\{[a-zA-Z]\}/, description: "Unprocessed cedilla" },
    { pattern: /\\text(?:it|bf|tt)\{/, description: "Unprocessed formatting command" },
    { pattern: /\\emph\{/, description: "Unprocessed \\emph" },
    { pattern: /\\(?:section|chapter|subsection)\*?\{/, description: "Unprocessed sectioning command" },
    { pattern: /\\begin\{/, description: "Unprocessed \\begin" },
    { pattern: /\\end\{/, description: "Unprocessed \\end" },
    { pattern: /(?<!\$[^$]*)\\(?:implies|iff|infty|ldots|cdots|dots|Rightarrow|Leftarrow|rightarrow|leftrightarrow)(?![a-zA-Z])(?![^$]*\$)/, description: "Unprocessed symbol command in prose" },
    { pattern: /``/, description: "Unprocessed left double quote" },
    { pattern: /''/, description: "Unprocessed right double quote" },
    { pattern: /(?<![- ])---(?![ -])/, description: "Unprocessed em-dash" },
];

export interface ValidationIssue {
    path: string;
    text: string;
    pattern: string;
    match: string;
}

/**
 * Scan transformed output for suspicious unprocessed LaTeX patterns.
 * Returns a list of issues found.
 */
export function validateOutput(sections: PaperSectionData[]): ValidationIssue[] {
    const issues: ValidationIssue[] = [];

    function scanText(text: string, path: string): void {
        // Strip math segments ($...$) before scanning — LaTeX inside math is valid
        const prose = text.replace(/\$[^$]*\$/g, "");
        for (const { pattern, description } of SUSPICIOUS_PATTERNS) {
            const match = prose.match(pattern);
            if (match) {
                issues.push({
                    path,
                    text: prose.substring(
                        Math.max(0, match.index! - 20),
                        Math.min(prose.length, match.index! + match[0].length + 20),
                    ),
                    pattern: description,
                    match: match[0],
                });
            }
        }
    }

    function scanNestedBlocks(blocks: PaperNestedBlock[], path: string): void {
        for (let i = 0; i < blocks.length; i++) {
            const block = blocks[i];
            if (typeof block === "string") {
                scanText(block, `${path}/block[${i}]`);
                continue;
            }
            if ("figure" in block) {
                scanText(block.figure.caption, `${path}/figure[${i}]/caption`);
            }
        }
    }

    function scanSection(section: PaperSectionData, prefix: string): void {
        const path = `${prefix}/${section.id}`;
        scanText(section.title, `${path}/title`);
        for (let i = 0; i < section.content.length; i++) {
            const block = section.content[i];
            if (typeof block === "string") {
                scanText(block, `${path}/paragraph[${i}]`);
            }
        }
        if (section.theorems) {
            for (let i = 0; i < section.theorems.length; i++) {
                const thm = section.theorems[i];
                if (thm.name) scanText(thm.name, `${path}/theorem[${i}]/name`);
                scanNestedBlocks(thm.content, `${path}/theorem[${i}]`);
            }
        }
        if (section.figures) {
            for (let i = 0; i < section.figures.length; i++) {
                scanText(section.figures[i].caption, `${path}/figure[${i}]/caption`);
            }
        }
        if (section.proofs) {
            for (let i = 0; i < section.proofs.length; i++) {
                scanNestedBlocks(section.proofs[i].content, `${path}/proof[${i}]`);
            }
        }
        if (section.subsections) {
            for (const sub of section.subsections) {
                scanSection(sub, path);
            }
        }
    }

    for (const section of sections) {
        scanSection(section, "");
    }

    return issues;
}
