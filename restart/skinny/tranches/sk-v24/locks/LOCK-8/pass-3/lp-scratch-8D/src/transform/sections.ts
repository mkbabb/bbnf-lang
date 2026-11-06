/**
 * SectionBuilder — build PaperSectionData[] hierarchy from flat AST nodes.
 */

import type { LatexNode, SectionNode } from "../types/ast";
import type {
    PaperSectionData,
    PaperLabelInfo,
    ContentBlock,
    TheoremBlock,
    FigureBlock,
    CodeBlock,
    ProofBlock,
} from "../types/output";
import type { CompiledTocEntry, CompiledPaperMetadata } from "../compiled/metadata";
import type { CounterManager } from "./counters";
import type { ContentExtractor, ContentContext } from "./content";
import { renderCompiledBibliography } from "./commands";
import { astToText } from "../grammar/document";
import { cleanRawLatex, slugify } from "./clean";

export interface SectionBuilderDeps {
    compiledMetadata?: CompiledPaperMetadata;
    callouts: Record<string, { text: string; link: string }>;
    counters: CounterManager;
    contentExtractor: ContentExtractor;
    resolveLabelNumber: (key: string) => string | undefined;
    nodesToHtml: (nodes: LatexNode[]) => string;
    /** Callbacks to update label tracking state on the orchestrator. */
    setSectionAnchor: (anchor: string, sectionId: string) => void;
    setSectionNumber: (number: string, sectionId: string) => void;
    setSectionTitle: (sectionId: string, title: string) => void;
    tagLabelsInNodes: (nodes: LatexNode[], sectionId: string) => void;
}

const LEVEL_MAP = {
    chapter: 0,
    section: 1,
    subsection: 2,
    subsubsection: 3,
} satisfies Record<SectionNode["level"], number>;

/** Build the hierarchical PaperSectionData[] tree from a flat node list. */
export function buildSections(
    nodes: LatexNode[],
    deps: SectionBuilderDeps,
): PaperSectionData[] {
    interface SectionRange {
        level: number;
        title: string;
        starred: boolean;
        headingIndex: number;
        startIdx: number;
        endIdx: number;
        compiledEntry?: CompiledTocEntry;
    }

    const ranges: SectionRange[] = [];

    for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];
        if (node.type !== "section") continue;
        ranges.push({
            level: LEVEL_MAP[node.level],
            title: cleanRawLatex(astToText(node.title), (key) =>
                deps.resolveLabelNumber(key),
            ),
            starred: node.starred,
            headingIndex: i,
            startIdx: i + 1,
            endIdx: nodes.length,
        });
    }

    for (let i = 0; i < ranges.length - 1; i++) {
        ranges[i].endIdx = ranges[i + 1].headingIndex;
    }

    const compiledTocEntries = deps.compiledMetadata?.tocEntries ?? [];
    if (compiledTocEntries.length > 0) {
        const trackedRanges = ranges.filter((range) => !range.starred);
        if (compiledTocEntries.length !== trackedRanges.length) {
            throw new Error(
                `latex-paper: compiled TOC mismatch (sections=${trackedRanges.length}, toc=${compiledTocEntries.length}).`,
            );
        }
        for (let i = 0; i < trackedRanges.length; i++) {
            const compiledEntry = compiledTocEntries[i];
            const expectedLevel = trackedRanges[i].level;
            const actualLevel = LEVEL_MAP[compiledEntry.level];
            if (expectedLevel !== actualLevel) {
                throw new Error(
                    `latex-paper: compiled TOC level mismatch at section ${i} (${compiledEntry.title}).`,
                );
            }
            trackedRanges[i].compiledEntry = compiledEntry;
        }
    }

    const bibliographyIndex = nodes.findIndex(
        (node) => node.type === "command" && node.name === "bibliography",
    );
    const topLevel: PaperSectionData[] = [];
    const levelParents = new Map<number, PaperSectionData>();
    const equationCounters = new Map<string, number>();
    const fallbackCounters = { chapter: 0, section: 0, subsection: 0, subsubsection: 0 };
    let bibliographyInserted = false;

    const insertBibliography = () => {
        if (bibliographyInserted || !deps.compiledMetadata?.bibliography.length) return;
        const bibliography = buildBibliographySection(topLevel, deps);
        topLevel.push(bibliography);
        bibliographyInserted = true;
        levelParents.clear();
    };

    for (const range of ranges) {
        if (
            bibliographyIndex !== -1 &&
            !bibliographyInserted &&
            bibliographyIndex < range.headingIndex
        ) {
            insertBibliography();
        }

        const bodyNodes = nodes.slice(range.startIdx, range.endIdx);
        const number =
            (range.starred && !range.compiledEntry
                ? ""
                : range.compiledEntry?.number) ??
            deps.counters.fallbackSectionNumber(range.level, fallbackCounters);
        const id = slugify(range.title);
        const equationScope = deps.counters.getEquationScope(number, range.level);
        const context: ContentContext = {
            sectionNumber: number,
            equationScope,
            equationCounters,
            theoremCounters: new Map<string, number>(),
        };
        const content = deps.contentExtractor.extractContent(bodyNodes, context);

        const theorems = content
            .filter((b): b is TheoremBlock => typeof b === "object" && b !== null && "theorem" in b)
            .map((b) => b.theorem);
        const figures = content
            .filter((b): b is FigureBlock => typeof b === "object" && b !== null && "figure" in b)
            .map((b) => b.figure);
        const codeBlocks = content
            .filter((b): b is CodeBlock => typeof b === "object" && b !== null && "code" in b)
            .map((b) => b.code);
        const proofs = content
            .filter((b): b is ProofBlock => typeof b === "object" && b !== null && "proof" in b)
            .map((b) => b.proof);
        const callout = deps.callouts[id];

        deps.tagLabelsInNodes(bodyNodes, id);

        const section: PaperSectionData = {
            id,
            number,
            title: range.title,
            sourceLevel: range.level,
            starred: range.starred,
            content,
            ...(theorems.length > 0 && { theorems }),
            ...(figures.length > 0 && { figures }),
            ...(codeBlocks.length > 0 && { codeBlocks }),
            ...(proofs.length > 0 && { proofs }),
            ...(callout && { callout }),
        };

        if (range.compiledEntry?.anchor) {
            deps.setSectionAnchor(range.compiledEntry.anchor, id);
        }
        deps.setSectionNumber(number, id);
        deps.setSectionTitle(id, range.compiledEntry?.title || range.title);

        let parent: PaperSectionData | null = null;
        for (let parentLevel = range.level - 1; parentLevel >= 0; parentLevel--) {
            const candidate = levelParents.get(parentLevel);
            if (candidate) {
                parent = candidate;
                break;
            }
        }

        if (!parent || (range.level === 1 && !levelParents.has(0))) {
            topLevel.push(section);
        } else {
            if (!parent.subsections) parent.subsections = [];
            parent.subsections.push(section);
        }

        for (const key of [...levelParents.keys()]) {
            if (key >= range.level) levelParents.delete(key);
        }
        levelParents.set(range.level, section);
    }

    if (!bibliographyInserted && bibliographyIndex !== -1) {
        insertBibliography();
    }

    generateSummaries(topLevel);
    cleanEmpty(topLevel);

    return topLevel;
}

/** Tag label nodes found in AST nodes with a section ID. */
export function tagLabelsInNodes(
    nodes: LatexNode[],
    sectionId: string,
    labelSections: Map<string, string>,
): void {
    for (const node of nodes) {
        if (node.type === "label") {
            labelSections.set(node.key, sectionId);
        } else if (node.type === "theorem") {
            tagLabelsInNodes(node.body, sectionId, labelSections);
        } else if (node.type === "figure") {
            if (node.label) {
                labelSections.set(node.label, sectionId);
            }
        } else if (node.type === "math" && node.display) {
            const source = node.rawValue ?? node.value;
            for (const match of source.matchAll(/\\label\{([^}]*)\}/g)) {
                labelSections.set(match[1], sectionId);
            }
        } else if (node.type === "environment") {
            tagLabelsInNodes(node.body, sectionId, labelSections);
        } else if (node.type === "proof") {
            tagLabelsInNodes(node.body, sectionId, labelSections);
        } else if (node.type === "list") {
            for (const item of node.items) tagLabelsInNodes(item, sectionId, labelSections);
        }
    }
}

/** Build the public labelMap from the registry. */
export function buildLabelMap(deps: {
    compiledMetadata?: CompiledPaperMetadata;
    labels: { all(): Map<string, { number: string }> };
    inferLabelType: (key: string, fallbackAnchor: string) => PaperLabelInfo["type"];
    findSectionIdForLabel: (key: string, title?: string, anchor?: string) => string;
    sectionAnchors: Map<string, string>;
    labelKeyToAnchorId: (key: string) => string;
}): Record<string, PaperLabelInfo> {
    const labelMap: Record<string, PaperLabelInfo> = {};
    const compiledEntries = deps.compiledMetadata?.labels ?? new Map();

    if (compiledEntries.size > 0) {
        for (const [key, entry] of compiledEntries.entries()) {
            const type = deps.inferLabelType(key, entry.anchor);
            const sectionId = deps.findSectionIdForLabel(key, entry.title, entry.anchor);
            const anchorId =
                type === "section"
                    ? deps.sectionAnchors.get(entry.anchor) ?? sectionId
                    : deps.labelKeyToAnchorId(key);
            labelMap[key] = {
                number: entry.number,
                type,
                sectionId,
                ...(type !== "section" && { elementId: anchorId }),
                ...(anchorId && { anchorId }),
            };
        }
        return labelMap;
    }

    for (const [key, info] of deps.labels.all()) {
        const type = deps.inferLabelType(key, "");
        const sectionId = deps.findSectionIdForLabel(key);
        const anchorId =
            type === "section"
                ? sectionId
                : deps.labelKeyToAnchorId(key);
        labelMap[key] = {
            number: info.number,
            type,
            sectionId,
            ...(type !== "section" && { elementId: anchorId }),
            ...(anchorId && { anchorId }),
        };
    }

    return labelMap;
}

// ── Internal helpers ──────────────────────────────────────────────────

function buildBibliographySection(
    topLevel: PaperSectionData[],
    deps: SectionBuilderDeps,
): PaperSectionData {
    return {
        id: "bibliography",
        number: deps.counters.nextBibliographyNumber(topLevel),
        title: "Bibliography",
        sourceLevel: 0,
        content: [renderCompiledBibliography(deps.compiledMetadata, deps.nodesToHtml)],
    };
}

function generateSummaries(sections: PaperSectionData[]): void {
    for (const section of sections) {
        if (section.subsections) generateSummaries(section.subsections);

        const counts: Record<string, number> = {};
        function countItems(s: PaperSectionData) {
            for (const t of s.theorems ?? []) {
                counts[t.type] = (counts[t.type] ?? 0) + 1;
            }
            for (const sub of s.subsections ?? []) countItems(sub);
        }
        countItems(section);

        const parts = Object.entries(counts)
            .map(([type, n]) => `${n} ${type}${n > 1 ? "s" : ""}`)
            .join(", ");
        if (parts) section.summary = parts;
    }
}

function cleanEmpty(sections: PaperSectionData[]): void {
    for (const s of sections) {
        if (s.subsections && s.subsections.length === 0) {
            delete s.subsections;
        } else if (s.subsections) {
            cleanEmpty(s.subsections);
        }
    }
}
