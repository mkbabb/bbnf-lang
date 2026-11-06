import type {
    CodeBlock,
    ContentBlock,
    FigureBlock,
    MathBlockData,
    PaperNestedBlock,
    PaperSectionData,
    ProofBlock,
    TheoremBlock,
} from "../types/output";

export interface FlatPaperSection {
    id: string;
    index: number;
    depth: number;
    sourceLevel: number;
    starred: boolean;
    parentId: string | null;
    rootId: string;
    rootIndex: number;
    section: PaperSectionData;
    estimatedHeight: number;
}

function stripMarkup(text: string): string {
    return text
        .replace(/<[^>]+>/g, " ")
        .replace(/&[a-z]+;/gi, " ")
        .replace(/\$[^$]*\$/g, " ")
        .replace(/\s+/g, " ")
        .trim();
}

function estimateTextHeight(text: string): number {
    const clean = stripMarkup(text);
    if (!clean) return 40;
    const lines = Math.ceil(clean.length / 180);
    return 40 + lines * 26;
}

function estimateBlockHeight(block: ContentBlock): number {
    if (typeof block === "string") {
        return estimateTextHeight(block);
    }
    if ("figure" in block) {
        const figure = (block as FigureBlock).figure;
        return 300 + estimateTextHeight(figure.caption);
    }
    if ("theorem" in block) {
        const theorem = (block as TheoremBlock).theorem;
        return 140 + estimateNestedContentHeight(theorem.content);
    }
    if ("code" in block) {
        const codeBlock = (block as CodeBlock).code;
        const lines = codeBlock.code.split("\n").length;
        return 96 + lines * 22 + (codeBlock.caption ? estimateTextHeight(codeBlock.caption) : 0);
    }
    if ("proof" in block) {
        const proof = (block as ProofBlock).proof;
        return 120 + estimateNestedContentHeight(proof.content);
    }
    const math = block as MathBlockData;
    return 104 + Math.min(120, Math.ceil(math.tex.length / 120) * 16);
}

function estimateNestedContentHeight(blocks: PaperNestedBlock[]): number {
    return blocks.reduce((sum, block) => {
        if (typeof block === "string") return sum + estimateTextHeight(block);
        return sum + estimateBlockHeight(block);
    }, 0);
}

export function estimatePaperSectionHeight(
    section: PaperSectionData,
    depth: number,
): number {
    const headingHeight = depth === 0 ? 124 : depth === 1 ? 88 : 72;
    const depthPadding = Math.max(0, 24 - depth * 4);
    const contentHeight = section.content.reduce(
        (sum, block) => sum + estimateBlockHeight(block),
        0,
    );
    const calloutHeight = section.callout ? 148 : 0;

    return Math.max(
        depth === 0 ? 320 : 220,
        Math.round(headingHeight + depthPadding + contentHeight + calloutHeight),
    );
}

export function flattenPaperSections(
    sections: PaperSectionData[],
): FlatPaperSection[] {
    const flat: FlatPaperSection[] = [];

    function walk(
        nodes: PaperSectionData[],
        depth: number,
        parentId: string | null,
        rootId: string,
        rootIndex: number,
    ) {
        for (const [nodeIndex, section] of nodes.entries()) {
            const nextRootId = depth === 0 ? section.id : rootId;
            const nextRootIndex = depth === 0 ? nodeIndex : rootIndex;
            flat.push({
                id: section.id,
                index: flat.length,
                depth,
                sourceLevel: section.sourceLevel ?? depth,
                starred: section.starred ?? false,
                parentId,
                rootId: nextRootId,
                rootIndex: nextRootIndex,
                section,
                estimatedHeight: estimatePaperSectionHeight(section, depth),
            });

            if (section.subsections?.length) {
                walk(
                    section.subsections,
                    depth + 1,
                    section.id,
                    nextRootId,
                    nextRootIndex,
                );
            }
        }
    }

    walk(sections, 0, null, "", 0);
    return flat;
}
