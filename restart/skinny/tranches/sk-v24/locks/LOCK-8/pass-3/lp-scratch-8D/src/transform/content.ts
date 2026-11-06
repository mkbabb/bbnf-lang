/**
 * ContentExtractor — walk AST nodes and build ContentBlock[] / PaperNestedBlock[].
 */

import type {
    LatexNode,
    MathNode,
    TheoremNode,
    ProofNode,
    CodeBlockNode,
} from "../types/ast";
import type {
    PaperTheoremData,
    PaperFigureData,
    PaperNestedBlock,
    MathBlockData,
    ContentBlock,
    PaperCodeBlockData,
    PaperProofData,
    PaperLabelInfo,
} from "../types/output";
import type { CounterManager } from "./counters";
import type { CommandRenderer } from "./commands";
import { parseInlineString } from "../grammar/document";

/** Callback to convert a single node to HTML (provided by the orchestrator). */
export type NodeToHtmlFn = (node: LatexNode) => string;
/** Callback to convert an array of nodes to HTML. */
export type NodesToHtmlFn = (nodes: LatexNode[]) => string;

/** Context shared across content-extraction calls for a single section. */
export interface ContentContext {
    sectionNumber: string;
    equationScope: string;
    equationCounters: Map<string, number>;
    theoremCounters: Map<string, number>;
}

export class ContentExtractor {
    private counters: CounterManager;
    private nodeToHtml: NodeToHtmlFn;
    private nodesToHtml: NodesToHtmlFn;
    private commandRenderer: CommandRenderer;
    private resolveLabelNumber: (key: string) => string | undefined;
    private resolveLabel: (key: string) => { number: string; type?: string } | undefined;
    private labelKeyToAnchorId: (key: string) => string;
    private compiledLabels?: Map<string, { number: string; title: string; anchor: string }>;
    private labelSections: Map<string, string>;
    private findSectionIdForLabel: (key: string, title?: string, anchor?: string) => string;
    private inferTheoremLikeType: (envType: string) => PaperLabelInfo["type"];

    constructor(deps: {
        counters: CounterManager;
        nodeToHtml: NodeToHtmlFn;
        nodesToHtml: NodesToHtmlFn;
        commandRenderer: CommandRenderer;
        resolveLabelNumber: (key: string) => string | undefined;
        resolveLabel: (key: string) => { number: string; type?: string } | undefined;
        labelKeyToAnchorId: (key: string) => string;
        compiledLabels?: Map<string, { number: string; title: string; anchor: string }>;
        labelSections: Map<string, string>;
        findSectionIdForLabel: (key: string, title?: string, anchor?: string) => string;
        inferTheoremLikeType: (envType: string) => PaperLabelInfo["type"];
    }) {
        this.counters = deps.counters;
        this.nodeToHtml = deps.nodeToHtml;
        this.nodesToHtml = deps.nodesToHtml;
        this.commandRenderer = deps.commandRenderer;
        this.resolveLabelNumber = deps.resolveLabelNumber;
        this.resolveLabel = deps.resolveLabel;
        this.labelKeyToAnchorId = deps.labelKeyToAnchorId;
        this.compiledLabels = deps.compiledLabels;
        this.labelSections = deps.labelSections;
        this.findSectionIdForLabel = deps.findSectionIdForLabel;
        this.inferTheoremLikeType = deps.inferTheoremLikeType;
    }

    /**
     * Extract all content blocks in document order: paragraphs, display math,
     * theorems, and figures interleaved as they appear in the source.
     */
    extractContent(nodes: LatexNode[], context: ContentContext): ContentBlock[] {
        const content: ContentBlock[] = [];
        let current: string[] = [];

        const flush = () => {
            const text = current.join("").replace(/  +/g, " ").trim();
            if (text.length > 0) {
                content.push(text);
            }
            current = [];
        };

        for (const node of nodes) {
            if (node.type === "section") continue;

            if (node.type === "theorem") {
                flush();
                const thm = this.transformTheorem(node as TheoremNode, context);
                if (thm) content.push({ theorem: thm });
                continue;
            }

            if (node.type === "figure" && (node as any).filename) {
                flush();
                const figLabel = (node as any).label as string | undefined;
                const figNumber = figLabel ? this.resolveLabelNumber(figLabel) : undefined;
                const fig: PaperFigureData = {
                    filename: (node as any).filename,
                    caption: (node as any).caption
                        ? this.nodesToHtml((node as any).caption)
                        : "",
                    ...(figLabel && { label: figLabel }),
                    ...(figNumber && { number: figNumber }),
                };
                content.push({ figure: fig });
                continue;
            }

            if (node.type === "proof") {
                flush();
                const proof = this.transformProof(node, context);
                if (proof) content.push({ proof });
                continue;
            }

            if (node.type === "codeBlock") {
                flush();
                const codeBlock = this.transformCodeBlock(node);
                if (codeBlock) content.push({ code: codeBlock });
                continue;
            }

            if (node.type === "command" && node.name === "bibliography") {
                flush();
                const bibliographyHtml = this.commandRenderer.render(node);
                if (bibliographyHtml) content.push(bibliographyHtml);
                continue;
            }

            if (node.type === "math" && node.display) {
                flush();
                content.push(this.transformDisplayMath(node, context));
                continue;
            }

            if (node.type === "paragraphBreak") {
                flush();
                continue;
            }

            current.push(this.nodeToHtml(node));
        }

        flush();
        return content;
    }

    extractNestedContent(
        nodes: LatexNode[],
        context: {
            equationScope: string;
            equationCounters: Map<string, number>;
        },
    ): PaperNestedBlock[] {
        const content: PaperNestedBlock[] = [];
        let current: string[] = [];

        const flush = () => {
            const text = current.join("").replace(/  +/g, " ").trim();
            if (text.length > 0) content.push(text);
            current = [];
        };

        for (const node of nodes) {
            if (node.type === "label") continue;

            if (node.type === "math" && node.display) {
                flush();
                content.push(this.transformDisplayMath(node, context));
                continue;
            }

            if (node.type === "figure" && node.filename) {
                flush();
                const nestedFigLabel = node.label as string | undefined;
                const nestedFigNumber = nestedFigLabel ? this.resolveLabelNumber(nestedFigLabel) : undefined;
                content.push({
                    figure: {
                        filename: node.filename,
                        caption: node.caption ? this.nodesToHtml(node.caption) : "",
                        ...(nestedFigLabel && { label: nestedFigLabel }),
                        ...(nestedFigNumber && { number: nestedFigNumber }),
                    },
                });
                continue;
            }

            if (node.type === "codeBlock") {
                flush();
                const codeBlock = this.transformCodeBlock(node);
                if (codeBlock) content.push({ code: codeBlock });
                continue;
            }

            if (node.type === "paragraphBreak") {
                flush();
                continue;
            }

            current.push(this.nodeToHtml(node));
        }

        flush();
        return content;
    }

    transformDisplayMath(
        node: MathNode,
        context: {
            equationScope: string;
            equationCounters: Map<string, number>;
        },
    ): MathBlockData {
        const labelKey = this.extractMathLabelKey(node);
        const resolvedNumber =
            labelKey != null
                ? this.resolveLabelNumber(labelKey)
                : undefined;
        const number =
            node.numbered === false
                ? undefined
                : this.counters.resolveEquationNumber(
                      context.equationCounters,
                      context.equationScope,
                      resolvedNumber,
                  );
        const anchorId = labelKey ? this.labelKeyToAnchorId(labelKey) : undefined;
        const id = anchorId;

        if (labelKey && resolvedNumber) {
            const labelInfo = this.compiledLabels?.get(labelKey);
            if (labelInfo && !this.labelSections.has(labelKey)) {
                const sectionId = this.findSectionIdForLabel(
                    labelKey,
                    labelInfo.title,
                    labelInfo.anchor,
                );
                if (sectionId) this.labelSections.set(labelKey, sectionId);
            }
        }

        return {
            tex: node.value,
            ...(id && { id }),
            ...(anchorId && { anchorId }),
            ...(number && { number }),
            numbered: node.numbered !== false,
        };
    }

    transformTheorem(
        node: TheoremNode,
        context: ContentContext,
    ): PaperTheoremData | null {
        const validTypes = new Set([
            "theorem", "definition", "lemma", "proposition",
            "corollary", "aside", "example",
        ]);

        if (!validTypes.has(node.envType)) return null;

        const content = this.extractNestedContent(
            node.body.filter((child) => child.type !== "proof"),
            context,
        );
        if (content.length === 0) return null;

        let label: string | undefined;
        for (const child of node.body) {
            if (child.type === "label") {
                label = child.key;
                break;
            }
        }

        const resolvedNumber = label ? this.resolveLabelNumber(label) : undefined;
        const number = this.counters.resolveTheoremNumber(
            node,
            context.sectionNumber,
            context.theoremCounters,
            resolvedNumber,
        );

        return {
            type: this.inferTheoremLikeType(node.envType) as PaperTheoremData["type"],
            ...(node.name && { name: this.nodesToHtml(node.name) }),
            ...(number && { number }),
            content,
            ...(label && { label }),
        };
    }

    transformProof(
        node: ProofNode,
        context: {
            equationScope: string;
            equationCounters: Map<string, number>;
        },
    ): PaperProofData | null {
        const content = this.extractNestedContent(node.body, context);
        if (content.length === 0) return null;

        const name = node.name
            ? this.nodesToHtml(parseInlineString(node.name))
            : undefined;

        return {
            ...(name && { name }),
            content,
        };
    }

    transformCodeBlock(node: CodeBlockNode): PaperCodeBlockData | null {
        const code = node.code.replace(/\s+$/, "");
        if (!code) return null;

        const caption = node.caption
            ? this.nodesToHtml(parseInlineString(node.caption))
            : undefined;
        const language = this.resolveCodeLanguage(node.language, caption);

        return {
            code,
            ...(caption && { caption }),
            ...(language && { language }),
        };
    }

    private extractMathLabelKey(node: { rawValue?: string; value: string }): string | null {
        const source = node.rawValue ?? node.value;
        const labelStart = source.indexOf("\\label{");
        if (labelStart === -1) return null;
        const start = labelStart + "\\label{".length;
        const end = source.indexOf("}", start);
        if (end === -1) return null;
        return source.slice(start, end);
    }

    private resolveCodeLanguage(explicit?: string, caption?: string): string | undefined {
        const normalized = explicit?.trim().toLowerCase();
        if (normalized) {
            if (normalized === "py") return "python";
            if (normalized === "rs") return "rust";
            if (normalized === "js") return "javascript";
            if (normalized === "ts") return "typescript";
            if (normalized === "sh" || normalized === "shell") return "bash";
            return normalized;
        }

        const lowerCaption = (caption ?? "").toLowerCase();
        if (lowerCaption.includes(".py")) return "python";
        if (lowerCaption.includes(".rs")) return "rust";
        if (lowerCaption.includes(".ts")) return "typescript";
        if (lowerCaption.includes(".js")) return "javascript";
        if (lowerCaption.includes(".json")) return "json";
        if (lowerCaption.includes(".sh")) return "bash";
        if (lowerCaption.includes(".cpp") || lowerCaption.includes(".cc")) return "cpp";
        return undefined;
    }
}
