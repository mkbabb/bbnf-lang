/**
 * AST → HTML transformer orchestrator.
 *
 * Delegates to:
 *   - CounterManager  (counters.ts)  — equation/theorem/section numbering
 *   - CommandRenderer  (commands.ts)  — \command → HTML dispatch
 *   - ContentExtractor (content.ts)   — walk nodes → ContentBlock[]
 *   - SectionBuilder   (sections.ts)  — build PaperSectionData[] hierarchy
 */

import type {
    LatexNode,
    ListNode,
    DescriptionNode,
} from "../types/ast";
import type {
    PaperSectionData,
    PaperLabelInfo,
} from "../types/output";
import type { BibEntry } from "../types/bibtex";
import type { CompiledPaperMetadata } from "../compiled/metadata";
import { LabelRegistry } from "./labels";
import { cleanRawLatex } from "./clean";

import { CounterManager } from "./counters";
import { CommandRenderer } from "./commands";
import type { RenderContext } from "./commands";
import { ContentExtractor } from "./content";
import { buildSections, tagLabelsInNodes, buildLabelMap } from "./sections";

// Re-export sub-modules for backwards compatibility
export { cleanRawLatex } from "./clean";
export { validateOutput } from "./validate";
export type { ValidationIssue } from "./validate";

export interface TransformOptions {
    /** KaTeX macros (merged with defaults). */
    macros?: Record<string, string>;
    /** Section callout mapping: section id → { text, link }. */
    callouts?: Record<string, { text: string; link: string }>;
    /** Custom math renderer. Default: KaTeX if available. */
    renderMath?: (tex: string, displayMode: boolean) => string;
    /** Bibliography entries for \cite resolution. */
    bibEntries?: Map<string, BibEntry>;
    /** Canonical compiled metadata from LaTeX artifacts. */
    compiledMetadata?: CompiledPaperMetadata;
}

/** Default KaTeX macros used in the fourier_paper. */
export const DEFAULT_MACROS: Record<string, string> = {};

// ── Transformer ─────────────────────────────────────────────────────

/**
 * Transform a LaTeX AST into structured PaperSectionData[].
 */
export class Transformer {
    private options: TransformOptions;
    private bibEntries: Map<string, BibEntry>;
    private compiledMetadata?: CompiledPaperMetadata;
    private labels: LabelRegistry;
    private citedKeys: string[] = [];
    private citedKeySet = new Set<string>();
    private sourceLabelTypes = new Map<string, PaperLabelInfo["type"]>();
    private labelSections = new Map<string, string>();
    private sectionAnchors = new Map<string, string>();
    private sectionNumbers = new Map<string, string>();
    private sectionTitles = new Map<string, string>();
    /** After transform(), contains label key → location mapping. */
    labelMap: Record<string, PaperLabelInfo> = {};

    private counters: CounterManager;
    private commandRenderer!: CommandRenderer;
    private contentExtractor!: ContentExtractor;

    constructor(options: TransformOptions = {}) {
        this.options = options;
        this.bibEntries = options.bibEntries ?? new Map();
        this.compiledMetadata = options.compiledMetadata;
        this.labels = new LabelRegistry();
        this.counters = new CounterManager(this.compiledMetadata);
        this.initModules();
    }

    private resolveLabel(key: string) {
        return this.compiledMetadata?.labels.get(key) ?? this.labels.resolve(key);
    }

    private resolveLabelNumber(key: string): string | undefined {
        return this.resolveLabel(key)?.number;
    }

    private labelKeyToAnchorId(key: string): string {
        return key.replace(/:/g, "-");
    }

    private inferLabelType(key: string, fallbackAnchor = ""): PaperLabelInfo["type"] {
        const explicit = this.compiledMetadata?.labels.get(key)?.type ?? this.sourceLabelTypes.get(key);
        if (explicit) return explicit;
        if (
            fallbackAnchor.startsWith("chapter.") ||
            fallbackAnchor.startsWith("section.") ||
            fallbackAnchor.startsWith("subsection.") ||
            fallbackAnchor.startsWith("subsubsection.")
        ) {
            return "section";
        }
        if (fallbackAnchor.startsWith("figure.")) return "figure";
        if (fallbackAnchor.startsWith("equation.")) return "equation";
        return "theorem";
    }

    private findSectionIdForLabel(key: string, title = "", anchor = ""): string {
        const direct = this.labelSections.get(key);
        if (direct) return direct;
        const byAnchor = this.sectionAnchors.get(anchor);
        if (byAnchor) return byAnchor;
        for (const [sectionId, sectionTitle] of this.sectionTitles.entries()) {
            if (sectionTitle === title) return sectionId;
        }
        return "";
    }

    private inferTheoremLikeType(envType: string): PaperLabelInfo["type"] {
        switch (envType) {
            case "definition":
            case "lemma":
            case "proposition":
            case "corollary":
            case "aside":
            case "example":
                return envType;
            default:
                return "theorem";
        }
    }

    /** Initialize sub-modules that need references to `this`. */
    private initModules(): void {
        const renderCtx: RenderContext = {
            resolveLabel: (key) => this.resolveLabel(key),
            nodesToHtml: (nodes) => this.nodesToHtml(nodes),
            recordCitations: (keys) => this.recordCitations(keys),
            bibEntries: this.bibEntries,
            citedKeys: this.citedKeys,
            compiledMetadata: this.compiledMetadata,
        };
        this.commandRenderer = new CommandRenderer(renderCtx);

        this.contentExtractor = new ContentExtractor({
            counters: this.counters,
            nodeToHtml: (node) => this.nodeToHtml(node),
            nodesToHtml: (nodes) => this.nodesToHtml(nodes),
            commandRenderer: this.commandRenderer,
            resolveLabelNumber: (key) => this.resolveLabelNumber(key),
            resolveLabel: (key) => this.resolveLabel(key),
            labelKeyToAnchorId: (key) => this.labelKeyToAnchorId(key),
            compiledLabels: this.compiledMetadata?.labels,
            labelSections: this.labelSections,
            findSectionIdForLabel: (key, title?, anchor?) =>
                this.findSectionIdForLabel(key, title, anchor),
            inferTheoremLikeType: (envType) => this.inferTheoremLikeType(envType),
        });
    }

    /** Transform a full AST (typically from parseLatex) into sections. */
    transform(nodes: LatexNode[]): PaperSectionData[] {
        this.citedKeys = [];
        this.citedKeySet.clear();
        this.labelMap = {};
        this.sourceLabelTypes.clear();
        this.labelSections.clear();
        this.sectionAnchors.clear();
        this.sectionNumbers.clear();
        this.sectionTitles.clear();

        // Pass 1: collect labels
        this.labels.collectLabels(nodes);
        this.collectSourceLabelTypes(nodes);

        // Initialize sub-modules (after label collection)
        this.initModules();

        // Extract the document body (inside \begin{document}...\end{document})
        let bodyNodes = nodes;
        for (const node of nodes) {
            if (
                node.type === "environment" &&
                node.name === "document"
            ) {
                bodyNodes = node.body;
                break;
            }
        }

        // Build section hierarchy via SectionBuilder
        const sections = buildSections(bodyNodes, {
            compiledMetadata: this.compiledMetadata,
            callouts: this.options.callouts ?? {},
            counters: this.counters,
            contentExtractor: this.contentExtractor,
            resolveLabelNumber: (key) => this.resolveLabelNumber(key),
            nodesToHtml: (nodes) => this.nodesToHtml(nodes),
            setSectionAnchor: (anchor, sectionId) =>
                this.sectionAnchors.set(anchor, sectionId),
            setSectionNumber: (number, sectionId) =>
                this.sectionNumbers.set(number, sectionId),
            setSectionTitle: (sectionId, title) =>
                this.sectionTitles.set(sectionId, title),
            tagLabelsInNodes: (nodes, sectionId) =>
                tagLabelsInNodes(nodes, sectionId, this.labelSections),
        });

        // Build the public labelMap
        this.labelMap = buildLabelMap({
            compiledMetadata: this.compiledMetadata,
            labels: this.labels,
            inferLabelType: (key, fallbackAnchor) => this.inferLabelType(key, fallbackAnchor),
            findSectionIdForLabel: (key, title?, anchor?) =>
                this.findSectionIdForLabel(key, title, anchor),
            sectionAnchors: this.sectionAnchors,
            labelKeyToAnchorId: (key) => this.labelKeyToAnchorId(key),
        });

        return sections;
    }

    private collectSourceLabelTypes(
        nodes: LatexNode[],
        contextType: PaperLabelInfo["type"] | null = null,
    ): void {
        for (const node of nodes) {
            if (node.type === "label") {
                this.sourceLabelTypes.set(node.key, contextType ?? "section");
                continue;
            }

            if (node.type === "section") {
                this.collectSourceLabelTypes(node.title, "section");
                continue;
            }

            if (node.type === "figure") {
                if (node.label) this.sourceLabelTypes.set(node.label, "figure");
                continue;
            }

            if (node.type === "theorem") {
                const theoremType = this.inferTheoremLikeType(node.envType);
                this.collectSourceLabelTypes(node.body, theoremType);
                continue;
            }

            if (node.type === "math" && node.display) {
                const source = node.rawValue ?? node.value;
                for (const match of source.matchAll(/\\label\{([^}]*)\}/g)) {
                    this.sourceLabelTypes.set(match[1], "equation");
                }
                continue;
            }

            if (node.type === "proof" || node.type === "quote" || node.type === "group") {
                this.collectSourceLabelTypes(node.body, contextType);
                continue;
            }

            if (node.type === "environment") {
                this.collectSourceLabelTypes(node.body, contextType);
                continue;
            }

            if (node.type === "list") {
                for (const item of node.items) this.collectSourceLabelTypes(item, contextType);
                continue;
            }

            if (node.type === "description") {
                for (const item of node.items) {
                    this.collectSourceLabelTypes(item.term, contextType);
                    this.collectSourceLabelTypes(item.body, contextType);
                }
            }
        }
    }

    private recordCitations(keys: string[]): void {
        for (const key of keys) {
            if (!key || this.citedKeySet.has(key)) continue;
            this.citedKeySet.add(key);
            this.citedKeys.push(key);
        }
    }

    /** Convert a single AST node to HTML string. */
    nodeToHtml(node: LatexNode): string {
        switch (node.type) {
            case "text":
                return cleanRawLatex(node.value, (key) =>
                    this.resolveLabelNumber(key),
                );

            case "math":
                if (node.display) {
                    return ""; // Display math extracted separately
                }
                return `$${node.value}$`;

            case "command":
                return this.commandRenderer.render(node);

            case "section":
                return ""; // Sections are structural, not inline

            case "theorem":
            case "figure":
            case "proof":
            case "codeBlock":
                return ""; // Extracted separately

            case "quote":
                return `<blockquote class="paper-quote">${this.nodesToHtml(node.body)}</blockquote>`;

            case "list":
                return this.listToHtml(node);

            case "description":
                return this.descriptionToHtml(node);

            case "environment":
                return this.nodesToHtml(node.body);

            case "group":
                return this.nodesToHtml(node.body);

            case "label":
                return ""; // Labels don't render

            case "comment":
                return "";

            case "paragraphBreak":
                return ""; // Handled by paragraph extraction

            default:
                return "";
        }
    }

    private listToHtml(node: ListNode): string {
        const tag = node.ordered ? "ol" : "ul";
        const items = node.items
            .map((item) => `<li>${this.nodesToHtml(item)}</li>`)
            .join("");
        return `<${tag} class="paper-list">${items}</${tag}>`;
    }

    private descriptionToHtml(node: DescriptionNode): string {
        let html = '<dl class="paper-description">';
        for (const item of node.items) {
            html += `<dt>${this.nodesToHtml(item.term)}</dt>`;
            html += `<dd>${this.nodesToHtml(item.body)}</dd>`;
        }
        html += "</dl>";
        return html;
    }

    /** Convert an array of nodes to HTML. */
    nodesToHtml(nodes: LatexNode[]): string {
        return nodes
            .map((n) => this.nodeToHtml(n))
            .join("")
            .replace(/  +/g, " ")
            .trim();
    }
}

/** Result of transformDocument including label map for cross-references. */
export interface TransformResult {
    sections: PaperSectionData[];
    labelMap: Record<string, PaperLabelInfo>;
}

/**
 * Full pipeline: parse LaTeX AST → PaperSectionData[] + label map.
 */
export function transformDocument(
    nodes: LatexNode[],
    options?: TransformOptions,
): TransformResult {
    const transformer = new Transformer(options);
    const sections = transformer.transform(nodes);
    return { sections, labelMap: transformer.labelMap };
}
