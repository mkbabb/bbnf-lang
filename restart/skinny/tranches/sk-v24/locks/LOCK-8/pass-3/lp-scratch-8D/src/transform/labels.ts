/**
 * LabelRegistry: two-pass cross-reference resolution.
 * Pass 1: collect all \label{key} nodes and assign numbers.
 * Pass 2: resolve \ref{key} and \eqref{key} to their numbers.
 */

import type {
    LatexNode,
    SectionNode,
    TheoremNode,
    FigureNode,
    MathNode,
} from "../types/ast";

export interface LabelInfo {
    key: string;
    number: string;
    type: "section" | "theorem" | "figure" | "equation";
    /** Section ID (slug) containing this label, set during transform. */
    sectionId?: string;
}

export class LabelRegistry {
    private labels = new Map<string, LabelInfo>();
    private sectionCounters = { chapter: 0, section: 0, subsection: 0, subsubsection: 0 };
    private theoremCounter = 0;
    private figureCounter = 0;
    private equationCounter = 0;
    /** Current section number string — standalone \label nodes inherit this. */
    private currentSectionNumber = "";
    /** Current section type for standalone labels. */
    private currentSectionType: "section" = "section";

    /** Collect all labels from an AST (pass 1). */
    collectLabels(nodes: LatexNode[]): void {
        for (const node of nodes) {
            this.visitNode(node);
        }
    }

    /** Resolve a label key to its info. Returns undefined if not found. */
    resolve(key: string): LabelInfo | undefined {
        return this.labels.get(key);
    }

    /** Get all collected labels. */
    all(): Map<string, LabelInfo> {
        return new Map(this.labels);
    }

    private visitNode(node: LatexNode): void {
        switch (node.type) {
            case "section":
                this.visitSection(node);
                break;
            case "theorem":
                this.visitTheorem(node);
                break;
            case "figure":
                this.visitFigure(node);
                break;
            case "math":
                if (node.display) this.visitMath(node);
                break;
            case "label":
                // Standalone label — associate with current section context
                if (this.currentSectionNumber && !this.labels.has(node.key)) {
                    this.labels.set(node.key, {
                        key: node.key,
                        number: this.currentSectionNumber,
                        type: this.currentSectionType,
                    });
                }
                break;
            case "environment":
                if (node.body) this.collectLabels(node.body);
                break;
            case "proof":
                this.collectLabels(node.body);
                break;
            case "quote":
                this.collectLabels(node.body);
                break;
            case "list":
                for (const item of node.items) this.collectLabels(item);
                break;
            case "description":
                for (const item of node.items) {
                    this.collectLabels(item.term);
                    this.collectLabels(item.body);
                }
                break;
            default:
                break;
        }
    }

    private visitSection(node: SectionNode): void {
        const level = node.level;
        this.sectionCounters[level]++;

        // Reset sub-counters
        if (level === "chapter") {
            this.sectionCounters.section = 0;
            this.sectionCounters.subsection = 0;
            this.sectionCounters.subsubsection = 0;
            this.theoremCounter = 0;
            this.figureCounter = 0;
            this.equationCounter = 0;
        } else if (level === "section") {
            this.sectionCounters.subsection = 0;
            this.sectionCounters.subsubsection = 0;
        } else if (level === "subsection") {
            this.sectionCounters.subsubsection = 0;
        }

        // Build number string
        let number: string;
        if (level === "chapter") {
            number = String(this.sectionCounters.chapter);
        } else if (level === "section") {
            number = `${this.sectionCounters.chapter}.${this.sectionCounters.section}`;
        } else if (level === "subsection") {
            number = `${this.sectionCounters.chapter}.${this.sectionCounters.section}.${this.sectionCounters.subsection}`;
        } else {
            number = `${this.sectionCounters.chapter}.${this.sectionCounters.section}.${this.sectionCounters.subsection}.${this.sectionCounters.subsubsection}`;
        }

        // Update current section context so standalone labels inherit this number
        this.currentSectionNumber = number;

        // Check for label in the title nodes (rare but possible)
        for (const child of node.title) {
            if (child.type === "label") {
                this.labels.set(child.key, {
                    key: child.key,
                    number,
                    type: "section",
                });
            }
        }
    }

    private visitTheorem(node: TheoremNode): void {
        this.theoremCounter++;
        const chNum = this.sectionCounters.chapter || 1;
        const number = `${chNum}.${this.theoremCounter}`;

        for (const child of node.body) {
            if (child.type === "label") {
                this.labels.set(child.key, {
                    key: child.key,
                    number,
                    type: "theorem",
                });
            }
        }
    }

    private visitFigure(node: FigureNode): void {
        this.figureCounter++;
        const chNum = this.sectionCounters.chapter || 1;
        const number = `${chNum}.${this.figureCounter}`;

        if (node.label) {
            this.labels.set(node.label, {
                key: node.label,
                number,
                type: "figure",
            });
        }
    }

    private visitMath(node: MathNode): void {
        const chNum = this.sectionCounters.chapter || 1;
        const source = node.rawValue ?? node.value;

        // Find all \label{...} in the math content
        const matches = [...source.matchAll(/\\label\{([^}]+)\}/g)];
        if (matches.length > 0) {
            for (const m of matches) {
                this.equationCounter++;
                const number = `${chNum}.${this.equationCounter}`;
                this.labels.set(m[1], {
                    key: m[1],
                    number,
                    type: "equation",
                });
            }
        } else {
            // Unlabeled display math still increments the counter once
            this.equationCounter++;
        }
    }
}
