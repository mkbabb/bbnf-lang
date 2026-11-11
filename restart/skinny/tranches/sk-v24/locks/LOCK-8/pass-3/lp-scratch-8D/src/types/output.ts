export type PaperNestedBlock =
    | string
    | MathBlockData
    | FigureBlock
    | CodeBlock;

export interface PaperTheoremData {
    type:
        | "theorem"
        | "definition"
        | "lemma"
        | "proposition"
        | "corollary"
        | "aside"
        | "example";
    name?: string;
    /** Theorem number (e.g., "1.3") from the label registry. */
    number?: string;
    content: PaperNestedBlock[];
    label?: string;
}

export interface PaperFigureData {
    filename: string;
    caption: string;
    label?: string;
    /** Figure number (e.g., "2.1") from the label registry. */
    number?: string;
}

export interface PaperCodeBlockData {
    code: string;
    caption?: string;
    language?: string;
}

export interface PaperProofData {
    name?: string;
    content: PaperNestedBlock[];
}

/** A standalone display equation between paragraphs. */
export interface MathBlockData {
    tex: string;
    /** Element ID for scroll-to targeting (e.g. "eq-f3"). */
    id?: string;
    anchorId?: string;
    number?: string;
    numbered?: boolean;
}

/** Wrapper to embed a theorem in the content stream. */
export interface TheoremBlock {
    theorem: PaperTheoremData;
}

/** Wrapper to embed a figure in the content stream. */
export interface FigureBlock {
    figure: PaperFigureData;
}

/** Wrapper to embed a code listing in the content stream. */
export interface CodeBlock {
    code: PaperCodeBlockData;
}

/** Wrapper to embed a proof in the content stream. */
export interface ProofBlock {
    proof: PaperProofData;
}

/**
 * A content block in document order.
 * - `string` → paragraph HTML
 * - `MathBlockData` → display equation (has `.tex`)
 * - `TheoremBlock` → theorem/definition/lemma (has `.theorem`)
 * - `FigureBlock` → figure (has `.figure`)
 * - `CodeBlock` → code listing (has `.code`)
 * - `ProofBlock` → proof body (has `.proof`)
 */
export type ContentBlock =
    | string
    | MathBlockData
    | TheoremBlock
    | FigureBlock
    | CodeBlock
    | ProofBlock;

export interface PaperSectionData {
    id: string;
    number: string;
    title: string;
    /** Original LaTeX sectioning level: 0=chapter, 1=section, 2=subsection, 3=subsubsection. */
    sourceLevel?: number;
    /** Whether the source heading used a starred LaTeX form (for example, \subsection*{...}). */
    starred?: boolean;
    /**
     * Interleaved paragraphs and display equations in document order.
     * Strings are paragraph HTML; MathBlockData are display equations.
     */
    content: ContentBlock[];
    theorems?: PaperTheoremData[];
    figures?: PaperFigureData[];
    codeBlocks?: PaperCodeBlockData[];
    proofs?: PaperProofData[];
    subsections?: PaperSectionData[];
    callout?: { text: string; link: string };
    /** Summary of content counts (e.g., "3 theorems, 2 definitions"). */
    summary?: string;
}

/** Maps label keys to their resolved location in the paper. */
export interface PaperLabelInfo {
    /** Display number (e.g. "2.3", "1.5") */
    number: string;
    /** Type of labeled item */
    type:
        | "section"
        | "theorem"
        | "definition"
        | "lemma"
        | "proposition"
        | "corollary"
        | "aside"
        | "example"
        | "figure"
        | "equation";
    /** Section ID (slug) containing this label */
    sectionId: string;
    /** Element-level ID for precise scroll targeting (e.g. "thm-sturm_proof") */
    elementId?: string;
    /** Canonical DOM anchor target for the labeled element or containing section. */
    anchorId?: string;
}
