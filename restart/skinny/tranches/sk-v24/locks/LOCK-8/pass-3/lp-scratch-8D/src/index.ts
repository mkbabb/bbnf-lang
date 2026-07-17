/**
 * @mkbabb/latex-paper — Pure parser entry point (no KaTeX dependency).
 *
 * Exports the LaTeX and BibTeX parsers, AST types, and utilities.
 */

// AST types
export type {
    LatexNode,
    TextNode,
    MathNode,
    CommandNode,
    EnvironmentNode,
    GroupNode,
    CommentNode,
    ParagraphBreakNode,
    SectionNode,
    TheoremNode,
    ListNode,
    DescriptionNode,
    FigureNode,
    ProofNode,
    QuoteNode,
    LabelNode,
    CodeBlockNode,
} from "./types/ast";

// Output types
export type {
    PaperSectionData,
    PaperTheoremData,
    PaperFigureData,
    PaperLabelInfo,
    PaperNestedBlock,
    MathBlockData,
    ContentBlock,
    TheoremBlock,
    FigureBlock,
    CodeBlock,
    ProofBlock,
    PaperCodeBlockData,
    PaperProofData,
} from "./types/output";

// BibTeX types
export type { BibEntry } from "./types/bibtex";
export type {
    CompiledTocEntry,
    CompiledLabelEntry,
    CompiledBibliographyItem,
    TheoremCounterConfig,
    CompiledPaperMetadata,
} from "./compiled/metadata";

// Parsers
export { parseLatex, parseInlineString, astToText } from "./grammar/document";
export { parseBibString, parseBibToMap } from "./bibtex/parser";
export {
    parseLatexTocEntries,
    parseLatexAuxLabels,
    parseBibliographyItems,
    parseTheoremCounterConfigs,
    parseLatexLogTotalPages,
    createCompiledPaperMetadata,
} from "./compiled/metadata";

// Labels
export { LabelRegistry } from "./transform/labels";
export type { LabelInfo } from "./transform/labels";

// Paper helpers
export {
    flattenPaperSections,
    estimatePaperSectionHeight,
} from "./paper/flattenPaperSections";
export type { FlatPaperSection } from "./paper/flattenPaperSections";
