/** Discriminated union of all LaTeX AST node types. */

export interface TextNode {
    type: "text";
    value: string;
}

export interface MathNode {
    type: "math";
    value: string;
    display: boolean;
    environment?: string;
    numbered?: boolean;
    /** Original content before label stripping — used by label collection. */
    rawValue?: string;
}

export interface CommandNode {
    type: "command";
    name: string;
    args: LatexNode[][];
    optArgs?: LatexNode[][];
}

export interface EnvironmentNode {
    type: "environment";
    name: string;
    args?: LatexNode[][];
    optArgs?: LatexNode[][];
    body: LatexNode[];
}

export interface GroupNode {
    type: "group";
    body: LatexNode[];
}

export interface CommentNode {
    type: "comment";
    value: string;
}

export interface ParagraphBreakNode {
    type: "paragraphBreak";
}

export interface SectionNode {
    type: "section";
    level: "chapter" | "section" | "subsection" | "subsubsection";
    starred: boolean;
    title: LatexNode[];
}

export interface TheoremNode {
    type: "theorem";
    envType: string;
    name?: LatexNode[];
    body: LatexNode[];
}

export interface ListNode {
    type: "list";
    ordered: boolean;
    items: LatexNode[][];
}

export interface DescriptionNode {
    type: "description";
    items: { term: LatexNode[]; body: LatexNode[] }[];
}

export interface FigureNode {
    type: "figure";
    filename?: string;
    caption?: LatexNode[];
    label?: string;
    options?: string;
}

export interface ProofNode {
    type: "proof";
    name?: string;
    body: LatexNode[];
}

export interface QuoteNode {
    type: "quote";
    body: LatexNode[];
}

export interface LabelNode {
    type: "label";
    key: string;
}

export interface CodeBlockNode {
    type: "codeBlock";
    code: string;
    caption?: string;
    language?: string;
}

export type LatexNode =
    | TextNode
    | MathNode
    | CommandNode
    | EnvironmentNode
    | GroupNode
    | CommentNode
    | ParagraphBreakNode
    | SectionNode
    | TheoremNode
    | ListNode
    | DescriptionNode
    | FigureNode
    | ProofNode
    | QuoteNode
    | LabelNode
    | CodeBlockNode;
