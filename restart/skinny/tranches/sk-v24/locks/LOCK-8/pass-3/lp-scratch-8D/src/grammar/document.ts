/**
 * Top-level document parser: inlineNode dispatch + parseLatex().
 */

import { Parser, regex, string, any, ParserState } from "@mkbabb/parse-that";
import type { LatexNode, ParagraphBreakNode, GroupNode } from "../types/ast";
import { setInlineNode, braceContent, ws } from "./primitives";
import {
    plainText,
    singleNewline,
    singleHyphen,
    singleQuote,
    singleBacktick,
    emDash,
    enDash,
    leftDoubleQuote,
    rightDoubleQuote,
    tilde,
    comment,
    escapedSpecial,
    escapedSpace,
    lineBreak,
    spacingCmd,
    quadCmd,
    accent,
    symbolCommand,
    textNode,
} from "./text";
import {
    inlineMath,
    displayMathDollar,
    displayMathBracket,
    inlineMathParen,
} from "./math";
import {
    sectionCommand,
    formattingCommand,
    refCommand,
    labelCommand,
    hyperrefCommand,
    citeCommand,
    urlCommand,
    hrefCommand,
    paragraphCommand,
    bibliographyCommand,
    skipCommand,
    vspaceCommand,
    newtheoremCommand,
    inputCommand,
    preambleCommand,
    footnoteCommand,
    atCommand,
    itemCommand,
    includegraphicsCommand,
    captionCommand,
    unknownCommand,
} from "./commands";
import { environment } from "./environments";

// ── Paragraph break ─────────────────────────────────────────────────

/** Blank line: paragraph separator. */
const paragraphBreak: Parser<ParagraphBreakNode> = regex(/\n[ \t]*\n\s*/).map(
    () => ({ type: "paragraphBreak" as const }),
);

// ── Brace group ─────────────────────────────────────────────────────

/** Bare brace group: { ... } not part of a command → GroupNode. */
const braceGroup: Parser<LatexNode> = braceContent().map((nodes) => ({
    type: "group" as const,
    body: nodes,
}));

// ── Inline node dispatch ────────────────────────────────────────────

/**
 * The core dispatch parser: tries all inline parsers in priority order.
 * Returns null for nodes that should be stripped (comments, skip commands).
 */
const inlineNode: Parser<LatexNode | null> = any(
    // Comments (stripped)
    comment,

    // Paragraph breaks
    paragraphBreak,

    // Math (display before inline to match $$ before $)
    displayMathDollar,
    displayMathBracket,
    inlineMathParen,
    inlineMath,

    // Multi-char specials (before single-char parsers)
    emDash,
    enDash,
    leftDoubleQuote,
    rightDoubleQuote,

    // Environments (\begin before other \commands)
    environment,

    // Sectioning commands
    sectionCommand,

    // Accents (before formatting commands to avoid ambiguity)
    accent,

    // Known commands with arguments
    formattingCommand,
    labelCommand,
    citeCommand,
    urlCommand,
    hrefCommand,
    hyperrefCommand,
    refCommand,
    paragraphCommand,
    bibliographyCommand,
    footnoteCommand,
    itemCommand,
    includegraphicsCommand,
    captionCommand,

    // Symbols (\implies, \infty, etc.)
    symbolCommand(),

    // Skip/spacing commands (return null)
    skipCommand,
    vspaceCommand,
    newtheoremCommand,
    inputCommand,
    preambleCommand,
    atCommand,

    // Escaped chars
    escapedSpecial,
    escapedSpace,
    lineBreak,
    spacingCmd,
    quadCmd,

    // Unknown \command (catch-all for backslash commands)
    unknownCommand,

    // Tilde
    tilde,

    // Brace group (bare {})
    braceGroup,

    // Plain text and newlines
    plainText,
    singleHyphen,
    singleQuote,
    singleBacktick,
    singleNewline,

    // Last resort: skip one character
    regex(/./).map((ch) => textNode(ch)),
);

// Register the inlineNode so environments and other modules can reference it
setInlineNode(inlineNode);

// ── Top-level parsers ───────────────────────────────────────────────

/** Parse a complete LaTeX document string into an AST. */
export function parseLatex(source: string): LatexNode[] {
    const nodes: LatexNode[] = [];
    const state = new ParserState<LatexNode | null>(source);

    while (state.offset < source.length) {
        const saved = state.offset;
        state.isError = false;

        inlineNode.parser(state);

        if (state.isError || state.offset === saved) {
            // Skip one character on failure
            state.offset = saved + 1;
            state.isError = false;
            continue;
        }

        if (state.value != null) {
            nodes.push(state.value);
        }
    }

    return nodes;
}

/** Parse a LaTeX string meant for inline content (e.g., theorem name, caption). */
export function parseInlineString(source: string): LatexNode[] {
    return parseLatex(source);
}

/**
 * Flatten an AST to plain text, stripping all commands and environments.
 * Useful for generating slugs, TOC entries, etc.
 */
export function astToText(nodes: LatexNode[]): string {
    const parts: string[] = [];

    for (const node of nodes) {
        switch (node.type) {
            case "text":
                parts.push(node.value);
                break;
            case "math":
                parts.push(node.display ? "" : `$${node.value}$`);
                break;
            case "command":
                if (node.name === "paragraph") {
                    parts.push(astToText(node.args[0] ?? []));
                } else if (
                    ["textit", "textbf", "emph", "texttt", "text"].includes(
                        node.name,
                    )
                ) {
                    parts.push(astToText(node.args[0] ?? []));
                }
                break;
            case "section":
                parts.push(astToText(node.title));
                break;
            case "group":
                parts.push(astToText(node.body));
                break;
            case "paragraphBreak":
                parts.push("\n\n");
                break;
            default:
                break;
        }
    }

    return parts.join("").replace(/  +/g, " ").trim();
}
