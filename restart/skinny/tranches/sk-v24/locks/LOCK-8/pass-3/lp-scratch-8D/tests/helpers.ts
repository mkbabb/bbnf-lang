/**
 * Test helper factories for creating AST nodes.
 */

import type {
    TextNode,
    MathNode,
    SectionNode,
    TheoremNode,
    CommandNode,
    LatexNode,
} from "../src/types/ast";

export function makeText(value: string): TextNode {
    return { type: "text", value };
}

export function makeMath(value: string, display = false): MathNode {
    return { type: "math", value, display };
}

export function makeSection(
    level: SectionNode["level"],
    title: string,
    starred = false,
): SectionNode {
    return {
        type: "section",
        level,
        starred,
        title: [makeText(title)],
    };
}

export function makeCommand(
    name: string,
    ...args: string[]
): CommandNode {
    return {
        type: "command",
        name,
        args: args.map((a) => [makeText(a)]),
    };
}

/** Create a minimal LaTeX document wrapping content. */
export function makeDoc(content: string): string {
    return `\\begin{document}\n${content}\n\\end{document}`;
}
