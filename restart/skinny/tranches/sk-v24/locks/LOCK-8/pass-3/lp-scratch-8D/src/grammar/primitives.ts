/**
 * Shared parser primitives and lazy registration for circular references.
 */

import { Parser, ParserState, regex, string } from "@mkbabb/parse-that";
import type { LatexNode } from "../types/ast";

// ── Lazy reference cell for inlineNode ──────────────────────────────
// Allows environments.ts and commands.ts to reference the top-level
// inline node parser without circular imports.

let _inlineNode: Parser<LatexNode | null> | null = null;

export function setInlineNode(p: Parser<LatexNode | null>): void {
    _inlineNode = p;
}

export function getInlineNode(): Parser<LatexNode | null> {
    if (!_inlineNode) throw new Error("inlineNode not registered yet");
    return _inlineNode;
}

/** Lazily reference the inline node parser. */
export const lazyInlineNode: Parser<LatexNode | null> = Parser.lazy(
    () => getInlineNode(),
);

// ── Utility parsers ─────────────────────────────────────────────────

/** Match optional whitespace (spaces, tabs, newlines). */
export const ws = regex(/\s*/);

/** Match at least one whitespace character. */
export const ws1 = regex(/\s+/);

/** Match a brace-balanced group {content}, returning the inner content as a string. */
export function braceBalanced(): Parser<string> {
    return new Parser((state) => {
        if (state.src[state.offset] !== "{") {
            state.isError = true;
            return state;
        }
        let depth = 1;
        let i = state.offset + 1;
        while (i < state.src.length && depth > 0) {
            if (state.src[i] === "{") depth++;
            else if (state.src[i] === "}") depth--;
            i++;
        }
        if (depth !== 0) {
            state.isError = true;
            return state;
        }
        state.value = state.src.slice(state.offset + 1, i - 1) as any;
        state.offset = i;
        state.isError = false;
        return state;
    });
}

/**
 * Parse a brace-delimited group {content} through the inline parser,
 * returning LatexNode[]. Unlike braceBalanced() which returns a raw string,
 * this produces a proper AST — accents, dashes, quotes, nested commands
 * are all parsed into their correct node types.
 *
 * Uses the lazily-registered inlineNode to dispatch, checking for the
 * closing } before each inline parse attempt to avoid consuming it.
 */
export function braceContent(): Parser<LatexNode[]> {
    return new Parser((state) => {
        if (state.src[state.offset] !== "{") {
            state.isError = true;
            return state;
        }
        state.offset++; // consume opening {

        const nodes: LatexNode[] = [];
        const nodeParser = getInlineNode();

        while (state.offset < state.src.length) {
            // Check for closing brace at current depth
            if (state.src[state.offset] === "}") {
                state.offset++; // consume closing }
                state.value = nodes as any;
                state.isError = false;
                return state;
            }

            const saved = state.offset;
            state.isError = false;

            nodeParser.parser(state);

            if (state.isError || state.offset === saved) {
                // Skip one character on failure
                state.offset = saved + 1;
                state.isError = false;
            } else if (state.value != null) {
                nodes.push(state.value as any);
            }
        }

        // Ran out of input — return what we have
        state.value = nodes as any;
        state.isError = false;
        return state;
    });
}

/** Match a bracket-balanced group [content], returning the inner content as a string. */
export function bracketBalanced(): Parser<string> {
    return new Parser((state) => {
        if (state.src[state.offset] !== "[") {
            state.isError = true;
            return state;
        }
        let depth = 1;
        let braceDepth = 0;
        let i = state.offset + 1;
        while (i < state.src.length && depth > 0) {
            const ch = state.src[i];
            if (ch === "{") braceDepth++;
            else if (ch === "}") braceDepth--;
            else if (braceDepth === 0) {
                if (ch === "[") depth++;
                else if (ch === "]") depth--;
            }
            i++;
        }
        if (depth !== 0) {
            state.isError = true;
            return state;
        }
        state.value = state.src.slice(state.offset + 1, i - 1) as any;
        state.offset = i;
        state.isError = false;
        return state;
    });
}

/** Scan forward until \end{envName} is found, returning raw content. */
export function rawUntilEnd(envName: string): Parser<string> {
    return new Parser((state) => {
        const target = `\\end{${envName}}`;
        const idx = state.src.indexOf(target, state.offset);
        if (idx === -1) {
            state.isError = true;
            return state;
        }
        state.value = state.src.slice(state.offset, idx);
        state.offset = idx + target.length;
        return state;
    });
}

/**
 * Parse inline nodes until \end{envName}, returning array of nodes.
 * Uses the lazily-registered inlineNode parser.
 */
export function nodesUntilEnd(envName: string): Parser<LatexNode[]> {
    return new Parser((state) => {
        const endTag = `\\end{${envName}}`;
        const nodes: LatexNode[] = [];
        const nodeParser = getInlineNode();

        while (state.offset < state.src.length) {
            // Check if we've reached \end{envName}
            if (state.src.startsWith(endTag, state.offset)) {
                state.offset += endTag.length;
                state.value = nodes as any;
                state.isError = false;
                return state;
            }

            // Try parsing an inline node
            const saved = state.offset;
            state.isError = false;

            nodeParser.parser(state);

            if (state.isError || state.offset === saved) {
                // Skip one character and continue
                state.offset = saved + 1;
                state.isError = false;
                continue;
            }

            if (state.value != null) {
                nodes.push(state.value as any);
            }
        }

        state.isError = true;
        return state;
    });
}

/**
 * Split list content on \item commands, returning arrays of nodes for each item.
 * Handles nesting depth tracking for nested lists.
 */
export function splitOnItem(body: LatexNode[]): LatexNode[][] {
    const items: LatexNode[][] = [];
    let current: LatexNode[] = [];

    for (const node of body) {
        if (
            node.type === "command" &&
            node.name === "item"
        ) {
            if (current.length > 0) {
                items.push(current);
            }
            current = [];
        } else {
            current.push(node);
        }
    }
    if (current.length > 0) {
        items.push(current);
    }

    // Filter out items with only whitespace/empty text nodes
    return items.filter((item) =>
        item.some(
            (node) =>
                node.type !== "text" ||
                node.value.trim().length > 0,
        ),
    );
}
