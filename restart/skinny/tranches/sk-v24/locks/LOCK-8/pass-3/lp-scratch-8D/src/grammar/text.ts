/**
 * Text node parsers: plain text, special characters, accents, symbols.
 */

import { Parser, regex, string, any } from "@mkbabb/parse-that";
import type { TextNode, LatexNode } from "../types/ast";
import { ACCENT_MAPS, SYMBOL_MAP } from "../utils/accents";
import { braceBalanced } from "./primitives";

/** Create a TextNode. */
export function textNode(value: string): TextNode {
    return { type: "text", value };
}

/** Plain text (no special chars). Stops before ``, '', ---, -- */
export const plainText: Parser<TextNode> = regex(/[^\\{}$%~\[\]\n`'-]+/).map(textNode);

/** Single quote (not '') */
export const singleQuote: Parser<TextNode> = new Parser((state) => {
    if (state.src[state.offset] === "'" && state.src[state.offset + 1] !== "'") {
        state.value = textNode("'") as any;
        state.offset += 1;
        state.isError = false;
        return state;
    }
    state.isError = true;
    return state;
});

/** Single backtick (not ``) */
export const singleBacktick: Parser<TextNode> = new Parser((state) => {
    if (state.src[state.offset] === "`" && state.src[state.offset + 1] !== "`") {
        state.value = textNode("`") as any;
        state.offset += 1;
        state.isError = false;
        return state;
    }
    state.isError = true;
    return state;
});

/** Single newline (not a paragraph break). */
export const singleNewline: Parser<TextNode> = regex(/\n(?![ \t]*\n)/).map(() =>
    textNode(" "),
);

/** Single hyphen (not em/en dash). */
export const singleHyphen: Parser<TextNode> = new Parser((state) => {
    if (state.src[state.offset] === "-" && state.src[state.offset + 1] !== "-") {
        state.value = textNode("-") as any;
        state.offset += 1;
        state.isError = false;
        return state;
    }
    state.isError = true;
    return state;
});

/** Em dash: --- → — */
export const emDash: Parser<TextNode> = string("---").map(() => textNode("\u2014"));

/** En dash: -- → – (emDash is tried first in dispatch, so no ambiguity) */
export const enDash: Parser<TextNode> = new Parser((state) => {
    if (
        state.src[state.offset] === "-" &&
        state.src[state.offset + 1] === "-" &&
        state.src[state.offset + 2] !== "-"
    ) {
        state.value = textNode("\u2013") as any;
        state.offset += 2;
        state.isError = false;
        return state;
    }
    state.isError = true;
    return state;
});

/** Left double quote: `` → " */
export const leftDoubleQuote: Parser<TextNode> = string("``").map(() =>
    textNode("\u201C"),
);

/** Right double quote: '' → " */
export const rightDoubleQuote: Parser<TextNode> = string("''").map(() =>
    textNode("\u201D"),
);

/** Tilde: ~ → space */
export const tilde: Parser<TextNode> = string("~").map(() => textNode(" "));

/** Comment: %...\n → skip */
export const comment: Parser<null> = regex(/%[^\n]*\n?/).map(() => null);

/** Escaped special: \# \$ \% \& \_ \{ \} */
export const escapedSpecial: Parser<TextNode> = string("\\")
    .then(regex(/[#$%&_{}]/))
    .map(([_, ch]) => textNode(ch === "&" ? "&amp;" : ch));

/** Escaped space: \  */
export const escapedSpace: Parser<TextNode> = string("\\ ").map(() => textNode(" "));

/** Line break: \\ → HTML break */
export const lineBreak: Parser<TextNode> = string("\\\\")
    .skip(regex(/\s*/))
    .map(() => textNode("<br />"));

/** Spacing commands: \, \; \: \! */
export const spacingCmd: Parser<TextNode> = string("\\")
    .then(regex(/[,;:!]/))
    .map(() => textNode(" "));

/** \quad and \qquad */
export const quadCmd: Parser<TextNode> = regex(/\\q?quad/).map(() => textNode(" "));

/** Braced accent: \"{a} → ä */
export const bracedAccent: Parser<TextNode> = string("\\")
    .then(regex(/['"`^~]/))
    .then(braceBalanced())
    .map(([[_, cmd], inner]) => {
        const char = inner.replace(/[{}]/g, "");
        const map = ACCENT_MAPS[cmd];
        return textNode(map?.[char] ?? char);
    });

/** Unbraced accent: \"a → ä */
export const unbracedAccent: Parser<TextNode> = string("\\")
    .then(regex(/['"`^~]/))
    .then(regex(/[a-zA-Z]/))
    .map(([[_, cmd], char]) => {
        const map = ACCENT_MAPS[cmd];
        return textNode(map?.[char] ?? char);
    });

/** Named accent: \c{c} → ç */
export const namedAccent: Parser<TextNode> = string("\\")
    .then(regex(/[Hcuv]/))
    .then(braceBalanced())
    .map(([[_, cmd], inner]) => {
        const char = inner.replace(/[{}]/g, "");
        const map = ACCENT_MAPS[cmd];
        return textNode(map?.[char] ?? char);
    });

/** All accent parsers combined. */
export const accent: Parser<TextNode> = any(bracedAccent, unbracedAccent, namedAccent);

/** Named symbol commands in prose: \implies → ⇒, \infty → ∞, etc. */
export function symbolCommand(): Parser<TextNode> {
    const names = Object.keys(SYMBOL_MAP).sort((a, b) => b.length - a.length);
    const alts = names.map((name) =>
        regex(new RegExp(`\\\\${name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}(?![a-zA-Z])`))
            .map(() => textNode(SYMBOL_MAP[name])),
    );
    return any(...alts);
}
