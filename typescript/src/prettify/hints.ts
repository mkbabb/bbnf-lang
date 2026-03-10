/**
 * Hint parsing + application.
 *
 * Takes PrettyHint[] from the grammar and resolves them into structural
 * formatting directives that the interpreter uses when building Doc trees.
 */

import type { Doc } from "@mkbabb/pprint";
import {
    group,
    indent,
    dedent,
    join,
    ifBreak,
    hardline,
    softline,
    text,
    concat,
    NULL_DOC,
} from "@mkbabb/pprint";
import type { PrettyHint } from "../types.js";

export interface ResolvedHints {
    separator?: Doc;
    breakSeparator?: Doc;
    wrapGroup?: boolean;
    wrapIndent?: boolean;
    wrapDedent?: boolean;
    splitDelim?: string;
    isOff?: boolean;
}

/**
 * Parse a `sep("...")` hint and extract the separator string.
 */
function extractSepString(hint: PrettyHint): string | undefined {
    if (hint.name === "sep" && hint.arg != null) {
        return hint.arg;
    }
    return undefined;
}

/**
 * Parse a `split("...")` hint and extract the delimiter string.
 */
function extractSplitDelim(hint: PrettyHint): string | undefined {
    if (hint.name === "split" && hint.arg != null) {
        return hint.arg;
    }
    return undefined;
}

/**
 * Resolve a list of PrettyHint[] into a ResolvedHints struct that drives
 * separator selection and structural wrapping.
 */
export function resolveHints(hints: PrettyHint[]): ResolvedHints {
    const resolved: ResolvedHints = {};

    for (const hint of hints) {
        switch (hint.name) {
            case "block":
            case "fast":
            case "hardbreak":
                resolved.separator = hardline;
                break;
            case "blankline":
                resolved.separator = concat(hardline, hardline);
                break;
            case "nobreak":
            case "compact":
                resolved.separator = text(" ");
                break;
            case "softbreak":
                resolved.separator = softline;
                break;
            case "group":
                resolved.wrapGroup = true;
                break;
            case "indent":
                resolved.wrapIndent = true;
                break;
            case "dedent":
                resolved.wrapDedent = true;
                break;
            case "off":
                resolved.isOff = true;
                resolved.separator = NULL_DOC;
                break;
            case "sep": {
                const sepText = extractSepString(hint);
                if (sepText != null) {
                    const trimmed = sepText.trimEnd();
                    resolved.separator = text(sepText);
                    resolved.breakSeparator = concat(text(trimmed), hardline);
                }
                break;
            }
            case "split": {
                const delim = extractSplitDelim(hint);
                if (delim != null) {
                    resolved.splitDelim = delim;
                }
                break;
            }
        }
    }

    return resolved;
}

/**
 * Compute the separator Doc to use between list/compound elements.
 *
 * When a `group` hint is present alongside a custom `sep(...)`, produces
 * an ifBreak Doc that uses the trimmed separator + hardline when breaking
 * and the original separator when inline.
 */
export function applySeparator(resolved: ResolvedHints): Doc {
    if (resolved.wrapGroup && resolved.breakSeparator) {
        return ifBreak(
            resolved.breakSeparator,
            resolved.separator ?? text(" "),
        );
    }
    if (resolved.separator != null) {
        return resolved.separator;
    }
    // Default: ifBreak(hardline, " ")
    return ifBreak(hardline, text(" "));
}

/**
 * Apply structural wrapping hints (indent, group, dedent) around a Doc.
 */
export function wrapDoc(doc: Doc, resolved: ResolvedHints): Doc {
    let result = doc;
    if (resolved.wrapDedent) result = dedent(result);
    if (resolved.wrapIndent) result = indent(result);
    if (resolved.wrapGroup) result = group(result);
    return result;
}
