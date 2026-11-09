/**
 * Environment parsers: \begin{X}...\end{X} dispatch.
 * Context-sensitive: env name captured by .chain() determines body parser.
 */

import { Parser, regex, string, any } from "@mkbabb/parse-that";
import type {
    LatexNode,
    EnvironmentNode,
    TheoremNode,
    ListNode,
    DescriptionNode,
    FigureNode,
    ProofNode,
    QuoteNode,
    CodeBlockNode,
} from "../types/ast";
import {
    braceBalanced,
    bracketBalanced,
    ws,
    rawUntilEnd,
    nodesUntilEnd,
    splitOnItem,
} from "./primitives";
import { isMathEnv, mathEnvBody } from "./math";

// ── Theorem-like environments ───────────────────────────────────────

const THEOREM_TYPES = new Set([
    "theorem",
    "definition",
    "lemma",
    "proposition",
    "corollary",
    "aside",
    "example",
    "remark",
    "notation",
]);

function parseTheoremEnv(envName: string): Parser<TheoremNode> {
    return ws
        .next(bracketBalanced().opt())
        .skip(ws)
        .then(nodesUntilEnd(envName))
        .map(([name, body]) => ({
            type: "theorem" as const,
            envType: envName,
            ...(name != null && {
                name: [{ type: "text" as const, value: name }],
            }),
            body,
        }));
}

// ── List environments ───────────────────────────────────────────────

function parseListEnv(envName: string, ordered: boolean): Parser<ListNode> {
    return nodesUntilEnd(envName).map((body) => ({
        type: "list" as const,
        ordered,
        items: splitOnItem(body),
    }));
}

// ── Description environment ─────────────────────────────────────────

function parseDescriptionEnv(): Parser<DescriptionNode> {
    return nodesUntilEnd("description").map((body) => {
        const rawItems = splitOnItem(body);
        const items: DescriptionNode["items"] = [];

        for (const itemNodes of rawItems) {
            // The first node might contain the [term] from \item[term]
            // which was parsed as an optArg on the item command
            const term: LatexNode[] = [];
            const bodyNodes: LatexNode[] = [];
            let foundTerm = false;

            for (const node of itemNodes) {
                if (
                    !foundTerm &&
                    node.type === "command" &&
                    node.name === "item" &&
                    node.optArgs?.length
                ) {
                    term.push(...node.optArgs[0]);
                    foundTerm = true;
                } else {
                    bodyNodes.push(node);
                }
            }

            items.push({ term, body: bodyNodes });
        }

        return {
            type: "description" as const,
            items,
        };
    });
}

// ── Figure environment ──────────────────────────────────────────────

function parseFigureEnv(): Parser<FigureNode> {
    return nodesUntilEnd("figure").map((body) => {
        let filename: string | undefined;
        let caption: LatexNode[] | undefined;
        let label: string | undefined;
        let options: string | undefined;

        for (const node of body) {
            if (node.type === "command") {
                if (node.name === "includegraphics") {
                    let f = node.args[0]?.[0];
                    if (f && f.type === "text") {
                        filename = f.value.replace(/^.*\//, "");
                        if (!filename.includes(".")) filename += ".png";
                        filename = filename.replace(/\.pdf$/, ".png");
                    }
                    if (node.optArgs?.[0]?.[0]?.type === "text") {
                        options = node.optArgs[0][0].value;
                    }
                } else if (node.name === "caption") {
                    caption = node.args[0];
                }
            } else if (node.type === "label") {
                label = node.key;
            }
        }

        return {
            type: "figure" as const,
            filename,
            caption,
            label,
            options,
        };
    });
}

// ── Proof environment ───────────────────────────────────────────────

function parseProofEnv(): Parser<ProofNode> {
    return ws
        .next(bracketBalanced().opt())
        .skip(ws)
        .then(nodesUntilEnd("proof"))
        .map(([name, body]) => ({
            type: "proof" as const,
            ...(name != null && { name }),
            body,
        }));
}

// ── Quote environment ───────────────────────────────────────────────

function parseQuoteEnv(): Parser<QuoteNode> {
    return nodesUntilEnd("quote").map((body) => ({
        type: "quote" as const,
        body,
    }));
}

function splitOptionPairs(options: string): string[] {
    const parts: string[] = [];
    let current = "";
    let braceDepth = 0;
    let bracketDepth = 0;
    let parenDepth = 0;

    for (const ch of options) {
        if (ch === "," && braceDepth === 0 && bracketDepth === 0 && parenDepth === 0) {
            if (current.trim()) parts.push(current.trim());
            current = "";
            continue;
        }

        current += ch;
        if (ch === "{") braceDepth++;
        else if (ch === "}") braceDepth = Math.max(0, braceDepth - 1);
        else if (ch === "[") bracketDepth++;
        else if (ch === "]") bracketDepth = Math.max(0, bracketDepth - 1);
        else if (ch === "(") parenDepth++;
        else if (ch === ")") parenDepth = Math.max(0, parenDepth - 1);
    }

    if (current.trim()) parts.push(current.trim());
    return parts;
}

function unwrapOptionValue(value: string): string {
    const trimmed = value.trim();
    if (
        (trimmed.startsWith("{") && trimmed.endsWith("}")) ||
        (trimmed.startsWith('"') && trimmed.endsWith('"'))
    ) {
        return trimmed.slice(1, -1).trim();
    }
    return trimmed;
}

function parseCodeBlockEnv(): Parser<CodeBlockNode> {
    return ws
        .next(bracketBalanced().opt())
        .skip(ws)
        .then(rawUntilEnd("lstlisting"))
        .map(([options, code]) => {
            const attrs: Pick<CodeBlockNode, "caption" | "language"> = {};

            if (options) {
                for (const part of splitOptionPairs(options)) {
                    const eqIdx = part.indexOf("=");
                    if (eqIdx === -1) continue;
                    const key = part.slice(0, eqIdx).trim().toLowerCase();
                    const value = unwrapOptionValue(part.slice(eqIdx + 1));
                    if (key === "caption" && value) {
                        attrs.caption = value;
                    } else if (key === "language" && value) {
                        attrs.language = value;
                    }
                }
            }

            return {
                type: "codeBlock" as const,
                code: code.replace(/^\n/, "").replace(/\n\s*$/, ""),
                ...attrs,
            };
        });
}

// ── Raw/skip environments ───────────────────────────────────────────

const SKIP_ENVS = new Set([
    "tabular",
    "tabular*",
    "table",
    "table*",
    "tikzpicture",
    "abstract",
    "titlepage",
    "minipage",
    "verbatim",
]);

// ── Main environment dispatcher ─────────────────────────────────────

/** Parse \begin{envName}...\end{envName} with dispatch on envName. */
export const environment: Parser<LatexNode | null> = string("\\begin")
    .skip(ws)
    .next(braceBalanced())
    .chain((envName: string): Parser<LatexNode | null> => {
        // Math environments
        if (isMathEnv(envName)) {
            return mathEnvBody(envName);
        }

        // Theorem-like
        if (THEOREM_TYPES.has(envName)) {
            return parseTheoremEnv(envName);
        }

        // Lists
        if (envName === "enumerate") return parseListEnv(envName, true);
        if (envName === "itemize") return parseListEnv(envName, false);
        if (envName === "description") return parseDescriptionEnv();

        // Figure
        if (envName === "figure" || envName === "figure*") return parseFigureEnv();

        // Proof
        if (envName === "proof") return parseProofEnv();

        // Quote
        if (envName === "quote" || envName === "quotation") return parseQuoteEnv();

        // Code listings
        if (envName === "lstlisting") return parseCodeBlockEnv();

        // Center (passthrough — parse body as nodes)
        if (envName === "center") {
            return nodesUntilEnd(envName).map((body) => ({
                type: "environment" as const,
                name: envName,
                body,
            }));
        }

        // Document (parse body normally)
        if (envName === "document") {
            return nodesUntilEnd(envName).map((body) => ({
                type: "environment" as const,
                name: envName,
                body,
            }));
        }

        // Skip environments (consume but return null)
        if (SKIP_ENVS.has(envName)) {
            return rawUntilEnd(envName).map(() => null);
        }

        // Unknown environment: parse body as nodes
        return nodesUntilEnd(envName).map((body) => ({
            type: "environment" as const,
            name: envName,
            body,
        }));
    });
