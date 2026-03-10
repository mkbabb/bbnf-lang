/**
 * Auto-inference of formatting hints when no @pretty directive exists.
 *
 * Mirrors the Rust heuristic pipeline in `heuristics.rs`:
 * - detect_toplevel: well-known names → block
 * - detect_block_delimited: brace-wrapped → group + indent
 * - detect_large_compound: >3 concatenation elements → group
 *
 * Each heuristic returns hints or null (first-match-wins).
 */

import type { Expression, AST } from "../types.js";
import type { ResolvedHints } from "./hints.js";

/**
 * Infer formatting hints for a rule with no explicit @pretty directive.
 *
 * Returns an empty ResolvedHints (use defaults) when no heuristic matches.
 */
export function inferHints(
    ruleName: string,
    expr: Expression,
    _ast: AST,
): ResolvedHints {
    // Rule 1: Top-level rules by name
    const TOPLEVEL_NAMES = [
        "grammar",
        "program",
        "stylesheet",
        "module",
        "document",
        "file",
        "root",
    ];
    if (TOPLEVEL_NAMES.includes(ruleName)) {
        return { separator: { type: "hardline" } };
    }

    // Rule 2: Vec-shaped rules that repeat nonterminals → block
    if (
        (expr.type === "many" || expr.type === "many1") &&
        isOrContainsNonterminal(expr.value as Expression)
    ) {
        return { separator: { type: "hardline" } };
    }

    // Rule 3: Brace-delimited → group + indent
    if (containsBraceWrapped(expr)) {
        return { wrapGroup: true, wrapIndent: true };
    }

    // Rule 4: Large concatenation (>3 elements) → group
    if (expr.type === "concatenation") {
        const elems = expr.value as Expression[];
        if (elems.length > 3) {
            return { wrapGroup: true };
        }
    }

    return {};
}

// ---------------------------------------------------------------------------
// Helper predicates
// ---------------------------------------------------------------------------

function isOrContainsNonterminal(expr: Expression): boolean {
    switch (expr.type) {
        case "nonterminal":
            return true;
        case "optionalWhitespace":
        case "group":
            return isOrContainsNonterminal(expr.value as Expression);
        case "concatenation":
            return (expr.value as Expression[]).some(isOrContainsNonterminal);
        default:
            return false;
    }
}

function containsBraceWrapped(expr: Expression): boolean {
    switch (expr.type) {
        case "skip": {
            const [left, right] = expr.value as [Expression, Expression];
            const leftU = unwrap(left);
            const rightU = unwrap(right);
            if (leftU.type === "next") {
                const [nextLeft] = leftU.value as [Expression, Expression];
                const nextLeftU = unwrap(nextLeft);
                if (
                    nextLeftU.type === "literal" &&
                    nextLeftU.value === "{" &&
                    rightU.type === "literal" &&
                    rightU.value === "}"
                ) {
                    return true;
                }
            }
            return false;
        }
        case "concatenation": {
            const children = expr.value as Expression[];
            // Check for concatenation([literal("{"), ..., literal("}")])
            if (children.length >= 3) {
                const firstU = unwrap(children[0]!);
                const lastU = unwrap(children[children.length - 1]!);
                if (
                    firstU.type === "literal" &&
                    firstU.value === "{" &&
                    lastU.type === "literal" &&
                    lastU.value === "}"
                ) {
                    return true;
                }
            }
            return children.some(containsBraceWrapped);
        }
        case "optionalWhitespace":
        case "group":
            return containsBraceWrapped(expr.value as Expression);
        default:
            return false;
    }
}

function unwrap(expr: Expression): Expression {
    if (expr.type === "optionalWhitespace" || expr.type === "group") {
        return unwrap(expr.value as Expression);
    }
    return expr;
}
