/**
 * Pattern detection utilities for recognizing common BBNF structures.
 *
 * These mirror the Rust `detect_wrapped_pattern` / `detect_key_value_pattern`
 * functions in `prettify_utils.rs`, operating on the TS Expression tree.
 */

import type { Expression, AST } from "../types.js";

export interface WrappedPattern {
    left: string;
    middle: Expression;
    right: string;
}

export interface KeyValuePattern {
    keyExpr: Expression;
    sepLiteral: string;
    valueExpr: Expression;
}

/**
 * Recognized `item , (sep , item)*` list pattern.
 *
 * Many BBNF grammars express comma-separated lists as:
 *   members = member , ("," , member)*;
 *
 * The interpreter needs to flatten these into a single list of items,
 * dropping the structural separator literals.
 */
export interface SepByPattern {
    firstExpr: Expression;
    sepLiteral: string;
    restExpr: Expression;
}

/**
 * Unwrap transparent expression wrappers (optionalWhitespace, group).
 */
function unwrapTransparent(expr: Expression): Expression {
    if (expr.type === "optionalWhitespace" || expr.type === "group") {
        return unwrapTransparent(expr.value as Expression);
    }
    return expr;
}

/**
 * Detect `"L" >> middle << "R"` (wrapped pattern).
 *
 * In the BBNF AST this is: `skip([next([Literal(L), middle]), Literal(R)])`
 *
 * Returns the literal delimiters and the middle expression, or null if
 * the expression does not match.
 */
export function detectWrappedPattern(expr: Expression): WrappedPattern | null {
    const unwrapped = unwrapTransparent(expr);

    // Shape: skip([next([Literal(L), middle]), Literal(R)])
    if (unwrapped.type === "skip") {
        const [left, right] = unwrapped.value as [Expression, Expression];
        const leftUnwrapped = unwrapTransparent(left);
        const rightUnwrapped = unwrapTransparent(right);

        if (leftUnwrapped.type === "next") {
            const [nextLeft, nextMiddle] = leftUnwrapped.value as [
                Expression,
                Expression,
            ];
            const nextLeftUnwrapped = unwrapTransparent(nextLeft);

            if (
                nextLeftUnwrapped.type === "literal" &&
                rightUnwrapped.type === "literal"
            ) {
                return {
                    left: nextLeftUnwrapped.value as string,
                    middle: nextMiddle,
                    right: rightUnwrapped.value as string,
                };
            }
        }
    }

    // Shape: concatenation([Literal(L), middle..., Literal(R)])
    // This handles `,` concatenation grammars like: object = "{" , members? , "}";
    if (unwrapped.type === "concatenation") {
        const children = unwrapped.value as Expression[];
        if (children.length >= 3) {
            const first = unwrapTransparent(children[0]!);
            const last = unwrapTransparent(children[children.length - 1]!);
            if (first.type === "literal" && last.type === "literal") {
                const leftStr = first.value as string;
                const rightStr = last.value as string;
                // Check for matching bracket pairs
                if (
                    (leftStr === "{" && rightStr === "}") ||
                    (leftStr === "[" && rightStr === "]") ||
                    (leftStr === "(" && rightStr === ")")
                ) {
                    // Middle is everything between first and last
                    const middleChildren = children.slice(1, -1);
                    const middle =
                        middleChildren.length === 1
                            ? middleChildren[0]!
                            : ({
                                  type: "concatenation" as const,
                                  value: middleChildren,
                              } as Expression);
                    return { left: leftStr, middle, right: rightStr };
                }
            }
        }
    }

    return null;
}

/**
 * Detect `first , (sep , rest)*` list pattern in a concatenation.
 *
 * In the BBNF AST this is:
 *   concatenation([firstExpr, many(concatenation([literal(sep), restExpr]))])
 *
 * This pattern is extremely common — e.g.:
 *   members = member , ("," , member)*;
 *   elements = value , ("," , value)*;
 */
export function detectSepByPattern(expr: Expression): SepByPattern | null {
    if (expr.type !== "concatenation") return null;
    const children = expr.value as Expression[];
    if (children.length !== 2) return null;

    const first = children[0]!;
    const second = unwrapTransparent(children[1]!);

    // Second child must be many/many1
    if (second.type !== "many" && second.type !== "many1") return null;

    const inner = unwrapTransparent(second.value as Expression);

    // Inner must be a concatenation starting with a literal separator
    if (inner.type !== "concatenation") return null;
    const innerChildren = inner.value as Expression[];
    if (innerChildren.length !== 2) return null;

    const sepExpr = unwrapTransparent(innerChildren[0]!);
    if (sepExpr.type !== "literal") return null;

    return {
        firstExpr: first,
        sepLiteral: sepExpr.value as string,
        restExpr: innerChildren[1]!,
    };
}

/**
 * Resolve a nonterminal reference in the AST and detect a wrapped pattern
 * in the referenced rule's expression.
 */
export function resolveAndDetectWrapped(
    expr: Expression,
    ast: AST,
): WrappedPattern | null {
    if (expr.type === "nonterminal") {
        const refName = expr.value as string;
        const rule = ast.get(refName);
        if (rule) {
            return detectWrappedPattern(rule.expression);
        }
    }
    return null;
}

/**
 * Detect `key, sep >> value` (key-value pair) pattern.
 *
 * In the BBNF AST: `concatenation([keyExpr, next([sep, value])])`
 * where sep is a Literal or Nonterminal.
 */
export function detectKeyValuePattern(
    expr: Expression,
    ast: AST,
): KeyValuePattern | null {
    if (expr.type !== "concatenation") return null;
    const elems = expr.value as Expression[];

    // Shape: concatenation([key, next([sep, value])]) — skip/next style
    if (elems.length === 2) {
        const second = elems[1]!;
        if (second.type === "next") {
            const [sep, valueExpr] = second.value as [Expression, Expression];
            const sepUnwrapped = unwrapTransparent(sep);

            if (sepUnwrapped.type === "literal") {
                return {
                    keyExpr: elems[0]!,
                    sepLiteral: sepUnwrapped.value as string,
                    valueExpr,
                };
            }

            // sep can be a nonterminal referencing a literal rule
            if (sepUnwrapped.type === "nonterminal") {
                const sepName = sepUnwrapped.value as string;
                const sepRule = ast.get(sepName);
                if (sepRule) {
                    const resolved = unwrapTransparent(sepRule.expression);
                    if (resolved.type === "literal") {
                        return {
                            keyExpr: elems[0]!,
                            sepLiteral: resolved.value as string,
                            valueExpr,
                        };
                    }
                }
            }
        }
    }

    // Shape: concatenation([key, Literal(sep), value])
    // This handles `,` concatenation grammars like: member = string , ":" , value;
    if (elems.length === 3) {
        const mid = unwrapTransparent(elems[1]!);
        if (mid.type === "literal") {
            const sepStr = mid.value as string;
            if (sepStr === ":" || sepStr === "=" || sepStr === "=>") {
                return {
                    keyExpr: elems[0]!,
                    sepLiteral: sepStr,
                    valueExpr: elems[2]!,
                };
            }
        }
    }

    return null;
}
