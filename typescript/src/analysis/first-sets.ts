import type { AST, Expression } from "../types.js";
import type { AnalysisCache } from "./metadata.js";
import { CharSet } from "./charset.js";
import { regexFirstChars } from "./regex-first.js";

// --- FIRST set and NULLABLE computation ---

export interface FirstNullable {
    firstSets: Map<string, CharSet>;
    nullable: Map<string, boolean>;
}

/**
 * Compute FIRST sets and nullability for all rules in the grammar.
 * Uses fixed-point iteration for recursive grammars.
 */
export function computeFirstSets(
    ast: AST,
    analysis: AnalysisCache,
): FirstNullable {
    const firstSets = new Map<string, CharSet>();
    const nullable = new Map<string, boolean>();

    // Initialize
    for (const [name] of ast) {
        firstSets.set(name, new CharSet());
        nullable.set(name, false);
    }

    // Fixed-point iteration
    let changed = true;
    let iterations = 0;
    const maxIterations = ast.size * 3;

    while (changed && iterations++ < maxIterations) {
        changed = false;

        for (const [name, rule] of ast) {
            const oldFirst = firstSets.get(name)!.clone();
            const oldNullable = nullable.get(name)!;

            const exprFirst = exprFirstSet(rule.expression, firstSets, nullable, ast);
            const exprNullable = exprIsNullable(rule.expression, nullable, ast);

            firstSets.get(name)!.union(exprFirst);
            if (exprNullable && !oldNullable) {
                nullable.set(name, true);
                changed = true;
            }

            // Check if first set changed
            const newFirst = firstSets.get(name)!;
            for (let w = 0; w < 4; w++) {
                if (
                    // eslint-disable-next-line @typescript-eslint/no-explicit-any
                    (oldFirst as any).bits[w] !== (newFirst as any).bits[w]
                ) {
                    changed = true;
                    break;
                }
            }
        }
    }

    return { firstSets, nullable };
}

export function exprFirstSet(
    expr: Expression,
    firstSets: Map<string, CharSet>,
    nullable: Map<string, boolean>,
    ast: AST,
): CharSet {
    if (!expr?.type) return new CharSet();

    switch (expr.type) {
        case "literal": {
            const s = expr.value as string;
            const cs = new CharSet();
            if (s.length > 0) cs.add(s.charCodeAt(0));
            return cs;
        }
        case "regex": {
            const cs = regexFirstChars(expr.value as RegExp);
            return cs ?? new CharSet();
        }
        case "nonterminal": {
            const name = expr.value as string;
            return firstSets.get(name)?.clone() ?? new CharSet();
        }
        case "epsilon":
            return new CharSet();
        case "group":
            return exprFirstSet(
                expr.value as Expression,
                firstSets,
                nullable,
                ast,
            );
        case "optionalWhitespace":
            return exprFirstSet(
                expr.value as unknown as Expression,
                firstSets,
                nullable,
                ast,
            );
        case "optional":
        case "many":
            return exprFirstSet(
                expr.value as Expression,
                firstSets,
                nullable,
                ast,
            );
        case "many1":
            return exprFirstSet(
                expr.value as Expression,
                firstSets,
                nullable,
                ast,
            );
        case "skip":
        case "next": {
            // sequence of two: FIRST(A) U (if nullable(A) then FIRST(B))
            const [a, b] = expr.value as [Expression, Expression];
            const cs = exprFirstSet(a, firstSets, nullable, ast);
            if (exprIsNullable(a, nullable, ast)) {
                cs.union(exprFirstSet(b, firstSets, nullable, ast));
            }
            return cs;
        }
        case "minus": {
            const [a] = expr.value as [Expression, Expression];
            return exprFirstSet(a, firstSets, nullable, ast);
        }
        case "concatenation": {
            const elems = expr.value as Expression[];
            const cs = new CharSet();
            for (const elem of elems) {
                cs.union(exprFirstSet(elem, firstSets, nullable, ast));
                if (!exprIsNullable(elem, nullable, ast)) break;
            }
            return cs;
        }
        case "alternation": {
            const alts = expr.value as Expression[];
            const cs = new CharSet();
            for (const alt of alts) {
                cs.union(exprFirstSet(alt, firstSets, nullable, ast));
            }
            return cs;
        }
    }
    return new CharSet();
}

export function exprIsNullable(
    expr: Expression,
    nullable: Map<string, boolean>,
    ast: AST,
): boolean {
    if (!expr?.type) return false;

    switch (expr.type) {
        case "literal":
            return (expr.value as string).length === 0;
        case "regex":
            return false;
        case "nonterminal":
            return nullable.get(expr.value as string) ?? false;
        case "epsilon":
            return true;
        case "group":
        case "optionalWhitespace":
            return exprIsNullable(expr.value as unknown as Expression, nullable, ast);
        case "optional":
        case "many":
            return true;
        case "many1":
            return exprIsNullable(expr.value as Expression, nullable, ast);
        case "skip":
        case "next": {
            const [a, b] = expr.value as [Expression, Expression];
            return (
                exprIsNullable(a, nullable, ast) &&
                exprIsNullable(b, nullable, ast)
            );
        }
        case "minus":
            return false;
        case "concatenation": {
            return (expr.value as Expression[]).every((e) =>
                exprIsNullable(e, nullable, ast),
            );
        }
        case "alternation": {
            return (expr.value as Expression[]).some((e) =>
                exprIsNullable(e, nullable, ast),
            );
        }
    }
    return false;
}
