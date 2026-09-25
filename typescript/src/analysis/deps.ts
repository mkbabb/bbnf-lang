import type { AST, Expression } from "../types.js";

/**
 * Recursively collect all nonterminal references from an expression.
 */
export function collectDependencies(expr: Expression, deps: Set<string>): void {
    if (!expr?.type) return;
    if (expr.type === "nonterminal") {
        deps.add(expr.value as string);
        return;
    }
    if (expr.value instanceof Array) {
        for (const child of expr.value) {
            collectDependencies(child as Expression, deps);
        }
    } else if (
        expr.value &&
        typeof expr.value === "object" &&
        "type" in (expr.value as object)
    ) {
        collectDependencies(expr.value as Expression, deps);
    }
}

/**
 * Build forward and reverse dependency graphs for the entire AST.
 */
export function buildDepGraphs(ast: AST) {
    const depGraph = new Map<string, Set<string>>();
    const rdepGraph = new Map<string, Set<string>>();

    for (const [name] of ast) {
        depGraph.set(name, new Set());
        rdepGraph.set(name, new Set());
    }

    for (const [name, rule] of ast) {
        const deps = new Set<string>();
        collectDependencies(rule.expression, deps);
        depGraph.set(name, deps);
        for (const dep of deps) {
            if (!rdepGraph.has(dep)) rdepGraph.set(dep, new Set());
            rdepGraph.get(dep)!.add(name);
        }
    }

    return { depGraph, rdepGraph };
}

export function traverseAST(
    ast: AST,
    callback: (
        node: Expression,
        parentNode?: Expression,
    ) => Expression | undefined,
) {
    const recurse = (node: Expression, parentNode?: Expression) => {
        if (!node?.type) return;

        node = callback(node, parentNode) ?? node;
        parentNode = node;

        if (node?.value instanceof Array) {
            for (let i = node.value.length - 1; i >= 0; i--) {
                recurse(node.value[i] as Expression, parentNode);
            }
        } else if (node?.value && typeof node.value === "object") {
            recurse(node.value as Expression, parentNode);
        }
    };

    for (const [, productionRule] of ast.entries()) {
        recurse(productionRule.expression);
    }
}

export function dedupGroups(ast: AST) {
    traverseAST(ast, (node, parentNode) => {
        const parentType = parentNode?.type;

        if (
            parentType === "group" &&
            parentNode &&
            (node.type === "group" || node.type === "nonterminal")
        ) {
            // eslint-disable-next-line @typescript-eslint/no-explicit-any
            (parentNode as any).value = node.value;
            parentNode.range = node.range;
            // eslint-disable-next-line @typescript-eslint/no-explicit-any
            (parentNode as any).type = node.type;

            // eslint-disable-next-line @typescript-eslint/no-explicit-any
            (parentNode as any).comment = {
                left: [
                    // eslint-disable-next-line @typescript-eslint/no-explicit-any
                    ...((parentNode as any).comment?.left ?? []),
                    // eslint-disable-next-line @typescript-eslint/no-explicit-any
                    ...((node as any).comment?.left ?? []),
                ],
                right: [
                    // eslint-disable-next-line @typescript-eslint/no-explicit-any
                    ...((parentNode as any).comment?.right ?? []),
                    // eslint-disable-next-line @typescript-eslint/no-explicit-any
                    ...((node as any).comment?.right ?? []),
                ],
            };
            return node.value as Expression;
        }
        return undefined;
    });
}
