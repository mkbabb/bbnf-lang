import type { AST, Expression, ProductionRule } from "../../src/types.js";

export function rule(name: string, expression: Expression): [string, ProductionRule] {
    return [
        name,
        {
            name: { type: "nonterminal", value: name },
            expression,
            comment: { above: [], below: [] },
        } as ProductionRule,
    ];
}

export function nonterminal(value: string): Expression {
    return { type: "nonterminal", value } as Expression;
}

export function literal(value: string): Expression {
    return { type: "literal", value } as Expression;
}

export function alternation(branches: Expression[]): Expression {
    return { type: "alternation", value: branches } as Expression;
}

export function concatenation(elems: Expression[]): Expression {
    return { type: "concatenation", value: elems } as Expression;
}

export function regexExpr(pattern: RegExp): Expression {
    return { type: "regex", value: pattern } as Expression;
}

export function optional(expr: Expression): Expression {
    return { type: "optional", value: expr } as Expression;
}

export function epsilon(): Expression {
    return { type: "epsilon" } as Expression;
}
