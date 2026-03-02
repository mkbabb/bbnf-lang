import type { Parser } from "@mkbabb/parse-that";

export type Expression =
    | Literal
    | Nonterminal
    | Group
    | Regex
    | Optional
    | Minus
    | Many
    | Many1
    | Skip
    | Next
    | Concatenation
    | Alteration
    | Epsilon
    | OptionalWhitespace;

interface Token<T, V = string> {
    type: T;
    value: V;

    range?: {
        start: number;
        end: number;
    };
}

export type Comment = Token<"comment">;

interface ExpressionToken<T, V = string> extends Token<T, V> {
    comment?: {
        left: Comment[];
        right: Comment[];
    };
}

export type Nonterminal = ExpressionToken<"nonterminal">;

export type Literal = ExpressionToken<"literal">;
export type Regex = ExpressionToken<"regex", RegExp>;
export type Epsilon = ExpressionToken<"epsilon">;

export type Group = ExpressionToken<"group", Expression>;
export type ManyGroup = ExpressionToken<"many", Expression>;
export type OptionalGroup = ExpressionToken<"optional", Expression>;

export type Optional = ExpressionToken<"optional", Expression>;
export type OptionalWhitespace = ExpressionToken<
    "optionalWhitespace",
    undefined
>;

export type Minus = ExpressionToken<"minus", [Expression, Expression]>;

export type Many = ExpressionToken<"many", Expression>;
export type Many1 = ExpressionToken<"many1", Expression>;
export type Skip = ExpressionToken<"skip", [Expression, Expression]>;
export type Next = ExpressionToken<"next", [Expression, Expression]>;

export type Concatenation = ExpressionToken<"concatenation", Expression[]>;
export type Alteration = ExpressionToken<"alternation", Expression[]>;

export type ProductionRule = {
    name: Nonterminal;
    expression: Expression;
    comment: {
        above: Comment[];
        below: Comment[];
    };
};

export type AST = Map<string, ProductionRule>;
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type Nonterminals = { [key: string]: Parser<any> };

export type ImportDirective = {
    /** Relative path to the imported file. */
    path: string;
    /** If set, only these rule names are imported (selective import). */
    items?: string[];
    range?: { start: number; end: number };
};

export type RecoverDirective = {
    /** The name of the rule to wrap with recovery. */
    ruleName: string;
    /** The sync expression to use for error recovery. */
    syncExpr: Expression;
    range?: { start: number; end: number };
};

export type PrettyDirective = {
    /** The name of the rule to apply formatting hints to. */
    ruleName: string;
    /** Formatting hints (e.g. "group", "indent", "block", "blankline", "nobreak", "fast"). */
    hints: string[];
    range?: { start: number; end: number };
};

export type ParsedGrammar = {
    imports: ImportDirective[];
    recovers: RecoverDirective[];
    pretties: PrettyDirective[];
    rules: AST;
};
