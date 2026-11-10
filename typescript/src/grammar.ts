/* eslint-disable @typescript-eslint/no-explicit-any */
import { Parser, string, all, any, regex } from "@mkbabb/parse-that";
import type { Expression, Literal, Epsilon, Nonterminal, Comment, Regex, Group, Optional, Many, Concatenation, Alteration, ProductionRule, ImportDirective, RecoverDirective, PrettyDirective, PrettyHint, NoCollapseDirective, ParsedGrammar, Mapping, WsDirective, RuleDirective } from "./types.js";

const operatorToType: Record<string, string> = {
    "|": "alternation",
    ",": "concatenation",
    "-": "minus",
    "<<": "skip",
    ">>": "next",
    "*": "many",
    "+": "many1",
    "?": "optional",
    "?w": "optionalWhitespace",
};

const reduceBinaryExpression = ([left, rightExpression]: [any, any[]]) => {
    if (rightExpression.length === 0) {
        return left;
    }
    return rightExpression.reduce((acc: Expression, [op, right]: [string, Expression]) => {
        return {
            type: operatorToType[op],
            value: [acc, right],
        } as Expression;
    }, left);
};

const mapFactor = ([term, op]: [any, any]) => {
    if (op === undefined) {
        return term;
    }
    const type = operatorToType[op];
    return {
        type,
        value: term,
    } as Expression;
};

function mapStatePosition(parser: Parser<any>) {
    return parser.mapSpan((value, start, end) => {
        if (value && value.range === undefined) {
            value.range = { start, end };
        }
        return value;
    });
}

type Options = {
    debug: boolean;
    comments: boolean;
};

const defaultOptions: Options = {
    debug: false,
    comments: true,
};

export class BBNFGrammar {
    options: Options;

    // Backing fields for lazy-memoized parsers
    private _blockComment?: Parser<any>;
    private _lineComment?: Parser<any>;
    private _comment?: Parser<any>;
    private _group?: Parser<any>;
    private _regex?: Parser<any>;
    private _optionalGroup?: Parser<any>;
    private _manyGroup?: Parser<any>;
    private _lhs?: Parser<any>;
    private _term?: Parser<any>;
    private _factor?: Parser<any>;
    private _mappedFactor?: Parser<any>;
    private _valueExpr?: Parser<string>;
    private _spanCapture?: Parser<any>;
    private _binaryFactor?: Parser<any>;
    private _concatenation?: Parser<any>;
    private _alternation?: Parser<any>;
    private _rhs?: Parser<any>;
    private _productionRule?: Parser<any>;
    private _grammar?: Parser<any>;
    private _importDirective?: Parser<any>;
    private _recoverDirective?: Parser<any>;
    private _prettyDirective?: Parser<any>;
    private _noCollapseDirective?: Parser<any>;
    private _wsDirective?: Parser<any>;
    private _tokenDirective?: Parser<any>;
    private _inlineDirective?: Parser<any>;
    private _debugDirective?: Parser<any>;
    private _grammarWithImports?: Parser<any>;

    constructor(options?: Partial<Options>) {
        this.options = {
            ...defaultOptions,
            ...(options ?? {}),
        };
    }

    identifier() {
        return regex(/[_a-zA-Z][_a-zA-Z0-9-]*/);
    }

    literal() {
        return any(
            regex(/(\\.|[^"\\])*/).wrap(string('"'), string('"')),
            regex(/(\\.|[^'\\])*/).wrap(string("'"), string("'")),
            regex(/(\\.|[^`\\])*/).wrap(string("`"), string("`")),
        ).map((value: any) => {
            value = value.replace(/\\(.)/g, "$1");
            return {
                type: "literal",
                value,
            } as Literal;
        });
    }

    epsilon() {
        return any(string("epsilon"), string("ε")).map(() => {
            return {
                type: "epsilon",
            } as Epsilon;
        });
    }

    nonterminal() {
        return this.identifier().map((value) => {
            return {
                type: "nonterminal",
                value,
            } as Nonterminal;
        });
    }

    blockComment(): Parser<any> {
        return (this._blockComment ??= Parser.lazy(() =>
            mapStatePosition(
                regex(/\/\*[^\*]*\*\//).map((v) => {
                    return {
                        type: "comment",
                        value: v,
                    } as Comment;
                }),
            ),
        ));
    }

    lineComment(): Parser<any> {
        return (this._lineComment ??= Parser.lazy(() =>
            mapStatePosition(
                regex(/\/\/.*/).map((v) => {
                    return {
                        type: "comment",
                        value: v,
                    } as Comment;
                }),
            ),
        ));
    }

    comment(): Parser<any> {
        return (this._comment ??= Parser.lazy(() =>
            any(this.blockComment(), this.lineComment()),
        ));
    }

    trimBigComment(e: Parser<any>) {
        return e
            .trim(this.blockComment().trim().many() as any, false)
            .map(([left, expression, right]: any) => {
                expression.comment = {
                    left,
                    right,
                };
                return expression as unknown as Expression;
            }) as Parser<Expression>;
    }

    group(): Parser<any> {
        return (this._group ??= Parser.lazy(() =>
            this.rhs()
                .trim()
                .wrap(string("("), string(")"))
                .map((value: any) => {
                    return {
                        type: "group",
                        value,
                    } as Group;
                }),
        ));
    }

    regexRule(): Parser<any> {
        return (this._regex ??= Parser.lazy(() =>
            regex(/(\\.|[^\/])+/)
                .wrap(string("/"), string("/"))
                .then(regex(/[gimuy]*/).opt())
                .map(([r, flags]: any) => {
                    // A leading inline-flag group, `(?s)…`, sets flags for the whole
                    // pattern; ECMAScript spells that as RegExp flags.
                    const inline = /^\(\?([imsu]+)\)/.exec(r);
                    const source = inline ? r.slice(inline[0].length) : r;
                    const all = [...new Set([...(flags ?? ""), ...(inline?.[1] ?? "")])].join("");
                    return {
                        type: "regex",
                        value: new RegExp(source, all || undefined),
                    } as Regex;
                }),
        ));
    }

    /**
     * `@{ rhs }`: a group whose value is the source span it matched
     * (docs/bbnf/grammar-syntax.md, "Span Capture").
     */
    spanCapture(): Parser<any> {
        return (this._spanCapture ??= Parser.lazy(() =>
            this.rhs()
                .trim()
                .wrap(string("@{"), string("}"))
                .map((value: any) => {
                    return {
                        type: "group",
                        value,
                        capture: "span",
                    } as Group;
                }),
        ));
    }

    optionalGroup(): Parser<any> {
        return (this._optionalGroup ??= Parser.lazy(() =>
            this.rhs()
                .trim()
                .wrap(string("["), string("]"))
                .map((value: any) => {
                    return {
                        type: "optional",
                        value: {
                            type: "group",
                            value,
                        },
                    } as Optional;
                }),
        ));
    }

    manyGroup(): Parser<any> {
        return (this._manyGroup ??= Parser.lazy(() =>
            this.rhs()
                .trim()
                .wrap(string("{"), string("}"))
                .map((value: any) => {
                    return {
                        type: "many",
                        value: {
                            type: "group",
                            value,
                        },
                    } as Many;
                }),
        ));
    }

    lhs(): Parser<any> {
        return (this._lhs ??= Parser.lazy(() =>
            mapStatePosition(this.nonterminal()),
        ));
    }

    term(): Parser<any> {
        return (this._term ??= Parser.lazy(() =>
            mapStatePosition(
                any(
                    this.epsilon(),
                    this.spanCapture(),
                    this.group(),
                    this.optionalGroup(),
                    this.manyGroup(),
                    this.nonterminal(),
                    this.literal(),
                    this.regexRule(),
                ) as any,
            ),
        ));
    }

    factor(): Parser<any> {
        return (this._factor ??= Parser.lazy(() =>
            this.trimBigComment(
                mapStatePosition(
                    all(
                        this.term(),
                        any(
                            string("?w"),
                            string("?"),
                            string("*"),
                            string("+"),
                        )
                            .trim()
                            .opt() as any,
                    ).map(mapFactor),
                ),
            ),
        ));
    }

    /**
     * The host expression on the right of `->`: the value-expression language
     * of `grammar/bbnf/expressions.bbnf` (literals, paths, `input` fields,
     * calls, unary and binary operators, closures). The TS front end keeps
     * its source text (`Mapping.expr`); it never evaluates it.
     */
    valueExpr(): Parser<string> {
        return (this._valueExpr ??= Parser.lazy(() => {
            const text = (p: Parser<any>) => p.map((v: any) => (Array.isArray(v) ? v.flat(Infinity).join("") : String(v)));
            const ws = (s: string) => string(s).trim().map(() => s);
            const ident = regex(/[_a-zA-Z][_a-zA-Z0-9]*/);
            const floatLit = regex(/[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?\w*/);
            const intLit = regex(/0[xX][0-9a-fA-F]+\w*|[0-9]+\w*/);
            const stringLit = regex(/"(\\.|[^"\\])*"/);
            const path = text(all(ident, all(string("::"), ident).many()));
            const input = text(all(string("input"), all(string("."), ident).many()));
            const args = text(all(this.valueExpr(), all(ws(","), this.valueExpr()).many()).opt());
            const call = text(all(path, string("("), args, string(")")));
            const paren = text(all(string("("), this.valueExpr(), ws(")")));
            const atom = any(floatLit, intLit, stringLit, call, input, path, paren);
            const unary = any(text(all(any(string("!"), string("-")), atom)), atom);
            const chain = (operand: Parser<string>, ...ops: string[]) =>
                text(all(operand, all(any(...ops.map(ws)), operand).many()));
            const mul = chain(unary, "*", "/", "%");
            const add = chain(mul, "+", "-");
            const cmp = chain(add, "==", "!=", "<=", ">=", "<", ">");
            const and = chain(cmp, "&&");
            const or = chain(and, "||");
            const closure = text(all(string("|"), ident, all(ws(","), ident).many(), string("|"), this.valueExpr()));
            return any(closure, or) as Parser<string>;
        }));
    }

    /** `factor -> valueExpr [: type]` (docs/bbnf/grammar-syntax.md, "Per-Expression Mapping"). */
    mappedFactor(): Parser<any> {
        return (this._mappedFactor ??= Parser.lazy(() =>
            all(
                this.factor(),
                all(
                    string("->").trim(),
                    this.valueExpr(),
                    all(string(":").trim(), regex(/[_a-zA-Z][_a-zA-Z0-9]*/)).opt(),
                ).opt(),
            ).map(([expression, mapping]: any) => {
                if (mapping === undefined) {
                    return expression;
                }
                const [, expr, annotation] = mapping;
                expression.mapping = annotation === undefined ? { expr } : { expr, type: annotation[1] } as Mapping;
                return expression;
            }),
        ));
    }

    binaryFactor(): Parser<any> {
        return (this._binaryFactor ??= Parser.lazy(() =>
            mapStatePosition(
                all(
                    this.mappedFactor(),
                    all(
                        any(string("<<"), string(">>"), string("-")).trim(),
                        this.mappedFactor(),
                    ).many(),
                ).map(reduceBinaryExpression),
            ),
        ));
    }

    concatenation(): Parser<any> {
        return (this._concatenation ??= Parser.lazy(() =>
            mapStatePosition(
                // The comma is optional (docs/bbnf/operators.md, "Concatenation").
                this.binaryFactor().sepBy(regex(/\s*(,\s*)?/)),
            ).map((value: any) => {
                if (value.length === 1) {
                    return value[0];
                }

                return {
                    type: "concatenation",
                    value,
                } as Concatenation;
            }),
        ));
    }

    alternation(): Parser<any> {
        return (this._alternation ??= Parser.lazy(() =>
            mapStatePosition(
                this.concatenation().sepBy(string("|").trim()),
            ).map((value: any) => {
                if (value.length === 1) {
                    return value[0];
                }

                return {
                    type: "alternation",
                    value,
                } as Alteration;
            }),
        ));
    }

    rhs(): Parser<any> {
        return (this._rhs ??= Parser.lazy(() =>
            this.alternation(),
        ));
    }

    productionRule(): Parser<any> {
        return (this._productionRule ??= Parser.lazy(() =>
            all(
                this.lhs(),
                string("=").trim(),
                this.rhs(),
                any(string(";"), string(".")).trim(),
            ).map(([name, , expression]: any) => {
                return { name, expression } as ProductionRule;
            }),
        ));
    }

    grammar(): Parser<any> {
        return (this._grammar ??= Parser.lazy(() =>
            this.productionRule()
                .trim(this.lineComment().trim().many() as any, false)
                .map(([above, rule, below]: any) => {
                    rule.comment = {
                        above,
                        below,
                    };
                    return rule;
                })
                .many(1)
                .trim(),
        ));
    }

    importDirective(): Parser<any> {
        return (this._importDirective ??= Parser.lazy(() => {
            const importPath = regex(/(\\.|[^"\\])*/)
                .wrap(string('"'), string('"'));

            const importItems = this.identifier()
                .sepBy(string(",").trim(), 1)
                .trim()
                .wrap(string("{"), string("}"));

            // Selective: @import { a, b } from "path"
            const selective = all(
                importItems,
                string("from").trim(),
                importPath,
            ).map(([items, , path]: any) => ({
                path,
                items,
            } as ImportDirective));

            // Glob: @import "path"
            const glob = importPath.map((path: any) => ({
                path,
            } as ImportDirective));

            return mapStatePosition(
                all(
                    string("@import").trim(),
                    any(selective, glob),
                    any(string(";"), string(".")).trim().opt(),
                ).map(([, directive]: any) => directive as ImportDirective),
            );
        }));
    }

    recoverDirective(): Parser<any> {
        return (this._recoverDirective ??= Parser.lazy(() => {
            return mapStatePosition(
                all(
                    string("@recover").trim(),
                    this.identifier().trim(),
                    this.rhs().trim(),
                    any(string(";"), string(".")).trim().opt(),
                ).map(([, ruleName, syncExpr]: any) => ({
                    ruleName,
                    syncExpr,
                } as RecoverDirective)),
            );
        }));
    }

    prettyDirective(): Parser<any> {
        return (this._prettyDirective ??= Parser.lazy(() => {
            // A hint is either `identifier("string")` or a bare `identifier`.
            const quotedArg = regex(/(\\.|[^"\\])*/)
                .wrap(string('"'), string('"'));

            const hintWithArg = all(
                this.identifier(),
                quotedArg.wrap(string("("), string(")")),
            ).map(([name, arg]: any) => ({ name, arg } as PrettyHint));

            const bareHint = this.identifier().map(
                (name: any) => ({ name } as PrettyHint),
            );

            const hint = any(hintWithArg, bareHint);

            return mapStatePosition(
                all(
                    string("@pretty").trim(),
                    this.identifier().trim(),
                    hint.trim().many(1),
                    any(string(";"), string(".")).trim().opt(),
                ).map(([, ruleName, hints]: any) => ({
                    ruleName,
                    hints,
                } as PrettyDirective)),
            );
        }));
    }

    noCollapseDirective(): Parser<any> {
        return (this._noCollapseDirective ??= Parser.lazy(() => {
            return mapStatePosition(
                all(
                    string("@no_collapse").trim(),
                    this.identifier().trim(),
                    any(string(";"), string(".")).trim().opt(),
                ).map(([, ruleName]: any) => ({
                    ruleName,
                } as NoCollapseDirective)),
            );
        }));
    }

    /** `@ws /regex/ ;` (docs/bbnf/grammar-syntax.md, "Directives Summary"). */
    wsDirective(): Parser<any> {
        return (this._wsDirective ??= Parser.lazy(() =>
            mapStatePosition(
                all(
                    string("@ws").trim(),
                    this.regexRule().trim(),
                    any(string(";"), string(".")).trim().opt(),
                ).map(([, re]: any) => ({ pattern: re.value } as WsDirective)),
            ),
        ));
    }

    /** `@<name> rule ;` for a directive that names one rule (`*` too, when `star`). */
    private ruleDirective(name: string, star: boolean): Parser<any> {
        const target = star ? any(string("*"), this.identifier()) : this.identifier();
        return mapStatePosition(
            all(
                string(name).trim(),
                target.trim(),
                any(string(";"), string(".")).trim().opt(),
            ).map(([, ruleName]: any) => ({ ruleName } as RuleDirective)),
        );
    }

    tokenDirective(): Parser<any> {
        return (this._tokenDirective ??= Parser.lazy(() => this.ruleDirective("@token", false)));
    }

    inlineDirective(): Parser<any> {
        return (this._inlineDirective ??= Parser.lazy(() => this.ruleDirective("@inline", false)));
    }

    debugDirective(): Parser<any> {
        return (this._debugDirective ??= Parser.lazy(() => this.ruleDirective("@debug", true)));
    }

    grammarWithImports(): Parser<any> {
        return (this._grammarWithImports ??= Parser.lazy(() => {
            const commentTrim = this.lineComment().trim().many() as any;

            const rule = this.productionRule()
                .trim(commentTrim, false)
                .map(([above, rule, below]: any) => {
                    rule.comment = {
                        above,
                        below,
                    };
                    return rule;
                });

            const importDir = this.importDirective()
                .trim(commentTrim, false)
                .map(([above, directive, below]: any) => directive);

            const recoverDir = this.recoverDirective()
                .trim(commentTrim, false)
                .map(([above, directive, below]: any) => directive);

            const prettyDir = this.prettyDirective()
                .trim(commentTrim, false)
                .map(([above, directive, below]: any) => directive);

            const noCollapseDir = this.noCollapseDirective()
                .trim(commentTrim, false)
                .map(([above, directive, below]: any) => directive);

            const directive = (p: Parser<any>) =>
                p.trim(commentTrim, false).map(([above, d, below]: any) => d);

            const item = any(
                importDir.map((d: ImportDirective) => ({ type: "import" as const, value: d })),
                recoverDir.map((d: RecoverDirective) => ({ type: "recover" as const, value: d })),
                noCollapseDir.map((d: NoCollapseDirective) => ({ type: "no_collapse" as const, value: d })),
                prettyDir.map((d: PrettyDirective) => ({ type: "pretty" as const, value: d })),
                directive(this.wsDirective()).map((d: WsDirective) => ({ type: "ws" as const, value: d })),
                directive(this.tokenDirective()).map((d: RuleDirective) => ({ type: "token" as const, value: d })),
                directive(this.inlineDirective()).map((d: RuleDirective) => ({ type: "inline" as const, value: d })),
                directive(this.debugDirective()).map((d: RuleDirective) => ({ type: "debug" as const, value: d })),
                rule.map((r: ProductionRule) => ({ type: "rule" as const, value: r })),
            );

            return item.many(1).trim().map((items: any[]) => {
                const imports: ImportDirective[] = [];
                const recovers: RecoverDirective[] = [];
                const no_collapses: NoCollapseDirective[] = [];
                const pretties: PrettyDirective[] = [];
                const ws: WsDirective[] = [];
                const tokens: RuleDirective[] = [];
                const inlines: RuleDirective[] = [];
                const debugs: RuleDirective[] = [];
                const rules: ProductionRule[] = [];
                for (const item of items) {
                    if (item.type === "import") {
                        imports.push(item.value);
                    } else if (item.type === "recover") {
                        recovers.push(item.value);
                    } else if (item.type === "no_collapse") {
                        no_collapses.push(item.value);
                    } else if (item.type === "pretty") {
                        pretties.push(item.value);
                    } else if (item.type === "ws") {
                        ws.push(item.value);
                    } else if (item.type === "token") {
                        tokens.push(item.value);
                    } else if (item.type === "inline") {
                        inlines.push(item.value);
                    } else if (item.type === "debug") {
                        debugs.push(item.value);
                    } else {
                        rules.push(item.value);
                    }
                }
                const rulesMap: Map<string, ProductionRule> = new Map(
                    rules.map((r) => [r.name.value, r]),
                );
                return { imports, recovers, no_collapses, pretties, ws, tokens, inlines, debugs, rules: rulesMap } as ParsedGrammar;
            });
        }));
    }
}
