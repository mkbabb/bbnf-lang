/**
 * Core prettify interpreter — walks an Expression tree + parsed value
 * simultaneously and produces pprint Doc trees.
 *
 * This is the runtime equivalent of the Rust compile-time prettify codegen
 * in `bbnf/src/generate/prettify/mod.rs`.
 *
 * The interpreter dispatches on Expression.type and destructures the parsed
 * value according to the shape that ASTToParser produces:
 *
 * | Expression type   | Parser output              | Value shape               |
 * |-------------------|----------------------------|---------------------------|
 * | literal           | string(s) → string         | string                    |
 * | regex             | regex(r) → string          | string                    |
 * | nonterminal(name) | Parser.lazy(...)           | varies (recurse into rule)|
 * | group(inner)      | same as inner              | same as inner             |
 * | optional(inner)   | .opt() → T | undefined     | value or undefined        |
 * | many/many1(inner) | .many() → T[]              | array                     |
 * | skip([left,right])| .skip(right) — keeps left  | value of left expression  |
 * | next([left,right])| .next(right) — keeps right | value of right expression |
 * | minus([main,excl])| .not(excluded) — keeps main| value of main expression  |
 * | concatenation     | all(...) → tuple/array     | [...values]               |
 * | alternation       | any(...) → tagged           | { _branch: n, value }    |
 */

import type { Doc } from "@mkbabb/pprint";
import {
    text,
    concat,
    group,
    indent,
    join,
    ifBreak,
    hardline,
    softline,
    NULL_DOC,
} from "@mkbabb/pprint";
import type { Expression, AST, PrettyHint } from "../types.js";
import { resolveHints, applySeparator, wrapDoc } from "./hints.js";
import type { ResolvedHints } from "./hints.js";
import { detectWrappedPattern, detectKeyValuePattern, detectSepByPattern } from "./patterns.js";
import { inferHints } from "./heuristics.js";
import { splitBalanced, containsDelimiter } from "@mkbabb/parse-that";

/**
 * Convert a parsed value to a pprint Doc, guided by the Expression tree
 * that describes the grammar structure.
 *
 * @param expr        The grammar expression describing this node's shape.
 * @param value       The parsed value produced by the parser for this expression.
 * @param ast         The full grammar AST (for resolving nonterminal references).
 * @param prettyMap   Map from rule name → PrettyHint[] (from @pretty directives).
 * @param currentRule The name of the rule currently being formatted (for hint lookup).
 */
export function expressionToDoc(
    expr: Expression,
    value: unknown,
    ast: AST,
    prettyMap: Map<string, PrettyHint[]>,
    currentRule?: string,
): Doc {
    if (value === undefined || value === null) return NULL_DOC;

    switch (expr.type) {
        case "literal":
            return text(String(value));

        case "regex":
            return text(String(value));

        case "epsilon":
            return NULL_DOC;

        case "optionalWhitespace":
            // Whitespace-trimmed wrapper — transparent, recurse into the inner
            // expression. The value shape is the same as the inner expression's.
            // Despite the type definition saying `value: undefined`, the BBNF
            // parser actually stores the inner expression in expr.value.
            if (expr.value != null && typeof expr.value === "object" && "type" in (expr.value as any)) {
                return expressionToDoc(expr.value as any, value, ast, prettyMap, currentRule);
            }
            return text(String(value));

        case "nonterminal": {
            const ruleName = expr.value as string;
            const rule = ast.get(ruleName);
            if (!rule) return text(String(value));
            return ruleToDoc(ruleName, rule.expression, value, ast, prettyMap);
        }

        case "group":
            return expressionToDoc(
                expr.value as Expression,
                value,
                ast,
                prettyMap,
                currentRule,
            );

        case "optional": {
            if (value === undefined) return NULL_DOC;
            return expressionToDoc(
                expr.value as Expression,
                value,
                ast,
                prettyMap,
                currentRule,
            );
        }

        case "many":
        case "many1": {
            if (!Array.isArray(value)) return text(String(value));
            if (value.length === 0) return NULL_DOC;

            const innerExpr = expr.value as Expression;
            const hints = currentRule
                ? (prettyMap.get(currentRule) ?? [])
                : [];
            const resolved = resolveHints(hints);

            const docs = value.map((item: unknown) =>
                expressionToDoc(innerExpr, item, ast, prettyMap, currentRule),
            );
            const sep = applySeparator(resolved);

            // When both indent and hard separator, use the Rust pattern:
            // Indent(Hardline + Join(sep, items)) + Hardline
            if (resolved.wrapIndent && isHardSep(resolved)) {
                const body = concat(
                    hardline,
                    join(sep, docs),
                );
                const result = concat(indent(body), hardline);
                // Filter indent from outer wrap to avoid double-wrapping
                return resolved.wrapGroup ? group(result) : result;
            }

            return wrapDoc(join(sep, docs), resolved);
        }

        case "skip": {
            // L << R — value is L's result (R was discarded during parsing).
            // The Expression's value is [leftExpr, rightExpr].
            const [leftExpr] = expr.value as [Expression, Expression];
            return expressionToDoc(
                leftExpr,
                value,
                ast,
                prettyMap,
                currentRule,
            );
        }

        case "next": {
            // L >> R — value is R's result (L was discarded during parsing).
            const [, rightExpr] = expr.value as [Expression, Expression];
            return expressionToDoc(
                rightExpr,
                value,
                ast,
                prettyMap,
                currentRule,
            );
        }

        case "minus": {
            // main - excluded — value is main's result.
            const [mainExpr] = expr.value as [Expression, Expression];
            return expressionToDoc(
                mainExpr,
                value,
                ast,
                prettyMap,
                currentRule,
            );
        }

        case "concatenation": {
            const children = expr.value as Expression[];
            const values = Array.isArray(value) ? value : [value];

            // Resolve hints for the current rule — shared by all pattern formatters
            const hints = currentRule
                ? (prettyMap.get(currentRule) ?? [])
                : [];
            const resolved = resolveHints(hints);

            // Try wrapped pattern detection: "L" >> middle << "R"
            const wrapped = detectWrappedPattern(expr);
            if (wrapped) {
                return formatWrapped(
                    wrapped.left,
                    wrapped.middle,
                    wrapped.right,
                    value,
                    ast,
                    prettyMap,
                    currentRule,
                    resolved,
                );
            }

            // Try sep-by list pattern: first , (sep , rest)*
            const sepBy = detectSepByPattern(expr);
            if (sepBy) {
                return formatSepByList(
                    sepBy,
                    values,
                    ast,
                    prettyMap,
                    currentRule,
                );
            }

            // Try key-value pattern detection
            const kv = detectKeyValuePattern(expr, ast);
            if (kv) {
                return formatKeyValue(
                    kv.keyExpr,
                    kv.sepLiteral,
                    kv.valueExpr,
                    values,
                    ast,
                    prettyMap,
                    currentRule,
                    resolved,
                );
            }

            // Default: concatenate all child docs with the rule's separator.
            // ASTToParser's all() filters undefined values from the output tuple,
            // so we need to map Expression children to non-discarded positions.
            //
            // Literal children whose parsed value matches the literal text are
            // treated as "structural" (emitted as-is with text()). Separators
            // are only inserted between "semantic" (non-structural) children.
            const keptChildren = getKeptChildren(children);

            // Classify each child as structural or semantic
            type ChildEntry = { doc: Doc; structural: boolean };
            const entries: ChildEntry[] = [];
            for (let i = 0; i < keptChildren.length; i++) {
                const child = keptChildren[i]!;
                const v = values[i];
                if (v === undefined) continue;

                const unwrapped = unwrapTransparentExpr(child);
                if (
                    unwrapped.type === "literal" &&
                    String(v) === String(unwrapped.value)
                ) {
                    // Structural literal — emit as text, no separator
                    entries.push({ doc: text(String(v)), structural: true });
                } else {
                    entries.push({
                        doc: expressionToDoc(
                            child,
                            v,
                            ast,
                            prettyMap,
                            currentRule,
                        ),
                        structural: false,
                    });
                }
            }

            if (entries.length === 0) return NULL_DOC;
            if (entries.length === 1) return wrapDoc(entries[0]!.doc, resolved);

            // Build the final doc: structural items concatenated inline,
            // semantic items joined with separator
            const sep = applySeparator(resolved);
            const parts: Doc[] = [];
            let pendingSemantic: Doc[] = [];

            const flushSemantic = () => {
                if (pendingSemantic.length > 0) {
                    parts.push(join(sep, pendingSemantic));
                    pendingSemantic = [];
                }
            };

            for (const entry of entries) {
                if (entry.structural) {
                    flushSemantic();
                    parts.push(entry.doc);
                } else {
                    pendingSemantic.push(entry.doc);
                }
            }
            flushSemantic();

            return wrapDoc(concat(...parts), resolved);
        }

        case "alternation": {
            const branches = expr.value as Expression[];
            // With tagAlternations=true, value is { _branch: number, value: any }
            if (
                typeof value === "object" &&
                value !== null &&
                "_branch" in value
            ) {
                const tagged = value as { _branch: number; value: unknown };
                const branch = branches[tagged._branch];
                if (branch) {
                    return expressionToDoc(
                        branch,
                        tagged.value,
                        ast,
                        prettyMap,
                        currentRule,
                    );
                }
            }
            // Fallback for untagged alternations: try each branch
            // (this is less precise but handles basic cases)
            return text(String(value));
        }

        default:
            return text(String(value));
    }
}

// ---------------------------------------------------------------------------
// Rule-level entry point (applies per-rule hints + heuristics)
// ---------------------------------------------------------------------------

/**
 * Format a value for a specific named rule, applying @pretty hints
 * and heuristic inference.
 */
function ruleToDoc(
    ruleName: string,
    ruleExpr: Expression,
    value: unknown,
    ast: AST,
    prettyMap: Map<string, PrettyHint[]>,
): Doc {
    // Check for split directive on span-like (string) values
    const hints = prettyMap.get(ruleName) ?? [];
    const resolved = resolveHints(hints);

    if (resolved.splitDelim && typeof value === "string") {
        return formatSplitSpan(value, resolved);
    }

    return expressionToDoc(ruleExpr, value, ast, prettyMap, ruleName);
}

// ---------------------------------------------------------------------------
// Pattern formatters
// ---------------------------------------------------------------------------

/**
 * Format a wrapped pattern: "L" >> middle << "R"
 *
 * Mirrors Rust prettify/mod.rs `wrapped_doc` generation (lines 734-750):
 *   Group(L + Indent(IfBreak(Hardline, Null) + body) + IfBreak(Hardline, Null) + R)
 */
function formatWrapped(
    leftDelim: string,
    middleExpr: Expression,
    rightDelim: string,
    value: unknown,
    ast: AST,
    prettyMap: Map<string, PrettyHint[]>,
    currentRule?: string,
    resolved?: ResolvedHints,
): Doc {
    // For concatenation-based wrapping (e.g. "{" , members , "}"),
    // value is the full array [leftVal, ...middleVals, rightVal].
    // Extract the middle portion.
    let middleValue: unknown = value;
    if (Array.isArray(value) && value.length >= 3) {
        const inner = value.slice(1, -1);
        middleValue = inner.length === 1 ? inner[0] : inner;
    }

    const innerDoc = expressionToDoc(
        middleExpr,
        middleValue,
        ast,
        prettyMap,
        currentRule,
    );

    // Check if inner value is empty (array with 0 elements, or undefined from optional)
    if (Array.isArray(middleValue) && middleValue.length === 0) {
        return concat(text(leftDelim), text(rightDelim));
    }
    if (middleValue === undefined || middleValue === null) {
        return concat(text(leftDelim), text(rightDelim));
    }

    // Mirrors Rust wrapped_doc: Group(L + Indent(IfBreak(Hardline, Null) + body) + IfBreak(Hardline, Null) + R)
    const lineOrNothing = ifBreak(hardline, NULL_DOC);
    const doc = group(
        concat(
            text(leftDelim),
            indent(concat(lineOrNothing, innerDoc)),
            lineOrNothing,
            text(rightDelim),
        ),
    );

    // Apply remaining hints (e.g. additional indent/dedent from @pretty)
    return resolved ? wrapDoc(doc, { ...resolved, wrapGroup: false }) : doc;
}

/**
 * Format a key-value pattern: key sep >> value
 *
 * Produces: keyDoc + "sep " + valueDoc
 */
function formatKeyValue(
    keyExpr: Expression,
    sepLiteral: string,
    valueExpr: Expression,
    values: unknown[],
    ast: AST,
    prettyMap: Map<string, PrettyHint[]>,
    currentRule?: string,
    resolved?: ResolvedHints,
): Doc {
    // For skip/next patterns: values = [key, value] (sep was discarded via >>)
    // For 3-element concatenation: values = [key, sep, value]
    const keyVal = values[0];
    const valueVal = values.length === 3 ? values[2] : values[1];

    const keyDoc = expressionToDoc(
        keyExpr,
        keyVal,
        ast,
        prettyMap,
        currentRule,
    );
    const valueDoc = expressionToDoc(
        valueExpr,
        valueVal,
        ast,
        prettyMap,
        currentRule,
    );
    const sepWithSpace = sepLiteral.trim() + " ";
    const doc = concat(keyDoc, text(sepWithSpace), valueDoc);
    return resolved ? wrapDoc(doc, resolved) : doc;
}

/**
 * Format a sep-by list pattern: first , (sep , rest)*.
 *
 * Runtime equivalent of the Rust `(A, Vec<A>) → Vec<A>` flatten pass
 * (type_inference.rs:255-270) + `generate_vec_doc` (prettify/mod.rs:402-508).
 *
 * The Rust codegen flattens the tuple at compile time; here we do it at
 * runtime by extracting the first item and the rest items from the parsed
 * value array, dropping the structural separator literals.
 *
 * Separator construction mirrors Rust's pattern exactly:
 *   IfBreak(trim_end(sep) + Hardline, sep)
 * — trailing separator style (comma at end of line when breaking).
 */
function formatSepByList(
    pattern: import("./patterns.js").SepByPattern,
    values: unknown[],
    ast: AST,
    prettyMap: Map<string, PrettyHint[]>,
    currentRule?: string,
): Doc {
    const firstVal = values[0];
    const restArr = Array.isArray(values[1]) ? (values[1] as unknown[][]) : [];

    // Flatten (A, Vec<(sep, A)>) → Vec<A>, mirroring type_inference flatten pass
    const allItems: unknown[] = [firstVal];
    for (const item of restArr) {
        if (Array.isArray(item) && item.length >= 2) {
            allItems.push(item[item.length - 1]);
        } else {
            allItems.push(item);
        }
    }

    const docs: Doc[] = allItems.map((item, i) =>
        expressionToDoc(
            i === 0 ? pattern.firstExpr : pattern.restExpr,
            item,
            ast,
            prettyMap,
            currentRule,
        ),
    );

    const hints = currentRule ? (prettyMap.get(currentRule) ?? []) : [];
    const resolved = resolveHints(hints);

    // Separator: prefer explicit @pretty sep() hints, otherwise derive from
    // the grammar's structural literal — mirrors generate_vec_doc separator
    // construction: IfBreak(trim_end(sep) + Hardline, Text(sep))
    let sep: Doc;
    if (resolved.separator || resolved.breakSeparator) {
        sep = applySeparator(resolved);
    } else {
        const trimmed = pattern.sepLiteral.trimEnd();
        sep = ifBreak(
            concat(text(trimmed), hardline),
            text(pattern.sepLiteral + " "),
        );
    }

    return wrapDoc(join(sep, docs), resolved);
}

/**
 * Format a span value with split("...") directive.
 *
 * Splits the string at the delimiter (respecting balanced nesting),
 * then joins with the resolved separator.
 */
function formatSplitSpan(value: string, resolved: ResolvedHints): Doc {
    const delim = resolved.splitDelim!;
    const delimByte = delim.charCodeAt(0);

    if (!containsDelimiter(value, delimByte)) {
        return wrapDoc(text(value), resolved);
    }

    const parts = splitBalanced(value, delimByte);
    if (parts.length <= 1) {
        return wrapDoc(text(value), resolved);
    }

    const docs = parts.map((s: string) => text(s.trim()));
    const sep = applySeparator(resolved);
    return wrapDoc(join(sep, docs), resolved);
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/**
 * Unwrap transparent expression wrappers (optionalWhitespace, group).
 */
function unwrapTransparentExpr(expr: Expression): Expression {
    if (expr.type === "optionalWhitespace" || expr.type === "group") {
        return unwrapTransparentExpr(expr.value as Expression);
    }
    return expr;
}

/**
 * Determine which children of a concatenation produce values.
 *
 * In ASTToParser, `all()` filters out undefined results. The expressions
 * that produce undefined are the discarded sides of skip/next operators.
 * However, in a concatenation, each direct child is a full expression that
 * produces a value, so we keep all of them. The undefined-filtering happens
 * within individual skip/next expressions, not at the concatenation level.
 *
 * Actually, looking at ASTToParser more carefully: all() skips `undefined`
 * values. This means if a child expression produces undefined (e.g., an
 * epsilon or optional that didn't match), it won't appear in the tuple.
 * For the interpreter, we simply map positionally.
 */
function getKeptChildren(children: Expression[]): Expression[] {
    return children;
}

/**
 * Check if resolved hints have a hard separator (block, blankline, hardbreak).
 */
function isHardSep(resolved: ResolvedHints): boolean {
    if (!resolved.separator) return false;
    return (
        resolved.separator.type === "hardline" ||
        (resolved.separator.type === "concat" &&
            resolved.separator.docs.every((d: Doc) => d.type === "hardline"))
    );
}
