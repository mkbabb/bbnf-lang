/**
 * Math parsers: inline math ($...$) and display math environments.
 */

import { Parser, regex, string } from "@mkbabb/parse-that";
import type { MathNode } from "../types/ast";
import { rawUntilEnd } from "./primitives";

/** Inline math: $...$ (not $$). Uses imperative parsing to avoid $$ ambiguity. */
export const inlineMath: Parser<MathNode> = new Parser((state) => {
    if (state.src[state.offset] !== "$" || state.src[state.offset + 1] === "$") {
        state.isError = true;
        return state;
    }
    const start = state.offset + 1;
    const end = state.src.indexOf("$", start);
    if (end === -1) {
        state.isError = true;
        return state;
    }
    state.value = {
        type: "math",
        value: state.src.slice(start, end),
        display: false,
    } as any;
    state.offset = end + 1;
    state.isError = false;
    return state;
});

/** Display math: $$...$$ */
export const displayMathDollar: Parser<MathNode> = string("$$")
    .next(regex(/[^$]+/))
    .skip(string("$$"))
    .map((value) => ({
        type: "math" as const,
        value: value.trim(),
        display: true,
        environment: "$$",
        numbered: false,
    }));

/** Display math: \[...\] */
export const displayMathBracket: Parser<MathNode> = string("\\[")
    .next(regex(/[\s\S]*?(?=\\\])/))
    .skip(string("\\]"))
    .map((value) => ({
        type: "math" as const,
        value: value.trim(),
        display: true,
        environment: "\\[\\]",
        numbered: false,
    }));

/** Inline math: \(...\) */
export const inlineMathParen: Parser<MathNode> = string("\\(")
    .next(regex(/[\s\S]*?(?=\\\))/))
    .skip(string("\\)"))
    .map((value) => ({
        type: "math" as const,
        value: value.trim(),
        display: false,
    }));

/** Parse a math environment body (equation, align, align*, gather, etc.). */
export function mathEnvBody(
    envName: string,
): Parser<MathNode> {
    return rawUntilEnd(envName).map((raw) => {
        const rawValue = raw.trim();
        // Remove \label{...} for rendering
        let value = rawValue.replace(/\\label\{[^}]*\}/g, "").trim();
        // Wrap align/align* in aligned for KaTeX
        if (envName === "align" || envName === "align*") {
            value = `\\begin{aligned} ${value} \\end{aligned}`;
        }
        return {
            type: "math" as const,
            value,
            display: true,
            environment: envName,
            numbered: !envName.endsWith("*") && envName !== "split",
            rawValue,
        };
    });
}

const MATH_ENVS = [
    "equation",
    "equation*",
    "align",
    "align*",
    "gather",
    "gather*",
    "multline",
    "multline*",
    "flalign",
    "flalign*",
    "split",
];

/** Check if an environment name is a math environment. */
export function isMathEnv(name: string): boolean {
    return MATH_ENVS.includes(name);
}
