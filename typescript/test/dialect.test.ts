/**
 * The BBNF constructs the TS front end gained at sk-v27 LW2.PAR-1, each against
 * the docs/bbnf sentence that rules it (restart/skinny/tranches/sk-v27/grammar/DIVERGENCE.tsv).
 */
import { describe, expect, it } from "vitest";

import { BBNFToASTWithImports } from "../src/parse";
import type { Expression, Group } from "../src/types";

function parse(src: string) {
    const result = BBNFToASTWithImports(src);
    expect(result.length, `rejected: ${src}`).toBe(2);
    return result[1]!;
}

function rule(src: string, name: string): Expression {
    return parse(src).rules.get(name)!.expression;
}

describe("BBNF dialect (LW2.PAR-1)", () => {
    it("maps a factor with ->, keeping the host expression as text (grammar-syntax.md, Per-Expression Mapping)", () => {
        const unit = rule(`unit = "px" -> 0u8 | "em" -> 1u8 ;`, "unit");
        expect(unit.type).toBe("alternation");
        const [px, em] = unit.value as Expression[];
        expect(px).toMatchObject({ type: "literal", value: "px", mapping: { expr: "0u8" } });
        expect(em).toMatchObject({ type: "literal", value: "em", mapping: { expr: "1u8" } });
    });

    it("reads a call, a path, `input` fields and a type annotation on the right of ->", () => {
        const hex = rule(`hex = /[0-9a-f]+/ -> crate::css_types::parse_hex_color(input) : u32 ;`, "hex");
        expect(hex.mapping).toEqual({ expr: "crate::css_types::parse_hex_color(input)", type: "u32" });
        const span = rule(`id = /[a-z]+/ -> Span ;`, "id");
        expect(span.mapping).toEqual({ expr: "Span" });
        const field = rule(`f = "x" -> input.start + 1 ;`, "f");
        expect(field.mapping?.expr).toBe("input.start+1");
    });

    it("reads a closure on the right of ->, without taking the alternation bar", () => {
        // expressions.bbnf: value_closure = "|" , value_ident , ... , "|" , value_expr (no
        // whitespace operator before the body, so the body follows the bar directly).
        const r = rule(`r = "a" -> |s|f(s) | "b" ;`, "r");
        expect(r.type).toBe("alternation");
        expect((r.value as Expression[])[0].mapping).toEqual({ expr: "|s|f(s)" });
    });

    it("does not change what a mapped factor matches", () => {
        const plain = rule(`a = "x" , ( "y" | "z" ) * ;`, "a");
        const mapped = rule(`a = "x" -> 0u8 , ( "y" | "z" ) * -> 1u8 ;`, "a");
        const strip = (e: any): any =>
            Array.isArray(e) ? e.map(strip) : e && typeof e === "object"
                ? Object.fromEntries(Object.entries(e).filter(([k]) => k !== "mapping" && k !== "range" && k !== "comment").map(([k, v]) => [k, strip(v)]))
                : e;
        expect(strip(mapped)).toEqual(strip(plain));
    });

    it("concatenates juxtaposed operands (operators.md, Concatenation: the comma is optional)", () => {
        const pair = rule(`pair = key ":" value ;`, "pair");
        expect(pair).toMatchObject({ type: "concatenation" });
        expect((pair.value as Expression[]).map((e) => e.type)).toEqual(["nonterminal", "literal", "nonterminal"]);
        const mixed = rule(`m = a , b c ;`, "m");
        expect((mixed.value as Expression[]).length).toBe(3);
    });

    it("captures a span with @{ … } (grammar-syntax.md, Span Capture)", () => {
        const url = rule(`url = @{ "url" , "(" >> /[^)]+/ << ")" } ;`, "url") as Group;
        expect(url.type).toBe("group");
        expect(url.capture).toBe("span");
    });

    it("collects @ws, @token, @inline and @debug (grammar-syntax.md, Directives Summary)", () => {
        const g = parse(`@ws /[ \\t]+/ ;\n@token ident ;\n@inline atom ;\n@debug * ;\n@debug ident ;\nident = /[a-z]+/ ;\natom = ident ;`);
        expect(g.ws.map((d) => d.pattern.source)).toEqual(["[ \\t]+"]);
        expect(g.tokens.map((d) => d.ruleName)).toEqual(["ident"]);
        expect(g.inlines.map((d) => d.ruleName)).toEqual(["atom"]);
        expect(g.debugs.map((d) => d.ruleName)).toEqual(["*", "ident"]);
        expect([...g.rules.keys()]).toEqual(["ident", "atom"]);
    });

    it("reads a leading inline flag group as RegExp flags (grammar-syntax.md, Regular Expressions)", () => {
        const g = parse(`@ws /(?s)(?:\\s|\\/\\*.*?\\*\\/)*/ ;\nx = "a" ;`);
        expect(g.ws[0].pattern.flags).toContain("s");
        expect(g.ws[0].pattern.source.startsWith("(?s)")).toBe(false);
        // `x` has no ECMAScript flag: the pattern stays as written, and RegExp refuses it.
        expect(() => BBNFToASTWithImports(`x = /(?x)a b/ ;`)).toThrow(SyntaxError);
    });
});
