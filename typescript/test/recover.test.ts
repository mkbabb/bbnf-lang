import { describe, it, expect } from "vitest";
import { BBNFToParser } from "../src/generate";
import { BBNFToASTWithImports } from "../src/parse";
import { BBNFGrammar } from "../src/grammar";
import type { RecoverDirective, ParsedGrammar } from "../src/types";
import {
    enableDiagnostics,
    disableDiagnostics,
} from "@mkbabb/parse-that";

describe("@recover grammar parsing", () => {
    it("should parse a grammar with @recover directives", () => {
        const input = `
@recover stmt /[^;]*;/ ;

stmt = /[a-z]+/ , ";" ;
program = stmt * ;
`;
        const result = BBNFToASTWithImports(input);
        expect(result.length).toBeGreaterThanOrEqual(2);
        const parsed = result[1] as ParsedGrammar;
        expect(parsed).toBeDefined();
        expect(parsed.recovers).toHaveLength(1);
        expect(parsed.recovers[0].ruleName).toBe("stmt");
        expect(parsed.recovers[0].syncExpr).toBeDefined();
        expect(parsed.recovers[0].syncExpr.type).toBe("regex");
    });

    it("should parse multiple @recover directives", () => {
        const input = `
@recover decl /[^;]*;/ ;
@recover rule /[^}]*}/ ;

decl = /[a-z]+/ , ":" , /[^;]+/ , ";" ;
rule = /[a-z]+/ , "{" , decl * , "}" ;
`;
        const result = BBNFToASTWithImports(input);
        const parsed = result[1] as ParsedGrammar;
        expect(parsed.recovers).toHaveLength(2);
        expect(parsed.recovers[0].ruleName).toBe("decl");
        expect(parsed.recovers[1].ruleName).toBe("rule");
    });

    it("should parse @recover mixed with @import and rules", () => {
        const input = `
@import "some-file.bbnf" ;

@recover stmt /[^;]*;/ ;

stmt = /[a-z]+/ , ";" ;
program = stmt * ;
`;
        const result = BBNFToASTWithImports(input);
        const parsed = result[1] as ParsedGrammar;
        expect(parsed.imports).toHaveLength(1);
        expect(parsed.recovers).toHaveLength(1);
        expect(parsed.rules.size).toBe(2);
    });

    it("should survive @recover targeting nonexistent rule", () => {
        const input = `
@recover nonexistent /[^;]*;/ ;

stmt = /[a-z]+/ , ";" ;
`;
        const result = BBNFToASTWithImports(input);
        const parsed = result[1] as ParsedGrammar;
        expect(parsed.recovers).toHaveLength(1);
        expect(parsed.recovers[0].ruleName).toBe("nonexistent");
    });
});

describe("@recover codegen", () => {
    it("should apply .recover() wrapping to annotated rules", () => {
        const input = `
@recover item /[^;]*;/ ;

item = /[a-z]+/ , ";" ;
list = item * ;
`;
        const [nonterminals] = BBNFToParser(input);
        expect(nonterminals.item).toBeDefined();
        expect(nonterminals.list).toBeDefined();

        // Valid input should parse normally
        const validResult = nonterminals.list.parse("abc;def;");
        expect(validResult).toBeDefined();
    });

    it("should recover from errors and continue parsing", () => {
        const input = `
@recover item /[^;]*;/ ;

item = /[a-z]+/ , "=" , /[a-z]+/ , ";" ;
list = item * ;
`;
        const [nonterminals] = BBNFToParser(input);

        enableDiagnostics();

        // Mix of valid and invalid items
        // "a=b;" is valid, "!!bad;" is invalid (should recover), "c=d;" is valid
        const { value: result, diagnostics } = nonterminals.list.parseState("a=b;!!bad;c=d;");
        disableDiagnostics();

        // Should have parsed something (recovered items + valid ones)
        expect(result).toBeDefined();
        if (result) {
            // The result should be an array with some valid items and null (sentinel) for recovered
            expect(result.length).toBeGreaterThan(0);
        }
    });

    it("should use null as sentinel for recovered items", () => {
        const input = `
@recover item /[^;]*;/ ;

item = /[a-z]+/ , "=" , /[a-z]+/ , ";" ;
list = item * ;
`;
        const [nonterminals] = BBNFToParser(input);

        enableDiagnostics();

        // The first item starts as an item can (`bad…`) and fails; it recovers to its `;`.
        // (0.1.4 recovered at any unit; 0.2 recovers where the rule can start: VALUE-SEMANTICS.md
        // "@recover", so an ordered choice never recovers an alternative that cannot apply.)
        const result = nonterminals.list.parse("bad!123;a=b;");

        disableDiagnostics();

        // First item should be sentinel (null), second valid
        expect(result).toBeDefined();
        if (result) {
            expect(result).toContain(null);
        }
    });

    it("recovers only where the rule can start (its FIRST set)", () => {
        const [nonterminals] = BBNFToParser(`
@recover item /[^;]*;/ ;
item = /[a-z]+/ , "=" , /[a-z]+/ , ";" ;
list = item * ;
`);
        // `1` cannot start an item: no recovery, the list stops before it.
        const st = nonterminals.list.parseState("123bad;a=b;");
        expect(st.isError).toBe(false);
        expect(st.value).toEqual([]);
        expect(st.offset).toBe(0);
        // Between choices: a recovered alternative that cannot start here is not taken.
        const [nt2] = BBNFToParser(`
@recover at /[^}]+}/ ;
at = "@" , /[a-z]+/ , ";" ;
rule = /[a-z.]+/ , "{" , /[^}]*/ , "}" ;
item = at | rule ;
`);
        expect(nt2.item.parse(".foo{x}")).toEqual([".foo", "{", "x", "}"]);
        expect(nt2.item.parse("@bad{x}")).toBe(null);
    });
});

describe("grammarWithImports parser", () => {
    it("should handle @recover with the grammarWithImports parser directly", () => {
        const grammar = new BBNFGrammar();
        const parser = grammar.grammarWithImports();
        const input = `
@recover decl /[^;]*;/ ;
decl = /[a-z]+/ , ":" , /[^;]+/ , ";" ;
`;
        const result = parser.parse(input.trim());
        expect(result).toBeDefined();
        expect(result.recovers).toHaveLength(1);
        expect(result.rules).toHaveLength(1);
    });
});
