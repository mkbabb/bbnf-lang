import { describe, it, expect } from "vitest";
import fs from "fs";
import path from "path";

import { BBNFToParser } from "../src/generate";

const grammarPath = path.resolve(__dirname, "../../grammar/google-sheets/google-sheets.bbnf");
const grammarSrc = fs.readFileSync(grammarPath, "utf8");

function sheetsParser() {
    const [nonterminals] = BBNFToParser(grammarSrc);
    return nonterminals.formula.eof();
}

describe("Google Sheets formula parser", () => {
    const parser = sheetsParser();

    // Formulas that must parse successfully and consume all input.
    const formulas: [string, string][] = [
        ["simple SUM", "=SUM(A1:A10)"],
        ["IF with literals", "=IF(1,2,3)"],
        ["LET with bindings", "=LET(x,1,y,2,x)"],
        [
            "complex nested LET",
            "=LET(base,SUMPRODUCT((revenue_range)*(margin_range)),tax,IF(base>1000000,base*0.35,base*0.21),net,base-tax,adjusted,IF(AND(net>500000,YEAR(TODAY())=2026),net*1.05,net),ROUND(adjusted,2))",
        ],
        ["nested functions", "=VLOOKUP(A1,Sheet1!B2:D100,3,FALSE)"],
        ["array literal", "={1,2,3;4,5,6}"],
        ["string argument", '=CONCATENATE("hello","world")'],
        ["percentage", "=A1*50%"],
        ["comparison", "=IF(A1>=B1,A1,B1)"],
        ["concatenation operator", '=A1&" "&B1'],
        ["unary minus", "=-SUM(A1:A10)"],
        ["exponentiation", "=2^10"],
        ["error literal", "=IFERROR(A1/B1,#N/A)"],
        ["sheet prefix with quotes", "='My Sheet'!A1"],
        ["boolean", "=AND(TRUE,FALSE)"],
    ];

    for (const [label, formula] of formulas) {
        it(`should parse: ${label}`, () => {
            const state = parser.parseState(formula);
            expect(state.isError).toBe(false);
            expect(state.offset).toBe(formula.length);
        });
    }

    it("should reject incomplete input", () => {
        // Missing closing paren — eof() should fail since input is not fully consumed.
        const bad = "=SUM(A1:A10";
        const state = parser.parseState(bad);
        expect(state.isError).toBe(true);
    });
});
