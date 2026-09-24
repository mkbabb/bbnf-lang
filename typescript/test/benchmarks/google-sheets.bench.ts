import { describe, bench, type BenchOptions } from "vitest";
import fs from "fs";
import path from "path";

import { BBNFToParser } from "../../src/generate";

// ---------------------------------------------------------------------------
// Grammar compilation (one-time)
// ---------------------------------------------------------------------------
const grammarPath = path.resolve(__dirname, "../../../grammar/google-sheets/google-sheets.bbnf");
const grammarSrc = fs.readFileSync(grammarPath, "utf8");
const [nonterminals] = BBNFToParser(grammarSrc);
const parser = nonterminals.formula;

// Suppress console.error from parse-that on intentional error paths
const origError = console.error;
function suppress() { console.error = () => {}; }
function restore() { console.error = origError; }

// ---------------------------------------------------------------------------
// Test formulas
// ---------------------------------------------------------------------------

// Pathological: deeply nested LET with chained function calls
const pathological =
    "=LET(base,SUMPRODUCT((revenue_range)*(margin_range)),tax,IF(base>1000000,base*0.35,base*0.21),net,base-tax,adjusted,IF(AND(net>500000,YEAR(TODAY())=2026),net*1.05,net),ROUND(adjusted,2))";

// ~1KB generated formula: wide SUMPRODUCT with many terms
function generateWideFormula(targetBytes: number): string {
    const parts: string[] = [];
    let current = "=SUMPRODUCT(";
    parts.push(current);

    let i = 0;
    while (Buffer.byteLength(parts.join(""), "utf-8") < targetBytes - 30) {
        if (i > 0) parts.push(",");
        // Each term: IF(A{n}>0,A{n}*B{n},0)
        const col1 = `A${i + 1}`;
        const col2 = `B${i + 1}`;
        parts.push(`IF(${col1}>0,${col1}*${col2},0)`);
        i++;
    }
    parts.push(")");
    return parts.join("");
}

const wideFormula = generateWideFormula(1024);
const wideFormulaBytes = Buffer.byteLength(wideFormula, "utf-8");

// Simple formula for baseline
const simple = "=SUM(A1:A10)";

// ---------------------------------------------------------------------------
// Benchmark options
// ---------------------------------------------------------------------------
const options: BenchOptions = {
    warmupIterations: 50,
    time: 2000,
};

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------
describe(`Google Sheets — simple (${Buffer.byteLength(simple)} B)`, () => {
    bench("parse", () => {
        suppress();
        try { parser.parse(simple); } finally { restore(); }
    }, options);
});

describe(`Google Sheets — pathological LET (${Buffer.byteLength(pathological)} B)`, () => {
    bench("parse", () => {
        suppress();
        try { parser.parse(pathological); } finally { restore(); }
    }, options);
});

describe(`Google Sheets — wide SUMPRODUCT (~${Math.round(wideFormulaBytes / 1024)} KB)`, () => {
    bench("parse", () => {
        suppress();
        try { parser.parse(wideFormula); } finally { restore(); }
    }, options);
});
