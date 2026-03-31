/**
 * WASM Google Sheets grammar test suite (Node.js).
 *
 * Validates compile, parse, and format operations for the Google Sheets
 * formula grammar via the bbnf-wasm Node.js build.
 */

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";

import {
    compile_grammar,
    parse_check,
    format_with_grammar,
    free_grammar,
    init_panic_hook,
} from "../pkg-node/bbnf_wasm.js";

// ---------------------------------------------------------------------------
// Setup
// ---------------------------------------------------------------------------

init_panic_hook();

const __dirname = dirname(fileURLToPath(import.meta.url));
const grammarPath = resolve(__dirname, "../../grammar/google-sheets/google-sheets.bbnf");
const grammar = readFileSync(grammarPath, "utf-8");

let passed = 0;
let failed = 0;

function assert(condition, name) {
    if (condition) {
        console.log(`  PASS  ${name}`);
        passed++;
    } else {
        console.log(`  FAIL  ${name}`);
        failed++;
    }
}

// ---------------------------------------------------------------------------
// Test formulas
// ---------------------------------------------------------------------------

const SIMPLE = "=SUM(A1:A10)";
const IF_FORMULA = "=IF(1,2,3)";
const LET_SIMPLE = "=LET(x,1,y,2,x)";

const LET_COMPLEX =
    '=LET(data, A1:Z100, filtered, FILTER(data, INDEX(data,,1)>0), count, ROWS(filtered), IF(count>0, MAKEARRAY(count, 3, LAMBDA(r, c, INDEX(filtered, r, c))), "No data"))';

const PATHOLOGICAL =
    '=LET(raw, A2:E1000, filtered, FILTER(raw, (INDEX(raw,,3)>100)*(INDEX(raw,,5)="Active")), sorted, SORT(filtered, 3, FALSE), IF(ROWS(sorted)>0, MAP(SEQUENCE(MIN(10, ROWS(sorted))), LAMBDA(i, INDEX(sorted, i, 1)&" - "&TEXT(INDEX(sorted, i, 3), "$#,##0"))), "No results"))';

// ---------------------------------------------------------------------------
// Compile
// ---------------------------------------------------------------------------

console.log("\n=== Google Sheets WASM Grammar Tests ===\n");
console.log(`Grammar: ${grammarPath} (${grammar.length} bytes)\n`);

const handle = compile_grammar(grammar, "formula");
assert(typeof handle === "number" && handle > 0, "compile_grammar returns valid handle");

// ---------------------------------------------------------------------------
// Parse tests
// ---------------------------------------------------------------------------

console.log("\n--- Parse ---\n");

{
    const r = parse_check(handle, SIMPLE);
    assert(r.success === true, `parse "${SIMPLE}" succeeds`);
    assert(r.offset === SIMPLE.length, `parse "${SIMPLE}" offset = ${SIMPLE.length} (got ${r.offset})`);
}

{
    const r = parse_check(handle, IF_FORMULA);
    assert(r.success === true, `parse "${IF_FORMULA}" succeeds`);
    assert(r.offset === IF_FORMULA.length, `parse "${IF_FORMULA}" offset = ${IF_FORMULA.length} (got ${r.offset})`);
}

{
    const r = parse_check(handle, LET_SIMPLE);
    assert(r.success === true, `parse "${LET_SIMPLE}" succeeds`);
}

{
    const r = parse_check(handle, LET_COMPLEX);
    assert(r.success === true, "parse LET_COMPLEX succeeds");
    assert(r.offset === LET_COMPLEX.length, `parse LET_COMPLEX full offset = ${LET_COMPLEX.length} (got ${r.offset})`);
}

{
    const r = parse_check(handle, PATHOLOGICAL);
    assert(r.success === true, "parse PATHOLOGICAL succeeds");
    assert(r.offset === PATHOLOGICAL.length, `parse PATHOLOGICAL full offset = ${PATHOLOGICAL.length} (got ${r.offset})`);
}

// ---------------------------------------------------------------------------
// Format tests
// ---------------------------------------------------------------------------

console.log("\n--- Format ---\n");

{
    const result = format_with_grammar(handle, PATHOLOGICAL, 80, 4, false);
    assert(result != null, "format PATHOLOGICAL returns non-null");
    assert(typeof result === "string" && result.includes("LET("), "format PATHOLOGICAL contains 'LET('");
    assert(typeof result === "string" && result.includes("\n"), "format PATHOLOGICAL contains newlines");
}

{
    const result = format_with_grammar(handle, SIMPLE, 80, 4, false);
    assert(result != null, "format SIMPLE returns non-null");
}

{
    const result = format_with_grammar(handle, LET_COMPLEX, 80, 4, false);
    assert(result != null, "format LET_COMPLEX returns non-null");
    assert(typeof result === "string" && result.includes("LET("), "format LET_COMPLEX contains 'LET('");
}

// ---------------------------------------------------------------------------
// Cleanup & summary
// ---------------------------------------------------------------------------

free_grammar(handle);

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===\n`);

if (failed > 0) {
    process.exit(1);
}
