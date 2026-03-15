/**
 * WASM benchmark for the Google Sheets grammar (Node.js).
 *
 * Measures compile, parse, and format throughput for three formula
 * complexities via the bbnf-wasm Node.js build.
 */

import { readFileSync } from "node:fs";
import { performance } from "node:perf_hooks";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";

import {
    compile_grammar,
    parse_with_grammar,
    parse_check,
    format_with_grammar,
    free_grammar,
    init_panic_hook,
} from "../pkg-node/bbnf_wasm.js";

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const SIMPLE = "=SUM(A1:A10)";

const LET =
    '=LET(data, A1:Z100, filtered, FILTER(data, INDEX(data,,1)>0), count, ROWS(filtered), IF(count>0, MAKEARRAY(count, 3, LAMBDA(r, c, INDEX(filtered, r, c))), "No data"))';

const PATHOLOGICAL =
    '=LET(raw, A2:E1000, filtered, FILTER(raw, (INDEX(raw,,3)>100)*(INDEX(raw,,5)="Active")), sorted, SORT(filtered, 3, FALSE), IF(ROWS(sorted)>0, MAP(SEQUENCE(MIN(10, ROWS(sorted))), LAMBDA(i, INDEX(sorted, i, 1)&" - "&TEXT(INDEX(sorted, i, 3), "$#,##0"))), "No results"))';

const ITERATIONS = 1000;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Collect `n` timing samples (ms) for `fn`, return sorted array. */
function bench(fn, n = ITERATIONS) {
    const samples = new Float64Array(n);
    // Warm-up: 10% of iterations, uncounted.
    const warmup = Math.max(10, Math.floor(n * 0.1));
    for (let i = 0; i < warmup; i++) fn();

    for (let i = 0; i < n; i++) {
        const t0 = performance.now();
        fn();
        samples[i] = performance.now() - t0;
    }
    samples.sort();
    return samples;
}

function median(sorted) {
    const mid = sorted.length >> 1;
    return sorted.length & 1
        ? sorted[mid]
        : (sorted[mid - 1] + sorted[mid]) / 2;
}

function mean(sorted) {
    let sum = 0;
    for (let i = 0; i < sorted.length; i++) sum += sorted[i];
    return sum / sorted.length;
}

function p99(sorted) {
    return sorted[Math.floor(sorted.length * 0.99)];
}

function fmtMs(ms) {
    if (ms < 0.001) return `${(ms * 1_000_000).toFixed(0)} ns`;
    if (ms < 1) return `${(ms * 1_000).toFixed(1)} us`;
    return `${ms.toFixed(3)} ms`;
}

function report(name, samples) {
    const med = median(samples);
    const avg = mean(samples);
    const p = p99(samples);
    console.log(
        `  ${name.padEnd(28)} median ${fmtMs(med).padStart(10)}  mean ${fmtMs(avg).padStart(10)}  p99 ${fmtMs(p).padStart(10)}`
    );
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

init_panic_hook();

const __dirname = dirname(fileURLToPath(import.meta.url));
const grammarPath = resolve(__dirname, "../../grammar/lang/google-sheets.bbnf");
const grammar = readFileSync(grammarPath, "utf-8");

console.log(`Google Sheets WASM benchmark — ${ITERATIONS} iterations\n`);
console.log(`Grammar: ${grammarPath}`);
console.log(`Grammar size: ${grammar.length} bytes\n`);

// -- compile ----------------------------------------------------------------

const compSamples = bench(() => {
    const h = compile_grammar(grammar);
    free_grammar(h);
});
report("compile", compSamples);

// Compile once for parse / format benchmarks.
const handle = compile_grammar(grammar);

// Verify parse succeeds before benchmarking.
for (const [label, input] of [["SIMPLE", SIMPLE], ["LET", LET], ["PATHOLOGICAL", PATHOLOGICAL]]) {
    const result = parse_with_grammar(handle, input);
    if (result == null || (typeof result === "object" && result.error)) {
        console.error(`Parse failed for ${label}:`, result);
        process.exit(1);
    }
}

// -- parse ------------------------------------------------------------------

console.log();
report("parse_simple", bench(() => parse_check(handle, SIMPLE)));
report("parse_let", bench(() => parse_check(handle, LET)));
report("parse_pathological", bench(() => parse_check(handle, PATHOLOGICAL)));

// -- format -----------------------------------------------------------------

console.log();
report("format_simple", bench(() => format_with_grammar(handle, SIMPLE, 80, 4, false)));
report("format_let", bench(() => format_with_grammar(handle, LET, 80, 4, false)));
report("format_pathological", bench(() => format_with_grammar(handle, PATHOLOGICAL, 80, 4, false)));

// -- throughput summary -----------------------------------------------------

console.log("\n--- throughput (median, bytes/sec) ---\n");

function throughput(label, input, samples) {
    const med = median(samples);
    const bps = (input.length / (med / 1000));
    const unit = bps > 1e6 ? `${(bps / 1e6).toFixed(1)} MB/s` : `${(bps / 1e3).toFixed(1)} KB/s`;
    console.log(`  ${label.padEnd(28)} ${unit.padStart(12)}  (${input.length} bytes)`);
}

const parseSamples = {
    simple: bench(() => parse_check(handle, SIMPLE)),
    let: bench(() => parse_check(handle, LET)),
    pathological: bench(() => parse_check(handle, PATHOLOGICAL)),
};

throughput("parse_simple", SIMPLE, parseSamples.simple);
throughput("parse_let", LET, parseSamples.let);
throughput("parse_pathological", PATHOLOGICAL, parseSamples.pathological);

free_grammar(handle);

console.log("\nDone.");
