#!/usr/bin/env node

/**
 * Native TypeScript/JavaScript parser benchmark.
 *
 * Measures parse throughput of the BBNF-generated TS parser running in Node.js.
 * Congruent with the Rust bencher pattern: cold per-parse, reports throughput.
 *
 * Usage:
 *   # Generate the parser (writes to benches/ts/generated_json.mjs):
 *   cargo test -p bbnf --test gen_ts_parser -- --nocapture
 *
 *   # Run the benchmark:
 *   node benches/ts/json_bench.mjs
 */

import { readFileSync, existsSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { performance } from "node:perf_hooks";

const __dirname = dirname(fileURLToPath(import.meta.url));
const dataDir = join(__dirname, "../../../../data/json");

// ── Load generated parser ───────────────────────────────────────────────────

const generatedPath = join(__dirname, "generated_json.mjs");

let generatedParse = null;
if (existsSync(generatedPath)) {
    const mod = await import(generatedPath);
    generatedParse = mod.parse;
}

// ── Benchmark runner ────────────────────────────────────────────────────────

function bench(name, input, parseFn, iterations = 100) {
    // Warmup.
    const warmup = parseFn(input);
    const parsed = warmup?.result !== null && warmup?.result !== undefined;

    // Measure.
    const start = performance.now();
    for (let i = 0; i < iterations; i++) {
        parseFn(input);
    }
    const elapsed = performance.now() - start;
    const avgMs = elapsed / iterations;
    const throughputMBs = (input.length / 1e6) / (avgMs / 1e3);
    const status = parsed ? "ok" : "FAIL";
    console.log(
        `  ${name.padEnd(24)} ${avgMs.toFixed(3).padStart(10)} ms/iter  ` +
        `${throughputMBs.toFixed(1).padStart(8)} MB/s  ` +
        `(${(input.length / 1024).toFixed(1)} KB)  [${status}]`
    );
}

// ── Main ────────────────────────────────────────────────────────────────────

const files = ["data.json", "twitter.json", "citm_catalog.json", "canada.json"];

console.log("BBNF JSON TypeScript parser benchmark (native Node.js)");
console.log("─".repeat(76));

if (!generatedParse) {
    console.log("\n  Generated parser not found at: " + generatedPath);
    console.log("  Run: cargo test -p bbnf --test gen_ts_parser -- --nocapture");
    console.log("  to generate benches/ts/generated_json.mjs\n");
    process.exit(1);
}

console.log("\n  BBNF-generated TS parser:\n");
for (const file of files) {
    try {
        const input = readFileSync(join(dataDir, file), "utf-8");
        const iterations = input.length > 500_000 ? 20 : 100;
        bench(file, input, (inp) => generatedParse(inp), iterations);
    } catch {
        console.log(`  ${file.padEnd(24)} (not found, skipping)`);
    }
}

// ── Comparison: JSON.parse ──────────────────────────────────────────────────

console.log("\n  JSON.parse (V8 native, baseline):\n");
for (const file of files) {
    try {
        const input = readFileSync(join(dataDir, file), "utf-8");
        const iterations = input.length > 500_000 ? 20 : 100;
        bench(file, input, (inp) => ({ result: JSON.parse(inp), offset: inp.length }), iterations);
    } catch {
        console.log(`  ${file.padEnd(24)} (not found, skipping)`);
    }
}
