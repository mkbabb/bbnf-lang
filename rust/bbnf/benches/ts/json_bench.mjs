#!/usr/bin/env node

/**
 * BBNF JSON TypeScript parser benchmark — native Node.js throughput.
 *
 * Compares three approaches:
 *   1. BBNF-generated TS (monolithic recursive descent from Rust backend)
 *   2. BBNF runtime TS (combinator chains via @mkbabb/bbnf-lang)
 *   3. JSON.parse (V8 native baseline)
 *
 * Usage:
 *   # First generate the TS parser:
 *   cargo test -p bbnf --test gen_ts_parser -- --nocapture
 *
 *   # Then run (from rust/bbnf/):
 *   node benches/ts/json_bench.mjs
 */

import { readFileSync, existsSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { performance } from "node:perf_hooks";

const __dirname = dirname(fileURLToPath(import.meta.url));
const repoRoot = join(__dirname, "../../../..");
const dataDir = join(repoRoot, "data/json");
const grammarPath = join(repoRoot, "grammar/json/json.bbnf");

// ── Load BBNF-generated TS parser ───────────────────────────────────────────

const generatedPath = join(__dirname, "generated_json.mjs");
let generatedParse = null;
if (existsSync(generatedPath)) {
    const mod = await import(generatedPath);
    generatedParse = mod.parse;
}

// ── Load BBNF runtime TS parser (combinator approach) ───────────────────────

let runtimeParser = null;
try {
    const bbnfPkg = join(repoRoot, "typescript/dist/bbnf.js");
    if (existsSync(bbnfPkg)) {
        const { BBNFToParser } = await import(bbnfPkg);
        // The TS runtime doesn't support `->` mapping syntax (Rust-only).
        // Strip `->` and suffixed values from the grammar, then apply
        // value mappings via .map() — same pattern as the TS test suite.
        let grammarSrc = readFileSync(grammarPath, "utf-8");
        grammarSrc = grammarSrc.replace(/\s*->\s*\S+/g, "");
        grammarSrc = grammarSrc.replace(/@pretty[^;]*;/g, "");
        const [nonterminals] = BBNFToParser(grammarSrc);
        // Apply value mappings (matches typescript/test/bbnf.test.ts JSONParser).
        nonterminals.null = nonterminals.null.map(() => null);
        nonterminals.bool = nonterminals.bool.map((v) => v === "true");
        nonterminals.number = nonterminals.number.map(Number);
        nonterminals.string = nonterminals.string.map((s) =>
            s.indexOf("\\") === -1 ? s.slice(1, -1) : JSON.parse(s));
        nonterminals.object = nonterminals.object.map(
            (pairs) => Object.fromEntries(pairs));
        runtimeParser = nonterminals.value.trim();
    }
} catch (e) {
    console.error(`  (BBNF runtime load error: ${e.message})`);
}

// ── Benchmark runner ────────────────────────────────────────────────────────

function bench(name, input, parseFn, iterations = 100) {
    // Warmup.
    const warmup = parseFn(input);
    const ok = warmup != null && warmup !== undefined;

    // Measure.
    const start = performance.now();
    for (let i = 0; i < iterations; i++) {
        parseFn(input);
    }
    const elapsed = performance.now() - start;
    const avgMs = elapsed / iterations;
    const throughputMBs = (input.length / 1e6) / (avgMs / 1e3);
    const status = ok ? "ok" : "FAIL";
    console.log(
        `    ${name.padEnd(20)} ${avgMs.toFixed(3).padStart(10)} ms/iter  ` +
        `${throughputMBs.toFixed(1).padStart(8)} MB/s  [${status}]`
    );
    return { avgMs, throughputMBs, ok };
}

// ── Main ────────────────────────────────────────────────────────────────────

const files = ["data.json", "twitter.json", "citm_catalog.json", "canada.json"];

console.log("BBNF JSON Parser Benchmark — TypeScript (Node.js)");
console.log("═".repeat(72));

for (const file of files) {
    let input;
    try {
        input = readFileSync(join(dataDir, file), "utf-8");
    } catch {
        console.log(`\n  ${file}: not found, skipping`);
        continue;
    }
    const kb = (input.length / 1024).toFixed(1);
    const iterations = input.length > 500_000 ? 20 : 100;
    console.log(`\n  ${file} (${kb} KB, ${iterations} iters):`);

    // JSON.parse baseline.
    bench("JSON.parse", input,
        (inp) => JSON.parse(inp), iterations);

    // BBNF runtime (combinator chains).
    if (runtimeParser) {
        bench("BBNF runtime TS", input,
            (inp) => runtimeParser.parse(inp), iterations);
    } else {
        console.log("    BBNF runtime TS     (not available — build typescript/ first)");
    }

    // BBNF-generated TS (monolithic recursive descent).
    if (generatedParse) {
        bench("BBNF generated TS", input,
            (inp) => generatedParse(inp), iterations);
    } else {
        console.log("    BBNF generated TS   (not available — run gen_ts_parser test first)");
    }
}

console.log("\n" + "═".repeat(72));
