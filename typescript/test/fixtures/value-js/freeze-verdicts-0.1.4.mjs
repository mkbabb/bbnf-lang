// SERVED MODEL: claude-opus-5-5
// Freezes the published @mkbabb/bbnf-lang 0.1.4 (on parse-that 0.8.2)'s verdicts — whole input accepted or not —
// over every source of value.js's bench of record (assay ∪ real, 29,944) that holds a non-ASCII code unit, for the
// 17 rules value.js calls (5 entries, ruleList, the 9 sheet.ts reader rules, the 3 splitters). The oracle of the
// stock-ASCII proof (test/stock-ascii.test.ts): 0.1.4's dispatch routes a non-ASCII unit by ASCII-only tables (F-b-4).
// usage: VALUE_JS=<value.js checkout, with node_modules> node freeze-verdicts-0.1.4.mjs <value.js commit> verdicts-0.1.4.json
import { readFileSync, writeFileSync } from "node:fs";
import { createHash } from "node:crypto";
const V = process.env.VALUE_JS ?? "../../value.js";
const stock = await import(`${V}/node_modules/@mkbabb/bbnf-lang/dist/bbnf.js`);
const PT = await import(`${V}/node_modules/@mkbabb/parse-that/dist/parse.js`);
const sha = (t) => createHash("sha256").update(t).digest("hex");
const corpusFiles = ["assay-corpus.json", "real-corpus.json"];
const corpusText = corpusFiles.map((f) => readFileSync(`${V}/bench/css-equivalence/${f}`, "utf8"));
const rows = (t) => JSON.parse(t).rows.map((r) => (typeof r.s === "string" ? r.s : r.s.src));
const inputs = [...new Set(corpusText.flatMap(rows))];
const sources = inputs.filter((s) => /[^\x00-\x7f]/.test(s));
const names = ["tokens", "math", "color", "value", "stylesheet"];
const texts = names.map((n) => readFileSync(`${V}/src/css/grammar/${n}.bbnf`, "utf8"));
const rules = ["colorTop", "scalarTop", "valueTop", "keyframeSelector", "timingFunction", "ruleList", "atPrelude", "declaration",
    "functionHead", "functionParam", "paramHead", "scopePrelude", "syntaxAlts", "syntaxText", "commaItems", "semiItems", "spaceItems"];
const [parsers] = stock.BBNFToParser(texts.join("\n"));
const err = console.error; console.error = () => {}; // 0.8.2 writes every failure to the console
const verdicts = Object.fromEntries(rules.map((r) => [r, sources.map((s) => {
    parsers[r].reset();
    const st = parsers[r].call(new PT.ParserState(s));
    return !st.isError && st.offset === s.length ? "1" : "0";
}).join("")]));
console.error = err;
const out = {
    producer: "@mkbabb/bbnf-lang 0.1.4 BBNFToParser over the concatenation (value.js load.ts:42) on @mkbabb/parse-that 0.8.2; verdict = whole input accepted",
    source: { repo: "value.js", commit: process.argv[2], corpus: Object.fromEntries(corpusFiles.map((f, i) => [f, sha(corpusText[i])])),
        modules: Object.fromEntries(names.map((n, i) => [n, sha(texts[i])])), population: inputs.length },
    rules, sources, verdicts,
};
writeFileSync(process.argv[3], JSON.stringify(out, null, 1) + "\n");
console.log("sources", sources.length, "of", inputs.length);
