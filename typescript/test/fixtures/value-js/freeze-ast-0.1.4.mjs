// SERVED MODEL: claude-opus-5-5
// Freezes the published @mkbabb/bbnf-lang 0.1.4's AST of value.js's five modules, concatenated (value.js load.ts:42).
import { readFileSync, writeFileSync } from "node:fs";
import { createHash } from "node:crypto";
const V = process.env.VALUE_JS ?? "../../value.js";
const stock = await import(`${V}/node_modules/@mkbabb/bbnf-lang/dist/bbnf.js`);
const names = ["tokens", "math", "color", "value", "stylesheet"];
const texts = names.map((n) => readFileSync(`${V}/src/css/grammar/${n}.bbnf`, "utf8"));
const [, g] = stock.BBNFToASTWithImports(texts.join("\n"));
const strip = (ast) => JSON.parse(JSON.stringify([...ast].map(([k, r]) => [k, r.expression]), (k, v) => (k === "range" || k === "comment" ? undefined : v instanceof RegExp ? `/${v.source}/${v.flags}` : v)));
// usage: VALUE_JS=<value.js checkout, with node_modules> node freeze-ast-0.1.4.mjs <value.js commit> ast-0.1.4.json
const out = {
  producer: "@mkbabb/bbnf-lang 0.1.4 (published dist) BBNFToASTWithImports over the concatenation (value.js src/css/bbnf/load.ts:42)",
  source: { repo: "value.js", commit: process.argv[2], modules: Object.fromEntries(names.map((n, i) => [n, createHash("sha256").update(texts[i]).digest("hex")])) },
  rules: g.rules.size,
  ast: strip(g.rules),
};
writeFileSync(process.argv[3], JSON.stringify(out) + "\n");
console.log("rules", g.rules.size);
