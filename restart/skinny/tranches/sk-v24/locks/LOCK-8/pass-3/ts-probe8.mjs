// LOCK-8 pass-3 probe 2 — TS half. Cross-language undefined-compaction.
// all(string('a'), whitespace, string('b')).parse('ab')  — whitespace = regex(/\s*/)
// empty-matches between 'a' and 'b', yields undefined, and `all` compacts it out
// (leaf.ts arity-3 branch: `if (state.value !== undefined) out[w++] = state.value;
//  ... if (w !== 3) out.length = w;`). We assert the runtime .length.
import { all, string, whitespace } from "/Users/mkbabb/Programming/parse-that/typescript/dist/core.js";

const p = all(string("a"), whitespace, string("b"));
const value = p.parse("ab"); // parse returns the produced value (the compacted array)

const arr = Array.isArray(value) ? value : [value];
console.log("TS_INPUT        = \"ab\"");
console.log("TS_STATIC_TYPE  = Parser<[string, string, string]>  (declared arity 3)");
console.log("TS_VALUE        = " + JSON.stringify(value));
console.log("TS_VALUE_LENGTH = " + arr.length);
console.log("VERDICT_LENGTH=" + arr.length);
