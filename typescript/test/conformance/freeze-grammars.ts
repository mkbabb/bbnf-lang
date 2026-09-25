// SERVED MODEL: claude-opus-5-5
// Freezes grammars.json: bbnf-lang's own grammars (test/fixtures/grammar), compiled at runtime,
// answering fixed inputs. Run from typescript/: npx vite-node test/conformance/freeze-grammars.ts
// The frozen answers are the reference every later face (the emitted module, the facade) must
// reproduce; they were read against VALUE-SEMANTICS.md, not taken on trust.
import { readFileSync, writeFileSync } from "node:fs";
import { BBNFToParserFromFile } from "../../src/generate.js";
import { CASES, encode } from "./grammar-cases.js";

const readFile = (p: string) => readFileSync(p, "utf8");
const out = CASES.map(({ grammar, rule, input }) => {
    const [nt] = BBNFToParserFromFile(`${process.cwd()}/test/fixtures/grammar/${grammar}`, readFile);
    const st = nt[rule].parseState(input);
    return { grammar, rule, input, expect: st.isError ? { ok: false } : { ok: true, value: encode(st.value), end: st.offset } };
});
writeFileSync("test/conformance/grammars.json", JSON.stringify({ note: "bbnf-lang's own grammars at runtime: see freeze-grammars.ts", cases: out }) + "\n");
console.log("cases", out.length, "ok", out.filter((c) => c.expect.ok).length);
