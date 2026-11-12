// sk-v27 LW3.COMP-5b: graphql-js as the oracle of the held-out GraphQL grammar
// (ruling H-1: GraphQL October 2021, the graphql-js corpus).
//
// Runs the pinned `graphql` package (crates/oracle/ts/pin.json, set
// "graphql-js"; unpacked by the harness after its sha256 is checked). One
// request on stdin: { "package": "<dir>", "inputs": ["<abs path>", ...] }.
// One JSON line per input: { "input", "verdict": "accept" | "reject" | "unreadable",
// "tokens"?: [raw token text, ...], "offset"?: <UTF-16 offset of the error> }.
// `tokens` is the lexer's token stream (no <SOF>/<EOF>; comments are not
// tokens), each as the source text it spans: the scalar events of a parse.
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";

const req = JSON.parse(readFileSync(0, "utf8"));
const require = createRequire(path.join(req.package, "package.json"));
const { parse, Lexer, Source, TokenKind } = require("graphql");

const out = [];
for (const input of req.inputs) {
    let src;
    try {
        src = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true }).decode(readFileSync(input));
    } catch {
        out.push({ input, verdict: "unreadable" });
        continue;
    }
    try {
        parse(src, { noLocation: false });
        const lexer = new Lexer(new Source(src));
        const tokens = [];
        for (let t = lexer.advance(); t.kind !== TokenKind.EOF; t = lexer.advance()) {
            tokens.push(src.slice(t.start, t.end));
        }
        out.push({ input, verdict: "accept", tokens });
    } catch (e) {
        out.push({ input, verdict: "reject", offset: e?.positions?.[0] ?? null, message: String(e?.message ?? e).slice(0, 200) });
    }
}
process.stdout.write(out.map((r) => JSON.stringify(r)).join("\n") + (out.length ? "\n" : ""));
