// sk-v27 LW3.ORC2: the peggy oracle's driver (ruling ORC-2).
//
// Runs the pinned peggy package (crates/oracle/ts/pin.json, set "peggy";
// unpacked by the harness after its sha256 is checked) over a hand
// transcription of a grammar of record (crates/oracle/peg/<id>.peggy). One
// request on stdin:
//   { "package": "<dir>", "grammar": "<abs .peggy>", "start": "<rule>",
//     "inputs": ["<abs path>", ...] }
// One JSON line per input: { "input", "verdict": "accept" | "reject" | "unreadable",
//   "offset"?: <UTF-16 offset of the failure>, "message"? }.
// An input is read as UTF-8 with its bytes kept: a byte-order mark is a
// character of the input (ignoreBOM), never stripped before the parse.
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";

const req = JSON.parse(readFileSync(0, "utf8"));
const require = createRequire(path.join(req.package, "package.json"));
const peggy = require("peggy");

const parser = peggy.generate(readFileSync(req.grammar, "utf8"), {
    allowedStartRules: [req.start],
    output: "parser",
});
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
        parser.parse(src, { startRule: req.start });
        out.push({ input, verdict: "accept" });
    } catch (e) {
        out.push({
            input,
            verdict: "reject",
            offset: e?.location?.start?.offset ?? null,
            message: String(e?.message ?? e).slice(0, 200),
        });
    }
}
process.stdout.write(out.map((r) => JSON.stringify(r)).join("\n") + (out.length ? "\n" : ""));
