// sk-v27 LW3.COMP-3a: the TS-package arm's driver.
//
// Runs the published @mkbabb/bbnf-lang package (never typescript/ HEAD) as a
// parse oracle, from the directory the harness unpacked its pinned,
// sha256-checked tarballs into (see ts/pin.json). One request on stdin:
//   { "package": "<dir whose node_modules holds @mkbabb/bbnf-lang>",
//     "sources": { "<abs path>": "<grammar text>" },     (optional; see below)
//     "jobs": [ { "id": "...", "mode": "parse" | "frontend", "grammar": "<abs path>",
//                 "entry": "<rule>" | null, "inputs": ["<abs path>", ...] } ] }
// "sources" overrides the text read for a grammar file: the harness passes each
// grammar with its `->` mappings erased (docs/bbnf: a front end that does not
// emit Rust keeps the expression as text, and a mapping never changes what the
// factor matches; 0.3.0 predates reading them). Mode "frontend" judges each
// input as a BBNF grammar text with the package's own front end
// (BBNFToASTWithImports): the oracle for the `bbnf` grammar.
// One JSON line per input on stdout:
//   { "job", "input", "verdict": "accept" | "reject" | "grammar-refused" | "unreadable",
//     "obs"?, "furthest"?, "expected"?, "message"? }
// The observation tree labels each node with the rule that produced it (a
// "span" action on every rule) and carries its span; a leaf is the string a
// terminal matched. A rejection reports parse-that's furthest offset and its
// expected set, sorted and deduplicated here: the TS state keeps the set
// unsorted (LW3.COMP-4b sorts it at the source).
// Each input is decoded with its bytes kept: a leading byte-order mark is a
// character of the input (ignoreBOM), never removed before the parse
// (LW3.ORC1-BOM).
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";

const req = JSON.parse(readFileSync(0, "utf8"));
const require = createRequire(path.join(req.package, "package.json"));
const bbnf = await import(require.resolve("@mkbabb/bbnf-lang"));
const sources = req.sources ?? {};
const readGrammar = (p) => sources[p] ?? readFileSync(p, "utf8");

function toObs(v, src) {
    if (v === null || v === undefined) return { t: "node", children: [] };
    if (typeof v === "string") return { t: "leaf", text: v };
    if (typeof v === "number" || typeof v === "boolean" || typeof v === "bigint") {
        return { t: "leaf", text: String(v) };
    }
    if (Array.isArray(v)) return { t: "node", children: v.map((x) => toObs(x, src)) };
    if (typeof v === "object" && "__rule" in v) {
        const inner = toObs(v.value, src);
        const span = [v.start, v.end];
        // A rule over an unlabelled value takes it over (a plain string is the
        // rule's leaf, a sequence its children); a rule whose body is one other
        // rule nests that rule's tree, so neither label is lost.
        if (inner.label !== undefined) return { t: "node", label: v.__rule, span, children: [inner] };
        if (inner.t === "leaf") return { t: "leaf", label: v.__rule, span, text: inner.text };
        return { t: "node", label: v.__rule, span, children: inner.children };
    }
    if (typeof v === "object" && typeof v.start === "number" && typeof v.end === "number") {
        return { t: "leaf", span: [v.start, v.end], text: src.slice(v.start, v.end) };
    }
    return { t: "leaf", text: JSON.stringify(v) ?? String(v) };
}

const out = [];
for (const job of req.jobs) {
    if (job.mode === "frontend") {
        for (const input of job.inputs) {
            let src;
            try {
                src = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true }).decode(readFileSync(input));
            } catch {
                out.push({ job: job.id, input, verdict: "unreadable", message: "not UTF-8" });
                continue;
            }
            let ok = false;
            try {
                const r = bbnf.BBNFToASTWithImports(src);
                ok = r.length === 2 && r[1] !== undefined;
            } catch {
                ok = false;
            }
            out.push(ok ? { job: job.id, input, verdict: "accept" } : { job: job.id, input, verdict: "reject", furthest: 0, expected: [] });
        }
        continue;
    }
    let parser;
    try {
        // Read the rule names first, then compile with a span action on each.
        const [, firstAst] = bbnf.BBNFToParserFromFile(job.grammar, readGrammar);
        const actions = {};
        for (const name of firstAst.keys()) {
            actions[name] = { kind: "span", fn: (value, start, end) => ({ __rule: name, value, start, end }) };
        }
        const [nts, ast] = bbnf.BBNFToParserFromFile(job.grammar, readGrammar, { actions });
        const entry = job.entry ?? [...ast.keys()][0];
        if (!nts[entry]) throw new Error(`the grammar has no rule ${entry}`);
        parser = nts[entry].eof();
    } catch (e) {
        for (const input of job.inputs) {
            out.push({ job: job.id, input, verdict: "grammar-refused", message: String(e?.message ?? e).slice(0, 300) });
        }
        continue;
    }
    for (const input of job.inputs) {
        let src;
        try {
            src = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true }).decode(readFileSync(input));
        } catch {
            out.push({ job: job.id, input, verdict: "unreadable", message: "not UTF-8" });
            continue;
        }
        try {
            const st = parser.parseState(src);
            if (st.isError) {
                const expected = [...new Set((st.expected ?? []).map(String))].sort();
                out.push({ job: job.id, input, verdict: "reject", furthest: st.furthest ?? st.offset, expected });
            } else {
                out.push({ job: job.id, input, verdict: "accept", obs: toObs(st.value, src) });
            }
        } catch (e) {
            const message = String(e?.message ?? e).slice(0, 300);
            out.push({ job: job.id, input, verdict: "reject", furthest: 0, expected: [], message });
        }
    }
}
process.stdout.write(out.map((r) => JSON.stringify(r)).join("\n") + (out.length ? "\n" : ""));
