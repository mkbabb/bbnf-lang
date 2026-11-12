// sk-v27 LW3.ORC2: the class proof and the rows for the peggy oracle's
// divergences on ebnf, bnf and math.
//
//   node ws_model.mjs <peggy pkg dir> <corpus dir> <g1 reading log> <dump>... [--rows]
//
// Each <dump> is `g1 --grammar <g> --arms root,ts-package --dump` (the arms'
// verdict on every case). For each model in models/ it reads every case of
// its grammar with that model and prints how many of the arm's verdicts it
// equals: a model that equals the arm on every case is the whole of that
// arm's reading there, so the arm's divergence from the oracle transcription
// (crates/oracle/peg/<g>.peggy) is the model's difference from it.
//
// With --rows it prints, for each unruled divergence of the reading log on
// ebnf, bnf and math, the row that rules it:
//   R  <ruled.tsv row>            root arm against the peggy oracle;
//   J  <DIVERGENCE id> <case>     a divergence where the TS package is a side.
// A divergence no proven model explains is printed as UNEXPLAINED and the
// script exits 1.
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const repo = path.resolve(here, "../../../../../..");
const args = process.argv.slice(2);
const rows = args.includes("--rows");
const [pkg, corpus, log, ...dumps] = args.filter((a) => a !== "--rows");
const peggy = createRequire(path.join(pkg, "package.json"))("peggy");
const START = { ebnf: "grammar", bnf: "grammar", math: "number" };
const load = (file, start) => {
    const p = peggy.generate(readFileSync(file, "utf8"), { allowedStartRules: [start] });
    return (s) => {
        try {
            p.parse(s);
            return "accept";
        } catch {
            return "reject";
        }
    };
};
const text = (c) => new TextDecoder("utf-8", { ignoreBOM: true }).decode(readFileSync(path.join(corpus, c)));

const verdict = {}; // verdict[g][arm][case]
for (const d of dumps) {
    for (const l of readFileSync(d, "utf8").split("\n")) {
        const m = l.match(/^dump grammar=(\S+) arm=(\S+) case=(\S+) \{"verdict":"([a-z-]+)"/);
        if (!m) continue;
        ((verdict[m[1]] ??= {})[m[2]] ??= {})[m[3]] = m[4];
    }
}

const MODELS = [
    ["ebnf", "root", "root-ebnf.peggy"],
    ["bnf", "root", "root-bnf.peggy"],
    ["math", "root", "root-math.peggy"],
    ["ebnf", "ts-package", "ts-ebnf.peggy"],
];
const proven = {}; // proven[g][arm] = model fn
const spec = {};
let bad = 0;
for (const [g, arm, file] of MODELS) {
    const model = load(path.join(here, "models", file), START[g]);
    spec[g] ??= load(path.join(repo, `crates/oracle/peg/${g}.peggy`), START[g]);
    const cases = Object.entries(verdict[g]?.[arm] ?? {});
    const differ = cases.filter(([c, v]) => model(text(c)) !== v).map(([c]) => c);
    console.log(`model ${file}: ${arm} on ${g}: equal on ${cases.length - differ.length} of ${cases.length}${differ.length ? `, differs on ${differ.slice(0, 5).join(" ")}` : ""}`);
    if (differ.length === 0 && cases.length > 0) (proven[g] ??= {})[arm] = model;
    else bad++;
}

if (rows) {
    const ROOT_OWNER =
        "LW4.COMP-1: root reads [ \\t\\n\\r]* where the grammar reads none (before every rule, as ?w, for the entry repetition's whitespace rule); cure LW4.COMP-1 at LW4 (born-RED crates/core/tests/rb_root_whitespace.rs::root_reads_only_the_whitespace_the_grammar_reads)";
    const ROOT_AUTH =
        "docs/bbnf/operators.md ?w (`ws` matches `\\s`) and the grammar's text; LW3.ORC2 class proof evidence/LW3.ORC/ws_model.mjs (models/root-<g>.peggy equals root on every case)";
    const J_ROOT = { ebnf: "J-14", bnf: "J-15", math: "J-16" };
    let arm = null;
    let grammar = null;
    for (const l of readFileSync(log, "utf8").split("\n")) {
        const h = l.match(/^grammar=(\S+) arm=(\S+) /);
        if (h) {
            [grammar, arm] = [h[1], h[2]];
            continue;
        }
        const u = l.match(/^ {4}(\S+) \[(\S+)\]: /);
        if (!u || !(grammar in START)) continue;
        const [, c, oracle] = u;
        const s = text(c);
        const want = spec[grammar](s);
        const rootWrong = proven[grammar]?.root && verdict[grammar].root[c] !== want;
        const tsWrong = verdict[grammar]["ts-package"][c] !== want;
        const tsProven = Boolean(proven[grammar]?.["ts-package"]);
        let out = [];
        if (arm === "root" && oracle === "peggy" && rootWrong) {
            out.push(`R\troot\t${grammar}\tpeggy\t${c}\t${ROOT_OWNER}\t${ROOT_AUTH}`);
        } else if (arm === "root" && oracle === "ts-package") {
            if (rootWrong) out.push(`J\t${J_ROOT[grammar]}\t${c}`);
            if (tsWrong && tsProven) out.push(`J\tJ-17\t${c}`);
            if (tsWrong && !tsProven) out = [];
        } else if (arm === "ts-package" && oracle === "peggy" && tsWrong && tsProven) {
            out.push(`J\tJ-18\t${c}`);
        }
        if (out.length === 0) {
            console.log(`UNEXPLAINED\t${grammar}\t${arm}\t${oracle}\t${c}`);
            bad++;
        }
        for (const o of out) console.log(o);
    }
}
process.exit(bad ? 1 : 0);
