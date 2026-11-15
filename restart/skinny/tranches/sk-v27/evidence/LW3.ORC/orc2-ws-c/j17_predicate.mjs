// sk-v27 LW3.ORC2-WS-C, RULINGS 2026-11-14g ORC2WSC-G: the checked predicate that admits an ebnf
// root-vs-TS-package divergence to DIVERGENCE J-17 ("the TS package reads a literal's \n \t \r \f \b
// as a backslash and a letter", owner LW4.COMP-4).
//
//   node j17_predicate.mjs <peggy pkg dir> <corpus dir> <g1 dump> <DIVERGENCE.tsv>
//
// <g1 dump> is `g1 --grammar ebnf --arms root,ts-package --dump` (each arm's verdict on every case).
// The rows are the cases where root's verdict and the TS package's differ. For each row:
//   (1) peggy's verdict (crates/oracle/peg/ebnf.peggy, entry `grammar`, whole input) equals root's;
//   (2) the TS package's verdict differs from root's (and so from peggy's);
//   (3) the case text holds the construct J-17 names: a terminal whose body contains one of the raw
//       characters U+000A, U+0009, U+000D, U+000C, U+0008 -- the characters grammar/ebnf/ebnf.bbnf's
//       `symbol` writes as the literals "\n" "\t" "\r" "\f" "\b", which the TS package reads as a
//       backslash and a letter. The exact test: scan the text left to right; outside a terminal, a `'`
//       or `"` opens one; inside, the terminal's body runs to the next occurrence of the same quote
//       (ebnf terminals have no escapes: `character - "'"` reads any other character), or to the end
//       of the text if none; the case passes when some body matches /[\n\t\r\f\b]/ (JS: U+000A,
//       U+0009, U+000D, U+000C, U+0008).
// It prints the counts per clause, every row failing a clause, and checks that DIVERGENCE J-17's case
// list equals exactly the rows passing all three. Exit 0 only when every row passes and the list matches.
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const repo = path.resolve(here, "../../../../../../..");
const [pkg, corpus, dump, divergence] = process.argv.slice(2);
const peggy = createRequire(path.join(pkg, "package.json"))("peggy");
const parser = peggy.generate(readFileSync(path.join(repo, "crates/oracle/peg/ebnf.peggy"), "utf8"), {
    allowedStartRules: ["grammar"],
});
const peg = (s) => {
    try {
        parser.parse(s);
        return "accept";
    } catch {
        return "reject";
    }
};
const text = (c) => new TextDecoder("utf-8", { ignoreBOM: true }).decode(readFileSync(path.join(corpus, c)));
const CLASS = /[\n\t\r\f\b]/;
const hasEscapeClassTerminal = (s) => {
    let i = 0;
    while (i < s.length) {
        const q = s[i];
        if (q === "'" || q === '"') {
            const end = s.indexOf(q, i + 1);
            const body = end < 0 ? s.slice(i + 1) : s.slice(i + 1, end);
            if (CLASS.test(body)) return true;
            i = end < 0 ? s.length : end + 1;
        } else i++;
    }
    return false;
};

const v = {};
for (const l of readFileSync(dump, "utf8").split("\n")) {
    const m = l.match(/^dump grammar=ebnf arm=(\S+) case=(\S+) \{"verdict":"([a-z-]+)"/);
    if (m) (v[m[2]] ??= {})[m[1]] = m[3];
}
const rows = Object.keys(v).filter((c) => v[c].root !== v[c]["ts-package"]).sort();
const fail = { 1: [], 2: [], 3: [] };
const pass = [];
for (const c of rows) {
    const s = text(c);
    const ok1 = peg(s) === v[c].root;
    const ok2 = v[c]["ts-package"] !== v[c].root;
    const ok3 = hasEscapeClassTerminal(s);
    if (!ok1) fail[1].push(c);
    if (!ok2) fail[2].push(c);
    if (!ok3) fail[3].push(c);
    if (ok1 && ok2 && ok3) pass.push(c);
}
console.log(`cases=${Object.keys(v).length} rows(root != ts-package)=${rows.length}`);
for (const k of [1, 2, 3]) console.log(`clause(${k}) passed=${rows.length - fail[k].length} failed=${fail[k].length}${fail[k].length ? " " + fail[k].join(" ") : ""}`);
const agreeing = Object.keys(v).filter((c) => v[c].root === v[c]["ts-package"]);
console.log(`control: of the ${agreeing.length} cases where root and the TS package agree, ${agreeing.filter((c) => hasEscapeClassTerminal(text(c))).length} hold the construct (clause (3) is not true of every case)`);
console.log(`all three passed=${pass.length} failed=${rows.length - pass.length}`);
const div = readFileSync(divergence, "utf8").split("\n");
const head = div.find((l) => l.startsWith("id\t")).split("\t");
const j17 = div.find((l) => l.startsWith("J-17\t")).split("\t");
const listed = j17[head.indexOf("files")].split(",").sort();
const same = listed.length === pass.length && listed.every((c, i) => c === pass[i]);
console.log(`J-17 lists ${listed.length} cases; equal to the rows passing all three: ${same}`);
process.exit(same && pass.length === rows.length ? 0 : 1);
